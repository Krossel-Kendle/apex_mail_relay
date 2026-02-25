unit Relay.Queue;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Relay.Config,
  Relay.Log,
  Relay.Types;

type
  TSpoolItem = record
    Id: string;
    CreatedUtc: TDateTime;
    Attempt: Integer;
    NextAttemptUtc: TDateTime;
    LastError: string;
    LastErrorAtUtc: TDateTime;
    HasLastErrorAtUtc: Boolean;
    MailFrom: string;
    Recipients: TArray<string>;
    SizeBytes: Int64;
    MessageBytes: TBytes;
    JsonPath: string;
    EmlPath: string;
  end;

  TQueueItemState = (qisNew, qisInFlight, qisDeferred, qisDead);

  TQueueItemInfo = record
    CreatedUtc: TDateTime;
    MailFrom: string;
    Recipients: TArray<string>;
    State: TQueueItemState;
  end;

  TSpoolQueue = class
  private
    FCritSec: TCriticalSection;
    FLogger: TRelayLogger;

    FRootPath: string;
    FDirNew: string;
    FDirInFlight: string;
    FDirDeferred: string;
    FDirDead: string;
    FDirTmp: string;

    FMaxQueueItems: Integer;
    FMaxQueueBytes: Int64;
    FInFlightStaleSec: Integer;

    FRetryMaxAttempts: Integer;
    FRetryBaseDelaySec: Integer;
    FRetryMaxDelaySec: Integer;
    FRetryJitterPct: Integer;

    function UtcNow: TDateTime;
    function NewItemId: string;
    procedure EnsureDirectories;
    function BuildJsonPath(const ADir, AId: string): string;
    function BuildEmlPath(const ADir, AId: string): string;
    function CalcDelaySec(AAttempt: Integer): Integer;

    function ReadMeta(const AJsonPath: string; out AItem: TSpoolItem): Boolean;
    function WriteMeta(const AJsonPath: string; const AItem: TSpoolItem): Boolean;
    function LoadItemFromDir(const ADir, AId: string; out AItem: TSpoolItem): Boolean;
    function MovePair(const AFromDir, AToDir, AId: string): Boolean;
    procedure CleanupTempPair(const ADir, AId: string);
    procedure GetUsage(out ACount: Integer; out ABytes: Int64);
    function TryTakeFromDir(const ADir: string; ACheckDue: Boolean; out AItem: TSpoolItem): Boolean;
  public
    constructor Create(const AConfig: TRelayConfig; ALogger: TRelayLogger);
    destructor Destroy; override;
    procedure ApplyConfig(const AConfig: TRelayConfig);

    function Enqueue(const AMessageBytes: TBytes; const AMailFrom: string; const ARecipients: TArray<string>;
      out ASmtpCode: Integer; out AReplyText: string): Boolean;
    function TryTake(out AItem: TSpoolItem): Boolean;
    procedure MarkSuccess(const AItem: TSpoolItem);
    procedure MarkFailure(const AItem: TSpoolItem; const AErrorText: string; APermanentFailure: Boolean);
    procedure RecoverStaleInFlight;
    function ForceDeferredNow: Integer;

    function Stats: TQueueStats;
    function SnapshotItems(const AMaxItems: Integer = 5000): TArray<TQueueItemInfo>;
    function PurgeDeadLetters: Integer;
    property RootPath: string read FRootPath;
  end;

implementation

uses
  System.Math,
  System.DateUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.JSON;

function GetDirectoryFilesSafe(const APath, APattern: string): TArray<string>;
begin
  if TDirectory.Exists(APath) then
    Result := TDirectory.GetFiles(APath, APattern)
  else
    Result := [];
end;

constructor TSpoolQueue.Create(const AConfig: TRelayConfig; ALogger: TRelayLogger);
begin
  inherited Create;
  FCritSec := TCriticalSection.Create;
  FLogger := ALogger;
  Randomize;
  ApplyConfig(AConfig);
  RecoverStaleInFlight;
end;

destructor TSpoolQueue.Destroy;
begin
  FCritSec.Free;
  inherited Destroy;
end;

function TSpoolQueue.UtcNow: TDateTime;
begin
  Result := TTimeZone.Local.ToUniversalTime(Now);
end;

function TSpoolQueue.NewItemId: string;
var
  LGUID: TGUID;
begin
  CreateGUID(LGUID);
  Result := GUIDToString(LGUID);
  Result := Result.Replace('{', '').Replace('}', '').Replace('-', '');
end;

procedure TSpoolQueue.EnsureDirectories;
begin
  TDirectory.CreateDirectory(FRootPath);
  TDirectory.CreateDirectory(FDirNew);
  TDirectory.CreateDirectory(FDirInFlight);
  TDirectory.CreateDirectory(FDirDeferred);
  TDirectory.CreateDirectory(FDirDead);
  TDirectory.CreateDirectory(FDirTmp);
end;

procedure TSpoolQueue.ApplyConfig(const AConfig: TRelayConfig);
begin
  FCritSec.Acquire;
  try
    FRootPath := AConfig.ResolveQueuePath(ExtractFilePath(ParamStr(0)));
    FDirNew := TPath.Combine(FRootPath, 'new');
    FDirInFlight := TPath.Combine(FRootPath, 'inflight');
    FDirDeferred := TPath.Combine(FRootPath, 'deferred');
    FDirDead := TPath.Combine(FRootPath, 'dead');
    FDirTmp := TPath.Combine(FRootPath, 'tmp');

    FMaxQueueItems := AConfig.QueueMaxItems;
    FMaxQueueBytes := Int64(AConfig.QueueMaxBytesMB) * 1024 * 1024;
    FInFlightStaleSec := AConfig.QueueInFlightStaleSec;

    FRetryMaxAttempts := AConfig.RetryMaxAttempts;
    FRetryBaseDelaySec := AConfig.RetryBaseDelaySec;
    FRetryMaxDelaySec := AConfig.RetryMaxDelaySec;
    FRetryJitterPct := AConfig.RetryJitterPct;

    EnsureDirectories;
  finally
    FCritSec.Release;
  end;
end;

function TSpoolQueue.BuildJsonPath(const ADir, AId: string): string;
begin
  Result := TPath.Combine(ADir, AId + '.json');
end;

function TSpoolQueue.BuildEmlPath(const ADir, AId: string): string;
begin
  Result := TPath.Combine(ADir, AId + '.eml');
end;

function TSpoolQueue.CalcDelaySec(AAttempt: Integer): Integer;
var
  LDelay: Double;
  LJitterRange: Double;
  LJitter: Double;
begin
  LDelay := FRetryBaseDelaySec * Power(2, AAttempt);
  if LDelay > FRetryMaxDelaySec then
    LDelay := FRetryMaxDelaySec;

  if FRetryJitterPct > 0 then
  begin
    LJitterRange := LDelay * (FRetryJitterPct / 100.0);
    LJitter := (Random * (2 * LJitterRange)) - LJitterRange;
    LDelay := LDelay + LJitter;
  end;

  if LDelay < 1 then
    LDelay := 1;
  Result := Round(LDelay);
end;

function TSpoolQueue.WriteMeta(const AJsonPath: string; const AItem: TSpoolItem): Boolean;
var
  LObj: TJSONObject;
  LArr: TJSONArray;
  LRecipient: string;
begin
  LObj := TJSONObject.Create;
  try
    LObj.AddPair('id', AItem.Id);
    LObj.AddPair('createdUtc', DateToISO8601(AItem.CreatedUtc, True));
    LObj.AddPair('attempt', TJSONNumber.Create(AItem.Attempt));
    LObj.AddPair('nextAttemptUtc', DateToISO8601(AItem.NextAttemptUtc, True));
    LObj.AddPair('lastError', AItem.LastError);
    if AItem.HasLastErrorAtUtc then
      LObj.AddPair('lastErrorAtUtc', DateToISO8601(AItem.LastErrorAtUtc, True))
    else
      LObj.AddPair('lastErrorAtUtc', TJSONNull.Create);
    LObj.AddPair('from', AItem.MailFrom);
    LArr := TJSONArray.Create;
    for LRecipient in AItem.Recipients do
      LArr.Add(LRecipient);
    LObj.AddPair('rcpt', LArr);
    LObj.AddPair('sizeBytes', TJSONNumber.Create(AItem.SizeBytes));
    TFile.WriteAllText(AJsonPath, LObj.ToJSON, TEncoding.UTF8);
    Result := True;
  except
    Result := False;
  end;
  LObj.Free;
end;

function TryParseIsoUtc(const AValue: string; out ADate: TDateTime): Boolean;
begin
  Result := TryISO8601ToDate(AValue, ADate, True);
  if not Result then
    Result := TryISO8601ToDate(AValue, ADate);
end;

function TSpoolQueue.ReadMeta(const AJsonPath: string; out AItem: TSpoolItem): Boolean;
var
  LText: string;
  LJson: TJSONValue;
  LObj: TJSONObject;
  LValue: TJSONValue;
  LArray: TJSONArray;
  I: Integer;
  LDate: TDateTime;
begin
  Result := False;
  AItem := Default(TSpoolItem);

  if not TFile.Exists(AJsonPath) then
    Exit;

  LText := TFile.ReadAllText(AJsonPath, TEncoding.UTF8);
  LJson := TJSONObject.ParseJSONValue(LText);
  try
    if not (LJson is TJSONObject) then
      Exit;
    LObj := TJSONObject(LJson);

    LValue := LObj.Values['id'];
    if not Assigned(LValue) then
      Exit;
    AItem.Id := LValue.Value;

    LValue := LObj.Values['createdUtc'];
    if Assigned(LValue) and TryParseIsoUtc(LValue.Value, LDate) then
      AItem.CreatedUtc := LDate
    else
      AItem.CreatedUtc := UtcNow;

    LValue := LObj.Values['attempt'];
    if Assigned(LValue) then
      AItem.Attempt := StrToIntDef(LValue.Value, 0);

    LValue := LObj.Values['nextAttemptUtc'];
    if Assigned(LValue) and TryParseIsoUtc(LValue.Value, LDate) then
      AItem.NextAttemptUtc := LDate
    else
      AItem.NextAttemptUtc := UtcNow;

    LValue := LObj.Values['lastError'];
    if Assigned(LValue) then
      AItem.LastError := LValue.Value;

    LValue := LObj.Values['lastErrorAtUtc'];
    if Assigned(LValue) and (not (LValue is TJSONNull)) and TryParseIsoUtc(LValue.Value, LDate) then
    begin
      AItem.HasLastErrorAtUtc := True;
      AItem.LastErrorAtUtc := LDate;
    end;

    LValue := LObj.Values['from'];
    if Assigned(LValue) then
      AItem.MailFrom := LValue.Value;

    LValue := LObj.Values['rcpt'];
    if Assigned(LValue) and (LValue is TJSONArray) then
    begin
      LArray := TJSONArray(LValue);
      SetLength(AItem.Recipients, LArray.Count);
      for I := 0 to LArray.Count - 1 do
        AItem.Recipients[I] := LArray.Items[I].Value;
    end
    else
      SetLength(AItem.Recipients, 0);

    LValue := LObj.Values['sizeBytes'];
    if Assigned(LValue) then
      AItem.SizeBytes := StrToInt64Def(LValue.Value, 0);

    Result := True;
  finally
    LJson.Free;
  end;
end;

function TSpoolQueue.LoadItemFromDir(const ADir, AId: string; out AItem: TSpoolItem): Boolean;
var
  LJsonPath: string;
  LEmlPath: string;
begin
  LJsonPath := BuildJsonPath(ADir, AId);
  LEmlPath := BuildEmlPath(ADir, AId);

  Result := ReadMeta(LJsonPath, AItem);
  if not Result then
    Exit;

  if not TFile.Exists(LEmlPath) then
    Exit(False);

  AItem.JsonPath := LJsonPath;
  AItem.EmlPath := LEmlPath;
  AItem.MessageBytes := TFile.ReadAllBytes(LEmlPath);
  if AItem.SizeBytes <= 0 then
    AItem.SizeBytes := Length(AItem.MessageBytes);
end;

function TSpoolQueue.MovePair(const AFromDir, AToDir, AId: string): Boolean;
var
  LFromJson: string;
  LFromEml: string;
  LToJson: string;
  LToEml: string;
begin
  LFromJson := BuildJsonPath(AFromDir, AId);
  LFromEml := BuildEmlPath(AFromDir, AId);
  LToJson := BuildJsonPath(AToDir, AId);
  LToEml := BuildEmlPath(AToDir, AId);

  if not TFile.Exists(LFromJson) then
    Exit(False);
  if not TFile.Exists(LFromEml) then
    Exit(False);

  try
    TFile.Move(LFromJson, LToJson);
    try
      TFile.Move(LFromEml, LToEml);
      Result := True;
    except
      on Exception do
      begin
        if TFile.Exists(LToJson) then
          TFile.Move(LToJson, LFromJson);
        raise;
      end;
    end;
  except
    Result := False;
  end;
end;

procedure TSpoolQueue.CleanupTempPair(const ADir, AId: string);
var
  LJson: string;
  LEml: string;
begin
  LJson := BuildJsonPath(ADir, AId);
  LEml := BuildEmlPath(ADir, AId);
  if TFile.Exists(LJson) then
    TFile.Delete(LJson);
  if TFile.Exists(LEml) then
    TFile.Delete(LEml);
end;

procedure TSpoolQueue.GetUsage(out ACount: Integer; out ABytes: Int64);
var
  LFiles: TArray<string>;
  LFile: string;
begin
  ACount := 0;
  ABytes := 0;

  LFiles := GetDirectoryFilesSafe(FDirNew, '*.eml');
  ACount := ACount + Length(LFiles);
  for LFile in LFiles do
    ABytes := ABytes + TFile.GetSize(LFile);

  LFiles := GetDirectoryFilesSafe(FDirInFlight, '*.eml');
  ACount := ACount + Length(LFiles);
  for LFile in LFiles do
    ABytes := ABytes + TFile.GetSize(LFile);

  LFiles := GetDirectoryFilesSafe(FDirDeferred, '*.eml');
  ACount := ACount + Length(LFiles);
  for LFile in LFiles do
    ABytes := ABytes + TFile.GetSize(LFile);
end;

function TSpoolQueue.Enqueue(const AMessageBytes: TBytes; const AMailFrom: string; const ARecipients: TArray<string>;
  out ASmtpCode: Integer; out AReplyText: string): Boolean;
var
  LItem: TSpoolItem;
  LCount: Integer;
  LBytes: Int64;
  LTmpJson: string;
  LTmpEml: string;
begin
  ASmtpCode := 451;
  AReplyText := '4.3.0 Temporary local problem';

  FCritSec.Acquire;
  try
    GetUsage(LCount, LBytes);
    if LCount >= FMaxQueueItems then
    begin
      ASmtpCode := 452;
      AReplyText := '4.3.1 Insufficient system storage';
      Exit(False);
    end;

    if LBytes + Length(AMessageBytes) > FMaxQueueBytes then
    begin
      ASmtpCode := 452;
      AReplyText := '4.3.1 Insufficient system storage';
      Exit(False);
    end;

    LItem := Default(TSpoolItem);
    LItem.Id := NewItemId;
    LItem.CreatedUtc := UtcNow;
    LItem.Attempt := 0;
    LItem.NextAttemptUtc := LItem.CreatedUtc;
    LItem.LastError := '';
    LItem.HasLastErrorAtUtc := False;
    LItem.MailFrom := AMailFrom;
    LItem.Recipients := Copy(ARecipients, 0, Length(ARecipients));
    LItem.SizeBytes := Length(AMessageBytes);

    LTmpJson := BuildJsonPath(FDirTmp, LItem.Id);
    LTmpEml := BuildEmlPath(FDirTmp, LItem.Id);

    TFile.WriteAllBytes(LTmpEml, AMessageBytes);
    if not WriteMeta(LTmpJson, LItem) then
      raise Exception.Create('Cannot write queue metadata.');

    if not MovePair(FDirTmp, FDirNew, LItem.Id) then
      raise Exception.Create('Cannot move queue item to new.');

    ASmtpCode := 250;
    AReplyText := '2.0.0 Queued';
    Result := True;
    if Assigned(FLogger) then
      FLogger.Add('QUEUE', Format('Enqueued id=%s size=%d bytes', [LItem.Id, LItem.SizeBytes]));
  except
    on E: Exception do
    begin
      CleanupTempPair(FDirTmp, LItem.Id);
      ASmtpCode := 451;
      AReplyText := '4.3.0 Temporary local problem';
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Queue enqueue failed: ' + E.Message);
      Result := False;
    end;
  end;
  FCritSec.Release;
end;

function TSpoolQueue.TryTakeFromDir(const ADir: string; ACheckDue: Boolean; out AItem: TSpoolItem): Boolean;
var
  LJsonFiles: TArray<string>;
  LJsonPath: string;
  LId: string;
  LMeta: TSpoolItem;
  LNowUtc: TDateTime;
begin
  Result := False;
  LNowUtc := UtcNow;

  LJsonFiles := GetDirectoryFilesSafe(ADir, '*.json');
  for LJsonPath in LJsonFiles do
  begin
    LId := TPath.GetFileNameWithoutExtension(LJsonPath);

    if not TFile.Exists(BuildEmlPath(ADir, LId)) then
    begin
      if Assigned(FLogger) then
        FLogger.Add('WARN', 'Queue item missing eml, moving to dead: ' + LId);
      TFile.Delete(LJsonPath);
      Continue;
    end;

    if ACheckDue then
    begin
      if not ReadMeta(LJsonPath, LMeta) then
      begin
        MovePair(ADir, FDirDead, LId);
        Continue;
      end;
      if LMeta.NextAttemptUtc > LNowUtc then
        Continue;
    end;

    if not MovePair(ADir, FDirInFlight, LId) then
      Continue;

    if LoadItemFromDir(FDirInFlight, LId, AItem) then
    begin
      Result := True;
      Exit;
    end;

    MovePair(FDirInFlight, FDirDead, LId);
  end;
end;

function TSpoolQueue.TryTake(out AItem: TSpoolItem): Boolean;
begin
  FCritSec.Acquire;
  try
    Result := TryTakeFromDir(FDirNew, False, AItem);
    if not Result then
      Result := TryTakeFromDir(FDirDeferred, True, AItem);
    if Result and Assigned(FLogger) then
      FLogger.Add('QUEUE', Format('Dequeued id=%s attempt=%d', [AItem.Id, AItem.Attempt]));
  finally
    FCritSec.Release;
  end;
end;

procedure TSpoolQueue.MarkSuccess(const AItem: TSpoolItem);
begin
  FCritSec.Acquire;
  try
    if TFile.Exists(AItem.JsonPath) then
      TFile.Delete(AItem.JsonPath);
    if TFile.Exists(AItem.EmlPath) then
      TFile.Delete(AItem.EmlPath);
    if Assigned(FLogger) then
      FLogger.Add('QUEUE', 'Committed id=' + AItem.Id);
  finally
    FCritSec.Release;
  end;
end;

procedure TSpoolQueue.MarkFailure(const AItem: TSpoolItem; const AErrorText: string; APermanentFailure: Boolean);
var
  LMeta: TSpoolItem;
  LDelaySec: Integer;
  LTargetDir: string;
begin
  FCritSec.Acquire;
  try
    if not ReadMeta(AItem.JsonPath, LMeta) then
      LMeta := AItem;

    LMeta.Attempt := LMeta.Attempt + 1;
    LMeta.LastError := AErrorText;
    LMeta.LastErrorAtUtc := UtcNow;
    LMeta.HasLastErrorAtUtc := True;

    if APermanentFailure or (LMeta.Attempt >= FRetryMaxAttempts) then
      LTargetDir := FDirDead
    else
    begin
      LDelaySec := CalcDelaySec(LMeta.Attempt);
      LMeta.NextAttemptUtc := IncSecond(UtcNow, LDelaySec);
      LTargetDir := FDirDeferred;
    end;

    if not WriteMeta(AItem.JsonPath, LMeta) then
      raise Exception.Create('Cannot update queue metadata.');

    if not MovePair(FDirInFlight, LTargetDir, LMeta.Id) then
      raise Exception.Create('Cannot move failed item.');

    if Assigned(FLogger) then
    begin
      if LTargetDir = FDirDead then
        FLogger.Add('ERROR', Format('Moved to dead id=%s error=%s', [LMeta.Id, AErrorText]))
      else
        FLogger.Add('WARN', Format('Deferred id=%s attempt=%d error=%s', [LMeta.Id, LMeta.Attempt, AErrorText]));
    end;
  except
    on E: Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'MarkFailure failed for id=' + AItem.Id + ': ' + E.Message);
    end;
  end;
  FCritSec.Release;
end;

procedure TSpoolQueue.RecoverStaleInFlight;
var
  LJsonFiles: TArray<string>;
  LJsonPath: string;
  LId: string;
  LAgeSec: Int64;
  LMeta: TSpoolItem;
begin
  FCritSec.Acquire;
  try
    LJsonFiles := GetDirectoryFilesSafe(FDirInFlight, '*.json');
    for LJsonPath in LJsonFiles do
    begin
      LId := TPath.GetFileNameWithoutExtension(LJsonPath);
      LAgeSec := SecondsBetween(UtcNow, TFile.GetLastWriteTimeUtc(LJsonPath));
      if LAgeSec < FInFlightStaleSec then
        Continue;

      if not ReadMeta(LJsonPath, LMeta) then
      begin
        MovePair(FDirInFlight, FDirDead, LId);
        Continue;
      end;

      LMeta.NextAttemptUtc := UtcNow;
      LMeta.LastError := 'Recovered stale in-flight item';
      LMeta.LastErrorAtUtc := UtcNow;
      LMeta.HasLastErrorAtUtc := True;
      WriteMeta(LJsonPath, LMeta);

      if MovePair(FDirInFlight, FDirDeferred, LId) and Assigned(FLogger) then
        FLogger.Add('WARN', Format('Recovered stale in-flight id=%s ageSec=%d', [LId, LAgeSec]));
    end;
  finally
    FCritSec.Release;
  end;
end;

function TSpoolQueue.ForceDeferredNow: Integer;
var
  LJsonFiles: TArray<string>;
  LJsonPath: string;
  LMeta: TSpoolItem;
begin
  Result := 0;
  FCritSec.Acquire;
  try
    LJsonFiles := GetDirectoryFilesSafe(FDirDeferred, '*.json');
    for LJsonPath in LJsonFiles do
    begin
      if not ReadMeta(LJsonPath, LMeta) then
        Continue;
      LMeta.NextAttemptUtc := UtcNow;
      if WriteMeta(LJsonPath, LMeta) then
        Inc(Result);
    end;
    if Assigned(FLogger) then
      FLogger.Add('QUEUE', Format('ForceDeferredNow updated=%d', [Result]));
  finally
    FCritSec.Release;
  end;
end;

function TSpoolQueue.Stats: TQueueStats;
var
  LJsonFiles: TArray<string>;
  LOldestUtc: TDateTime;
  LHasOldest: Boolean;
  LWriteUtc: TDateTime;

  procedure ScanDir(const APath: string; out ACount: Integer; AIncludeInUsage: Boolean);
  var
    LFileLocal: string;
    LId: string;
    LEmlPath: string;
  begin
    ACount := 0;
    LJsonFiles := GetDirectoryFilesSafe(APath, '*.json');
    ACount := Length(LJsonFiles);
    for LFileLocal in LJsonFiles do
    begin
      LId := TPath.GetFileNameWithoutExtension(LFileLocal);
      LEmlPath := BuildEmlPath(APath, LId);
      if AIncludeInUsage and TFile.Exists(LEmlPath) then
        Result.TotalBytes := Result.TotalBytes + TFile.GetSize(LEmlPath);

      if AIncludeInUsage then
      begin
        LWriteUtc := TFile.GetLastWriteTimeUtc(LFileLocal);
        if (not LHasOldest) or (LWriteUtc < LOldestUtc) then
        begin
          LOldestUtc := LWriteUtc;
          LHasOldest := True;
        end;
      end;
    end;
  end;
begin
  Result := Default(TQueueStats);
  LHasOldest := False;
  LOldestUtc := 0;

  FCritSec.Acquire;
  try
    ScanDir(FDirNew, Result.NewCount, True);
    ScanDir(FDirInFlight, Result.InFlightCount, True);
    ScanDir(FDirDeferred, Result.DeferredCount, True);
    ScanDir(FDirDead, Result.DeadCount, False);

    Result.TotalCount := Result.NewCount + Result.InFlightCount + Result.DeferredCount;
    if LHasOldest then
      Result.OldestAgeSec := SecondsBetween(UtcNow, LOldestUtc)
    else
      Result.OldestAgeSec := 0;
  finally
    FCritSec.Release;
  end;
end;

function TSpoolQueue.PurgeDeadLetters: Integer;
var
  LFiles: TArray<string>;
  LFile: string;
begin
  Result := 0;
  FCritSec.Acquire;
  try
    LFiles := GetDirectoryFilesSafe(FDirDead, '*.*');
    for LFile in LFiles do
    begin
      TFile.Delete(LFile);
      Inc(Result);
    end;
    if Assigned(FLogger) then
      FLogger.Add('QUEUE', Format('Purged dead letters files=%d', [Result]));
  finally
    FCritSec.Release;
  end;
end;

function TSpoolQueue.SnapshotItems(const AMaxItems: Integer): TArray<TQueueItemInfo>;
var
  LList: TList<TQueueItemInfo>;

  function ReachedMax: Boolean;
  begin
    Result := (AMaxItems > 0) and (LList.Count >= AMaxItems);
  end;

  procedure AddFromDir(const ADir: string; const AState: TQueueItemState);
  var
    LJsonFiles: TArray<string>;
    LJsonPath: string;
    LMeta: TSpoolItem;
    LInfo: TQueueItemInfo;
  begin
    LJsonFiles := GetDirectoryFilesSafe(ADir, '*.json');
    for LJsonPath in LJsonFiles do
    begin
      if ReachedMax then
        Exit;
      if not ReadMeta(LJsonPath, LMeta) then
        Continue;

      LInfo.CreatedUtc := LMeta.CreatedUtc;
      LInfo.MailFrom := LMeta.MailFrom;
      LInfo.Recipients := Copy(LMeta.Recipients, 0, Length(LMeta.Recipients));
      LInfo.State := AState;
      LList.Add(LInfo);
    end;
  end;
begin
  LList := TList<TQueueItemInfo>.Create;
  FCritSec.Acquire;
  try
    AddFromDir(FDirNew, qisNew);
    if not ReachedMax then
      AddFromDir(FDirInFlight, qisInFlight);
    if not ReachedMax then
      AddFromDir(FDirDeferred, qisDeferred);
    if not ReachedMax then
      AddFromDir(FDirDead, qisDead);
    Result := LList.ToArray;
  finally
    FCritSec.Release;
    LList.Free;
  end;
end;

end.
