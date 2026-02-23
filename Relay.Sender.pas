unit Relay.Sender;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  Relay.Config,
  Relay.Log,
  Relay.Queue,
  Relay.Types;

type
  TRelaySenderManager = class;

  TRelaySenderWorker = class(TThread)
  private
    FManager: TRelaySenderManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TRelaySenderManager);
  end;

  TRelaySenderManager = class
  private
    FCritSec: TCriticalSection;
    FQueue: TSpoolQueue;
    FLogger: TRelayLogger;
    FWorkers: TObjectList<TRelaySenderWorker>;
    FWakeEvent: TEvent;

    FStatus: TRelaySenderStatus;
    FLastError: string;
    FRunning: Boolean;
    FActiveSenders: Integer;

    FHost: string;
    FPort: Integer;
    FTlsMode: TRelayTlsMode;
    FWorkerCount: Integer;
    FConnectTimeoutSec: Integer;
    FReadTimeoutSec: Integer;
    FWriteTimeoutSec: Integer;
    FAuthUserEnc: string;
    FAuthPasswordEnc: string;

    function GetStatus: TRelaySenderStatus;
    function GetLastError: string;
    function GetActiveSenders: Integer;
    function IsRunning: Boolean;
    procedure SetStatus(AStatus: TRelaySenderStatus; const ALastError: string = '');
    function SendOne(const AItem: TSpoolItem; out AErrorText: string; out APermanentFailure: Boolean): Boolean;
  public
    constructor Create(AQueue: TSpoolQueue; ALogger: TRelayLogger);
    destructor Destroy; override;

    procedure ApplyConfig(const AConfig: TRelayConfig);
    function Start: Boolean;
    procedure Stop;
    procedure FlushNow;

    property Status: TRelaySenderStatus read GetStatus;
    property LastError: string read GetLastError;
    property ActiveSenders: Integer read GetActiveSenders;
  end;

implementation

uses
  System.StrUtils,
  Relay.Crypto,
  IdSMTP,
  IdMessage,
  IdEMailAddress,
  IdExplicitTLSClientServerBase,
  IdSSL,
  IdSSLOpenSSL,
  IdReplySMTP;

{ TRelaySenderWorker }

constructor TRelaySenderWorker.Create(AManager: TRelaySenderManager);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FManager := AManager;
end;

procedure TRelaySenderWorker.Execute;
var
  LItem: TSpoolItem;
  LError: string;
  LPermanent: Boolean;
begin
  while not Terminated do
  begin
    if not FManager.IsRunning then
      Break;

    if FManager.FQueue.TryTake(LItem) then
    begin
      TInterlocked.Increment(FManager.FActiveSenders);
      try
        if FManager.SendOne(LItem, LError, LPermanent) then
          FManager.FQueue.MarkSuccess(LItem)
        else
          FManager.FQueue.MarkFailure(LItem, LError, LPermanent);
      finally
        TInterlocked.Decrement(FManager.FActiveSenders);
      end;
      Continue;
    end;

    FManager.FWakeEvent.WaitFor(1000 + Random(1000));
  end;
end;

{ TRelaySenderManager }

constructor TRelaySenderManager.Create(AQueue: TSpoolQueue; ALogger: TRelayLogger);
begin
  inherited Create;
  FCritSec := TCriticalSection.Create;
  FQueue := AQueue;
  FLogger := ALogger;
  FWorkers := TObjectList<TRelaySenderWorker>.Create(True);
  FWakeEvent := TEvent.Create(nil, False, False, '');
  SetStatus(ssStopped);
end;

destructor TRelaySenderManager.Destroy;
begin
  Stop;
  FWakeEvent.Free;
  FWorkers.Free;
  FCritSec.Free;
  inherited Destroy;
end;

procedure TRelaySenderManager.SetStatus(AStatus: TRelaySenderStatus; const ALastError: string);
begin
  FCritSec.Acquire;
  try
    FStatus := AStatus;
    if ALastError <> '' then
      FLastError := ALastError;
  finally
    FCritSec.Release;
  end;
end;

function TRelaySenderManager.GetStatus: TRelaySenderStatus;
begin
  FCritSec.Acquire;
  try
    Result := FStatus;
  finally
    FCritSec.Release;
  end;
end;

function TRelaySenderManager.GetLastError: string;
begin
  FCritSec.Acquire;
  try
    Result := FLastError;
  finally
    FCritSec.Release;
  end;
end;

function TRelaySenderManager.GetActiveSenders: Integer;
begin
  Result := TInterlocked.CompareExchange(FActiveSenders, 0, 0);
end;

function TRelaySenderManager.IsRunning: Boolean;
begin
  FCritSec.Acquire;
  try
    Result := FRunning;
  finally
    FCritSec.Release;
  end;
end;

procedure TRelaySenderManager.ApplyConfig(const AConfig: TRelayConfig);
begin
  FCritSec.Acquire;
  try
    FHost := AConfig.OutboundHost;
    FPort := AConfig.OutboundPort;
    FTlsMode := AConfig.OutboundTlsMode;
    FWorkerCount := AConfig.OutboundWorkers;
    FConnectTimeoutSec := AConfig.OutboundConnectTimeoutSec;
    FReadTimeoutSec := AConfig.OutboundReadTimeoutSec;
    FWriteTimeoutSec := AConfig.OutboundWriteTimeoutSec;
    FAuthUserEnc := AConfig.OutboundAuthUserEnc;
    FAuthPasswordEnc := AConfig.OutboundAuthPasswordEnc;
  finally
    FCritSec.Release;
  end;
end;

function TRelaySenderManager.Start: Boolean;
var
  I: Integer;
  LWorker: TRelaySenderWorker;
begin
  FCritSec.Acquire;
  try
    if FRunning then
      Exit(True);
    if Trim(FHost) = '' then
    begin
      FStatus := ssError;
      FLastError := 'Outbound host is empty.';
      Exit(False);
    end;

    FRunning := True;
    FLastError := '';
    FWorkers.Clear;
    for I := 1 to FWorkerCount do
    begin
      LWorker := TRelaySenderWorker.Create(Self);
      FWorkers.Add(LWorker);
      LWorker.Start;
    end;
    FStatus := ssRunning;
    Result := True;
    if Assigned(FLogger) then
      FLogger.Add('SENDER', Format('Sender workers started count=%d host=%s:%d', [FWorkerCount, FHost, FPort]));
  finally
    FCritSec.Release;
  end;
end;

procedure TRelaySenderManager.Stop;
var
  LWorker: TRelaySenderWorker;
begin
  FCritSec.Acquire;
  try
    if not FRunning then
    begin
      FStatus := ssStopped;
      Exit;
    end;

    FRunning := False;
    for LWorker in FWorkers do
      LWorker.Terminate;
    for LWorker in FWorkers do
      FWakeEvent.SetEvent;
  finally
    FCritSec.Release;
  end;

  for LWorker in FWorkers do
    LWorker.WaitFor;

  FCritSec.Acquire;
  try
    FWorkers.Clear;
    FStatus := ssStopped;
    if Assigned(FLogger) then
      FLogger.Add('SENDER', 'Sender workers stopped');
  finally
    FCritSec.Release;
  end;
end;

procedure TRelaySenderManager.FlushNow;
var
  I: Integer;
  LForcedCount: Integer;
begin
  FCritSec.Acquire;
  try
    LForcedCount := FQueue.ForceDeferredNow;
    for I := 0 to FWorkerCount - 1 do
      FWakeEvent.SetEvent;
    if Assigned(FLogger) then
      FLogger.Add('SENDER', Format('Flush requested forcedDeferred=%d', [LForcedCount]));
  finally
    FCritSec.Release;
  end;
end;

function TRelaySenderManager.SendOne(const AItem: TSpoolItem; out AErrorText: string;
  out APermanentFailure: Boolean): Boolean;
var
  LHost: string;
  LPort: Integer;
  LTlsMode: TRelayTlsMode;
  LConnectTimeoutSec: Integer;
  LReadTimeoutSec: Integer;
  LWriteTimeoutSec: Integer;
  LAuthUserEnc: string;
  LAuthPasswordEnc: string;
  LAuthUser: string;
  LAuthPassword: string;

  LSMTP: TIdSMTP;
  LMessage: TIdMessage;
  LRecipients: TIdEmailAddressList;
  LStream: TBytesStream;
  LRecipient: string;
begin
  Result := False;
  APermanentFailure := False;
  AErrorText := '';

  FCritSec.Acquire;
  try
    LHost := FHost;
    LPort := FPort;
    LTlsMode := FTlsMode;
    LConnectTimeoutSec := FConnectTimeoutSec;
    LReadTimeoutSec := FReadTimeoutSec;
    LWriteTimeoutSec := FWriteTimeoutSec;
    LAuthUserEnc := FAuthUserEnc;
    LAuthPasswordEnc := FAuthPasswordEnc;
  finally
    FCritSec.Release;
  end;
  LAuthUser := Trim(DecryptFromSingleLine(LAuthUserEnc)).Replace(#13, '').Replace(#10, '');
  LAuthPassword := Trim(DecryptFromSingleLine(LAuthPasswordEnc)).Replace(#13, '').Replace(#10, '');

  LSMTP := TIdSMTP.Create(nil);
  LMessage := TIdMessage.Create(nil);
  LRecipients := TIdEmailAddressList.Create(nil);
  LStream := TBytesStream.Create(AItem.MessageBytes);
  try
    LSMTP.Host := LHost;
    LSMTP.Port := LPort;
    LSMTP.ConnectTimeout := LConnectTimeoutSec * 1000;
    LSMTP.ReadTimeout := LReadTimeoutSec * 1000;

    if LTlsMode <> tmPlain then
    begin
      LSMTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LSMTP);
      LSMTP.UseTLS := utUseExplicitTLS;
      if LTlsMode = tmSslImplicit then
        LSMTP.UseTLS := utUseImplicitTLS;
    end
    else
      LSMTP.UseTLS := utNoTLSSupport;

    // Indy IOHandler in this version exposes ConnectTimeout/ReadTimeout only.
    // Keep WriteTimeout in config for future compatibility and parity with spec.

    if Trim(LAuthUser) <> '' then
    begin
      LSMTP.AuthType := satDefault;
      LSMTP.Username := LAuthUser;
      LSMTP.Password := LAuthPassword;
      if Assigned(FLogger) then
        FLogger.Add('SENDER', Format('SMTP auth enabled user=%s passLen=%d', [LAuthUser, Length(LAuthPassword)]));
    end
    else
    begin
      LSMTP.AuthType := satNone;
      if Assigned(FLogger) then
        FLogger.Add('SENDER', 'SMTP auth disabled (no username configured)');
    end;

    LStream.Position := 0;
    LMessage.LoadFromStream(LStream);

    for LRecipient in AItem.Recipients do
      LRecipients.Add.Address := LRecipient;
    if (LRecipients.Count = 0) and (LMessage.Recipients.Count > 0) then
      LRecipients.Assign(LMessage.Recipients);

    if Assigned(FLogger) then
      FLogger.Add('SENDER', Format('Sending id=%s attempt=%d rcpt=%d writeTimeoutSec=%d',
        [AItem.Id, AItem.Attempt, LRecipients.Count, LWriteTimeoutSec]));

    LSMTP.Connect;
    try
      LSMTP.Send(LMessage, LRecipients, AItem.MailFrom);
    finally
      if LSMTP.Connected then
        LSMTP.Disconnect;
    end;

    if Assigned(FLogger) then
      FLogger.Add('SENDER', 'Send success id=' + AItem.Id);
    Result := True;
  except
    on E: EIdSMTPReplyError do
    begin
      APermanentFailure := (E.ErrorCode >= 500) and (E.ErrorCode <= 599);
      if (E.ErrorCode = 530) or (E.ErrorCode = 535) then
        APermanentFailure := True;
      AErrorText := Format('SMTP %d: %s', [E.ErrorCode, E.Message]);
    end;
    on E: Exception do
    begin
      APermanentFailure := ContainsText(E.Message, 'authentication failed');
      AErrorText := E.Message;
    end;
  end;

  if (not Result) and Assigned(FLogger) then
    FLogger.Add('ERROR', 'Send failed id=' + AItem.Id + ' error=' + AErrorText);

  LStream.Free;
  LRecipients.Free;
  LMessage.Free;
  LSMTP.Free;
end;

end.
