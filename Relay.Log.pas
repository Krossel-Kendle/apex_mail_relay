unit Relay.Log;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs;

type
  TRelayLogger = class
  private
    FCritSec: TCriticalSection;
    FLines: TStringList;
    FMaxLines: Integer;
    FVersion: Int64;
    function BuildLine(const ALevel, AMessage: string): string;
    procedure TrimToMax;
  public
    constructor Create(AMaxLines: Integer);
    destructor Destroy; override;
    procedure SetMaxLines(AMaxLines: Integer);
    procedure Add(const AMessage: string); overload;
    procedure Add(const ALevel, AMessage: string); overload;
    procedure Clear;
    procedure Snapshot(ADest: TStrings);
    function Version: Int64;
  end;

implementation

uses
  System.DateUtils;

constructor TRelayLogger.Create(AMaxLines: Integer);
begin
  inherited Create;
  FCritSec := TCriticalSection.Create;
  FLines := TStringList.Create;
  FLines.LineBreak := sLineBreak;
  FLines.StrictDelimiter := False;
  if AMaxLines < 100 then
    FMaxLines := 100
  else
    FMaxLines := AMaxLines;
end;

destructor TRelayLogger.Destroy;
begin
  FLines.Free;
  FCritSec.Free;
  inherited Destroy;
end;

function TRelayLogger.BuildLine(const ALevel, AMessage: string): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' [' + ALevel + '] ' + AMessage;
end;

procedure TRelayLogger.TrimToMax;
begin
  while FLines.Count > FMaxLines do
    FLines.Delete(0);
end;

procedure TRelayLogger.SetMaxLines(AMaxLines: Integer);
begin
  if AMaxLines < 100 then
    AMaxLines := 100;
  FCritSec.Acquire;
  try
    FMaxLines := AMaxLines;
    TrimToMax;
    Inc(FVersion);
  finally
    FCritSec.Release;
  end;
end;

procedure TRelayLogger.Add(const AMessage: string);
begin
  Add('INFO', AMessage);
end;

procedure TRelayLogger.Add(const ALevel, AMessage: string);
begin
  FCritSec.Acquire;
  try
    FLines.Add(BuildLine(ALevel, AMessage));
    TrimToMax;
    Inc(FVersion);
  finally
    FCritSec.Release;
  end;
end;

procedure TRelayLogger.Clear;
begin
  FCritSec.Acquire;
  try
    FLines.Clear;
    Inc(FVersion);
  finally
    FCritSec.Release;
  end;
end;

procedure TRelayLogger.Snapshot(ADest: TStrings);
begin
  if ADest = nil then
    Exit;
  FCritSec.Acquire;
  try
    ADest.Assign(FLines);
  finally
    FCritSec.Release;
  end;
end;

function TRelayLogger.Version: Int64;
begin
  FCritSec.Acquire;
  try
    Result := FVersion;
  finally
    FCritSec.Release;
  end;
end;

end.
