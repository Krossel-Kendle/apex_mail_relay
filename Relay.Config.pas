unit Relay.Config;

interface

uses
  Relay.Types;

type
  TRelayConfig = class
  private
    function IsValidIPv4(const AValue: string): Boolean;
  public
    InboundBindIP: string;
    InboundBindPort: Integer;
    InboundAllowedClientIP: string;
    InboundMaxMessageSizeMB: Integer;
    InboundSessionIdleTimeoutSec: Integer;
    InboundCommandTimeoutSec: Integer;
    InboundAuthUserEnc: string;
    InboundAuthPasswordEnc: string;

    OutboundHost: string;
    OutboundPort: Integer;
    OutboundTlsMode: TRelayTlsMode;
    OutboundWorkers: Integer;
    OutboundConnectTimeoutSec: Integer;
    OutboundReadTimeoutSec: Integer;
    OutboundWriteTimeoutSec: Integer;
    OutboundAuthUserEnc: string;
    OutboundAuthPasswordEnc: string;

    QueuePath: string;
    QueueMaxItems: Integer;
    QueueMaxBytesMB: Integer;
    QueueInFlightStaleSec: Integer;

    RetryMaxAttempts: Integer;
    RetryBaseDelaySec: Integer;
    RetryMaxDelaySec: Integer;
    RetryJitterPct: Integer;

    UILogTailLines: Integer;
    UIStatsRefreshMs: Integer;
    UILanguage: string;
    UIDetailLoggingEnabled: Boolean;

    StateListenerStarted: Boolean;
    StateSendersStarted: Boolean;

    constructor Create;
    procedure SetDefaults;
    procedure LoadFromIni(const AFileName: string);
    procedure SaveToIni(const AFileName: string);
    function Validate(out AError: string): Boolean;
    function ResolveQueuePath(const ABaseDir: string): string;
    procedure SetAuthPlain(const AUser, APassword: string);
    procedure GetAuthPlain(out AUser, APassword: string);
    function AuthConfigured: Boolean;
    class function DefaultIniPath: string; static;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  Relay.Crypto,
  Relay.I18n;

constructor TRelayConfig.Create;
begin
  inherited Create;
  SetDefaults;
end;

class function TRelayConfig.DefaultIniPath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'relay.ini');
end;

procedure TRelayConfig.SetDefaults;
begin
  InboundBindIP := '127.0.0.1';
  InboundBindPort := 2525;
  InboundAllowedClientIP := '127.0.0.1';
  InboundMaxMessageSizeMB := 20;
  InboundSessionIdleTimeoutSec := 60;
  InboundCommandTimeoutSec := 30;
  InboundAuthUserEnc := '';
  InboundAuthPasswordEnc := '';

  OutboundHost := '';
  OutboundPort := 587;
  OutboundTlsMode := tmStartTls;
  OutboundWorkers := 2;
  OutboundConnectTimeoutSec := 10;
  OutboundReadTimeoutSec := 30;
  OutboundWriteTimeoutSec := 30;
  OutboundAuthUserEnc := '';
  OutboundAuthPasswordEnc := '';

  QueuePath := '.\spool';
  QueueMaxItems := 100000;
  QueueMaxBytesMB := 2048;
  QueueInFlightStaleSec := 600;

  RetryMaxAttempts := 12;
  RetryBaseDelaySec := 10;
  RetryMaxDelaySec := 3600;
  RetryJitterPct := 20;

  UILogTailLines := 1000;
  UIStatsRefreshMs := 1000;
  UILanguage := 'EN';
  UIDetailLoggingEnabled := False;

  StateListenerStarted := False;
  StateSendersStarted := False;
end;

function EnsureEncrypted(const AValue: string): string;
var
  LValue: string;
begin
  LValue := Trim(AValue);
  if LValue = '' then
    Exit('');
  if IsEncryptedValue(LValue) then
    Exit(LValue);
  try
    Result := EncryptToSingleLine(LValue);
  except
    Result := '';
  end;
end;

procedure TRelayConfig.LoadFromIni(const AFileName: string);
var
  LIni: TIniFile;
  LTlsMode: TRelayTlsMode;
  LTlsText: string;
begin
  if not TFile.Exists(AFileName) then
    Exit;

  LIni := TIniFile.Create(AFileName);
  try
    InboundBindIP := LIni.ReadString('Inbound', 'BindIP', InboundBindIP);
    InboundBindPort := LIni.ReadInteger('Inbound', 'BindPort', InboundBindPort);
    InboundAllowedClientIP := LIni.ReadString('Inbound', 'AllowedClientIP', InboundAllowedClientIP);
    InboundMaxMessageSizeMB := LIni.ReadInteger('Inbound', 'MaxMessageSizeMB', InboundMaxMessageSizeMB);
    InboundSessionIdleTimeoutSec := LIni.ReadInteger('Inbound', 'SessionIdleTimeoutSec', InboundSessionIdleTimeoutSec);
    InboundCommandTimeoutSec := LIni.ReadInteger('Inbound', 'CommandTimeoutSec', InboundCommandTimeoutSec);
    InboundAuthUserEnc := EnsureEncrypted(LIni.ReadString('Inbound', 'AuthUser', InboundAuthUserEnc));
    InboundAuthPasswordEnc := EnsureEncrypted(LIni.ReadString('Inbound', 'AuthPassword', InboundAuthPasswordEnc));

    OutboundHost := LIni.ReadString('Outbound', 'Host', OutboundHost);
    OutboundPort := LIni.ReadInteger('Outbound', 'Port', OutboundPort);
    LTlsText := LIni.ReadString('Outbound', 'TlsMode', RelayTlsModeToString(OutboundTlsMode));
    if RelayTlsModeFromString(LTlsText, LTlsMode) then
      OutboundTlsMode := LTlsMode;
    OutboundWorkers := LIni.ReadInteger('Outbound', 'Workers', OutboundWorkers);
    OutboundConnectTimeoutSec := LIni.ReadInteger('Outbound', 'ConnectTimeoutSec', OutboundConnectTimeoutSec);
    OutboundReadTimeoutSec := LIni.ReadInteger('Outbound', 'ReadTimeoutSec', OutboundReadTimeoutSec);
    OutboundWriteTimeoutSec := LIni.ReadInteger('Outbound', 'WriteTimeoutSec', OutboundWriteTimeoutSec);
    OutboundAuthUserEnc := EnsureEncrypted(LIni.ReadString('Outbound', 'AuthUser', OutboundAuthUserEnc));
    OutboundAuthPasswordEnc := EnsureEncrypted(LIni.ReadString('Outbound', 'AuthPassword', OutboundAuthPasswordEnc));

    QueuePath := LIni.ReadString('Queue', 'Path', QueuePath);
    QueueMaxItems := LIni.ReadInteger('Queue', 'MaxQueueItems', QueueMaxItems);
    QueueMaxBytesMB := LIni.ReadInteger('Queue', 'MaxQueueBytesMB', QueueMaxBytesMB);
    QueueInFlightStaleSec := LIni.ReadInteger('Queue', 'InFlightStaleSec', QueueInFlightStaleSec);

    RetryMaxAttempts := LIni.ReadInteger('Retry', 'MaxAttempts', RetryMaxAttempts);
    RetryBaseDelaySec := LIni.ReadInteger('Retry', 'BaseDelaySec', RetryBaseDelaySec);
    RetryMaxDelaySec := LIni.ReadInteger('Retry', 'MaxDelaySec', RetryMaxDelaySec);
    RetryJitterPct := LIni.ReadInteger('Retry', 'JitterPct', RetryJitterPct);

    UILogTailLines := LIni.ReadInteger('UI', 'LogTailLines', UILogTailLines);
    UIStatsRefreshMs := LIni.ReadInteger('UI', 'StatsRefreshMs', UIStatsRefreshMs);
    UILanguage := LIni.ReadString('UI', 'Language', UILanguage);
    UIDetailLoggingEnabled := LIni.ReadBool('UI', 'EnableDetailLogging', UIDetailLoggingEnabled);

    StateListenerStarted := LIni.ReadBool('State', 'ListenerStarted', StateListenerStarted);
    StateSendersStarted := LIni.ReadBool('State', 'SendersStarted', StateSendersStarted);
  finally
    LIni.Free;
  end;
end;

procedure TRelayConfig.SaveToIni(const AFileName: string);
var
  LIni: TIniFile;
begin
  LIni := TIniFile.Create(AFileName);
  try
    LIni.WriteString('Inbound', 'BindIP', InboundBindIP);
    LIni.WriteInteger('Inbound', 'BindPort', InboundBindPort);
    LIni.WriteString('Inbound', 'AllowedClientIP', InboundAllowedClientIP);
    LIni.WriteInteger('Inbound', 'MaxMessageSizeMB', InboundMaxMessageSizeMB);
    LIni.WriteInteger('Inbound', 'SessionIdleTimeoutSec', InboundSessionIdleTimeoutSec);
    LIni.WriteInteger('Inbound', 'CommandTimeoutSec', InboundCommandTimeoutSec);
    LIni.WriteString('Inbound', 'AuthUser', InboundAuthUserEnc.Replace(#13, '').Replace(#10, ''));
    LIni.WriteString('Inbound', 'AuthPassword', InboundAuthPasswordEnc.Replace(#13, '').Replace(#10, ''));

    LIni.WriteString('Outbound', 'Host', OutboundHost);
    LIni.WriteInteger('Outbound', 'Port', OutboundPort);
    LIni.WriteString('Outbound', 'TlsMode', RelayTlsModeToString(OutboundTlsMode));
    LIni.WriteInteger('Outbound', 'Workers', OutboundWorkers);
    LIni.WriteInteger('Outbound', 'ConnectTimeoutSec', OutboundConnectTimeoutSec);
    LIni.WriteInteger('Outbound', 'ReadTimeoutSec', OutboundReadTimeoutSec);
    LIni.WriteInteger('Outbound', 'WriteTimeoutSec', OutboundWriteTimeoutSec);
    LIni.WriteString('Outbound', 'AuthUser', OutboundAuthUserEnc.Replace(#13, '').Replace(#10, ''));
    LIni.WriteString('Outbound', 'AuthPassword', OutboundAuthPasswordEnc.Replace(#13, '').Replace(#10, ''));

    LIni.WriteString('Queue', 'Path', QueuePath);
    LIni.WriteInteger('Queue', 'MaxQueueItems', QueueMaxItems);
    LIni.WriteInteger('Queue', 'MaxQueueBytesMB', QueueMaxBytesMB);
    LIni.WriteInteger('Queue', 'InFlightStaleSec', QueueInFlightStaleSec);

    LIni.WriteInteger('Retry', 'MaxAttempts', RetryMaxAttempts);
    LIni.WriteInteger('Retry', 'BaseDelaySec', RetryBaseDelaySec);
    LIni.WriteInteger('Retry', 'MaxDelaySec', RetryMaxDelaySec);
    LIni.WriteInteger('Retry', 'JitterPct', RetryJitterPct);

    LIni.WriteInteger('UI', 'LogTailLines', UILogTailLines);
    LIni.WriteInteger('UI', 'StatsRefreshMs', UIStatsRefreshMs);
    LIni.WriteString('UI', 'Language', UILanguage);
    LIni.WriteBool('UI', 'EnableDetailLogging', UIDetailLoggingEnabled);

    LIni.WriteBool('State', 'ListenerStarted', StateListenerStarted);
    LIni.WriteBool('State', 'SendersStarted', StateSendersStarted);
  finally
    LIni.Free;
  end;
end;

function TRelayConfig.IsValidIPv4(const AValue: string): Boolean;
var
  LParts: TArray<string>;
  LPart: string;
  LNum: Integer;
begin
  Result := False;
  LParts := AValue.Split(['.']);
  if Length(LParts) <> 4 then
    Exit;

  for LPart in LParts do
  begin
    if (LPart = '') or (Length(LPart) > 3) then
      Exit;
    if not TryStrToInt(LPart, LNum) then
      Exit;
    if (LNum < 0) or (LNum > 255) then
      Exit;
  end;

  Result := True;
end;

function TRelayConfig.Validate(out AError: string): Boolean;
var
  LLang: TRelayLanguage;
begin
  AError := '';

  if not IsValidIPv4(InboundBindIP) then
  begin
    AError := 'Inbound.BindIP must be a valid IPv4 address.';
    Exit(False);
  end;

  if not IsValidIPv4(InboundAllowedClientIP) then
  begin
    AError := 'Inbound.AllowedClientIP must be a valid IPv4 address.';
    Exit(False);
  end;

  if (InboundBindPort < 1) or (InboundBindPort > 65535) then
  begin
    AError := 'Inbound.BindPort must be in range 1..65535.';
    Exit(False);
  end;

  if (OutboundPort < 1) or (OutboundPort > 65535) then
  begin
    AError := 'Outbound.Port must be in range 1..65535.';
    Exit(False);
  end;

  if (OutboundWorkers < 1) or (OutboundWorkers > 8) then
  begin
    AError := 'Outbound.Workers must be in range 1..8.';
    Exit(False);
  end;

  if (InboundMaxMessageSizeMB < 1) or (InboundMaxMessageSizeMB > 100) then
  begin
    AError := 'Inbound.MaxMessageSizeMB must be in range 1..100.';
    Exit(False);
  end;

  if QueuePath.Trim = '' then
  begin
    AError := 'Queue.Path cannot be empty.';
    Exit(False);
  end;

  if (QueueMaxItems < 1) then
  begin
    AError := 'Queue.MaxQueueItems must be >= 1.';
    Exit(False);
  end;

  if (QueueMaxBytesMB < 10) then
  begin
    AError := 'Queue.MaxQueueBytesMB must be >= 10.';
    Exit(False);
  end;

  if (RetryMaxAttempts < 1) then
  begin
    AError := 'Retry.MaxAttempts must be >= 1.';
    Exit(False);
  end;

  if (RetryBaseDelaySec < 1) or (RetryMaxDelaySec < RetryBaseDelaySec) then
  begin
    AError := 'Retry delays are invalid.';
    Exit(False);
  end;

  if (RetryJitterPct < 0) or (RetryJitterPct > 90) then
  begin
    AError := 'Retry.JitterPct must be in range 0..90.';
    Exit(False);
  end;

  if UILogTailLines < 100 then
  begin
    AError := 'UI.LogTailLines must be >= 100.';
    Exit(False);
  end;

  if UIStatsRefreshMs < 200 then
  begin
    AError := 'UI.StatsRefreshMs must be >= 200.';
    Exit(False);
  end;

  if not TryRelayLanguageFromCode(UILanguage, LLang) then
  begin
    AError := 'UI.Language must be EN, RU or ID.';
    Exit(False);
  end;

  Result := True;
end;

function TRelayConfig.ResolveQueuePath(const ABaseDir: string): string;
begin
  if TPath.IsPathRooted(QueuePath) then
    Result := QueuePath
  else
    Result := TPath.GetFullPath(TPath.Combine(ABaseDir, QueuePath));
end;

procedure TRelayConfig.SetAuthPlain(const AUser, APassword: string);
var
  LUser: string;
  LPassword: string;
begin
  LUser := Trim(AUser).Replace(#13, '').Replace(#10, '');
  LPassword := Trim(APassword).Replace(#13, '').Replace(#10, '');
  OutboundAuthUserEnc := EncryptToSingleLine(LUser);
  OutboundAuthPasswordEnc := EncryptToSingleLine(LPassword);
end;

procedure TRelayConfig.GetAuthPlain(out AUser, APassword: string);
begin
  AUser := DecryptFromSingleLine(OutboundAuthUserEnc);
  APassword := DecryptFromSingleLine(OutboundAuthPasswordEnc);
end;

function TRelayConfig.AuthConfigured: Boolean;
begin
  Result := (Trim(OutboundAuthUserEnc) <> '') and (Trim(OutboundAuthPasswordEnc) <> '');
end;

end.
