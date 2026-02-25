unit Relay.Inbound;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Relay.Config,
  Relay.Log,
  Relay.Queue,
  Relay.Types,
  IdContext,
  IdTCPServer,
  IdServerIOHandler,
  IdServerIOHandlerSocket;

type
  TRelayInboundServer = class
  private
    FCritSec: TCriticalSection;
    FServer: TIdTCPServer;
    FQueue: TSpoolQueue;
    FLogger: TRelayLogger;

    FBindIP: string;
    FBindPort: Integer;
    FAllowedClientIP: string;
    FMaxMessageBytes: Int64;
    FSessionIdleTimeoutMs: Integer;
    FCommandTimeoutMs: Integer;
    FMaxSessions: Integer;

    FStatus: TRelayListenerStatus;
    FLastError: string;
    FActiveSessions: Integer;
    FReplyHost: string;
    FEnableDetailLogging: Boolean;
    FRequireAuthCredentials: Boolean;
    FAuthUserEnc: string;
    FAuthPasswordEnc: string;

    procedure SetStatus(AStatus: TRelayListenerStatus; const ALastError: string = '');
    function GetStatus: TRelayListenerStatus;
    function GetLastError: string;
    function GetActiveSessions: Integer;
    function DetailLoggingEnabled: Boolean;
    function IsClientAllowed(const APeerIP: string): Boolean;
    function AuthRequired: Boolean;
    function TryGetConfiguredAuth(out AUser, APassword: string): Boolean;
    function VerifyLoginCredentials(const AUser, APassword: string): Boolean;
    function VerifyCramMd5Credentials(const AUser, ADigestHex, AChallenge: string): Boolean;
    function ProcessAuthCommand(AContext: TIdContext; ASession: TObject; const ALine: string): Boolean;
    function ProcessAuthContinuation(AContext: TIdContext; ASession: TObject; const ALine: string): Boolean;
    function ContextTag(AContext: TIdContext): string;
    procedure LogDetail(AContext: TIdContext; const APrefix, AMessage: string);

    procedure ServerConnect(AContext: TIdContext);
    procedure ServerDisconnect(AContext: TIdContext);
    procedure ServerExecute(AContext: TIdContext);

    procedure SendReply(AContext: TIdContext; const ALine: string);
    function ReceiveData(AContext: TIdContext; out AMessage: TBytes;
      out ARejectCode: Integer; out ARejectText: string): Boolean;
  public
    constructor Create(AQueue: TSpoolQueue; ALogger: TRelayLogger);
    destructor Destroy; override;

    procedure ApplyConfig(const AConfig: TRelayConfig);
    function Start: Boolean;
    procedure Stop;

    property Status: TRelayListenerStatus read GetStatus;
    property LastError: string read GetLastError;
    property ActiveSessions: Integer read GetActiveSessions;
  end;

implementation

uses
  System.NetEncoding,
  System.StrUtils,
  System.DateUtils,
  Relay.Crypto,
  IdGlobal,
  IdHMACMD5,
  IdSocketHandle,
  IdException,
  IdExceptionCore;

type
  TSmtpAuthStage = (asNone, asLoginUser, asLoginPassword, asPlainResponse, asCramMd5Response);

  TSmtpSession = class
  public
    HeloDone: Boolean;
    Authenticated: Boolean;
    AuthStage: TSmtpAuthStage;
    AuthLoginUser: string;
    AuthChallenge: string;
    MailFrom: string;
    Recipients: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure ResetTransaction;
    procedure ResetAuthFlow;
    function RecipientArray: TArray<string>;
  end;

constructor TSmtpSession.Create;
begin
  inherited Create;
  Authenticated := False;
  AuthStage := asNone;
  AuthLoginUser := '';
  AuthChallenge := '';
  Recipients := TStringList.Create;
  Recipients.CaseSensitive := False;
  Recipients.Duplicates := dupIgnore;
  Recipients.Sorted := False;
  ResetTransaction;
end;

destructor TSmtpSession.Destroy;
begin
  Recipients.Free;
  inherited Destroy;
end;

procedure TSmtpSession.ResetTransaction;
begin
  MailFrom := '';
  Recipients.Clear;
end;

procedure TSmtpSession.ResetAuthFlow;
begin
  AuthStage := asNone;
  AuthLoginUser := '';
  AuthChallenge := '';
end;

function TSmtpSession.RecipientArray: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Recipients.Count);
  for I := 0 to Recipients.Count - 1 do
    Result[I] := Recipients[I];
end;

function ExtractAddressFromPath(const ALine, APrefix: string; out AAddress: string): Boolean;
var
  LRaw: string;
  LPos: Integer;
begin
  Result := False;
  AAddress := '';
  if not StartsText(APrefix, ALine) then
    Exit;

  LRaw := Trim(Copy(ALine, Length(APrefix) + 1, MaxInt));
  if LRaw = '' then
    Exit;

  if LRaw[1] = '<' then
  begin
    LPos := Pos('>', LRaw);
    if LPos > 1 then
      AAddress := Copy(LRaw, 2, LPos - 2)
    else
      Exit;
  end
  else
  begin
    LPos := Pos(' ', LRaw);
    if LPos > 0 then
      AAddress := Copy(LRaw, 1, LPos - 1)
    else
      AAddress := LRaw;
  end;

  AAddress := Trim(AAddress);
  Result := AAddress <> '';
end;

function NextToken(var AText: string): string;
var
  LPos: Integer;
begin
  AText := TrimLeft(AText);
  if AText = '' then
    Exit('');

  LPos := Pos(' ', AText);
  if LPos = 0 then
  begin
    Result := AText;
    AText := '';
  end
  else
  begin
    Result := Copy(AText, 1, LPos - 1);
    AText := TrimLeft(Copy(AText, LPos + 1, MaxInt));
  end;
end;

function Base64EncodeNoBreaks(const AValue: string): string;
begin
  Result := TNetEncoding.Base64.EncodeBytesToString(TEncoding.UTF8.GetBytes(AValue))
    .Replace(#13, '').Replace(#10, '');
end;

function TryBase64DecodeUtf8(const AValue: string; out ADecoded: string): Boolean;
var
  LBytes: TBytes;
begin
  ADecoded := '';
  try
    LBytes := TNetEncoding.Base64.DecodeStringToBytes(Trim(AValue));
    ADecoded := TEncoding.UTF8.GetString(LBytes);
    Result := True;
  except
    on Exception do
      Result := False;
  end;
end;

function TryParseAuthPlainPayload(const APayload: string; out AUser, APassword: string): Boolean;
var
  LParts: TArray<string>;
begin
  Result := False;
  AUser := '';
  APassword := '';
  LParts := APayload.Split([#0]);
  if Length(LParts) < 3 then
    Exit;
  AUser := LParts[1];
  APassword := LParts[2];
  Result := True;
end;

function TryParseCramMd5Response(const AResponse: string; out AUser, ADigestHex: string): Boolean;
var
  LTrimmed: string;
  LPos: Integer;
begin
  Result := False;
  AUser := '';
  ADigestHex := '';
  LTrimmed := Trim(AResponse);
  if LTrimmed = '' then
    Exit;
  LPos := Pos(' ', LTrimmed);
  if LPos <= 1 then
    Exit;
  AUser := Trim(Copy(LTrimmed, 1, LPos - 1));
  ADigestHex := LowerCase(Trim(Copy(LTrimmed, LPos + 1, MaxInt)));
  Result := (AUser <> '') and (ADigestHex <> '');
end;

function BuildCramMd5Challenge(const AHost: string): string;
begin
  Result := Format('<%d.%d@%s>', [DateTimeToUnix(Now, False), Random(MaxInt), AHost]);
end;

function ComputeCramMd5Digest(const AChallenge, APassword: string): string;
var
  LHmac: TIdHMACMD5;
  LDigest: TIdBytes;
begin
  LHmac := TIdHMACMD5.Create;
  try
    LHmac.Key := ToBytes(APassword, IndyTextEncoding_UTF8);
    LDigest := LHmac.HashValue(ToBytes(AChallenge, IndyTextEncoding_UTF8));
    Result := LowerCase(ToHex(LDigest));
  finally
    LHmac.Free;
  end;
end;

constructor TRelayInboundServer.Create(AQueue: TSpoolQueue; ALogger: TRelayLogger);
begin
  inherited Create;
  Randomize;
  FCritSec := TCriticalSection.Create;
  FQueue := AQueue;
  FLogger := ALogger;
  FMaxSessions := 20;
  FReplyHost := 'localhost';
  FEnableDetailLogging := False;
  FRequireAuthCredentials := False;
  FAuthUserEnc := '';
  FAuthPasswordEnc := '';
  SetStatus(lsStopped);

  FServer := TIdTCPServer.Create(nil);
  FServer.ReuseSocket := rsTrue;
  FServer.OnConnect := ServerConnect;
  FServer.OnDisconnect := ServerDisconnect;
  FServer.OnExecute := ServerExecute;
end;

destructor TRelayInboundServer.Destroy;
begin
  Stop;
  FServer.Free;
  FCritSec.Free;
  inherited Destroy;
end;

procedure TRelayInboundServer.SetStatus(AStatus: TRelayListenerStatus; const ALastError: string);
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

function TRelayInboundServer.GetStatus: TRelayListenerStatus;
begin
  FCritSec.Acquire;
  try
    Result := FStatus;
  finally
    FCritSec.Release;
  end;
end;

function TRelayInboundServer.GetLastError: string;
begin
  FCritSec.Acquire;
  try
    Result := FLastError;
  finally
    FCritSec.Release;
  end;
end;

function TRelayInboundServer.GetActiveSessions: Integer;
begin
  Result := TInterlocked.CompareExchange(FActiveSessions, 0, 0);
end;

function TRelayInboundServer.DetailLoggingEnabled: Boolean;
begin
  FCritSec.Acquire;
  try
    Result := FEnableDetailLogging;
  finally
    FCritSec.Release;
  end;
end;

function TRelayInboundServer.AuthRequired: Boolean;
begin
  FCritSec.Acquire;
  try
    Result := FRequireAuthCredentials and (Trim(FAuthUserEnc) <> '') and (Trim(FAuthPasswordEnc) <> '');
  finally
    FCritSec.Release;
  end;
end;

function TRelayInboundServer.IsClientAllowed(const APeerIP: string): Boolean;
var
  LAllowedListText: string;
  LAllowedList: TArray<string>;
  LAllowedIP: string;
begin
  Result := False;

  FCritSec.Acquire;
  try
    LAllowedListText := FAllowedClientIP;
  finally
    FCritSec.Release;
  end;

  LAllowedList := LAllowedListText.Replace(';', ',')
    .Split([','], TStringSplitOptions.ExcludeEmpty);
  for LAllowedIP in LAllowedList do
    if SameText(Trim(LAllowedIP), Trim(APeerIP)) then
      Exit(True);
end;

function TRelayInboundServer.TryGetConfiguredAuth(out AUser, APassword: string): Boolean;
var
  LUserEnc: string;
  LPasswordEnc: string;
begin
  AUser := '';
  APassword := '';

  FCritSec.Acquire;
  try
    LUserEnc := FAuthUserEnc;
    LPasswordEnc := FAuthPasswordEnc;
  finally
    FCritSec.Release;
  end;

  if (Trim(LUserEnc) = '') or (Trim(LPasswordEnc) = '') then
    Exit(False);

  try
    AUser := DecryptFromSingleLine(LUserEnc);
    APassword := DecryptFromSingleLine(LPasswordEnc);
  except
    on E: Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Decrypt SMTP AUTH credentials failed: ' + E.Message);
      Exit(False);
    end;
  end;

  Result := (AUser <> '') and (APassword <> '');
end;

function TRelayInboundServer.VerifyLoginCredentials(const AUser, APassword: string): Boolean;
var
  LConfiguredUser: string;
  LConfiguredPassword: string;
begin
  if not AuthRequired then
    Exit(True);
  if not TryGetConfiguredAuth(LConfiguredUser, LConfiguredPassword) then
    Exit(False);
  Result := SameText(AUser, LConfiguredUser) and (APassword = LConfiguredPassword);
end;

function TRelayInboundServer.VerifyCramMd5Credentials(const AUser, ADigestHex, AChallenge: string): Boolean;
var
  LConfiguredUser: string;
  LConfiguredPassword: string;
  LExpectedDigest: string;
begin
  if not AuthRequired then
    Exit(True);
  if not TryGetConfiguredAuth(LConfiguredUser, LConfiguredPassword) then
    Exit(False);
  if not SameText(AUser, LConfiguredUser) then
    Exit(False);
  LExpectedDigest := ComputeCramMd5Digest(AChallenge, LConfiguredPassword);
  Result := SameText(ADigestHex, LExpectedDigest);
end;

function TRelayInboundServer.ProcessAuthCommand(AContext: TIdContext; ASession: TObject;
  const ALine: string): Boolean;
var
  LSession: TSmtpSession;
  LArgs: string;
  LMechanism: string;
  LInitial: string;
  LDecoded: string;
  LUser: string;
  LPassword: string;
begin
  Result := False;
  if not (SameText(Trim(ALine), 'AUTH') or StartsText('AUTH ', UpperCase(ALine))) then
    Exit;

  Result := True;
  if not (ASession is TSmtpSession) then
  begin
    SendReply(AContext, '451 4.3.0 Local processing error');
    Exit;
  end;

  LSession := TSmtpSession(ASession);
  LSession.ResetAuthFlow;

  LArgs := Trim(Copy(ALine, 5, MaxInt));
  if LArgs = '' then
  begin
    SendReply(AContext, '501 5.5.4 Syntax: AUTH <mechanism> [initial-response]');
    Exit;
  end;

  LMechanism := UpperCase(NextToken(LArgs));
  LInitial := Trim(LArgs);

  if LMechanism = 'CRAM-MD5' then
  begin
    LSession.AuthChallenge := BuildCramMd5Challenge(FReplyHost);
    LSession.AuthStage := asCramMd5Response;
    SendReply(AContext, '334 ' + Base64EncodeNoBreaks(LSession.AuthChallenge));
    Exit;
  end;

  if LMechanism = 'LOGIN' then
  begin
    if not AuthRequired then
    begin
      if (LInitial <> '') and (LInitial <> '=') then
        LSession.AuthStage := asLoginPassword
      else
        LSession.AuthStage := asLoginUser;
      if LSession.AuthStage = asLoginUser then
        SendReply(AContext, '334 ' + Base64EncodeNoBreaks('Username:'))
      else
        SendReply(AContext, '334 ' + Base64EncodeNoBreaks('Password:'));
      Exit;
    end;

    if (LInitial <> '') and (LInitial <> '=') then
    begin
      if not TryBase64DecodeUtf8(LInitial, LUser) then
      begin
        SendReply(AContext, '501 5.5.2 Cannot decode AUTH LOGIN username');
        Exit;
      end;
      LSession.AuthLoginUser := LUser;
      LSession.AuthStage := asLoginPassword;
      SendReply(AContext, '334 ' + Base64EncodeNoBreaks('Password:'));
    end
    else
    begin
      LSession.AuthStage := asLoginUser;
      SendReply(AContext, '334 ' + Base64EncodeNoBreaks('Username:'));
    end;
    Exit;
  end;

  if LMechanism = 'PLAIN' then
  begin
    if (LInitial = '') or (LInitial = '=') then
    begin
      LSession.AuthStage := asPlainResponse;
      SendReply(AContext, '334 ');
      Exit;
    end;

    if not AuthRequired then
    begin
      LSession.Authenticated := True;
      SendReply(AContext, '235 2.7.0 Authentication successful');
      LogDetail(AContext, 'EVT:', 'auth success method=PLAIN mode=allow-all');
      Exit;
    end;

    if not TryBase64DecodeUtf8(LInitial, LDecoded) then
    begin
      SendReply(AContext, '501 5.5.2 Cannot decode AUTH PLAIN payload');
      Exit;
    end;

    if not TryParseAuthPlainPayload(LDecoded, LUser, LPassword) then
    begin
      SendReply(AContext, '501 5.5.2 Invalid AUTH PLAIN payload');
      Exit;
    end;

    if VerifyLoginCredentials(LUser, LPassword) then
    begin
      LSession.Authenticated := True;
      SendReply(AContext, '235 2.7.0 Authentication successful');
      LogDetail(AContext, 'EVT:', 'auth success method=PLAIN user=' + LUser);
    end
    else
    begin
      LSession.Authenticated := False;
      SendReply(AContext, '535 5.7.8 Authentication credentials invalid');
      LogDetail(AContext, 'EVT:', 'auth failed method=PLAIN user=' + LUser);
    end;
    Exit;
  end;

  if not AuthRequired then
  begin
    LSession.Authenticated := True;
    SendReply(AContext, '235 2.7.0 Authentication successful');
    LogDetail(AContext, 'EVT:', 'auth success method=' + LMechanism + ' mode=allow-all');
    Exit;
  end;

  SendReply(AContext, '504 5.5.4 Unrecognized authentication type');
end;

function TRelayInboundServer.ProcessAuthContinuation(AContext: TIdContext; ASession: TObject;
  const ALine: string): Boolean;
var
  LSession: TSmtpSession;
  LDecoded: string;
  LUser: string;
  LPassword: string;
  LDigestHex: string;
begin
  Result := False;
  if not (ASession is TSmtpSession) then
    Exit;

  LSession := TSmtpSession(ASession);
  if LSession.AuthStage = asNone then
    Exit;

  Result := True;

  if Trim(ALine) = '*' then
  begin
    LSession.ResetAuthFlow;
    SendReply(AContext, '501 5.7.0 Authentication canceled');
    Exit;
  end;

  case LSession.AuthStage of
    asLoginUser:
      begin
        if not AuthRequired then
        begin
          LSession.AuthStage := asLoginPassword;
          SendReply(AContext, '334 ' + Base64EncodeNoBreaks('Password:'));
          Exit;
        end;

        if not TryBase64DecodeUtf8(ALine, LUser) then
        begin
          LSession.ResetAuthFlow;
          SendReply(AContext, '501 5.5.2 Cannot decode AUTH LOGIN username');
          Exit;
        end;
        LSession.AuthLoginUser := LUser;
        LSession.AuthStage := asLoginPassword;
        SendReply(AContext, '334 ' + Base64EncodeNoBreaks('Password:'));
      end;
    asLoginPassword:
      begin
        if not AuthRequired then
        begin
          LSession.Authenticated := True;
          LSession.ResetAuthFlow;
          SendReply(AContext, '235 2.7.0 Authentication successful');
          LogDetail(AContext, 'EVT:', 'auth success method=LOGIN mode=allow-all');
          Exit;
        end;

        if not TryBase64DecodeUtf8(ALine, LPassword) then
        begin
          LSession.ResetAuthFlow;
          SendReply(AContext, '501 5.5.2 Cannot decode AUTH LOGIN password');
          Exit;
        end;

        if VerifyLoginCredentials(LSession.AuthLoginUser, LPassword) then
        begin
          LSession.Authenticated := True;
          SendReply(AContext, '235 2.7.0 Authentication successful');
          LogDetail(AContext, 'EVT:', 'auth success method=LOGIN user=' + LSession.AuthLoginUser);
        end
        else
        begin
          LSession.Authenticated := False;
          SendReply(AContext, '535 5.7.8 Authentication credentials invalid');
          LogDetail(AContext, 'EVT:', 'auth failed method=LOGIN user=' + LSession.AuthLoginUser);
        end;
        LSession.ResetAuthFlow;
      end;
    asPlainResponse:
      begin
        if not AuthRequired then
        begin
          LSession.Authenticated := True;
          LSession.ResetAuthFlow;
          SendReply(AContext, '235 2.7.0 Authentication successful');
          LogDetail(AContext, 'EVT:', 'auth success method=PLAIN mode=allow-all');
          Exit;
        end;

        if not TryBase64DecodeUtf8(ALine, LDecoded) then
        begin
          LSession.ResetAuthFlow;
          SendReply(AContext, '501 5.5.2 Cannot decode AUTH PLAIN payload');
          Exit;
        end;

        if not TryParseAuthPlainPayload(LDecoded, LUser, LPassword) then
        begin
          LSession.ResetAuthFlow;
          SendReply(AContext, '501 5.5.2 Invalid AUTH PLAIN payload');
          Exit;
        end;

        if VerifyLoginCredentials(LUser, LPassword) then
        begin
          LSession.Authenticated := True;
          SendReply(AContext, '235 2.7.0 Authentication successful');
          LogDetail(AContext, 'EVT:', 'auth success method=PLAIN user=' + LUser);
        end
        else
        begin
          LSession.Authenticated := False;
          SendReply(AContext, '535 5.7.8 Authentication credentials invalid');
          LogDetail(AContext, 'EVT:', 'auth failed method=PLAIN user=' + LUser);
        end;
        LSession.ResetAuthFlow;
      end;
    asCramMd5Response:
      begin
        if not AuthRequired then
        begin
          LSession.Authenticated := True;
          LSession.ResetAuthFlow;
          SendReply(AContext, '235 2.7.0 Authentication successful');
          LogDetail(AContext, 'EVT:', 'auth success method=CRAM-MD5 mode=allow-all');
          Exit;
        end;

        if not TryBase64DecodeUtf8(ALine, LDecoded) then
        begin
          LSession.ResetAuthFlow;
          SendReply(AContext, '501 5.5.2 Cannot decode AUTH CRAM-MD5 response');
          Exit;
        end;

        if not TryParseCramMd5Response(LDecoded, LUser, LDigestHex) then
        begin
          LSession.ResetAuthFlow;
          SendReply(AContext, '501 5.5.2 Invalid AUTH CRAM-MD5 response');
          Exit;
        end;

        if VerifyCramMd5Credentials(LUser, LDigestHex, LSession.AuthChallenge) then
        begin
          LSession.Authenticated := True;
          SendReply(AContext, '235 2.7.0 Authentication successful');
          LogDetail(AContext, 'EVT:', 'auth success method=CRAM-MD5 user=' + LUser);
        end
        else
        begin
          LSession.Authenticated := False;
          SendReply(AContext, '535 5.7.8 Authentication credentials invalid');
          LogDetail(AContext, 'EVT:', 'auth failed method=CRAM-MD5 user=' + LUser);
        end;
        LSession.ResetAuthFlow;
      end;
  else
    begin
      LSession.ResetAuthFlow;
      SendReply(AContext, '501 5.5.1 Invalid authentication state');
    end;
  end;
end;

function TRelayInboundServer.ContextTag(AContext: TIdContext): string;
var
  LIP: string;
  LPort: string;
begin
  Result := 'unknown';
  if not Assigned(AContext) then
    Exit;
  try
    LIP := AContext.Binding.PeerIP;
    LPort := IntToStr(AContext.Binding.PeerPort);
    Result := LIP + ':' + LPort;
  except
    on Exception do
      Result := 'unknown';
  end;
end;

procedure TRelayInboundServer.LogDetail(AContext: TIdContext; const APrefix, AMessage: string);
begin
  if (not DetailLoggingEnabled) or (not Assigned(FLogger)) then
    Exit;
  FLogger.Add('SMTP', Format('%s %s %s', [APrefix, ContextTag(AContext), AMessage]));
end;

procedure TRelayInboundServer.ApplyConfig(const AConfig: TRelayConfig);
begin
  FCritSec.Acquire;
  try
    FBindIP := AConfig.InboundBindIP;
    FBindPort := AConfig.InboundBindPort;
    FAllowedClientIP := AConfig.InboundAllowedClientIP;
    FMaxMessageBytes := Int64(AConfig.InboundMaxMessageSizeMB) * 1024 * 1024;
    FSessionIdleTimeoutMs := AConfig.InboundSessionIdleTimeoutSec * 1000;
    FCommandTimeoutMs := AConfig.InboundCommandTimeoutSec * 1000;
    FEnableDetailLogging := AConfig.UIDetailLoggingEnabled;
    FRequireAuthCredentials := AConfig.InboundRequireAuthCredentials;
    FAuthUserEnc := AConfig.InboundAuthUserEnc;
    FAuthPasswordEnc := AConfig.InboundAuthPasswordEnc;
  finally
    FCritSec.Release;
  end;
end;

function TRelayInboundServer.Start: Boolean;
begin
  FCritSec.Acquire;
  try
    if FServer.Active then
    begin
      Result := True;
      Exit;
    end;

    FLastError := '';
    FServer.Bindings.Clear;
    with FServer.Bindings.Add do
    begin
      IP := FBindIP;
      Port := FBindPort;
    end;
    FServer.DefaultPort := FBindPort;
    FServer.MaxConnections := FMaxSessions;
    FServer.Active := True;
    FStatus := lsListening;
    Result := True;
    if Assigned(FLogger) then
      FLogger.Add('INBOUND', Format('Listener started at %s:%d allowed=%s',
        [FBindIP, FBindPort, FAllowedClientIP]));
  except
    on E: Exception do
    begin
      FStatus := lsError;
      FLastError := E.Message;
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Listener start failed: ' + E.Message);
      Result := False;
    end;
  end;
  FCritSec.Release;
end;

procedure TRelayInboundServer.Stop;
begin
  FCritSec.Acquire;
  try
    if FServer.Active then
      FServer.Active := False;
    FStatus := lsStopped;
    if Assigned(FLogger) then
      FLogger.Add('INBOUND', 'Listener stopped');
  except
    on E: Exception do
    begin
      FStatus := lsError;
      FLastError := E.Message;
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Listener stop failed: ' + E.Message);
    end;
  end;
  FCritSec.Release;
end;

procedure TRelayInboundServer.SendReply(AContext: TIdContext; const ALine: string);
begin
  try
    AContext.Connection.IOHandler.WriteLn(ALine, IndyTextEncoding_ASCII);
    LogDetail(AContext, 'S:', ALine);
  except
    on E: Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Send reply failed: ' + E.Message);
      raise;
    end;
  end;
end;

function TRelayInboundServer.ReceiveData(AContext: TIdContext; out AMessage: TBytes;
  out ARejectCode: Integer; out ARejectText: string): Boolean;
var
  LBuffer: TBytesStream;
  LLine: string;
  LBytes: TBytes;
  LOversized: Boolean;
begin
  ARejectCode := 451;
  ARejectText := '4.3.0 Local processing error';
  SetLength(AMessage, 0);

  LBuffer := TBytesStream.Create;
  try
    LOversized := False;
    while AContext.Connection.Connected do
    begin
      LLine := AContext.Connection.IOHandler.ReadLn('', FCommandTimeoutMs, -1, IndyTextEncoding_8Bit);
      LogDetail(AContext, 'C-DATA:', LLine);
      if LLine = '.' then
        Break;
      if StartsText('..', LLine) then
        Delete(LLine, 1, 1);

      LBytes := TEncoding.UTF8.GetBytes(LLine + #13#10);
      if not LOversized then
      begin
        if (LBuffer.Size + Length(LBytes)) > FMaxMessageBytes then
          LOversized := True
        else if Length(LBytes) > 0 then
          LBuffer.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    end;

    if LOversized then
    begin
      ARejectCode := 552;
      ARejectText := '5.3.4 Message size exceeds fixed maximum message size';
      Exit(False);
    end;

    if LBuffer.Size > 0 then
    begin
      SetLength(AMessage, LBuffer.Size);
      Move(LBuffer.Memory^, AMessage[0], LBuffer.Size);
    end;
    Result := True;
  finally
    LBuffer.Free;
  end;
end;

procedure TRelayInboundServer.ServerConnect(AContext: TIdContext);
var
  LSession: TSmtpSession;
  LPeerIP: string;
begin
  try
    LPeerIP := AContext.Binding.PeerIP;

    if not IsClientAllowed(LPeerIP) then
    begin
      SendReply(AContext, '554 5.7.1 Access denied');
      if Assigned(FLogger) then
        FLogger.Add('WARN', Format('Denied connection from %s', [LPeerIP]));
      AContext.Connection.Disconnect;
      Exit;
    end;

    if TInterlocked.CompareExchange(FActiveSessions, 0, 0) >= FMaxSessions then
    begin
      SendReply(AContext, '421 4.3.2 Service not available');
      if Assigned(FLogger) then
        FLogger.Add('WARN', 'Connection rejected due to max sessions');
      AContext.Connection.Disconnect;
      Exit;
    end;

    LSession := TSmtpSession.Create;
    AContext.Data := LSession;
    TInterlocked.Increment(FActiveSessions);

    AContext.Connection.IOHandler.ReadTimeout := FSessionIdleTimeoutMs;
    SendReply(AContext, '220 ' + FReplyHost + ' ESMTP APEX-Relay');
    if Assigned(FLogger) then
      FLogger.Add('INBOUND', Format('Accepted connection from %s', [LPeerIP]));
    LogDetail(AContext, 'EVT:', 'session accepted');
  except
    on E: Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Connect handling failed: ' + E.Message);
      LogDetail(AContext, 'EVT:', 'connect handling failed: ' + E.Message);
      AContext.Connection.Disconnect;
    end;
  end;
end;

procedure TRelayInboundServer.ServerDisconnect(AContext: TIdContext);
var
  LSession: TSmtpSession;
begin
  LogDetail(AContext, 'EVT:', 'session disconnected');
  if Assigned(AContext.Data) then
  begin
    LSession := TSmtpSession(AContext.Data);
    AContext.Data := nil;
    LSession.Free;
    TInterlocked.Decrement(FActiveSessions);
  end;
end;

procedure TRelayInboundServer.ServerExecute(AContext: TIdContext);
var
  LSession: TSmtpSession;
  LLine: string;
  LUpper: string;
  LAddress: string;
  LMessageBytes: TBytes;
  LSmtpCode: Integer;
  LReplyText: string;
  LQueued: Boolean;
begin
  LSession := TSmtpSession(AContext.Data);
  if not Assigned(LSession) then
  begin
    LogDetail(AContext, 'EVT:', 'session has no context data');
    AContext.Connection.Disconnect;
    Exit;
  end;

  try
    LLine := AContext.Connection.IOHandler.ReadLn('', FSessionIdleTimeoutMs, -1, IndyTextEncoding_8Bit);
    LogDetail(AContext, 'C:', LLine);
  except
    on E: EIdReadTimeout do
    begin
      SendReply(AContext, '421 4.4.2 Idle timeout');
      LogDetail(AContext, 'EVT:', 'idle timeout');
      AContext.Connection.Disconnect;
      Exit;
    end;
    on E: Exception do
    begin
      if Assigned(FLogger) then
        FLogger.Add('ERROR', 'Read command failed: ' + E.Message);
      LogDetail(AContext, 'EVT:', 'read command failed: ' + E.Message);
      AContext.Connection.Disconnect;
      Exit;
    end;
  end;

  if ProcessAuthContinuation(AContext, LSession, LLine) then
    Exit;

  if LLine = '' then
  begin
    SendReply(AContext, '500 5.5.2 Empty command');
    Exit;
  end;

  LUpper := UpperCase(LLine);

  if StartsText('EHLO', LUpper) then
  begin
    LSession.HeloDone := True;
    SendReply(AContext, '250-' + FReplyHost);
    SendReply(AContext, '250-SIZE ' + IntToStr(FMaxMessageBytes));
    SendReply(AContext, '250-AUTH CRAM-MD5 LOGIN PLAIN');
    SendReply(AContext, '250-8BITMIME');
    SendReply(AContext, '250 PIPELINING');
    Exit;
  end;

  if StartsText('HELO', LUpper) then
  begin
    LSession.HeloDone := True;
    SendReply(AContext, '250 ' + FReplyHost);
    Exit;
  end;

  if StartsText('RSET', LUpper) then
  begin
    LSession.ResetTransaction;
    SendReply(AContext, '250 2.0.0 OK');
    Exit;
  end;

  if StartsText('NOOP', LUpper) then
  begin
    SendReply(AContext, '250 2.0.0 OK');
    Exit;
  end;

  if StartsText('QUIT', LUpper) then
  begin
    SendReply(AContext, '221 2.0.0 Bye');
    AContext.Connection.Disconnect;
    Exit;
  end;

  if ProcessAuthCommand(AContext, LSession, LLine) then
    Exit;

  if StartsText('MAIL FROM:', LUpper) then
  begin
    if not LSession.HeloDone then
    begin
      SendReply(AContext, '503 5.5.1 Send HELO/EHLO first');
      Exit;
    end;
    if AuthRequired and (not LSession.Authenticated) then
    begin
      SendReply(AContext, '530 5.7.0 Authentication required');
      Exit;
    end;
    if not ExtractAddressFromPath(LLine, 'MAIL FROM:', LAddress) then
    begin
      SendReply(AContext, '501 5.5.4 Invalid MAIL FROM');
      Exit;
    end;
    LSession.ResetTransaction;
    LSession.MailFrom := LAddress;
    SendReply(AContext, '250 2.1.0 Sender OK');
    Exit;
  end;

  if StartsText('RCPT TO:', LUpper) then
  begin
    if LSession.MailFrom = '' then
    begin
      SendReply(AContext, '503 5.5.1 Need MAIL FROM first');
      Exit;
    end;
    if not ExtractAddressFromPath(LLine, 'RCPT TO:', LAddress) then
    begin
      SendReply(AContext, '501 5.5.4 Invalid RCPT TO');
      Exit;
    end;
    LSession.Recipients.Add(LAddress);
    SendReply(AContext, '250 2.1.5 Recipient OK');
    Exit;
  end;

  if StartsText('DATA', LUpper) then
  begin
    if LSession.Recipients.Count = 0 then
    begin
      SendReply(AContext, '503 5.5.1 Need RCPT TO first');
      Exit;
    end;

    SendReply(AContext, '354 End data with <CRLF>.<CRLF>');
    if not ReceiveData(AContext, LMessageBytes, LSmtpCode, LReplyText) then
    begin
      SendReply(AContext, IntToStr(LSmtpCode) + ' ' + LReplyText);
      LSession.ResetTransaction;
      Exit;
    end;

    LQueued := FQueue.Enqueue(LMessageBytes, LSession.MailFrom, LSession.RecipientArray, LSmtpCode, LReplyText);
    if LQueued then
      SendReply(AContext, '250 ' + LReplyText)
    else
      SendReply(AContext, IntToStr(LSmtpCode) + ' ' + LReplyText);

    LSession.ResetTransaction;
    Exit;
  end;

  SendReply(AContext, '502 5.5.1 Command not implemented');
end;

end.
