unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Clipbrd,
  Relay.Config,
  Relay.Log,
  Relay.Queue,
  Relay.Inbound,
  Relay.Sender,
  Relay.Types,
  Relay.I18n;

type
  TForm1 = class(TForm)
  private
    FConfig: TRelayConfig;
    FLogger: TRelayLogger;
    FQueue: TSpoolQueue;
    FInbound: TRelayInboundServer;
    FSenders: TRelaySenderManager;
    FIniPath: string;
    FUiTimer: TTimer;
    FLastLogVersion: Int64;
    FCurrentLanguage: TRelayLanguage;

    GInbound: TGroupBox;
    LblBindIP: TLabel;
    LblBindPort: TLabel;
    LblAllowedIP: TLabel;
    LblMaxMessageMB: TLabel;
    LblInboundStatusCaption: TLabel;
    LblInboundSessionsCaption: TLabel;
    EdBindIP: TEdit;
    EdBindPort: TEdit;
    EdAllowedIP: TEdit;
    EdMaxMessageMB: TEdit;
    BtnStartListener: TButton;
    BtnStopListener: TButton;
    LblInboundStatusValue: TLabel;
    LblInboundSessionsValue: TLabel;

    GOutbound: TGroupBox;
    LblOutHost: TLabel;
    LblOutPort: TLabel;
    LblTlsMode: TLabel;
    LblWorkers: TLabel;
    LblSenderStatusCaption: TLabel;
    LblActiveSendersCaption: TLabel;
    EdOutHost: TEdit;
    EdOutPort: TEdit;
    CbTlsMode: TComboBox;
    EdWorkers: TEdit;
    BtnStartSenders: TButton;
    BtnStopSenders: TButton;
    BtnFlush: TButton;
    BtnEditCreds: TButton;
    LblSenderStatusValue: TLabel;
    LblActiveSendersValue: TLabel;

    PCreds: TPanel;
    LblCredTitle: TLabel;
    LblCredUser: TLabel;
    LblCredPassword: TLabel;
    EdCredUser: TEdit;
    EdCredPassword: TEdit;
    BtnCredSave: TButton;
    BtnCredCancel: TButton;

    GQueue: TGroupBox;
    LblQueuePathCaption: TLabel;
    LblQueueSizeCaption: TLabel;
    LblInflightCaption: TLabel;
    LblDeferredCaption: TLabel;
    LblDeadCaption: TLabel;
    LblOldestCaption: TLabel;
    EdQueuePath: TEdit;
    LblQueueSizeValue: TLabel;
    LblInflightValue: TLabel;
    LblDeferredValue: TLabel;
    LblDeadValue: TLabel;
    LblOldestValue: TLabel;
    BtnPurgeDead: TButton;

    GConfig: TGroupBox;
    LblLanguageCaption: TLabel;
    CbLanguage: TComboBox;
    CbDetailLogging: TCheckBox;
    BtnSaveConfig: TButton;
    BtnReloadConfig: TButton;
    BtnRestoreDefaults: TButton;

    GLogs: TGroupBox;
    MemoLog: TMemo;
    BtnClearLog: TButton;
    BtnCopyLog: TButton;

    function AddLabel(AParent: TWinControl; const ACaption: string; ALeft, ATop: Integer): TLabel;
    function AddEdit(AParent: TWinControl; const AText: string; ALeft, ATop, AWidth: Integer): TEdit;
    function AddButton(AParent: TWinControl; const ACaption: string; ALeft, ATop, AWidth: Integer;
      AOnClick: TNotifyEvent): TButton;
    procedure BuildUi;
    procedure ApplyLanguage;
    function Tx(const AKey: string): string;
    procedure SetHint(AControl: TControl; const AHintText: string);
    procedure SetLanguageFromConfig;
    function SelectedLanguageCode: string;
    procedure ShowCredentialsPanel(AShow: Boolean);
    procedure PersistRuntimeState;
    procedure AutoStartByState;

    procedure LoadConfigToUi;
    function SaveUiToConfig: Boolean;
    procedure ApplyConfigToServices;
    procedure UpdateUi;
    function ListenerStatusText: string;
    function SenderStatusText: string;

    procedure OnUiTimer(Sender: TObject);
    procedure OnStartListener(Sender: TObject);
    procedure OnStopListener(Sender: TObject);
    procedure OnStartSenders(Sender: TObject);
    procedure OnStopSenders(Sender: TObject);
    procedure OnFlush(Sender: TObject);
    procedure OnEditCreds(Sender: TObject);
    procedure OnCredSave(Sender: TObject);
    procedure OnCredCancel(Sender: TObject);
    procedure OnPurgeDead(Sender: TObject);
    procedure OnClearLog(Sender: TObject);
    procedure OnCopyLog(Sender: TObject);
    procedure OnDetailLoggingClick(Sender: TObject);
    procedure OnSaveConfig(Sender: TObject);
    procedure OnReloadConfig(Sender: TObject);
    procedure OnRestoreDefaults(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.AddLabel(AParent: TWinControl; const ACaption: string; ALeft, ATop: Integer): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.Left := ALeft;
  Result.Top := ATop;
end;

function TForm1.AddEdit(AParent: TWinControl; const AText: string; ALeft, ATop, AWidth: Integer): TEdit;
begin
  Result := TEdit.Create(Self);
  Result.Parent := AParent;
  Result.Text := AText;
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
end;

function TForm1.AddButton(AParent: TWinControl; const ACaption: string; ALeft, ATop, AWidth: Integer;
  AOnClick: TNotifyEvent): TButton;
begin
  Result := TButton.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.OnClick := AOnClick;
end;

function TForm1.Tx(const AKey: string): string;
begin
  Result := Tr(FCurrentLanguage, AKey);
end;

procedure TForm1.SetHint(AControl: TControl; const AHintText: string);
begin
  if not Assigned(AControl) then
    Exit;
  AControl.ShowHint := True;
  AControl.Hint := AHintText;
end;

procedure TForm1.SetLanguageFromConfig;
begin
  if not TryRelayLanguageFromCode(FConfig.UILanguage, FCurrentLanguage) then
  begin
    FCurrentLanguage := rlEn;
    FConfig.UILanguage := 'EN';
  end;
end;

function TForm1.SelectedLanguageCode: string;
begin
  case CbLanguage.ItemIndex of
    1:
      Result := 'RU';
    2:
      Result := 'ID';
  else
    Result := 'EN';
  end;
end;

constructor TForm1.Create(AOwner: TComponent);
var
  LValidationError: string;
begin
  inherited Create(AOwner);
  Position := poScreenCenter;
  Width := 1180;
  Height := 790;
  Application.ShowHint := True;
  ShowHint := True;

  FIniPath := TRelayConfig.DefaultIniPath;
  FConfig := TRelayConfig.Create;
  FConfig.LoadFromIni(FIniPath);
  if not FConfig.Validate(LValidationError) then
    FConfig.SetDefaults;
  SetLanguageFromConfig;

  FLogger := TRelayLogger.Create(FConfig.UILogTailLines);
  FQueue := TSpoolQueue.Create(FConfig, FLogger);
  FInbound := TRelayInboundServer.Create(FQueue, FLogger);
  FSenders := TRelaySenderManager.Create(FQueue, FLogger);
  ApplyConfigToServices;

  BuildUi;
  LoadConfigToUi;
  ApplyLanguage;

  FUiTimer := TTimer.Create(Self);
  FUiTimer.Interval := FConfig.UIStatsRefreshMs;
  FUiTimer.OnTimer := OnUiTimer;
  FUiTimer.Enabled := True;

  FLogger.Add('CONFIG', 'Using INI: ' + FIniPath);
  FLogger.Add('INFO', 'Application started');
  AutoStartByState;
  UpdateUi;
end;

destructor TForm1.Destroy;
begin
  if Assigned(FUiTimer) then
    FUiTimer.Enabled := False;

  PersistRuntimeState;

  if Assigned(FSenders) then
    FSenders.Stop;
  if Assigned(FInbound) then
    FInbound.Stop;

  FSenders.Free;
  FInbound.Free;
  FQueue.Free;
  FLogger.Free;
  FConfig.Free;
  inherited Destroy;
end;

procedure TForm1.BuildUi;
begin
  Caption := 'MailRelay';

  GInbound := TGroupBox.Create(Self);
  GInbound.Parent := Self;
  GInbound.SetBounds(8, 8, 365, 250);
  LblBindIP := AddLabel(GInbound, '', 12, 28);
  EdBindIP := AddEdit(GInbound, '', 150, 24, 190);
  LblBindPort := AddLabel(GInbound, '', 12, 58);
  EdBindPort := AddEdit(GInbound, '', 150, 54, 190);
  LblAllowedIP := AddLabel(GInbound, '', 12, 88);
  EdAllowedIP := AddEdit(GInbound, '', 150, 84, 190);
  LblMaxMessageMB := AddLabel(GInbound, '', 12, 118);
  EdMaxMessageMB := AddEdit(GInbound, '', 150, 114, 190);
  BtnStartListener := AddButton(GInbound, '', 12, 150, 150, OnStartListener);
  BtnStopListener := AddButton(GInbound, '', 190, 150, 150, OnStopListener);
  LblInboundStatusCaption := AddLabel(GInbound, '', 12, 198);
  LblInboundStatusValue := AddLabel(GInbound, '-', 150, 198);
  LblInboundSessionsCaption := AddLabel(GInbound, '', 12, 218);
  LblInboundSessionsValue := AddLabel(GInbound, '0', 150, 218);

  GOutbound := TGroupBox.Create(Self);
  GOutbound.Parent := Self;
  GOutbound.SetBounds(380, 8, 365, 250);
  LblOutHost := AddLabel(GOutbound, '', 12, 28);
  EdOutHost := AddEdit(GOutbound, '', 150, 24, 190);
  LblOutPort := AddLabel(GOutbound, '', 12, 58);
  EdOutPort := AddEdit(GOutbound, '', 150, 54, 190);
  LblTlsMode := AddLabel(GOutbound, '', 12, 88);
  CbTlsMode := TComboBox.Create(Self);
  CbTlsMode.Parent := GOutbound;
  CbTlsMode.Left := 150;
  CbTlsMode.Top := 84;
  CbTlsMode.Width := 190;
  CbTlsMode.Style := csDropDownList;
  CbTlsMode.Items.Add('PLAIN');
  CbTlsMode.Items.Add('STARTTLS');
  CbTlsMode.Items.Add('SSL_IMPLICIT');
  LblWorkers := AddLabel(GOutbound, '', 12, 118);
  EdWorkers := AddEdit(GOutbound, '', 150, 114, 190);
  BtnStartSenders := AddButton(GOutbound, '', 12, 150, 105, OnStartSenders);
  BtnStopSenders := AddButton(GOutbound, '', 125, 150, 105, OnStopSenders);
  BtnFlush := AddButton(GOutbound, '', 238, 150, 102, OnFlush);
  BtnEditCreds := AddButton(GOutbound, '', 12, 178, 160, OnEditCreds);
  LblSenderStatusCaption := AddLabel(GOutbound, '', 12, 206);
  LblSenderStatusValue := AddLabel(GOutbound, '-', 150, 206);
  LblActiveSendersCaption := AddLabel(GOutbound, '', 12, 224);
  LblActiveSendersValue := AddLabel(GOutbound, '0', 150, 224);

  PCreds := TPanel.Create(Self);
  PCreds.Parent := GOutbound;
  PCreds.SetBounds(8, 20, 348, 190);
  PCreds.BevelOuter := bvRaised;
  PCreds.Visible := False;
  LblCredTitle := AddLabel(PCreds, '', 10, 10);
  LblCredTitle.Font.Style := [fsBold];
  LblCredUser := AddLabel(PCreds, '', 10, 44);
  EdCredUser := AddEdit(PCreds, '', 120, 40, 210);
  LblCredPassword := AddLabel(PCreds, '', 10, 74);
  EdCredPassword := AddEdit(PCreds, '', 120, 70, 210);
  EdCredPassword.PasswordChar := '*';
  BtnCredSave := AddButton(PCreds, '', 120, 110, 95, OnCredSave);
  BtnCredCancel := AddButton(PCreds, '', 235, 110, 95, OnCredCancel);

  GQueue := TGroupBox.Create(Self);
  GQueue.Parent := Self;
  GQueue.SetBounds(752, 8, 400, 250);
  LblQueuePathCaption := AddLabel(GQueue, '', 12, 28);
  EdQueuePath := AddEdit(GQueue, '', 90, 24, 295);
  LblQueueSizeCaption := AddLabel(GQueue, '', 12, 60);
  LblQueueSizeValue := AddLabel(GQueue, '0', 120, 60);
  LblInflightCaption := AddLabel(GQueue, '', 12, 82);
  LblInflightValue := AddLabel(GQueue, '0', 120, 82);
  LblDeferredCaption := AddLabel(GQueue, '', 12, 104);
  LblDeferredValue := AddLabel(GQueue, '0', 120, 104);
  LblDeadCaption := AddLabel(GQueue, '', 12, 126);
  LblDeadValue := AddLabel(GQueue, '0', 120, 126);
  LblOldestCaption := AddLabel(GQueue, '', 12, 148);
  LblOldestValue := AddLabel(GQueue, '0', 120, 148);
  BtnPurgeDead := AddButton(GQueue, '', 12, 176, 180, OnPurgeDead);

  GConfig := TGroupBox.Create(Self);
  GConfig.Parent := Self;
  GConfig.SetBounds(752, 264, 400, 110);
  LblLanguageCaption := AddLabel(GConfig, '', 12, 26);
  CbLanguage := TComboBox.Create(Self);
  CbLanguage.Parent := GConfig;
  CbLanguage.Left := 90;
  CbLanguage.Top := 22;
  CbLanguage.Width := 180;
  CbLanguage.Style := csDropDownList;
  CbDetailLogging := TCheckBox.Create(Self);
  CbDetailLogging.Parent := GConfig;
  CbDetailLogging.Left := 280;
  CbDetailLogging.Top := 24;
  CbDetailLogging.Width := 110;
  CbDetailLogging.OnClick := OnDetailLoggingClick;
  BtnSaveConfig := AddButton(GConfig, '', 12, 64, 90, OnSaveConfig);
  BtnReloadConfig := AddButton(GConfig, '', 112, 64, 90, OnReloadConfig);
  BtnRestoreDefaults := AddButton(GConfig, '', 212, 64, 130, OnRestoreDefaults);

  GLogs := TGroupBox.Create(Self);
  GLogs.Parent := Self;
  GLogs.SetBounds(8, 264, 738, 470);
  MemoLog := TMemo.Create(Self);
  MemoLog.Parent := GLogs;
  MemoLog.ReadOnly := True;
  MemoLog.ScrollBars := ssVertical;
  MemoLog.WordWrap := False;
  MemoLog.Font.Name := 'Consolas';
  MemoLog.SetBounds(12, 24, 710, 400);
  BtnClearLog := AddButton(GLogs, '', 12, 432, 90, OnClearLog);
  BtnCopyLog := AddButton(GLogs, '', 110, 432, 90, OnCopyLog);
end;

procedure TForm1.ApplyLanguage;
var
  LLang: TRelayLanguage;
begin
  Caption := Tx('app_title');
  GInbound.Caption := Tx('group_inbound');
  GOutbound.Caption := Tx('group_outbound');
  GQueue.Caption := Tx('group_queue');
  GConfig.Caption := Tx('group_config');
  GLogs.Caption := Tx('group_logs');

  LblBindIP.Caption := Tx('bind_ip');
  LblBindPort.Caption := Tx('bind_port');
  LblAllowedIP.Caption := Tx('allowed_ip');
  LblMaxMessageMB.Caption := Tx('max_message_mb');
  LblInboundStatusCaption.Caption := Tx('status');
  LblInboundSessionsCaption.Caption := Tx('active_sessions');
  BtnStartListener.Caption := Tx('btn_start_listener');
  BtnStopListener.Caption := Tx('btn_stop_listener');

  LblOutHost.Caption := Tx('remote_host');
  LblOutPort.Caption := Tx('remote_port');
  LblTlsMode.Caption := Tx('tls_mode');
  LblWorkers.Caption := Tx('workers');
  LblSenderStatusCaption.Caption := Tx('status');
  LblActiveSendersCaption.Caption := Tx('active_senders');
  BtnStartSenders.Caption := Tx('btn_start_senders');
  BtnStopSenders.Caption := Tx('btn_stop_senders');
  BtnFlush.Caption := Tx('btn_flush');
  BtnEditCreds.Caption := Tx('btn_edit_credentials');

  LblCredTitle.Caption := Tx('cred_title');
  LblCredUser.Caption := Tx('cred_user');
  LblCredPassword.Caption := Tx('cred_password');
  BtnCredSave.Caption := Tx('btn_cred_save');
  BtnCredCancel.Caption := Tx('btn_cred_cancel');

  LblQueuePathCaption.Caption := Tx('queue_path');
  LblQueueSizeCaption.Caption := Tx('queue_size');
  LblInflightCaption.Caption := Tx('inflight');
  LblDeferredCaption.Caption := Tx('deferred');
  LblDeadCaption.Caption := Tx('dead');
  LblOldestCaption.Caption := Tx('oldest_age');
  BtnPurgeDead.Caption := Tx('btn_purge_dead');

  LblLanguageCaption.Caption := Tx('language');
  BtnSaveConfig.Caption := Tx('btn_save');
  BtnReloadConfig.Caption := Tx('btn_reload');
  BtnRestoreDefaults.Caption := Tx('btn_restore_defaults');
  CbDetailLogging.Caption := Tx('detail_logging');
  BtnClearLog.Caption := Tx('btn_clear_log');
  BtnCopyLog.Caption := Tx('btn_copy_log');

  CbLanguage.Items.BeginUpdate;
  try
    CbLanguage.Items.Clear;
    CbLanguage.Items.Add(RelayLanguageDisplayName(rlEn));
    CbLanguage.Items.Add(RelayLanguageDisplayName(rlRu));
    CbLanguage.Items.Add(RelayLanguageDisplayName(rlId));
    if TryRelayLanguageFromCode(FConfig.UILanguage, LLang) then
      CbLanguage.ItemIndex := Ord(LLang)
    else
      CbLanguage.ItemIndex := 0;
  finally
    CbLanguage.Items.EndUpdate;
  end;

  SetHint(EdBindIP, Tx('hint_bind_ip'));
  SetHint(EdBindPort, Tx('hint_bind_port'));
  SetHint(EdAllowedIP, Tx('hint_allowed_ip'));
  SetHint(EdMaxMessageMB, Tx('hint_max_message_mb'));
  SetHint(EdOutHost, Tx('hint_remote_host'));
  SetHint(EdOutPort, Tx('hint_remote_port'));
  SetHint(CbTlsMode, Tx('hint_tls_mode'));
  SetHint(EdWorkers, Tx('hint_workers'));
  SetHint(EdQueuePath, Tx('hint_queue_path'));
  SetHint(BtnFlush, Tx('hint_flush'));
  SetHint(BtnEditCreds, Tx('hint_edit_credentials'));
  SetHint(CbLanguage, Tx('hint_language'));
  SetHint(CbDetailLogging, Tx('hint_detail_logging'));
end;

procedure TForm1.LoadConfigToUi;
var
  LLang: TRelayLanguage;
begin
  EdBindIP.Text := FConfig.InboundBindIP;
  EdBindPort.Text := IntToStr(FConfig.InboundBindPort);
  EdAllowedIP.Text := FConfig.InboundAllowedClientIP;
  EdMaxMessageMB.Text := IntToStr(FConfig.InboundMaxMessageSizeMB);

  EdOutHost.Text := FConfig.OutboundHost;
  EdOutPort.Text := IntToStr(FConfig.OutboundPort);
  CbTlsMode.ItemIndex := CbTlsMode.Items.IndexOf(RelayTlsModeToString(FConfig.OutboundTlsMode));
  if CbTlsMode.ItemIndex < 0 then
    CbTlsMode.ItemIndex := 1;
  EdWorkers.Text := IntToStr(FConfig.OutboundWorkers);

  EdQueuePath.Text := FConfig.QueuePath;

  if TryRelayLanguageFromCode(FConfig.UILanguage, LLang) then
    CbLanguage.ItemIndex := Ord(LLang)
  else
    CbLanguage.ItemIndex := 0;
  CbDetailLogging.Checked := FConfig.UIDetailLoggingEnabled;
end;

function TForm1.SaveUiToConfig: Boolean;
var
  LInt: Integer;
  LError: string;
  LTlsMode: TRelayTlsMode;
begin
  Result := False;
  FConfig.InboundBindIP := Trim(EdBindIP.Text);

  if not TryStrToInt(Trim(EdBindPort.Text), LInt) then
  begin
    MessageDlg(Tx('msg_port_inbound_int'), mtError, [mbOK], 0);
    Exit;
  end;
  FConfig.InboundBindPort := LInt;

  FConfig.InboundAllowedClientIP := Trim(EdAllowedIP.Text);

  if not TryStrToInt(Trim(EdMaxMessageMB.Text), LInt) then
  begin
    MessageDlg(Tx('msg_max_message_int'), mtError, [mbOK], 0);
    Exit;
  end;
  FConfig.InboundMaxMessageSizeMB := LInt;

  FConfig.OutboundHost := Trim(EdOutHost.Text);

  if not TryStrToInt(Trim(EdOutPort.Text), LInt) then
  begin
    MessageDlg(Tx('msg_port_outbound_int'), mtError, [mbOK], 0);
    Exit;
  end;
  FConfig.OutboundPort := LInt;

  if not RelayTlsModeFromString(CbTlsMode.Text, LTlsMode) then
  begin
    MessageDlg(Tx('msg_tls_invalid'), mtError, [mbOK], 0);
    Exit;
  end;
  FConfig.OutboundTlsMode := LTlsMode;

  if not TryStrToInt(Trim(EdWorkers.Text), LInt) then
  begin
    MessageDlg(Tx('msg_workers_int'), mtError, [mbOK], 0);
    Exit;
  end;
  FConfig.OutboundWorkers := LInt;

  FConfig.QueuePath := Trim(EdQueuePath.Text);
  FConfig.UILanguage := SelectedLanguageCode;
  FConfig.UIDetailLoggingEnabled := CbDetailLogging.Checked;

  if not FConfig.Validate(LError) then
  begin
    MessageDlg(Tx('msg_config_invalid') + sLineBreak + LError, mtError, [mbOK], 0);
    Exit;
  end;

  Result := True;
end;

procedure TForm1.ApplyConfigToServices;
begin
  FLogger.SetMaxLines(FConfig.UILogTailLines);
  FQueue.ApplyConfig(FConfig);
  FInbound.ApplyConfig(FConfig);
  FSenders.ApplyConfig(FConfig);
  if Assigned(FUiTimer) then
    FUiTimer.Interval := FConfig.UIStatsRefreshMs;
end;

function TForm1.ListenerStatusText: string;
begin
  case FInbound.Status of
    lsStopped:
      Result := Tx('state_stopped');
    lsListening:
      Result := Tx('state_listening');
    lsError:
      Result := Tx('state_error') + ': ' + FInbound.LastError;
  else
    Result := Tx('state_unknown');
  end;
end;

function TForm1.SenderStatusText: string;
begin
  case FSenders.Status of
    ssStopped:
      Result := Tx('state_stopped');
    ssRunning:
      Result := Tx('state_running');
    ssError:
      Result := Tx('state_error') + ': ' + FSenders.LastError;
  else
    Result := Tx('state_unknown');
  end;
end;

procedure TForm1.PersistRuntimeState;
begin
  if not Assigned(FConfig) then
    Exit;
  FConfig.StateListenerStarted := Assigned(FInbound) and (FInbound.Status = lsListening);
  FConfig.StateSendersStarted := Assigned(FSenders) and (FSenders.Status = ssRunning);
  FConfig.SaveToIni(FIniPath);
end;

procedure TForm1.AutoStartByState;
begin
  if FConfig.StateListenerStarted then
  begin
    if not FInbound.Start then
      FLogger.Add('ERROR', 'Autostart listener failed: ' + FInbound.LastError);
  end;

  if FConfig.StateSendersStarted then
  begin
    if not FSenders.Start then
      FLogger.Add('ERROR', 'Autostart senders failed: ' + FSenders.LastError);
  end;
end;

procedure TForm1.UpdateUi;
var
  LStats: TQueueStats;
  LVersion: Int64;
begin
  LStats := FQueue.Stats;
  LblInboundStatusValue.Caption := ListenerStatusText;
  LblInboundSessionsValue.Caption := IntToStr(FInbound.ActiveSessions);
  LblSenderStatusValue.Caption := SenderStatusText;
  LblActiveSendersValue.Caption := IntToStr(FSenders.ActiveSenders);
  LblQueueSizeValue.Caption := IntToStr(LStats.TotalCount);
  LblInflightValue.Caption := IntToStr(LStats.InFlightCount);
  LblDeferredValue.Caption := IntToStr(LStats.DeferredCount);
  LblDeadValue.Caption := IntToStr(LStats.DeadCount);
  LblOldestValue.Caption := IntToStr(LStats.OldestAgeSec);

  LVersion := FLogger.Version;
  if LVersion <> FLastLogVersion then
  begin
    FLogger.Snapshot(MemoLog.Lines);
    FLastLogVersion := LVersion;
    MemoLog.SelStart := MemoLog.GetTextLen;
    MemoLog.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TForm1.ShowCredentialsPanel(AShow: Boolean);
var
  LUser: string;
  LPassword: string;
begin
  if AShow then
  begin
    try
      FConfig.GetAuthPlain(LUser, LPassword);
    except
      on E: Exception do
      begin
        LUser := '';
        LPassword := '';
        FLogger.Add('ERROR', 'Cannot decrypt SMTP credentials: ' + E.Message);
      end;
    end;
    EdCredUser.Text := LUser;
    EdCredPassword.Text := LPassword;
    PCreds.BringToFront;
    PCreds.Visible := True;
    EdCredUser.SetFocus;
  end
  else
  begin
    EdCredUser.Text := '';
    EdCredPassword.Text := '';
    PCreds.Visible := False;
  end;
end;

procedure TForm1.OnUiTimer(Sender: TObject);
begin
  UpdateUi;
end;

procedure TForm1.OnStartListener(Sender: TObject);
begin
  if not SaveUiToConfig then
    Exit;
  ApplyConfigToServices;
  if FInbound.Status = lsListening then
    FInbound.Stop;
  if not FInbound.Start then
    MessageDlg(Tx('msg_listener_start_fail') + sLineBreak + FInbound.LastError, mtError, [mbOK], 0)
  else
  begin
    FConfig.StateListenerStarted := True;
    FConfig.SaveToIni(FIniPath);
  end;
  UpdateUi;
end;

procedure TForm1.OnStopListener(Sender: TObject);
begin
  FInbound.Stop;
  FConfig.StateListenerStarted := False;
  FConfig.SaveToIni(FIniPath);
  UpdateUi;
end;

procedure TForm1.OnStartSenders(Sender: TObject);
begin
  if not SaveUiToConfig then
    Exit;
  ApplyConfigToServices;
  if FSenders.Status = ssRunning then
    FSenders.Stop;
  if not FSenders.Start then
    MessageDlg(Tx('msg_senders_start_fail') + sLineBreak + FSenders.LastError, mtError, [mbOK], 0)
  else
  begin
    FConfig.StateSendersStarted := True;
    FConfig.SaveToIni(FIniPath);
  end;
  UpdateUi;
end;

procedure TForm1.OnStopSenders(Sender: TObject);
begin
  FSenders.Stop;
  FConfig.StateSendersStarted := False;
  FConfig.SaveToIni(FIniPath);
  UpdateUi;
end;

procedure TForm1.OnFlush(Sender: TObject);
begin
  if FSenders.Status <> ssRunning then
    FLogger.Add('WARN', Tx('msg_flush_no_senders'));
  FSenders.FlushNow;
  UpdateUi;
end;

procedure TForm1.OnEditCreds(Sender: TObject);
begin
  ShowCredentialsPanel(True);
end;

procedure TForm1.OnCredSave(Sender: TObject);
var
  LUser: string;
  LPassword: string;
  LVerifyUser: string;
  LVerifyPassword: string;
begin
  try
    LUser := Trim(EdCredUser.Text);
    LPassword := Trim(EdCredPassword.Text);
    FConfig.SetAuthPlain(LUser, LPassword);
    FConfig.GetAuthPlain(LVerifyUser, LVerifyPassword);
    if (LVerifyUser <> LUser) or (LVerifyPassword <> LPassword) then
      raise Exception.Create('Credential round-trip verification failed');
    ApplyConfigToServices;
    FConfig.SaveToIni(FIniPath);
    FLogger.Add('CONFIG', Format('SMTP credentials updated (encrypted) user=%s passLen=%d',
      [LUser, Length(LPassword)]));
    MessageDlg(Tx('msg_creds_saved'), mtInformation, [mbOK], 0);
    ShowCredentialsPanel(False);
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TForm1.OnCredCancel(Sender: TObject);
begin
  ShowCredentialsPanel(False);
end;

procedure TForm1.OnPurgeDead(Sender: TObject);
var
  LDeleted: Integer;
begin
  if MessageDlg(Tx('msg_purge_confirm'), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  LDeleted := FQueue.PurgeDeadLetters;
  FLogger.Add('QUEUE', Format('Dead-letter files removed: %d', [LDeleted]));
  UpdateUi;
end;

procedure TForm1.OnClearLog(Sender: TObject);
begin
  FLogger.Clear;
  UpdateUi;
end;

procedure TForm1.OnCopyLog(Sender: TObject);
begin
  Clipboard.AsText := MemoLog.Lines.Text;
end;

procedure TForm1.OnDetailLoggingClick(Sender: TObject);
begin
  FConfig.UIDetailLoggingEnabled := CbDetailLogging.Checked;
  ApplyConfigToServices;
  FConfig.SaveToIni(FIniPath);
  FLogger.Add('CONFIG', Format('Detail logging %s', [BoolToStr(CbDetailLogging.Checked, True)]));
end;

procedure TForm1.OnSaveConfig(Sender: TObject);
begin
  if not SaveUiToConfig then
    Exit;
  ShowCredentialsPanel(False);
  SetLanguageFromConfig;
  ApplyLanguage;
  ApplyConfigToServices;
  PersistRuntimeState;
  FLogger.Add('CONFIG', 'Saved to ' + FIniPath);
  UpdateUi;
end;

procedure TForm1.OnReloadConfig(Sender: TObject);
begin
  ShowCredentialsPanel(False);
  FConfig.SetDefaults;
  FConfig.LoadFromIni(FIniPath);
  SetLanguageFromConfig;
  LoadConfigToUi;
  ApplyLanguage;
  ApplyConfigToServices;
  FLogger.Add('CONFIG', 'Reloaded from ' + FIniPath);
  UpdateUi;
end;

procedure TForm1.OnRestoreDefaults(Sender: TObject);
begin
  if MessageDlg(Tx('msg_defaults_confirm'), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  ShowCredentialsPanel(False);
  FConfig.SetDefaults;
  SetLanguageFromConfig;
  LoadConfigToUi;
  ApplyLanguage;
  ApplyConfigToServices;
  FLogger.Add('CONFIG', 'Defaults restored');
  UpdateUi;
end;

end.

