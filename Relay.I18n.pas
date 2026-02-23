unit Relay.I18n;


interface

type
  TRelayLanguage = (rlEn, rlRu, rlId);

function TryRelayLanguageFromCode(const ACode: string; out ALanguage: TRelayLanguage): Boolean;
function RelayLanguageToCode(const ALanguage: TRelayLanguage): string;
function RelayLanguageDisplayName(const ALanguage: TRelayLanguage): string;
function Tr(const ALanguage: TRelayLanguage; const AKey: string): string;

implementation

uses
  System.SysUtils;

function TryRelayLanguageFromCode(const ACode: string; out ALanguage: TRelayLanguage): Boolean;
var
  LCode: string;
begin
  LCode := UpperCase(Trim(ACode));
  if LCode = 'RU' then
  begin
    ALanguage := rlRu;
    Exit(True);
  end;
  if LCode = 'ID' then
  begin
    ALanguage := rlId;
    Exit(True);
  end;
  if LCode = 'EN' then
  begin
    ALanguage := rlEn;
    Exit(True);
  end;

  Result := False;
end;

function RelayLanguageToCode(const ALanguage: TRelayLanguage): string;
begin
  case ALanguage of
    rlRu:
      Result := 'RU';
    rlId:
      Result := 'ID';
  else
    Result := 'EN';
  end;
end;

function RelayLanguageDisplayName(const ALanguage: TRelayLanguage): string;
begin
  case ALanguage of
    rlRu:
      Result := 'Русский (RU)';
    rlId:
      Result := 'Bahasa Indonesia (ID)';
  else
    Result := 'English (EN)';
  end;
end;

function Tr(const ALanguage: TRelayLanguage; const AKey: string): string;
begin
  case ALanguage of
    rlRu:
      begin
        if SameText(AKey, 'app_title') then Exit('MailRelay для Oracle APEX');
        if SameText(AKey, 'group_inbound') then Exit('Входящий SMTP (APEX -> Relay)');
        if SameText(AKey, 'group_outbound') then Exit('Исходящий SMTP (Relay -> Smarthost)');
        if SameText(AKey, 'group_queue') then Exit('Очередь');
        if SameText(AKey, 'group_config') then Exit('Конфигурация');
        if SameText(AKey, 'group_logs') then Exit('Логи');

        if SameText(AKey, 'bind_ip') then Exit('Bind IP');
        if SameText(AKey, 'bind_port') then Exit('Bind Port');
        if SameText(AKey, 'allowed_ip') then Exit('Разрешенный IP клиента');
        if SameText(AKey, 'max_message_mb') then Exit('Макс. размер письма (МБ)');
        if SameText(AKey, 'remote_host') then Exit('Удалённый SMTP хост');
        if SameText(AKey, 'remote_port') then Exit('Удалённый SMTP порт');
        if SameText(AKey, 'tls_mode') then Exit('Режим TLS/SSL');
        if SameText(AKey, 'workers') then Exit('Воркеры (1..8)');
        if SameText(AKey, 'status') then Exit('Статус');
        if SameText(AKey, 'active_sessions') then Exit('Активные сессии');
        if SameText(AKey, 'active_senders') then Exit('Активные отправители');
        if SameText(AKey, 'queue_path') then Exit('Путь очереди');
        if SameText(AKey, 'queue_size') then Exit('Размер очереди');
        if SameText(AKey, 'inflight') then Exit('In-flight');
        if SameText(AKey, 'deferred') then Exit('Отложено');
        if SameText(AKey, 'dead') then Exit('Dead');
        if SameText(AKey, 'oldest_age') then Exit('Возраст старейшего (сек)');
        if SameText(AKey, 'language') then Exit('Язык');
        if SameText(AKey, 'detail_logging') then Exit('Детальный SMTP лог');

        if SameText(AKey, 'btn_start_listener') then Exit('Запустить Listener');
        if SameText(AKey, 'btn_stop_listener') then Exit('Остановить Listener');
        if SameText(AKey, 'btn_start_senders') then Exit('Запустить Senders');
        if SameText(AKey, 'btn_stop_senders') then Exit('Остановить Senders');
        if SameText(AKey, 'btn_flush') then Exit('Flush Now');
        if SameText(AKey, 'btn_purge_dead') then Exit('Очистить Dead-Letters');
        if SameText(AKey, 'btn_save') then Exit('Сохранить');
        if SameText(AKey, 'btn_reload') then Exit('Перечитать');
        if SameText(AKey, 'btn_restore_defaults') then Exit('Сбросить по умолчанию');
        if SameText(AKey, 'btn_clear_log') then Exit('Очистить лог');
        if SameText(AKey, 'btn_copy_log') then Exit('Копировать всё');
        if SameText(AKey, 'btn_edit_credentials') then Exit('Логин/пароль SMTP');
        if SameText(AKey, 'btn_cred_save') then Exit('Сохранить');
        if SameText(AKey, 'btn_cred_cancel') then Exit('Отмена');

        if SameText(AKey, 'cred_title') then Exit('Учетные данные SMTP');
        if SameText(AKey, 'cred_user') then Exit('Логин');
        if SameText(AKey, 'cred_password') then Exit('Пароль');

        if SameText(AKey, 'state_stopped') then Exit('ОСТАНОВЛЕН');
        if SameText(AKey, 'state_listening') then Exit('СЛУШАЕТ');
        if SameText(AKey, 'state_running') then Exit('РАБОТАЕТ');
        if SameText(AKey, 'state_error') then Exit('ОШИБКА');
        if SameText(AKey, 'state_unknown') then Exit('НЕИЗВЕСТНО');

        if SameText(AKey, 'msg_port_inbound_int') then Exit('Inbound Bind Port должен быть целым числом.');
        if SameText(AKey, 'msg_max_message_int') then Exit('Inbound Max Message Size должен быть целым числом.');
        if SameText(AKey, 'msg_port_outbound_int') then Exit('Outbound Port должен быть целым числом.');
        if SameText(AKey, 'msg_workers_int') then Exit('Outbound Workers должен быть целым числом.');
        if SameText(AKey, 'msg_tls_invalid') then Exit('Некорректный TLS режим.');
        if SameText(AKey, 'msg_config_invalid') then Exit('Ошибка валидации конфигурации:');
        if SameText(AKey, 'msg_listener_start_fail') then Exit('Не удалось запустить listener:');
        if SameText(AKey, 'msg_senders_start_fail') then Exit('Не удалось запустить senders:');
        if SameText(AKey, 'msg_purge_confirm') then Exit('Удалить все файлы в dead-letter?');
        if SameText(AKey, 'msg_defaults_confirm') then Exit('Восстановить значения по умолчанию?');
        if SameText(AKey, 'msg_creds_saved') then Exit('Учетные данные SMTP сохранены (зашифрованы).');
        if SameText(AKey, 'msg_flush_no_senders') then Exit('Flush запрошен, но senders не запущены.');

        if SameText(AKey, 'hint_bind_ip') then Exit('Локальный IP адрес, на котором listener принимает SMTP-соединения от APEX.');
        if SameText(AKey, 'hint_bind_port') then Exit('Локальный SMTP порт listener. Обычно 25 или 2525.');
        if SameText(AKey, 'hint_allowed_ip') then Exit('Разрешён только этот IP клиента (Oracle APEX). Любой другой будет отклонён.');
        if SameText(AKey, 'hint_max_message_mb') then Exit('Жёсткий лимит размера входящего письма в мегабайтах.');
        if SameText(AKey, 'hint_remote_host') then Exit('Smarthost SMTP для исходящей доставки.');
        if SameText(AKey, 'hint_remote_port') then Exit('Порт smarthost SMTP (обычно 587 для STARTTLS, 465 для implicit SSL).');
        if SameText(AKey, 'hint_tls_mode') then Exit('PLAIN без TLS, STARTTLS с апгрейдом TLS, SSL_IMPLICIT с TLS с начала сессии.');
        if SameText(AKey, 'hint_workers') then Exit('Количество параллельных потоков отправки.');
        if SameText(AKey, 'hint_queue_path') then Exit('Путь к durable spool очереди на диске.');
        if SameText(AKey, 'hint_flush') then Exit('Ставит deferred письма в немедленную попытку и будит воркеры.');
        if SameText(AKey, 'hint_edit_credentials') then Exit('Открыть блок редактирования логина/пароля SMTP. Значения хранятся в ini только в шифрованном виде.');
        if SameText(AKey, 'hint_language') then Exit('Язык интерфейса приложения.');
        if SameText(AKey, 'hint_detail_logging') then Exit('Логировать полный входящий SMTP-диалог: C:/S:, команды, ответы и строки DATA.');
      end;
    rlId:
      begin
        if SameText(AKey, 'app_title') then Exit('MailRelay untuk Oracle APEX');
        if SameText(AKey, 'group_inbound') then Exit('SMTP Masuk (APEX -> Relay)');
        if SameText(AKey, 'group_outbound') then Exit('SMTP Keluar (Relay -> Smarthost)');
        if SameText(AKey, 'group_queue') then Exit('Antrean');
        if SameText(AKey, 'group_config') then Exit('Konfigurasi');
        if SameText(AKey, 'group_logs') then Exit('Log');

        if SameText(AKey, 'bind_ip') then Exit('Bind IP');
        if SameText(AKey, 'bind_port') then Exit('Bind Port');
        if SameText(AKey, 'allowed_ip') then Exit('IP Klien Diizinkan');
        if SameText(AKey, 'max_message_mb') then Exit('Ukuran Pesan Maks (MB)');
        if SameText(AKey, 'remote_host') then Exit('Host SMTP Tujuan');
        if SameText(AKey, 'remote_port') then Exit('Port SMTP Tujuan');
        if SameText(AKey, 'tls_mode') then Exit('Mode TLS/SSL');
        if SameText(AKey, 'workers') then Exit('Worker (1..8)');
        if SameText(AKey, 'status') then Exit('Status');
        if SameText(AKey, 'active_sessions') then Exit('Sesi Aktif');
        if SameText(AKey, 'active_senders') then Exit('Pengirim Aktif');
        if SameText(AKey, 'queue_path') then Exit('Path Antrean');
        if SameText(AKey, 'queue_size') then Exit('Ukuran Antrean');
        if SameText(AKey, 'inflight') then Exit('In-flight');
        if SameText(AKey, 'deferred') then Exit('Ditunda');
        if SameText(AKey, 'dead') then Exit('Dead');
        if SameText(AKey, 'oldest_age') then Exit('Usia Tertua (detik)');
        if SameText(AKey, 'language') then Exit('Bahasa');
        if SameText(AKey, 'detail_logging') then Exit('Logging SMTP detail');

        if SameText(AKey, 'btn_start_listener') then Exit('Mulai Listener');
        if SameText(AKey, 'btn_stop_listener') then Exit('Hentikan Listener');
        if SameText(AKey, 'btn_start_senders') then Exit('Mulai Sender');
        if SameText(AKey, 'btn_stop_senders') then Exit('Hentikan Sender');
        if SameText(AKey, 'btn_flush') then Exit('Flush Now');
        if SameText(AKey, 'btn_purge_dead') then Exit('Hapus Dead-Letters');
        if SameText(AKey, 'btn_save') then Exit('Simpan');
        if SameText(AKey, 'btn_reload') then Exit('Muat Ulang');
        if SameText(AKey, 'btn_restore_defaults') then Exit('Kembalikan Default');
        if SameText(AKey, 'btn_clear_log') then Exit('Bersihkan Log');
        if SameText(AKey, 'btn_copy_log') then Exit('Salin Semua');
        if SameText(AKey, 'btn_edit_credentials') then Exit('Login/Kata Sandi SMTP');
        if SameText(AKey, 'btn_cred_save') then Exit('Simpan');
        if SameText(AKey, 'btn_cred_cancel') then Exit('Batal');

        if SameText(AKey, 'cred_title') then Exit('Kredensial SMTP');
        if SameText(AKey, 'cred_user') then Exit('Login');
        if SameText(AKey, 'cred_password') then Exit('Kata Sandi');

        if SameText(AKey, 'state_stopped') then Exit('BERHENTI');
        if SameText(AKey, 'state_listening') then Exit('MENDENGAR');
        if SameText(AKey, 'state_running') then Exit('BERJALAN');
        if SameText(AKey, 'state_error') then Exit('ERROR');
        if SameText(AKey, 'state_unknown') then Exit('TIDAK DIKETAHUI');

        if SameText(AKey, 'msg_port_inbound_int') then Exit('Inbound Bind Port harus berupa angka.');
        if SameText(AKey, 'msg_max_message_int') then Exit('Inbound Max Message Size harus berupa angka.');
        if SameText(AKey, 'msg_port_outbound_int') then Exit('Outbound Port harus berupa angka.');
        if SameText(AKey, 'msg_workers_int') then Exit('Outbound Workers harus berupa angka.');
        if SameText(AKey, 'msg_tls_invalid') then Exit('Mode TLS tidak valid.');
        if SameText(AKey, 'msg_config_invalid') then Exit('Validasi konfigurasi gagal:');
        if SameText(AKey, 'msg_listener_start_fail') then Exit('Gagal memulai listener:');
        if SameText(AKey, 'msg_senders_start_fail') then Exit('Gagal memulai sender:');
        if SameText(AKey, 'msg_purge_confirm') then Exit('Hapus semua file dead-letter?');
        if SameText(AKey, 'msg_defaults_confirm') then Exit('Pulihkan nilai default?');
        if SameText(AKey, 'msg_creds_saved') then Exit('Kredensial SMTP tersimpan (terenkripsi).');
        if SameText(AKey, 'msg_flush_no_senders') then Exit('Flush diminta, tetapi sender tidak berjalan.');

        if SameText(AKey, 'hint_bind_ip') then Exit('Alamat IP lokal untuk listener menerima koneksi SMTP dari APEX.');
        if SameText(AKey, 'hint_bind_port') then Exit('Port SMTP lokal listener. Umumnya 25 atau 2525.');
        if SameText(AKey, 'hint_allowed_ip') then Exit('Hanya IP klien ini (Oracle APEX) yang diizinkan.');
        if SameText(AKey, 'hint_max_message_mb') then Exit('Batas keras ukuran email masuk dalam MB.');
        if SameText(AKey, 'hint_remote_host') then Exit('Smarthost SMTP untuk pengiriman keluar.');
        if SameText(AKey, 'hint_remote_port') then Exit('Port smarthost SMTP (biasanya 587 STARTTLS, 465 SSL implicit).');
        if SameText(AKey, 'hint_tls_mode') then Exit('PLAIN tanpa TLS, STARTTLS upgrade TLS, SSL_IMPLICIT TLS sejak awal.');
        if SameText(AKey, 'hint_workers') then Exit('Jumlah thread pengiriman paralel.');
        if SameText(AKey, 'hint_queue_path') then Exit('Path antrean durable spool di disk.');
        if SameText(AKey, 'hint_flush') then Exit('Mengubah semua email deferred menjadi due-now lalu membangunkan worker.');
        if SameText(AKey, 'hint_edit_credentials') then Exit('Buka editor login/kata sandi SMTP. Nilai di ini disimpan terenkripsi.');
        if SameText(AKey, 'hint_language') then Exit('Bahasa antarmuka aplikasi.');
        if SameText(AKey, 'hint_detail_logging') then Exit('Catat seluruh dialog SMTP masuk: C:/S:, perintah, respons, dan baris DATA.');
      end;
  else
    begin
      if SameText(AKey, 'app_title') then Exit('MailRelay for Oracle APEX');
      if SameText(AKey, 'group_inbound') then Exit('Inbound (APEX -> Relay)');
      if SameText(AKey, 'group_outbound') then Exit('Outbound (Relay -> Smarthost)');
      if SameText(AKey, 'group_queue') then Exit('Queue');
      if SameText(AKey, 'group_config') then Exit('Config');
      if SameText(AKey, 'group_logs') then Exit('Logs');

      if SameText(AKey, 'bind_ip') then Exit('Bind IP');
      if SameText(AKey, 'bind_port') then Exit('Bind Port');
      if SameText(AKey, 'allowed_ip') then Exit('Allowed Client IP');
      if SameText(AKey, 'max_message_mb') then Exit('Max Message Size (MB)');
      if SameText(AKey, 'remote_host') then Exit('Remote Host');
      if SameText(AKey, 'remote_port') then Exit('Remote Port');
      if SameText(AKey, 'tls_mode') then Exit('TLS/SSL Mode');
      if SameText(AKey, 'workers') then Exit('Workers (1..8)');
      if SameText(AKey, 'status') then Exit('Status');
      if SameText(AKey, 'active_sessions') then Exit('Active Sessions');
      if SameText(AKey, 'active_senders') then Exit('Active Senders');
      if SameText(AKey, 'queue_path') then Exit('Queue Path');
      if SameText(AKey, 'queue_size') then Exit('Queue Size');
      if SameText(AKey, 'inflight') then Exit('In-flight');
      if SameText(AKey, 'deferred') then Exit('Deferred');
      if SameText(AKey, 'dead') then Exit('Dead');
      if SameText(AKey, 'oldest_age') then Exit('Oldest Age (sec)');
      if SameText(AKey, 'language') then Exit('Language');
      if SameText(AKey, 'detail_logging') then Exit('Enable detail logging');

      if SameText(AKey, 'btn_start_listener') then Exit('Start Listener');
      if SameText(AKey, 'btn_stop_listener') then Exit('Stop Listener');
      if SameText(AKey, 'btn_start_senders') then Exit('Start Senders');
      if SameText(AKey, 'btn_stop_senders') then Exit('Stop Senders');
      if SameText(AKey, 'btn_flush') then Exit('Flush Now');
      if SameText(AKey, 'btn_purge_dead') then Exit('Purge Dead-Letters');
      if SameText(AKey, 'btn_save') then Exit('Save');
      if SameText(AKey, 'btn_reload') then Exit('Reload');
      if SameText(AKey, 'btn_restore_defaults') then Exit('Restore Defaults');
      if SameText(AKey, 'btn_clear_log') then Exit('Clear Log');
      if SameText(AKey, 'btn_copy_log') then Exit('Copy All');
      if SameText(AKey, 'btn_edit_credentials') then Exit('SMTP Credentials');
      if SameText(AKey, 'btn_cred_save') then Exit('Save');
      if SameText(AKey, 'btn_cred_cancel') then Exit('Cancel');

      if SameText(AKey, 'cred_title') then Exit('SMTP Credentials');
      if SameText(AKey, 'cred_user') then Exit('Login');
      if SameText(AKey, 'cred_password') then Exit('Password');

      if SameText(AKey, 'state_stopped') then Exit('STOPPED');
      if SameText(AKey, 'state_listening') then Exit('LISTENING');
      if SameText(AKey, 'state_running') then Exit('RUNNING');
      if SameText(AKey, 'state_error') then Exit('ERROR');
      if SameText(AKey, 'state_unknown') then Exit('UNKNOWN');

      if SameText(AKey, 'msg_port_inbound_int') then Exit('Inbound Bind Port must be an integer.');
      if SameText(AKey, 'msg_max_message_int') then Exit('Inbound Max Message Size must be an integer.');
      if SameText(AKey, 'msg_port_outbound_int') then Exit('Outbound Port must be an integer.');
      if SameText(AKey, 'msg_workers_int') then Exit('Outbound workers must be an integer.');
      if SameText(AKey, 'msg_tls_invalid') then Exit('TLS mode is invalid.');
      if SameText(AKey, 'msg_config_invalid') then Exit('Config validation failed:');
      if SameText(AKey, 'msg_listener_start_fail') then Exit('Cannot start listener:');
      if SameText(AKey, 'msg_senders_start_fail') then Exit('Cannot start senders:');
      if SameText(AKey, 'msg_purge_confirm') then Exit('Purge all dead-letter files?');
      if SameText(AKey, 'msg_defaults_confirm') then Exit('Restore default config values?');
      if SameText(AKey, 'msg_creds_saved') then Exit('SMTP credentials saved (encrypted).');
      if SameText(AKey, 'msg_flush_no_senders') then Exit('Flush requested, but senders are not running.');

      if SameText(AKey, 'hint_bind_ip') then Exit('Local IP where the listener accepts SMTP connections from APEX.');
      if SameText(AKey, 'hint_bind_port') then Exit('Local listener SMTP port. Usually 25 or 2525.');
      if SameText(AKey, 'hint_allowed_ip') then Exit('Only this client IP (Oracle APEX) is allowed. Others are rejected.');
      if SameText(AKey, 'hint_max_message_mb') then Exit('Hard limit for inbound message size in megabytes.');
      if SameText(AKey, 'hint_remote_host') then Exit('Smarthost SMTP server used for outbound delivery.');
      if SameText(AKey, 'hint_remote_port') then Exit('Smarthost SMTP port (usually 587 for STARTTLS, 465 for implicit SSL).');
      if SameText(AKey, 'hint_tls_mode') then Exit('PLAIN for no TLS, STARTTLS for explicit TLS upgrade, SSL_IMPLICIT for direct TLS.');
      if SameText(AKey, 'hint_workers') then Exit('Number of parallel sender worker threads.');
      if SameText(AKey, 'hint_queue_path') then Exit('Disk path to the durable spool queue.');
      if SameText(AKey, 'hint_flush') then Exit('Marks deferred items as due now and wakes sender workers immediately.');
      if SameText(AKey, 'hint_edit_credentials') then Exit('Open SMTP login/password editor. Values are stored encrypted in ini.');
      if SameText(AKey, 'hint_language') then Exit('Application UI language.');
      if SameText(AKey, 'hint_detail_logging') then Exit('Logs full inbound SMTP dialog: C:/S:, commands, replies, and DATA lines.');
    end;
  end;

  Result := AKey;
end;

end.
