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
        if SameText(AKey, 'tab_dashboard') then Exit('Панель');
        if SameText(AKey, 'tab_config') then Exit('Конфиг');
        if SameText(AKey, 'tab_log') then Exit('Логи');

        if SameText(AKey, 'group_dash_queue') then Exit('Очередь (сводка)');
        if SameText(AKey, 'group_dash_actions') then Exit('Действия');
        if SameText(AKey, 'group_dash_relay_status') then Exit('Статус релея');
        if SameText(AKey, 'group_dash_hourly') then Exit('Писем в час (последние 24ч)');
        if SameText(AKey, 'group_dash_inbound_distribution') then Exit('Распределение почты inbound');
        if SameText(AKey, 'group_dash_outbound_distribution') then Exit('Распределение почты outbound');
        if SameText(AKey, 'group_dash_problematic') then Exit('Процент проблемной почты');

        if SameText(AKey, 'group_inbound_config') then Exit('Inbound конфигурация');
        if SameText(AKey, 'group_inbound_auth') then Exit('Inbound auth конфигурация');
        if SameText(AKey, 'group_outbound_config') then Exit('Outbound конфигурация');
        if SameText(AKey, 'group_outbound_auth') then Exit('Outbound auth credentials');
        if SameText(AKey, 'group_queue_config') then Exit('Queue');
        if SameText(AKey, 'group_ui_config') then Exit('UI');
        if SameText(AKey, 'group_retry_config') then Exit('Retry');
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
        if SameText(AKey, 'relay_listener_status') then Exit('Status Listener:');
        if SameText(AKey, 'relay_senders_status') then Exit('Status Senders:');
        if SameText(AKey, 'relay_active_listener_sessions') then Exit('Active Listener Sessions:');
        if SameText(AKey, 'relay_active_senders') then Exit('Active Senders:');
        if SameText(AKey, 'inbound_auth_username') then Exit('Имя пользователя');
        if SameText(AKey, 'inbound_auth_password') then Exit('Пароль');
        if SameText(AKey, 'inbound_require_auth') then Exit('Требовать auth credentials');
        if SameText(AKey, 'outbound_auth_username') then Exit('SMTP имя пользователя');
        if SameText(AKey, 'outbound_auth_password') then Exit('SMTP пароль');
        if SameText(AKey, 'queue_max_items') then Exit('Макс. элементов очереди');
        if SameText(AKey, 'queue_max_bytes_mb') then Exit('Макс. размер очереди (МБ)');
        if SameText(AKey, 'queue_inflight_stale_sec') then Exit('In-flight stale timeout (сек)');
        if SameText(AKey, 'log_tail_lines') then Exit('Строк в логе');
        if SameText(AKey, 'stats_refresh_ms') then Exit('Обновление статистики (мс)');
        if SameText(AKey, 'retry_max_attempts') then Exit('Макс. попыток');
        if SameText(AKey, 'retry_base_delay_sec') then Exit('Базовая задержка (сек)');
        if SameText(AKey, 'retry_max_delay_sec') then Exit('Макс. задержка (сек)');
        if SameText(AKey, 'retry_jitter_pct') then Exit('Jitter (%)');

        if SameText(AKey, 'btn_start_listener') then Exit('Запустить Listener');
        if SameText(AKey, 'btn_stop_listener') then Exit('Остановить Listener');
        if SameText(AKey, 'btn_start_senders') then Exit('Запустить Senders');
        if SameText(AKey, 'btn_stop_senders') then Exit('Остановить Senders');
        if SameText(AKey, 'btn_flush') then Exit('Отправить сейчас');
        if SameText(AKey, 'btn_purge_dead') then Exit('Очистить Dead-Letters');
        if SameText(AKey, 'btn_save') then Exit('Сохранить');
        if SameText(AKey, 'btn_reload') then Exit('Перечитать');
        if SameText(AKey, 'btn_restore_defaults') then Exit('Сбросить по умолчанию');
        if SameText(AKey, 'btn_clear_log') then Exit('Очистить лог');
        if SameText(AKey, 'btn_copy_log') then Exit('Копировать всё');
        if SameText(AKey, 'btn_edit_credentials') then Exit('Логин/пароль SMTP');
        if SameText(AKey, 'btn_cred_save') then Exit('Сохранить');
        if SameText(AKey, 'btn_cred_cancel') then Exit('Отмена');
        if SameText(AKey, 'btn_reset') then Exit('Сбросить');
        if SameText(AKey, 'statusbar_made_by') then Exit('Сделано Krossel Apps');
        if SameText(AKey, 'dash_state_persist_note') then Exit('Сервер сохранит состояние после перезапуска.');
        if SameText(AKey, 'bind_ip_help_caption') then Exit('Bind IP определяет интерфейс прослушивания listener. Пример: 127.0.0.1');
        if SameText(AKey, 'allowed_ip_help_caption') then Exit('Allowed Client IP задаёт список разрешённых клиентов. Пример: 127.0.0.1, 212.12.27.17');
        if SameText(AKey, 'workers_help_caption') then Exit('Workers - это число параллельных потоков отправки. Увеличивайте с учётом лимитов провайдера.');
        if SameText(AKey, 'detail_logging_help_caption') then Exit('Дополнительно логируются SMTP строки: C:, S:, C-DATA:, EVT:.');
        if SameText(AKey, 'retry_jitter_help_caption') then Exit('Jitter добавляет случайный разброс задержки, чтобы избежать пиков повторных попыток.');
        if SameText(AKey, 'autosave_note') then Exit('Все изменения сохраняются автоматически');

        if SameText(AKey, 'cred_title') then Exit('Учетные данные SMTP');
        if SameText(AKey, 'cred_user') then Exit('Логин');
        if SameText(AKey, 'cred_password') then Exit('Пароль');

        if SameText(AKey, 'state_stopped') then Exit('ОСТАНОВЛЕН');
        if SameText(AKey, 'state_listening') then Exit('ЗАПУЩЕН');
        if SameText(AKey, 'state_running') then Exit('РАБОТАЕТ');
        if SameText(AKey, 'state_error') then Exit('ОШИБКА');
        if SameText(AKey, 'state_unknown') then Exit('НЕИЗВЕСТНО');
        if SameText(AKey, 'chart_hourly_title') then Exit('Объем очереди по часам');
        if SameText(AKey, 'chart_inbound_title') then Exit('Распределение inbound');
        if SameText(AKey, 'chart_outbound_title') then Exit('Распределение outbound');
        if SameText(AKey, 'chart_problem_title') then Exit('Проблемные vs здоровые');
        if SameText(AKey, 'chart_no_data') then Exit('Нет данных');
        if SameText(AKey, 'chart_unknown') then Exit('неизвестно');
        if SameText(AKey, 'chart_problematic') then Exit('Проблемные');
        if SameText(AKey, 'chart_healthy') then Exit('Здоровые');

        if SameText(AKey, 'msg_field_int') then Exit('Поле "%s" должно быть целым числом.');
        if SameText(AKey, 'msg_port_inbound_int') then Exit('Inbound Bind Port должен быть целым числом.');
        if SameText(AKey, 'msg_max_message_int') then Exit('Inbound Max Message Size должен быть целым числом.');
        if SameText(AKey, 'msg_port_outbound_int') then Exit('Outbound Port должен быть целым числом.');
        if SameText(AKey, 'msg_workers_int') then Exit('Outbound Workers должен быть целым числом.');
        if SameText(AKey, 'msg_tls_invalid') then Exit('Некорректный TLS режим.');
        if SameText(AKey, 'msg_config_invalid') then Exit('Ошибка валидации конфигурации:');
        if SameText(AKey, 'msg_listener_start_fail') then Exit('Не удалось запустить listener:');
        if SameText(AKey, 'msg_senders_start_fail') then Exit('Не удалось запустить senders:');
        if SameText(AKey, 'msg_purge_confirm') then Exit('Удалить все файлы в dead-letter?');
        if SameText(AKey, 'msg_defaults_confirm') then Exit('Сбросить значения по умолчанию? (outbound credentials сохраняются)');
        if SameText(AKey, 'msg_creds_saved') then Exit('Учетные данные SMTP сохранены (зашифрованы).');
        if SameText(AKey, 'msg_flush_no_senders') then Exit('Flush запрошен, но senders не запущены.');

        if SameText(AKey, 'hint_bind_ip') then Exit('Локальный IP адрес, на котором listener принимает SMTP-соединения от APEX.');
        if SameText(AKey, 'hint_bind_port') then Exit('Локальный SMTP порт listener. Обычно 25 или 2525.');
        if SameText(AKey, 'hint_allowed_ip') then Exit('Список разрешённых IP через запятую. Пример: 127.0.0.1, 212.12.27.17');
        if SameText(AKey, 'hint_max_message_mb') then Exit('Жёсткий лимит размера входящего письма в мегабайтах.');
        if SameText(AKey, 'hint_remote_host') then Exit('Smarthost SMTP для исходящей доставки.');
        if SameText(AKey, 'hint_remote_port') then Exit('Порт smarthost SMTP (обычно 587 для STARTTLS, 465 для implicit SSL).');
        if SameText(AKey, 'hint_tls_mode') then Exit('PLAIN без TLS, STARTTLS с апгрейдом TLS, SSL_IMPLICIT с TLS с начала сессии.');
        if SameText(AKey, 'hint_workers') then Exit('Количество параллельных потоков отправки. Учитывайте лимиты smarthost.');
        if SameText(AKey, 'hint_queue_path') then Exit('Путь к durable spool очереди на диске.');
        if SameText(AKey, 'hint_flush') then Exit('Ставит deferred письма в немедленную попытку и будит воркеры.');
        if SameText(AKey, 'hint_edit_credentials') then Exit('Открыть блок редактирования логина/пароля SMTP. Значения хранятся в ini только в шифрованном виде.');
        if SameText(AKey, 'hint_language') then Exit('Язык интерфейса приложения.');
        if SameText(AKey, 'hint_detail_logging') then Exit('Логировать полный входящий SMTP-диалог: C:/S:, команды, ответы и строки DATA.');
        if SameText(AKey, 'hint_inbound_auth_user') then Exit('Username для входящей SMTP-аутентификации.');
        if SameText(AKey, 'hint_inbound_auth_password') then Exit('Password для входящей SMTP-аутентификации.');
        if SameText(AKey, 'hint_inbound_require_auth') then Exit('Если включено и заданы логин/пароль, MAIL FROM потребует успешный AUTH.');
        if SameText(AKey, 'hint_reset_inbound_auth') then Exit('Очищает inbound username/password и снимает флаг обязательной аутентификации.');
        if SameText(AKey, 'hint_outbound_auth_user') then Exit('Username для SMTP-аутентификации на внешнем smarthost.');
        if SameText(AKey, 'hint_outbound_auth_password') then Exit('Password для SMTP-аутентификации на внешнем smarthost.');
        if SameText(AKey, 'hint_queue_max_items') then Exit('Максимальное количество сообщений в очереди new+inflight+deferred.');
        if SameText(AKey, 'hint_queue_max_bytes_mb') then Exit('Максимальный общий размер очереди в мегабайтах.');
        if SameText(AKey, 'hint_queue_inflight_stale_sec') then Exit('Через сколько секунд in-flight письмо считается зависшим и переносится в deferred.');
        if SameText(AKey, 'hint_log_tail_lines') then Exit('Сколько строк показывать в UI memo логов.');
        if SameText(AKey, 'hint_stats_refresh_ms') then Exit('Интервал обновления dashboard и логов в миллисекундах.');
        if SameText(AKey, 'hint_retry_max_attempts') then Exit('Сколько попыток отправки делать до перемещения в dead-letter.');
        if SameText(AKey, 'hint_retry_base_delay_sec') then Exit('Базовая задержка между повторными попытками (экспоненциальный backoff).');
        if SameText(AKey, 'hint_retry_max_delay_sec') then Exit('Верхняя граница задержки между попытками.');
        if SameText(AKey, 'hint_retry_jitter_pct') then Exit('Случайное отклонение задержки в процентах для разгрузки bursts.');
        if SameText(AKey, 'hint_purge_dead') then Exit('Удаляет все файлы из папки dead-letter очереди.');
      end;
    rlId:
      begin
        if SameText(AKey, 'app_title') then Exit('MailRelay untuk Oracle APEX');
        if SameText(AKey, 'tab_dashboard') then Exit('Dashboard');
        if SameText(AKey, 'tab_config') then Exit('Konfig');
        if SameText(AKey, 'tab_log') then Exit('Log');

        if SameText(AKey, 'group_dash_queue') then Exit('Antrean (ringkasan)');
        if SameText(AKey, 'group_dash_actions') then Exit('Aksi');
        if SameText(AKey, 'group_dash_relay_status') then Exit('Status relay');
        if SameText(AKey, 'group_dash_hourly') then Exit('Pesan per jam (24 jam terakhir)');
        if SameText(AKey, 'group_dash_inbound_distribution') then Exit('Distribusi email inbound');
        if SameText(AKey, 'group_dash_outbound_distribution') then Exit('Distribusi email outbound');
        if SameText(AKey, 'group_dash_problematic') then Exit('Persentase email bermasalah');

        if SameText(AKey, 'group_inbound_config') then Exit('Konfigurasi inbound');
        if SameText(AKey, 'group_inbound_auth') then Exit('Konfigurasi auth inbound');
        if SameText(AKey, 'group_outbound_config') then Exit('Konfigurasi outbound');
        if SameText(AKey, 'group_outbound_auth') then Exit('Kredensial auth outbound');
        if SameText(AKey, 'group_queue_config') then Exit('Antrean');
        if SameText(AKey, 'group_ui_config') then Exit('UI');
        if SameText(AKey, 'group_retry_config') then Exit('Retry');
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
        if SameText(AKey, 'relay_listener_status') then Exit('Status Listener:');
        if SameText(AKey, 'relay_senders_status') then Exit('Status Senders:');
        if SameText(AKey, 'relay_active_listener_sessions') then Exit('Active Listener Sessions:');
        if SameText(AKey, 'relay_active_senders') then Exit('Active Senders:');
        if SameText(AKey, 'inbound_auth_username') then Exit('Nama pengguna');
        if SameText(AKey, 'inbound_auth_password') then Exit('Kata sandi');
        if SameText(AKey, 'inbound_require_auth') then Exit('Wajibkan kredensial auth');
        if SameText(AKey, 'outbound_auth_username') then Exit('Nama pengguna SMTP');
        if SameText(AKey, 'outbound_auth_password') then Exit('Kata sandi SMTP');
        if SameText(AKey, 'queue_max_items') then Exit('Maks item antrean');
        if SameText(AKey, 'queue_max_bytes_mb') then Exit('Maks ukuran antrean (MB)');
        if SameText(AKey, 'queue_inflight_stale_sec') then Exit('Batas in-flight stale (detik)');
        if SameText(AKey, 'log_tail_lines') then Exit('Baris log tail');
        if SameText(AKey, 'stats_refresh_ms') then Exit('Refresh statistik (ms)');
        if SameText(AKey, 'retry_max_attempts') then Exit('Maks percobaan');
        if SameText(AKey, 'retry_base_delay_sec') then Exit('Delay dasar (detik)');
        if SameText(AKey, 'retry_max_delay_sec') then Exit('Delay maksimum (detik)');
        if SameText(AKey, 'retry_jitter_pct') then Exit('Jitter (%)');

        if SameText(AKey, 'btn_start_listener') then Exit('Mulai Listener');
        if SameText(AKey, 'btn_stop_listener') then Exit('Hentikan Listener');
        if SameText(AKey, 'btn_start_senders') then Exit('Mulai Sender');
        if SameText(AKey, 'btn_stop_senders') then Exit('Hentikan Sender');
        if SameText(AKey, 'btn_flush') then Exit('Kirim sekarang');
        if SameText(AKey, 'btn_purge_dead') then Exit('Hapus Dead-Letters');
        if SameText(AKey, 'btn_save') then Exit('Simpan');
        if SameText(AKey, 'btn_reload') then Exit('Muat Ulang');
        if SameText(AKey, 'btn_restore_defaults') then Exit('Kembalikan Default');
        if SameText(AKey, 'btn_clear_log') then Exit('Bersihkan Log');
        if SameText(AKey, 'btn_copy_log') then Exit('Salin Semua');
        if SameText(AKey, 'btn_edit_credentials') then Exit('Login/Kata Sandi SMTP');
        if SameText(AKey, 'btn_cred_save') then Exit('Simpan');
        if SameText(AKey, 'btn_cred_cancel') then Exit('Batal');
        if SameText(AKey, 'btn_reset') then Exit('Reset');
        if SameText(AKey, 'statusbar_made_by') then Exit('Dibuat oleh Krossel Apps');
        if SameText(AKey, 'dash_state_persist_note') then Exit('Server akan menyimpan status setelah restart.');
        if SameText(AKey, 'bind_ip_help_caption') then Exit('Bind IP menentukan antarmuka listener. Contoh: 127.0.0.1');
        if SameText(AKey, 'allowed_ip_help_caption') then Exit('Allowed Client IP menerima daftar IP dipisah koma. Contoh: 127.0.0.1, 212.12.27.17');
        if SameText(AKey, 'workers_help_caption') then Exit('Workers adalah jumlah thread pengiriman paralel. Sesuaikan dengan limit provider SMTP.');
        if SameText(AKey, 'detail_logging_help_caption') then Exit('Menambahkan baris log SMTP: C:, S:, C-DATA:, EVT:.');
        if SameText(AKey, 'retry_jitter_help_caption') then Exit('Jitter menambahkan variasi delay acak agar retry tidak meledak bersamaan.');
        if SameText(AKey, 'autosave_note') then Exit('Semua perubahan disimpan otomatis');

        if SameText(AKey, 'cred_title') then Exit('Kredensial SMTP');
        if SameText(AKey, 'cred_user') then Exit('Login');
        if SameText(AKey, 'cred_password') then Exit('Kata Sandi');

        if SameText(AKey, 'state_stopped') then Exit('BERHENTI');
        if SameText(AKey, 'state_listening') then Exit('BERJALAN');
        if SameText(AKey, 'state_running') then Exit('BERJALAN');
        if SameText(AKey, 'state_error') then Exit('ERROR');
        if SameText(AKey, 'state_unknown') then Exit('TIDAK DIKETAHUI');
        if SameText(AKey, 'chart_hourly_title') then Exit('Volume antrean per jam');
        if SameText(AKey, 'chart_inbound_title') then Exit('Distribusi inbound');
        if SameText(AKey, 'chart_outbound_title') then Exit('Distribusi outbound');
        if SameText(AKey, 'chart_problem_title') then Exit('Bermasalah vs sehat');
        if SameText(AKey, 'chart_no_data') then Exit('Tidak ada data');
        if SameText(AKey, 'chart_unknown') then Exit('tidak diketahui');
        if SameText(AKey, 'chart_problematic') then Exit('Bermasalah');
        if SameText(AKey, 'chart_healthy') then Exit('Sehat');

        if SameText(AKey, 'msg_field_int') then Exit('Field "%s" harus berupa angka.');
        if SameText(AKey, 'msg_port_inbound_int') then Exit('Inbound Bind Port harus berupa angka.');
        if SameText(AKey, 'msg_max_message_int') then Exit('Inbound Max Message Size harus berupa angka.');
        if SameText(AKey, 'msg_port_outbound_int') then Exit('Outbound Port harus berupa angka.');
        if SameText(AKey, 'msg_workers_int') then Exit('Outbound Workers harus berupa angka.');
        if SameText(AKey, 'msg_tls_invalid') then Exit('Mode TLS tidak valid.');
        if SameText(AKey, 'msg_config_invalid') then Exit('Validasi konfigurasi gagal:');
        if SameText(AKey, 'msg_listener_start_fail') then Exit('Gagal memulai listener:');
        if SameText(AKey, 'msg_senders_start_fail') then Exit('Gagal memulai sender:');
        if SameText(AKey, 'msg_purge_confirm') then Exit('Hapus semua file dead-letter?');
        if SameText(AKey, 'msg_defaults_confirm') then Exit('Reset nilai default? (kredensial outbound dipertahankan)');
        if SameText(AKey, 'msg_creds_saved') then Exit('Kredensial SMTP tersimpan (terenkripsi).');
        if SameText(AKey, 'msg_flush_no_senders') then Exit('Flush diminta, tetapi sender tidak berjalan.');

        if SameText(AKey, 'hint_bind_ip') then Exit('Alamat IP lokal untuk listener menerima koneksi SMTP dari APEX.');
        if SameText(AKey, 'hint_bind_port') then Exit('Port SMTP lokal listener. Umumnya 25 atau 2525.');
        if SameText(AKey, 'hint_allowed_ip') then Exit('Daftar IP klien yang diizinkan, pisahkan dengan koma. Contoh: 127.0.0.1, 212.12.27.17');
        if SameText(AKey, 'hint_max_message_mb') then Exit('Batas keras ukuran email masuk dalam MB.');
        if SameText(AKey, 'hint_remote_host') then Exit('Smarthost SMTP untuk pengiriman keluar.');
        if SameText(AKey, 'hint_remote_port') then Exit('Port smarthost SMTP (biasanya 587 STARTTLS, 465 SSL implicit).');
        if SameText(AKey, 'hint_tls_mode') then Exit('PLAIN tanpa TLS, STARTTLS upgrade TLS, SSL_IMPLICIT TLS sejak awal.');
        if SameText(AKey, 'hint_workers') then Exit('Jumlah thread pengiriman paralel. Perhatikan limit smarthost.');
        if SameText(AKey, 'hint_queue_path') then Exit('Path antrean durable spool di disk.');
        if SameText(AKey, 'hint_flush') then Exit('Mengubah semua email deferred menjadi due-now lalu membangunkan worker.');
        if SameText(AKey, 'hint_edit_credentials') then Exit('Buka editor login/kata sandi SMTP. Nilai di ini disimpan terenkripsi.');
        if SameText(AKey, 'hint_language') then Exit('Bahasa antarmuka aplikasi.');
        if SameText(AKey, 'hint_detail_logging') then Exit('Catat seluruh dialog SMTP masuk: C:/S:, perintah, respons, dan baris DATA.');
        if SameText(AKey, 'hint_inbound_auth_user') then Exit('Username untuk autentikasi SMTP inbound.');
        if SameText(AKey, 'hint_inbound_auth_password') then Exit('Password untuk autentikasi SMTP inbound.');
        if SameText(AKey, 'hint_inbound_require_auth') then Exit('Jika aktif dan kredensial diisi, MAIL FROM wajib AUTH sukses.');
        if SameText(AKey, 'hint_reset_inbound_auth') then Exit('Hapus username/password inbound dan nonaktifkan auth wajib.');
        if SameText(AKey, 'hint_outbound_auth_user') then Exit('Username untuk autentikasi SMTP smarthost outbound.');
        if SameText(AKey, 'hint_outbound_auth_password') then Exit('Password untuk autentikasi SMTP smarthost outbound.');
        if SameText(AKey, 'hint_queue_max_items') then Exit('Jumlah maksimal pesan di antrean new+inflight+deferred.');
        if SameText(AKey, 'hint_queue_max_bytes_mb') then Exit('Ukuran total maksimal antrean dalam megabyte.');
        if SameText(AKey, 'hint_queue_inflight_stale_sec') then Exit('Detik sebelum pesan in-flight dianggap stale dan dipindah ke deferred.');
        if SameText(AKey, 'hint_log_tail_lines') then Exit('Jumlah baris log yang ditampilkan di memo UI.');
        if SameText(AKey, 'hint_stats_refresh_ms') then Exit('Interval refresh dashboard dan log dalam milidetik.');
        if SameText(AKey, 'hint_retry_max_attempts') then Exit('Jumlah percobaan kirim sebelum masuk dead-letter.');
        if SameText(AKey, 'hint_retry_base_delay_sec') then Exit('Delay dasar antara retry (exponential backoff).');
        if SameText(AKey, 'hint_retry_max_delay_sec') then Exit('Batas maksimum delay retry.');
        if SameText(AKey, 'hint_retry_jitter_pct') then Exit('Variasi acak delay dalam persen untuk mencegah burst.');
        if SameText(AKey, 'hint_purge_dead') then Exit('Menghapus semua file dari folder dead-letter antrean.');
      end;
  else
    begin
      if SameText(AKey, 'app_title') then Exit('MailRelay for Oracle APEX');
      if SameText(AKey, 'tab_dashboard') then Exit('Dashboard');
      if SameText(AKey, 'tab_config') then Exit('Config');
      if SameText(AKey, 'tab_log') then Exit('Log');

      if SameText(AKey, 'group_dash_queue') then Exit('Queue summary');
      if SameText(AKey, 'group_dash_actions') then Exit('Actions');
      if SameText(AKey, 'group_dash_relay_status') then Exit('Relay status');
      if SameText(AKey, 'group_dash_hourly') then Exit('Messages per hour (last 24h)');
      if SameText(AKey, 'group_dash_inbound_distribution') then Exit('Mail distribution inbound');
      if SameText(AKey, 'group_dash_outbound_distribution') then Exit('Mail distribution outbound');
      if SameText(AKey, 'group_dash_problematic') then Exit('Problematic mail percentage');

      if SameText(AKey, 'group_inbound_config') then Exit('Inbound config');
      if SameText(AKey, 'group_inbound_auth') then Exit('Inbound auth config');
      if SameText(AKey, 'group_outbound_config') then Exit('Outbound config');
      if SameText(AKey, 'group_outbound_auth') then Exit('Outbound auth credentials');
      if SameText(AKey, 'group_queue_config') then Exit('Queue');
      if SameText(AKey, 'group_ui_config') then Exit('UI');
      if SameText(AKey, 'group_retry_config') then Exit('Retry');
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
      if SameText(AKey, 'relay_listener_status') then Exit('Status Listener:');
      if SameText(AKey, 'relay_senders_status') then Exit('Status Senders:');
      if SameText(AKey, 'relay_active_listener_sessions') then Exit('Active Listener Sessions:');
      if SameText(AKey, 'relay_active_senders') then Exit('Active Senders:');
      if SameText(AKey, 'inbound_auth_username') then Exit('Username');
      if SameText(AKey, 'inbound_auth_password') then Exit('Password');
      if SameText(AKey, 'inbound_require_auth') then Exit('Require auth credentials');
      if SameText(AKey, 'outbound_auth_username') then Exit('SMTP username');
      if SameText(AKey, 'outbound_auth_password') then Exit('SMTP password');
      if SameText(AKey, 'queue_max_items') then Exit('Max queue items');
      if SameText(AKey, 'queue_max_bytes_mb') then Exit('Max queue bytes (MB)');
      if SameText(AKey, 'queue_inflight_stale_sec') then Exit('In-flight stale timeout (sec)');
      if SameText(AKey, 'log_tail_lines') then Exit('Log tail lines');
      if SameText(AKey, 'stats_refresh_ms') then Exit('Stats refresh (ms)');
      if SameText(AKey, 'retry_max_attempts') then Exit('Retry max attempts');
      if SameText(AKey, 'retry_base_delay_sec') then Exit('Retry base delay (sec)');
      if SameText(AKey, 'retry_max_delay_sec') then Exit('Retry max delay (sec)');
      if SameText(AKey, 'retry_jitter_pct') then Exit('Retry jitter (%)');

      if SameText(AKey, 'btn_start_listener') then Exit('Start Listener');
      if SameText(AKey, 'btn_stop_listener') then Exit('Stop Listener');
      if SameText(AKey, 'btn_start_senders') then Exit('Start Senders');
      if SameText(AKey, 'btn_stop_senders') then Exit('Stop Senders');
      if SameText(AKey, 'btn_flush') then Exit('Flush now');
      if SameText(AKey, 'btn_purge_dead') then Exit('Purge Dead-Letters');
      if SameText(AKey, 'btn_save') then Exit('Save');
      if SameText(AKey, 'btn_reload') then Exit('Reload');
      if SameText(AKey, 'btn_restore_defaults') then Exit('Restore Defaults');
      if SameText(AKey, 'btn_clear_log') then Exit('Clear Log');
      if SameText(AKey, 'btn_copy_log') then Exit('Copy All');
      if SameText(AKey, 'btn_edit_credentials') then Exit('SMTP Credentials');
      if SameText(AKey, 'btn_cred_save') then Exit('Save');
      if SameText(AKey, 'btn_cred_cancel') then Exit('Cancel');
      if SameText(AKey, 'btn_reset') then Exit('Reset');
      if SameText(AKey, 'statusbar_made_by') then Exit('Made by Krossel Apps');
      if SameText(AKey, 'dash_state_persist_note') then Exit('Server will save it''s state after restart.');
      if SameText(AKey, 'bind_ip_help_caption') then Exit('Bind IP controls listener network interface. Example: 127.0.0.1');
      if SameText(AKey, 'allowed_ip_help_caption') then Exit('Allowed Client IP accepts comma-separated client list. Example: 127.0.0.1, 212.12.27.17');
      if SameText(AKey, 'workers_help_caption') then Exit('Workers are parallel sender threads. Increase carefully based on SMTP provider limits.');
      if SameText(AKey, 'detail_logging_help_caption') then Exit('Adds extra SMTP trace lines: C:, S:, C-DATA:, EVT:.');
      if SameText(AKey, 'retry_jitter_help_caption') then Exit('Jitter adds random delay variance to avoid retry bursts and synchronized reconnect spikes.');
      if SameText(AKey, 'autosave_note') then Exit('All changes will be saved automatically');

      if SameText(AKey, 'cred_title') then Exit('SMTP Credentials');
      if SameText(AKey, 'cred_user') then Exit('Login');
      if SameText(AKey, 'cred_password') then Exit('Password');

      if SameText(AKey, 'state_stopped') then Exit('STOPPED');
      if SameText(AKey, 'state_listening') then Exit('RUNNING');
      if SameText(AKey, 'state_running') then Exit('RUNNING');
      if SameText(AKey, 'state_error') then Exit('ERROR');
      if SameText(AKey, 'state_unknown') then Exit('UNKNOWN');
      if SameText(AKey, 'chart_hourly_title') then Exit('Queue volume by hour');
      if SameText(AKey, 'chart_inbound_title') then Exit('Inbound distribution');
      if SameText(AKey, 'chart_outbound_title') then Exit('Outbound distribution');
      if SameText(AKey, 'chart_problem_title') then Exit('Problematic vs healthy queue');
      if SameText(AKey, 'chart_no_data') then Exit('No data');
      if SameText(AKey, 'chart_unknown') then Exit('(unknown)');
      if SameText(AKey, 'chart_problematic') then Exit('Problematic');
      if SameText(AKey, 'chart_healthy') then Exit('Healthy');

      if SameText(AKey, 'msg_field_int') then Exit('Field "%s" must be an integer.');
      if SameText(AKey, 'msg_port_inbound_int') then Exit('Inbound Bind Port must be an integer.');
      if SameText(AKey, 'msg_max_message_int') then Exit('Inbound Max Message Size must be an integer.');
      if SameText(AKey, 'msg_port_outbound_int') then Exit('Outbound Port must be an integer.');
      if SameText(AKey, 'msg_workers_int') then Exit('Outbound workers must be an integer.');
      if SameText(AKey, 'msg_tls_invalid') then Exit('TLS mode is invalid.');
      if SameText(AKey, 'msg_config_invalid') then Exit('Config validation failed:');
      if SameText(AKey, 'msg_listener_start_fail') then Exit('Cannot start listener:');
      if SameText(AKey, 'msg_senders_start_fail') then Exit('Cannot start senders:');
      if SameText(AKey, 'msg_purge_confirm') then Exit('Purge all dead-letter files?');
      if SameText(AKey, 'msg_defaults_confirm') then Exit('Reset values to defaults? (outbound credentials are preserved)');
      if SameText(AKey, 'msg_creds_saved') then Exit('SMTP credentials saved (encrypted).');
      if SameText(AKey, 'msg_flush_no_senders') then Exit('Flush requested, but senders are not running.');

      if SameText(AKey, 'hint_bind_ip') then Exit('Local IP where the listener accepts SMTP connections from APEX.');
      if SameText(AKey, 'hint_bind_port') then Exit('Local listener SMTP port. Usually 25 or 2525.');
      if SameText(AKey, 'hint_allowed_ip') then Exit('Comma-separated list of allowed client IPs. Example: 127.0.0.1, 212.12.27.17');
      if SameText(AKey, 'hint_max_message_mb') then Exit('Hard limit for inbound message size in megabytes.');
      if SameText(AKey, 'hint_remote_host') then Exit('Smarthost SMTP server used for outbound delivery.');
      if SameText(AKey, 'hint_remote_port') then Exit('Smarthost SMTP port (usually 587 for STARTTLS, 465 for implicit SSL).');
      if SameText(AKey, 'hint_tls_mode') then Exit('PLAIN for no TLS, STARTTLS for explicit TLS upgrade, SSL_IMPLICIT for direct TLS.');
      if SameText(AKey, 'hint_workers') then Exit('Number of parallel sender worker threads. Respect smarthost limits.');
      if SameText(AKey, 'hint_queue_path') then Exit('Disk path to the durable spool queue.');
      if SameText(AKey, 'hint_flush') then Exit('Marks deferred items as due now and wakes sender workers immediately.');
      if SameText(AKey, 'hint_edit_credentials') then Exit('Open SMTP login/password editor. Values are stored encrypted in ini.');
      if SameText(AKey, 'hint_language') then Exit('Application UI language.');
      if SameText(AKey, 'hint_detail_logging') then Exit('Logs full inbound SMTP dialog: C:/S:, commands, replies, and DATA lines.');
      if SameText(AKey, 'hint_inbound_auth_user') then Exit('Username for inbound SMTP authentication.');
      if SameText(AKey, 'hint_inbound_auth_password') then Exit('Password for inbound SMTP authentication.');
      if SameText(AKey, 'hint_inbound_require_auth') then Exit('When enabled and credentials exist, MAIL FROM requires successful AUTH.');
      if SameText(AKey, 'hint_reset_inbound_auth') then Exit('Clears inbound username/password and disables required authentication flag.');
      if SameText(AKey, 'hint_outbound_auth_user') then Exit('Username used for outbound smarthost SMTP authentication.');
      if SameText(AKey, 'hint_outbound_auth_password') then Exit('Password used for outbound smarthost SMTP authentication.');
      if SameText(AKey, 'hint_queue_max_items') then Exit('Maximum number of messages in new+inflight+deferred queue states.');
      if SameText(AKey, 'hint_queue_max_bytes_mb') then Exit('Maximum total queue size in megabytes.');
      if SameText(AKey, 'hint_queue_inflight_stale_sec') then Exit('How long before an in-flight item is considered stale and moved to deferred.');
      if SameText(AKey, 'hint_log_tail_lines') then Exit('Number of log lines retained and shown in the UI memo.');
      if SameText(AKey, 'hint_stats_refresh_ms') then Exit('Dashboard and log refresh interval in milliseconds.');
      if SameText(AKey, 'hint_retry_max_attempts') then Exit('How many delivery attempts before moving message to dead-letter.');
      if SameText(AKey, 'hint_retry_base_delay_sec') then Exit('Base retry delay in seconds for exponential backoff.');
      if SameText(AKey, 'hint_retry_max_delay_sec') then Exit('Upper bound for retry delay in seconds.');
      if SameText(AKey, 'hint_retry_jitter_pct') then Exit('Random delay variation percentage to reduce retry bursts.');
      if SameText(AKey, 'hint_purge_dead') then Exit('Deletes all files from the dead-letter queue directory.');
    end;
  end;

  Result := AKey;
end;

end.
