# SMTP Relay Documentation

## Russian (Русский)

### Назначение

Локальный SMTP relay для Oracle APEX на Windows:

1. Принимает SMTP только от `AllowedClientIP`.
2. Складывает письма в надежную очередь на диске (`spool`).
3. Отправляет письма на внешний SMTP (smarthost) с ретраями.

### Inbound AUTH (APEX -> Relay)

Поддерживаемые механизмы:

1. `AUTH CRAM-MD5`
2. `AUTH LOGIN`
3. `AUTH PLAIN`

Режимы работы:

1. Если `[Inbound] AuthUser` и `[Inbound] AuthPassword` пустые, AUTH принимается (compat mode).
2. Если inbound-креды заданы, сервер проверяет их.
3. При заданных inbound-кредах команда `MAIL FROM` разрешается только после успешного AUTH.

### Outbound AUTH (Relay -> Smarthost)

1. Креды smarthost задаются в UI через блок Credentials.
2. В `relay.ini` сохраняются в `[Outbound] AuthUser/AuthPassword`.
3. Значения сохраняются в зашифрованном виде и одной строкой.
4. При отправке письмо использует дешифровку "на лету".

### Основные разделы UI

1. **Inbound**: bind IP/port, allowed IP, max size, start/stop listener.
2. **Outbound**: host/port, TLS mode, workers, start/stop senders, flush.
3. **Queue**: queue size, inflight/deferred/dead, purge dead.
4. **Logs**: in-memory ring log.
5. **Config**: save/reload/defaults, language, detail logging.
6. **Credentials**: скрытая панель редактирования логина/пароля, пароль маскируется.

### Детальное логирование SMTP

При включенном `Enable detail logging` логируются:

1. Команды клиента (`C:`)
2. Ответы сервера (`S:`)
3. DATA-строки (`C-DATA:`)
4. События сессии (`EVT:`)

### `relay.ini` (ключевые секции)

1. `[Inbound]`
2. `[Outbound]`
3. `[Queue]`
4. `[Retry]`
5. `[UI]`
6. `[State]`

Пример:

```ini
[Inbound]
BindIP=127.0.0.1
BindPort=2525
AllowedClientIP=127.0.0.1
MaxMessageSizeMB=20
SessionIdleTimeoutSec=60
CommandTimeoutSec=30
AuthUser=
AuthPassword=

[Outbound]
Host=smtp.example.com
Port=587
TlsMode=STARTTLS
Workers=2
ConnectTimeoutSec=10
ReadTimeoutSec=30
WriteTimeoutSec=30
AuthUser=dpapi:...
AuthPassword=dpapi:...

[UI]
Language=EN
EnableDetailLogging=0

[State]
ListenerStarted=0
SendersStarted=0
```

### Безопасность credential-данных

1. Логин/пароль хранятся в INI в зашифрованном виде.
2. Шифротекст сохраняется в одну строку (без `CR/LF`).
3. В runtime креды нормализуются и используются только при необходимости.

### Автовосстановление состояния

1. При закрытии приложения сохраняются флаги `ListenerStarted` и `SendersStarted`.
2. При следующем старте приложение пытается автоматически поднять эти подсистемы.

### Диагностика

1. В лог пишется активный путь конфигурации: `Using INI: ...`.
2. Для outbound AUTH пишутся строки `SMTP auth enabled user=... passLen=...`.
3. Если smarthost отвечает `535`, проверьте:
   - что редактируете тот же INI, который показан в `Using INI: ...`;
   - корректность `[Outbound] AuthUser/AuthPassword`;
   - TLS mode/port в `[Outbound]`.

---

## English

### Purpose

Local Windows SMTP relay for Oracle APEX:

1. Accepts inbound SMTP only from `AllowedClientIP`.
2. Stores messages in durable on-disk spool.
3. Delivers messages to external smarthost with retry/defer/dead-letter flow.

### Inbound AUTH (APEX -> Relay)

Supported mechanisms:

1. `AUTH CRAM-MD5`
2. `AUTH LOGIN`
3. `AUTH PLAIN`

Modes:

1. If `[Inbound] AuthUser` and `[Inbound] AuthPassword` are empty, AUTH is accepted (compat mode).
2. If inbound credentials are configured, AUTH is validated.
3. When inbound credentials are configured, `MAIL FROM` is allowed only after successful AUTH.

### Outbound AUTH (Relay -> Smarthost)

1. Smarthost credentials are edited in UI Credentials panel.
2. Stored in `[Outbound] AuthUser/AuthPassword`.
3. Values are encrypted and saved as single-line strings.
4. Decryption is done on demand during send.

### Main UI Areas

1. **Inbound**: bind IP/port, allowed IP, max size, listener start/stop.
2. **Outbound**: host/port, TLS mode, workers, sender start/stop, flush.
3. **Queue**: queue metrics and dead-letter purge.
4. **Logs**: in-memory ring log.
5. **Config**: save/reload/defaults, language, detail logging.
6. **Credentials**: hidden login/password editor; password is masked.

### Detailed SMTP Logging

When `Enable detail logging` is enabled, relay logs:

1. Client commands (`C:`)
2. Server replies (`S:`)
3. DATA lines (`C-DATA:`)
4. Session events (`EVT:`)

### `relay.ini` Sections

1. `[Inbound]`
2. `[Outbound]`
3. `[Queue]`
4. `[Retry]`
5. `[UI]`
6. `[State]`

### Credential Security

1. Credentials are encrypted in INI.
2. Encrypted values are single-line strings.
3. Runtime decrypts credentials only when required.

### Runtime State Persistence

1. On shutdown, `ListenerStarted` and `SendersStarted` are persisted.
2. On next start, relay auto-starts those subsystems if they were previously running.

### Diagnostics

1. Active config path is logged as `Using INI: ...`.
2. Outbound auth status is logged as `SMTP auth enabled user=... passLen=...`.
3. For `SMTP 535` from smarthost, verify active INI path, outbound credentials, TLS mode, and port.

---

## Indonesian (Bahasa Indonesia)

### Tujuan

SMTP relay lokal Windows untuk Oracle APEX:

1. Menerima SMTP masuk hanya dari `AllowedClientIP`.
2. Menyimpan pesan ke spool tahan-crash di disk.
3. Mengirim pesan ke smarthost eksternal dengan mekanisme retry/defer/dead-letter.

### Inbound AUTH (APEX -> Relay)

Mekanisme yang didukung:

1. `AUTH CRAM-MD5`
2. `AUTH LOGIN`
3. `AUTH PLAIN`

Mode:

1. Jika `[Inbound] AuthUser` dan `[Inbound] AuthPassword` kosong, AUTH diterima (compat mode).
2. Jika kredensial inbound diisi, AUTH akan divalidasi.
3. Saat kredensial inbound diisi, `MAIL FROM` hanya boleh setelah AUTH sukses.

### Outbound AUTH (Relay -> Smarthost)

1. Kredensial smarthost diubah lewat panel Credentials di UI.
2. Disimpan di `[Outbound] AuthUser/AuthPassword`.
3. Nilai disimpan terenkripsi dalam satu baris.
4. Dekripsi dilakukan saat diperlukan ketika mengirim.

### Area UI Utama

1. **Inbound**: bind IP/port, allowed IP, batas ukuran, start/stop listener.
2. **Outbound**: host/port, mode TLS, jumlah worker, start/stop sender, flush.
3. **Queue**: metrik antrean dan purge dead-letter.
4. **Logs**: ring log di memori.
5. **Config**: simpan/muat ulang/default, bahasa, detail logging.
6. **Credentials**: panel login/password tersembunyi; password dimask.

### Logging SMTP Detail

Saat `Enable detail logging` aktif, relay mencatat:

1. Perintah klien (`C:`)
2. Balasan server (`S:`)
3. Baris DATA (`C-DATA:`)
4. Event sesi (`EVT:`)

### Bagian `relay.ini`

1. `[Inbound]`
2. `[Outbound]`
3. `[Queue]`
4. `[Retry]`
5. `[UI]`
6. `[State]`

### Keamanan Kredensial

1. Kredensial disimpan terenkripsi di INI.
2. Nilai terenkripsi disimpan satu baris.
3. Runtime hanya mendekripsi saat benar-benar diperlukan.

### Persistensi Status Runtime

1. Saat aplikasi ditutup, `ListenerStarted` dan `SendersStarted` disimpan.
2. Saat start berikutnya, relay mencoba auto-start sesuai status terakhir.

### Diagnostik

1. Path konfigurasi aktif dicatat sebagai `Using INI: ...`.
2. Status outbound auth dicatat sebagai `SMTP auth enabled user=... passLen=...`.
3. Jika smarthost membalas `SMTP 535`, cek path INI aktif, kredensial outbound, mode TLS, dan port.
