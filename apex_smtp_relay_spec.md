# SMTP Relay для Oracle APEX (Delphi 12) — Production Specification (Fail-Safe)

> **Назначение:** локальный SMTP-relay на Windows, который принимает письма **только от Oracle APEX** (единственный клиент) и **переадресует** их на внешний SMTP-сервер (smarthost).  
> **Фокус:** одна функция — *переадресация того, что APEX пришлёт*.  
> **Приоритет:** отказоустойчивость, корректная очередь, ретраи, отсутствие потерь, безопасный доступ.  
> **UI/настройки:** одна главная форма; конфиг в INI; логи только в Memo (хвост 1000 строк), **на диск логи не пишем**.

---

## 0. Термины и границы системы

### 0.1. Inbound / Outbound
- **Inbound (приём):** APEX подключается к relay по SMTP и отправляет письмо.
- **Outbound (доставка):** relay отправляет это письмо на внешний SMTP-сервер (smarthost).

### 0.2. Надёжность и “не писать логи на диск”
- Логи (debug/trace) — **только в памяти** (Memo UI).
- Но **очередь** для отказоустойчивости должна быть **устойчива к падениям процесса**.  
  → Следовательно, **спул/очередь письма хранится на диске** (минимально необходимый durable storage).  
  Логи на диск не пишем, но спул писем — пишем, иначе “production fail-safe” невозможен при рестарте/краше.

---

## 1. Требования

### 1.1. Функциональные
1. Принимать SMTP-сообщения от Oracle APEX.
2. Поддерживать базовый SMTP диалог, достаточный для APEX:
   - `EHLO`/`HELO`
   - `MAIL FROM:`
   - `RCPT TO:` (1..N)
   - `DATA` (приём тела до `<CRLF>.<CRLF>`)
   - `RSET`, `NOOP`, `QUIT`
3. Ограничить доступ:
   - Inbound разрешён **только** от одного клиента (APEX):
     - либо `127.0.0.1` (если APEX на том же сервере),
     - либо конкретный IP APEX-сервера (если APEX/DB на другом хосте).
4. Переадресовывать сообщения на внешний SMTP:
   - настройки: `host`, `port`, `TLS/SSL mode`.
   - SMTP AUTH **не настраиваем через UI** (по требованиям); но система должна:
     - либо поддерживать отсутствие auth,
     - либо иметь **встроенный режим** auth (параметры могут быть захардкожены/подготовлены в коде для конкретного провайдера), **но в UI не выводить**.
5. Очередь/ретраи:
   - сообщения не теряются при рестарте приложения/краше.
   - гарантировать *at-least-once delivery* на smarthost.
6. Метрики/статус:
   - UI показывает состояние (LISTENING, QUEUE SIZE, ACTIVE SENDERS, LAST ERROR).
7. Управление:
   - Start/Stop listener
   - Start/Stop sender workers
   - Flush queue (attempt now)

### 1.2. Нефункциональные
- **Отказоустойчивость:** при падении процесса/перезагрузке Windows очередь восстанавливается.
- **Производительность:** выдерживать bursts APEX (например, пачки уведомлений).
- **Безопасность:** no open relay; ограничение клиента; ограничение размера; защита от DoS со стороны клиента (хотя клиент один).
- **Простота:** одна форма, INI, минимум настроек.

---

## 2. Технологический стек

- **Delphi 12**
- **Indy** (рекомендуется, так как зрелая реализация SMTP/TLS):
  - Inbound: `TIdTCPServer` (или `TIdSMTPServer` при условии детального контроля; предпочтительнее `TIdTCPServer` для полного управления протоколом).
  - Outbound: `TIdSMTP` + `TIdSSLIOHandlerSocketOpenSSL` (OpenSSL DLL рядом с exe).
- **Threading:**
  - Inbound sessions: отдельный поток на соединение (Indy worker thread).
  - Outbound sending: пул worker threads (настраиваемый, по умолчанию 2).
- **Persistent queue:** файловый spool (структура каталогов + атомарные rename операции).

---

## 3. Архитектура

### 3.1. Подсистемы
1. **UI Layer**
   - MainForm: конфиг, статусы, лог-memo, кнопки управления.
2. **Config Layer**
   - чтение/запись INI
   - валидация
3. **Inbound SMTP Layer**
   - listener на `bind_ip:bind_port`
   - state machine SMTP per session
4. **Queue Layer**
   - durable spool on disk
   - транзакции через rename/move
   - idempotency keys
5. **Outbound Sender Layer**
   - dispatcher workers
   - TLS/SSL handshake
   - retry policy
6. **Diagnostics Layer**
   - in-memory ring buffer log (1000 lines)
   - counters + last error

### 3.2. Потоки и жизненный цикл
- Main thread: UI.
- Listener thread: поднимает Indy server, принимает connections.
- Per-connection worker threads: обслуживают SMTP state machine.
- Sender pool: N потоков, каждый:
  1) берёт письмо из очереди
  2) отправляет
  3) на успех: commit (delete/mark)
  4) на ошибку: reschedule (retry metadata)
- Housekeeper thread (опционально, 1 поток):
  - чистит “stuck in-flight”
  - проверяет TTL/expired
  - обновляет статистику раз в N секунд

---

## 4. UI/UX спецификация (MainForm)

### 4.1. Layout (одна форма)
#### Секция “Inbound (APEX → Relay)”
- **Bind IP** (TEdit, default `127.0.0.1`)
- **Bind Port** (TEdit numeric, default `25` или `2525` если порт 25 занят)
- **Allowed Client IP** (TEdit, default `127.0.0.1`)
- **Max Message Size (MB)** (TEdit numeric, default `20`)
- **Start Listener** (TButton)
- **Stop Listener** (TButton)
- **Status** (TLabel): `STOPPED | LISTENING | ERROR`

#### Секция “Outbound (Relay → Smarthost)”
- **Remote Host** (TEdit, required)
- **Remote Port** (TEdit numeric, default `587`)
- **TLS/SSL Mode** (TComboBox):
  - `PLAIN` (no TLS)
  - `STARTTLS` (explicit TLS upgrade)
  - `SSL_IMPLICIT` (SMTPS; TLS сразу)
- **Workers** (TEdit numeric, default `2`, min `1`, max `8`)
- **Start Senders** (TButton)
- **Stop Senders** (TButton)
- **Flush Now** (TButton): сигнал воркерам “попробовать немедленно”

#### Секция “Queue”
- **Queue Path** (TEdit, default `.\\spool`)
- **Queue Size** (TLabel)
- **In-flight** (TLabel)
- **Deferred** (TLabel)
- **Oldest Age** (TLabel)
- **Purge Dead-Letters** (TButton, с подтверждением)

#### Секция “Logs”
- **MemoLog** (TMemo, readonly, monospace)
- Хранить только **последние 1000 строк**
- **Clear Log** (TButton)
- **Copy All** (TButton)

#### Секция “Config”
- **Save** (TButton)
- **Reload** (TButton)
- **Restore Defaults** (TButton + Confirm)

### 4.2. Поведение UI
- Все операции start/stop — асинхронные, UI не блокируется.
- Любое исключение подсистем — отображать в Status + лог.
- При изменении настроек:
  - если listener/senders running — предложить restart соответствующей подсистемы.

---

## 5. Конфигурация INI

### 5.1. Файл и путь
- `relay.ini` рядом с exe.
- Кодировка ANSI/UTF-8 без BOM (допускается).

### 5.2. Секции и ключи
```ini
[Inbound]
BindIP=127.0.0.1
BindPort=25
AllowedClientIP=127.0.0.1
MaxMessageSizeMB=20
SessionIdleTimeoutSec=60
CommandTimeoutSec=30

[Outbound]
Host=smtp.example.com
Port=587
TlsMode=STARTTLS ; PLAIN|STARTTLS|SSL_IMPLICIT
Workers=2
ConnectTimeoutSec=10
ReadTimeoutSec=30
WriteTimeoutSec=30

[Queue]
Path=.\spool
MaxQueueItems=100000
MaxQueueBytesMB=2048
InFlightStaleSec=600

[Retry]
MaxAttempts=12
BaseDelaySec=10
MaxDelaySec=3600
JitterPct=20

[UI]
LogTailLines=1000
StatsRefreshMs=1000
```

### 5.3. Валидация
- `BindPort`, `Port`: 1..65535
- `AllowedClientIP`: IPv4 only (в рамках MVP), при ошибке — блокировать Save.
- `TlsMode`: enum
- `Workers`: 1..8
- `MaxMessageSizeMB`: 1..100 (или другое ограничение)

---

## 6. Inbound SMTP — протокол и state machine

### 6.1. Ограничение клиента
- После accept socket:
  - сравнить `PeerIP` с `AllowedClientIP`
  - если не совпадает → **немедленно close** (без greeting), либо greeting `554` и close.
  - Рекомендуется: greeting `554 Access denied` и close, чтобы было видно в диагностике (но APEX не должен подключаться извне).

### 6.2. SMTP Greeting
- При успешном accept:
  - отправить `220 <local-hostname> ESMTP APEX-Relay`
- `<local-hostname>`: использовать `BindIP`/machine name или `kapps.at` (не критично).

### 6.3. Команды и ответы
#### EHLO/HELO
- `EHLO x` → `250-<hostname>` + capabilities:
  - `250-SIZE <max-bytes>`
  - `250-8BITMIME`
  - `250 PIPELINING` (опционально)
  - `250 HELP`
- `HELO x` → `250 <hostname>`

#### MAIL FROM
- Требуется после EHLO/HELO.
- Ответ: `250 2.1.0 Sender OK`

#### RCPT TO
- Разрешать 1..N получателей.
- Ответ: `250 2.1.5 Recipient OK`
- Не проверять домены/существование (relay).

#### DATA
- Если нет RCPT → `503 5.5.1 Need RCPT TO first`
- Иначе:
  - `354 End data with <CRLF>.<CRLF>`
  - Принимать до terminator
  - Поддержать dot-stuffing: строка `..x` → `.x`

#### RSET
- сброс транзакции (очистить FROM/RCPT/BUFFER)
- `250 2.0.0 OK`

#### NOOP
- `250 2.0.0 OK`

#### QUIT
- `221 2.0.0 Bye` + close

### 6.4. Лимиты/таймауты
- `MaxMessageSizeMB`: hard limit.
  - при превышении: `552 5.3.4 Message size exceeds fixed maximum message size`
- Idle timeout для сессии: закрыть.
- Command timeout: если команда не завершена — закрыть.

### 6.5. Параллельность
- Одновременно активных inbound sessions: лимит (например 20).
- Если превышение → `421 4.3.2 Service not available` и close.

---

## 7. Очередь (Durable spool) — отказоустойчивость

### 7.1. Цель
- Никаких потерь при падении процесса.
- Минимум зависимостей.
- Атомарность: использовать операции rename/move.

### 7.2. Структура каталогов
В `Queue.Path`:
- `new\` — принятые, готовые к отправке
- `inflight\` — взятые воркером
- `deferred\` — отложенные до времени retry
- `dead\` — исчерпаны ретраи / невалидны
- `tmp\` — временные файлы при записи

### 7.3. Формат spool item
Каждое письмо = 2 файла:
- `<id>.eml` — сырое сообщение RFC822 (headers + body)
- `<id>.json` — метаданные (retry, timestamps)

`<id>` = ULID/UUIDv4 (строка).

**Метаданные JSON:**
```json
{
  "id": "01HS....",
  "createdUtc": "2026-02-23T08:00:00Z",
  "attempt": 0,
  "nextAttemptUtc": "2026-02-23T08:00:00Z",
  "lastError": "",
  "lastErrorAtUtc": null,
  "from": "noreply@kapps.at",
  "rcpt": ["admin@kapps.at"],
  "sizeBytes": 12345
}
```

### 7.4. Приём письма → запись в очередь (atomic)
Алгоритм:
1. Создать `<id>.eml` в `tmp\`.
2. Создать `<id>.json` в `tmp\`.
3. Атомарно переместить оба файла в `new\` (rename).
4. Только после этого отвечать клиенту `250 Queued`.

### 7.5. Выбор письма воркером
- Приоритет: `new\` → `deferred\` где `nextAttemptUtc <= now`
- Взятие: атомарный move в `inflight\`.

### 7.6. Коммит/откат
- Успех: удалить `inflight\<id>.eml/.json`
- Ошибка:
  - `attempt++`, рассчитать `nextAttemptUtc`, записать `lastError`
  - переместить в `deferred\` или `dead\`.

### 7.7. Восстановление после краша
- На старте:
  - просканировать `inflight\`
  - если старше `InFlightStaleSec` → вернуть в `deferred\` с `nextAttemptUtc=now`.

### 7.8. Ограничения очереди
- `MaxQueueItems`, `MaxQueueBytesMB`:
  - превышение → inbound отвечает `452 4.3.1 Insufficient system storage`.

---

## 8. Outbound отправка — TLS/SSL, ошибки, ретраи

### 8.1. TLS/SSL Mode
- `PLAIN`: без TLS
- `STARTTLS`: explicit TLS upgrade
- `SSL_IMPLICIT`: TLS сразу (обычно 465)

### 8.2. Классификация ошибок и retry policy
- 4xx → retry
- 5xx → permanent → dead
- connect/timeout → retry
- auth failed (535/530) → permanent + “CONFIG ERROR”

### 8.3. Backoff
Экспоненциальный:
- `delay = min(MaxDelaySec, BaseDelaySec * 2^attempt)`
- jitter ±`JitterPct`%

### 8.4. Concurrency
- `Workers` потоков; пустая очередь → sleep 500–2000ms.

---

## 9. Логи и диагностика (только память)

### 9.1. Ring buffer (1000 строк)
- thread-safe
- UI обновлять таймером

### 9.2. Обязательные события
- start/stop listener/senders
- accept/deny connect
- enqueue/dequeue/commit/defer/dead/recover
- outbound connect/TLS/send/result
- конфиг load/save

---

## 10. Безопасность
- Bind по умолчанию на `127.0.0.1`
- AllowedClientIP строго один
- лимиты и таймауты
- no open relay

---

## 11. Деплой
- GUI EXE, автозапуск через Task Scheduler (recommended), либо отдельный сервис (опционально)
- OpenSSL DLL рядом с exe для TLS режимов Indy

---

## 12. Приёмка (Definition of Done)
1. APEX отправляет без auth на relay.
2. Письма доставляются на smarthost с заданным TLS/SSL.
3. При падении процесса письма не теряются (spool).
4. Retry/Dead-letter работают.
5. UI показывает статус/очередь/логи (1000 строк).
6. Доступ только от APEX (AllowedClientIP).
