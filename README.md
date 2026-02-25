# Athexa APEX SMTP Relay

Windows SMTP relay for Oracle APEX (Delphi 12, Indy) with durable disk spool, retry policy, and restricted inbound access.

## What Is New In UI

1. Static DFM-based UI (no runtime component generation).
2. Tabs: `Dashboard`, `Config`, `Log`.
3. Dashboard includes:
   - Queue summary block,
   - Actions block,
   - Relay status block,
   - `Messages per hour (last 24h)` chart,
   - pie charts for inbound/outbound distribution and problematic percentage.
4. Config tab includes grouped settings for:
   - Inbound,
   - Inbound auth,
   - Outbound,
   - Outbound auth,
   - Queue,
   - UI config,
   - Retry policy.
5. Log tab contains live in-memory log memo.
6. Multi-language captions/hints: English, Russian, Indonesian.

## Highlights

1. Inbound SMTP listener for APEX only (`AllowedClientIP`).
2. Inbound SMTP auth support: `CRAM-MD5`, `LOGIN`, `PLAIN`.
3. Inbound auth requirement logic:
   - if `RequireAuthCredentials = false`, anonymous sending is allowed;
   - if `RequireAuthCredentials = true` and inbound credentials are filled, `MAIL FROM` requires successful AUTH;
   - if inbound username/password are empty, relay allows both authenticated and anonymous flow regardless of checkbox.
4. Outbound delivery to smarthost with `PLAIN`, `STARTTLS`, or `SSL_IMPLICIT`.
5. Durable queue (`new`, `inflight`, `deferred`, `dead`, `tmp`) with crash recovery.
6. Retry policy with exponential backoff + jitter.
7. `Flush Now` forces deferred items to `due now` and wakes sender workers.
8. Dead-letter purge from UI.
9. In-memory ring log (no file logs).
10. Optional detailed SMTP tracing (`C:`, `S:`, `C-DATA:`, `EVT:`) via UI checkbox.
11. Credentials are encrypted and stored in INI:
    - `[Inbound] AuthUser/AuthPassword`,
    - `[Outbound] AuthUser/AuthPassword`.
12. Runtime state persistence: listener/senders auto-start after restart if they were running before shutdown.
13. `Reset to defaults` keeps outbound SMTP credentials intact.
14. Any setting change is saved automatically (no explicit Save button).
15. If listener/senders are running, they are restarted automatically after config change.

## Build

Open `MailRelay.dproj` in RAD Studio 12 and build `Win32 Debug` (or Release).

## Runtime Notes

1. Keep OpenSSL DLLs near executable for TLS modes in Indy.
2. Default queue path is `./spool`.
3. Active config path is logged as `Using INI: <path>`.
4. Queue and dashboard charts are based on queue snapshot metadata.

## User Scenarios

### 1) Basic Oracle APEX Relay (No Inbound AUTH Required)

Use when Oracle APEX is in trusted local network.

1. Open `Config` tab.
2. In `Inbound config`, set `Bind IP`, `Bind Port`, `Allowed Client IP` (comma-separated list is supported).
3. In `Inbound auth config`, keep username/password empty and uncheck `Require auth credentials`.
4. In `Outbound config`, set smarthost host/port/TLS/workers.
5. In `Outbound auth credentials`, set smarthost login/password if required by provider.
6. Changes are saved automatically after edit.
7. Go to `Dashboard`, click `Start Listener`, then `Start Senders`.

### 2) Strict Inbound AUTH Required

Use when inbound clients must always authenticate.

1. Open `Config` tab -> `Inbound auth config`.
2. Fill inbound `Username` and `Password`.
3. Enable `Require auth credentials`.
4. Changes are saved automatically after edit.
5. Restart listener from `Dashboard` (`Stop Listener` -> `Start Listener`).

Expected behavior:
- `MAIL FROM` is rejected with auth-required response until AUTH succeeds.

### 3) Compatibility Mode For Mixed Clients

Use when some clients authenticate and some do not.

1. Keep inbound username/password empty.
2. `Require auth credentials` may be on or off.
3. Changes are saved automatically after edit.

Expected behavior:
- relay accepts both authenticated and anonymous sessions.

### 4) Smarthost Credential Rotation Without Downtime

1. Open `Config` tab -> `Outbound auth credentials`.
2. Update SMTP username/password.
3. Changes are saved automatically after edit.
4. If senders are running, click `Flush Now` to accelerate processing.

### 5) Recover Deferred Queue Immediately

Use after temporary smarthost outage.

1. Ensure senders are running.
2. Click `Flush Now` in `Dashboard`.
3. Watch queue counters and charts.

Expected behavior:
- deferred items are marked due-now and reattempted.

### 6) Clean Up Dead-Letters

1. Click `Purge Dead-letters` in `Dashboard`.
2. Confirm action.
3. Verify dead count drops.

### 7) Safe Reset Of Settings

1. Click `Reset to defaults` in `Config`.
2. Confirm action.

Expected behavior:
- most config values return to defaults,
- outbound credentials remain preserved.

### 8) Capacity Tuning For High Volume

1. Increase `Queue MaxQueueItems` and `Queue MaxQueueBytesMB`.
2. Tune retry (`MaxAttempts`, `BaseDelaySec`, `MaxDelaySec`, `JitterPct`).
3. Increase `Workers` if smarthost allows parallel sends.
4. Changes are saved automatically; monitor `Messages per hour` chart.

### 9) Observability / Troubleshooting Session

1. Enable `Enable detail logging`.
2. Reproduce issue.
3. Open `Log` tab and inspect SMTP dialog lines.
4. Disable detail logging after diagnostics to reduce noise.

### 10) Planned Restart With State Restore

1. Keep listener/senders running.
2. Close application normally.
3. Start application again.

Expected behavior:
- relay restores previous runtime state (`ListenerStarted`/`SendersStarted`).

## Troubleshooting

1. If inbound AUTH fails unexpectedly, verify inbound username/password and `RequireAuthCredentials`.
2. If outbound send fails with `SMTP 535`, re-save outbound credentials and verify:
   - smarthost host/port/TLS,
   - provider account policy,
   - active INI path from `Using INI: ...`.
3. If dashboard looks stale, verify `StatsRefreshMs` and sender/listener status.
4. If queue growth is too fast, increase queue limits or reduce upstream flow.

## License

See `LICENSE.md`.
