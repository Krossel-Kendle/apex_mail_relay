# Athexa APEX SMTP Relay

Windows SMTP relay for Oracle APEX (Delphi 12, Indy) with durable disk spool, retry policy, and restricted inbound access.

## Highlights

1. Inbound SMTP listener for APEX only (`AllowedClientIP`).
2. Inbound SMTP auth support: `CRAM-MD5`, `LOGIN`, `PLAIN`.
3. Inbound auth mode:
   - if `[Inbound] AuthUser/AuthPassword` are empty: AUTH is accepted (compat mode for clients that always authenticate);
   - if configured: credentials are validated and `MAIL FROM` requires successful AUTH.
4. Outbound delivery to smarthost with `PLAIN`, `STARTTLS`, or `SSL_IMPLICIT`.
5. Durable queue (`new`, `inflight`, `deferred`, `dead`, `tmp`) with crash recovery.
6. Retry policy with exponential backoff + jitter.
7. `Flush Now` forces deferred items to `due now` and wakes sender workers.
8. In-memory ring log (no file logs).
9. Optional detailed SMTP tracing (`C:`, `S:`, `C-DATA:`, `EVT:`) via UI checkbox.
10. Credentials are encrypted and stored as single-line values in INI:
    - `[Inbound] AuthUser/AuthPassword` (inbound AUTH),
    - `[Outbound] AuthUser/AuthPassword` (smarthost AUTH).
11. Runtime state persistence: listener/senders auto-start after restart if they were running before shutdown.
12. UI language from INI: English, Russian, Indonesian.

## Build

Open `MailRelay.dproj` in RAD Studio 12 and build `Win32 Debug` (or Release).

## Runtime Notes

1. Keep OpenSSL DLLs near executable for TLS modes in Indy.
2. Default queue path is `.\spool`.
3. Credentials editor is hidden by default and opened by the credentials button.
4. At startup the app writes `Using INI: <path>` to the log so you can verify the active configuration file.

## Troubleshooting

1. If inbound AUTH from APEX fails unexpectedly, verify `[Inbound] AuthUser/AuthPassword`.
2. If outbound send fails with `SMTP 535`, re-save outbound credentials and verify log lines:
   - `SMTP auth enabled user=... passLen=...`
   - `Send failed ... SMTP 535`
3. Ensure you are editing the same INI file shown in `Using INI: ...`.

## License

See `LICENSE.md`.
