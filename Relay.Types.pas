unit Relay.Types;

interface

type
  TRelayTlsMode = (tmPlain, tmStartTls, tmSslImplicit);
  TRelayListenerStatus = (lsStopped, lsListening, lsError);
  TRelaySenderStatus = (ssStopped, ssRunning, ssError);

  TQueueStats = record
    NewCount: Integer;
    InFlightCount: Integer;
    DeferredCount: Integer;
    DeadCount: Integer;
    TotalCount: Integer;
    TotalBytes: Int64;
    OldestAgeSec: Integer;
  end;

function RelayTlsModeToString(const AMode: TRelayTlsMode): string;
function RelayTlsModeFromString(const AValue: string; out AMode: TRelayTlsMode): Boolean;

implementation

uses
  System.SysUtils;

function RelayTlsModeToString(const AMode: TRelayTlsMode): string;
begin
  case AMode of
    tmPlain:
      Result := 'PLAIN';
    tmStartTls:
      Result := 'STARTTLS';
    tmSslImplicit:
      Result := 'SSL_IMPLICIT';
  else
    Result := 'PLAIN';
  end;
end;

function RelayTlsModeFromString(const AValue: string; out AMode: TRelayTlsMode): Boolean;
var
  LUpper: string;
begin
  LUpper := UpperCase(Trim(AValue));
  if LUpper = 'PLAIN' then
  begin
    AMode := tmPlain;
    Exit(True);
  end;
  if LUpper = 'STARTTLS' then
  begin
    AMode := tmStartTls;
    Exit(True);
  end;
  if LUpper = 'SSL_IMPLICIT' then
  begin
    AMode := tmSslImplicit;
    Exit(True);
  end;

  Result := False;
end;

end.
