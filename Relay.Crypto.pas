unit Relay.Crypto;

interface

function EncryptToSingleLine(const APlainText: string): string;
function DecryptFromSingleLine(const ACipherText: string): string;
function IsEncryptedValue(const AValue: string): Boolean;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.NetEncoding;

const
  CEncryptPrefix = 'dpapi:';
  CRYPTPROTECT_UI_FORBIDDEN = $1;

function CryptProtectData(
  pDataIn: PDATA_BLOB;
  szDataDescr: LPCWSTR;
  pOptionalEntropy: PDATA_BLOB;
  pvReserved: Pointer;
  pPromptStruct: Pointer;
  dwFlags: DWORD;
  pDataOut: PDATA_BLOB
): BOOL; stdcall; external 'Crypt32.dll' name 'CryptProtectData';

function CryptUnprotectData(
  pDataIn: PDATA_BLOB;
  ppszDataDescr: PLPWSTR;
  pOptionalEntropy: PDATA_BLOB;
  pvReserved: Pointer;
  pPromptStruct: Pointer;
  dwFlags: DWORD;
  pDataOut: PDATA_BLOB
): BOOL; stdcall; external 'Crypt32.dll' name 'CryptUnprotectData';

function IsEncryptedValue(const AValue: string): Boolean;
var
  LValue: string;
begin
  LValue := Trim(AValue);
  Result := LValue.StartsWith(CEncryptPrefix, True);
end;

function BytesFromString(const AValue: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(AValue);
end;

function StringFromBytes(const AValue: TBytes): string;
begin
  Result := TEncoding.UTF8.GetString(AValue);
end;

function EncryptToSingleLine(const APlainText: string): string;
var
  LInBlob: DATA_BLOB;
  LOutBlob: DATA_BLOB;
  LInBytes: TBytes;
  LOutBytes: TBytes;
begin
  if APlainText = '' then
    Exit('');

  LInBytes := BytesFromString(APlainText);
  ZeroMemory(@LInBlob, SizeOf(LInBlob));
  ZeroMemory(@LOutBlob, SizeOf(LOutBlob));
  LInBlob.cbData := Length(LInBytes);
  if LInBlob.cbData > 0 then
    LInBlob.pbData := @LInBytes[0];

  if not CryptProtectData(@LInBlob, nil, nil, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, @LOutBlob) then
    raise Exception.CreateFmt('CryptProtectData failed, code=%d', [GetLastError]);
  try
    SetLength(LOutBytes, LOutBlob.cbData);
    if LOutBlob.cbData > 0 then
      Move(LOutBlob.pbData^, LOutBytes[0], LOutBlob.cbData);
  finally
    if Assigned(LOutBlob.pbData) then
      LocalFree(HLOCAL(LOutBlob.pbData));
  end;

  Result := CEncryptPrefix + TNetEncoding.Base64.EncodeBytesToString(LOutBytes).Replace(#13, '').Replace(#10, '');
end;

function DecryptFromSingleLine(const ACipherText: string): string;
var
  LCipherText: string;
  LBase64: string;
  LInBlob: DATA_BLOB;
  LOutBlob: DATA_BLOB;
  LInBytes: TBytes;
  LOutBytes: TBytes;
begin
  LCipherText := Trim(ACipherText);
  if LCipherText = '' then
    Exit('');

  if not IsEncryptedValue(LCipherText) then
    Exit(ACipherText);

  LBase64 := Trim(LCipherText.Substring(Length(CEncryptPrefix)));
  LInBytes := TNetEncoding.Base64.DecodeStringToBytes(LBase64);

  ZeroMemory(@LInBlob, SizeOf(LInBlob));
  ZeroMemory(@LOutBlob, SizeOf(LOutBlob));
  LInBlob.cbData := Length(LInBytes);
  if LInBlob.cbData > 0 then
    LInBlob.pbData := @LInBytes[0];

  if not CryptUnprotectData(@LInBlob, nil, nil, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, @LOutBlob) then
    raise Exception.CreateFmt('CryptUnprotectData failed, code=%d', [GetLastError]);
  try
    SetLength(LOutBytes, LOutBlob.cbData);
    if LOutBlob.cbData > 0 then
      Move(LOutBlob.pbData^, LOutBytes[0], LOutBlob.cbData);
  finally
    if Assigned(LOutBlob.pbData) then
      LocalFree(HLOCAL(LOutBlob.pbData));
  end;

  Result := StringFromBytes(LOutBytes);
end;

end.
