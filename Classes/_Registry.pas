{$I ..\CompilerVer.inc}
{$IFDEF DELPHI_XE}
{$MESSAGE Warn '_Registry.pas has problems in Delphi XE'}
{$ENDIF}

{$WARNINGS OFF}
unit _Registry;

interface

uses SysUtils, Windows, Registry, Controls, _Misc;

resourcestring
  sValueNotFound = 'Ошибка в реестре:'#13#10'Не найден параметр "%s" в разделе "%s\%s"';
  sKeyNotFound = 'Ошибка в реестре:'#13#10'Не найден раздел "%s\%s"';

type
  TRegistryEx = class;

  TEnumSubKeysProc1 = procedure(const S: AnsiString; Index, Data: Integer) of object;
  TEnumSubKeysProc2 = procedure(Self: TRegistryEx; const S: AnsiString; Index, Data: Integer);

  TRegistryEx = class(TRegistry)
  public
    function EnumSubKeys(proc: TEnumSubKeysProc1; Data: Integer): Integer; overload;
    function EnumSubKeys(proc: TEnumSubKeysProc2; Data: Integer): Integer; overload;
    function ReadBoolValue(AName: AnsiString; ADefault: Boolean): Boolean;
    function ReadIntValue(AName: AnsiString; ADefault: Integer): Integer;
    function ReadStrValue(AName: AnsiString; ADefault: AnsiString): AnsiString;
    procedure WriteBoolValueDef(AName: AnsiString; AValue, ADefault: Boolean);
    procedure WriteIntValueDef(AName: AnsiString; AValue, ADefault: Integer);
    procedure WriteStrValueDef(AName: AnsiString; AValue, ADefault: AnsiString);

    procedure WriteWindowPos(ALeft, ATop, AWidth, AHeight: Integer);
    function ReadWindowPos(var ALeft, ATop, AWidth, AHeight: Integer): Boolean; 
  end;

function CheckOpenKey(R: TRegistry; AKey: AnsiString; ANeedDestroy: Boolean = True; ACaption: AnsiString = 'Ошибка'): Boolean;
function CheckOpenKey2(R: TRegistry; AKey: AnsiString): Boolean;
procedure ShowRegError(R: TRegistry; AStr: AnsiString; AParams: array of const; ANeedDestroy: Boolean = True; ACaption: AnsiString = 'Ошибка');
function RootToString(Root: HKEY): AnsiString;

procedure SaveWindowPos(AWindow: TWinControl; const ARegKey: AnsiString);
procedure LoadWindowPos(AWindow: TWinControl; const ARegKey: AnsiString);

implementation

function RootToString(Root: HKEY): AnsiString;
begin
case Root of
HKEY_CLASSES_ROOT: Result:='HKEY_CLASSES_ROOT';
HKEY_CURRENT_USER: Result:='HKEY_CURRENT_USER';
HKEY_LOCAL_MACHINE: Result:='HKEY_LOCAL_MACHINE';
HKEY_USERS: Result:='HKEY_USERS';
HKEY_PERFORMANCE_DATA: Result:='HKEY_PERFORMANCE_DATA';
HKEY_CURRENT_CONFIG: Result:='HKEY_CURRENT_CONFIG';
HKEY_DYN_DATA: Result:='HKEY_DYN_DATA';
else
Result:='';
end; // case
end;

procedure ShowRegError(R: TRegistry; AStr: AnsiString; AParams: array of const; ANeedDestroy: Boolean = True; ACaption: AnsiString = 'Ошибка');
begin
ShowErrorFmt(AStr, AParams, ACaption);
R.CloseKey;
if ANeedDestroy then R.Free;
end;

function CheckOpenKey2(R: TRegistry; AKey: AnsiString): Boolean;
begin
Result:=R.OpenKey(AKey, False);
if not Result then raise Exception.CreateFmt(sKeyNotFound, [RootToString(R.RootKey), AKey]);
end;

function CheckOpenKey(R: TRegistry; AKey: AnsiString; ANeedDestroy: Boolean = True; ACaption: AnsiString = 'Ошибка'): Boolean;
begin
Result:=R.OpenKey(AKey, False);
if not Result then ShowRegError(R, sKeyNotFound, [RootToString(R.RootKey), AKey], ANeedDestroy, ACaption);
end;

function TRegistryEx.EnumSubKeys(proc: TEnumSubKeysProc1; Data: Integer): Integer;
var Len: DWORD; I: Integer; Info: TRegKeyInfo; Buff: Pointer;
begin
Result:=0;
if GetKeyInfo(Info) then
  begin
  Result:=Info.NumSubKeys;
  GetMem(Buff, Info.MaxSubKeyLen+1);
  for I:=0 to Info.NumSubKeys-1 do
    begin
    Len:=Info.MaxSubKeyLen+1;
    ZeroMemory(Buff, Len);
    RegEnumKeyEx(CurrentKey, I, Buff, Len, nil, nil, nil, nil);
    Proc(StrPas(PAnsiChar(Buff)), I, Data);
    end;
  FreeMem(Buff, Info.MaxSubKeyLen+1);
  end;
end;

function TRegistryEx.EnumSubKeys(proc: TEnumSubKeysProc2; Data: Integer): Integer;
var Method: TMethod;
begin
Method.Code:=@Proc;
Method.Data:=Self;
result:=EnumSubKeys(TEnumSubKeysProc1(Method), Data);
end;

function TRegistryEx.ReadBoolValue(AName: AnsiString; ADefault: Boolean): Boolean;
begin
if (ValueExists(AName)) and (GetDataType(AName)=rdInteger) then
  Result:=ReadBool(AName)
else
  Result:=ADefault;
end;

function TRegistryEx.ReadIntValue(AName: AnsiString; ADefault: Integer): Integer;
begin
if (ValueExists(AName)) and (GetDataType(AName)=rdInteger) then
  Result:=ReadInteger(AName)
else
  Result:=ADefault;
end;

function TRegistryEx.ReadStrValue(AName: AnsiString; ADefault: AnsiString): AnsiString;
begin
if (ValueExists(AName)) and
    ((GetDataType(AName)=rdString) or (GetDataType(AName)=rdExpandString)) then
  Result:=ReadString(AName)
else
  Result:=ADefault;
end;

function TRegistryEx.ReadWindowPos(var ALeft, ATop, AWidth, AHeight: Integer): Boolean;
begin
ALeft:=ReadIntValue('WindowLeft', 0);
ATop:=ReadIntValue('WindowTop', 0);
AWidth:=ReadIntValue('WindowWidth', 0);
AHeight:=ReadIntValue('WindowHeight', 0);
Result:=(ALeft<>0) and (ATop<>0) and (AWidth<>0) and (AHeight<>0)
end;

procedure TRegistryEx.WriteBoolValueDef(AName: AnsiString; AValue, ADefault: Boolean);
begin
if ADefault=AValue then DeleteValue(AName) else WriteBool(AName, AValue)
end;

procedure TRegistryEx.WriteIntValueDef(AName: AnsiString; AValue, ADefault: Integer);
begin
if ADefault=AValue then DeleteValue(AName) else WriteInteger(AName, AValue)
end;

procedure TRegistryEx.WriteStrValueDef(AName: AnsiString; AValue, ADefault: AnsiString);
begin
if ADefault=AValue then DeleteValue(AName) else WriteString(AName, AValue)
end;

procedure TRegistryEx.WriteWindowPos(ALeft, ATop, AWidth, AHeight: Integer);
begin
WriteInteger('WindowLeft', ALeft);
WriteInteger('WindowTop', ATop);
WriteInteger('WindowWidth', AWidth);
WriteInteger('WindowHeight', AHeight);
end;

procedure SaveWindowPos(AWindow: TWinControl; const ARegKey: AnsiString);
var R: TRegistryEx;
begin
R:=TRegistryEx.Create;
try
  R.RootKey:=HKEY_CURRENT_USER;
  R.OpenKey(ARegKey, True);
  R.WriteWindowPos(AWindow.Left, AWindow.Top, AWindow.Width, AWindow.Height);
  R.CloseKey;
finally
  R.Free;
end; // try
end;

procedure LoadWindowPos(AWindow: TWinControl; const ARegKey: AnsiString);
var R: TRegistryEx; L, T, W, H: Integer;
begin
R:=TRegistryEx.Create;
try
  R.RootKey:=HKEY_CURRENT_USER;
  R.OpenKey(ARegKey, True);
  if R.ReadWindowPos(L, T, W, H) then AWindow.SetBounds(L, T, W, H);
  R.CloseKey;
finally
  R.Free;
end; // try
end;

end.
{$WARNINGS ON}
