unit _Misc;

interface

{$I ..\CompilerVer.inc}

uses SysUtils, Classes, ActiveX, Windows, ComObj, Messages;

const NullID = 0;

type
  TByteSet = set of byte;
  PByteSet = ^TByteSet;

type
  TIntArray = array[0..1023] of Integer;
  PIntArray = ^TIntArray;

function Min(A, B: Integer): Integer;
function Max(A, B: Integer): Integer;

// Делит Num1 нацело на Num2. Если не делится без остатка, то прибавляет к результату единицу
function ExDiv(Num1, Num2: LongInt): LongInt;

procedure GrowRect(var ARect: TRect; ALeftOffset, ATopOffset, ARightOffset, ABottomOffset: Integer);

function MyCreateComObject(const ClassID: TGUID): IUnknown;
function MyCreateOleObject(const ClassName: string): IDispatch;

procedure SendDataOverWMCOPYDATA(ADestWnd: hWnd; AMemory: Pointer; ASize: Integer; ASourceWnd: hWnd);

procedure GetInterfaceOrRaise(AObject: TObject; const IID: TGUID;
  const AInterfaceName: String; out Obj);

function Method(AObject: TObject; AMethod: Pointer): TMethod;

const NullMethod: TMethod = (Code: NIL; Data: NIL);
var AlwaysTrue: TMethod = (Code: NIL; Data: NIL);

function MethodIsNull(AMethod: TMethod): Boolean;

type Func_Int = function(): Integer of object;
type Func_String = function(): String of object;

function GetLocalIP: AnsiString;

procedure ShowError(AStr: String; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0);
procedure ShowErrorFmt(const AStr: String; AParams: array of const; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0);

var ShowErrorParentWnd: hWnd = 0;

function mLoadLibrary(const AFileName: String): HMODULE;
function mGetProcAddress(AHandle: HMODULE; const AFunctionName: String): Pointer;

function ThreadHasModalWindow: Boolean;

{$IFDEF DELPHI_XE}
procedure RunOnMainThread(AProcedure: TThreadProcedure); overload;
procedure RunOnMainThread(AMethod: TThreadMethod); overload;
{$ENDIF}

implementation

uses WinSock;

{$IFDEF DELPHI_XE}
procedure RunOnMainThread(AProcedure: TThreadProcedure);
begin
TThread.Queue(nil, AProcedure);
end;

procedure RunOnMainThread(AMethod: TThreadMethod);
begin
TThread.Queue(nil, AMethod);
end;
{$ENDIF}

function CheckModal(h: HWnd; Data: Longint): Bool; stdcall;
begin
Result:=True;
if (GetWindowLong(h, GWL_EXSTYLE) and WS_EX_DLGMODALFRAME<>0) and IsWindowVisible(h) then
  begin
  Result:=False;
  PBoolean(Data)^:=True;
  end;
end;

function ThreadHasModalWindow: Boolean;
begin
Result:=False;
EnumThreadWindows(GetCurrentThreadID, @CheckModal, Integer(@Result));
end;

function mLoadLibrary(const AFileName: String): HMODULE;
begin
  Result := LoadLibrary(PChar(AFileName));
  if Result <= HINSTANCE_ERROR then
    raise Exception.CreateFmt('Ошибка при загрузке библиотеки "%s": %s',
        [AFileName, SysErrorMessage(GetLastError)]);
end;

function mGetProcAddress(AHandle: HMODULE; const AFunctionName: String): Pointer;
begin
  Result := GetProcAddress(AHandle, PChar(AFunctionName));
  if not Assigned(Result) then
    raise Exception.CreateFmt('Не найдена функция "%s"', [AFunctionName]);
end;

procedure ShowError(AStr: String; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0);
begin
if AWnd=0 then AWnd:=ShowErrorParentWnd;
MessageBox(AWnd, PChar(AStr), PChar(ACaption), MB_ICONERROR);
end;

procedure ShowErrorFmt(const AStr: String; AParams: array of const; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0);
begin
ShowError(Format(AStr, AParams), ACaption, AWnd);
end;

function GetLocalIP: AnsiString;
const WSVer=$101;
var wsaData: TWSAData; P: PHostEnt; Buf: array [0..127] of AnsiChar;
begin
Result:='';
if WSAStartup(WSVer, wsaData)=0 then
  begin
  if GetHostName(@Buf, 128)=0 then
    begin
    P:=GetHostByName(@Buf);
    if P<>NIL then Result:=iNet_ntoa(PInAddr(p^.h_addr_list^)^);
    end;
  WSACleanup;
  end;
end;

function MethodIsNull(AMethod: TMethod): Boolean;
begin
Result:=AMethod.Code=NIL
end;

function Method(AObject: TObject; AMethod: Pointer): TMethod;
begin
Result.Code:=AMethod;
Result.Data:=AObject;
end;

const sNotRegistred = 'COM класс %s не зарегистрирован';

procedure GetInterfaceOrRaise(AObject: TObject; const IID: TGUID;
  const AInterfaceName: String; out Obj);
begin
if not Assigned(AObject) then
  raise Exception.CreateFmt('Can''t get %s interface from NIL', [AInterfaceName]);
  
if not AObject.GetInterface(IID, Obj) then
  raise Exception.CreateFmt('Class %s doesn''t implement %s interface',
    [AObject.ClassName, AInterfaceName]);
end;

function Min(A, B: Integer): Integer;
begin
if A<B then Result:=A else Result:=B;
end;

function Max(A, B: Integer): Integer;
begin
if A>B then Result:=A else Result:=B;
end;

function ExDiv(Num1, Num2: LongInt): LongInt;
begin
Result:=Num1 div Num2;
if Num1 mod Num2<>0 then Inc(Result);
end;

function MyCreateComObject(const ClassID: TGUID): IUnknown;
var h: HResult;
begin
h:=CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, Result);
if not Succeeded(h) then
  begin
  if h = REGDB_E_CLASSNOTREG then
    raise Exception.CreateFmt(sNotRegistred, [GUIDToString(ClassID)])
  else
    OleError(h);
  end;
end;

function MyCreateOleObject(const ClassName: string): IDispatch;
var h: HResult; ClassID: TCLSID;
begin
try
  ClassID:=ProgIDToClassID(ClassName);
except
  raise Exception.CreateFmt(sNotRegistred, [ClassName])
end; // try

h:=CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IDispatch, Result);
if not Succeeded(h) then
  begin
  if h=REGDB_E_CLASSNOTREG then
    raise Exception.CreateFmt(sNotRegistred, [GUIDToString(ClassID)])
  else
    OleError(h);
  end;
end;

procedure GrowRect(var ARect: TRect; ALeftOffset, ATopOffset, ARightOffset, ABottomOffset: Integer);
begin
ARect.Left:=ARect.Left+ALeftOffset;
ARect.Top:=ARect.Top+ATopOffset;
ARect.Right:=ARect.Right+ARightOffset;
ARect.Bottom:=ARect.Bottom+ABottomOffset;
end;

procedure SendDataOverWMCOPYDATA(ADestWnd: hWnd; AMemory: Pointer; ASize: Integer; ASourceWnd: hWnd);
var Struct: COPYDATASTRUCT;
begin
Struct.dwData:=0;
Struct.cbData:=ASize;
Struct.lpData:=AMemory;
SendMessage(ADestWnd, WM_COPYDATA, ASourceWnd, Integer(@Struct));
end;

function AlwaysTrueProc(ACommand: TObject; AContext: Pointer): Boolean;
begin
Result:=True;
end;

initialization
AlwaysTrue.Code:=@AlwaysTrueProc;

end.
