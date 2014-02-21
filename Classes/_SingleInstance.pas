unit _SingleInstance;

interface

// HOW TO USE
// 1. � DPR �����:
//   if not _SingleInstance.DoInit(<SIW_Class>) then exit;
// 2. ���-������ � ����� (��������, � FormDestroy):
//   _SingleInstance.DoDone
// 3. ��� ��������� ������� �� SIW ����� ������������ ��� ��������:
//   - ���� ����� ���������� ������� OnSIW
//   - ���� ����� ���������� TSingleInstaceWindow � � ��� �������������� WndProc;
//     � ���� ������ ��� ������-���������� �������� � �������� ������� ���������
//     � DoInit
// 4. �������������� ��� ������� WM_SINGLEINSTANCE � WM_SINGLEINSTANCESTR(=LB_ADDSTRING):
//    ������ - ��� �������� ����� ����� ����������, ������ - ��� ����� ����������
// 5. � �������� wParam ����� ������� ���������� ��������� SIW_xxx. ���� ���� ����������������
//    ��������� ��� ������� WM_SINGLEINSTANCE - SIW_RESTOREAPP ��� ��������������
//    ��� ���������� ����� ����������. ��� ���, � ������ �������������,
//    ����� ������ ���������� ��������������, �.�. � ����� TSingleInstaceWindow
//    ����� ���������������� �� ��������

uses SysUtils, Messages, Windows, _UtilWindow;

var WM_SINGLEINSTANCE: UINT;
const WM_SINGLEINSTANCESTR = LB_ADDSTRING;

const SIW_RESTOREAPP = 1;

type TSIWHandlerProc = procedure(var M: TMessage) of object;

type TSingleInstaceWindow = class(TUtilWindow)
  private
    FOnSIW: TSIWHandlerProc;
  protected
    procedure WndProc(var M: TMessage); override;
  public
    property OnSIW: TSIWHandlerProc read FOnSIW write FOnSIW;
  end;

type TSingleInstaceWindowClass = class of TSingleInstaceWindow;

var SIW: TSingleInstaceWindow;

function DoInit(const ASIW_Class: String; AVCLClass: TSingleInstaceWindowClass = NIL; AData: Integer = 0): Boolean;
function DoInit2(const ASIW_Class: String; AVCLClass: TSingleInstaceWindowClass = NIL): hWnd;

procedure DoDone;

implementation

var SIW_Class: String;

type PWnd = ^hWnd;

function EWP(Handle: HWND; Info: Pointer): BOOL; stdcall;
var C: array[0..255] of char;
begin
Result := True;
FillChar(C, SizeOf(C), 0);
GetClassName(Handle, C, SizeOf(C));
if (StrPas(C)=SIW_Class) then
  begin
  PWnd(Info)^:=Handle;
  Result:=False;
  end;
end;

{ TSingleInstaceWindow }

function DoInit(const ASIW_Class: String; AVCLClass: TSingleInstaceWindowClass = NIL; AData: Integer = 0): Boolean;
var h: hWnd;
begin
SIW_Class:=ASIW_Class;

Result:=False;
h:=0;
EnumWindows(@EWP, Integer(@h));
if h<>0 then
  begin
  SendMessage(h, WM_SINGLEINSTANCE, SIW_RestoreApp, AData);
  exit;
  end;

Result:=True;
if AVCLClass=NIL then AVCLClass:=TSingleInstaceWindow;
SIW:=AVCLClass.Create(PChar(SIW_Class));
end;

function DoInit2(const ASIW_Class: String; AVCLClass: TSingleInstaceWindowClass = NIL): hWnd;
begin
SIW_Class:=ASIW_Class;

Result:=0;
EnumWindows(@EWP, Integer(@Result));

if Result=0 then
	begin
	if AVCLClass=NIL then AVCLClass:=TSingleInstaceWindow;
	SIW:=AVCLClass.Create(PChar(SIW_Class));
	end;
end;

procedure DoDone;
begin
SIW.Free;
end;

{ TSingleInstaceWindow }

procedure TSingleInstaceWindow.WndProc(var M: TMessage);
begin
if (M.Msg=WM_SINGLEINSTANCE) or (M.Msg=WM_SINGLEINSTANCESTR) then
  begin
  if Assigned(FOnSIW) then FOnSIW(M);
  end
else
  inherited WndProc(M);
end;

initialization
WM_SINGLEINSTANCE:=RegisterWindowMessage('WM_SINGLEINSTANCE');

end.


