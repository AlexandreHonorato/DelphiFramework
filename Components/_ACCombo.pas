unit _ACCombo;

interface

uses SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Forms;

type // �����, ����������� ���������� ���� ��������������
  TDropDownListBox = class(TListBox)
  protected
    procedure WMActivateApp(var M: TMessage); message WM_ACTIVATEAPP;
    procedure WMMouseMove(var M: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var M: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateItemIndex;
  end;

type
  TCheckStringEvent = procedure(Sender: TObject; const EditText, S: String; var AddString: Boolean) of object;

type
  TAutoCompleteType = (actSimple, actSubString, actCustom);

type // �����-���� � ���������� ����� ��������������
  TACComboBox = class(TComboBox)
  private
    FDropped: Boolean; // True, ����� �������� ���������� ����
    FACItems: TStrings;
    FOldFormWndProc, FNewFormWndProc: Pointer; // ������������ ��� ������� ������� ��������� ������������ �����
    FParentFormWnd: hWnd; // Handle ������������ �����
    FACType: TAutoCompleteType;
    FOnCheckString: TCheckStringEvent;
    procedure SetACItems(const Value: TStrings);

    procedure ParentFormWndProc(var Message: TMessage); // ������� ���������, ������� ��������� ������� ��������� ������������ �����
  protected
    FDropDown: TDropDownListBox; // ���������� ���� ��������������

    procedure CNCommand(var M: TWMCommand); message CN_COMMAND;
    procedure WMMouseWheel(var M: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMGetDlgCode(var M: TWMGetDlgCode); message WM_GETDLGCODE;

    procedure CMExit(var M: TCMExit); message CM_EXIT;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure SetParent(AParent: TWinControl); override;

    procedure PrepareACStrings(const AText: String); virtual;
    procedure CheckStringSimple(Sender: TObject; const EditText, S: String; var AddString: Boolean);
    procedure CheckStringSubString(Sender: TObject; const EditText, S: String; var AddString: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowAC;
    procedure HideAC(ApplySelection: Boolean);
  published
    property ACItems: TStrings read FACItems write SetACItems;
    property ACType: TAutoCompleteType read FACType write FACType default actSubString;
    property OnCheckString: TCheckStringEvent read FOnCheckString write FOnCheckString;
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
RegisterComponents('My components', [TACComboBox]);
end;

// ����� ������������ ��� ������������� � ListBox'� ��������, ��� ������� ��������� ������ ����
procedure TDropDownListBox.UpdateItemIndex;
var P: TPoint; I: Integer;
begin
GetCursorPos(P);
P:=ScreenToClient(P);
I:=ItemAtPos(P, True);
if I<>-1 then ItemIndex:=I;
end;

procedure TDropDownListBox.WMLButtonUp(var M: TWMLButtonUp);
begin
inherited;

// ��� ������ �������� �� ������ �������������� �������� ��� � ���� ����� �����-����� � ��������� ���� ��������������.
if ItemIndex<>-1 then TACComboBox(Owner).HideAC(True);
end;

procedure TDropDownListBox.WMMouseMove(var M: TWMMouseMove);
begin
inherited;
// ��� �������� ���� ��� ���������� �����, ������������ �������, ��� ������� ��������� ������ ����.
UpdateItemIndex;
end;

procedure TDropDownListBox.CreateParams(var Params: TCreateParams);
begin
inherited CreateParams(Params);
Params.ExStyle:=WS_EX_TOOLWINDOW;
Params.WndParent:=GetDesktopWindow;
Params.Style:=WS_CHILD or WS_BORDER or WS_CLIPSIBLINGS or WS_OVERLAPPED or WS_VSCROLL or LBS_NOINTEGRALHEIGHT;
end;

procedure TDropDownListBox.WMActivateApp(var M: TMessage);
begin
inherited;

// ��� ������������ �� ������ ���������� (Alt+Tab, etc.) ������ ���� ��������������
TACComboBox(Owner).HideAC(False);
end;

{ TACEdit }

// ���������� �� ������ PrepareACStrings ��� ACType=actSimple ��� ������� �������� ������ ACItems;
// � ���������� � ������ ��������� �������������� ����������� ������ �� ACItems,
// ������� ���������� � ���������� � ���� �����-����� ������
procedure TACComboBox.CheckStringSimple(Sender: TObject; const EditText,
  S: String; var AddString: Boolean);
begin
AddString:=AnsiUpperCase(copy(S, 1, Length(EditText)))=AnsiUpperCase(EditText);
end;

// ���������� �� ������ PrepareACStrings ��� ACType=actSubString ��� ������� �������� ������ ACItems;
// � ���������� � ������ ��������� �������������� ����������� ������ �� ACItems,
// ������� �������� ��������� � ���� �����-����� �����
procedure TACComboBox.CheckStringSubString(Sender: TObject; const EditText,
  S: String; var AddString: Boolean);
begin
AddString:=pos(AnsiUpperCase(EditText), AnsiUpperCase(S))<>0
end;

procedure TACComboBox.CMExit(var M: TCMExit);
begin
inherited;

// ��� ������ ������ �����-������ ������ ������ ��������������
HideAC(False);
end;

procedure TACComboBox.CNCommand(var M: TWMCommand);
begin
if M.NotifyCode=CBN_DROPDOWN then
  // ���� ������������ ���������� �����-����, ������ ��� ������ ��������������
  begin
  HideAC(False);
  inherited;
  end
else
if (M.NotifyCode=CBN_EDITCHANGE) then
  begin
  inherited;
  ShowAC // ��� ����� ������ � ���� �����-����� ���������� ������ ��������������
  end
else
  inherited;
end;

constructor TACComboBox.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FACItems:=TStringList.Create;
FACType:=actSubString;
FDropped:=False;
FParentFormWnd:=0;

// � Design-Mode ������ �������������� ��� �� �����, ������� ������� ��� ������ � Run-Time
if not (csDesigning in ComponentState) then
  FDropDown:=TDropDownListBox.Create(Self);
end;

destructor TACComboBox.Destroy;
begin
if not (csDesigning in ComponentState) then FDropDown.Free;

FACItems.Free;
inherited Destroy;
end;

procedure TACComboBox.HideAC(ApplySelection: Boolean);
var I: Integer;
begin
ShowWindow(FDropDown.Handle, SW_HIDE); // ������ ������ ��������������

// ���� ApplySelection=True, �� �������� � ���� �������������� �����-�����
// ��������� ������������� ������� �� ������ ��������������
if ApplySelection then
  begin
  I:=FDropDown.ItemIndex;
  if I<>-1 then
    begin
    Text:=FDropDown.Items[I];
    Change;
    SelectAll;
    end;
  end;
FDropped:=False;
end;

procedure TACComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var M: TWMKeyDown; Msg: TMsg;
begin
// ���� ������ �������������� �� �������, �� ������� ������ ������������
// ������� ��� �����-����� �������
if not FDropped then
  begin
  inherited KeyDown(Key, Shift);
  exit;
  end;

case Key of
VK_ESCAPE: // �� ������� Esc ������ ������ ��������������
  begin
  HideAC(False);
  PeekMessage(Msg, 0, WM_CHAR, WM_CHAR, PM_REMOVE); // ����� �������� �������� ������ (beep)
  end;

// ������� ������� �����/����, PgUp/PgDn �������� ���� ��������������, ����� ������������ �� ������ ���������
VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR:
  begin
  FillChar(M, SizeOf(M), 0);
  M.Msg:=WM_KEYDOWN;
  M.CharCode:=Key;
  SendMessage(FDropDown.Handle, TMessage(M).Msg, TMessage(M).WParam, TMessage(M).LParam);
  FillChar(M, SizeOf(M), 0);
  M.Msg:=WM_KEYUP;
  M.CharCode:=Key;
  SendMessage(FDropDown.Handle, TMessage(M).Msg, TMessage(M).WParam, TMessage(M).LParam);

  // �������� �� ������� ���� ������� �������/PgUp/PgDn, �����, ����� ����������� �� ������
  // ��������������, ����� ����������� �������� ��� �����-����� �� ���������, �.�.
  // � ���� ����� ���������� ����� ������������ ������ �� �������� Items
  Key:=0;
  end;

// �� ������� Enter ��������� ���� �������������� � �������� � ���� ��������������
// �����-����� ��������� ������� �� ������ ��������������
VK_RETURN:
  begin
  HideAC(True);
  PeekMessage(Msg, 0, WM_CHAR, WM_CHAR, PM_REMOVE); // ����� �������� �������� ������ (beep)
  end
else
  inherited KeyDown(Key, Shift);
end; // case
end;

procedure TACComboBox.KeyPress(var Key: Char);
begin
// ������� ������� BackSpace (� ������� �� ��������-�������� ������ � ������� Del)
// �� ������� ��������� CBN_EDITCHANGE, � ��� ���������, ���������� ������ ��������������
// �� ����������, ������� ���������� ������������ VK_BACK ��������
if Ord(Key)=VK_BACK then
  begin
  inherited KeyPress(Key);
  ShowAC;
  exit;
  end
else
  inherited KeyPress(Key);
end;

// ���� ���������� ��������� ������� ��������� ������������ ����� (��� ����� �� �����������
// �������� ���������������� Parent'�� �����-�����)
procedure TACComboBox.ParentFormWndProc(var Message: TMessage);

  procedure Default;
  begin
  with Message do Result:=CallWindowProc(FOldFormWndProc, FParentFormWnd, Msg, wParam, lParam);
  end;

begin
case Message.Msg of // ��� ��������� ��������� ������������ ����� ������ ���� ��������������
WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED: HideAC(False);
end; // case
Default;
end;

// ���� ����� ��������� ������ ��������� �������������� � ����������� �� ��������
// �������� ACType. � ������ ���������� �������� ������
// ������� �� ������ ��������, ������� ������������ ������������� (�������� ACItems),
// �� � �������� �������� ������ ����� ������� �� ������ ���������.
// ��� ����� ����� ��������� (override) ������ ����� � ���������� ������ TACComboBox,
// � ��������� FDropDown.Items ������ ���������� ����������.
// � ��������� AText ���������� ��������� � ���� �������������� ���������� �����.
procedure TACComboBox.PrepareACStrings(const AText: String);
var I: Integer; Proc: TCheckStringEvent; B: Boolean;
begin
Proc:=NIL; // to avoid compiler message "Variable 'Proc' might not have been initialized"
case FACType of
actSimple: Proc:=CheckStringSimple;
actSubString: Proc:=CheckStringSubString;
actCustom:
  begin
  if Assigned(FOnCheckString) then
    Proc:=FOnCheckString
  else // ���� ACType=actCustom � ���������� ������� OnCheckString �� ���������, ����������� ����������
    raise Exception.Create('Event handler for OnCheckString is unassigned');
  end;
end; // case

FDropDown.Items.Clear; // ������� ������ ��������� ���������������
for i:=0 to FACItems.Count-1 do // ���������� ������ �� ������ ACItems...
  begin
  B:=False;
  Proc(Self, AText, FACItems[i], B);
  if B then FDropDown.Items.Add(FACItems[i]); // ...���� ACItems[i] "��������", ��������� ��� � ������ ��������������
  end;
// ��� ������� ���������� ��������� ��������������� �����
// ���� ���� ��������� � FDropDown.Items.BeginUpdate/FDropDown.Items.EndUpdate
end;

procedure TACComboBox.SetACItems(const Value: TStrings);
begin
FACItems.Assign(Value);
end;

procedure TACComboBox.SetParent(AParent: TWinControl);
var Frm: TCustomForm;
begin
// ���� ����� (��� ������������ :)) ��������� ����������� � ����� ����� �� ������,
// ���������� ����� �� "������" ������� ���������
if not (csDesigning in ComponentState) and (FParentFormWnd<>0) then
  SetWindowLong(FParentFormWnd, GWL_WNDPROC, Integer(FOldFormWndProc));

inherited SetParent(AParent);

// ��������� ������� ��������� ������������ �����. ������ ��� ������ � Run-Time,
// �.�. � Design-Time ������ �������������� �� ���������
if not (csDesigning in ComponentState) then
  begin
  Frm:=GetParentForm(Self);

  if Assigned(Frm) then
	  begin
	  FParentFormWnd:=Frm.Handle;
	  FNewFormWndProc:=MakeObjectInstance(ParentFormWndProc);
	  FOldFormWndProc:=Pointer(GetWindowLong(FParentFormWnd, GWL_WNDPROC));
	  SetWindowLong(FParentFormWnd, GWL_WNDPROC, Integer(FNewFormWndProc));
	  end;
  end;
end;

procedure TACComboBox.ShowAC;
var P: TPoint; Cnt: Integer;
begin
if Text='' then // ���� � ���� ����� ���������� �����, ������ ������ ��������������
	begin
  HideAC(False);
	exit;
	end;

PrepareACStrings(Text); // ��������� ������ ������������� ����������, ���������������� ���������� ������

Cnt:=FDropDown.Items.Count;
if Cnt=0 then // ���� ��� ���������� ������ ���������� ��������� ���, ������ ������ ��������������
  begin
  HideAC(False);
  exit;
  end;

// ������ ������ ���� �������������� ����� �������, ����� � ��� ���������� �� ����� ���� �����;
// ���� ��������� ����������� ����� ����, ����� �������� ������������ ������ ���������
if Cnt>5 then Cnt:=5;

FDropped:=True;

SendMessage(Handle, CB_SHOWDROPDOWN, 0, 0); // ������ "������" ���������� ���� ����������

// ���������� ���� �������������� ��� �����������. ������ ������,
// ���� �� ���������� �������, ����� ��� ���� ������������ ��� �����������,
// ���� ��������� ��������� ������� ������ � ������� ���� ������
P.X:=1;
P.Y:=Height-1;
P:=ClientToScreen(P);
SetWindowPos(FDropDown.Handle, HWND_TOPMOST, P.X, P.Y, Width-GetSystemMetrics(SM_CXVSCROLL)-2,
  Cnt*FDropDown.ItemHeight+2, SWP_SHOWWINDOW);
end;

// �.�. ����� �������� � ����������, ��������� �� ������ ���� ����������� ���� �� ��������,
// ������� �������� �� �������������
procedure TACComboBox.WMGetDlgCode(var M: TWMGetDlgCode);
begin
inherited;
if FDropped then
  M.Result:=M.Result or DLGC_WANTMESSAGE;
end;

procedure TACComboBox.WMMouseWheel(var M: TWMMouseWheel);
begin
if FDropped then
	begin
	TMessage(M).Result:=SendMessage(FDropDown.Handle, TMessage(M).Msg, TMessage(M).WParam, TMessage(M).LParam);
	FDropDown.UpdateItemIndex
	end
else
  inherited;
end;

end.
