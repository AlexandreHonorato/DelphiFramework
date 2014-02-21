unit _ACCombo;

interface

uses SysUtils, Windows, Messages, Classes, Controls, StdCtrls, Forms;

type // Класс, реализующий выпадающее окно автозавершения
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

type // Комбо-бокс с выпадающем окном автозавершения
  TACComboBox = class(TComboBox)
  private
    FDropped: Boolean; // True, когда показано выпадающее окно
    FACItems: TStrings;
    FOldFormWndProc, FNewFormWndProc: Pointer; // Используется для подмены оконной процедуры родительской формы
    FParentFormWnd: hWnd; // Handle родительской формы
    FACType: TAutoCompleteType;
    FOnCheckString: TCheckStringEvent;
    procedure SetACItems(const Value: TStrings);

    procedure ParentFormWndProc(var Message: TMessage); // Оконная процедура, которой подменяем оконную процедуру родительской формы
  protected
    FDropDown: TDropDownListBox; // Выпадающее окно автозавершения

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

// Метод используется для подсвечивания в ListBox'е элемента, над которым находится курсор мыши
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

// При выборе элемента из списка автозавершения помещаем его в поле ввода комбо-бокса и закрываем окно автозавершения.
if ItemIndex<>-1 then TACComboBox(Owner).HideAC(True);
end;

procedure TDropDownListBox.WMMouseMove(var M: TWMMouseMove);
begin
inherited;
// При движении мыши над выпадающем окном, подсвечиваем элемент, над которым находится курсор мыши.
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

// При переключении на другое приложение (Alt+Tab, etc.) прячем окно автозавершения
TACComboBox(Owner).HideAC(False);
end;

{ TACEdit }

// Вызывается из метода PrepareACStrings при ACType=actSimple для каждого элемента списка ACItems;
// в результате в список вариантов автозавершения добавляются строки из ACItems,
// которые начинаются с введенного в поле комбо-бокса текста
procedure TACComboBox.CheckStringSimple(Sender: TObject; const EditText,
  S: String; var AddString: Boolean);
begin
AddString:=AnsiUpperCase(copy(S, 1, Length(EditText)))=AnsiUpperCase(EditText);
end;

// Вызывается из метода PrepareACStrings при ACType=actSubString для каждого элемента списка ACItems;
// в результате в список вариантов автозавершения добавляются строки из ACItems,
// которые содержат введенный в поле комбо-бокса текст
procedure TACComboBox.CheckStringSubString(Sender: TObject; const EditText,
  S: String; var AddString: Boolean);
begin
AddString:=pos(AnsiUpperCase(EditText), AnsiUpperCase(S))<>0
end;

procedure TACComboBox.CMExit(var M: TCMExit);
begin
inherited;

// при потере фокуса комбо-боксом прячем список автозавершения
HideAC(False);
end;

procedure TACComboBox.CNCommand(var M: TWMCommand);
begin
if M.NotifyCode=CBN_DROPDOWN then
  // если пользователь раскрывает комбо-бокс, прячем наш список автозавершения
  begin
  HideAC(False);
  inherited;
  end
else
if (M.NotifyCode=CBN_EDITCHANGE) then
  begin
  inherited;
  ShowAC // при вводе текста в поле комбо-бокса показываем список автозавершения
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

// в Design-Mode список автозавершения нам не нужен, поэтому создаем его только в Run-Time
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
ShowWindow(FDropDown.Handle, SW_HIDE); // прячем список автозавершения

// если ApplySelection=True, то помещаем в поле редактирования комбо-бокса
// выбранный пользователем элемент из списка автозавершения
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
// если список автозавершения не раскрыт, то нажатия клавиш обрабатываем
// обычным для комбо-бокса образом
if not FDropped then
  begin
  inherited KeyDown(Key, Shift);
  exit;
  end;

case Key of
VK_ESCAPE: // по нажатию Esc прячем список автозавершения
  begin
  HideAC(False);
  PeekMessage(Msg, 0, WM_CHAR, WM_CHAR, PM_REMOVE); // чтобы подавить звуковой сигнал (beep)
  end;

// нажатия стрелок вверх/вниз, PgUp/PgDn перадаем окну автозавершения, чтобы перемещаться по списку вариантов
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

  // скрываем от системы факт нажатия стрелки/PgUp/PgDn, иначе, кроме перемещения по списку
  // автозавершения, будут выполняться действия для комбо-бокса по умолчанию, т.е.
  // в поле ввода комбобокса будут показываться строки из свойства Items
  Key:=0;
  end;

// по нажатию Enter закрываем окно автозавершения и помещаем в поле редактирования
// комбо-бокса выбранный элемент из списка автозавершения
VK_RETURN:
  begin
  HideAC(True);
  PeekMessage(Msg, 0, WM_CHAR, WM_CHAR, PM_REMOVE); // чтобы подавить звуковой сигнал (beep)
  end
else
  inherited KeyDown(Key, Shift);
end; // case
end;

procedure TACComboBox.KeyPress(var Key: Char);
begin
// нажатие клавиши BackSpace (в отличие от буквенно-цифровых клавиш и клавиши Del)
// не генерит сообщения CBN_EDITCHANGE, и как следствие, обновление списка автозавершения
// не происходит, поэтому приходится обрабатывать VK_BACK отдельно
if Ord(Key)=VK_BACK then
  begin
  inherited KeyPress(Key);
  ShowAC;
  exit;
  end
else
  inherited KeyPress(Key);
end;

// Этой процедурой подменяем оконную процедуру родительской формы (эта форма не обязательно
// является непосредственным Parent'ом комбо-бокса)
procedure TACComboBox.ParentFormWndProc(var Message: TMessage);

  procedure Default;
  begin
  with Message do Result:=CallWindowProc(FOldFormWndProc, FParentFormWnd, Msg, wParam, lParam);
  end;

begin
case Message.Msg of // при изменении положения родительской формы прячем окно автозавершения
WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED: HideAC(False);
end; // case
Default;
end;

// этот метод заполняет список вариантов автозавершения в зависимости от значения
// свойства ACType. В данной реализации исходные данные
// берутся из списка значений, заранее определенных пользователем (свойство ACItems),
// но в принципе исходные данные могут браться из любого источника.
// Для этого нужно перекрыть (override) данный метод в наследнике класса TACComboBox,
// и заполнить FDropDown.Items любыми требуемыми значениями.
// В параметре AText передается введенный в поле редактирования комбобокса текст.
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
  else // если ACType=actCustom и обработчик события OnCheckString не определен, выбрасываем исключение
    raise Exception.Create('Event handler for OnCheckString is unassigned');
  end;
end; // case

FDropDown.Items.Clear; // очищаем список вариантов автозаверешения
for i:=0 to FACItems.Count-1 do // перебираем строки из списка ACItems...
  begin
  B:=False;
  Proc(Self, AText, FACItems[i], B);
  if B then FDropDown.Items.Add(FACItems[i]); // ...если ACItems[i] "подходит", добавляем его в список автозавершения
  end;
// при большом количестве вариантов автозаверешения можно
// этот цикл заключить в FDropDown.Items.BeginUpdate/FDropDown.Items.EndUpdate
end;

procedure TACComboBox.SetACItems(const Value: TStrings);
begin
FACItems.Assign(Value);
end;

procedure TACComboBox.SetParent(AParent: TWinControl);
var Frm: TCustomForm;
begin
// Если вдруг (что маловероятно :)) компонент переносится с одной формы на другую,
// возвращаем форме ее "родную" оконную процедуру
if not (csDesigning in ComponentState) and (FParentFormWnd<>0) then
  SetWindowLong(FParentFormWnd, GWL_WNDPROC, Integer(FOldFormWndProc));

inherited SetParent(AParent);

// Подменяем оконную процедуру родительской формы. Делаем это только в Run-Time,
// т.к. в Design-Time список автозавершения не создается
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
if Text='' then // если в поле ввода комбобокса пусто, прячем список автозавершения
	begin
  HideAC(False);
	exit;
	end;

PrepareACStrings(Text); // заполняем список автозаверения вариантами, соответствующими введенному тексту

Cnt:=FDropDown.Items.Count;
if Cnt=0 then // если для введенного текста подходящих вариантов нет, прячем список автозавершения
  begin
  HideAC(False);
  exit;
  end;

// задаем высоту окна автозавершения таким образом, чтобы в нем помещалось не более пяти строк;
// если вариантов заверешения более пяти, будет показана вертикальная полоса прокрутки
if Cnt>5 then Cnt:=5;

FDropped:=True;

SendMessage(Handle, CB_SHOWDROPDOWN, 0, 0); // прячем "родное" выпадающее окно комбобокса

// показываем окно автозавершения под комбобоксом. Вообще говоря,
// было бы правильным сделать, чтобы это окно показывалось над комбобоксом,
// если комбобокс находится слишком близко к нижнему краю экрана
P.X:=1;
P.Y:=Height-1;
P:=ClientToScreen(P);
SetWindowPos(FDropDown.Handle, HWND_TOPMOST, P.X, P.Y, Width-GetSystemMetrics(SM_CXVSCROLL)-2,
  Cnt*FDropDown.ItemHeight+2, SWP_SHOWWINDOW);
end;

// т.к. фокус остается в комбобоксе, сообщения от колеса мыши выпадающему окну не приходят,
// поэтому передаем их принудительно
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
