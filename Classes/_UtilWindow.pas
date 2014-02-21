unit _UtilWindow;

interface

uses Windows, Messages, Classes;

{ TODO : можно использовать message-only window (http://www.transl-gunsmoker.ru/2012/04/windows.html#message_only) }

type TUtilWindow = class
  private
    FHandle: hWnd;
    OldWindowProc: LongInt;
  protected
    FWndProc: Pointer;
    procedure WndProc(var M: TMessage); virtual;
  public
    constructor Create(const AClassName: String = 'TMyUtilWindow'); virtual;
    destructor Destroy; override;
    procedure DefaultHandler(var M); override;
    property Handle: hWnd read FHandle;
  end;

implementation

constructor TUtilWindow.Create(const AClassName: String = 'TMyUtilWindow');
var ClassRegistered: Boolean; dummy, UtilWindowClass: TWndClass;
begin
inherited Create;
FillChar(UtilWindowClass, SizeOf(UtilWindowClass), 0);
UtilWindowClass.lpszClassName:=PChar(AClassName);
UtilWindowClass.lpfnWndProc:=@DefWindowProc;

ClassRegistered:=GetClassInfo(HInstance, UtilWindowClass.lpszClassName, dummy);
if not ClassRegistered then
  Windows.RegisterClass(UtilWindowClass);

FHandle:=CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName, '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);

FWndProc:=MakeObjectInstance(WndProc);
OldWindowProc:=SetWindowLong(FHandle, GWL_WNDPROC, Longint(FWndProc));
end;

destructor TUtilWindow.Destroy;
begin
SetWindowLong(FHandle, GWL_WNDPROC, OldWindowProc);
FreeObjectInstance(FWndProc);

DestroyWindow(FHandle);
inherited Destroy;
end;

{ TAPIWindowTray }

procedure TUtilWindow.DefaultHandler(var M);
begin
with TMessage(M) do Result:=DefWindowProc(FHandle, Msg, WParam, LParam);
end;

procedure TUtilWindow.WndProc(var M: TMessage);
begin
M.Result:=0;
Dispatch(M);
end;

end.
