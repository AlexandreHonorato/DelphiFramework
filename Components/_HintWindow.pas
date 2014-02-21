unit _HintWindow;

interface

uses Windows, Messages, Classes, Controls, Forms;

type
  TGetPosEvent = procedure(Sender: TObject; var P: TPoint) of Object;
  TGetCaptionEvent = procedure(Sender: TObject; var S: String) of Object;

type TMyHintWindow = class(THintWindow)
  private
    FOnGetPos: TGetPosEvent;
    FOnGetCaption: TGetCaptionEvent;
  protected
    procedure WMActivateApp(var M: TMessage); message WM_ACTIVATEAPP;
  public
    procedure HideHint;
    procedure ShowHint(P: TPoint; const AHint: String = ''); overload;
    procedure ShowHint(const AHint: String = ''); overload;
    constructor Create(AOwner: TComponent); override;
    property OnGetPos: TGetPosEvent read FOnGetPos write FOnGetPos;
    property OnGetCaption: TGetCaptionEvent read FOnGetCaption write FOnGetCaption;
  end;

var AppHW: TMyHintWindow;

implementation

procedure TMyHintWindow.ShowHint(P: TPoint; const AHint: String = '');
var R1, R: TRect; S: String;
begin
S:=AHint;
if S='' then
  begin
  if Assigned(FOnGetCaption) then
    FOnGetCaption(Self, S)
  else
    S:=Caption
  end;

if Caption<>S then
	begin
	Caption:=S;
  Invalidate
	end;

if not IsWindowVisible(Handle) then
	begin
	R1:=CalcHintRect(Screen.Width div 2, S, NIL);
	R:=Bounds(P.X, P.Y, R1.Right-R1.Left, R1.Bottom-R1.Top);
	ActivateHint(R, S);
	end
else
  SetWindowPos(Handle, HWND_TOP, P.X, P.Y, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOZORDER);
end;

procedure TMyHintWindow.HideHint;
begin
ShowWindow(Handle, SW_HIDE);
end;

procedure TMyHintWindow.ShowHint(const AHint: String = '');
var P: TPoint;
begin
if Assigned(FOnGetPos) then
  FOnGetPos(Self, P)
else
	begin
	GetCursorPos(P);
	P.X:=P.X+12;
	P.Y:=P.Y+24;
	end;
ShowHint(P, AHint)
end;

constructor TMyHintWindow.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
Color:=Application.HintColor
end;

procedure TMyHintWindow.WMActivateApp(var M: TMessage);
begin
inherited;
HideHint
end;

end.
