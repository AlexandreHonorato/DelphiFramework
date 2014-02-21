unit _VCL_ZZZ;

interface

uses Windows, Forms, Menus, Controls;

procedure CheckSecondMonitor(AForm: TForm);
procedure TrackMenuAtControl(Menu: TPopupMenu; Control: TControl);
function ControlBottom(AControl: TControl): Integer;
function ControlRight(AControl: TControl): Integer;

implementation

procedure CheckSecondMonitor(AForm: TForm);
begin
if Screen.MonitorCount>1 then AForm.Left:=AForm.Left+Screen.Monitors[0].Width;
end;

procedure TrackMenuAtControl(Menu: TPopupMenu; Control: TControl);
var P: TPoint;
begin
P.X:=0;
P.Y:=Control.Height;
P:=Control.ClientToScreen(P);
Menu.Popup(P.X, P.Y);
end;

function ControlBottom(AControl: TControl): Integer;
begin
Result:=AControl.Top+AControl.Height
end;

function ControlRight(AControl: TControl): Integer;
begin
Result:=AControl.Left+AControl.Width
end;

end.
