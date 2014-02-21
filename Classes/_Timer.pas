unit _Timer;

interface

uses Windows, Classes, Messages, Forms, Consts;

type
  TTimer = class
  private
    FInterval: Cardinal;
    FWindowHandle: HWND;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AInterval: Cardinal; AOnTimer: TNotifyEvent); virtual;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;
  end;

implementation

{ TTimer }

constructor TTimer.Create(AInterval: Cardinal; AOnTimer: TNotifyEvent);
begin
  inherited Create;
  FEnabled := False;
  FInterval := AInterval;
  FOnTimer:=AOnTimer;
  FWindowHandle := Classes.AllocateHWnd(WndProc);
end;

destructor TTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  Classes.DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

procedure TTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        Timer;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TTimer.UpdateTimer;
begin
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
end;

procedure TTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

end.
