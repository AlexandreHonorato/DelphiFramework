unit _AsyncCalls;

interface

uses Messages, Windows, SysUtils, _MessageBus, _UtilWindow;

procedure CallAsyncMethod(ASelf: TObject; AMethod: Pointer; AData: Pointer);
procedure CallAsyncProc(AProc: Pointer; AData: Pointer);

implementation

type TAsyncCallerWnd = class(TUtilWindow)
  protected
    procedure WndProc(var M: TMessage); override;
  end;

const
  WM_CallAsyncMethod = WM_USER + 1;
  WM_CallAsyncProc = WM_USER + 2;

{ TAsyncCallerWnd }

procedure TAsyncCallerWnd.WndProc(var M: TMessage);
type TAsyncMethod = procedure(AData: Integer) of object;
type TAsyncProc = procedure(AData: Integer);
var P: PMethod;
begin
if (M.Msg=WM_CallAsyncMethod) then
  begin
  P:=PMethod(M.WParam);
  TAsyncMethod(P^)(M.LParam);
  FreeMem(P, SizeOf(TMethod));
  end
else
if (M.Msg=WM_CallAsyncProc) then
  TAsyncProc(M.WParam)(M.LParam)
else
  inherited WndProc(M);
end;

{ DomainEventsAsyncDispatcher }

var FAsyncCallerWnd: TAsyncCallerWnd;

function AsyncCallerWnd: TAsyncCallerWnd;
begin
if not Assigned(FAsyncCallerWnd) then
  FAsyncCallerWnd:=TAsyncCallerWnd.Create;
Result:=FAsyncCallerWnd;
end;

procedure CallAsyncMethod(ASelf: TObject; AMethod: Pointer; AData: Pointer);
var P: PMethod;
begin
GetMem(P, SizeOf(TMethod));
P^.Code:=AMethod;
P^.Data:=ASelf;
PostMessage(AsyncCallerWnd.Handle, WM_CallAsyncMethod, Integer(P), Integer(AData));
end;

procedure CallAsyncProc(AProc: Pointer; AData: Pointer);
begin
PostMessage(AsyncCallerWnd.Handle, WM_CallAsyncProc, Integer(AProc), Integer(AData));
end;

initialization

finalization
if Assigned(FAsyncCallerWnd) then FreeAndNIL(FAsyncCallerWnd);

end.
