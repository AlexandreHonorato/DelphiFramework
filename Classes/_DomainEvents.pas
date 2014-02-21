unit _DomainEvents;

interface

uses _Debug, _Misc;

type DomainEvent = class(TDebugObject)
  end;

type DomainEventClass = class of DomainEvent;

type TAskParams_Handler = function(AEvent: DomainEvent): Boolean of object;
type TAskParamsMethod = TMethod;
var NoEditParams: TAskParamsMethod = (Code: NIL; Data: NIL);

type DomainEventEdit = class(DomainEvent)
  public
    Changes: TByteSet;
    OnAskParams: TAskParamsMethod;
    function AskParams: Boolean; virtual;
  end;

implementation

{ DomainEventEdit }

function DomainEventEdit.AskParams: Boolean;
begin
if (OnAskParams.Code=NIL) then
  Result:=False
else
  begin
  Changes:=[];
  Result:=TAskParams_Handler(OnAskParams)(Self);
  end;
end;

function NoParamsProc(AEvent: DomainEvent): Boolean;
begin
Result:=True;
end;

initialization
NoEditParams.Code:=@NoParamsProc;

end.
