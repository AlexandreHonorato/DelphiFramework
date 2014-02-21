unit _MVC_Model;

interface

uses _Debug, _MessageBus, SysUtils, _DomainEvents, _NonCOMInterface, _Misc;

type TMVC_Model = class(TNonCOMInterface)
  private
    FMessageBus: IMessageBus;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure StartListening; virtual;
    procedure StopListening; virtual;
    property MessageBus: IMessageBus read FMessageBus;

    procedure Dispose;
    procedure NotifyChanged(AChanges: TByteSet);
  end;

type NEDisposeModel = class(DomainEvent)
  public
    Model: TMVC_Model;
  end;

type NEModelChanged = class(DomainEvent)
  public
    Changes: TByteSet;
  end;

implementation

constructor TMVC_Model.Create;
begin
inherited Create;
FMessageBus:=CreateMessageBus;
end;

destructor TMVC_Model.Destroy;
begin
FreeMessageBus(FMessageBus);
inherited Destroy;
end;

procedure TMVC_Model.Dispose;
var Event: NEDisposeModel;
begin
Event:=NEDisposeModel.Create;
try
  Event.Model:=Self;

  MessageBus.Notify(Event);
finally
  FreeAndNIL(Event);
end; // try
end;

{ TMVC_Model }

procedure TMVC_Model.NotifyChanged(AChanges: TByteSet);
var evnt: NEModelChanged;
begin
evnt:=NEModelChanged.Create;
try
  evnt.Changes:=AChanges;

  MessageBus.Notify(evnt);
finally
  FreeAndNIL(evnt);
end; // try
end;

procedure TMVC_Model.StartListening;
begin
//do nothing
end;

procedure TMVC_Model.StopListening;
begin
//do nothing
end;

end.
