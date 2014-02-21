unit _MessageBus;

interface

uses
  _Debug, contnrs, _Dictionary, _DomainEvents, SysUtils, Messages,
  Windows;

type PMethod = ^TMethod;

type IMessageBus = class(TDebugObject)
  public
    procedure Notify(AEvent: DomainEvent); overload; virtual; abstract;
    procedure Notify(AEvent: DomainEvent; var StopNotification: Boolean); overload; virtual; abstract;
    procedure NotifyNoParams(AEventClass: DomainEventClass); virtual; abstract;

    procedure RegisterListener(AClass: DomainEventClass; AObject: TObject;
      AMethodAddr: Pointer); virtual; abstract;
    procedure UnregisterListener(AClass: DomainEventClass; AObject: TObject); virtual; abstract;

    procedure UnsignObject(AObject: TObject); virtual; abstract;
    procedure SignObject(AObject: TObject; AClasses: array of DomainEventClass;
      AMethodAddresses: array of Pointer); overload; virtual; abstract;
    procedure SignObject(AObject: TObject; AClasses: array of DomainEventClass;
        AMethodAddress: Pointer); overload; virtual; abstract;
  end;

type TListener = class(TDebugObject)
  protected
    procedure StartListening; virtual;
    procedure StopListening; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

type TListenerClass = class of TListener;

function CreateMessageBus: IMessageBus;
procedure FreeMessageBus(var AMessageBus: IMessageBus);

implementation

uses _AutoDestroyList;

type TEventSource = class(TDebugList)
  private
    function GetMethods(Index: Integer): PMethod;
  public
    function IndexOfMethod(AObject: TObject): Integer;
    procedure AddMethod(AObject: TObject; AMethodAddr: Pointer);
    procedure DeleteMethod(Index: Integer);
    procedure RemoveMethod(AObject: TObject);
    property Methods[Index: Integer]: PMethod read GetMethods;
    procedure Clear; override;
  end;

type TMessageBus = class(IMessageBus)
  private
    FCanDestroy: Boolean;
    FEventSources: TDictionary;
    function GetEventSources(Index: Integer): TEventSource;
    function GetByClass(AClass: DomainEventClass): TEventSource;
    property EventSources[Index: Integer]: TEventSource read GetEventSources;
    property ByClass[AClass: DomainEventClass]: TEventSource read GetByClass; default;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Notify(AEvent: DomainEvent); overload; override;
    procedure Notify(AEvent: DomainEvent; var StopNotification: Boolean); overload; override;
    procedure NotifyNoParams(AEventClass: DomainEventClass); override;

    procedure RegisterListener(AClass: DomainEventClass; AObject: TObject;
      AMethodAddr: Pointer); override;
    procedure UnregisterListener(AClass: DomainEventClass; AObject: TObject); override;

    procedure UnsignObject(AObject: TObject); override;
    procedure SignObject(AObject: TObject; AClasses: array of DomainEventClass;
      AMethodAddresses: array of Pointer); overload; override;
    procedure SignObject(AObject: TObject; AClasses: array of DomainEventClass;
        AMethodAddress: Pointer); overload; override;
  end;

function CreateMessageBus: IMessageBus;
begin
Result:=TMessageBus.Create;
end;

procedure FreeMessageBus(var AMessageBus: IMessageBus);
var mb: TMessageBus; I: Integer; hasListeners: Boolean;
begin
if not Assigned(AMessageBus) then exit;

if AMessageBus is TMessageBus then
  begin
  mb:=TMessageBus(AMessageBus);
  mb.FCanDestroy:=True;

  hasListeners:=False;
  for i:=0 to mb.FEventSources.Count-1 do
    if mb.EventSources[i].Count<>0 then
      begin
      hasListeners:=True;
      break;
      end;
    { TODO : else remember in list and free later, when MB become empty }

  if hasListeners then
    RegisterAutoDestroy(ADL_Domain_Orphan_MessageBuses, mb)
  else
    mb.Free;

  AMessageBus:=NIL;
  end
else
  FreeAndNIL(AMessageBus);
end;

destructor TMessageBus.Destroy;
var I: Integer;
begin
if not FCanDestroy then
  raise Exception.Create('You can''t free MessageBus indirectly. Use FreeMessageBus() procedure');
  
if Assigned(FEventSources) then
	begin
  for i:=0 to FEventSources.Count-1 do EventSources[i].Free;
	FreeAndNIL(FEventSources);
	end;
inherited Destroy;
end;

function TMessageBus.GetByClass(AClass: DomainEventClass): TEventSource;
var Key: Integer;
begin
Key:=Integer(AClass);
Result:=TEventSource(FEventSources.ByKey[Key]);
if Result=NIL then
	begin
  Result:=TEventSource.Create;
	FEventSources.Add(Key, Result);
	end;
end;

function TMessageBus.GetEventSources(Index: Integer): TEventSource;
begin
Result:=TEventSource(FEventSources.Values[Index]);
end;

procedure TMessageBus.Notify(AEvent: DomainEvent);
type TDomainEventHandler = procedure(AEvent: DomainEvent) of object;
var I: Integer; ES: TEventSource; Cls: DomainEventClass;
begin
Cls:=DomainEventClass(AEvent.ClassType);
ES:=ByClass[Cls];
for i:=0 to ES.Count-1 do
  TDomainEventHandler(ES.Methods[i]^)(AEvent);
end;

procedure TMessageBus.Notify(AEvent: DomainEvent; var StopNotification: Boolean);
type TDomainEventHandlerStop = procedure(AEvent: DomainEvent; var StopNotification: Boolean) of object;
var ES: TEventSource; Cls: DomainEventClass; I: Integer;
begin
Cls:=DomainEventClass(AEvent.ClassType);
ES:=ByClass[Cls];

StopNotification:=False;
for i:=0 to ES.Count-1 do
  begin
  TDomainEventHandlerStop(ES.Methods[i]^)(AEvent, StopNotification);
  if StopNotification then break;
  end;
end;

procedure TMessageBus.NotifyNoParams(AEventClass: DomainEventClass);
var Evnt: DomainEvent;
begin
Evnt:=AEventClass.Create;
try
  Notify(Evnt);
finally
  FreeAndNIL(Evnt);
end; // try
end;

procedure TMessageBus.RegisterListener(AClass: DomainEventClass;
  AObject: TObject; AMethodAddr: Pointer);
var I: Integer;
begin
with ByClass[AClass] do
  begin
  I:=IndexOfMethod(AObject);
  if I<>-1 then
    Methods[i]^.Code:=AMethodAddr
  else
    AddMethod(AObject, AMethodAddr);
  end;
end;

procedure TMessageBus.UnregisterListener(AClass: DomainEventClass; AObject: TObject);
begin
ByClass[AClass].RemoveMethod(AObject);
end;

procedure TMessageBus.SignObject(AObject: TObject;
  AClasses: array of DomainEventClass; AMethodAddresses: array of Pointer);
var I: Integer;
begin
for i:=Low(AClasses) to High(AClasses) do
  RegisterListener(AClasses[i], AObject, AMethodAddresses[i]);
end;

procedure TMessageBus.SignObject(AObject: TObject; AClasses: array of
    DomainEventClass; AMethodAddress: Pointer);
var I: Integer;
begin
for i:=Low(AClasses) to High(AClasses) do
  RegisterListener(AClasses[i], AObject, AMethodAddress);
end;

procedure TMessageBus.UnsignObject(AObject: TObject);
var I: Integer;
begin
for i:=0 to FEventSources.Count-1 do
  EventSources[i].RemoveMethod(AObject);
end;

procedure TEventSource.AddMethod(AObject: TObject; AMethodAddr: Pointer);
var P: PMethod;
begin
GetMem(P, SizeOf(TMethod));
P^.Code:=AMethodAddr;
P^.Data:=AObject;
Add(P)
end;

procedure TEventSource.DeleteMethod(Index: Integer);
var P: PMethod;
begin
P:=Methods[Index];
Delete(Index);
FreeMem(P, SizeOf(TMethod));
end;

function TEventSource.IndexOfMethod(AObject: TObject): Integer;
var I: Integer; P: PMethod;
begin
Result:=-1;

for i:=0 to Count-1 do
  begin
  P:=Methods[i];
  if (P^.Data=AObject) then
    begin
    Result:=i;
    break;
    end;
  end;
end;

function TEventSource.GetMethods(Index: Integer): PMethod;
begin
Result:=PMethod(Items[Index])
end;

procedure TEventSource.RemoveMethod(AObject: TObject);
var Idx: Integer;
begin
Idx:=IndexOfMethod(AObject);
if Idx<>-1 then DeleteMethod(Idx);
end;

procedure TEventSource.Clear;
var I: Integer;
begin
for i:=0 to Count-1 do FreeMem(Methods[i], SizeOf(TMethod));
inherited Clear;
end;

{ TListener }

procedure TListener.AfterConstruction;
begin
inherited AfterConstruction;
StartListening;
end;

procedure TListener.BeforeDestruction;
begin
StopListening;
inherited BeforeDestruction;
end;

procedure TListener.StartListening;
begin
//do nothing
end;

procedure TListener.StopListening;
begin
//do nothing
end;

constructor TMessageBus.Create;
begin
inherited Create;
FEventSources:=TDictionary.Create;
FCanDestroy:=False;
end;

end.


