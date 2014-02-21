unit _Null;

interface

uses _IntList, _Debug, SysUtils, _MessageBus,
  _DomainEvents, _BaseDataSource_ViewModel,
  _AutoDestroyList, Windows, _EntitySelection, Classes;

type TBaseNull = class(TDebugObject)
  private
    // костыль, для корректного разрушения объектов из FNullObjects, которые ссылаются на другие Null-объекты,
    // оказавшиеся в этом списке раньше, и, следовательно, уже разрушенные
    FDestroying: Boolean;

    FMessageBus: IMessageBus;
    FIntList: TIntList;
    FBaseDataSource: TBaseDataSource_ViewModel;
    FEntitySelection: TEntitySelection;
    FStrings: TStrings;
  protected
    FNullObjects: TDebugList;
  public
    function IsNull(AObject: TObject): Boolean;
    function IsNotNull(AObject: TObject): Boolean;

    // только для функций IsNull / IsNotNull; не разрушается в деструкторе TNull
    procedure MarkNull(AObject: TObject);

    function _IntList: TIntList;
    function _BaseDataSource_ViewModel: TBaseDataSource_ViewModel;
    function _IMessageBus: IMessageBus; overload;
    function _TEntitySelection: TEntitySelection; overload;
    function _Strings: TStrings;

    constructor Create; override;
    destructor Destroy; override;
  end;

type TBaseNullClass = class of TBaseNull;
var NullClass: TBaseNullClass = TBaseNull;

function BaseNull: TBaseNull;

implementation

var FNull: TBaseNull;

function BaseNull: TBaseNull;
begin
if not Assigned(FNull) then
  begin
  FNull:=NullClass.Create;
  RegisterAutoDestroy(ADL_Domain_Null, FNull)
  end;
Result:=FNull;
end;

type TNullMessageBusImpl = class(IMessageBus)
  public
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

{ TNullMessageBusImpl }

procedure TNullMessageBusImpl.Notify(AEvent: DomainEvent);
begin
//do nothing
end;

procedure TNullMessageBusImpl.Notify(AEvent: DomainEvent; var StopNotification: Boolean);
begin
//do nothing
end;

procedure TNullMessageBusImpl.NotifyNoParams(AEventClass: DomainEventClass);
begin
//do nothing
end;

procedure TNullMessageBusImpl.RegisterListener(AClass: DomainEventClass;
  AObject: TObject; AMethodAddr: Pointer);
begin
//do nothing
end;

procedure TNullMessageBusImpl.SignObject(AObject: TObject;
  AClasses: array of DomainEventClass; AMethodAddresses: array of Pointer);
begin
//do nothing
end;

procedure TNullMessageBusImpl.SignObject(AObject: TObject;
  AClasses: array of DomainEventClass; AMethodAddress: Pointer);
begin
//do nothing
end;

procedure TNullMessageBusImpl.UnregisterListener(AClass: DomainEventClass; AObject: TObject);
begin
//do nothing
end;

procedure TNullMessageBusImpl.UnsignObject(AObject: TObject);
begin
//do nothing
end;

{ TBaseNull }

constructor TBaseNull.Create;
begin
inherited Create;
FDestroying:=False;
FNullObjects:=TDebugList.Create;
end;

destructor TBaseNull.Destroy;
begin
if Assigned(FIntList) then FreeAndNIL(FIntList);
if Assigned(FStrings) then FreeAndNIL(FStrings);
if Assigned(FEntitySelection) then FreeAndNIL(FEntitySelection);
if Assigned(FBaseDataSource) then FreeAndNIL(FBaseDataSource);
if Assigned(FMessageBus) then FreeAndNIL(FMessageBus);

if Assigned(FNullObjects) then FreeAndNIL(FNullObjects);
inherited Destroy;
end;

function TBaseNull._IntList: TIntList;
begin
if not Assigned(FIntList) then
  begin
  FIntList:=TIntList.Create;
  FNullObjects.Add(FIntList);
  end;
Result:=FIntList;
end;

function TBaseNull.IsNull(AObject: TObject): Boolean;
begin
if FDestroying then
  Result:=True
else
  Result:=FNullObjects.IndexOf(AObject)<>-1
end;

function TBaseNull.IsNotNull(AObject: TObject): Boolean;
begin
if FDestroying then
  Result:=False
else
  Result:=FNullObjects.IndexOf(AObject)=-1
end;

function TBaseNull._BaseDataSource_ViewModel: TBaseDataSource_ViewModel;
begin
if not Assigned(FBaseDataSource) then
  begin
  FBaseDataSource:=TBaseDataSource_ViewModel.CreateWithNullMessageBus;
  FNullObjects.Add(FBaseDataSource);
  end;
Result:=FBaseDataSource;
end;


function TBaseNull._IMessageBus: IMessageBus;
begin
if not Assigned(FMessageBus) then
  begin
  FMessageBus:=TNullMessageBusImpl.Create;
  FNullObjects.Add(FMessageBus);
  end;
Result:=FMessageBus;
end;

function TBaseNull._TEntitySelection: TEntitySelection;
begin
if not Assigned(FEntitySelection) then
  begin
  FEntitySelection:=TEntitySelection.Create(TEntitySelectionImpl_Null.Create);
  FNullObjects.Add(FEntitySelection);
  end;
Result:=FEntitySelection;
end;

function TBaseNull._Strings: TStrings;
begin
if not Assigned(FStrings) then
  begin
  FStrings:=TStringList.Create;
  FNullObjects.Add(FStrings);
  end;
Result:=FStrings;
end;

procedure TBaseNull.MarkNull(AObject: TObject);
begin
FNullObjects.Add(AObject)
end;

end.
