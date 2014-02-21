unit uCodeBase;

interface

uses
  uCodeEntities, SysUtils;

type TCodeBase = class
  private
    FRoot: TCodeUnit;
    FUnits: TCodeUnits;
    FCaption: String;
    FClassesIndex: TUnitElementIndex;
    FInterfacesIndex: TUnitElementIndex;
    FMetaclassesIndex: TUnitElementIndex;
    FSingletonsIndex: TUnitElementIndex;
    FLayers: TApplicationLayers;
  public
    property Caption: String read FCaption write FCaption;

    property Units: TCodeUnits read FUnits;
    property Root: TCodeUnit read FRoot;

    property ClassesIndex: TUnitElementIndex read FClassesIndex;
    property InterfacesIndex: TUnitElementIndex read FInterfacesIndex;
    property MetaclassesIndex: TUnitElementIndex read FMetaclassesIndex;
    property SingletonsIndex: TUnitElementIndex read FSingletonsIndex;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    procedure RecreateRoot;

    procedure AddUnit(AUnit: TCodeUnit);
    procedure RemoveUnit(AUnit: TCodeUnit);
    procedure AddClass(AClass: TCodeClass; AUnit: TCodeUnit);
    procedure AddInterface(AInterface: TCodeInterface; AUnit: TCodeUnit);

    procedure NotifyChanged; virtual;
    procedure NotifyClear; virtual;

    procedure GetAllClasses(AClasses: TCodeClassList; ASortByName: Boolean);
    procedure GetAllInterfaces(AInterfaces: TCodeInterfaceList; ASortByName: Boolean);
    procedure GetAllMetaclasses(AMetaclasses: TCodeMetaclassList; ASortByName: Boolean);
    procedure GetAllSingletons(ASingletons: TCodeSingletonList; ASortByName: Boolean);

    function FindOrCreateClass(const AClassName: String): TCodeClass;
    function FindOrCreateInterface(const AInterfaceName: String): TCodeInterface;

    property Layers: TApplicationLayers read FLayers;
  end;

implementation

constructor TCodeBase.Create;
begin
inherited Create;
FUnits:=TCodeUnits.Create(True);

FClassesIndex:=TUnitElementIndex.Create;
FInterfacesIndex:=TUnitElementIndex.Create;
FMetaclassesIndex:=TUnitElementIndex.Create;
FSingletonsIndex:=TUnitElementIndex.Create;

FLayers:=TApplicationLayers.Create;

RecreateRoot;
end;

destructor TCodeBase.Destroy;
begin
if Assigned(FLayers) then FreeAndNIL(FLayers);

if Assigned(FSingletonsIndex) then FreeAndNIL(FSingletonsIndex);
if Assigned(FMetaclassesIndex) then FreeAndNIL(FMetaclassesIndex);
if Assigned(FClassesIndex) then FreeAndNIL(FClassesIndex);
if Assigned(FInterfacesIndex) then FreeAndNIL(FInterfacesIndex);
if Assigned(FUnits) then FreeAndNIL(FUnits);
if Assigned(FRoot) then FreeAndNIL(FRoot);
inherited Destroy;
end;

procedure TCodeBase.GetAllClasses(AClasses: TCodeClassList; ASortByName: Boolean);
var I, J: Integer; unit_: TCodeUnit;
begin
AClasses.Clear;

for i:=0 to FUnits.Count-1 do
  begin
  unit_:=FUnits[i];
  for j:=0 to unit_.Classes.Count-1 do
    AClasses.Add(unit_.Classes[j])
  end;

if ASortByName then
  AClasses.SortByName(False);
end;

procedure TCodeBase.Reset;
begin
FCaption:='';

FClassesIndex.Clear;
FInterfacesIndex.Clear;
FMetaclassesIndex.Clear;
FSingletonsIndex.Clear;

FUnits.Clear;

RecreateRoot;

NotifyClear;
end;

procedure TCodeBase.AddUnit(AUnit: TCodeUnit);
begin
FClassesIndex.AddElementList(AUnit.Classes);
FInterfacesIndex.AddElementList(AUnit.Interfaces);
FMetaclassesIndex.AddElementList(AUnit.Metaclasses);
FSingletonsIndex.AddElementList(AUnit.Singletons);
FUnits.AddUnit(AUnit);
end;

procedure TCodeBase.RemoveUnit(AUnit: TCodeUnit);
begin
FClassesIndex.RemoveElementList(AUnit.Classes);
FInterfacesIndex.RemoveElementList(AUnit.Interfaces);
FMetaclassesIndex.RemoveElementList(AUnit.Metaclasses);
FSingletonsIndex.RemoveElementList(AUnit.Singletons);
FUnits.RemoveUnit(AUnit.ID);
end;

procedure TCodeBase.NotifyChanged;
begin
// do nothing
end;

procedure TCodeBase.NotifyClear;
begin
// do nothing
end;

procedure TCodeBase.RecreateRoot;
begin
if Assigned(FRoot) then
  begin
  FClassesIndex.RemoveElementList(FRoot.Classes);
  FInterfacesIndex.RemoveElementList(FRoot.Interfaces);
  FMetaclassesIndex.RemoveElementList(FRoot.Metaclasses);
  FSingletonsIndex.RemoveElementList(FRoot.Singletons);
  FreeAndNIL(FRoot);
  end;

FRoot:=TCodeUnit.CreateWithCaption('<Root classes>');  
end;

procedure TCodeBase.AddClass(AClass: TCodeClass; AUnit: TCodeUnit);
begin
FClassesIndex.AddElement(AClass);
AUnit.Classes.Add(AClass);
end;

procedure TCodeBase.AddInterface(AInterface: TCodeInterface;
  AUnit: TCodeUnit);
begin
FInterfacesIndex.AddElement(AInterface);
AUnit.Interfaces.Add(AInterface);
end;

{ TCodeBase }

function TCodeBase.FindOrCreateClass(const AClassName: String): TCodeClass;
begin
Result:=TCodeClass(FClassesIndex.ByName(AClassName));
if Assigned(Result) then exit;

// если не нашли, то класс объ€влен не в проекте, поэтому добавл€ем его в CodeBase.Root
Result:=TCodeClass.Create(FRoot, AClassName, -1);

try
  AddClass(Result, FRoot);
except
  FreeAndNIL(Result);
  raise
end; // try
end;

{ TCodeBase }

function TCodeBase.FindOrCreateInterface(const AInterfaceName: String):
    TCodeInterface;
begin
Result:=TCodeInterface(FInterfacesIndex.ByName(AInterfaceName));
if Assigned(Result) then exit;

// если не нашли, то интерфейс объ€влен не в проекте, поэтому добавл€ем его в CodeBase.Root
Result:=TCodeInterface.Create(FRoot, AInterfaceName, -1);

try
  AddInterface(Result, FRoot);
except
  FreeAndNIL(Result);
  raise
end; // try
end;

procedure TCodeBase.GetAllInterfaces(AInterfaces: TCodeInterfaceList;
    ASortByName: Boolean);
var I, J: Integer; unit_: TCodeUnit;
begin
AInterfaces.Clear;

for i:=0 to FUnits.Count-1 do
  begin
  unit_:=FUnits[i];
  for j:=0 to unit_.Interfaces.Count-1 do
    AInterfaces.Add(unit_.Interfaces[j])
  end;

if ASortByName then
  AInterfaces.SortByName;
end;

procedure TCodeBase.GetAllMetaclasses(AMetaclasses: TCodeMetaclassList;
    ASortByName: Boolean);
var I, J: Integer; unit_: TCodeUnit;
begin
AMetaclasses.Clear;

for i:=0 to FUnits.Count-1 do
  begin
  unit_:=FUnits[i];
  for j:=0 to unit_.Metaclasses.Count-1 do
    AMetaclasses.Add(unit_.Metaclasses[j])
  end;

if ASortByName then
  AMetaclasses.SortByName; 
end;

procedure TCodeBase.GetAllSingletons(ASingletons: TCodeSingletonList;
    ASortByName: Boolean);
var I, J: Integer; unit_: TCodeUnit;
begin
ASingletons.Clear;

for i:=0 to FUnits.Count-1 do
  begin
  unit_:=FUnits[i];
  for j:=0 to unit_.Singletons.Count-1 do
    ASingletons.Add(unit_.Singletons[j])
  end;

if ASortByName then
  ASingletons.SortByName;
end;

end.
