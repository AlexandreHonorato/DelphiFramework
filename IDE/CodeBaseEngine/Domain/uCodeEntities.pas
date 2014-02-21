unit uCodeEntities;

interface

uses Classes, SysUtils, _MessageBus, _DomainEvents, _DomainEventsList, Windows,
  _Lists, _Dictionary, _Entity, Graphics, _XML10, TLB_MSXML;

const
  sTObject = 'TObject';
  sUnknownParent = '';

type
  TUnitDependence = (udUsesItf, udUsesImpl, udUsedByItf, udUsedByImpl);
  TUnitSection = (usItf, usImpl);

function UnitSection2Uses(AUnitSection: TUnitSection): TUnitDependence;
function UnitSection2UsedBy(AUnitSection: TUnitSection): TUnitDependence;

type TCodeObjectType = (coUnknown, coUnit, coClass, coInterface, coMetaclass,
  coSingleton);
type TCodeObjectTypes = set of TCodeObjectType;

type TUnitSource = (usUnknown, usProject, usFramework);

type
  TCodeObject = class
  private
    FParseInfo: TObject;
  public
    property ParseInfo: TObject read FParseInfo write FParseInfo;
    constructor Create; virtual;
    destructor Destroy; override;
    class function ObjectType: TCodeObjectType; virtual;
  end;

type TCodeObjectList = class(TOwningList)
  private
    function GetItems(Index: Integer): TCodeObject;
  public
    property Items[Index: Integer]: TCodeObject read GetItems; default;
  end;

type TBaseCodeUnit = class(TCodeObject)
  private
    FCaptionWithExt: String;
    FCaptionWithoutExt: String;
    FFileName: String;
    FID: string;
    FUnitSource: TUnitSource;
    procedure SetFileName(const Value: String);
  public
    class function ObjectType: TCodeObjectType; override;
    class function FileNameToUnitID(const AFileName: String): String;

    property ID: string read FID;
    function Caption(AWithExt: Boolean): String;
    property FileName: String read FFileName write SetFileName;
    property UnitSource: TUnitSource read FUnitSource write FUnitSource;
  end;

type TBaseCodeUnitList = class(TOwningStringList)
  private
    function GetItems(Index: Integer): TBaseCodeUnit;
  public
    procedure AddUnit(AUnit: TBaseCodeUnit);
    procedure RemoveUnit(const AUnitID: String);

    function ByUnitID(const AUnitID: String): TBaseCodeUnit; overload;
    function ByUnitID(const AUnitID: String; out AIndex: Integer): TBaseCodeUnit; overload;

    property Items[Index: Integer]: TBaseCodeUnit read GetItems; default;
  end;

type
  TCodeUnit = class;
  TCodeClassList = class;
  TCodeInterfaceList = class;

  TUnitElement = class(TCodeObject)
  private
    FInterfaceLine: Integer;
    FName: String;
    FUnit: TCodeUnit;
  public
    property InterfaceLine: Integer read FInterfaceLine;
    property Name: String read FName;
    property Unit_: TCodeUnit read FUnit;
    constructor Create(AUnit: TCodeUnit; const AName: String; AInterfaceLine: Integer); reintroduce; virtual;
    class function TypeName: String; virtual; abstract;
  end;

  TUnitElementList = class(TOwningList)
  private
    function GetItems(Index: Integer): TUnitElement;
  public
    property Items[Index: Integer]: TUnitElement read GetItems; default;

    function ByName(const AUnitElementName: String): TUnitElement; overload;
    function ByName(const AUnitElementName: String; out AIndex: Integer): TUnitElement; overload;

    procedure SortByName;
  end;

  TCodeClass = class(TUnitElement)
  private
    FParent: TCodeClass;
    FChildren: TCodeClassList;
    FImplementedInterfaces: TCodeInterfaceList;
  public
    property Parent: TCodeClass read FParent write FParent;
    property Children: TCodeClassList read FChildren;
    property ImplementedInterfaces: TCodeInterfaceList read FImplementedInterfaces;
    constructor Create(AUnit: TCodeUnit; const AName: String; AInterfaceLine: Integer); override;
    destructor Destroy; override;
    class function ObjectType: TCodeObjectType; override;
    class function TypeName: String; override;
  end;

  TCodeClassList = class(TUnitElementList)
  private
    function GetItems(Index: Integer): TCodeClass;
  public
    property Items[Index: Integer]: TCodeClass read GetItems; default;

    procedure SortByName(ARecursive: Boolean); reintroduce;
  end;

  TCodeInterface = class(TUnitElement)
  private
    FImplementedIn: TCodeClassList;
  public
    constructor Create(AUnit: TCodeUnit; const AName: String; AInterfaceLine: Integer); override;
    destructor Destroy; override;
    class function ObjectType: TCodeObjectType; override;
    class function TypeName: String; override;
    property ImplementedIn: TCodeClassList read FImplementedIn;
  end;

  TCodeInterfaceList = class(TUnitElementList)
  private
    function GetItems(Index: Integer): TCodeInterface;
  public
    property Items[Index: Integer]: TCodeInterface read GetItems; default;
  end;

  TCodeMetaclass = class(TUnitElement)
  private
    FClass: TCodeClass;
  public
    class function ObjectType: TCodeObjectType; override;
    class function TypeName: String; override;
    property Class_: TCodeClass read FClass write FClass;
  end;

  TCodeMetaclassList = class(TUnitElementList)
  private
    function GetItems(Index: Integer): TCodeMetaclass;
  public
    property Items[Index: Integer]: TCodeMetaclass read GetItems; default;
  end;

  TCodeSingleton = class(TUnitElement)
  public
    class function ObjectType: TCodeObjectType; override;
    class function TypeName: String; override;
  end;

  TCodeSingletonList = class(TUnitElementList)
  private
    function GetItems(Index: Integer): TCodeSingleton;
  public
    property Items[Index: Integer]: TCodeSingleton read GetItems; default;
  end;

  TCodeUnits = class;

  TCodeUnit = class(TBaseCodeUnit)
  private
    FClasses: TCodeClassList;
    FInterfaces: TCodeInterfaceList;
    FMetaclasses: TCodeMetaclassList;
    FSingletons: TCodeSingletonList;
    FImplementationLine: Integer;
    FLayer: Integer;
    FUnitLinks: array[TUnitDependence] of TCodeUnits;
    function GetUnitLinks(Index: TUnitDependence): TCodeUnits;
  public
    property Layer: Integer read FLayer write FLayer;
    property Classes: TCodeClassList read FClasses;
    property Interfaces: TCodeInterfaceList read FInterfaces;
    property Metaclasses: TCodeMetaclassList read FMetaclasses;
    property Singletons: TCodeSingletonList read FSingletons;
    property ImplementationLine: Integer read FImplementationLine write FImplementationLine;
    property UnitLinks[Index: TUnitDependence]: TCodeUnits read GetUnitLinks;
    constructor Create; override;
    constructor CreateWithCaption(const ACaption: String);
    destructor Destroy; override;
    function CanUse(AUsedUnit: TCodeUnit; AUnitSection: TUnitSection): Boolean;
  end;

  TCodeUnits = class(TBaseCodeUnitList)
  private
    function GetItems(Index: Integer): TCodeUnit;
  public
    property Items[Index: Integer]: TCodeUnit read GetItems; default;
  end;

type TUnitElementIndex = class(TStringList)
  private
    function GetUnitElements(Index: Integer): TUnitElement;
  public
    constructor Create; virtual;

    function ByName(const AUnitElementName: String): TUnitElement; overload;
    function ByName(const AUnitElementName: String; out AIndex: Integer): TUnitElement; overload;

    procedure AddElement(AUnitElement: TUnitElement);
    procedure RemoveElement(AUnitElement: TUnitElement);

    procedure AddElementList(AUnitElementList: TUnitElementList);
    procedure RemoveElementList(AUnitElementList: TUnitElementList);

    property UnitElements[Index: Integer]: TUnitElement read GetUnitElements;
  end;

const MAX_LAYER_COUNT = 15;

type TUsesStatus = (sError, sNotResolved, sOK, sNoUnits);

type TUnitLayer = class(TEntity)
  private
    FColor: TColor;
  public
    UsesStatus: array[TUnitSection] of array[0..MAX_LAYER_COUNT-1] of TUsesStatus;
    property Color: TColor read FColor;
    constructor Create(AID: Integer; const ACaption: String; AColor: TColor); reintroduce;
  end;

type TApplicationLayers = class(TEntityList)
  private
    procedure UpdateLayers;
    function GetItems(Index: Integer): TUnitLayer;
  public
    constructor Create; reintroduce;
    function ByID(ID: Integer): TUnitLayer; reintroduce; overload;
    function ByID(ID: Integer; var Idx: Integer): TUnitLayer; reintroduce; overload;

    function ByName(const ALayerName: String): TUnitLayer;

    property Items[Index: Integer]: TUnitLayer read GetItems; default;
  end;

type TFrameWork = class
  private
    FUnits: TBaseCodeUnitList;
  public
    procedure UpdateUnits;
    property Units: TBaseCodeUnitList read FUnits;
    constructor Create;
    destructor Destroy; override;

    procedure LoadFileNames(AFileNames: TStrings);
    procedure SaveFileNames(AFileNames: TStrings);
  end;

implementation

uses uParseInfo;

const
  FN_FrameworkUnits = 'd:\andrey\delphi\__FrameWork\IDEConfig\UseUnits.dat';
  FN_ApplicationLayers = 'd:\andrey\delphi\__FrameWork\IDEConfig\layers.xml';

{ TCodeClass }

constructor TCodeClass.Create(AUnit: TCodeUnit; const AName: String;
    AInterfaceLine: Integer);
begin
inherited Create(AUnit, AName, AInterfaceLine);
FParent:=NIL;
FChildren:=TCodeClassList.Create(False);
FImplementedInterfaces:=TCodeInterfaceList.Create(False);
end;

destructor TCodeClass.Destroy;
begin
if Assigned(FChildren) then FreeAndNIL(FChildren);
if Assigned(FImplementedInterfaces) then FreeAndNIL(FImplementedInterfaces);
inherited Destroy;
end;

{ TCodeClass }

class function TCodeClass.ObjectType: TCodeObjectType;
begin
Result:=coClass
end;

function TCodeClassList.GetItems(Index: Integer): TCodeClass;
begin
Result:=TCodeClass(inherited Items[Index])
end;

function TCodeUnit.CanUse(AUsedUnit: TCodeUnit; AUnitSection: TUnitSection): Boolean;
begin
Result:=TParseInfo_Unit(ParseInfo).CanUse[AUnitSection].IndexOf(AUsedUnit.ID)<>-1
end;

constructor TCodeUnit.Create;
var ud: TUnitDependence;
begin
inherited Create;
FImplementationLine:=-1;
FClasses:=TCodeClassList.Create(True);
FInterfaces:=TCodeInterfaceList.Create(True);
FMetaclasses:=TCodeMetaclassList.Create(True);
FSingletons:=TCodeSingletonList.Create(True);

for ud:=Low(TUnitDependence) to High(TUnitDependence) do
  FUnitLinks[ud]:=TCodeUnits.Create(False);
end;

constructor TCodeUnit.CreateWithCaption(const ACaption: String);
begin
Create;
FCaptionWithExt:=ACaption;
FCaptionWithoutExt:=ACaption;
end;

destructor TCodeUnit.Destroy;
var ud: TUnitDependence;
begin
if Assigned(FClasses) then FreeAndNIL(FClasses);
if Assigned(FInterfaces) then FreeAndNIL(FInterfaces);
if Assigned(FMetaclasses) then FreeAndNIL(FMetaclasses);
if Assigned(FSingletons) then FreeAndNIL(FSingletons);

for ud:=Low(TUnitDependence) to High(TUnitDependence) do
  FUnitLinks[ud].Free;

inherited Destroy;
end;

procedure TCodeClassList.SortByName(ARecursive: Boolean);
var I: Integer;
begin
inherited SortByName;

if ARecursive then
  for i:=0 to Count-1 do
    Items[i].Children.SortByName(True);
end;

class function TCodeClass.TypeName: String;
begin
Result:='Class'
end;

{ TCodeUnits }

function TCodeUnits.GetItems(Index: Integer): TCodeUnit;
begin
Result:=TCodeUnit(inherited Items[Index])
end;

{ TCodeObject }

constructor TCodeObject.Create;
begin
inherited Create;
end;

destructor TCodeObject.Destroy;
begin
if Assigned(FParseInfo) then FreeAndNIL(FParseInfo);
inherited Destroy;
end;

class function TCodeObject.ObjectType: TCodeObjectType;
begin
Result:=coUnknown
end;

{ TCodeObjectList }

function TCodeObjectList.GetItems(Index: Integer): TCodeObject;
begin
Result:=TCodeObject(inherited Items[Index])
end;

function TBaseCodeUnit.Caption(AWithExt: Boolean): String;
begin
if AWithExt then
  Result:=FCaptionWithExt
else
  Result:=FCaptionWithoutExt;
end;

{ TBaseCodeUnit }

class function TBaseCodeUnit.FileNameToUnitID(const AFileName: String): String;
begin
Result:=AnsiUpperCase(ExtractFileName(ChangeFileExt(AFileName, '')));
end;

class function TBaseCodeUnit.ObjectType: TCodeObjectType;
begin
Result:=coUnit
end;

procedure TBaseCodeUnit.SetFileName(const Value: String);
begin
FFileName:=Value;
FCaptionWithExt:=ExtractFileName(FFileName);
FCaptionWithoutExt:=ChangeFileExt(FCaptionWithExt, '');
FID:=FileNameToUnitID(Value)
end;

procedure TBaseCodeUnitList.AddUnit(AUnit: TBaseCodeUnit);
begin
AddObject(AUnit.ID, AUnit)
end;

function TBaseCodeUnitList.ByUnitID(const AUnitID: String): TBaseCodeUnit;
var dummy: Integer;
begin
Result:=ByUnitID(AUnitID, dummy);
end;

function TBaseCodeUnitList.ByUnitID(const AUnitID: String; out AIndex: Integer): TBaseCodeUnit;
begin
AIndex:=IndexOf(AUnitID);
if AIndex<>-1 then
  Result:=Items[AIndex]
else
  Result:=NIL;
end;

{ TBaseCodeUnitList }

function TBaseCodeUnitList.GetItems(Index: Integer): TBaseCodeUnit;
begin
Result:=TBaseCodeUnit(Objects[Index])
end;

procedure TBaseCodeUnitList.RemoveUnit(const AUnitID: String);
var Idx: Integer; unit_: TBaseCodeUnit;
begin
unit_:=ByUnitID(AUnitID, Idx);
if Assigned(unit_) then
  begin
  Delete(Idx);
  if OwnsItems then FreeAndNIL(unit_);
  end;
end;

{ TFrameWork }

constructor TFrameWork.Create;
begin
inherited Create;
FUnits:=TBaseCodeUnitList.Create(True);

UpdateUnits;
end;

destructor TFrameWork.Destroy;
begin
if Assigned(FUnits) then FreeAndNIL(FUnits);
inherited Destroy;
end;

procedure TFrameWork.LoadFileNames(AFileNames: TStrings);
begin
AFileNames.LoadFromFile(FN_FrameworkUnits);
end;

procedure TFrameWork.SaveFileNames(AFileNames: TStrings);
begin
AFileNames.SaveToFile(FN_FrameworkUnits);
end;

procedure TFrameWork.UpdateUnits;
var F: TextFile; S: String; unit_: TBaseCodeUnit;
begin
FUnits.Clear;

if FileExists(FN_FrameworkUnits) then
	begin
	AssignFile(F, FN_FrameworkUnits);
	try
	  Reset(F);
    while not EOF(F) do
      begin
      Readln(F, S);

	    unit_:=TBaseCodeUnit.Create;
      unit_.FileName:=S;
      unit_.UnitSource:=usFramework;
      FUnits.AddUnit(unit_);
      end;
	finally
	  CloseFile(F);
	end; // try
	end;
end;

{ TCodeInterface }

constructor TCodeInterface.Create(AUnit: TCodeUnit; const AName: String;
    AInterfaceLine: Integer);
begin
inherited Create(AUnit, AName, AInterfaceLine);
FImplementedIn:=TCodeClassList.Create(False);
end;

{ TCodeInterface }

destructor TCodeInterface.Destroy;
begin
if Assigned(FImplementedIn) then FreeAndNIL(FImplementedIn);
inherited Destroy;
end;

class function TCodeInterface.ObjectType: TCodeObjectType;
begin
Result:=coInterface
end;

function TCodeInterfaceList.GetItems(Index: Integer): TCodeInterface;
begin
Result:=TCodeInterface(inherited Items[Index])
end;

class function TCodeInterface.TypeName: String;
begin
Result:='Interface'
end;

class function TCodeMetaclass.ObjectType: TCodeObjectType;
begin
Result:=coMetaclass
end;

function TCodeMetaclassList.GetItems(Index: Integer): TCodeMetaclass;
begin
Result:=TCodeMetaclass(inherited Items[Index])
end;

class function TCodeSingleton.ObjectType: TCodeObjectType;
begin
Result:=coSingleton
end;

function TCodeSingletonList.GetItems(Index: Integer): TCodeSingleton;
begin
Result:=TCodeSingleton(inherited Items[Index])
end;

{ TUnitElement }

constructor TUnitElement.Create(AUnit: TCodeUnit; const AName: String;
    AInterfaceLine: Integer);
begin
inherited Create;
FUnit:=AUnit;
FName:=AName;
FInterfaceLine:=AInterfaceLine;
end;

function TUnitElementList.ByName(const AUnitElementName: String; out AIndex:
    Integer): TUnitElement;
var I: Integer; Tmp: TUnitElement; elementName: String;
begin
Result:=NIL;
AIndex:=-1;
elementName:=AnsiUpperCase(AUnitElementName);
for i:=0 to Count-1 do
  begin
  Tmp:=Items[i];
  if AnsiUpperCase(Tmp.Name)=elementName then
    begin
    Result:=Tmp;
    AIndex:=i;
    break;
    end;
  end;
end;

{ TUnitElementList }

function TUnitElementList.ByName(const AUnitElementName: String): TUnitElement;
var dummy: Integer;
begin
Result:=ByName(AUnitElementName, dummy)
end;

function TUnitElementList.GetItems(Index: Integer): TUnitElement;
begin
Result:=TUnitElement(inherited Items[Index])
end;

function SortByUnitElementName(Item1, Item2: Pointer): Integer;
begin
Result:=AnsiCompareText(TUnitElement(Item1).Name, TUnitElement(Item2).Name);
end;

procedure TUnitElementList.SortByName;
begin
Sort(SortByUnitElementName);
end;

constructor TUnitElementIndex.Create;
begin
inherited Create;
Sorted:=True;
Duplicates:=dupAccept;
end;

{ TUnitElementIndex }

procedure TUnitElementIndex.AddElement(AUnitElement: TUnitElement);
var unitElement: TUnitElement;
begin
unitElement:=ByName(AUnitElement.Name);
if Assigned(unitElement) then
  raise Exception.CreateFmt('Duplicate UnitElement name "%s" (units: "%s", "%s")',
    [unitElement.Name, unitElement.Unit_.Caption(True), AUnitElement.Unit_.Caption(True)]);

AddObject(AnsiUpperCase(AUnitElement.Name), AUnitElement);
end;

function TUnitElementIndex.ByName(const AUnitElementName: String; out AIndex:
    Integer): TUnitElement;
begin
AIndex:=IndexOf(AnsiUpperCase(AUnitElementName));
if AIndex<>-1 then
  Result:=UnitElements[AIndex]
else
  Result:=NIL;
end;

function TUnitElementIndex.ByName(const AUnitElementName: String): TUnitElement;
var dummy: Integer;
begin
Result:=ByName(AUnitElementName, dummy)
end;

procedure TUnitElementIndex.RemoveElement(AUnitElement: TUnitElement);
var Idx: Integer;
begin
ByName(AUnitElement.Name, Idx);
if Idx<>-1 then Delete(Idx);
end;

procedure TUnitElementIndex.AddElementList(AUnitElementList: TUnitElementList);
var I: Integer;
begin
for i:=0 to AUnitElementList.Count-1 do
  AddElement(AUnitElementList[i]);
end;

procedure TUnitElementIndex.RemoveElementList(AUnitElementList: TUnitElementList);
var I: Integer;
begin
for i:=0 to AUnitElementList.Count-1 do
  RemoveElement(AUnitElementList[i]);
end;

class function TCodeMetaclass.TypeName: String;
begin
Result:='Metaclass'
end;

class function TCodeSingleton.TypeName: String;
begin
Result:='Singleton'
end;

function TUnitElementIndex.GetUnitElements(Index: Integer): TUnitElement;
begin
Result:=TUnitElement(Objects[Index])
end;

function TCodeUnit.GetUnitLinks(Index: TUnitDependence): TCodeUnits;
begin
Result:=FUnitLinks[Index]
end;

function UnitSection2Uses(AUnitSection: TUnitSection): TUnitDependence;
begin
case AUnitSection of
usItf: Result:=udUsesItf;
usImpl: Result:=udUsesImpl;
end; // case
end;

function UnitSection2UsedBy(AUnitSection: TUnitSection): TUnitDependence;
begin
case AUnitSection of
usItf: Result:=udUsedByItf;
usImpl: Result:=udUsedByImpl;
end; // case
end;

{ TUnitLayer }

constructor TUnitLayer.Create(AID: Integer; const ACaption: String; AColor: TColor);
begin
inherited Create;
ID:=AID;
Caption:=ACaption;
FColor:=AColor;
end;

{ TApplicationLayers }

function TApplicationLayers.ByID(ID: Integer): TUnitLayer;
begin
Result:=TUnitLayer(inherited ByID(ID));

if Result=NIL then
  Result:=Items[0];
end;

function TApplicationLayers.ByID(ID: Integer; var Idx: Integer): TUnitLayer;
begin
Result:=TUnitLayer(inherited ByID(ID, Idx));

if Result=NIL then
  begin
  Result:=Items[0];
  Idx:=0;
  end;
end;

function TApplicationLayers.ByName(const ALayerName: String): TUnitLayer;
var I: Integer; tmp: TUnitLayer;
begin
Result:=Items[0];

for i:=0 to Count-1 do { TODO : можно оптимизировать скорость с помощью индекса }
  begin
  tmp:=Items[i];
  if tmp.Caption=ALayerName then
    begin
    Result:=tmp;
    break;
    end;
  end;
end;

constructor TApplicationLayers.Create;
begin
inherited Create(True);
UpdateLayers;
end;

function TApplicationLayers.GetItems(Index: Integer): TUnitLayer;
begin
Result:=TUnitLayer(inherited Items[Index])
end;

procedure TApplicationLayers.UpdateLayers;
var doc: IXMLDoc; Node, Node1, Node2, Node3: IXMLDOMNode; I, J: Integer;
  _ID, _UsesStatus: Integer; _Name, _Color: String; layer: TUnitLayer; us: TUnitSection;
begin
Clear;

xmlLoad(FN_ApplicationLayers, doc);

Node:=xmlGetNode(Doc.documentElement, 'Names', True);
for i:=0 to Node.childNodes.length-1 do
  begin
  Node1:=Node.childNodes.item[i];
  _ID:=xmlGetAttribute(Node1, 'ID');
  _Name:=xmlGetAttribute(Node1, 'Name');
  _Color:=xmlGetAttribute(Node1, 'Color');

  layer:=TUnitLayer.Create(_ID, _Name, TColor(StrToInt('$'+_Color)));
  Add(layer);
  end;

Node:=xmlGetNode(Doc.documentElement, 'Map', True);
for i:=0 to Node.childNodes.length-1 do
  begin
  Node1:=Node.childNodes.item[i];
  _ID:=xmlGetAttribute(Node1, 'ID');
  layer:=ByID(_ID);

  for us:=Low(TUnitSection) to High(TUnitSection) do
    begin
    Node2:=Node1.childNodes.item[Integer(us)];
    for j:=0 to Node2.childNodes.length-1 do
      begin
      Node3:=Node2.childNodes.item[j];
      _ID:=xmlGetAttribute(Node3, 'ID');
      _UsesStatus:=xmlGetAttribute(Node3, 'UsesStatus');
      layer.UsesStatus[us][_ID]:=TUsesStatus(_UsesStatus);
      end;
    end;
  end;
end;

end.
