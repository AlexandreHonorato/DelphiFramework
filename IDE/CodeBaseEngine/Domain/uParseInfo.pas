unit uParseInfo;

interface

uses
  uCodeEntities, Classes, SysUtils, uCodeBase;

type TParseInfo = class
  private
    FCodeObject: TCodeObject;
  public
    procedure DefineLinks; virtual;
    procedure ResetLinks; virtual;
    property CodeObject: TCodeObject read FCodeObject write FCodeObject;
  end;

type TParseInfo_Interface = class(TParseInfo)
  public
    procedure ResetLinks; override;
  end;

type TParseInfo_Unit = class(TParseInfo)
  private
    FCodeBase: TCodeBase;
    FUses: array[TUnitSection] of TStrings;
    FCanUse: array[TUnitSection] of TStrings;
    function GetUses(Index: TUnitSection): TStrings;
    function GetCanUse(Index: TUnitSection): TStrings;
  public
    constructor Create(ACodeBase: TCodeBase); reintroduce;
    destructor Destroy; override;
    procedure DefineLinks; override;
    procedure ResetLinks; override;
    property Uses_[Index: TUnitSection]: TStrings read GetUses;
    property CanUse[Index: TUnitSection]: TStrings read GetCanUse;
  end;

type TParseInfo_Class = class(TParseInfo)
  private
    FCodeBase: TCodeBase;
    FParentName: String;
  public
    ImplementedInterfaces: TStringList;
    property ParentName: String read FParentName;
    constructor Create(ACodeBase: TCodeBase; const AParentName: String); reintroduce;
    destructor Destroy; override;
    procedure DefineLinks; override;
    procedure ResetLinks; override;
  end;

type TParseInfo_Metaclass = class(TParseInfo)
  private
    FCodeBase: TCodeBase;
    FMetaclassFor: String;
  public
    property MetaclassFor: String read FMetaclassFor;
    constructor Create(ACodeBase: TCodeBase; const AMetaclassFor: String); reintroduce;
    procedure DefineLinks; override;
    procedure ResetLinks; override;
  end;

procedure SetParseInfo(ACodeObject: TCodeObject; AParseInfo: TParseInfo);
function GetParseInfo(ACodeObject: TCodeObject): TParseInfo;

implementation

{ TParseInfo }

procedure TParseInfo.DefineLinks;
begin
// do nothing
end;

procedure TParseInfo.ResetLinks;
begin
// do nothing
end;

{ TParseInfo_Interface }

procedure TParseInfo_Interface.ResetLinks;
begin
TCodeInterface(CodeObject).ImplementedIn.Clear;
end;

{ TParseInfo_Unit }

constructor TParseInfo_Unit.Create(ACodeBase: TCodeBase);
var us: TUnitSection; lst: TStringList;
begin
inherited Create;
FCodeBase:=ACodeBase;

for us:=Low(TUnitSection) to High(TUnitSection) do
  begin
  FUses[us]:=TStringList.Create;

  lst:=TStringList.Create;
  lst.Sorted:=True;
  FCanUse[us]:=lst;
  end;
end;

procedure TParseInfo_Unit.DefineLinks;
var I: Integer; thisUnit, otherUnit: TCodeUnit; unitID: String; us: TUnitSection;
  lst: TStrings;
begin
thisUnit:=TCodeUnit(CodeObject);
for i:=0 to thisUnit.Classes.Count-1 do
  GetParseInfo(thisUnit.Classes[i]).DefineLinks;

for i:=0 to thisUnit.Interfaces.Count-1 do
  GetParseInfo(thisUnit.Interfaces[i]).DefineLinks;

for i:=0 to thisUnit.Metaclasses.Count-1 do
  GetParseInfo(thisUnit.Metaclasses[i]).DefineLinks;

for us:=Low(TUnitSection) to High(TUnitSection) do
  begin
  lst:=FUses[us];
  for i:=0 to lst.Count-1 do
    begin
    unitID:=TCodeUnit.FileNameToUnitID(lst[i]);
    otherUnit:=TCodeUnit(FCodeBase.Units.ByUnitID(unitID));

    if not Assigned(otherUnit) then continue;

    thisUnit.UnitLinks[UnitSection2Uses(us)].AddUnit(otherUnit);
    otherUnit.UnitLinks[UnitSection2UsedBy(us)].AddUnit(thisUnit);
    end;
  end;
end;

destructor TParseInfo_Unit.Destroy;
var us: TUnitSection;
begin
for us:=Low(TUnitSection) to High(TUnitSection) do
  begin
  FUses[us].Free;
  FCanUse[us].Free;
  end;
inherited Destroy;
end;

function TParseInfo_Unit.GetCanUse(Index: TUnitSection): TStrings;
begin
Result:=FCanUse[Index]
end;

function TParseInfo_Unit.GetUses(Index: TUnitSection): TStrings;
begin
Result:=FUses[Index]
end;

procedure TParseInfo_Unit.ResetLinks;
var I: Integer; unit_: TCodeUnit; ud: TUnitDependence;
begin
unit_:=TCodeUnit(CodeObject);

for i:=0 to unit_.Classes.Count-1 do
  GetParseInfo(unit_.Classes[i]).ResetLinks;

for i:=0 to unit_.Interfaces.Count-1 do
  GetParseInfo(unit_.Interfaces[i]).ResetLinks;

for i:=0 to unit_.Metaclasses.Count-1 do
  GetParseInfo(unit_.Metaclasses[i]).ResetLinks;

for ud:=Low(TUnitDependence) to High(TUnitDependence) do
  unit_.UnitLinks[ud].Clear;
end;

{ TParseInfo_Class }

constructor TParseInfo_Class.Create(ACodeBase: TCodeBase; const AParentName: String);
begin
inherited Create;
FParentName:=AParentName;
FCodeBase:=ACodeBase;
ImplementedInterfaces:=TStringList.Create;
end;

procedure TParseInfo_Class.DefineLinks;
var parentClass: TCodeClass; I: Integer; interface_: TCodeInterface; class_: TCodeClass;
begin
class_:=TCodeClass(CodeObject);
parentClass:=FCodeBase.FindOrCreateClass(ParentName);

class_.Parent:=parentClass;
parentClass.Children.Add(class_);

for i:=0 to ImplementedInterfaces.Count-1 do
  begin
  interface_:=FCodeBase.FindOrCreateInterface(ImplementedInterfaces[i]);
  class_.ImplementedInterfaces.Add(interface_);
  interface_.ImplementedIn.Add(class_);
  end;
end;

destructor TParseInfo_Class.Destroy;
begin
if Assigned(ImplementedInterfaces) then FreeAndNIL(ImplementedInterfaces);
inherited Destroy; 
end;

procedure TParseInfo_Class.ResetLinks;
var class_: TCodeClass;
begin
class_:=TCodeClass(CodeObject);
class_.Parent:=NIL;
class_.Children.Clear;
class_.ImplementedInterfaces.Clear;
end;

procedure SetParseInfo(ACodeObject: TCodeObject; AParseInfo: TParseInfo);
begin
ACodeObject.ParseInfo:=AParseInfo;
if Assigned(AParseInfo) then AParseInfo.CodeObject:=ACodeObject;
end;

function GetParseInfo(ACodeObject: TCodeObject): TParseInfo;
begin
Result:=TParseInfo(ACodeObject.ParseInfo)
end;

{ TParseInfo_Metaclass }

constructor TParseInfo_Metaclass.Create(ACodeBase: TCodeBase;
  const AMetaclassFor: String);
begin
inherited Create;
FCodeBase:=ACodeBase;
FMetaclassFor:=AMetaclassFor;
end;

procedure TParseInfo_Metaclass.DefineLinks;
begin
TCodeMetaclass(CodeObject).Class_:=FCodeBase.FindOrCreateClass(FMetaclassFor);
end;

procedure TParseInfo_Metaclass.ResetLinks;
begin
TCodeMetaclass(CodeObject).Class_:=NIL
end;

end.
