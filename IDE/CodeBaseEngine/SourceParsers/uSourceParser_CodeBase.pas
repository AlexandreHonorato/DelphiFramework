unit uSourceParser_CodeBase;

interface

uses
  classes, uCodeEntities, SysUtils, _Strings,
  uSourceParser, uCodeBase, uFileMappers;

type TSourceParser_CodeBase = class(TSourceParser)
  private
    FCodeBase: TCodeBase;

    function AreUnitsEqual(AOldUnit, ANewUnit: TCodeUnit): Boolean;
    procedure DefineLinks;
    procedure ResetLinks;
  public
    procedure FullRebuild;
    procedure ApplyChangesInUnits(AChangedUnitNames: TStringList);

    constructor Create(AFileMappers: TFileMappers;
      AGetUnitNames: TGetUnitNamesDelegate; ACodeBase: TCodeBase); reintroduce; virtual;
    property CodeBase: TCodeBase read FCodeBase;
  end;

implementation

uses uParseInfo, uStreamParser, uStreamParser_CodeUnit;

type TStreamParser_CodeUnit_FullRebuild = class(TStreamParser_CodeUnit)
  public
    procedure ApplyParseResults(AResultUnit: TCodeUnit); override;
  end;

type TStreamParser_CodeUnit_ApplyChanges = class(TStreamParser_CodeUnit)
  public
    ChangedUnits: TCodeUnits;
    procedure ApplyParseResults(AResultUnit: TCodeUnit); override;
    constructor Create(ACodeBase: TCodeBase; AChangedUnits: TCodeUnits); reintroduce;
  end;
  
{ TSourceParser_CodeBase }

constructor TSourceParser_CodeBase.Create(AFileMappers: TFileMappers;
    AGetUnitNames: TGetUnitNamesDelegate; ACodeBase: TCodeBase);
begin
inherited Create(AFileMappers, AGetUnitNames);
FCodeBase:=ACodeBase;
end;

procedure TSourceParser_CodeBase.ApplyChangesInUnits(AChangedUnitNames: TStringList);
var changedUnits: TCodeUnits; I: Integer; newUnit, oldUnit: TCodeUnit;
  unitChanged, codeBaseChanged: Boolean; streamParser: TStreamParser_CodeUnit_ApplyChanges;
begin
try
  changedUnits:=TCodeUnits.Create(True);
  try
    streamParser:=TStreamParser_CodeUnit_ApplyChanges.Create(FCodeBase, changedUnits);
    try
      ParseUnits(AChangedUnitNames, streamParser);
    finally
      FreeAndNIL(streamParser)
    end; // try

    codeBaseChanged:=False;
    for i:=changedUnits.Count-1 downto 0 do
      begin
      newUnit:=changedUnits[i];
      oldUnit:=TCodeUnit(FCodeBase.Units.ByUnitID(newUnit.ID));

      if Assigned(oldUnit) then
        unitChanged:=not AreUnitsEqual(newUnit, oldUnit)
      else
        unitChanged:=True;

      if unitChanged then
        begin
        if Assigned(oldUnit) then
          FCodeBase.RemoveUnit(oldUnit);

        FCodeBase.AddUnit(newUnit);
        changedUnits.Delete(i); // этот модуль перенесен в CodeBase, не нужно его разрушать при разрушении списка changedUnits
        codeBaseChanged:=True;
        end;
      end;

    if codeBaseChanged then
      begin
      FCodeBase.RecreateRoot;

      ResetLinks;
      DefineLinks;

      FCodeBase.NotifyChanged;
      end;
  finally
    FreeAndNIL(changedUnits);
  end; // try
except
  on E: Exception do
    HandleException(E);
end; // try
end;

function TSourceParser_CodeBase.AreUnitsEqual(AOldUnit, ANewUnit: TCodeUnit): Boolean;
var oldClasses, newClasses: TCodeClassList; I: Integer; oldClass, newClass: TCodeClass;
begin
Result:=False;

oldClasses:=AOldUnit.Classes;
newClasses:=ANewUnit.Classes;

if oldClasses.Count<>newClasses.Count then exit;

for i:=0 to oldClasses.Count-1 do
  begin
  oldClass:=oldClasses[i];
  newClass:=TCodeClass(newClasses.ByName(oldClass.Name));

  if not Assigned(newClass) then exit;
  if not TC(TParseInfo_Class(GetParseInfo(oldClass)).ParentName, TParseInfo_Class(GetParseInfo(newClass)).ParentName) then exit;
  end;

Result:=True;
end;

procedure TSourceParser_CodeBase.FullRebuild;
var streamParser: TStreamParser_CodeUnit_FullRebuild;
begin
FCodeBase.Reset;
try
  streamParser:=TStreamParser_CodeUnit_FullRebuild.Create(FCodeBase);
  try
    ParseUnits(streamParser);
  finally
    FreeAndNIL(streamParser);
  end; // try

  DefineLinks;
  FCodeBase.NotifyChanged;
except
  on E: Exception do
    HandleException(E);
end; // try
end;

procedure TSourceParser_CodeBase.DefineLinks;
var I: Integer;
begin
for i:=0 to FCodeBase.Units.Count-1 do
  GetParseInfo(FCodeBase.Units[i]).DefineLinks;

FCodeBase.Root.Classes.SortByName(True);
FCodeBase.Root.Interfaces.SortByName;
FCodeBase.Root.Metaclasses.SortByName;
FCodeBase.Root.Singletons.SortByName;

FCodeBase.ClassesIndex.RemoveElementList(FCodeBase.Root.Classes);
FCodeBase.InterfacesIndex.RemoveElementList(FCodeBase.Root.Interfaces);
end;

procedure TSourceParser_CodeBase.ResetLinks;
var I: Integer;
begin
for i:=0 to FCodeBase.Units.Count-1 do
  GetParseInfo(FCodeBase.Units[i]).ResetLinks;
end;

{ TStreamParser_CodeUnit_ApplyChanges }

procedure TStreamParser_CodeUnit_ApplyChanges.ApplyParseResults(AResultUnit: TCodeUnit);
begin
ChangedUnits.AddUnit(AResultUnit);
end;

constructor TStreamParser_CodeUnit_ApplyChanges.Create(ACodeBase: TCodeBase;
  AChangedUnits: TCodeUnits);
begin
inherited Create(ACodeBase);
ChangedUnits:=AChangedUnits;
end;

{ TStreamParser_CodeUnit_FullRebuild }

procedure TStreamParser_CodeUnit_FullRebuild.ApplyParseResults(AResultUnit: TCodeUnit);
begin
CodeBase.AddUnit(AResultUnit);
end;

end.
