unit uCOD_ModelBuilder;

interface

uses uCOD_Model_Base, uCodeEntities, ToolsAPI, _Strings, SysUtils, uCodeBase;

type ModelBuilder = class
  private
    FCodeBase: TCodeBase;
    FModel: m_COD_DlgParams;
  public
    function Caption(const ACaption: String): ModelBuilder;
    function AddOpenedUnits: ModelBuilder;
    function AddProjectUnits: ModelBuilder;
    function AddProjectClasses: ModelBuilder;
    function AddProjectInterfaces: ModelBuilder;
    function AddProjectMetaclasses: ModelBuilder;
    function AddProjectSingletons: ModelBuilder;
    function AddFrameworkUnits: ModelBuilder;
    constructor BeginModel(out AModel: m_COD_DlgParams; AOwnsCodeObjects: Boolean);
    destructor EndModel;
  end;

implementation

uses uDOMAIN;

{ ModelBuilder }

function ModelBuilder.AddOpenedUnits: ModelBuilder;
var svc: IOTAModuleServices; unitFileName: String; unit_: TBaseCodeUnit; I: Integer;
begin
try
  svc:=(BorlandIDEServices as IOTAModuleServices);
  for i:=0 to svc.ModuleCount-1 do
    begin
    unitFileName:=svc.Modules[i].FileName;
    if IsPas(unitFileName) then
      begin
      unit_:=TBaseCodeUnit.Create;
      unit_.FileName:=unitFileName;

      if Assigned(FCodeBase.Units.ByUnitID(unit_.ID)) then
        unit_.UnitSource:=usProject
      else
      if Assigned(DOMAIN.FrameWork.Units.ByUnitID(unit_.ID)) then
        unit_.UnitSource:=usFramework
      else
        unit_.UnitSource:=usUnknown;

      FModel.CodeObjects.Add(unit_);
      end;
    end;
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

function ModelBuilder.AddProjectClasses: ModelBuilder;
var I: Integer;
begin
try
  for i:=0 to FCodeBase.ClassesIndex.Count-1 do
    FModel.CodeObjects.Add(FCodeBase.ClassesIndex.UnitElements[i]);
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

function ModelBuilder.AddProjectInterfaces: ModelBuilder;
var I: Integer;
begin
try
  for i:=0 to FCodeBase.InterfacesIndex.Count-1 do
    FModel.CodeObjects.Add(FCodeBase.InterfacesIndex.UnitElements[i]);
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

function ModelBuilder.AddProjectUnits: ModelBuilder;
var I: Integer;
begin
try
  for i:=0 to FCodeBase.Units.Count-1 do
    FModel.CodeObjects.Add(FCodeBase.Units[i]);
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

constructor ModelBuilder.BeginModel(out AModel: m_COD_DlgParams;
    AOwnsCodeObjects: Boolean);
begin
FCodeBase:=DOMAIN.CodeBase(True);
FModel:=m_COD_DlgParams.Create(AOwnsCodeObjects);

AModel:=FModel;
end;

function ModelBuilder.Caption(const ACaption: String): ModelBuilder;
begin
FModel.Caption:=ACaption;
Result:=Self;
end;

destructor ModelBuilder.EndModel;
begin
// do nothing
end;

function ModelBuilder.AddProjectMetaclasses: ModelBuilder;
var I: Integer;
begin
try
  for i:=0 to FCodeBase.MetaclassesIndex.Count-1 do
    FModel.CodeObjects.Add(FCodeBase.MetaclassesIndex.UnitElements[i]);
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

function ModelBuilder.AddProjectSingletons: ModelBuilder;
var I: Integer;
begin
try
  for i:=0 to FCodeBase.SingletonsIndex.Count-1 do
    FModel.CodeObjects.Add(FCodeBase.SingletonsIndex.UnitElements[i]);
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

function ModelBuilder.AddFrameworkUnits: ModelBuilder;
var I: Integer;
begin
try
  for i:=0 to DOMAIN.FrameWork.Units.Count-1 do
    FModel.CodeObjects.Add(DOMAIN.FrameWork.Units[i]);
except
  if Assigned(FModel) then FreeAndNIL(FModel);
  raise
end; // try

Result:=Self;
end;

end.
