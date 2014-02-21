unit uClasses;

interface

{$I ..\..\CompilerVer.inc}

uses SysUtils, Windows, Classes, Controls, uCodeEntities, uIDEOperations, ToolsAPI, Menus,
  _ToolsAPI;

procedure GotoUnitElement(AUnitElement: TUnitElement);

procedure GotoCodeObjectWithDlg;

procedure ShowTokenContextMenu(const AEditBuffer: IOTAEditBuffer);

procedure InsertTypeWithDlg(const AEditBuffer: IOTAEditBuffer);

procedure GotoParentClass(const AEditBuffer: IOTAEditBuffer);
procedure GotoDescendantClass(const AEditBuffer: IOTAEditBuffer);

implementation

uses uEditorOperations, uTokenMenu,
  uCOD_Model_Base, uCOD_Controller_GotoGodeObject,
  uCOD_Model_Base_SingleResult, uCOD_Controller_InsertType, uDOMAIN,
  uCOD_ModelBuilder, uCodeBase;

procedure GotoUnitElement(AUnitElement: TUnitElement);
begin
OpenUnit(AUnitElement.Unit_.FileName, AUnitElement.InterfaceLine);
end;

procedure GotoCodeObjectWithDlg;
var dlgParams: m_COD_DlgParams; controller: c_COD_Controller_GotoGodeObject;
begin
ModelBuilder
  .BeginModel(dlgParams, False)
    .Caption('Goto code object')
    .AddProjectUnits
    .AddProjectClasses
    .AddProjectInterfaces
  .EndModel;

controller:=c_COD_Controller_GotoGodeObject.Create(dlgParams);
controller.TypeFilter:=[coUnit, coClass, coInterface];
try
  if controller.AskCodeObjects then
    begin
    if controller.DlgResult.CodeObject.ObjectType=coUnit then
      OpenUnit(TBaseCodeUnit(controller.DlgResult.CodeObject).FileName, 1)
    else
      GotoUnitElement(TUnitElement(controller.DlgResult.CodeObject));
    end;
finally
  FreeAndNIL(controller);
end; // try
end;

procedure InsertTypeWithDlg(const AEditBuffer: IOTAEditBuffer);
var dlgParams: m_COD_DlgParams; controller: c_COD_Controller_InsertType;
begin
ModelBuilder
  .BeginModel(dlgParams, False)
    .Caption('Insert type declaration')
    .AddProjectClasses
    .AddProjectInterfaces
    .AddProjectMetaclasses
  .EndModel;

MarkWordBeforeCursor(AEditBuffer);
dlgParams.InitialText:=AEditBuffer.EditBlock.Text;

controller:=c_COD_Controller_InsertType.Create(dlgParams);
controller.TypeFilter:=[coClass, coInterface, coMetaclass];
try
  if controller.AskCodeObjects then
    AEditBuffer.EditPosition.InsertText(TUnitElement(controller.DlgResult.CodeObject).Name);
finally
  FreeAndNIL(controller);
end; // try
end;

function BuildTokenMenu(const AToken: String): TPopupMenu;
var codeBase: TCodeBase; class_: TCodeClass; unitElement: TUnitElement; interface_: TCodeInterface;
begin
Result:=NIL;
codeBase:=DOMAIN.CodeBase(True);

class_:=TCodeClass(codeBase.ClassesIndex.ByName(AToken));

if Assigned(class_) and Assigned(class_.Parent) then
  Result:=TClassMenu.Create(class_);

if not Assigned(Result) then
  begin
  unitElement:=codeBase.SingletonsIndex.ByName(AToken);
  if Assigned(unitElement) then
    Result:=TUnitElementMenu.Create(unitElement, True);
  end;

if not Assigned(Result) then
  begin
  unitElement:=codeBase.MetaclassesIndex.ByName(AToken);
  if Assigned(unitElement) then
    Result:=TUnitElementMenu.Create(unitElement, True);
  end;

if not Assigned(Result) then
  begin
  interface_:=TCodeInterface(codeBase.InterfacesIndex.ByName(AToken));
  if Assigned(interface_) then
    Result:=TInterfaceMenu.Create(interface_);
  end;
end;

function BuildClassInheritanceMenu(const AToken: String): TPopupMenu;
var codeBase: TCodeBase; class_: TCodeClass; 
begin
Result:=NIL;
codeBase:=DOMAIN.CodeBase(True);

class_:=TCodeClass(codeBase.ClassesIndex.ByName(AToken));

if Assigned(class_) and Assigned(class_.Parent) then
  Result:=TClassMenu.Create(class_);
end;

procedure GotoParentClass(const AEditBuffer: IOTAEditBuffer);
var selection: IOTAEditBlock; token: String; editView: IOTAEditView;
  codeBase: TCodeBase; class_: TCodeClass;
begin
editView:=AEditBuffer.TopView;
selection:=AEditBuffer.EditBlock;

MarkWord(AEditBuffer);
editView.Paint;

token:=selection.Text;

codeBase:=DOMAIN.CodeBase(True);

class_:=TCodeClass(codeBase.ClassesIndex.ByName(token));

if Assigned(class_) and Assigned(class_.Parent) then
  GotoUnitElement(class_.Parent);
end;

procedure GotoDescendantClass(const AEditBuffer: IOTAEditBuffer);
var selection: IOTAEditBlock; token: String; editView: IOTAEditView;
  codeBase: TCodeBase; class_: TCodeClass; mnu: TUnitElementMenu;
  I: Integer; pt: TPoint;
begin
editView:=AEditBuffer.TopView;
selection:=AEditBuffer.EditBlock;

MarkWord(AEditBuffer);
editView.Paint;

token:=selection.Text;

codeBase:=DOMAIN.CodeBase(True);

class_:=TCodeClass(codeBase.ClassesIndex.ByName(token));

if Assigned(class_) and Assigned(class_.Parent) then
  begin
  if class_.Children.Count=1 then
    GotoUnitElement(class_.Children[0])
  else
  if class_.Children.Count>1 then
    begin
    mnu:=TUnitElementMenu.Create(class_, False);
    for i:=0 to class_.Children.Count-1 do
      mnu.AddGotoMenuItem(class_.Children[i]);

    pt:=taGetPointForSelection(AEditBuffer, True);
    mnu.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure ShowTokenContextMenu(const AEditBuffer: IOTAEditBuffer);
var selection: IOTAEditBlock; token: String; mnu: TPopupMenu; editView: IOTAEditView; pt: TPoint;
begin
editView:=AEditBuffer.TopView;
selection:=AEditBuffer.EditBlock;

MarkWord(AEditBuffer);
editView.Paint;

token:=selection.Text;

mnu:=BuildTokenMenu(token);
if Assigned(mnu) then
  begin
  pt:=taGetPointForSelection(AEditBuffer, True);
  mnu.Popup(pt.X, pt.Y);
  end;
end;

end.
