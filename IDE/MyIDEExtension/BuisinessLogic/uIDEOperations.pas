unit uIDEOperations;

interface

uses SysUtils, Windows, Controls, Forms, ToolsAPI, _Strings, _ToolsAPI, _Misc;

procedure MaximizeApp;

procedure OpenUnit(const AUnitName: String; ALineNo: Integer);

implementation

procedure OpenUnit(const AUnitName: String; ALineNo: Integer);
var module: IOTAModule;
  I: Integer; moduleServices: IOTAModuleServices; editor: IOTAEditor; editView: IOTAEditView;
  actionServices: IOTAActionServices; topPos: TOTAEditPos;
begin
moduleServices:=(BorlandIDEServices as IOTAModuleServices);
actionServices:=(BorlandIDEServices as IOTAActionServices);

module:=moduleServices.FindModule(AUnitName);
if not Assigned(module) then
  begin                                         { TODO : дублируется с OpenUnits }
  actionServices.OpenFile(AUnitName);
  module:=moduleServices.FindModule(AUnitName);
  end;

if Assigned(module) then
  begin
  editor:=NIL;
  // если сделать просто M.CurrentEditor.Show, то может показать форму, поэтому
  // ищем среди M.ModuleFileEditors редактор кода и показываем именно его
  for i:=0 to module.ModuleFileCount-1 do
    begin
    if IsPas(module.ModuleFileEditors[i].FileName) then
      begin
      editor:=module.ModuleFileEditors[i];
      break;
      end;
    end;

  if editor=NIL then editor:=module.CurrentEditor;
  editor.Show;

  editView:=(editor as IOTASourceEditor).GetEditView(0);
  editView.Position.GotoLine(ALineNo); // Move(ALineNo, 1);

  topPos.Col:=1;
  topPos.Line:=Max(ALineNo-15, 1);
  editView.TopPos:=topPos;

  editView.Paint;
  end;
end;

procedure MaximizeApp;
var F, F1, F2: TForm; I: Integer;
begin
F1:=NIL; // to avoid compiler message
F2:=NIL;
for i:=0 to Screen.FormCount-1 do
  begin
  F:=Screen.Forms[i];
  if (F.ClassName='TAppBuilder') then
    F1:=F
  else
  if (F.ClassName='TEditWindow') then
    F2:=F;
  end;
if Assigned(F1) then
	begin
	F1.WindowState:=wsNormal;
	F1.WindowState:=wsMaximized;
	end;
if Assigned(F2) then
	begin
	F2.WindowState:=wsNormal;
	F2.WindowState:=wsMaximized;
	end;
end;

end.
