unit _ToolsAPI;

interface

{$I ..\..\CompilerVer.inc}

uses SysUtils, Windows, Classes, Controls, ToolsAPI, Menus, DesignEditors, DesignIntf, _Strings;

function taGetCurrentProject: IOTAModule;
function taGetCurrentProjectGroup: IOTAProjectGroup;
procedure taOTAEditReaderToStream(EditReader: IOTAEditReader; Stream: TStream);
function taGetCurrentDesigner: IDesigner;
function taGetCurrentModule: IOTAModule;
function taCurrentEditView: IOTAEditView;

type TToolsAPIMenu = class
  protected
    FMenuItem: TMenuItem;
    procedure InitMenu; virtual; abstract;
    procedure DoneMenu; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function taGetPointForSelection(const AEditBuffer: IOTAEditBuffer; AReturnStartPoint: Boolean): TPoint;

implementation

function taGetPointForSelection(const AEditBuffer: IOTAEditBuffer; AReturnStartPoint: Boolean): TPoint;
{$IFDEF DELPHI_XE}
const
  LeftOffset10 = 58;
  LeftOffset100 = LeftOffset10 + 9;
  LeftOffset1000 = LeftOffset100 + 9;
  SymbolWidth = 9;
  RowHeight = 19;

  function GetLeftOffset(const AEditView: IOTAEditView): Integer;
  var rowCount: Integer;
  begin
  rowCount:=AEditView.Position.LastRow;
  if rowCount<100 then
    Result:=LeftOffset10
  else
  if rowCount<1000 then
    Result:=LeftOffset100
  else
    Result:=LeftOffset1000
  end;
{$ELSE}
const
  LeftOffset = 29;
  SymbolWidth = 9;
  RowHeight = 19;

  function GetLeftOffset(const AEditView: IOTAEditView): Integer;
  begin
  Result:=LeftOffset;
  end;
{$ENDIF}

var col, row: Integer; selection: IOTAEditBlock; editView: IOTAEditView; ctrl: TControl;
begin
editView:=AEditBuffer.TopView;
selection:=AEditBuffer.EditBlock;

row:=editView.Position.Row-editView.TopRow+1;
if AReturnStartPoint then
  col:=selection.StartingColumn-1
else
  col:=selection.EndingColumn-1;

ctrl:=TControl(editView.GetEditWindow.Form.FindComponent('Editor'));

Result.X:=GetLeftOffset(editView)+col*SymbolWidth;
Result.Y:=row*RowHeight;
Result:=ctrl.ClientToScreen(Result);
end;

function taCurrentEditView: IOTAEditView;
begin
Result:=(BorlandIDEServices as IOTAEditorServices).TopView;
end;

procedure taOTAEditReaderToStream(EditReader: IOTAEditReader; Stream: TStream);
const
  TerminatingNullChar: AnsiChar = #0;
  BufferSize = 1024 * 24;
var
  EditReaderPos: Integer;
  ReadDataSize: Integer;
  Buffer: array[0..BufferSize] of AnsiChar;
begin
  Assert(EditReader <> nil);
  Assert(Stream <> nil);

  EditReaderPos := 0;
  ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BufferSize);
  Inc(EditReaderPos, ReadDataSize);
  while ReadDataSize = BufferSize do
  begin
    Stream.Write(Buffer, ReadDataSize);
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BufferSize);
    Inc(EditReaderPos, ReadDataSize);
  end;
  Stream.Write(Buffer, ReadDataSize);
  Stream.Write(TerminatingNullChar, SizeOf(TerminatingNullChar));
end;

function taGetCurrentProject: IOTAModule;
var Module: IOTAModule; PG: IOTAProjectGroup; ModuleServices: IOTAModuleServices;
  I: Integer;
begin
Result:=NIL;

PG:=taGetCurrentProjectGroup;
if Assigned(PG) then
  Result:=PG.ActiveProject
else
  begin
  ModuleServices:=BorlandIDEServices as IOTAModuleServices;

  for i:=0 to ModuleServices.ModuleCount-1 do
    begin
    Module:=ModuleServices.Modules[i];
    if Supports(Module, IOTAProject, Result) then Break;
    end;
  end;
end;

function taGetCurrentProjectGroup: IOTAProjectGroup;
var Services: IOTAModuleServices; Module: IOTAModule; I: Integer;
begin
Result:=NIL;
Services:=BorlandIDEServices as IOTAModuleServices;
for i:=0 to Services.ModuleCount-1 do
  begin
  Module:=Services.Modules[i];
  if Module.QueryInterface(IOTAProjectGroup, Result)=S_OK then break;
  end;
end;

function taGetCurrentModule: IOTAModule;
begin
Result:=(BorlandIDEServices as IOTAModuleServices).CurrentModule;
end;

function taGetCurrentDesigner: IDesigner;
var I: Integer; CM: IOTAModule; FE: INTAFormEditor;
begin
Result:=NIL;

CM:=taGetCurrentModule;
if CM=NIL then exit;

FE:=NIL;
for i:=0 to CM.GetModuleFileCount-1 do
  if Supports(CM.GetModuleFileEditor(i), INTAFormEditor, FE) then
    break;

if FE=NIL then exit;

Result:=FE.FormDesigner;
end;

{ TToolsAPIMenu }

constructor TToolsAPIMenu.Create;
begin
inherited Create;
FMenuItem:=(BorlandIDEServices as INTAServices40).MainMenu.Items;
InitMenu;
end;

destructor TToolsAPIMenu.Destroy;
begin
DoneMenu;
inherited Destroy;
end;

end.