unit uEditorOperations;

interface

{$I ..\..\CompilerVer.inc}

uses SysUtils, Windows, ToolsAPI, Classes, _ToolsAPI, _XML10, TLB_MSXML;

type
  TBlockRows = record
    StartRow: Integer;
    EndRow: Integer;
  end;

procedure SelectBlock(const AEditView: IOTAEditView; AStartRow, AEndRow: Integer);
function GetBlockRows(const AEditView: IOTAEditView; ASingleRowSimpleSelect: Boolean = True): TBlockRows;
procedure MarkWord(const AEditBuffer: IOTAEditBuffer);
procedure MarkWordBeforeCursor(const AEditBuffer: IOTAEditBuffer);

procedure IncBlockIndent(const AEditView: IOTAEditView);
procedure DecBlockIndent(const AEditView: IOTAEditView);
procedure CommentBlockIndent(const AEditView: IOTAEditView);

procedure CopyWord(const AEditBuffer: IOTAEditBuffer);
procedure CutWord(const AEditBuffer: IOTAEditBuffer);
procedure PasteWord(const AEditBuffer: IOTAEditBuffer);

procedure SurroundWithBeginEnd(const AEditView: IOTAEditView);
procedure SurroundWithBeginEndIndent(const AEditView: IOTAEditView);

procedure AddToDo(const AEditView: IOTAEditView; const AText: String);
procedure AddToDoWithDlg(const AEditView: IOTAEditView);

procedure SearchFromStartOfFile(const AEditBuffer: IOTAEditBuffer; AIncremental: Boolean);

procedure RunConfig(const AEditView: IOTAEditView);

implementation

uses uAddToDoDlg, uRunConfigDlg;

procedure SelectBlock(const AEditView: IOTAEditView; AStartRow, AEndRow: Integer);
begin
with AEditView do
  begin
  position.GotoLine(AStartRow);
  position.MoveBOL;
  Block.BeginBlock;

  position.GotoLine(AEndRow);
  position.MoveEOL;
  Block.EndBlock;
  end;
end;

function GetBlockRows(const AEditView: IOTAEditView; ASingleRowSimpleSelect: Boolean = True): TBlockRows;
begin
with AEditView do
  begin
  if Block.Text='' then
    begin
    Result.StartRow:=position.Row;
    Result.EndRow:=position.Row;
    if not ASingleRowSimpleSelect then SelectBlock(AEditView, Result.StartRow, Result.EndRow)
    end
  else
    begin
    Result.StartRow:=Block.StartingRow;
    Result.EndRow:=Block.EndingRow;
    if position.Column=1 then Dec(Result.EndRow);
    end;
  end;
end;

procedure MarkWordBeforeCursor(const AEditBuffer: IOTAEditBuffer);
var EP: IOTAEditPosition; EB: IOTAEditBlock; B: Boolean;
begin
EP:=AEditBuffer.EditPosition;
EB:=AEditBuffer.EditBlock;

if EB.Text<>'' then exit;
if EP.Column=1 then exit;

if not EP.IsWordCharacter then
  begin
  EP.MoveRelative(0, -1);
  B:=EP.IsWordCharacter;
  EP.MoveRelative(0, 1);
  if not B then exit;
  end;

EP.MoveCursor(mmSkipLeft or mmSkipWord);

EB.Reset;
EB.Style:=btNonInclusive;
EB.BeginBlock;
EP.MoveCursor(mmSkipRight or mmSkipWord);
EB.EndBlock;
end;

procedure MarkWord(const AEditBuffer: IOTAEditBuffer);
var EP: IOTAEditPosition; EB: IOTAEditBlock;
begin
EP:=AEditBuffer.EditPosition;
EB:=AEditBuffer.EditBlock;
if EP.IsWordCharacter then EP.MoveCursor(mmSkipLeft or mmSkipWord);
if EP.IsWhiteSpace then EP.MoveCursor(mmSkipRight or mmSkipWhite);
if not EP.IsWhiteSpace then
  begin
  if not EP.IsWordCharacter then EP.MoveCursor(mmSkipRight or mmSkipNonWord);
  if EP.IsWordCharacter then
    begin
    EB.Reset;
    EB.Style:=btNonInclusive;
    EB.BeginBlock;
    EP.MoveCursor(mmSkipRight or mmSkipWord);
    EB.EndBlock;
    end;
  end;
end;

procedure IncBlockIndent(const AEditView: IOTAEditView);
var i: Integer; BR: TBlockRows;
begin
with AEditView do
  begin
  BR:=GetBlockRows(AEditView);

  for i:=BR.StartRow to BR.EndRow do
    begin
    position.GotoLine(i);
    position.InsertText('  '); { TODO : этот класс и TIncIndent очень похожи }
    end;

  if BR.StartRow<>BR.EndRow then SelectBlock(AEditView, BR.StartRow, BR.EndRow);
  Paint;
  end;
end;

procedure CommentBlockIndent(const AEditView: IOTAEditView);
var i: Integer; BR: TBlockRows;
begin
with AEditView do
  begin
  BR:=GetBlockRows(AEditView);

  for i:=BR.StartRow to BR.EndRow do
    begin
    position.GotoLine(i);
    position.InsertText('//'); { TODO : этот класс и TIncIndent очень похожи }
    end;

  if BR.StartRow<>BR.EndRow then SelectBlock(AEditView, BR.StartRow, BR.EndRow);
  Paint;
  end;
end;

procedure DecBlockIndent(const AEditView: IOTAEditView);
var BR: TBlockRows; SaveStyle: TOTABlockType;
begin
with AEditView do
  begin
  BR:=GetBlockRows(AEditView);

  SaveStyle:=Block.Style;
  Block.Style:=btColumn;
  position.Move(BR.StartRow, 1); Block.BeginBlock;
  position.Move(BR.EndRow, 2); Block.EndBlock;
  Block.Delete;
  Block.Style:=SaveStyle;

  if BR.StartRow<>BR.EndRow then SelectBlock(AEditView, BR.StartRow, BR.EndRow);
  Paint;
  end;
end;

procedure CopyWord(const AEditBuffer: IOTAEditBuffer);
begin
if AEditBuffer.EditBlock.Size=0 then MarkWord(AEditBuffer);
AEditBuffer.EditBlock.Copy(False);
end;

procedure CutWord(const AEditBuffer: IOTAEditBuffer);
begin
if AEditBuffer.EditBlock.Size=0 then MarkWord(AEditBuffer);
AEditBuffer.EditBlock.Cut(False);
end;

procedure PasteWord(const AEditBuffer: IOTAEditBuffer);
begin
if ((GetKeyState(VK_SCROLL) and 1)<>0) and (AEditBuffer.EditBlock.Size=0) then MarkWord(AEditBuffer);
AEditBuffer.EditPosition.Paste;
end;

procedure SurroundWithBeginEnd(const AEditView: IOTAEditView);
var sIndent, sSelectedText: String; I: Integer; BR: TBlockRows;
begin
with AEditView do
  begin
  BR:=GetBlockRows(AEditView, False);
  if Block.Text<>'' then SelectBlock(AEditView, BR.StartRow, BR.StartRow);

  sIndent:='';
  sSelectedText:=Block.Text;
  if sSelectedText<>'' then
    begin
    I:=1;
    {$IFDEF DELPHI_XE}
    while CharInSet(sSelectedText[i], [' ', #9]) do begin sIndent:=sIndent+sSelectedText[i]; Inc(I); end;
    {$ELSE}
    while sSelectedText[i] in [' ', #9] do begin sIndent:=sIndent+sSelectedText[i]; Inc(I); end;
    {$ENDIF}
    end;

  position.GotoLine(BR.StartRow); Position.InsertText(#13#10);
  position.GotoLine(BR.StartRow); Position.InsertText(sIndent+'begin');

  position.GotoLine(BR.EndRow+1); Position.MoveEOL; Position.InsertText(#13#10);
  position.GotoLine(BR.EndRow+2); Position.InsertText(sIndent+'end;');

  Paint;
  end;
end;

procedure SurroundWithBeginEndIndent(const AEditView: IOTAEditView);
var I: Integer; BR: TBlockRows;
begin
BR:=GetBlockRows(AEditView, False);

SurroundWithBeginEnd(AEditView);

with AEditView do
  begin
  for i:=BR.StartRow to BR.EndRow+2 do
    begin
    position.GotoLine(i);
    position.MoveBOL;
    Position.InsertText(#9);
    end;
  Paint;
  end;
end;

procedure AddToDo(const AEditView: IOTAEditView; const AText: String);
begin
with AEditView do
  begin
  Position.InsertText(Format('{ TODO : %s }', [AText]));
  Paint;
  end;
end;

procedure AddToDoWithDlg(const AEditView: IOTAEditView);
var Text: String;
begin
if AskToDoText(Text) then
  AddToDo(AEditView, Text);
end;

procedure SearchFromStartOfFile(const AEditBuffer: IOTAEditBuffer; AIncremental: Boolean);
var EditView: IOTAEditView; SourceEditor: IOTASourceEditor; EditActions: IOTAEditActions;
  EditPosition: IOTAEditPosition;
begin
SourceEditor:=IOTASourceEditor(AEditBuffer);
EditView:=SourceEditor.GetEditView(0);
EditPosition:=EditView.Position;
EditPosition.Move(1, 1);
if AIncremental then
	begin
  if Supports(EditView, IOTAEditActions, EditActions) then
    EditActions.IncrementalSearch
	end
else
  begin
  EditPosition.Search;
	end;
end;

procedure ReadRunConfigs(const AFileName: String; AConfigNames, AConfigParams: TStrings);
var doc: IXMLDoc; I: Integer; node: IXMLDOMNode; _configName, _configParams: String;
begin
if not FileExists(AFileName) then exit;

xmlLoad(AFileName, doc);
for i:=0 to doc.documentElement.childNodes.length-1 do
  begin
  node:=doc.documentElement.childNodes.item[i];
  _configName:=xmlGetAttribute(node, 'Caption');
  try
    _configParams:=xmlGetAttribute(node, 'Params');
  except
    _configParams:=''
  end; // try
  AConfigNames.Add(_configName);
  AConfigParams.Add(_configParams);
  end;
end;

procedure RunConfig(const AEditView: IOTAEditView);
var
  dlg: TdlgRunConfig;
  project: IOTAProject;
  options: IOTAProjectOptions;
  params: String;
  editActions: IOTAEditActions;
  lstConfigNames, lstConfigParams: TStringList;
begin
if not Supports(AEditView, IOTAEditActions, editActions) then exit;

project:=taGetCurrentProject as IOTAProject;
options:=project.ProjectOptions;

lstConfigNames:=TStringList.Create;
lstConfigParams:=TStringList.Create;
try
  ReadRunConfigs(ChangeFileExt(project.FileName, '.RunConfigs.xml'), lstConfigNames, lstConfigParams);

  params:=options.Values['RunParams'];

  dlg:=TdlgRunConfig.Create(NIL);
  try
    if dlg.SelectConfig(lstConfigNames, lstConfigParams, params) then
      begin
      options.Values['RunParams']:=params;
      editActions.RunProgram
      end;
  finally
    FreeAndNIL(dlg);
  end; // try
finally
  FreeAndNIL(lstConfigNames);
  FreeAndNIL(lstConfigParams);
end; // try
end;

end.
