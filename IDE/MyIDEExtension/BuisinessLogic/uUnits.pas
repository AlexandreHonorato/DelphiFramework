unit uUnits;

interface

uses Windows, SysUtils, Classes, Forms, ToolsAPI, _Strings, mwPasParser, _ToolsAPI, uCodeEntities, uIDEOperations,
  Controls, StdCtrls, _VCL_ZZZ, uCOD_Controller_Base_MultipleResults,
  _Misc, _Files, uNotifications, _MessageBus;

procedure UseUnit(const AEditBuffer: IOTAEditBuffer; const AUnitName: String; AItfSection: Boolean);
procedure UseUnitWithDlg(const AEditBuffer: IOTAEditBuffer);
procedure UseUnitPrev(const AEditBuffer: IOTAEditBuffer);

procedure OpenUnits(AFileNames: TStrings);
procedure OpenUnitsWithDlg;

procedure CloseUnits(AUnitNames: TStrings);
procedure CloseUnitsWithDlg;

procedure RemoveUnitFromProject(const AUnitName: String);
procedure RemoveUnitFromProjectWithDlg;

procedure GotoImplementationLine(const AEditBuffer: IOTAEditBuffer);

const UNIT_IS_NOT_RECENT = -1;

type TLastUsedUnits = class(TStringList)
  private
    FLastUnit_Name: String;
    FLastUnit_IsItfSection: Boolean;
    procedure On_NEUseUnit(AEvent: NEUseUnit);
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOfUnit(const AUnitID: String): Integer;

    property LastUnit_Name: String read FLastUnit_Name write FLastUnit_Name;
    property LastUnit_IsItfSection: Boolean read FLastUnit_IsItfSection write FLastUnit_IsItfSection;
  end;

procedure EditFrameworkUnits;

implementation

uses uCOD_Model_Base, uCOD_Controller_UseUnit, uCOD_Controller_CloseUnits,
  uCOD_Controller_Base_SingleResult,
  uCOD_Controller_OpenUnits, uCOD_Controller_RemoveUnit, uDeleteUnitFilesDlg,
  uDOMAIN, uCOD_ModelBuilder, uFrameWorkUnitsDlg;

const UseUnitHistory = 5;

const
  FN_UseUnitsOrder = 'd:\andrey\delphi\__FrameWork\IDEConfig\UseUnitsOrder.dat';

procedure Uses_GetKeyPositions(AStream: TMemoryStream;
  var AItfSectionEnd, AImplSectionEnd, AItfUsesSectionEnd, AImplUsesSectionEnd: Integer);
var Parser: TmPasParser; CT, CT1: TTokenKind; IsImplementation, IsInterface: Boolean;
begin
AItfUsesSectionEnd:=0;
AImplUsesSectionEnd:=0;
AItfSectionEnd:=0;
AImplSectionEnd:=0;
Parser:=TmPasParser.Create;
try
  Parser.Origin:=AStream.Memory;

  IsImplementation:=False;
  IsInterface:=False;
  while True do
    begin
    CT:=Parser.Token.ID;
    case CT of
    tkImplementation:
      begin
      AImplSectionEnd:=Parser.Token.Position+Parser.Token.Length;
      IsImplementation:=True;
      IsInterface:=False;
      end;
    tkInterface:
      begin
      if AItfSectionEnd=0 then // запоминаем положение первого Interface, чтобы потом не путать с последующими объявлениями интерфейсов
	      begin
	      AItfSectionEnd:=Parser.Token.Position+Parser.Token.Length;
	      IsInterface:=True;
	      end;
      end;
    tkUses:
      begin
      while True do
        begin
        Parser.NextNonJunk;
        CT1:=Parser.Token.ID;
        if CT1=tkIdentifier then
          begin
//          ALst.Add(Parser.Token.Data)
          end
        else
        if CT1=tkSemiColon then
          begin
          if IsInterface then
            AItfUsesSectionEnd:=Parser.Token.Position
          else
          if IsImplementation then
            AImplUsesSectionEnd:=Parser.Token.Position;
          break;
          end;
        end;
      if IsImplementation then break;
      end;
    tkPoint:
      if Parser.PrevToken=tkEnd then break;
    end; // case

    Parser.NextNonJunk;
    end; // while
finally
  FreeAndNil(Parser);
end; // try
end;

procedure UseUnit(const AEditBuffer: IOTAEditBuffer; const AUnitName: String; AItfSection: Boolean);
var WR: IOTAEditWriter; MS: TMemoryStream; ItfPos, ImplPos, ItfUsesPos, ImplUsesPos: Integer;
  RD: IOTAEditReader; S: AnsiString; evnt: NEUseUnit;
begin
MS:=TMemoryStream.Create;
try
  RD:=IOTASourceEditor(AEditBuffer).CreateReader;
  taOTAEditReaderToStream(RD, MS);
  RD:=NIL; // IOTAEditReader should never be active at the same time as an IOTAEditWriter
  Uses_GetKeyPositions(MS, ItfPos, ImplPos, ItfUsesPos, ImplUsesPos);
finally
  MS.Free;
end; // try

WR:=IOTASourceEditor(AEditBuffer).CreateUndoableWriter;
S:=AnsiString(AUnitName); // to avoid compiler message W1057
if AItfSection then
	begin
	if ItfUsesPos<>0 then
	  begin
	  WR.CopyTo(ItfUsesPos);
    if WR.CurrentPos.CharIndex+Length(S)+3>80 then
  	  WR.Insert(PAnsiChar(','+#13#10+'  '+S))
    else
  	  WR.Insert(PAnsiChar(', '+S));
	  end
	else
	  begin
	  if ItfPos<>0 then
	    begin
	    WR.CopyTo(ItfPos);
	    WR.Insert(PAnsiChar(#13#10#13#10+'uses '+S+';'));
	    end;
	  end;
	end
else
	begin
	if ImplUsesPos<>0 then
	  begin
	  WR.CopyTo(ImplUsesPos);
    if WR.CurrentPos.CharIndex+Length(S)+3>80 then
  	  WR.Insert(PAnsiChar(','+#13#10+'  '+S))
    else
  	  WR.Insert(PAnsiChar(', '+S));
	  end
	else
	  begin
	  if ItfPos<>0 then
	    begin
	    WR.CopyTo(ImplPos);
	    WR.Insert(PAnsiChar(#13#10#13#10+'uses '+S+';'));
	    end;
	  end;
	end;

evnt:=NEUseUnit.Create;
try
  evnt.UnitID:=TBaseCodeUnit.FileNameToUnitID(AUnitName);
  evnt.UnitName:=AUnitName;
  evnt.IsItfSection:=AItfSection;

  mbExtension.Notify(evnt);
finally
  FreeAndNIL(evnt);
end; // try
end;

procedure UseUnitWithDlg(const AEditBuffer: IOTAEditBuffer);
var dlgParams: m_COD_DlgParams; controller: c_COD_Controller_UseUnit;
begin
ModelBuilder
  .BeginModel(dlgParams, False)
    .Caption('Use unit')
    .AddProjectUnits
    .AddFrameworkUnits
  .EndModel;

controller:=c_COD_Controller_UseUnit.Create(dlgParams);
try
  if controller.AskCodeObjects then
    UseUnit(AEditBuffer, controller.DlgResult.UnitName, controller.DlgResult.ItfSection);
finally
  FreeAndNIL(controller);
end; // try
end;

procedure UseUnitPrev(const AEditBuffer: IOTAEditBuffer);
begin
if DOMAIN.LastUsedUnits.LastUnit_Name<>'' then
	UseUnit(AEditBuffer, DOMAIN.LastUsedUnits.LastUnit_Name, DOMAIN.LastUsedUnits.LastUnit_IsItfSection);
end;

procedure OpenUnits(AFileNames: TStrings);
var I, J: Integer; ActionServices: IOTAActionServices; ModuleServices: IOTAModuleServices;
  S: String; M: IOTAModule; E: IOTAEditor;
begin
ActionServices:=(BorlandIDEServices as IOTAActionServices);
ModuleServices:=(BorlandIDEServices as IOTAModuleServices);
for i:=0 to AFileNames.Count-1 do
  begin
  S:=AFileNames[i];
  M:=ModuleServices.FindModule(S);
  if M=NIL then
    ActionServices.OpenFile(S)
  else
    begin
    E:=NIL;
    // если сделать просто M.CurrentEditor.Show, то может показать форму, поэтому
    // ищем среди M.ModuleFileEditors редактор кода и показываем именно его
    for j:=0 to M.ModuleFileCount-1 do
      begin
      if IsPas(M.ModuleFileEditors[j].FileName) then
        begin
        E:=M.ModuleFileEditors[j];
        break;
        end;
      end;
    if E=NIL then E:=M.CurrentEditor;
    E.Show;
    end;
  end;
end;

procedure OpenUnitsWithDlg;
var dlgParams: m_COD_DlgParams; controller: c_COD_Controller_OpenUnits;
begin
ModelBuilder
  .BeginModel(dlgParams, False)
    .Caption('Open unit(s)')
    .AddProjectUnits
    .AddFrameworkUnits
  .EndModel;

controller:=c_COD_Controller_OpenUnits.Create(dlgParams);
try
  if controller.AskCodeObjects then
    OpenUnits(controller.DlgResult.SelectedFileNames);
finally
  FreeAndNIL(controller);
end; // try
end;

procedure CloseUnits(AUnitNames: TStrings);
var I: Integer; svc: IOTAActionServices;
begin
svc:=(BorlandIDEServices as IOTAActionServices);
for i:=0 to AUnitNames.Count-1 do svc.CloseFile(AUnitNames[i])
end;

procedure CloseUnitsWithDlg;
var dlgParams: m_COD_DlgParams; controller: c_COD_Controller_CloseUnits;
begin
ModelBuilder                   
  .BeginModel(dlgParams, True)
    .Caption('Close unit(s)')
    .AddOpenedUnits
  .EndModel;

controller:=c_COD_Controller_CloseUnits.Create(dlgParams);
try
  if controller.AskCodeObjects then
    CloseUnits(controller.DlgResult.SelectedFileNames);
finally
  FreeAndNIL(controller);
end; // try
end;

procedure RemoveUnitFromProject(const AUnitName: String);
var project: IOTAProject;
begin
project:=taGetCurrentProject as IOTAProject;
project.RemoveFile(AUnitName);
end;

function ProjectHasUnit(const AUnitFileName: String): Boolean;
var project: IOTAProject; I: Integer;
begin
Result:=False;
project:=taGetCurrentProject as IOTAProject;
for i:=0 to project.GetModuleCount-1 do
  if TC(project.GetModule(i).FileName, AUnitFileName) then
    begin
    Result:=True;
    break;
    end;
end;

procedure GetUnitFileNames(const AUnitFileName: String; AFileNames: TStrings);
var status: Integer; sr: TSearchRec; path: String;
begin
path:=ExtractFilePath(AUnitFileName);
status:=FindFirst(ChangeFileExt(AUnitFileName, '.*'), faAnyFile, sr);
while status=0 do
  begin
  AFileNames.Add(path+sr.Name);
	status:=FindNext(sr);
  end;
FindClose(sr);
end;

procedure RemoveUnitFromProjectWithDlg;
var dlgParams: m_COD_DlgParams; controller: c_COD_Controller_RemoveUnit;
  module: IOTAModule; unitFileName: String; lstUnitFileNames: TStringList;
  dlg: TdlgDeleteUnitFiles; I: Integer;
begin
ModelBuilder
  .BeginModel(dlgParams, False)
    .Caption('Remove unit')
    .AddProjectUnits
  .EndModel;

module:=taGetCurrentModule;
if Assigned(module) and IsPas(module.FileName) then
  dlgParams.InitialText:=ChangeFileExt(ExtractFileName(module.FileName), '');

controller:=c_COD_Controller_RemoveUnit.Create(dlgParams);
try
  if controller.AskCodeObjects then
    begin
    unitFileName:=controller.DlgResult.FileName;

    RemoveUnitFromProject(unitFileName);

    if not ProjectHasUnit(unitFileName) then
      begin
      lstUnitFileNames:=TStringList.Create;
      try
        GetUnitFileNames(unitFileName, lstUnitFileNames);

        Application.CreateForm(TdlgDeleteUnitFiles, dlg);
        try
          if dlg.ConfirmDeletion(lstUnitFileNames) then
            for i:=0 to lstUnitFileNames.Count-1 do
              MyDeleteFile(lstUnitFileNames[i]);
        finally
          FreeAndNIL(dlg);
        end; // try
      finally
        FreeAndNIL(lstUnitFileNames);
      end; // try
      end;
    end;
finally
  FreeAndNIL(controller);
end; // try
end;

function GetImplementationPosition(AStream: TMemoryStream): Integer;
var Parser: TmPasParser;
begin
Result:=-1;
Parser:=TmPasParser.Create;
try
  Parser.Origin:=AStream.Memory;

  while True do
    begin
    case Parser.Token.ID of
    tkImplementation:
      begin
      Result:=Parser.Token.LineNumber+1;
      break;
      end;
    tkPoint:
      if Parser.PrevToken=tkEnd then break;
    end; // case

    Parser.NextNonJunk;
    end; // while
finally
  FreeAndNil(Parser);
end; // try
end;

procedure GotoImplementationLine(const AEditBuffer: IOTAEditBuffer);
var MS: TMemoryStream; ImplPos: Integer; RD: IOTAEditReader;
  topPos: TOTAEditPos; editView: IOTAEditView;
begin
MS:=TMemoryStream.Create;
try
  RD:=IOTASourceEditor(AEditBuffer).CreateReader;
  taOTAEditReaderToStream(RD, MS);
  RD:=NIL; // IOTAEditReader should never be active at the same time as an IOTAEditWriter

  ImplPos:=GetImplementationPosition(MS);
finally
  MS.Free;
end; // try

if ImplPos<>-1 then
	begin
	editView:=AEditBuffer.TopView;
	editView.Position.GotoLine(ImplPos);

	topPos.Col:=1;
	topPos.Line:=Max(ImplPos-15, 1); { TODO : дублируется с OpenUnit }
	editView.TopPos:=topPos;

	editView.Paint;
	end;
end;

{ TLastUsedUnits }

function TLastUsedUnits.IndexOfUnit(const AUnitID: String): Integer;
var I: Integer;
begin
Result:=UNIT_IS_NOT_RECENT;
for i:=0 to Count-1 do
  begin
  if AUnitID=Strings[i] then
    begin
    Result:=UseUnitHistory-i;
    break
    end;
  end;
end;

constructor TLastUsedUnits.Create;
begin
inherited Create;
LoadFromFile(FN_UseUnitsOrder);

mbExtension.SignObject(Self,
  [NEUseUnit],
  [@TLastUsedUnits.On_NEUseUnit]);
end;

procedure EditFrameworkUnits;
var dlg: TdlgFrameWorkUnits; lst: TStringList;
begin
Application.CreateForm(TdlgFrameWorkUnits, dlg);
try
  lst:=TStringList.Create;
  try
    DOMAIN.FrameWork.LoadFileNames(lst);

    if dlg.Edit(lst) then
      begin
      DOMAIN.FrameWork.SaveFileNames(lst);
      DOMAIN.FrameWork.UpdateUnits;
      end;
  finally
    FreeAndNIL(lst);
  end; // try
finally
  FreeAndNIL(dlg);
end; // try
end;

procedure TLastUsedUnits.On_NEUseUnit(AEvent: NEUseUnit);
var Idx: Integer;
begin
Idx:=IndexOf(AEvent.UnitID);
if Idx=-1 then
  Insert(0, AEvent.UnitID)
else
  Move(Idx, 0);

while Count>UseUnitHistory do Delete(Count-1);

FLastUnit_Name:=AEvent.UnitName;
FLastUnit_IsItfSection:=AEvent.IsItfSection;

SaveToFile(FN_UseUnitsOrder);
end;

destructor TLastUsedUnits.Destroy;
begin
mbExtension.UnsignObject(Self);
inherited Destroy;
end;

end.
