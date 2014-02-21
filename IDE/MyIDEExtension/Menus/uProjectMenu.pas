unit uProjectMenu;

interface

uses SysUtils, Windows, Messages, Classes, Menus, Forms, ToolsAPI,
  Registry, uErrorGUIDProcess, _Strings, _ToolsAPI, _Misc, _Files, uIDEOperations;

const
  SN_OPENUNIT = LB_ADDSTRING;

  F10_ACTION_ERROR = 0;
  F10_ACTION_SYNTAX_CHECK = 1;
  F10_ACTION_BUILD_ALL = 2;

type TMainMenu_Project = class(TToolsAPIMenu)
  private
    F10ShortCut: TShortCut;
    FNotifyHandle: HWND;
    procedure NotifyWndProc(var Msg: TMessage);  { TODO : нотификатор должен быть отдельным классом в отдельном модуле; выделить, унаследовать от TAPIWindow }
  private
    mnuUsesAnalyze: TMenuItem;
    mnuCallStackAnalyze: TMenuItem;
    mnuClassesAnalyze: TMenuItem;
    mnuErrorGUIDs: TMenuItem;
    mnuErrorGUIDsForce: TMenuItem;
    mnuRemoveUnit: TMenuItem;
    mnuEditFrameworkUnits: TMenuItem;

    procedure mnuUsesAnalyzeClick(Sender: TObject);
    procedure mnuCallStackAnalyzeClick(Sender: TObject);
    procedure mnuClassesAnalyzeClick(Sender: TObject);
    procedure mnuErrorGUIDsClick(Sender: TObject);
    procedure mnuErrorGUIDsForceClick(Sender: TObject);
    procedure mnuRemoveUnitClick(Sender: TObject);
    procedure mnuEditFrameworkUnitsClick(Sender: TObject);
    procedure RunAnalyzer(const AnalyzerName: String);
    procedure ProcessErrorGUIDs(AForce: Boolean);
  protected
    procedure InitMenu; override;
    procedure DoneMenu; override;
  public
    function InvertF10ShortCut(AProjectMenuItem: TMenuItem = NIL): Integer;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses uUnits, uUsesAnalyzeFrm, uDOMAIN, uCallStackAnalyzeFrm,
  uCallStackAnalyzeBootstrap_IDE, uUsesAnalyzeBootstrap_IDE;

const
  sSyntaxCheckMenuItemName = 'ProjectSyntaxItem';
  sBuildAllMenuItemName = 'ProjectBuildAllItem';

procedure TMainMenu_Project.DoneMenu;
begin
if Assigned(mnuEditFrameworkUnits) then FreeAndNIL(mnuEditFrameworkUnits);
if Assigned(mnuUsesAnalyze) then FreeAndNIL(mnuUsesAnalyze);
if Assigned(mnuCallStackAnalyze) then FreeAndNIL(mnuCallStackAnalyze);
if Assigned(mnuClassesAnalyze) then mnuClassesAnalyze.Free;
if Assigned(mnuErrorGUIDs) then mnuErrorGUIDs.Free;
if Assigned(mnuErrorGUIDsForce) then mnuErrorGUIDsForce.Free;
if Assigned(mnuRemoveUnit) then mnuRemoveUnit.Free;
end;

procedure TMainMenu_Project.InitMenu;
var mnuProject: TMenuItem;
begin
mnuProject:=(BorlandIDEServices as INTAServices40).MainMenu.Items.Find('&Project');
if not Assigned(mnuProject) then exit;

F10ShortCut:=ShortCut(VK_F10, []);
InvertF10ShortCut(mnuProject);

mnuUsesAnalyze:=NewItem('Uses Analyze...', 0, False, True, mnuUsesAnalyzeClick, 0, '');
mnuProject.Add(mnuUsesAnalyze);

mnuClassesAnalyze:=NewItem('Classes Analyze...', 0, False, True, mnuClassesAnalyzeClick, 0, '');
mnuProject.Add(mnuClassesAnalyze);

mnuCallStackAnalyze:=NewItem('Call Stack Analyze...', 0, False, True, mnuCallStackAnalyzeClick, 0, '');
mnuProject.Add(mnuCallStackAnalyze);

mnuErrorGUIDs:=NewItem('Process ErrorGUIDs', 0, False, True, mnuErrorGUIDsClick, 0, '');
mnuProject.Add(mnuErrorGUIDs);

mnuErrorGUIDsForce:=NewItem('Process ErrorGUIDs (force)', 0, False, True, mnuErrorGUIDsForceClick, 0, '');
mnuProject.Add(mnuErrorGUIDsForce);

mnuRemoveUnit:=NewItem('Remove unit...', 0, False, True, mnuRemoveUnitClick, 0, '');
mnuProject.Add(mnuRemoveUnit);

mnuEditFrameworkUnits:=NewItem('Framework units...', 0, False, True, mnuEditFrameworkUnitsClick, 0, '');
mnuProject.Add(mnuEditFrameworkUnits);
end;

procedure TMainMenu_Project.mnuClassesAnalyzeClick(Sender: TObject);
begin
RunAnalyzer('ClassesAnalyze');
end;

procedure TMainMenu_Project.RunAnalyzer(const AnalyzerName: String);
var CurrProject: IOTAModule; R: TRegistry; ExePath, S: String; F: TextFile; I: Integer; P: IOTAProject;
begin
CurrProject:=taGetCurrentProject;
if CurrProject=NIL then exit;
P:=CurrProject as IOTAProject;

ExePath:='';
R:=TRegistry.Create;
R.RootKey:=HKEY_CURRENT_USER;
if R.OpenKey('Software\Borland\Delphi', False) then ExePath:=R.ReadString(AnalyzerName+'Path');
R.Free;

if ExePath='' then
  begin
  MessageBox(Application.Handle, PChar('Value "'+AnalyzerName+'Path" not found in "HKEY_CURRENT_USER\Software\Borland\Delphi" key'), 'Analyze', MB_ICONERROR);
  exit;
  end;

AssignFile(F, ChangeFileExt(ExePath, '.txt'));
Rewrite(F);
Writeln(F, CurrProject.FileName);
for i:=0 to P.GetModuleCount-1 do
  begin
  S:=P.GetModule(i).FileName;
  if not IsPas(S) then continue;
  Writeln(F, S);
  end;
CloseFile(F);

Execute(ExePath)
end;

constructor TMainMenu_Project.Create;
begin
inherited Create;
FNotifyHandle:=Classes.AllocateHWnd(NotifyWndProc);
SetWindowText(FNotifyHandle, '{988FC693-CF2E-4E6E-A698-8EB115FAFB4C}');
end;

destructor TMainMenu_Project.Destroy;
begin
Classes.DeallocateHWnd(FNotifyHandle);
inherited Destroy;
end;

procedure TMainMenu_Project.NotifyWndProc(var Msg: TMessage);
var S: String;
begin
with Msg do
  if Msg=SN_OPENUNIT then
    try
      S:=StrPas(PChar(LParam));
      OpenUnit(S, WParam);
    except
      Application.HandleException(Self);
    end
  else
    Result:=DefWindowProc(FNotifyHandle, Msg, wParam, lParam);
end;

procedure TMainMenu_Project.mnuErrorGUIDsClick(Sender: TObject);
begin
ProcessErrorGUIDs(False)
end;

procedure TMainMenu_Project.ProcessErrorGUIDs(AForce: Boolean);
var CurrProject: IOTAModule; S: String; Idx, Cnt, I: Integer; P: IOTAProject;
  AServices: IOTAActionServices;
begin
CurrProject:=taGetCurrentProject;
if CurrProject=NIL then exit;

P:=CurrProject as IOTAProject;
(BorlandIDEServices as IOTAModuleServices).SaveAll;

AServices:=(BorlandIDEServices as IOTAActionServices);
if AForce then
  begin
	Idx:=1;
	for i:=0 to P.GetModuleCount-1 do
	  begin
	  S:=P.GetModule(i).FileName;
	  if IsPas(S) then
		  begin
		  Cnt:=EG_ProcessFile(S, Idx, True);
      try
        AServices.ReloadFile(S);
      except
        // если открыта форма с фреймом, дает ошибку "Module 'd:\andrey\delphi\doc\uTaskDateFrame.pas' has open descendents or linked modules. Cannot reload"
      end; // try

		  Idx:=Idx+Cnt;
		  end;
	  end;
  end
else
	begin
	Idx:=0;
	for i:=0 to P.GetModuleCount-1 do
	  begin
	  S:=P.GetModule(i).FileName;
	  if IsPas(S) then
	    Idx:=Max(Idx, EG_GetMaxID(S));
	  end;

	Inc(Idx);
	for i:=0 to P.GetModuleCount-1 do
	  begin
	  S:=P.GetModule(i).FileName;
	  if IsPas(S) then
		  begin
		  Cnt:=EG_ProcessFile(S, Idx, False);
      try
        AServices.ReloadFile(S);
      except
        // если открыта форма с фреймом, дает ошибку "Module 'd:\andrey\delphi\doc\uTaskDateFrame.pas' has open descendents or linked modules. Cannot reload"
      end; // try
		  Idx:=Idx+Cnt;
		  end;
	  end;
	end;
end;

procedure TMainMenu_Project.mnuErrorGUIDsForceClick(Sender: TObject);
begin
if MessageBox(Application.Handle, 'Are you sure you want to override ALL ErrorGUIDs?',
    'Force ErrorGUIDs', MB_ICONQUESTION or MB_OKCANCEL)=IDOK then
  ProcessErrorGUIDs(True)
end;

procedure TMainMenu_Project.mnuRemoveUnitClick(Sender: TObject);
begin
RemoveUnitFromProjectWithDlg;
end;

procedure TMainMenu_Project.mnuEditFrameworkUnitsClick(Sender: TObject);
begin
EditFrameworkUnits;
end;

procedure TMainMenu_Project.mnuUsesAnalyzeClick(Sender: TObject);
var frm: TfrmUsesAnalyze;
begin
Application.CreateForm(TfrmUsesAnalyze, frm);
frm.SetBootstrap(TUsesAnalyzeBootstrap_IDE.Create);
frm.Show;
frm.UpdateContents;
end;

function TMainMenu_Project.InvertF10ShortCut(AProjectMenuItem: TMenuItem = NIL): Integer;
var mnuSyntaxCheck, mnuBuildAll, MI: TMenuItem; I: Integer;
begin
Result:=F10_ACTION_ERROR;

if not Assigned(AProjectMenuItem) then
  AProjectMenuItem:=(BorlandIDEServices as INTAServices40).MainMenu.Items.Find('&Project');
if not Assigned(AProjectMenuItem) then
  exit;

mnuSyntaxCheck:=NIL;
mnuBuildAll:=NIL;
for i:=0 to AProjectMenuItem.Count-1 do
  begin
  MI:=AProjectMenuItem.Items[i];
  if MI.Name=sSyntaxCheckMenuItemName then mnuSyntaxCheck:=MI;
  if MI.Name=sBuildAllMenuItemName then mnuBuildAll:=MI;
  if Assigned(mnuSyntaxCheck) and Assigned(mnuBuildAll) then break;
  end;

if not Assigned(mnuSyntaxCheck) then exit;
if not Assigned(mnuBuildAll) then exit;

if mnuSyntaxCheck.ShortCut=F10ShortCut then
  begin
  mnuSyntaxCheck.ShortCut:=0;
  mnuBuildAll.ShortCut:=F10ShortCut;
  Result:=F10_ACTION_BUILD_ALL
  end
else
  begin
  mnuSyntaxCheck.ShortCut:=F10ShortCut;
  mnuBuildAll.ShortCut:=0;
  Result:=F10_ACTION_SYNTAX_CHECK
  end;
end;

procedure TMainMenu_Project.mnuCallStackAnalyzeClick(Sender: TObject);
var frm: TfrmCallStackAnalyze;
begin
Application.CreateForm(TfrmCallStackAnalyze, frm);
frm.SetBootstrap(TCallStackAnalyzeBootstrap_IDE.Create);
frm.Show;
frm.UpdateContents;
end;

end.

