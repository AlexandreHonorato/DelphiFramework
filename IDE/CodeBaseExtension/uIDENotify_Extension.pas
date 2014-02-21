unit uIDENotify_Extension;

interface

uses ToolsAPI, _Debug, Windows, uCodeEntities, SysUtils, _Strings, Forms,
  Classes, _ToolsAPI, uParseEngineIDE, uClassTreeFrm, _Timer, uCodeBaseIDE;

type TIDENotifierExpert = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  private
    FNotifierIndex: Integer;
    FCodeBaseView: TfrmClassTree;
    procedure CodeBaseView_OnClose(Sender: TObject; var Action: TCloseAction);
  public
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    constructor Create;
    destructor Destroy; override;
    function GetMenuText: string;
  end;

type TIDENotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  public
    {From IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    {From IOTAIDENotifier}
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string;
  	   var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject;
         var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

type TModifyTimer = class(TTimer)
  private
    FModifiedUnits: TStringList;
    procedure Self_OnTimer(Sender: TObject);
  public
    procedure UnitModified(const AUnitName: String);
    constructor Create(AInterval: Cardinal); reintroduce;
    destructor Destroy; override;
  end;

type TModuleNotifier = class(TNotifierObject, IOTANotifier, IOTAModuleNotifier)
  private
    FFileName: String;
  public
    {From IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    {From IOTAModuleNotifier}
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
    constructor Create(const AFileName: String); reintroduce;
  end;

var CodeBase_CurrentProject: TCodeBaseIDE;
  
procedure Register;

implementation

uses uParseUtils;

var ModifyTimer: TModifyTimer;

procedure Register;
begin
RegisterPackageWizard(TIDENotifierExpert.Create);
end;

{ TIDENotifier }

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TIDENotifier.AfterSave;
begin

end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TIDENotifier.BeforeSave;
begin

end;

procedure TIDENotifier.Destroyed;
begin

end;

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var Module: IOTAModule; parseEngine: TParseEngineIDE;
begin
if NotifyCode=ofnFileOpened then
  begin
  Module:=(BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
  if Assigned(Module) then
     Module.AddNotifier(TModuleNotifier.Create(Filename));
  end
else
if NotifyCode=ofnActiveProjectChanged then
  begin
  parseEngine:=TParseEngineIDE.Create(CodeBase_CurrentProject);
  try
    parseEngine.FullRebuild;
  finally
    FreeAndNIL(parseEngine);
  end; // try
  end;
end;

procedure TIDENotifier.Modified;
begin

end;

{ TIDENotifierExpert }

constructor TIDENotifierExpert.Create;
begin
inherited Create;
FNotifierIndex:=(BorlandIDEServices as IOTAServices).AddNotifier(TIDENotifier.Create);

CodeBase_CurrentProject:=TCodeBaseIDE.Create;

ModifyTimer:=TModifyTimer.Create(2000);
end;

destructor TIDENotifierExpert.Destroy;
begin
if Assigned(ModifyTimer) then FreeAndNIL(ModifyTimer);

if Assigned(FCodeBaseView) then FreeAndNIL(FCodeBaseView);
if Assigned(CodeBase_CurrentProject) then FreeAndNIL(CodeBase_CurrentProject);

if FNotifierIndex<>-1 then
  (BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifierIndex);
inherited Destroy;
end;

function TIDENotifierExpert.GetIDString: string;
begin
Result:='MyNotifierExpert'
end;

function TIDENotifierExpert.GetMenuText: string;
begin
Result:='Build Tree'
end;

function TIDENotifierExpert.GetName: string;
begin
Result:='My Notifier Expert'
end;

function TIDENotifierExpert.GetState: TWizardState;
begin
Result := [wsEnabled];
end;

procedure TIDENotifierExpert.Execute;
var parseEngine: TParseEngineIDE;
begin
parseEngine:=TParseEngineIDE.Create(CodeBase_CurrentProject);
try
  parseEngine.FullRebuild;
finally
  FreeAndNIL(parseEngine);
end; // try

if not Assigned(FCodeBaseView) then
  begin
  FCodeBaseView:=TfrmClassTree.Create(NIL);
  FCodeBaseView.OnClose:=CodeBaseView_OnClose;
  FCodeBaseView.Model:=CodeBase_CurrentProject;
  FCodeBaseView.Show;
  end;
end;

procedure TIDENotifierExpert.CodeBaseView_OnClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caFree;
FCodeBaseView:=NIL;
end;

{ TModuleNotifier }

procedure TModuleNotifier.AfterSave;
begin

end;

procedure TModuleNotifier.BeforeSave;
begin

end;

function TModuleNotifier.CheckOverwrite: Boolean;
begin
Result:=True;
end;

constructor TModuleNotifier.Create(const AFileName: String);
begin
inherited Create;
FFileName:=AFileName;
end;

procedure TModuleNotifier.Destroyed;
begin

end;

procedure TModuleNotifier.Modified;
begin
ModifyTimer.UnitModified(FFileName);
end;

procedure TModuleNotifier.ModuleRenamed(const NewName: string);
begin
//ODS('Renamed: '+FFileName+' to '+NewName);
FFileName:=NewName;
end;

constructor TModifyTimer.Create(AInterval: Cardinal);
begin
inherited Create(AInterval, Self_OnTimer);
FModifiedUnits:=TStringList.Create;
end;

destructor TModifyTimer.Destroy;
begin
if Assigned(FModifiedUnits) then FreeAndNIL(FModifiedUnits);
inherited Destroy;
end;

procedure TModifyTimer.Self_OnTimer(Sender: TObject);
var parseEngine: TParseEngineIDE;
begin
Enabled:=False;
if FModifiedUnits.Count>0 then
  begin
  parseEngine:=TParseEngineIDE.Create(CodeBase_CurrentProject);
  try
    parseEngine.ApplyChangesInUnits(FModifiedUnits);
  finally
    FreeAndNIL(parseEngine);
  end; // try
  end;
FModifiedUnits.Clear;
end;

procedure TModifyTimer.UnitModified(const AUnitName: String);
var I: Integer; S: String;
begin
Enabled:=False;
if IsPas(AUnitName) then
	begin
	S:=AnsiUpperCase(AUnitName);
	I:=FModifiedUnits.IndexOf(S);
	if I=-1 then
	  FModifiedUnits.Add(S);
	end;
Enabled:=True;
end;

end.


