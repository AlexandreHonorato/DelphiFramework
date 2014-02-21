unit uCallStackAnalyzeBootstrap_IDE;

interface

uses SysUtils, uCallStackAnalyzeBootstrap_Itf, uFileMappers, Classes,
  uCallStack_Domain, _ToolsAPI, ToolsAPI;

type TCallStackAnalyzeBootstrap_IDE = class(ICallStackAnalyzeBootstrap)
  public
    function CSA_Params_FileName: String; override;
    procedure GetUnitNames(AUnitNames: TStrings); override;
    function FileMappers: TFileMappers; override;
    procedure OpenMethod(AMethod: TcsMethod); override;
    procedure OpenUnit(AUnit: TcsUnit); override;
    procedure OpenErrorID(ACommand: TcsCommand); override;
  end;

implementation

uses uFileMappers_IDE, uSourceParser, uIDEOperations;

{ TCallStackAnalyzeBootstrap_IDE }

function TCallStackAnalyzeBootstrap_IDE.CSA_Params_FileName: String;
var project: IOTAModule;
begin
project:=taGetCurrentProject;
if Assigned(project) then
  Result:=ChangeFileExt(project.FileName, '.csa')
else
  Result:=''
end;

function TCallStackAnalyzeBootstrap_IDE.FileMappers: TFileMappers;
begin
Result:=FileMappers_IDE.Build
end;

procedure TCallStackAnalyzeBootstrap_IDE.GetUnitNames(AUnitNames: TStrings);
var delegate: TGetUnitNamesDelegate;
begin
delegate:=GetUnitNames_IDE();
delegate(AUnitNames)
end;

procedure TCallStackAnalyzeBootstrap_IDE.OpenErrorID(ACommand: TcsCommand);
begin
uIDEOperations.OpenUnit(ACommand.Method.Unit_.FileName, ACommand.Line);
end;

procedure TCallStackAnalyzeBootstrap_IDE.OpenMethod(AMethod: TcsMethod);
begin
uIDEOperations.OpenUnit(AMethod.Unit_.FileName, AMethod.StartLine);
end;

procedure TCallStackAnalyzeBootstrap_IDE.OpenUnit(AUnit: TcsUnit);
begin
uIDEOperations.OpenUnit(AUnit.FileName, 1);
end;

end.
