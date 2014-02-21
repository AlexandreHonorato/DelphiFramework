unit uFileMappers_IDE;

interface

uses
  classes, uFileMappers, ToolsAPI, _ToolsAPI, uSourceParser, _Strings, _Misc;

type FileMappers_IDE = class(FileMappers_Default)
  public
    class procedure Map_FromEditor(AStream: TMemoryStream; const AFileName: String;
        out AGotData: Boolean; out AStopIteration: Boolean);
    class function Build: TFileMappers; override;
  end;

function GetUnitNames_IDE: TGetUnitNamesDelegate; { TODO : должно быть не здесь, но из-за одной фукции отдельный модуль городить не хочется }

implementation

procedure GetUnitNames(Self: TObject; AUnitNames: TStrings);
var project: IOTAProject; I: Integer; unitFileName: String;
begin
AUnitNames.Clear;

project:=taGetCurrentProject as IOTAProject;
if Assigned(project) then
  for i:=0 to project.GetModuleCount-1 do
    begin
    unitFileName:=project.GetModule(i).FileName;
    if not IsPas(unitFileName) then continue;

    AUnitNames.Add(unitFileName);
    end;
end;

function GetUnitNames_IDE: TGetUnitNamesDelegate;
begin
Result:=TGetUnitNamesDelegate(Method(NIL, @GetUnitNames))
end;

class function FileMappers_IDE.Build: TFileMappers;
begin
Result:=TFileMappers.Create
  .AddFMSDelegate(Map_FromEditor)
  .AddFMSDelegate(Map_FromFile);
end;

class procedure FileMappers_IDE.Map_FromEditor(AStream: TMemoryStream; const
    AFileName: String; out AGotData: Boolean; out AStopIteration: Boolean);
var
  Module: IOTAModule;
  sourceEditor: IOTASourceEditor;
  reader: IOTAEditReader;
begin
AGotData:=False;
Module:=(BorlandIDEServices as IOTAModuleServices).FindModule(AFileName);
if Assigned(Module) then
  begin
  sourceEditor:=Module.GetModuleFileEditor(0) as IOTASourceEditor;
  if Assigned(sourceEditor) then
    begin
    reader:=sourceEditor.CreateReader;
    try
      taOTAEditReaderToStream(reader, AStream);
      AGotData:=True;
      AStopIteration:=True;
    finally
      reader:=NIL;
    end; // try
    end;
  end;
end;

end.
