unit uCallStackAnalyzeBootstrap_Test;

interface

uses uCallStackAnalyzeBootstrap_Itf, classes, uFileMappers, uCallStack_Domain,
  SysUtils;

type TCallStackAnalyzeBootstrap_Test = class(ICallStackAnalyzeBootstrap)
  public
    function CSA_Params_FileName: String; override;
    procedure GetUnitNames(AUnitNames: TStrings); override;
    function FileMappers: TFileMappers; override;
    procedure OpenMethod(AMethod: TcsMethod); override;
    procedure OpenUnit(AUnit: TcsUnit); override;
    procedure OpenErrorID(ACommand: TcsCommand); override;
  end;

implementation

{ TCallStackAnalyzeBootstrap_Test }

function TCallStackAnalyzeBootstrap_Test.CSA_Params_FileName: String;
var tmp: TStringList;
begin
tmp:=TStringList.Create;
try
  tmp.LoadFromFile('d:\Andrey\Desktop\units.txt');
  Result:=tmp[0];
finally
  FreeAndNIL(tmp);
end; // try
end;

function TCallStackAnalyzeBootstrap_Test.FileMappers: TFileMappers;
begin
Result:=FileMappers_Default.Build
end;

procedure TCallStackAnalyzeBootstrap_Test.GetUnitNames(AUnitNames: TStrings);
begin
AUnitNames.LoadFromFile('d:\Andrey\Desktop\units.txt');
AUnitNames.Delete(0); // AUnitNames[0] is .csa file
end;

procedure TCallStackAnalyzeBootstrap_Test.OpenErrorID(
  ACommand: TcsCommand);
begin
// do nothing
end;

procedure TCallStackAnalyzeBootstrap_Test.OpenMethod(AMethod: TcsMethod);
begin
// do nothing
end;

procedure TCallStackAnalyzeBootstrap_Test.OpenUnit(AUnit: TcsUnit);
begin
// do nothing
end;

end.
