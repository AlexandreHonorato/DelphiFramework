unit uUsesAnalyzeBootstrap_Test;

interface

uses uUsesAnalyzeBootstrap_Itf, Classes, uFileMappers, uSourceParser;

type TUsesAnalyzeBootstrap_Test = class(IUsesAnalyzeBootstrap)
  public
    procedure GetUnitNames(AUnitNames: TStrings); override;
    function FileMappers: TFileMappers; override;
    function OnParseError: TParseErrorEvent; override;
    procedure OpenUnit(const AUnitName: String; ALineNo: Integer); override;
  end;

implementation

{ TUsesAnalyzeBootstrap_Test }

function TUsesAnalyzeBootstrap_Test.FileMappers: TFileMappers;
begin
Result:=FileMappers_Default.Build
end;

procedure TUsesAnalyzeBootstrap_Test.GetUnitNames(AUnitNames: TStrings);
begin
AUnitNames.LoadFromFile('d:\Andrey\Desktop\units.txt');
AUnitNames.Delete(0); // AUnitNames[0] is .csa file
end;

function TUsesAnalyzeBootstrap_Test.OnParseError: TParseErrorEvent;
begin
Result:=NIL;
end;

procedure TUsesAnalyzeBootstrap_Test.OpenUnit(const AUnitName: String; ALineNo: Integer);
begin
// do nothing
end;

end.
