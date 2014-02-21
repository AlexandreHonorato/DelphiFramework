unit uUsesAnalyzeBootstrap_IDE;

interface

uses uUsesAnalyzeBootstrap_Itf, Classes, uFileMappers, uSourceParser;

type TUsesAnalyzeBootstrap_IDE = class(IUsesAnalyzeBootstrap)
  public
    procedure GetUnitNames(AUnitNames: TStrings); override;
    function FileMappers: TFileMappers; override;
    function OnParseError: TParseErrorEvent; override;
    procedure OpenUnit(const AUnitName: String; ALineNo: Integer); override;
  end;

implementation

uses uDomain, uFileMappers_IDE, uIDEOperations;

{ TUsesAnalyzeBootstrap_IDE }

function TUsesAnalyzeBootstrap_IDE.FileMappers: TFileMappers;
begin
Result:=FileMappers_IDE.Build
end;

procedure TUsesAnalyzeBootstrap_IDE.GetUnitNames(AUnitNames: TStrings);
var delegate: TGetUnitNamesDelegate;
begin
delegate:=GetUnitNames_IDE();
delegate(AUnitNames)
end;

function TUsesAnalyzeBootstrap_IDE.OnParseError: TParseErrorEvent;
begin
Result:=DOMAIN.OnParseError;
end;

procedure TUsesAnalyzeBootstrap_IDE.OpenUnit(const AUnitName: String; ALineNo: Integer);
begin
uIDEOperations.OpenUnit(AUnitName, ALineNo);
end;

end.
