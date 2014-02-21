unit uUsesAnalyzeBootstrap_Itf;

interface

uses Classes, uFileMappers, uSourceParser;

type IUsesAnalyzeBootstrap = class
  public
    procedure GetUnitNames(AUnitNames: TStrings); virtual; abstract;
    function FileMappers: TFileMappers; virtual; abstract;
    function OnParseError: TParseErrorEvent; virtual; abstract;
    procedure OpenUnit(const AUnitName: String; ALineNo: Integer); virtual; abstract;
  end;

implementation

end.
