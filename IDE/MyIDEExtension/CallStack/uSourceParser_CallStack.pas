unit uSourceParser_CallStack;

interface

uses uSourceParser, uCallStack_Domain, uFileMappers, SysUtils, Classes;

type TSourceParser_CallStack = class(TSourceParser)
  private
    FUnits: TcsUnitList;
    FErrorIdIndex: TcsErrorIdIndex;
    FCallStackNames: TStrings;
  public
    property Units: TcsUnitList read FUnits;
    property ErrorIdIndex: TcsErrorIdIndex read FErrorIdIndex;
    constructor Create(AFileMappers: TFileMappers;
      AGetUnitNames: TGetUnitNamesDelegate; AUnits: TcsUnitList;
      AErrorIdIndex: TcsErrorIdIndex; ACallStackNames: TStrings); reintroduce;
    procedure Parse;
  end;

implementation

uses uStreamParser_CallStack;

{ TSourceParser_CallStack }

constructor TSourceParser_CallStack.Create(AFileMappers: TFileMappers;
  AGetUnitNames: TGetUnitNamesDelegate; AUnits: TcsUnitList;
  AErrorIdIndex: TcsErrorIdIndex; ACallStackNames: TStrings);
begin
inherited Create(AFileMappers, AGetUnitNames);
FUnits:=AUnits;
FErrorIdIndex:=AErrorIdIndex;
FCallStackNames:=ACallStackNames;
end;

procedure TSourceParser_CallStack.Parse;
var streamParser: TStreamParser_CallStack;
begin
streamParser:=TStreamParser_CallStack.Create(FUnits, FErrorIdIndex, FCallStackNames);
try
  ParseUnits(streamParser);
finally
  FreeAndNIL(streamParser);
end; // try
end;

end.
