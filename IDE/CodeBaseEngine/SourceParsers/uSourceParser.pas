unit uSourceParser;

interface

uses
  classes, uCodeEntities, SysUtils, _Strings, _MessageBus, uParseInfo,
  uStreamParser, uFileMappers;

type TParseErrorEvent = procedure(AException: Exception) of object;

type TGetUnitNamesDelegate = procedure(AUnitNames: TStrings) of object;

type TSourceParser = class
  private
    FOnParseError: TParseErrorEvent;
    FGetUnitNames: TGetUnitNamesDelegate;
    FFileMappers: TFileMappers;
  protected
    procedure HandleException(AException: Exception);
  public
    constructor Create(AFileMappers: TFileMappers;
      AGetUnitNames: TGetUnitNamesDelegate); reintroduce; virtual;
    destructor Destroy; override;
    property OnParseError: TParseErrorEvent read FOnParseError write FOnParseError;
    procedure ParseUnits(AUnitNames: TStrings; AStreamParser: TStreamParser); overload;
    procedure ParseUnits(AStreamParser: TStreamParser); overload;
  end;

implementation

{ TSourceParser }

constructor TSourceParser.Create(AFileMappers: TFileMappers;
    AGetUnitNames: TGetUnitNamesDelegate);
begin
inherited Create;
FFileMappers:=AFileMappers;
FGetUnitNames:=AGetUnitNames;
end;

destructor TSourceParser.Destroy;
begin
if Assigned(FFileMappers) then FreeAndNIL(FFileMappers);
inherited Destroy;
end;

procedure TSourceParser.HandleException(AException: Exception);
begin
if Assigned(FOnParseError) then
  FOnParseError(AException)
else
  raise AException;
end;

procedure TSourceParser.ParseUnits(AUnitNames: TStrings; AStreamParser: TStreamParser);
var I: Integer; stream: TMemoryStream; unitFileName: String;
begin
for i:=0 to AUnitNames.Count-1 do
  begin
  unitFileName:=AUnitNames[i];

  stream:=TMemoryStream.Create;
  try
    if FFileMappers.MapFile(unitFileName, stream) then
      AStreamParser.ParseStream(unitFileName, stream);
  finally
    FreeAndNIL(stream);
  end; // try;
  end;
end;

procedure TSourceParser.ParseUnits(AStreamParser: TStreamParser);
var lstAllUnitNames: TStringList;
begin
if not Assigned(FGetUnitNames) then exit;

lstAllUnitNames:=TStringList.Create;
try
  FGetUnitNames(lstAllUnitNames);
  ParseUnits(lstAllUnitNames, AStreamParser);
finally
  FreeAndNIL(lstAllUnitNames);
end; // try
end;

end.
