unit uCOD_Model_OpenUnits;

interface

uses
  classes, uCOD_Model_Base_MultipleResults, SysUtils, uCodeEntities;

type m_COD_DlgResult_OpenUnits = class(m_COD_DlgResult_MultipleResults)
  private
    FSelectedFileNames: TStrings;
    function GetSelectedFileNames: TStrings;
  public
    property SelectedFileNames: TStrings read GetSelectedFileNames;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ m_COD_DlgResult_OpenUnits }

constructor m_COD_DlgResult_OpenUnits.Create;
begin
inherited Create;
FSelectedFileNames:=TStringList.Create;
end;

destructor m_COD_DlgResult_OpenUnits.Destroy;
begin
if Assigned(FSelectedFileNames) then FreeAndNIL(FSelectedFileNames);
inherited Destroy;
end;

function m_COD_DlgResult_OpenUnits.GetSelectedFileNames: TStrings;
var I: Integer;
begin
FSelectedFileNames.Clear;

for i:=0 to SelectedCodeObjects.Count-1 do
  begin
  if SelectedCodeObjects[i].ObjectType<>coUnit then
    raise Exception.Create('Not a unit');

  FSelectedFileNames.Add(TBaseCodeUnit(SelectedCodeObjects[i]).FileName);
  end;

Result:=FSelectedFileNames
end;

end.
