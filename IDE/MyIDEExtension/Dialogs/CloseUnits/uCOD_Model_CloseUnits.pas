unit uCOD_Model_CloseUnits;

interface

uses uCOD_Model_Base_MultipleResults, Classes, SysUtils, uCodeEntities;

type m_COD_DlgResult_CloseUnits = class(m_COD_DlgResult_MultipleResults)
  private
    FSelectedFileNames: TStrings;
    function GetSelectedFileNames: TStrings;
  public
    property SelectedFileNames: TStrings read GetSelectedFileNames;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ m_COD_DlgResult_CloseUnits }

constructor m_COD_DlgResult_CloseUnits.Create;
begin
inherited Create;
FSelectedFileNames:=TStringList.Create;
end;

destructor m_COD_DlgResult_CloseUnits.Destroy;
begin
if Assigned(FSelectedFileNames) then FreeAndNIL(FSelectedFileNames);
inherited Destroy;
end;

function m_COD_DlgResult_CloseUnits.GetSelectedFileNames: TStrings;
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
