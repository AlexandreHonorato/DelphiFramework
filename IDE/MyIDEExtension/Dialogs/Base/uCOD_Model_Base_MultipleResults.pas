unit uCOD_Model_Base_MultipleResults;

interface

uses SysUtils, Classes, uCOD_Model_Base, uCodeEntities;

type m_COD_DlgResult_MultipleResults = class(m_COD_DlgResult_Base)
  private
    FSelectedCodeObjects: TCodeObjectList;
  public
    property SelectedCodeObjects: TCodeObjectList read FSelectedCodeObjects;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ m_COD_DlgResult_MultipleResults }

constructor m_COD_DlgResult_MultipleResults.Create;
begin
inherited Create;
FSelectedCodeObjects:=TCodeObjectList.Create(False);
end;

destructor m_COD_DlgResult_MultipleResults.Destroy;
begin
if Assigned(FSelectedCodeObjects) then FreeAndNIL(FSelectedCodeObjects);
inherited Destroy;
end;

end.
