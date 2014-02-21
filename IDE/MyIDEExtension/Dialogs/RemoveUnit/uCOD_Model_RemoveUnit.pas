unit uCOD_Model_RemoveUnit;

interface

uses uCOD_Model_Base_SingleResult, uCodeEntities, SysUtils;

type m_COD_DlgResult_RemoveUnit = class(m_COD_DlgResult_SingleResult)
  private
    function GetFileName: String;
  public
    property FileName: String read GetFileName; 
  end;
  
implementation

{ m_COD_DlgResult_RemoveUnit }

function m_COD_DlgResult_RemoveUnit.GetFileName: String;
begin
if CodeObject.ObjectType<>coUnit then
  raise Exception.Create('Not a unit');

Result:=TBaseCodeUnit(CodeObject).FileName
end;

end.
