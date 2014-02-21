unit uCOD_Controller_OpenUnits;

interface

uses uCOD_Controller_Base_MultipleResults, uCOD_Model_Base,
  uCOD_Model_OpenUnits;

type c_COD_Controller_OpenUnits = class(c_COD_Controller_MultipleResults)
  private
    function GetDlgResult: m_COD_DlgResult_OpenUnits;
  protected
    function DoCreateDlgResult: m_COD_DlgResult_Base; override;
  public
    property DlgResult: m_COD_DlgResult_OpenUnits read GetDlgResult;
  end;

implementation

function c_COD_Controller_OpenUnits.DoCreateDlgResult: m_COD_DlgResult_Base;
begin
Result:=m_COD_DlgResult_OpenUnits.Create;
end;

function c_COD_Controller_OpenUnits.GetDlgResult: m_COD_DlgResult_OpenUnits;
begin
Result:=m_COD_DlgResult_OpenUnits(inherited DlgResult)
end;

end.
