unit uCOD_Controller_RemoveUnit;

interface

uses uCOD_Controller_Base_SingleResult, uCOD_Model_RemoveUnit, uCOD_Model_Base;

type c_COD_Controller_RemoveUnit = class(c_COD_Controller_SingleResult)
  private
    function GetDlgResult: m_COD_DlgResult_RemoveUnit;
  protected
    procedure Dialog_OnShow(Sender: TObject); override;
    function DoCreateDlgResult: m_COD_DlgResult_Base; override;
  public
    property DlgResult: m_COD_DlgResult_RemoveUnit read GetDlgResult;
  end;

implementation

uses uCOD_Controller_Base;

{ c_COD_Controller_RemoveUnit }

procedure c_COD_Controller_RemoveUnit.Dialog_OnShow(Sender: TObject);
begin
inherited Dialog_OnShow(Sender);
if Dialog.lbCodeObjects.Items.Count=1 then
  Dialog.lbCodeObjects.ItemIndex:=0;
end;

function c_COD_Controller_RemoveUnit.DoCreateDlgResult: m_COD_DlgResult_Base;
begin
Result:=m_COD_DlgResult_RemoveUnit.Create;
end;

function c_COD_Controller_RemoveUnit.GetDlgResult: m_COD_DlgResult_RemoveUnit;
begin
Result:=m_COD_DlgResult_RemoveUnit(inherited DlgResult)
end;

end.
