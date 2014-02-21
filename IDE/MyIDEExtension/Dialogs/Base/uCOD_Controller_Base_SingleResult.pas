unit uCOD_Controller_Base_SingleResult;

interface

uses SysUtils, uCOD_Controller_Base, uCOD_Model_Base_SingleResult,
  uCOD_Model_Base, Controls, uCodeEntities;

type c_COD_Controller_SingleResult = class(c_COD_Controller_Base)
  private
    procedure btnOKClick(Sender: TObject);
    function GetDlgResult: m_COD_DlgResult_SingleResult;
  protected
    procedure DoFillDlgResult; override;
    procedure DoSetupUI; override;
  public
    property DlgResult: m_COD_DlgResult_SingleResult read GetDlgResult;
  end;

implementation

procedure c_COD_Controller_SingleResult.btnOKClick(Sender: TObject);
begin
if Dialog.lbCodeObjects.ItemIndex=-1 then
  raise Exception.Create('CodeObject is not selected');

Dialog.ModalResult:=mrOK;
end;

procedure c_COD_Controller_SingleResult.DoFillDlgResult;
begin
DlgResult.CodeObject:=ViewModels(Dialog.lbCodeObjects.ItemIndex).CodeObject;
end;

procedure c_COD_Controller_SingleResult.DoSetupUI;
begin
inherited DoSetupUI;
Dialog.lbCodeObjects.MultiSelect:=False;
Dialog.btnOK.OnClick:=btnOKClick;
end;

function c_COD_Controller_SingleResult.GetDlgResult: m_COD_DlgResult_SingleResult;
begin
Result:=m_COD_DlgResult_SingleResult(inherited DlgResult)
end;

end.
