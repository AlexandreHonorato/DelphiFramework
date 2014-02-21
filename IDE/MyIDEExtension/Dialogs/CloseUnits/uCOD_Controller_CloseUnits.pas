unit uCOD_Controller_CloseUnits;

interface

uses Classes, Controls, uCOD_Controller_Base_MultipleResults, StdCtrls,
  _VCL_ZZZ, Windows, uCOD_Model_Base, uCOD_Model_CloseUnits, uCodeEntities,
  SysUtils;

type c_COD_Controller_CloseUnits = class(c_COD_Controller_MultipleResults)
  private
    rbCloseExceptSelected: TRadioButton;
    rbCloseSelected: TRadioButton;
    function GetDlgResult: m_COD_DlgResult_CloseUnits;
  protected
    function DoCreateDlgResult: m_COD_DlgResult_Base; override;
    procedure DoSetupUI; override;
    procedure DoFillDlgResult; override;
  public
    property DlgResult: m_COD_DlgResult_CloseUnits read GetDlgResult;
  end;

implementation

uses uCOD_Controller_Base;

{ c_COD_Controller_CloseUnits }

procedure c_COD_Controller_CloseUnits.DoSetupUI;
begin
inherited DoSetupUI;

rbCloseExceptSelected:=TRadioButton.Create(Dialog);
rbCloseExceptSelected.Parent:=Dialog;
rbCloseExceptSelected.Caption:='Close all except selected';
rbCloseExceptSelected.SetBounds(8, Dialog.AdditionalControlsTop, 145, rbCloseExceptSelected.Height);
rbCloseExceptSelected.Anchors:=[akLeft, akBottom];
rbCloseExceptSelected.Checked:=True;

rbCloseSelected:=TRadioButton.Create(Dialog);
rbCloseSelected.Parent:=Dialog;
rbCloseSelected.Caption:='Close selected';
rbCloseSelected.SetBounds(168, Dialog.AdditionalControlsTop, 97, rbCloseExceptSelected.Height);
rbCloseSelected.Anchors:=[akLeft, akBottom];
end;

function c_COD_Controller_CloseUnits.DoCreateDlgResult: m_COD_DlgResult_Base;
begin
Result:=m_COD_DlgResult_CloseUnits.Create;
end;

function c_COD_Controller_CloseUnits.GetDlgResult: m_COD_DlgResult_CloseUnits;
begin
Result:=m_COD_DlgResult_CloseUnits(inherited DlgResult);
end;

procedure c_COD_Controller_CloseUnits.DoFillDlgResult;
var lstSelected: TCodeObjectList; I: Integer; co: TCodeObject;
begin
if rbCloseSelected.Checked then
  inherited DoFillDlgResult
else
  begin
  lstSelected:=TCodeObjectList.Create(False);
  try
    for i:=0 to Dialog.lbCodeObjects.Items.Count-1 do
      if Dialog.lbCodeObjects.Selected[i] then
        lstSelected.Add(ViewModels(i).CodeObject);

    for i:=0 to DlgParams.CodeObjects.Count-1 do
      begin
      co:=DlgParams.CodeObjects[i];
      if lstSelected.IndexOf(co)=-1 then
        DlgResult.SelectedCodeObjects.Add(co);
      end;
  finally
    FreeAndNIL(lstSelected);
  end; // try
  end;
end;

end.
