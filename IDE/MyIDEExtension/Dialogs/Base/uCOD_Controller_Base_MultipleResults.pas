unit uCOD_Controller_Base_MultipleResults;

interface

uses SysUtils, Classes, Windows, Controls, uCOD_Controller_Base, uCOD_Model_Base_MultipleResults,
  uCOD_Model_Base, uCodeEntities;

type c_COD_Controller_MultipleResults = class(c_COD_Controller_Base)
  private
    procedure btnOKClick(Sender: TObject);
    procedure txtUnits_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetDlgResult: m_COD_DlgResult_MultipleResults;
  protected
    procedure DoFillDlgResult; override;
    procedure DoSetupUI; override;
  public
    property DlgResult: m_COD_DlgResult_MultipleResults read GetDlgResult;
  end;

implementation

procedure c_COD_Controller_MultipleResults.btnOKClick(Sender: TObject);
begin
if Dialog.lbCodeObjects.SelCount=0 then
  raise Exception.Create('CodeObjects are not selected');

Dialog.ModalResult:=mrOK;
end;

procedure c_COD_Controller_MultipleResults.DoFillDlgResult;
var I: Integer;
begin
for i:=0 to Dialog.lbCodeObjects.Items.Count-1 do
  if Dialog.lbCodeObjects.Selected[i] then
    DlgResult.SelectedCodeObjects.Add(ViewModels(i).CodeObject);
end;

procedure c_COD_Controller_MultipleResults.DoSetupUI;
begin
inherited DoSetupUI;
Dialog.lbCodeObjects.MultiSelect:=True;
Dialog.txtCodeObject.OnKeyDown:=txtUnits_OnKeyDown;
Dialog.btnOK.OnClick:=btnOKClick;
end;

function c_COD_Controller_MultipleResults.GetDlgResult:
    m_COD_DlgResult_MultipleResults;
begin
Result:=m_COD_DlgResult_MultipleResults(inherited DlgResult)
end;

procedure c_COD_Controller_MultipleResults.txtUnits_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
if Key=VK_DOWN then
  begin
  if Dialog.txtCodeObject.ListBox.Count>0 then
    Dialog.txtCodeObject.ListBox.Selected[0]:=True;
  end;
end;

end.
