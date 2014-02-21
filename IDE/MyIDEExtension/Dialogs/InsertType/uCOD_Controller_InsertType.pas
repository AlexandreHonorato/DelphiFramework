unit uCOD_Controller_InsertType;

interface

uses
  uCOD_Controller_Base_SingleResult, uCOD_Model_Base, uCodeEntities, SysUtils,
  uCOD_Model_Base_SingleResult;

type c_COD_Controller_InsertType = class(c_COD_Controller_SingleResult)
  protected
    procedure Dialog_OnShow(Sender: TObject); override;
    function DoCreateDlgResult: m_COD_DlgResult_Base; override;
    function DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject; override;
  end;

implementation

procedure c_COD_Controller_InsertType.Dialog_OnShow(Sender: TObject);
begin
inherited Dialog_OnShow(Sender);
Dialog.txtCodeObject.SelStart:=Length(Dialog.txtCodeObject.Text);
Dialog.txtCodeObject.SelLength:=0;
end;

function c_COD_Controller_InsertType.DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject;
begin
case ACodeObject.ObjectType of
coClass, coInterface, coMetaclass, coSingleton:
  Result:=VM_UnitElement.Create(ACodeObject);
else
  raise Exception.CreateFmt('Unsupported ObjectType=%d', [Integer(ACodeObject.ObjectType)]);
end; // case
end;

function c_COD_Controller_InsertType.DoCreateDlgResult:
    m_COD_DlgResult_Base;
begin
Result:=m_COD_DlgResult_SingleResult.Create;
end;

end.
