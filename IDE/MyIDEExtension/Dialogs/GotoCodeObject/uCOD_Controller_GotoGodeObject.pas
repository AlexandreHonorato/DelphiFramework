unit uCOD_Controller_GotoGodeObject;

interface

uses
  uCOD_Model_Base, uCOD_Controller_Base_SingleResult,
  uCodeEntities, SysUtils, uCOD_Model_Base_SingleResult;

type c_COD_Controller_GotoGodeObject = class(c_COD_Controller_SingleResult)
  protected
    function DoCreateDlgResult: m_COD_DlgResult_Base; override;
    function DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject; override;
  end;

implementation

function c_COD_Controller_GotoGodeObject.DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject;
begin
case ACodeObject.ObjectType of
coUnit:
  Result:=VM_Unit.Create(ACodeObject);
coClass, coInterface:
  Result:=VM_UnitElement.Create(ACodeObject);
else
  raise Exception.CreateFmt('Unsupported ObjectType=%d', [Integer(ACodeObject.ObjectType)]);
end; // case
end;

function c_COD_Controller_GotoGodeObject.DoCreateDlgResult:
    m_COD_DlgResult_Base;
begin
Result:=m_COD_DlgResult_SingleResult.Create;
end;

end.
