unit uCOD_Controller_UseUnit;

interface

uses SysUtils, Windows, Classes, StdCtrls, Graphics, uCOD_Controller_Base_SingleResult, uCOD_Model_UseUnit, uCodeEntities,
  uCOD_Model_Base, _VCL_ZZZ, Controls, _gdi;

type c_COD_Controller_UseUnit = class(c_COD_Controller_SingleResult)
  private
    chkImplSection: TCheckBox;
    function GetDlgResult: m_COD_DlgResult_UseUnit;
  protected
    function DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject; override;
    function GetSortVariantsProc: TStringListSortCompare; override;
    function DoCreateDlgResult: m_COD_DlgResult_Base; override;
    procedure DoFillDlgResult; override;
    procedure DoSetupUI; override;
  public
    property DlgResult: m_COD_DlgResult_UseUnit read GetDlgResult;
  end;

implementation

uses uUnits, uDOMAIN;

function c_COD_Controller_UseUnit.DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject;
var vm: VM_Unit_ForUseUnit; unit_: TBaseCodeUnit;
begin
case ACodeObject.ObjectType of
coUnit:
  begin
  unit_:=TBaseCodeUnit(ACodeObject);
  vm:=VM_Unit_ForUseUnit.Create(unit_);
  vm.RecentIndex:=DOMAIN.LastUsedUnits.IndexOfUnit(unit_.ID);
  Result:=vm;
  end;
else
  raise Exception.CreateFmt('Unsupported ObjectType=%d', [Integer(ACodeObject.ObjectType)]);
end; // case
end;

function c_COD_Controller_UseUnit.DoCreateDlgResult: m_COD_DlgResult_Base;
begin
Result:=m_COD_DlgResult_UseUnit.Create;
end;

procedure c_COD_Controller_UseUnit.DoFillDlgResult;
begin
inherited DoFillDlgResult;
DlgResult.ItfSection:=not chkImplSection.Checked;
end;

procedure c_COD_Controller_UseUnit.DoSetupUI;
begin
inherited DoSetupUI;

chkImplSection:=TCheckBox.Create(Dialog);
chkImplSection.Parent:=Dialog;
chkImplSection.Caption:='&Implementation section';
chkImplSection.Left:=8;
chkImplSection.Width:=145;
chkImplSection.Top:=Dialog.AdditionalControlsTop;
chkImplSection.Checked:=False;
chkImplSection.Anchors:=[akLeft, akBottom];
end;

function c_COD_Controller_UseUnit.GetDlgResult: m_COD_DlgResult_UseUnit;
begin
Result:=m_COD_DlgResult_UseUnit(inherited DlgResult)
end;

function CompareUseUnit(List: TStringList; Index1, Index2: Integer): Integer;
var vm1, vm2: VM_Unit_ForUseUnit;
begin
vm1:=VM_Unit_ForUseUnit(List.Objects[Index1]);
vm2:=VM_Unit_ForUseUnit(List.Objects[Index2]);
Result:=Integer(vm2.RecentIndex)-Integer(vm1.RecentIndex);

if Result=0 then
  Result:=AnsiCompareText(vm1.Caption, vm2.Caption);
end;

function c_COD_Controller_UseUnit.GetSortVariantsProc: TStringListSortCompare;
begin
Result:=@CompareUseUnit
end;

end.
