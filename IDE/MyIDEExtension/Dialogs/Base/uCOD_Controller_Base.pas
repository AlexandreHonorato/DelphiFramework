unit uCOD_Controller_Base;

interface

uses SysUtils, Classes, Controls, Windows, Forms, Graphics, uCOD_Model_Base, uCodeEntities, _gdi,
  Menus, uCodeObjectsDlg;

const coAllTypes = coUnknown;

type c_COD_Controller_Base = class
  private
    FDlgResult: m_COD_DlgResult_Base;
    FDialog: TdlgCodeObjects;
    FDlgParams: m_COD_DlgParams;
    FTypeFilter: TCodeObjectTypes;
    procedure DoFillVariantsList(AStrings: TStrings; AFilter: TCodeObjectType);
    procedure DoClearVariantsList(AStrings: TStrings);
    procedure lbCodeObjects_OnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure SetTypeFilter(const Value: TCodeObjectTypes);
    procedure SetupFilterUI;
  protected
    function GetSortVariantsProc: TStringListSortCompare; virtual;
    function DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject; virtual;
    procedure DoSetupUI; virtual;
    function DoCreateDlgResult: m_COD_DlgResult_Base; virtual; abstract;
    procedure DoFillDlgResult; virtual; abstract;
    procedure Dialog_OnFilterChanged(Sender: TObject; ANewFilter: TCodeObjectType); virtual;
    procedure Dialog_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure Dialog_OnShow(Sender: TObject); virtual;
  public
    property DlgParams: m_COD_DlgParams read FDlgParams;
    property DlgResult: m_COD_DlgResult_Base read FDlgResult;
    property Dialog: TdlgCodeObjects read FDialog;
    property TypeFilter: TCodeObjectTypes read FTypeFilter write SetTypeFilter;
    function AskCodeObjects: Boolean;
    constructor Create(ADlgParams: m_COD_DlgParams); reintroduce;
    destructor Destroy; override;
    function ViewModels(Index: Integer): VM_CodeObject;
  end;

implementation

const FilterShortCuts: array[Low(TCodeObjectType)..High(TCodeObjectType)] of Integer =
  (scCtrl+scAlt+Ord('A'), scCtrl+scAlt+Ord('U'), scCtrl+scAlt+Ord('C'),
   scCtrl+scAlt+Ord('I'), scCtrl+scAlt+Ord('M'), scCtrl+scAlt+Ord('S'));

const FilterCaptions: array[Low(TCodeObjectType)..High(TCodeObjectType)] of String =
  ('All objects', 'Units', 'Classes', 'Interfaces', 'Metaclasses', 'Singletons');

{ c_COD_Controller_Base }

function c_COD_Controller_Base.AskCodeObjects: Boolean;
begin
Application.CreateForm(TdlgCodeObjects, FDialog);
try
  DoSetupUI;
  DoFillVariantsList(FDialog.txtCodeObject.Variants, coAllTypes);

  FDialog.txtCodeObject.Text:=DlgParams.InitialText;
  FDialog.txtCodeObject.Text2ListBox;

  Result:=FDialog.ShowModal=mrOK;

  if Result then
    begin
    FDlgResult:=DoCreateDlgResult;

    DoFillDlgResult;
    end
  else
    FDlgResult:=NIL;
finally
  DoClearVariantsList(FDialog.txtCodeObject.Variants);
  FreeAndNIL(FDialog);
end; // try
end;

constructor c_COD_Controller_Base.Create(ADlgParams: m_COD_DlgParams);
begin
inherited Create;
FTypeFilter:=[coAllTypes];
FDlgParams:=ADlgParams;
end;

destructor c_COD_Controller_Base.Destroy;
begin
if Assigned(FDlgResult) then FreeAndNIL(FDlgResult);
if Assigned(FDlgParams) then FreeAndNIL(FDlgParams);
inherited Destroy;
end;

procedure c_COD_Controller_Base.DoSetupUI;
begin
SetupFilterUI;

FDialog.lbCodeObjects.OnDrawItem:=lbCodeObjects_OnDrawItem;
FDialog.OnKeyDown:=Dialog_OnKeyDown;
FDialog.OnShow:=Dialog_OnShow;
FDialog.Caption:=FDlgParams.Caption;
end;

procedure c_COD_Controller_Base.DoFillVariantsList(AStrings: TStrings; AFilter:
    TCodeObjectType);

  function SatisfiesFilter(ACodeObject: TCodeObject): Boolean;
  begin
  Result:=(AFilter=coAllTypes) or (AFilter=ACodeObject.ObjectType)
  end;

var I: Integer; vm: VM_CodeObject; codeObject: TCodeObject;
begin
AStrings.Clear;

for i:=0 to FDlgParams.CodeObjects.Count-1 do
  begin
  codeObject:=FDlgParams.CodeObjects[i];
  if SatisfiesFilter(codeObject) then
	  begin
	  vm:=DoBuildViewModel(codeObject);
	  AStrings.AddObject(vm.Caption, vm);
	  end;
  end;

TStringList(AStrings).CustomSort(GetSortVariantsProc());
end;

procedure c_COD_Controller_Base.DoClearVariantsList(AStrings: TStrings);
var I: Integer;
begin
for i:=0 to AStrings.Count-1 do VM_CodeObject(AStrings.Objects[i]).Free;
AStrings.Clear;
end;

function CompareByCaption(List: TStringList; Index1, Index2: Integer): Integer;
begin
Result:=AnsiCompareText(List[Index1], List[Index2]);
end;

function c_COD_Controller_Base.GetSortVariantsProc: TStringListSortCompare;
begin
Result:=@CompareByCaption
end;

procedure c_COD_Controller_Base.lbCodeObjects_OnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
Dialog.lbCodeObjects.Canvas.FillRect(Rect);
ViewModels(Index).Draw(Dialog.lbCodeObjects.Canvas, Rect, odSelected in State);
end;

function c_COD_Controller_Base.ViewModels(Index: Integer): VM_CodeObject;
begin
Result:=VM_CodeObject(Dialog.lbCodeObjects.Items.Objects[Index])
end;

function c_COD_Controller_Base.DoBuildViewModel(ACodeObject: TCodeObject): VM_CodeObject;
begin
case ACodeObject.ObjectType of
coUnit:
  Result:=VM_Unit.Create(ACodeObject);
else
  raise Exception.CreateFmt('Unsupported ObjectType=%d', [Integer(ACodeObject.ObjectType)]);
end; // case
end;

procedure c_COD_Controller_Base.SetupFilterUI;

  function GetFilterCaption(ACodeObjectType: TCodeObjectType): String;
  begin
  Result:=FilterCaptions[ACodeObjectType];
  if FilterShortCuts[ACodeObjectType]<>0 then
    Result:=Result+' ('+ShortCutToText(FilterShortCuts[ACodeObjectType])+')';
  end;

var co: TCodeObjectType;
begin
Dialog.ShowFilter:=FTypeFilter<>[coAllTypes];
if not Dialog.ShowFilter then exit;

for co:=Low(TCodeObjectType) to High(TCodeObjectType) do
  if (co in FTypeFilter) then
    Dialog.AddFilter(GetFilterCaption(co), co);

Dialog.Filter:=coAllTypes;
Dialog.OnFilterChanged:=Dialog_OnFilterChanged;
end;

procedure c_COD_Controller_Base.Dialog_OnFilterChanged(Sender: TObject;
  ANewFilter: TCodeObjectType);
begin
DoClearVariantsList(FDialog.txtCodeObject.Variants);
DoFillVariantsList(FDialog.txtCodeObject.Variants, ANewFilter);
end;

procedure c_COD_Controller_Base.Dialog_OnKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var sc: Integer; co: TCodeObjectType;
begin
if Dialog.ShowFilter then
	begin
	sc:=ShortCut(Key, Shift);

	for co:=Low(TCodeObjectType) to High(TCodeObjectType) do
	  if (FilterShortCuts[co]=sc) and (co in FTypeFilter) then
	    begin
	    Key:=0;
	    Dialog.Filter:=co;
	    exit;
	    end;
	end;

if Key=VK_F5 then Dialog.lbCodeObjects.Invalidate;
end;

procedure c_COD_Controller_Base.Dialog_OnShow(Sender: TObject);
begin
Dialog.txtCodeObject.SetFocus;
end;

procedure c_COD_Controller_Base.SetTypeFilter(const Value: TCodeObjectTypes);
begin
FTypeFilter:=Value;
if not (coAllTypes in FTypeFilter) then
  Include(FTypeFilter, coAllTypes);
end;

end.




