unit uUsesAnalyzeFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, uCodeEntities, ExtCtrls, _Misc, _Lists, ImgList,
  _Entity, _VCL_ZZZ, Menus, StdCtrls,
  CheckLst, _Strings, TLB_MSXML, _XML10, _GDI, uCodeBase,
  uSourceParser_CodeBase, uUsesAnalyzeBootstrap_Itf;

const
  iiError = 0;
  iiNotResolved = 1;
  iiOK = 2;
  iiFolder = 3;
  iiUnit = 4;
  iiCheck = 5;
  iiUncheck = 6;

type VM_Unit_ForUsesAnalyze = class
  private
    FLayer: TUnitLayer;
    FUsesStatus: array[TUnitSection] of TUsesStatus;
    FUnit: TCodeUnit;
    function GetUsesStatus(Index: TUnitSection): TUsesStatus;
  public
    property Layer: TUnitLayer read FLayer;
    property Unit_: TCodeUnit read FUnit;
    property UsesStatus[Index: TUnitSection]: TUsesStatus read GetUsesStatus;
    constructor Create(AUnit: TCodeUnit; ALayers: TApplicationLayers); reintroduce;
  end;

type DS_Units_ForUsesAnalyze = class(TOwningList)
  private
    function GetItems(Index: Integer): VM_Unit_ForUsesAnalyze;
  public
    property Items[Index: Integer]: VM_Unit_ForUsesAnalyze read GetItems; default;
  end;

type VM_Details_Item_ForUsesAnalyze = class
  protected
    function GetCaption: String; virtual; abstract;
    function GetSubItem: String; virtual; abstract;
    function GetImageIndex: Integer; virtual; abstract;
    function GetIndent: Integer; virtual; abstract;
    function GetSubItemColor: TColor; virtual; abstract;
  public
    property Caption: String read GetCaption;
    property SubItem: String read GetSubItem;
    property SubItemColor: TColor read GetSubItemColor;
    property ImageIndex: Integer read GetImageIndex;
    property Indent: Integer read GetIndent;
  end;

type VM_Details_Header_ForUsesAnalyze = class(VM_Details_Item_ForUsesAnalyze)
  private
    FUnitDependence: TUnitDependence;
  protected
    function GetCaption: String; override;
    function GetSubItem: String; override;
    function GetImageIndex: Integer; override;
    function GetIndent: Integer; override;
    function GetSubItemColor: TColor; override;
  public
    constructor Create(AUnitDependence: TUnitDependence); reintroduce;
  end;

type VM_Details_Unit_ForUsesAnalyze = class(VM_Details_Item_ForUsesAnalyze)
  private
    FUnit: TCodeUnit;
    FUsesStatus: TUsesStatus;
    FLayer: TUnitLayer;
  protected
    function GetCaption: String; override;
    function GetSubItem: String; override;
    function GetImageIndex: Integer; override;
    function GetIndent: Integer; override;
    function GetSubItemColor: TColor; override;
  public
    constructor Create(AUnit: TCodeUnit; ALayers: TApplicationLayers); reintroduce;
    property Unit_: TCodeUnit read FUnit;
    property Layer: TUnitLayer read FLayer;
    property UsesStatus: TUsesStatus read FUsesStatus write FUsesStatus;
  end;

type DS_Details_ForUsesAnalyze = class(TOwningList)
  private
    function GetItems(Index: Integer): VM_Details_Item_ForUsesAnalyze;
  public
    property Items[Index: Integer]: VM_Details_Item_ForUsesAnalyze read GetItems; default;
  end;

type rulesUses = class
  public
    class function CanUse(ALayers: TApplicationLayers; AUsingUnit, AUsedUnit: TCodeUnit; AUnitSection: TUnitSection): TUsesStatus;
  end;

type UsesStatusUI = class
  public
    class function Caption(AStatus: TUsesStatus): String;
    class function Color(AStatus: TUsesStatus): TColor;
    class function ImageIndex(AStatus: TUsesStatus): Integer;
  end;

type
  TfrmUsesAnalyze = class(TForm)
    lvUnits: TListView;
    Splitter1: TSplitter;
    lvDetails: TListView;
    Panel1: TPanel;
    btnUpdate: TButton;
    sbStatus: TStatusBar;
    clbStatusFilter: TCheckListBox;
    mnuFilterCLB: TPopupMenu;
    mnuCheckAll: TMenuItem;
    mnuUncheckAll: TMenuItem;
    chkOpenUnitWithDetails: TCheckBox;
    mnuUnits: TPopupMenu;
    mnuDetails: TPopupMenu;
    mnuCopyUnitNames_Units: TMenuItem;
    mnuCopyFullUnitNames_Units: TMenuItem;
    mnuCopyUnitNames_Details: TMenuItem;
    mnuCopyFullUnitNames_Details: TMenuItem;
    N1: TMenuItem;
    mnuOpenUnit_Units: TMenuItem;
    mnuCopyUnitLayerNames_Units: TMenuItem;
    mnuCopyUnitLayerNames_Details: TMenuItem;
    Label1: TLabel;
    clbLayerFilter: TCheckListBox;
    Label2: TLabel;
    Label3: TLabel;
    txtUnitNameFilter: TEdit;
    il16: TImageList;
    procedure btnUpdateClick(Sender: TObject);
    procedure clbFilterDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure frmUsesAnalyzeCreate(Sender: TObject);
    procedure frmUsesAnalyzeDestroy(Sender: TObject);
    procedure lvDetailsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State:
        TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvDetailsData(Sender: TObject; Item: TListItem);
    procedure lvDetailsDblClick(Sender: TObject);
    procedure lvUnitsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState;
        var DefaultDraw: Boolean);
    procedure lvUnitsData(Sender: TObject; Item: TListItem);
    procedure lvUnitsDblClick(Sender: TObject);
    procedure mnuCheckAllClick(Sender: TObject);
    procedure mnuCopyFullUnitNames_DetailsClick(Sender: TObject);
    procedure mnuCopyFullUnitNames_UnitsClick(Sender: TObject);
    procedure mnuCopyUnitLayerNames_DetailsClick(Sender: TObject);
    procedure mnuCopyUnitLayerNames_UnitsClick(Sender: TObject);
    procedure mnuCopyUnitNames_DetailsClick(Sender: TObject);
    procedure mnuCopyUnitNames_UnitsClick(Sender: TObject);
    procedure mnuOpenUnit_UnitsClick(Sender: TObject);
    procedure mnuUncheckAllClick(Sender: TObject);
  private
    FCodeBase: TCodeBase;
    FOpenedUnitID: String;
    FBootstrap: IUsesAnalyzeBootstrap;
    FParseEngine: TSourceParser_CodeBase;
    dsUnits: DS_Units_ForUsesAnalyze;
    dsDetails: DS_Details_ForUsesAnalyze;
    procedure GetDetails(AUnit: TCodeUnit; ADetails: DS_Details_ForUsesAnalyze);
    function SelectedUnit: VM_Unit_ForUsesAnalyze;
    function SelectedDetailItem: VM_Details_Item_ForUsesAnalyze;
  public
    procedure UpdateContents;
    procedure SetBootstrap(ABootstrap: IUsesAnalyzeBootstrap);
  end;

var
  frmUsesAnalyze: TfrmUsesAnalyze;

implementation

{$R *.dfm}

{ TfrmUsesAnalyze }

procedure TfrmUsesAnalyze.GetDetails(AUnit: TCodeUnit; ADetails: DS_Details_ForUsesAnalyze);
var ud: TUnitDependence; lst: TCodeUnits; I: Integer; otherUnit: TCodeUnit;
  vmHeader: VM_Details_Header_ForUsesAnalyze; vmUnit: VM_Details_Unit_ForUsesAnalyze;
begin
ADetails.Clear;

for ud:=Low(TUnitDependence) to High(TUnitDependence) do
  begin
  vmHeader:=VM_Details_Header_ForUsesAnalyze.Create(ud);
  ADetails.Add(vmHeader);

  lst:=AUnit.UnitLinks[ud];

  for i:=0 to lst.Count-1 do
    begin
    otherUnit:=lst[i];

    vmUnit:=VM_Details_Unit_ForUsesAnalyze.Create(otherUnit, FCodeBase.Layers);

    case ud of
    udUsesItf:
      vmUnit.UsesStatus:=rulesUses.CanUse(FCodeBase.Layers, AUnit, otherUnit, usItf);
    udUsesImpl:
      vmUnit.UsesStatus:=rulesUses.CanUse(FCodeBase.Layers, AUnit, otherUnit, usImpl);
    udUsedByItf:
      vmUnit.UsesStatus:=rulesUses.CanUse(FCodeBase.Layers, otherUnit, AUnit, usItf);
    udUsedByImpl:
      vmUnit.UsesStatus:=rulesUses.CanUse(FCodeBase.Layers, otherUnit, AUnit, usImpl);
    end; // case

    ADetails.Add(vmUnit);
    end;
  end;

FOpenedUnitID:=AUnit.ID;  
end;

procedure TfrmUsesAnalyze.UpdateContents;
var statusFilter: set of TUsesStatus; layerFilter: set of byte;
  unitNameFitlter: String;

  function CheckLayerFilter(AViewModel: VM_Unit_ForUsesAnalyze): Boolean;
  begin
  Result:=(AViewModel.Layer.ID in layerFilter)
  end;

  function CheckUsesStatusFilter(AViewModel: VM_Unit_ForUsesAnalyze): Boolean;
  begin
  Result:=((AViewModel.UsesStatus[usItf] in statusFilter)
        or (AViewModel.UsesStatus[usImpl] in statusFilter))
  end;

  function CheckNameFilter(AViewModel: VM_Unit_ForUsesAnalyze): Boolean;
  begin
  Result:=(unitNameFitlter='') or (Pos(unitNameFitlter, AViewModel.Unit_.ID)<>0)
  end;

  function CheckFilters(AViewModel: VM_Unit_ForUsesAnalyze): Boolean;
  begin
  Result:=CheckNameFilter(AViewModel)
      and CheckLayerFilter(AViewModel)
      and CheckUsesStatusFilter(AViewModel)
  end;

var I: Integer; unit_: TCodeUnit; vm: VM_Unit_ForUsesAnalyze; us: TUsesStatus;
begin
lvUnits.Items.Count:=0;
lvDetails.Items.Count:=0;

dsUnits.Clear;
dsDetails.Clear;

FParseEngine.FullRebuild;

unitNameFitlter:=AnsiUpperCase(txtUnitNameFilter.Text);

statusFilter:=[];
i:=0;
for us:=Low(TUsesStatus) to High(TUsesStatus) do
  begin
  if clbStatusFilter.Checked[i] then statusFilter:=statusFilter+[us];
  Inc(i);
  end;

layerFilter:=[];
for i:=0 to FCodeBase.Layers.Count-1 do
  if clbLayerFilter.Checked[i] then
    layerFilter:=layerFilter+[FCodeBase.Layers[i].ID];

for i:=0 to FCodeBase.Units.Count-1 do
  begin
  unit_:=FCodeBase.Units[i];
  vm:=VM_Unit_ForUsesAnalyze.Create(unit_, FCodeBase.Layers);

  if CheckFilters(vm) then
    dsUnits.Add(vm)
  else
    FreeAndNIL(vm);
  end;

lvUnits.Items.Count:=dsUnits.Count;
lvUnits.Invalidate;

if FOpenedUnitID<>'' then
	begin
  unit_:=TCodeUnit(FCodeBase.Units.ByUnitID(FOpenedUnitID));
  if unit_<>NIL then
		begin
    GetDetails(unit_, dsDetails);
		lvDetails.Items.Count:=dsDetails.Count;
		lvDetails.Invalidate;
		end;
	end;

sbStatus.Panels[0].Text:=IntToStr(dsUnits.Count)+' unit(s)'
end;

function TfrmUsesAnalyze.SelectedDetailItem: VM_Details_Item_ForUsesAnalyze;
begin
if lvDetails.Selected=NIL then
  Result:=NIL
else
  Result:=dsDetails[lvDetails.Selected.Index]
end;

function TfrmUsesAnalyze.SelectedUnit: VM_Unit_ForUsesAnalyze;
begin
if lvUnits.Selected=NIL then
  Result:=NIL
else
  Result:=dsUnits[lvUnits.Selected.Index]
end;

procedure TfrmUsesAnalyze.SetBootstrap(ABootstrap: IUsesAnalyzeBootstrap);
var us: TUsesStatus; I: Integer;
begin
FBootstrap:=ABootstrap;

FCodeBase:=TCodeBase.Create;

FParseEngine:=TSourceParser_CodeBase.Create(FBootstrap.FileMappers, FBootstrap.GetUnitNames, FCodeBase);
FParseEngine.OnParseError:=FBootstrap.OnParseError();

for us:=Low(TUsesStatus) to High(TUsesStatus) do
  begin
  i:=clbStatusFilter.Items.Add(UsesStatusUI.Caption(us));
  clbStatusFilter.Checked[i]:=True;
  end;

for i:=0 to FCodeBase.Layers.Count-1 do
  begin
  clbLayerFilter.Items.Add(FCodeBase.Layers[i].Caption);
  clbLayerFilter.Checked[i]:=True;
  end;
end;

{ VM_Unit_ForUsesAnalyze }

constructor VM_Unit_ForUsesAnalyze.Create(AUnit: TCodeUnit; ALayers: TApplicationLayers);

  function Min(AStatus1, AStatus2: TUsesStatus): TUsesStatus;
  begin
  if AStatus1<AStatus2 then
    Result:=AStatus1
  else
    Result:=AStatus2;
  end;

var I: Integer; us: TUnitSection; otherUnit: TCodeUnit; lst: TCodeUnits;
begin
inherited Create;
FUnit:=AUnit;
FLayer:=ALayers.ByID(FUnit.Layer);

for us:=Low(TUnitSection) to High(TUnitSection) do
  begin
  FUsesStatus[us]:=High(TUsesStatus);
  lst:=AUnit.UnitLinks[UnitSection2Uses(us)];

  for i:=0 to lst.Count-1 do
    begin
    otherUnit:=lst[i];
    FUsesStatus[us]:=Min(FUsesStatus[us], rulesUses.CanUse(ALayers, AUnit, otherUnit, us));
    if FUsesStatus[us]=Low(TUsesStatus) then break;
    end;
  end;
end;

function VM_Unit_ForUsesAnalyze.GetUsesStatus(Index: TUnitSection): TUsesStatus;
begin
Result:=FUsesStatus[Index]
end;

{ rulesUses }

class function rulesUses.CanUse(ALayers: TApplicationLayers; AUsingUnit, AUsedUnit: TCodeUnit; AUnitSection:
    TUnitSection): TUsesStatus;
begin
Result:=ALayers.ByID(AUsingUnit.Layer).UsesStatus[AUnitSection][AUsedUnit.Layer];

if Result<>sOK then
  begin
  if AUsingUnit.CanUse(AUsedUnit, AUnitSection) then
    Result:=sOK
  end;
end;

{ DS_Units_ForUsesAnalyze }

function DS_Units_ForUsesAnalyze.GetItems(Index: Integer): VM_Unit_ForUsesAnalyze;
begin
Result:=VM_Unit_ForUsesAnalyze(inherited Items[Index])
end;

procedure TfrmUsesAnalyze.btnUpdateClick(Sender: TObject);
begin
UpdateContents
end;

procedure TfrmUsesAnalyze.clbFilterDblClick(Sender: TObject);
var I, idx: Integer; clb: TCheckListBox;
begin
clb:=TCheckListBox(Sender);
idx:=clb.ItemIndex;
for i:=0 to clb.Items.Count-1 do
  clb.Checked[i]:=i=idx
end;

procedure TfrmUsesAnalyze.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action:=caFree
end;

procedure TfrmUsesAnalyze.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
if Key=VK_F5 then
  UpdateContents
end;

procedure TfrmUsesAnalyze.FormShow(Sender: TObject);
begin
CheckSecondMonitor(Self);
WindowState:=wsMaximized
end;

procedure TfrmUsesAnalyze.frmUsesAnalyzeCreate(Sender: TObject);
begin
dsUnits:=DS_Units_ForUsesAnalyze.Create(True);
dsDetails:=DS_Details_ForUsesAnalyze.Create(True);
end;

procedure TfrmUsesAnalyze.frmUsesAnalyzeDestroy(Sender: TObject);
begin
if Assigned(dsDetails) then FreeAndNIL(dsDetails);
if Assigned(dsUnits) then FreeAndNIL(dsUnits);

if Assigned(FParseEngine) then FreeAndNIL(FParseEngine);
if Assigned(FCodeBase) then FreeAndNIL(FCodeBase);
if Assigned(FBootstrap) then FreeAndNIL(FBootstrap);
end;

procedure TfrmUsesAnalyze.lvDetailsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State:
    TCustomDrawState; var DefaultDraw: Boolean);
var vm: VM_Details_Item_ForUsesAnalyze;
begin
if SubItem=1 then
	begin
  vm:=dsDetails[Item.Index];
  Sender.Canvas.Brush.Color:=vm.SubItemColor
	end;
end;

procedure TfrmUsesAnalyze.lvDetailsData(Sender: TObject; Item: TListItem);
var vm: VM_Details_Item_ForUsesAnalyze;
begin
vm:=dsDetails[Item.Index];
Item.Caption:=vm.Caption;
Item.SubItems.Add(vm.SubItem);
Item.ImageIndex:=vm.ImageIndex;
Item.Indent:=vm.Indent;
end;

procedure TfrmUsesAnalyze.lvDetailsDblClick(Sender: TObject);
var vm: VM_Details_Item_ForUsesAnalyze;
begin
vm:=SelectedDetailItem;
if Assigned(vm) and (vm is VM_Details_Unit_ForUsesAnalyze) then
  FBootstrap.OpenUnit(VM_Details_Unit_ForUsesAnalyze(vm).Unit_.FileName, 1);
end;

procedure TfrmUsesAnalyze.lvUnitsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State:
    TCustomDrawState; var DefaultDraw: Boolean);
var vm: VM_Unit_ForUsesAnalyze;
begin
vm:=dsUnits[Item.Index];
case SubItem of
1: Sender.Canvas.Brush.Color:=vm.Layer.Color;
2: Sender.Canvas.Brush.Color:=UsesStatusUI.Color(vm.UsesStatus[usItf]);
3: Sender.Canvas.Brush.Color:=UsesStatusUI.Color(vm.UsesStatus[usImpl]);
end; // case
end;

procedure TfrmUsesAnalyze.lvUnitsData(Sender: TObject; Item: TListItem);
var vm: VM_Unit_ForUsesAnalyze;
begin
vm:=dsUnits[Item.Index];
Item.Caption:=vm.Unit_.Caption(True);
Item.SubItems.Add(vm.Layer.Caption);
Item.SubItems.Add(Format('%d unit(s)', [vm.Unit_.UnitLinks[UnitSection2Uses(usItf)].Count]));
Item.SubItems.Add(Format('%d unit(s)', [vm.Unit_.UnitLinks[UnitSection2Uses(usImpl)].Count]));
Item.ImageIndex:=iiUnit;
end;

procedure TfrmUsesAnalyze.lvUnitsDblClick(Sender: TObject);
var vm: VM_Unit_ForUsesAnalyze;
begin
vm:=SelectedUnit;
if Assigned(vm) then
	begin
	GetDetails(vm.Unit_, dsDetails);
	lvDetails.Items.Count:=dsDetails.Count;
	lvDetails.Invalidate;

  if chkOpenUnitWithDetails.Checked then
    FBootstrap.OpenUnit(vm.Unit_.FileName, 1);
	end;
end;

procedure TfrmUsesAnalyze.mnuCheckAllClick(Sender: TObject);
var I: Integer; clb: TCheckListBox;
begin
clb:=TCheckListBox(mnuFilterCLB.PopupComponent);
for i:=0 to clb.Items.Count-1 do
  clb.Checked[i]:=True
end;

procedure TfrmUsesAnalyze.mnuCopyFullUnitNames_DetailsClick(Sender: TObject);
var I: Integer; vm: VM_Details_Item_ForUsesAnalyze; S: String;
begin
S:='';
for i:=0 to dsDetails.Count-1 do
  if lvDetails.Items[i].Selected then
    begin
    vm:=dsDetails[i];
    if not (vm is VM_Details_Unit_ForUsesAnalyze) then continue;
    S:=S+VM_Details_Unit_ForUsesAnalyze(vm).Unit_.FileName+#13#10
    end;
StrToClipbrd(S);
end;

procedure TfrmUsesAnalyze.mnuCopyFullUnitNames_UnitsClick(Sender: TObject);
var I: Integer; vm: VM_Unit_ForUsesAnalyze; S: String;
begin
S:='';
for i:=0 to dsUnits.Count-1 do
  if lvUnits.Items[i].Selected then
    begin
    vm:=dsUnits[i];
    S:=S+vm.Unit_.FileName+#13#10
    end;
StrToClipbrd(S);
end;

procedure TfrmUsesAnalyze.mnuCopyUnitLayerNames_DetailsClick(Sender: TObject);
var I: Integer; vm: VM_Details_Item_ForUsesAnalyze; S: String;
  vmUnit: VM_Details_Unit_ForUsesAnalyze;
begin
S:='';
for i:=0 to dsDetails.Count-1 do
  if lvDetails.Items[i].Selected then
    begin
    vm:=dsDetails[i];
    if not (vm is VM_Details_Unit_ForUsesAnalyze) then continue;

    vmUnit:=VM_Details_Unit_ForUsesAnalyze(vm);
    S:=S+vmUnit.Unit_.Caption(True)+#9+vmUnit.Layer.Caption+#13#10
    end;
StrToClipbrd(S);
end;

procedure TfrmUsesAnalyze.mnuCopyUnitLayerNames_UnitsClick(Sender: TObject);
var I: Integer; vm: VM_Unit_ForUsesAnalyze; S: String;
begin
S:='';
for i:=0 to dsUnits.Count-1 do
  if lvUnits.Items[i].Selected then
    begin
    vm:=dsUnits[i];
    S:=S+vm.Unit_.Caption(True)+#9+vm.Layer.Caption+#13#10
    end;
StrToClipbrd(S);
end;

procedure TfrmUsesAnalyze.mnuCopyUnitNames_DetailsClick(Sender: TObject);
var I: Integer; vm: VM_Details_Item_ForUsesAnalyze; S: String;
begin
S:='';
for i:=0 to dsDetails.Count-1 do
  if lvDetails.Items[i].Selected then
    begin
    vm:=dsDetails[i];
    if not (vm is VM_Details_Unit_ForUsesAnalyze) then continue;
    S:=S+VM_Details_Unit_ForUsesAnalyze(vm).Unit_.Caption(True)+#13#10
    end;
StrToClipbrd(S);
end;

procedure TfrmUsesAnalyze.mnuCopyUnitNames_UnitsClick(Sender: TObject);
var I: Integer; vm: VM_Unit_ForUsesAnalyze; S: String;
begin
S:='';
for i:=0 to dsUnits.Count-1 do
  if lvUnits.Items[i].Selected then
    begin
    vm:=dsUnits[i];
    S:=S+vm.Unit_.Caption(True)+#13#10
    end;
StrToClipbrd(S);
end;

procedure TfrmUsesAnalyze.mnuOpenUnit_UnitsClick(Sender: TObject);
var vm: VM_Unit_ForUsesAnalyze;
begin
vm:=SelectedUnit;
if Assigned(vm) then
  FBootstrap.OpenUnit(vm.Unit_.FileName, 1);
end;

procedure TfrmUsesAnalyze.mnuUncheckAllClick(Sender: TObject);
var I: Integer; clb: TCheckListBox;
begin
clb:=TCheckListBox(mnuFilterCLB.PopupComponent);
for i:=0 to clb.Items.Count-1 do
  clb.Checked[i]:=False
end;

{ VM_Details_Unit_ForUsesAnalyze }

constructor VM_Details_Unit_ForUsesAnalyze.Create(AUnit: TCodeUnit; ALayers: TApplicationLayers);
begin
inherited Create;
FUnit:=AUnit;
FLayer:=ALayers.ByID(FUnit.Layer);
FUsesStatus:=sNotResolved;
end;

function VM_Details_Unit_ForUsesAnalyze.GetCaption: String;
begin
Result:=Unit_.Caption(True)
end;

function VM_Details_Unit_ForUsesAnalyze.GetImageIndex: Integer;
begin
Result:=UsesStatusUI.ImageIndex(FUsesStatus)
end;

function VM_Details_Unit_ForUsesAnalyze.GetIndent: Integer;
begin
Result:=2;
end;

function VM_Details_Unit_ForUsesAnalyze.GetSubItem: String;
begin
Result:=FLayer.Caption
end;

function VM_Details_Unit_ForUsesAnalyze.GetSubItemColor: TColor;
begin
Result:=FLayer.Color
end;

{ VM_Details_Header_ForUsesAnalyze }

constructor VM_Details_Header_ForUsesAnalyze.Create(AUnitDependence: TUnitDependence);
begin
inherited Create;
FUnitDependence:=AUnitDependence;
end;

function VM_Details_Header_ForUsesAnalyze.GetCaption: String;
const Captions: array[TUnitDependence] of String = ('Uses (in interface)',
  'Uses (in implementation)', 'Used by (from interface)', 'Used by (from implementation)');
begin
Result:=Captions[FUnitDependence];
end;

function VM_Details_Header_ForUsesAnalyze.GetImageIndex: Integer;
begin
Result:=iiFolder
end;

function VM_Details_Header_ForUsesAnalyze.GetIndent: Integer;
begin
Result:=0;
end;

function VM_Details_Header_ForUsesAnalyze.GetSubItem: String;
begin
Result:='';
end;

function VM_Details_Header_ForUsesAnalyze.GetSubItemColor: TColor;
begin
Result:=clWindow
end;

{ DS_Details_ForUsesAnalyze }

function DS_Details_ForUsesAnalyze.GetItems(Index: Integer): VM_Details_Item_ForUsesAnalyze;
begin
Result:=VM_Details_Item_ForUsesAnalyze(inherited Items[Index])
end;

{ UsesStatusUI }

class function UsesStatusUI.Caption(AStatus: TUsesStatus): String;
const Captions: array[TUsesStatus] of String = ('Error', 'Not Resolved', 'OK', 'No Units');
begin
Result:=Captions[AStatus]
end;

class function UsesStatusUI.Color(AStatus: TUsesStatus): TColor;
const Colors: array[TUsesStatus] of TColor = (clRed, clYellow, clLime, clWhite);
begin
Result:=Colors[AStatus]
end;

class function UsesStatusUI.ImageIndex(AStatus: TUsesStatus): Integer;
const ImageIndexes: array[TUsesStatus] of Integer = (iiError, iiNotResolved, iiOK, -1);
begin
Result:=ImageIndexes[AStatus]
end;

end.
