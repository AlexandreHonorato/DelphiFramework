unit uCallStackAnalyzeFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uCallStack_Domain, uStreamParser_CallStack,
  ExtCtrls, ComCtrls, uFileMappers, _TreeView_VCL, ImgList,
  uSourceParser_CallStack, uCallStackAnalyzeBootstrap_Itf,
  uCallStack_ViewModels, _VCL_ZZZ, Menus, _Strings;

type
  TfrmCallStackAnalyze = class(TForm)
    mnuListView: TPopupMenu;
    mnuExclude: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lvMethods: TListView;
    TabSheet2: TTabSheet;
    lvCallStack: TListView;
    Panel1: TPanel;
    Button2: TButton;
    btnExclude: TButton;
    chkShowErrorsOnly: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    txtErrorStack: TEdit;
    btnParseErrorStack: TButton;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Label2: TLabel;
    lbDuplicateErrorIDs: TListBox;
    il16: TImageList;
    procedure btnExcludeClick(Sender: TObject);
    procedure btnParseErrorStackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure frmCallStackAnalyzeDestroy(Sender: TObject);
    procedure lvCallStackDblClick(Sender: TObject);
    procedure lvMethodsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw:
        Boolean);
    procedure lvMethodsData(Sender: TObject; Item: TListItem);
    procedure lvMethodsDblClick(Sender: TObject);
    procedure mnuListViewPopup(Sender: TObject);
  private
    FDataSource: DS_cs_Items;
    FUnits: TcsUnitList;
    FCallStackNames: TStrings;
    FErrorIdIndex: TcsErrorIdIndex;
    FParser: TSourceParser_CallStack;
    FBootstrap: ICallStackAnalyzeBootstrap;
    procedure BuildViewModels(ADomainModels: TcsUnitList; AViewModels: DS_cs_Items;
      AErrorsOnly: Boolean);
  public
    procedure SetBootstrap(ABootstrap: ICallStackAnalyzeBootstrap);
    procedure UpdateContents;
  end;

var
  frmCallStackAnalyze: TfrmCallStackAnalyze;

implementation

{$R *.dfm}

procedure TfrmCallStackAnalyze.FormCreate(Sender: TObject);
begin
FDataSource:=DS_cs_Items.Create(True);
end;

procedure TfrmCallStackAnalyze.Button2Click(Sender: TObject);
begin
UpdateContents;
end;

procedure TfrmCallStackAnalyze.UpdateContents;
var I: Integer;
begin
FErrorIdIndex.Clear;
FUnits.Clear;
FParser.Parse;

BuildViewModels(FUnits, FDataSource, chkShowErrorsOnly.Checked);
lvMethods.Items.Count:=FDataSource.Count;
lvMethods.Invalidate;

lbDuplicateErrorIDs.Items.Clear;
for i:=0 to FErrorIdIndex.DuplicateCommands.Count-1 do
  lbDuplicateErrorIDs.Items.Add(FErrorIdIndex.DuplicateCommands[i].ErrorID);
if lbDuplicateErrorIDs.Items.Count=0 then
  lbDuplicateErrorIDs.Color:=clWindow
else
  lbDuplicateErrorIDs.Color:=$9999FF
end;

procedure TfrmCallStackAnalyze.frmCallStackAnalyzeDestroy(Sender: TObject);
begin
if Assigned(FErrorIdIndex) then FreeAndNIL(FErrorIdIndex);
if Assigned(FParser) then FreeAndNIL(FParser);
if Assigned(FDataSource) then FreeAndNIL(FDataSource);
if Assigned(FUnits) then FreeAndNIL(FUnits);
if Assigned(FCallStackNames) then FreeAndNIL(FCallStackNames);
if Assigned(FBootstrap) then FreeAndNIL(FBootstrap);
end;

procedure TfrmCallStackAnalyze.SetBootstrap(ABootstrap: ICallStackAnalyzeBootstrap);
var csaFileName: String;
begin
FBootstrap:=ABootstrap;

FUnits:=TcsUnitList.Create(True);
FErrorIdIndex:=TcsErrorIdIndex.Create;

FCallStackNames:=TStringList.Create;
csaFileName:=FBootstrap.CSA_Params_FileName;
if FileExists(csaFileName) then
  FCallStackNames.LoadFromFile(csaFileName);
if FCallStackNames.Count=0 then FCallStackNames.Add('cs');
  
FParser:=TSourceParser_CallStack.Create(FBootstrap.FileMappers, FBootstrap.GetUnitNames, FUnits, FErrorIdIndex, FCallStackNames);
end;

procedure TfrmCallStackAnalyze.BuildViewModels(ADomainModels: TcsUnitList; AViewModels: DS_cs_Items; AErrorsOnly:
    Boolean);

  procedure AddMethods(AMethods: TcsMethodList);
  var I: Integer; method: TcsMethod;
  begin
  for i:=0 to AMethods.Count-1 do
    begin
    method:=AMethods[i];

    if AErrorsOnly and (not method.HasErrors(True)) then Continue;

    AViewModels.Add(VM_cs_Method.Create(method));
    AddMethods(method.NestedMethods);
    end;
  end;

var unit_: TcsUnit; I: Integer;
begin
AViewModels.Clear;

for i:=0 to ADomainModels.Count-1 do
  begin
  unit_:=ADomainModels[i];
  CallStackRules.CheckUnit(unit_);

  if AErrorsOnly and (not unit_.HasErrors) then Continue;

  AViewModels.Add(VM_cs_Unit.Create(unit_));
  AddMethods(unit_.Methods);
  end;
end;

procedure TfrmCallStackAnalyze.btnExcludeClick(Sender: TObject);
var vm: VM_cs_Item; vmMethod: VM_cs_Method; exclude: TcsMethodsIndex;
begin
if lvMethods.Selected=NIL then exit;

vm:=FDataSource[lvMethods.Selected.Index];
if vm is VM_cs_Method then
  begin
  vmMethod:=VM_cs_Method(vm);
  
  exclude:=TcsMethodsIndex.Create(vmMethod.Method.Unit_);
  try
    exclude.AddMethod(vmMethod.Method);
    exclude.Save;
  finally
    FreeAndNIL(exclude);
  end; // try
  end;
end;

procedure TfrmCallStackAnalyze.btnParseErrorStackClick(Sender: TObject);
var S: String; errorID: String; p, i: Integer; lstErrorIDs: TStrings;
  li: TListItem; command: TcsCommand;
begin
lstErrorIDs:=TStringList.Create;
try
  S:=txtErrorStack.Text;
  p:=Pos('->', S);
  while p<>0 do
    begin
    errorID:=copy(S, 1, p-1);
    lstErrorIDs.Add(errorID);
    S:=StrTail(S, p+1);
    p:=Pos('->', S);
    end;
  lstErrorIDs.Add(S);

  lvCallStack.Items.Clear;
  for i:=0 to lstErrorIDs.Count-1 do
    begin
    errorID:=lstErrorIDs[i];
    li:=lvCallStack.Items.Add;
    li.Caption:=errorID;

    command:=FErrorIdIndex.ByErrorID(lstErrorIDs[i]);
    li.Data:=command;
    if Assigned(command) then
      begin
      li.SubItems.Add(command.method.ShortName);
      li.SubItems.Add(command.method.Unit_.Caption(True));
      li.ImageIndex:=iiErrorID;
      end
    else
      begin
      li.SubItems.Add('(not found)');
      li.ImageIndex:=iiError;
      end;
    end;
finally
  if Assigned(lstErrorIDs) then FreeAndNIL(lstErrorIDs);
end; // try
end;

procedure TfrmCallStackAnalyze.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action:=caFree
end;

procedure TfrmCallStackAnalyze.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
if Key=VK_F6 then
  btnExcludeClick(NIL)
end;

procedure TfrmCallStackAnalyze.FormShow(Sender: TObject);
begin
CheckSecondMonitor(Self);
WindowState:=wsMaximized
end;

procedure TfrmCallStackAnalyze.lvCallStackDblClick(Sender: TObject);
var command: TcsCommand;
begin
if (not Assigned(lvCallStack.Selected)) or (not Assigned(lvCallStack.Selected.Data)) then exit;

command:=TcsCommand(lvCallStack.Selected.Data);
FBootstrap.OpenErrorID(command);
end;

procedure TfrmCallStackAnalyze.lvMethodsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State:
    TCustomDrawState; var DefaultDraw: Boolean);
var vm: VM_cs_Item;
begin
vm:=FDataSource[Item.Index];
Sender.Canvas.Font.Style:=vm.FontStyle
end;

procedure TfrmCallStackAnalyze.lvMethodsData(Sender: TObject; Item: TListItem);
var vm: VM_cs_Item;
begin
vm:=FDataSource[Item.Index];
Item.Caption:=vm.Caption;
Item.SubItems.Add(vm.SubItem);
Item.ImageIndex:=vm.ImageIndex;
Item.Indent:=vm.Indent;
end;

procedure TfrmCallStackAnalyze.lvMethodsDblClick(Sender: TObject);
var vm: VM_cs_Item;
begin
if lvMethods.Selected=NIL then exit;

vm:=FDataSource[lvMethods.Selected.Index];
if vm is VM_cs_Unit then
  FBootstrap.OpenUnit(VM_cs_Unit(vm).Unit_)
else
if vm is VM_cs_Method then
  FBootstrap.OpenMethod(VM_cs_Method(vm).Method);
end;

procedure TfrmCallStackAnalyze.mnuListViewPopup(Sender: TObject);
begin
mnuExclude.Enabled:=(lvMethods.Selected<>NIL) and (FDataSource[lvMethods.Selected.Index] is VM_cs_Method);
end;

end.
