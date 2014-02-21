unit uFrameWorkUnitsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, _Lists;

type VM_Unit_ForEditUnits = class
  public
    Caption: String;
    Path: String;
    Exists: Boolean;
    function ImageIndex: Integer;
    function FileName: String;
  end;

type
  TdlgFrameWorkUnits = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    Label1: TLabel;
    lvUnits: TListView;
    btnDeleteInvalid: TButton;
    btnDeleteSelected: TButton;
    btnAdd: TButton;
    dlgSelect: TOpenDialog;
    lblCount: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteInvalidClick(Sender: TObject);
    procedure btnDeleteSelectedClick(Sender: TObject);
    procedure CheckBtnOK(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dlgFrameWorkUnitsCreate(Sender: TObject);
    procedure dlgFrameWorkUnitsDestroy(Sender: TObject);
    procedure lvUnitsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw:
        Boolean);
    procedure lvUnitsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState;
        var DefaultDraw: Boolean);
    procedure lvUnitsData(Sender: TObject; Item: TListItem);
  private
    function GetViewModels(Index: Integer): VM_Unit_ForEditUnits;
    procedure UpdateUI;
    procedure AddViewModel(const AFileName: String);
  private
    FViewModels: TOwningList;
    property ViewModels[Index: Integer]: VM_Unit_ForEditUnits read GetViewModels;
    procedure SortViewModels;
  public
    function Edit(AUnitNames: TStrings): Boolean;
  end;

var
  dlgFrameWorkUnits: TdlgFrameWorkUnits;

implementation

uses uUI;

{$R *.DFM}

procedure TdlgFrameWorkUnits.btnDeleteInvalidClick(Sender: TObject);
var I, cnt: Integer; vm: VM_Unit_ForEditUnits;
begin
cnt:=0;
for i:=0 to FViewModels.Count-1 do
  if not ViewModels[i].Exists then
    Inc(cnt);

if cnt=0 then
  begin
  MessageBox(Handle, 'There are no invalid filenames', 'Delete invalid filenames', MB_ICONINFORMATION);
  exit;
  end
else
  begin
  if MessageBox(Handle, PChar(Format('Delete invalid filenames (%d)?', [cnt])),
    'Delete invalid filenames', MB_ICONQUESTION or MB_OKCANCEL)<>IDOK then exit;
  end;

for i:=FViewModels.Count-1 downto 0 do
  begin
  vm:=ViewModels[i];
  if not vm.Exists then
    begin
    FViewModels.Delete(i);
    FreeAndNIL(vm);
    end;
  end;

UpdateUI;
end;

procedure TdlgFrameWorkUnits.CheckBtnOK(Sender: TObject);
begin
btnOK.Enabled:=True;
end;

procedure TdlgFrameWorkUnits.AddViewModel(const AFileName: String);
var vm: VM_Unit_ForEditUnits;
begin
vm:=VM_Unit_ForEditUnits.Create;
vm.Caption:=ExtractFileName(AFileName);
vm.Path:=ExtractFilePath(AFileName);
vm.Exists:=FileExists(AFileName);
FViewModels.Add(vm);
end;

function TdlgFrameWorkUnits.Edit(AUnitNames: TStrings): Boolean;
var I: Integer;
begin
for i:=0 to AUnitNames.Count-1 do
  AddViewModel(AUnitNames[i]);

SortViewModels;
UpdateUI;

Result:=ShowModal=mrOK;

if Result then
  begin
  SortViewModels;
  AUnitNames.Clear;
  for i:=0 to FViewModels.Count-1 do
    AUnitNames.Add(ViewModels[i].FileName)
  end;
end;

procedure TdlgFrameWorkUnits.FormShow(Sender: TObject);
begin
CheckBtnOK(NIL)
end;

procedure TdlgFrameWorkUnits.dlgFrameWorkUnitsCreate(Sender: TObject);
begin
FViewModels:=TOwningList.Create(True);
lvUnits.SmallImages:=UI.il16;
end;

procedure TdlgFrameWorkUnits.dlgFrameWorkUnitsDestroy(Sender: TObject);
begin
if Assigned(FViewModels) then FreeAndNIL(FViewModels);
end;

procedure TdlgFrameWorkUnits.lvUnitsData(Sender: TObject; Item: TListItem);
var vm: VM_Unit_ForEditUnits;
begin
vm:=ViewModels[Item.Index];
Item.Caption:=vm.Caption;
Item.SubItems.Add(vm.Path);
Item.ImageIndex:=vm.ImageIndex;
end;

function TdlgFrameWorkUnits.GetViewModels(Index: Integer): VM_Unit_ForEditUnits;
begin
Result:=VM_Unit_ForEditUnits(FViewModels[Index])
end;

procedure TdlgFrameWorkUnits.lvUnitsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
    var DefaultDraw: Boolean);
var vm: VM_Unit_ForEditUnits; clr: TColor;
begin
vm:=ViewModels[Item.Index];
if vm.ImageIndex=iiUnitGray then
  clr:=clGray
else
  clr:=clBlack;
Sender.Canvas.Font.Color:=clr;
end;

procedure TdlgFrameWorkUnits.lvUnitsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;
    State: TCustomDrawState; var DefaultDraw: Boolean);
var vm: VM_Unit_ForEditUnits; clr: TColor;
begin
vm:=ViewModels[Item.Index];
if vm.ImageIndex=iiUnitGray then
  clr:=clGray
else
  clr:=clBlack;
Sender.Canvas.Font.Color:=clr;
end;

{ VM_Unit_ForEditUnits }

function VM_Unit_ForEditUnits.FileName: String;
begin
Result:=Path+Caption;
end;

function VM_Unit_ForEditUnits.ImageIndex: Integer;
begin
if Exists then
  Result:=iiUnit
else
  Result:=iiUnitGray;
end;

procedure TdlgFrameWorkUnits.btnAddClick(Sender: TObject);
var I: Integer;
begin
if not dlgSelect.Execute then exit;

for i:=0 to dlgSelect.Files.Count-1 do
  AddViewModel(dlgSelect.Files[i]);

UpdateUI;
end;

procedure TdlgFrameWorkUnits.btnDeleteSelectedClick(Sender: TObject);
var I: Integer; vm: VM_Unit_ForEditUnits;
begin
if lvUnits.SelCount=0 then
  begin
  MessageBox(Handle, 'There are no selected filenames', 'Delete selected filenames', MB_ICONINFORMATION);
  exit;
  end  
else
	begin
	if MessageBox(Handle, PChar(Format('Delete selected filenames (%d)?', [lvUnits.SelCount])),
	  'Delete selected filenames', MB_ICONQUESTION or MB_OKCANCEL)<>IDOK then exit;
	end;

for i:=FViewModels.Count-1 downto 0 do
  if lvUnits.Items[i].Selected then
    begin
    vm:=ViewModels[i];
    FViewModels.Delete(i);
    FreeAndNIL(vm);
    end;

UpdateUI;
end;

procedure TdlgFrameWorkUnits.UpdateUI;
begin
lvUnits.Items.Count:=FViewModels.Count;
lvUnits.ClearSelection;
lvUnits.Invalidate;

lblCount.Caption:=Format('%d unit(s)', [FViewModels.Count]);
end;

function SortByCaption(Item1, Item2: Pointer): Integer;
begin
Result:=AnsiCompareText(VM_Unit_ForEditUnits(Item1).Caption, VM_Unit_ForEditUnits(Item2).Caption)
end;

procedure TdlgFrameWorkUnits.SortViewModels;
begin
FViewModels.Sort(SortByCaption);
end;

end.

