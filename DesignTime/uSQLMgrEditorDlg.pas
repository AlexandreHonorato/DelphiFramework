unit uSQLMgrEditorDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, uAddEditSQLDlg, ImgList, _ListView_VCL_ZZZ,
  _DBSQL,
  DesignEditors, DesignIntf, uAddMultipleSQLsDlg, _Misc;

type
  TfrmSQLMgrEditor = class(TForm)
    lvMain: TListView;
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    ImageList1: TImageList;
    btnAddMultiple: TButton;
    GroupBox1: TGroupBox;
    txtFilter: TEdit;
    btnApplyFilter: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure lvMainSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvMainKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvMainDblClick(Sender: TObject);
    procedure btnAddMultipleClick(Sender: TObject);
    procedure lvMainResize(Sender: TObject);
    procedure frmSQLMgrEditorCreate(Sender: TObject);
    procedure frmSQLMgrEditorDestroy(Sender: TObject);
    procedure lvMainColumnClick(Sender: TObject; Column: TListColumn);
    procedure btnApplyFilterClick(Sender: TObject);
    procedure txtFilterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FSQLs: TStringList;
    frmAddEditSQL: TfrmAddEditSQL;
    function IDExists(AID: Integer; const ACaption: string): Boolean;
    function GetMaxID: Integer;
    procedure UpdateLV(const AFilter: String);
  public
    function ShowModalEx(AStrings: TStrings): Integer;
  end;

type
  TSQLMgrEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

{$R *.dfm}

procedure Register;
begin
RegisterComponentEditor(TDataSetSQLMgr, TSQLMgrEditor);
end;

procedure TfrmSQLMgrEditor.btnAddClick(Sender: TObject);
label 1;
var ID: Integer; S: String; LI: TListItem;
begin
ID:=GetMaxID;
S:='';

if not Assigned(frmAddEditSQL) then frmAddEditSQL:=TfrmAddEditSQL.Create(Self);
1: if frmAddEditSQL.ShowModalEx(False, ID, S)<>mrOK then exit;

if IDExists(ID, 'Add SQL') then goto 1;

LI:=lvMain.Items.Add;
LI.Caption:=IntToStr(ID);
LI.SubItems.Add(S);
LI.Data:=Pointer(FSQLs.AddObject(S, TObject(ID)));
ShowItem(LI);
end;

procedure TfrmSQLMgrEditor.lvMainSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
btnEdit.Enabled:=lvMain.SelCount=1;
btnDelete.Enabled:=lvMain.SelCount>0;
end;

procedure TfrmSQLMgrEditor.btnEditClick(Sender: TObject);
label 1;
var PrimaryID, ID, Idx: Integer; S: String;
begin
ID:=StrToInt(lvMain.Selected.Caption);
PrimaryID:=ID;
S:=lvMain.Selected.SubItems[0];

if not Assigned(frmAddEditSQL) then frmAddEditSQL:=TfrmAddEditSQL.Create(Self);
1: if frmAddEditSQL.ShowModalEx(True, ID, S)<>mrOK then exit;

if (ID<>PrimaryID) and IDExists(ID, 'Edit SQL') then goto 1; { TODO : здесь можно через while без goto}

Idx:=Integer(lvMain.Selected.Data);
FSQLs[Idx]:=S;
FSQLs.Objects[Idx]:=TObject(ID);

lvMain.Selected.SubItems[0]:=S;
lvMain.Selected.Caption:=IntToStr(ID);
end;

procedure TfrmSQLMgrEditor.btnDeleteClick(Sender: TObject);
var I, Idx: Integer;
begin
if MessageBox(Handle, 'Delete selected items?', 'Confirmation', MB_ICONQUESTION or MB_OKCANCEL)<>IDOK then exit;

for i:=lvMain.Items.Count-1 downto 0 do
  if lvMain.Items[i].Selected then
    begin
    Idx:=Integer(lvMain.Items[i].Data);
    lvMain.Items.Delete(i);
    FSQLs.Delete(Idx);
    end;
end;

procedure TfrmSQLMgrEditor.lvMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
case Key of
VK_RETURN: if btnEdit.Enabled then btnEditClick(NIL);
VK_INSERT: btnAddClick(NIL);
VK_DELETE: if btnDelete.Enabled then btnDeleteClick(NIL);
end; // case
end;

function TfrmSQLMgrEditor.ShowModalEx(AStrings: TStrings): Integer;
begin
FSQLs.Assign(AStrings);

UpdateLV('');

Result:=ShowModal;
if Result<>mrOK then exit;

AStrings.Assign(FSQLs);
end;

procedure TfrmSQLMgrEditor.lvMainDblClick(Sender: TObject);
begin
if btnEdit.Enabled then btnEditClick(NIL);
end;

{ TSQLMgrEditor }

procedure TSQLMgrEditor.ExecuteVerb(Index: Integer);
var frmSQLMgrEditor: TfrmSQLMgrEditor;
begin
frmSQLMgrEditor:=TfrmSQLMgrEditor.Create(nil);
try
  if frmSQLMgrEditor.ShowModalEx(TDataSetSQLMgr(Component).SQLs)=mrOK then Designer.Modified;
finally
  frmSQLMgrEditor.Free;
end; // try
end;

function TSQLMgrEditor.GetVerb(Index: Integer): String;
begin
Result:='Edit SQLs...'
end;

function TSQLMgrEditor.GetVerbCount: Integer;
begin
Result:=1;
end;

procedure TfrmSQLMgrEditor.btnAddMultipleClick(Sender: TObject);
var I, ID: Integer; S: String; B: Boolean;
begin
ID:=GetMaxID;
S:='';

if not Assigned(frmAddMultipleSQLs) then frmAddMultipleSQLs:=TfrmAddMultipleSQLs.Create(Self);

while True do
	begin
	if frmAddMultipleSQLs.ShowModalEx(ID, S)<>mrOK then exit;

  B:=False;
  for i:=ID to ID+frmAddMultipleSQLs.mmSQLs.Lines.Count-1 do
    begin
    B:=B or IDExists(i, 'Add multiple SQLs');
    if B then break;
    end;
    
  if not B then break;
	end;

for i:=0 to frmAddMultipleSQLs.mmSQLs.Lines.Count-1 do
  with lvMain.Items.Add do
    begin
    Caption:=IntToStr(i+ID);
    S:=frmAddMultipleSQLs.mmSQLs.Lines[i];
    SubItems.Add(S);
    Data:=Pointer(FSQLs.AddObject(S, TObject(i+ID)))
    end;
end;

function TfrmSQLMgrEditor.IDExists(AID: Integer; const ACaption: string): Boolean;
begin
Result:=False;

if FSQLs.IndexOfObject(TObject(AID))<>-1 then
  begin
  MessageBox(Handle, PChar(Format('SQL with ID=%d already exists', [AID])), PChar(ACaption), MB_ICONERROR);
  Result:=True;
  end;
end;

procedure TfrmSQLMgrEditor.lvMainResize(Sender: TObject);
begin
lvMain.Columns[1].Width:=lvMain.Width-lvMain.Columns[0].Width-30
end;

function TfrmSQLMgrEditor.GetMaxID: Integer;
var I: Integer;
begin
Result:=0;
for i:=0 to FSQLs.Count-1 do Result:=Max(Result, Integer(FSQLs.Objects[i]));
Inc(Result);
end;

procedure TfrmSQLMgrEditor.frmSQLMgrEditorCreate(Sender: TObject);
begin
FSQLs:=TStringList.Create;
end;

procedure TfrmSQLMgrEditor.frmSQLMgrEditorDestroy(Sender: TObject);
begin
FSQLs.Free;
end;

function lstSortByIDProc(List: TStringList; Index1, Index2: Integer): Integer;
begin
Result:=Integer(List.Objects[Index1])-Integer(List.Objects[Index2])
end;

function lstSortBySQLProc(List: TStringList; Index1, Index2: Integer): Integer;
begin
Result:=AnsiCompareText(List[Index1], List[Index2])
end;

procedure TfrmSQLMgrEditor.lvMainColumnClick(Sender: TObject;
  Column: TListColumn);
begin
case Column.Tag of
1: begin
   FSQLs.CustomSort(lstSortByIDProc);
   UpdateLV(txtFilter.Text)
   end;
2: begin
   FSQLs.CustomSort(lstSortBySQLProc);
   UpdateLV(txtFilter.Text)
   end;
end; // case
end;

procedure TfrmSQLMgrEditor.UpdateLV(const AFilter: String);
var I: Integer; S: String;
begin
lvMain.Items.BeginUpdate;
lvMain.Items.Clear;
for i:=0 to FSQLs.Count-1 do
  begin
  S:=FSQLs[i];
  if (AFilter<>'') and (Pos(AnsiUpperCase(AFilter), AnsiUpperCase(S))=0) then continue;
  with lvMain.Items.Add do
    begin
    Caption:=IntToStr(Integer(FSQLs.Objects[i]));
    SubItems.Add(S);
    Data:=Pointer(i);
    end;
  end;
lvMain.Items.EndUpdate
end;

procedure TfrmSQLMgrEditor.btnApplyFilterClick(Sender: TObject);
begin
UpdateLV(txtFilter.Text);
end;

procedure TfrmSQLMgrEditor.txtFilterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if Key=VK_RETURN then
  begin
  Key:=0;
  btnApplyFilterClick(NIL);
  end;
end;

end.


