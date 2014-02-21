unit uCodeTemplatesFrm;

interface

//UAL 2

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uCodeTemplates_L3, ExtCtrls,
  ComCtrls, uCodeTemplateFrm, ImgList, uCodeTemplates_L2, _TreeView_VCL, _VCL_ZZZ;

const
  CTRL_Margin = 5;

type
  TfrmCodeTemplates = class(TForm)
    Splitter1: TSplitter;
    pnlTemplate: TPanel;
    pnlVars: TPanel;
    splTemplate: TSplitter;
    lblTemplate: TLabel;
    Panel1: TPanel;
    tvMain: TTreeView;
    Label1: TLabel;
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
    procedure pnlVarsResize(Sender: TObject);
    procedure pnlTemplateResize(Sender: TObject);
    procedure tvMainDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FVarLabels, FVarEdits, FCodeForms: TList;
    procedure ShowTemplate(ATemplate: TCTTemplate; AParams: TStrings; ASelectItem: Boolean);
    procedure VarEditOnChange(Sender: TObject);
  public
    procedure ShowEx(ATemplate: TCTTemplate; AParams: TStrings);
  end;

var
  frmCodeTemplates: TfrmCodeTemplates;

implementation

{$R *.dfm}

procedure TfrmCodeTemplates.ShowTemplate(ATemplate: TCTTemplate; AParams: TStrings; ASelectItem: Boolean);
var I, T: Integer; V: TTemplateVarEx; Lbl: TLabel; Edit: TEdit; Frm: TfrmCodeTemplate;
  Block: TTemplateBlock; N: TTreeNode;
begin
CT_BLL.CurrTemplate:=ATemplate;

for i:=0 to FVarLabels.Count-1 do
  begin
  TLabel(FVarLabels[i]).Free;
  TEdit(FVarEdits[i]).Free;
  end;
for i:=0 to FCodeForms.Count-1 do
  TfrmCodeTemplate(FCodeForms[i]).Free;
FVarLabels.Clear;
FVarEdits.Clear;
FCodeForms.Clear;
lblTemplate.Caption:='';

if ATemplate<>NIL then
	begin
	lblTemplate.Caption:=' Template "'+CT_BLL.CurrTemplate.Caption+'"';
  ATemplate.ResetVars(AParams);

	T:=8;
	for i:=0 to ATemplate.Vars.Count-1 do
	  begin
	  V:=ATemplate.Vars[i];

	  Lbl:=TLabel.Create(Self);
	  Lbl.Caption:=V.Caption+':';
	  Lbl.Left:=CTRL_Margin;
	  Lbl.Top:=T;
	  Lbl.Parent:=pnlVars;
	  FVarLabels.Add(Lbl);

	  Edit:=TEdit.Create(Self);
	  Edit.Text:=V.Value;
	  Edit.Left:=CTRL_Margin;
	  Edit.Top:=T+16;
	  Edit.Parent:=pnlVars;
	  Edit.OnChange:=VarEditOnChange;
	  Edit.Tag:=i;
	  FVarEdits.Add(Edit);

	  Inc(T, 48);
	  end;

	for i:=0 to ATemplate.BlockCount-1 do
	  begin
	  Block:=ATemplate.Blocks[i];
	  Frm:=TfrmCodeTemplate.Create(Application);
	  Frm.Parent:=pnlTemplate;
	  Frm.lblTitle.Caption:=Block.Caption+':';
	  Frm.Visible:=True;
	  FCodeForms.Add(Frm);
	  end;

	VarEditOnChange(NIL);
	pnlVarsResize(NIL);

  if ASelectItem then
    for i:=0 to tvMain.Items.Count-1 do
      begin
      N:=tvMain.Items[i];
      if N.Data=ATemplate then
        begin
        tvMain.Selected:=N;
        N.MakeVisible;
        end;
      end;
	end;
end;

procedure TfrmCodeTemplates.Form1Create(Sender: TObject);

  procedure AddItems(AFolder: TCTFolder; ANode: TTreeNode);
  var I: Integer; Node: TTreeNode; Item: TCTItem;
  begin
  for i:=0 to AFolder.Count-1 do
    begin
    Item:=AFolder[i];
    Node:=tvMain.Items.AddChild(ANode, Item.Caption);
    Node.Data:=Item;
    if Item is TCTFolder then
	    begin
	    AddItems(TCTFolder(Item), Node);
      SetNodeImageIndex(Node, iiFolder);
	    end
    else
      SetNodeImageIndex(Node, iiPage);
    end;
  end;

begin
tvMain.Images:=CT_ML.ImageList;
tvMain.StateImages:=CT_ML.ImageList;

FVarLabels:=TList.Create;
FVarEdits:=TList.Create;
FCodeForms:=TList.Create;

AddItems(CT_BLL, NIL);
end;

procedure TfrmCodeTemplates.Form1Destroy(Sender: TObject);
begin
FCodeForms.Free;
FVarLabels.Free;
FVarEdits.Free;
end;

procedure TfrmCodeTemplates.pnlVarsResize(Sender: TObject);
var I: Integer;
begin
for i:=0 to FVarEdits.Count-1 do TEdit(FVarEdits[i]).Width:=pnlVars.Width-2*CTRL_Margin;
pnlTemplateResize(NIL);
end;

procedure TfrmCodeTemplates.pnlTemplateResize(Sender: TObject);
var I, H, T, W, L: Integer;
begin
if CT_BLL.CurrTemplate=NIL then exit;

L:=splTemplate.Left+splTemplate.Width+CTRL_Margin;
T:=CTRL_Margin+lblTemplate.Height;
W:=pnlTemplate.Width-L-CTRL_Margin;
H:=((pnlTemplate.Height-lblTemplate.Height-CTRL_Margin) div FCodeForms.Count)-CTRL_Margin;
for i:=0 to FCodeForms.Count-1 do
  begin
  TfrmCodeTemplate(FCodeForms[i]).SetBounds(L, T, W, H);
  T:=T+H+CTRL_Margin;
  end;
end;

procedure TfrmCodeTemplates.VarEditOnChange(Sender: TObject);
var I: Integer; Block: TTemplateBlock;
begin
if Sender<>NIL then
  CT_BLL.CurrTemplate.Vars[TEdit(Sender).Tag].Value:=TEdit(Sender).Text;

for i:=0 to CT_BLL.CurrTemplate.BlockCount-1 do
  begin
  Block:=CT_BLL.CurrTemplate.Blocks[i];
  Block.Reset(False);
  Block.Compile;
  TfrmCodeTemplate(FCodeForms[i]).mmCode.Lines.Assign(Block.CompiledLines);
  end;
end;

procedure TfrmCodeTemplates.tvMainDblClick(Sender: TObject);
var O: TObject;
begin
O:=TObject(tvMain.Selected.Data);
if O is TCTTemplate then ShowTemplate(TCTTemplate(O), NIL, False);
end;

procedure TfrmCodeTemplates.FormShow(Sender: TObject);
begin
CheckSecondMonitor(Self);
WindowState:=wsMaximized;
end;

procedure TfrmCodeTemplates.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caHide;
CT_BLL.CurrTemplate:=NIL;
end;

procedure TfrmCodeTemplates.ShowEx(ATemplate: TCTTemplate; AParams: TStrings);
begin
Show;
ShowTemplate(ATemplate, AParams, True);
end;

end.
