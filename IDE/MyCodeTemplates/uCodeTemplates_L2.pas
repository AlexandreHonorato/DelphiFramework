unit uCodeTemplates_L2;

interface

//UAL 2

uses SysUtils, Windows, Classes, Graphics, Controls, Forms, Menus, ImgList, uCodeTemplates_L3,
  ToolsAPI, _Strings, _ToolsAPI;

const
  iiFolder = 0;
  iiPage = 1;
  iiPageRed = 2;

type TCT_ML = class
  private
    FText: String;
    mnuTemplates: TPopupMenu;

    procedure mnuTemplateClick(Sender: TObject);
    procedure mnuBlockClick(Sender: TObject);
    procedure ShowTemplate(ATemplate: TCTTemplate; AParams: TStrings);
  public
    ImageList: TImageList;
    destructor Destroy; override;
    constructor Create;
    procedure TrackTemplatesMenu(const AText: String; Pt: TPoint);
    procedure InsertCompiledLines(const EV: IOTAEditView; ABlock: TTemplateBlock);
  end;

var CT_ML: TCT_ML;

procedure Init_CT_ML;
procedure Done_CT_ML;

implementation

uses uCodeTemplatesFrm;

{$R CodeTemplates.res}

procedure Init_CT_ML;
begin
Init_CT_BLL;
CT_ML:=TCT_ML.Create;
end;

procedure Done_CT_ML;
begin
CT_ML.Free;
Done_CT_BLL;
end;

{ TCT_ML }

constructor TCT_ML.Create;
var bmp: TBitmap;
begin
inherited Create;
ImageList:=TImageList.Create(NIL);
bmp:=TBitmap.Create;
bmp.Transparent:=True;

bmp.LoadFromResourceName(hInstance, 'bmpFolder');
ImageList.AddMasked(bmp, bmp.TransparentColor);

bmp.LoadFromResourceName(hInstance, 'bmpPage');
ImageList.AddMasked(bmp, bmp.TransparentColor);

bmp.LoadFromResourceName(hInstance, 'bmpPageRed');
ImageList.AddMasked(bmp, bmp.TransparentColor);

bmp.Free;

mnuTemplates:=TPopupMenu.Create(NIL);
mnuTemplates.Images:=ImageList;
mnuTemplates.AutoHotkeys:=maManual
end;

destructor TCT_ML.Destroy;
begin
mnuTemplates.Free;
ImageList.Free;
inherited Destroy;
end;

procedure TCT_ML.InsertCompiledLines(const EV: IOTAEditView; ABlock: TTemplateBlock);

  function InsertLineWithPos(const AText: String; var ACursorPos: TPoint): Boolean;
  var P: Integer;
  begin
  P:=Pos('|', AText);
  if P=0 then
    begin
    Result:=False;
    EV.Position.InsertText(AText);
    end
  else
    begin
    Result:=True;
    EV.Position.InsertText(copy(AText, 1, P-1));
    ACursorPos.X:=EV.Position.Column;
    ACursorPos.Y:=EV.Position.Row;
    EV.Position.InsertText(StrTail(AText, P));
    end;
  end;

var I, Indent: Integer; CursorPos: TPoint; HasCursor: Boolean; EB: IOTAEditBlock;
begin
ABlock.Reset(False);
ABlock.Compile;
EB:=EV.Block;
if (EB.StartingColumn=EB.EndingColumn) and (EB.StartingRow=EB.EndingRow) then
  Indent:=EV.Position.Column-1
else
  Indent:=EB.StartingColumn-1;

HasCursor:=InsertLineWithPos(ABlock.CompiledLines[0], CursorPos);
for i:=1 to ABlock.CompiledLines.Count-1 do
  begin
  EV.Position.InsertCharacter(#10);
  HasCursor:=InsertLineWithPos(StringOfChar(' ', Indent)+ABlock.CompiledLines[i], CursorPos) or HasCursor;
  end;

if HasCursor then EV.Position.Move(CursorPos.Y, CursorPos.X);  
EV.Paint;
end;

procedure TCT_ML.mnuBlockClick(Sender: TObject);
var EV: IOTAEditView;
begin
EV:=taCurrentEditView;
if EV<>NIL then
  InsertCompiledLines(EV, TTemplateBlock(TMenuItem(Sender).Tag));
end;

procedure TCT_ML.mnuTemplateClick(Sender: TObject);
var Template: TCTTemplate; Lst: TStringList; EV: IOTAEditView;
begin
EV:=taCurrentEditView;
if EV=NIL then exit;

Lst:=TStringList.Create;
ParseWords(FText, Lst, [' ']);
Template:=TCTTemplate(TMenuItem(Sender).Tag);
Template.ResetVars(Lst);
if Template.VarsDefined then
  begin
  InsertCompiledLines(EV, Template.Blocks[0]);
  if (Template.BlockCount>1) or (CT_BLL.CurrTemplate<>NIL {== frmCodeTemplates.IsVisible}) then
	  begin
	  CT_ML.ShowTemplate(Template, Lst);
    EV.GetEditWindow.Form.BringToFront;
	  end;
  end
else
  CT_ML.ShowTemplate(Template, Lst);
Lst.Free;
end;

procedure TCT_ML.ShowTemplate(ATemplate: TCTTemplate; AParams: TStrings);
begin
if not Assigned(frmCodeTemplates) then frmCodeTemplates:=TfrmCodeTemplates.Create(Application);
frmCodeTemplates.ShowEx(ATemplate, AParams);
end;

procedure TCT_ML.TrackTemplatesMenu(const AText: String; Pt: TPoint);

  procedure AddFolder(AMenuItem: TMenuItem; AFolder: TCTFolder);
  var MI: TMenuItem; I: Integer; CT: TCTItem; S: String;
  begin
  for i:=0 to AFolder.Count-1 do
    begin
    CT:=AFolder[i];
    S:=CT.Caption;
    if CT.HotKey<>'' then S:=S+' (&'+CT.HotKey+')';
    MI:=NewItem(S, 0, False, True, NIL, 0, '');
    AMenuItem.Add(MI);
    if CT is TCTFolder then
	    begin
	    AddFolder(MI, TCTFolder(CT));
      MI.ImageIndex:=iiFolder
	    end
    else
      begin
      MI.OnClick:=mnuTemplateClick;
      MI.ImageIndex:=iiPage;
      MI.Tag:=Integer(CT)
      end;
    end;
  end;

var I: Integer; Template: TCTTemplate; Block: TTemplateBlock; MI: TMenuItem;
  S: String;
begin
FText:=AText;

mnuTemplates.Items.Clear;
AddFolder(mnuTemplates.Items, CT_BLL);

Template:=CT_BLL.CurrTemplate;
if Template<>NIL then
  begin
  mnuTemplates.Items.Add(NewLine);
  for i:=0 to Template.BlockCount-1 do
    begin
    Block:=Template.Blocks[i];
    S:=Template.Caption+' - '+Block.Caption;
    if i<9 then S:=S+' (&'+IntToStr(i+1)+')';
    MI:=NewItem(S, 0, False, True, mnuBlockClick, 0, '');
    MI.Tag:=Integer(Block);
    MI.ImageIndex:=iiPageRed;
    mnuTemplates.Items.Add(MI);
    end;
  end;
mnuTemplates.Popup(Pt.X, Pt.Y);
end;

end.


