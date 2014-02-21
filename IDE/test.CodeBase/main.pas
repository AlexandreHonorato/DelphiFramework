unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, _Strings, VariantEdit, uCodeBase,
  uCodeEntities, uSourceParser_CodeBase, uFileMappers;

type
  TForm1 = class(TForm)
    lbAllUnits: TListBox;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Button1: TButton;
    Splitter2: TSplitter;
    pgEditors: TPageControl;
    Button2: TButton;
    Panel2: TPanel;
    tvClasses: TTreeView;
    mmErrors: TMemo;
    Splitter3: TSplitter;
    Panel3: TPanel;
    Label1: TLabel;
    vtxtClasses: TVarEdit;
    vlbClasses: TVarListBox;
    Splitter4: TSplitter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    tvInterfaces: TTreeView;
    TabSheet2: TTabSheet;
    tvMetaclasses: TTreeView;
    TabSheet3: TTabSheet;
    tvSingletons: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
    procedure lbAllUnitsDblClick(Sender: TObject);
    procedure vtxtClassesCheckVariant(Sender: TObject; const AText, AVariant: string; var Accept: Boolean);
  private
    FOpenedUnits: TStringList;
    procedure FMS_FromEditor(AStream: TMemoryStream; const AFileName: String;
      out AGotData: Boolean; out AStopIteration: Boolean);
    procedure OnParseError(AException: Exception);
  public
    procedure OpenUnit(const AUnitName: String);
    procedure CloseUnit(AIndex: Integer);
    procedure GetUnitNames(AUnitNames: TStrings);
    procedure ShowCodeBaseTree(ACodeBase: TCodeBase);
  end;

var
  Form1: TForm1; codeBase: TCodeBase;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);

  procedure ClassesAndUnitsToStrings(AUnits: TCodeUnits; AStrings: TStrings);
  var I, J: Integer; unit_: TCodeUnit; class_: TCodeClass; interface_: TCodeInterface;
  begin
  AStrings.Clear;
  for i:=0 to AUnits.Count-1 do
    begin
    unit_:=AUnits[i];
    AStrings.AddObject(unit_.Caption(True), unit_);
    for j:=0 to unit_.Classes.Count-1 do
      begin
      class_:=unit_.Classes[j];
      AStrings.AddObject(class_.Name, class_);
      end;
    for j:=0 to unit_.Interfaces.Count-1 do
      begin
      interface_:=unit_.Interfaces[j];
      AStrings.AddObject(interface_.Name, interface_);
      end;
    end;
  TStringList(AStrings).Sort;
  end;

var sourceParser: TSourceParser_CodeBase; fileMappers: TFileMappers;
begin
mmErrors.Lines.Clear;
tvClasses.Items.Clear;

fileMappers:=TFileMappers.Create
  .AddFMSDelegate(FMS_FromEditor)
  .AddFMSDelegate(FileMappers_Default.Map_FromFile);

sourceParser:=TSourceParser_CodeBase.Create(fileMappers, GetUnitNames, codeBase);
try
  sourceParser.OnParseError:=OnParseError;
  sourceParser.FullRebuild;
finally
  FreeAndNIL(sourceParser);
end; // try

ShowCodeBaseTree(codeBase);

ClassesAndUnitsToStrings(codeBase.Units, vtxtClasses.Variants);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
CloseUnit(pgEditors.ActivePageIndex);
end;

procedure TForm1.CloseUnit(AIndex: Integer);
begin
FOpenedUnits.Delete(AIndex);
pgEditors.Pages[AIndex].Free;
end;

procedure TForm1.FMS_FromEditor(AStream: TMemoryStream; const AFileName: String; out AGotData: Boolean; out
    AStopIteration: Boolean);
var I: Integer;
begin
AGotData:=False;

for i:=0 to FOpenedUnits.Count-1 do
  if TC(AFileName, FOpenedUnits[i]) then
    begin
    TMemo(pgEditors.Pages[i].Controls[0]).Lines.SaveToStream(AStream);
    AGotData:=True;
    AStopIteration:=True;
    break;
    end;
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
codeBase:=TCodeBase.Create;
FOpenedUnits:=TStringList.Create;
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
if Assigned(FOpenedUnits) then FreeAndNIL(FOpenedUnits);
if Assigned(codeBase) then FreeAndNIL(codeBase);
end;

procedure TForm1.GetUnitNames(AUnitNames: TStrings);
begin
AUnitNames.LoadFromFile('d:\Andrey\Desktop\units.txt');
AUnitNames.Delete(0); // AUnitNames[0] is .csa file
end;

procedure TForm1.lbAllUnitsDblClick(Sender: TObject);
begin
OpenUnit(lbAllUnits.Items[lbAllUnits.ItemIndex]);
end;

procedure TForm1.OnParseError(AException: Exception);
begin
mmErrors.Lines.Text:=AException.Message;
end;

procedure TForm1.OpenUnit(const AUnitName: String);
var page: TTabSheet; memo: TMemo;
begin
FOpenedUnits.Add(AUnitName);

page:=TTabSheet.Create(Self);
page.PageControl:=pgEditors;
page.Caption:=ExtractFileName(AUnitName);

memo:=TMemo.Create(page);
memo.Parent:=page;
memo.Align:=alClient;
memo.ScrollBars:=ssBoth;
memo.WordWrap:=False;
memo.Lines.LoadFromFile(AUnitName);
end;

procedure TForm1.ShowCodeBaseTree(ACodeBase: TCodeBase);

  procedure AddClassToTree(AClass: TCodeClass; AParentNode: TTreeNode);
  var node: TTreeNode; I: Integer; S: String;
  begin
  S:=AClass.Name;
  if AClass.ImplementedInterfaces.Count<>0 then
    begin
    S:=S+' (';
    for i:=0 to AClass.ImplementedInterfaces.Count-1 do
      S:=S+AClass.ImplementedInterfaces[i].Name+', ';
    S:=S+')';
    end;

  node:=tvClasses.Items.AddChild(AParentNode, S);

  for i:=0 to AClass.Children.Count-1 do
    AddClassToTree(AClass.Children[i], node);
  end;

  procedure AddInterfaceToTree(AInterface: TCodeInterface; AParentNode: TTreeNode);
  var node: TTreeNode; I: Integer;
  begin
  node:=tvInterfaces.Items.AddChild(AParentNode, AInterface.Name);
  for i:=0 to AInterface.ImplementedIn.Count-1 do
    tvInterfaces.Items.AddChild(node, AInterface.ImplementedIn[i].Name);
  end;

  procedure AddMetaclasssToTree(AMetaclass: TCodeMetaclass; AParentNode: TTreeNode);
  var node: TTreeNode;
  begin
  node:=tvMetaclasses.Items.AddChild(AParentNode, AMetaclass.Name);
  tvInterfaces.Items.AddChild(node, AMetaclass.Class_.Name);
  end;

  procedure AddSingletonToTree(ASingleton: TCodeSingleton; AParentNode: TTreeNode);
  begin
  tvSingletons.Items.AddChild(AParentNode, ASingleton.Name);
  end;

var I: Integer; rootClasses: TCodeClassList; interfaces: TCodeInterfaceList;
  metaclasses: TCodeMetaclassList; singletons: TCodeSingletonList;
begin
rootClasses:=ACodeBase.Root.Classes;
for i:=0 to rootClasses.Count-1 do
  AddClassToTree(rootClasses[i], NIL);

interfaces:=TCodeInterfaceList.Create(False);
try
  ACodeBase.GetAllInterfaces(interfaces, True);

  for i:=0 to interfaces.Count-1 do
    AddInterfaceToTree(interfaces[i], NIL);
finally
  FreeAndNIL(interfaces);
end; // try

metaclasses:=TCodeMetaclassList.Create(False);
try
  ACodeBase.GetAllMetaclasses(metaclasses, True);

  for i:=0 to metaclasses.Count-1 do
    AddMetaclasssToTree(metaclasses[i], NIL);
finally
  FreeAndNIL(metaclasses);
end; // try

singletons:=TCodeSingletonList.Create(False);
try
  ACodeBase.GetAllSingletons(singletons, True);

  for i:=0 to singletons.Count-1 do
    AddSingletonToTree(singletons[i], NIL);
finally
  FreeAndNIL(singletons);
end; // try
end;

procedure TForm1.vtxtClassesCheckVariant(Sender: TObject; const AText, AVariant: string; var Accept: Boolean);
var P: Integer; S, _Word, _Variant: String;
begin
Accept:=True;
S:=AText;
_Variant:=AnsiUpperCase(AVariant);
P:=Pos(';', S);
while P<>0 do
  begin
  _Word:=AnsiUpperCase(copy(S, 1, P-1));
  if _Word<>'' then Accept:=Accept and (Pos(_Word, _Variant)<>0);
  if not Accept then break;
  S:=StrTail(S, P);
  P:=Pos(';', S);
  end;

if Accept then
  begin
  _Word:=AnsiUpperCase(S);
  if _Word<>'' then Accept:=Accept and (Pos(_Word, _Variant)<>0);
  end;
end;

end.
