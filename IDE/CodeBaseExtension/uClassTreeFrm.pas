unit uClassTreeFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uCodeEntities, ComCtrls, uParseEngineIDE, _DomainEventsList,
  _Registry, ExtCtrls, StdCtrls, uCodeBaseIDE;

type
  TfrmClassTree = class(TForm)
    tvClasses: TTreeView;
    mmErrors: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    btnFullRebuild: TButton;
    procedure btnFullRebuildClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure frmClassTreeDestroy(Sender: TObject);
  private
    procedure On_DEListClear(AEvent: DEListClear);
    procedure On_DEListChanged(AEvent: DEListChanged);
    procedure On_DECodeBaseError(AEvent: DECodeBaseError);
  private
    FModel: TCodeBaseIDE;
    procedure SetModel(const Value: TCodeBaseIDE);

    procedure UpdateUI;
    procedure ClearUI;
  public
    property Model: TCodeBaseIDE read FModel write SetModel;
  end;

implementation

{$R *.dfm}

const
  {$IFDEF DELPHI_XE}
  sRegKey = 'Software\My\DelphiIDE\XE\CodeBase';
  {$ELSE}
  sRegKey = 'Software\My\DelphiIDE\D7\CodeBase';
  {$ENDIF}

procedure TfrmClassTree.btnFullRebuildClick(Sender: TObject);
var parseEngine: TParseEngineIDE;
begin
if Assigned(FModel) then
  begin
  parseEngine:=TParseEngineIDE.Create(FModel);
  try
    parseEngine.FullRebuild;
  finally
    FreeAndNIL(parseEngine);
  end; // try
  end;
end;

procedure TfrmClassTree.On_DEListChanged(AEvent: DEListChanged);
begin
UpdateUI;
end;

procedure TfrmClassTree.On_DEListClear(AEvent: DEListClear);
begin
ClearUI
end;

procedure TfrmClassTree.frmClassTreeDestroy(Sender: TObject);
begin
SaveWindowPos(Self, sRegKey);
end;

procedure TfrmClassTree.ClearUI;
begin
Caption:='Classes - (no project)';
tvClasses.Items.Clear;
mmErrors.Lines.Clear;
end;

procedure TfrmClassTree.FormShow(Sender: TObject);
begin
LoadWindowPos(Self, sRegKey);
end;

procedure TfrmClassTree.UpdateUI;

  procedure AddClassToTree(AClass: TCodeClass; AParentNode: TTreeNode);
  var node: TTreeNode; I: Integer;
  begin
  node:=tvClasses.Items.AddChild(AParentNode, AClass.Name);
  for i:=0 to AClass.Children.Count-1 do
    AddClassToTree(AClass.Children[i], node);
  end;

var I: Integer;
begin
Caption:='Classes - '+Model.Caption;
tvClasses.Items.Clear;
for i:=0 to FModel.ExternalClasses.Count-1 do
  AddClassToTree(FModel.ExternalClasses[i], NIL);
mmErrors.Lines.Clear;
end;

procedure TfrmClassTree.SetModel(const Value: TCodeBaseIDE);
begin
if Assigned(FModel) then
  FModel.MessageBus.UnsignObject(Self);

ClearUI;

FModel:=Value;
if not Assigned(FModel) then exit;

UpdateUI;

if Assigned(FModel) then
  FModel.MessageBus.SignObject(Self,
    [DEListChanged,
     DEListClear,
     DECodeBaseError],
    [@TfrmClassTree.On_DEListChanged,
     @TfrmClassTree.On_DEListClear,
     @TfrmClassTree.On_DECodeBaseError]);
end;

procedure TfrmClassTree.On_DECodeBaseError(AEvent: DECodeBaseError);
begin
tvClasses.Items.Clear;
mmErrors.Text:=AEvent.ex.Message;
end;

end.
