unit uCodeObjectsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, _VariantEdit, _Strings, ExtCtrls, _GDI,
  ToolsAPI, _VCL_ZZZ, uCodeEntities, Menus;

type TObjectTypeFilterChanged = procedure(Sender: TObject; ANewFilter: TCodeObjectType) of object;

type
  TdlgCodeObjects = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    txtCodeObject: TVarEdit;
    lbCodeObjects: TVarListBox;
    Label1: TLabel;
    cmbFilter: TComboBox;
    lblFilter: TLabel;
    lblCount: TLabel;
    procedure cmbFilterChange(Sender: TObject);
    procedure lbCodeObjectsDblClick(Sender: TObject);
    procedure dlgCodeObjectsCreate(Sender: TObject);
    procedure txtCodeObjectCheckVariantsComplete(Sender: TObject);
  private
    FShowFilter: Boolean;
    FFilter: TCodeObjectType;
    FOnFilterChanged: TObjectTypeFilterChanged;
    procedure SetShowFilter(const Value: Boolean);
    procedure SetFilter(const Value: TCodeObjectType);
  public
    function AdditionalControlsTop: Integer;
    property ShowFilter: Boolean read FShowFilter write SetShowFilter;
    procedure AddFilter(const ACaption: String; ACodeObjectType: TCodeObjectType);
    property Filter: TCodeObjectType read FFilter write SetFilter;
    property OnFilterChanged: TObjectTypeFilterChanged read FOnFilterChanged write FOnFilterChanged;
  end;

implementation

{$R *.DFM}

function TdlgCodeObjects.AdditionalControlsTop: Integer;
begin
Result:=btnOK.Top;
exit;
if FShowFilter then
  Result:=ControlBottom(cmbFilter)+11
else
  Result:=ControlBottom(lbCodeObjects)+8;
end;

procedure TdlgCodeObjects.lbCodeObjectsDblClick(Sender: TObject);
begin
if Assigned(btnOK.OnClick) then btnOK.OnClick(NIL);
end;

procedure TdlgCodeObjects.SetShowFilter(const Value: Boolean);
begin
FShowFilter:=Value;
if FShowFilter then
  begin
  cmbFilter.Visible:=True;
  lblFilter.Visible:=True;
  lbCodeObjects.Height:=332;
  end
else
  begin
  cmbFilter.Visible:=False;
  lblFilter.Visible:=False;
  lbCodeObjects.Height:=364;
  end;
lblCount.Top:=ControlBottom(lbCodeObjects)+2;  
end;

procedure TdlgCodeObjects.AddFilter(const ACaption: String; ACodeObjectType:
    TCodeObjectType);
begin
cmbFilter.Items.AddObject(ACaption, TObject(ACodeObjectType));
end;

procedure TdlgCodeObjects.cmbFilterChange(Sender: TObject);
begin
Filter:=TCodeObjectType(cmbFilter.Items.Objects[cmbFilter.ItemIndex])
end;

procedure TdlgCodeObjects.SetFilter(const Value: TCodeObjectType);
begin
FFilter:=Value;
cmbFilter.ItemIndex:=cmbFilter.Items.IndexOfObject(TObject(FFilter));
if Assigned(FOnFilterChanged) then FOnFilterChanged(Self, FFilter);
txtCodeObject.Text2ListBox;
if Visible then txtCodeObject.SetFocus;
end;

procedure TdlgCodeObjects.dlgCodeObjectsCreate(Sender: TObject);
begin
ShowFilter:=False;
FFilter:=coUnknown;
end;

procedure TdlgCodeObjects.txtCodeObjectCheckVariantsComplete(Sender: TObject);
begin
lblCount.Caption:=IntToStr(lbCodeObjects.Items.Count)+' object(s)';
end;

end.
