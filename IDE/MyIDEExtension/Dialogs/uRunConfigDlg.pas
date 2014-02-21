unit uRunConfigDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, _ListView_VCL_ZZZ, _Strings, ImgList;

type
  TdlgRunConfig = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    Label1: TLabel;
    lvConfigs: TListView;
    ImageList1: TImageList;
    Label2: TLabel;
    txtParams: TEdit;
    procedure FormShow(Sender: TObject);
    procedure lvConfigsDblClick(Sender: TObject);
    procedure lvConfigsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    { Private declarations }
  public
    function SelectConfig(AConfigNames, AConfigParams: TStrings; var AParams: String): Boolean;
  end;

implementation

{$R *.DFM}

procedure TdlgRunConfig.FormShow(Sender: TObject);
begin
lvConfigs.SetFocus;
end;

procedure TdlgRunConfig.lvConfigsDblClick(Sender: TObject);
begin
if btnOK.Enabled then
  ModalResult:=mrOK
end;

procedure TdlgRunConfig.lvConfigsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
if lvConfigs.Selected <> NIL then
  txtParams.Text := lvConfigs.Selected.SubItems[0]
end;

function TdlgRunConfig.SelectConfig(AConfigNames, AConfigParams: TStrings; var AParams: String): Boolean;

  procedure AddConfig(const AConfigName, AConfigParam: String);
  begin
  with lvConfigs.Items.Add do
	  begin
	  Caption:=AConfigName;
	  SubItems.Add(AConfigParam);
	  ImageIndex:=0;
	  end;
  end;

var I, idx: Integer;
begin
if AConfigNames.Count<>AConfigParams.Count then
  raise Exception.Create('ConfigNames.Count<>ConfigParams.Count');

AddConfig('(no params)', '');

for i:=0 to AConfigNames.Count-1 do
  AddConfig(AConfigNames[i], AConfigParams[i]);

idx:=-1;
for i:=0 to lvConfigs.Items.Count-1 do
  if TC(lvConfigs.Items[i].SubItems[0], AParams) then
    begin
    idx:=i;
    break;
    end;

if idx<>-1 then
  ShowItem2(lvConfigs, idx);

Result:=ShowModal=mrOK;

if Result then
  AParams:=txtParams.Text;
end;

end.
