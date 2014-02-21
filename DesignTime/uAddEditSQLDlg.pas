unit uAddEditSQLDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TfrmAddEditSQL = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    txtSQL: TMemo;
    seID: TSpinEdit;
    procedure CheckBtnOK(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtSQLKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    function ShowModalEx(AEdit: Boolean; var AID: Integer; var ASQL: String): Integer;
  end;

implementation

{$R *.DFM}

procedure TfrmAddEditSQL.CheckBtnOK(Sender: TObject);
begin
btnOK.Enabled:=txtSQL.Text<>''
end;

procedure TfrmAddEditSQL.FormShow(Sender: TObject);
begin
CheckBtnOK(NIL)
end;

function TfrmAddEditSQL.ShowModalEx(AEdit: Boolean; var AID: Integer; var ASQL: String): Integer;
begin
seID.Value:=AID;
txtSQL.Text:=ASQL;
if AEdit then Caption:='Edit SQL' else Caption:='Add SQL';
CheckBtnOK(NIL);

Result:=ShowModal;
if Result<>mrOK then exit;

AID:=seID.Value;
ASQL:=txtSQL.Text;
end;

procedure TfrmAddEditSQL.txtSQLKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Key=VK_RETURN) and (Shift=[ssCtrl]) and btnOK.Enabled then
  ModalResult:=mrOK
else
if Key=VK_ESCAPE then
  ModalResult:=mrCancel
end;

end.
