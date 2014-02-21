unit uAddMultipleSQLsDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TfrmAddMultipleSQLs = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    mmSQLs: TMemo;
    Label1: TLabel;
    seStartID: TSpinEdit;
    Label2: TLabel;
    procedure CheckBtnOK(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    function ShowModalEx(var StartID: Integer; var AText: String): Integer;
  end;

var
  frmAddMultipleSQLs: TfrmAddMultipleSQLs;

implementation

{$R *.DFM}

procedure TfrmAddMultipleSQLs.CheckBtnOK(Sender: TObject);
begin
btnOK.Enabled:=mmSQLs.Text<>''
end;

procedure TfrmAddMultipleSQLs.FormShow(Sender: TObject);
begin
CheckBtnOK(NIL)
end;

function TfrmAddMultipleSQLs.ShowModalEx(var StartID: Integer; var AText: String): Integer;
begin
seStartID.Value:=StartID;
mmSQLs.Text:=AText;

Result:=ShowModal;
if Result<>mrOK then exit;

StartID:=seStartID.Value;
AText:=mmSQLs.Text;
end;

end.
