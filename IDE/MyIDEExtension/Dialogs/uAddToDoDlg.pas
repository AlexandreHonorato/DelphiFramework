unit uAddToDoDlg;

interface

uses
  SysUtils, Windows, Forms, StdCtrls, Controls, Classes;

type
  TfrmAddToDo = class(TForm)
    txtText: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
  private
  public
  end;

function AskToDoText(out AText: String): Boolean;

implementation

{$R *.DFM}

function AskToDoText(out AText: String): Boolean;
var Frm: TfrmAddToDo;
begin
Application.CreateForm(TfrmAddToDo, Frm);
try
  Result:=Frm.ShowModal=mrOK;
  if Result then
    AText:=Frm.txtText.Text;
finally
  FreeAndNil(Frm);
end; // try
end;

end.


