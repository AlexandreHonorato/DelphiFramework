unit uDeleteUnitFilesDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TdlgDeleteUnitFiles = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lbFileNames: TListBox;
  private
    { Private declarations }
  public
    function ConfirmDeletion(AFileNames: TStrings): Boolean;
  end;

implementation

{$R *.DFM}

function TdlgDeleteUnitFiles.ConfirmDeletion(AFileNames: TStrings): Boolean;
begin
lbFileNames.Items.Assign(AFileNames);
Result:=ShowModal=mrOK
end;

end.
