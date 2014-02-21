unit _InputBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmInputBox = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    lblPrompt: TLabel;
    txtValue: TEdit;
    procedure CheckBtnOK(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAllowEmptyRes: Boolean;
  public
    function Ask(const ACaption, APrompt: String; var AText: String; AAllowEmptyRes: Boolean): Boolean;
  end;

var
  frmInputBox: TfrmInputBox;

function InputBox(const ACaption, APrompt: String; var AText: String; AAllowEmptyRes: Boolean): Boolean;
  
implementation

{$R *.DFM}

function TfrmInputBox.Ask(const ACaption, APrompt: String;
  var AText: String; AAllowEmptyRes: Boolean): Boolean;
var prompt: String;
begin
Caption:=ACaption;

prompt:=APrompt;
if (Length(prompt)>0) and (prompt[Length(prompt)]<>':') then prompt:=prompt+':';
lblPrompt.Caption:=prompt;

txtValue.Text:=AText;
FAllowEmptyRes:=AAllowEmptyRes;

Result:=ShowModal=mrOK;

if Result then
  AText:=txtValue.Text;
end;

procedure TfrmInputBox.CheckBtnOK(Sender: TObject);
begin
btnOK.Enabled:=FAllowEmptyRes or (txtValue.Text<>'')
end;

procedure TfrmInputBox.FormShow(Sender: TObject);
begin
txtValue.SetFocus;
CheckBtnOK(NIL)
end;

function InputBox(const ACaption, APrompt: String; var AText: String; AAllowEmptyRes: Boolean): Boolean;
var dlg: TfrmInputBox;
begin
dlg:=TfrmInputBox.Create(NIL);
try
  Result:=dlg.Ask(ACaption, APrompt, AText, AAllowEmptyRes);
finally
  FreeAndNIL(dlg);
end; // try
end;

end.
