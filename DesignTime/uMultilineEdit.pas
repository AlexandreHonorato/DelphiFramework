unit uMultilineEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, _UFKBBanner, DesignEditors, DesignIntf;

type
  TfrmMultilineEdit = class(TForm)
    btnOK: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure CheckBtnOK(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TMultilineEditor = class(TPropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;
  
implementation

{$R *.DFM}

procedure Register;
begin
RegisterPropertyEditor(TypeInfo(String), TUFKBBanner, 'Caption', TMultilineEditor)
end;

procedure TfrmMultilineEdit.CheckBtnOK(Sender: TObject);
begin
btnOK.Enabled:=True;
end;

procedure TfrmMultilineEdit.FormShow(Sender: TObject);
begin
CheckBtnOK(NIL)
end;

{ TMultilineEditor }

procedure TMultilineEditor.Edit;
var frmMultilineEdit: TfrmMultilineEdit;
begin
frmMultilineEdit:=TfrmMultilineEdit.Create(nil);
try
  frmMultilineEdit.Memo1.Text:=GetStrValue;
  if frmMultilineEdit.ShowModal=mrOK then SetStrValue(frmMultilineEdit.Memo1.Text);
finally
  frmMultilineEdit.Free;
end; // try
end;

function TMultilineEditor.GetAttributes: TPropertyAttributes;
begin
Result:=[paDialog];
end;

function TMultilineEditor.GetValue: string;
begin
Result:=GetStrValue
end;

procedure TMultilineEditor.SetValue(const Value: string);
begin
SetStrValue(Value);
end;

end.
