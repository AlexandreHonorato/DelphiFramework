unit uCodeTemplateFrm;

interface

//UAL 2

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfrmCodeTemplate = class(TForm)
    lblTitle: TLabel;
    mmCode: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCodeTemplate: TfrmCodeTemplate;

implementation

{$R *.dfm}

end.
