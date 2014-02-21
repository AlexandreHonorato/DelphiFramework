unit uInfoPanelFrm;

{$I ..\..\CompilerVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uNotifications, _Registry;

type
  TfrmInfoPanel = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblLastUsedUnit: TLabel;
    btnF10: TButton;
    Label3: TLabel;
    procedure btnF10Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure frmInfoPanelCreate(Sender: TObject);
    procedure frmInfoPanelDestroy(Sender: TObject);
  private
    procedure On_NEUseUnit(AEvent: NEUseUnit);
    function F10Caption(AValue: Integer): String;
    procedure ReadWindowPos;
    procedure WriteWindowPos;
  protected
    procedure WMNCHitTest(var M: TWMNCHitTest); message WM_NCHITTEST;
  public
    { Public declarations }
  end;

var
  frmInfoPanel: TfrmInfoPanel;

implementation

uses uDOMAIN, uProjectMenu, uUI;

{$R *.dfm}

const
  {$IFDEF DELPHI_XE}
  sRegKey = 'Software\My\DelphiIDE\XE\MyIDEExtension';
  {$ELSE}
  sRegKey = 'Software\My\DelphiIDE\D7\MyIDEExtension';
  {$ENDIF}

procedure TfrmInfoPanel.btnF10Click(Sender: TObject);
begin
btnF10.Caption:=F10Caption(UI.ProjectMenu.InvertF10ShortCut);
end;

procedure TfrmInfoPanel.FormPaint(Sender: TObject);
begin
Canvas.Brush.Color:=clBlack;
Canvas.FrameRect(ClientRect);
end;

{ TForm1 }

procedure TfrmInfoPanel.On_NEUseUnit(AEvent: NEUseUnit);
const Captions: array[Boolean] of String = ('Impl', 'Itf');
begin
lblLastUsedUnit.Caption:=Captions[AEvent.IsItfSection]+': '+AEvent.UnitName
end;

procedure TfrmInfoPanel.WMNCHitTest(var M: TWMNCHitTest);
begin
inherited;
if M.Result=HTCLIENT then M.Result:=HTCAPTION
end;

procedure TfrmInfoPanel.frmInfoPanelCreate(Sender: TObject);
begin
ReadWindowPos;

btnF10.Caption:=F10Caption(F10_ACTION_SYNTAX_CHECK);

mbExtension.SignObject(Self,
  [NEUseUnit],
  [@TfrmInfoPanel.On_NEUseUnit]);
end;

procedure TfrmInfoPanel.frmInfoPanelDestroy(Sender: TObject);
begin
mbExtension.UnsignObject(Self);

WriteWindowPos;
end;

function TfrmInfoPanel.F10Caption(AValue: Integer): String;
begin
case AValue of
F10_ACTION_SYNTAX_CHECK:
  Result:='Syntax Check';
F10_ACTION_BUILD_ALL:
  Result:='Build All';
else
  Result:='(unknown)';
end; // case
end;

procedure TfrmInfoPanel.ReadWindowPos;
var R: TRegistryEx; L, T: Integer;
begin
R:=TRegistryEx.Create;
try
  R.RootKey:=HKEY_CURRENT_USER;
  R.OpenKey(sRegKey, True);
  L:=R.ReadIntValue('InfoPanel_Left', -1);
  T:=R.ReadIntValue('InfoPanel_Top', -1);

  if (L=-1) and (T=-1) then exit;

  Left:=L;
  Top:=T;
finally
  FreeAndNIL(R);
end; // try
end;

procedure TfrmInfoPanel.WriteWindowPos;
var R: TRegistryEx;
begin
R:=TRegistryEx.Create;
try
  R.RootKey:=HKEY_CURRENT_USER;
  R.OpenKey(sRegKey, True);
  R.WriteInteger('InfoPanel_Left', Left);
  R.WriteInteger('InfoPanel_Top', Top);
finally
  FreeAndNIL(R);
end; // try
end;

end.
