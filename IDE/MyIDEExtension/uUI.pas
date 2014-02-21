unit uUI;

interface

uses SysUtils, Graphics, Controls, uCodeEntities, uComponentsMenu, uProjectMenu,
  uInfoPanelFrm, Forms;

const
  iiClass = 0;
  iiUnit = 1;
  iiInterface = 2;
  iiVariable = 3;
  iiType = 4;
  iiUnitGray = 5;

type TUI = class
  private
    FInfoPanel: TfrmInfoPanel;
    FComponentsMenu: TMainMenu_Components;
    FProjectMenu: TMainMenu_Project;
    procedure InitResourceImages;
  public
    il16: TImageList;
    constructor Create;
    destructor Destroy; override;
    function COT2II(ACodeObjectType: TCodeObjectType): Integer;
    procedure DrawCodeObject(ACanvas: TCanvas; ALeft, ATop: Integer; ACodeObjectType: TCodeObjectType);
    property ProjectMenu: TMainMenu_Project read FProjectMenu; 
  end;

{.SINGLETON}
var UI: TUI;

implementation

{$R Images\IDEImages.res}

{ TUI }

function TUI.COT2II(ACodeObjectType: TCodeObjectType): Integer;
const ImageIndexes: array[TCodeObjectType] of Integer = (-1, iiUnit,
  iiClass, iiInterface, iiType, iiVariable);
begin
Result:=ImageIndexes[ACodeObjectType]
end;

constructor TUI.Create;
begin
inherited Create;
il16:=TImageList.Create(NIL);
InitResourceImages;

FProjectMenu:=TMainMenu_Project.Create;
FComponentsMenu:=TMainMenu_Components.Create;

FInfoPanel:=TfrmInfoPanel.Create(NIL);
FInfoPanel.Show;
end;

destructor TUI.Destroy;
begin
if Assigned(FInfoPanel) then FreeAndNIL(FInfoPanel);

if Assigned(FComponentsMenu) then FreeAndNIL(FComponentsMenu);
if Assigned(FProjectMenu) then FreeAndNIL(FProjectMenu);

if Assigned(il16) then FreeAndNIL(il16);
inherited Destroy;
end;

procedure TUI.DrawCodeObject(ACanvas: TCanvas; ALeft, ATop: Integer;
  ACodeObjectType: TCodeObjectType);
begin
il16.Draw(ACanvas, ALeft, ATop, COT2II(ACodeObjectType));
end;

procedure TUI.InitResourceImages;

  procedure AddImage(const AResourceName: String);
  var bmp: TBitmap;
  begin
  bmp:=TBitmap.Create;
  bmp.LoadFromResourceName(hInstance, PChar(AResourceName));
  il16.AddMasked(bmp, bmp.Canvas.Pixels[0, 0]);
  FreeAndNIL(bmp);
  end;

begin
AddImage('imgClass');
AddImage('imgUnit');
AddImage('imgInterface');
AddImage('imgVariable');
AddImage('imgType');
AddImage('imgUnitGray');
end;

end.


