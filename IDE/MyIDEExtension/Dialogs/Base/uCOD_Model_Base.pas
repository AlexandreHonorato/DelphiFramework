unit uCOD_Model_Base;

interface

uses SysUtils, Windows, uCodeEntities, Graphics, ToolsAPI, _Strings, _GDI;

type VM_CodeObject = class
  private
    FCodeObject: TCodeObject;
  protected
    function GetCaption: String; virtual; abstract;
  public
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIsSelected: Boolean); virtual;
    property CodeObject: TCodeObject read FCodeObject;
    property Caption: String read GetCaption;
    constructor Create(ACodeObject: TCodeObject); reintroduce;
  end;

type VM_Unit = class(VM_CodeObject)
  protected
    function GetCaption: String; override;
  public
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIsSelected: Boolean); override;
    function Unit__: TBaseCodeUnit;
    function Color: TColor;
    function UnitSource: String;
  end;

type VM_UnitElement = class(VM_CodeObject)
  protected
    function GetCaption: String; override;
  public
    function UnitElement: TUnitElement;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIsSelected: Boolean); override;
  end;

type m_COD_DlgResult_Base = class
  public
    constructor Create; virtual;
  end;

type m_COD_DlgParams = class
  private
    FCodeObjects: TCodeObjectList;
    FCaption: String;
    FInitialText: String;
  public
    property Caption: String read FCaption write FCaption;
    property CodeObjects: TCodeObjectList read FCodeObjects write FCodeObjects;
    property InitialText: String read FInitialText write FInitialText; 

    constructor Create(AOwnsCodeObjects: Boolean); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses uUI;

{ m_COD_DlgParams }

constructor m_COD_DlgParams.Create(AOwnsCodeObjects: Boolean);
begin
inherited Create;
FCodeObjects:=TCodeObjectList.Create(AOwnsCodeObjects);
end;

destructor m_COD_DlgParams.Destroy;
begin
if Assigned(FCodeObjects) then
  FreeAndNIL(FCodeObjects);
inherited Destroy;
end;

{ VM_CodeObject }

constructor VM_CodeObject.Create(ACodeObject: TCodeObject);
begin
inherited Create;
FCodeObject:=ACodeObject;
end;

procedure VM_CodeObject.Draw(ACanvas: TCanvas; ARect: TRect;
  AIsSelected: Boolean);
begin
// do nothing
end;

{ VM_Unit }

function VM_Unit.Color: TColor;
const
  UnitColors: array[TUnitSource] of TColor = (clBlack, clGreen, clBlue);
begin
Result:=UnitColors[Unit__.UnitSource]
end;

procedure VM_Unit.Draw(ACanvas: TCanvas; ARect: TRect;
  AIsSelected: Boolean);
begin
ARect.Left:=ARect.Left+2;
ARect.Right:=ARect.Right-4;

UI.DrawCodeObject(ACanvas, ARect.Left, ARect.Top+1, CodeObject.ObjectType);
ARect.Left:=ARect.Left+20;

if AIsSelected then
  ACanvas.Font.Color:=clHighlightText
else
  ACanvas.Font.Color:=Color;

MyDrawText(ACanvas.Handle, UnitSource, ARect, DT_VCENTER or DT_SINGLELINE or DT_RIGHT);
MyDrawText(ACanvas.Handle, Caption, ARect, DT_VCENTER or DT_SINGLELINE);
end;

function VM_Unit.GetCaption: String;
begin
Result:=Unit__.Caption(False)
end;

function VM_Unit.UnitSource: String;
const
  UnitTypes: array[TUnitSource] of String = ('Unknown unit', 'Project unit', 'Framework unit');
begin
Result:=UnitTypes[Unit__.UnitSource]
end;

function VM_Unit.Unit__: TBaseCodeUnit;
begin
Result:=TBaseCodeUnit(CodeObject)
end;

{ m_COD_DlgResult_Base }

constructor m_COD_DlgResult_Base.Create;
begin
inherited Create
end;

{ VM_UnitElement }

procedure VM_UnitElement.Draw(ACanvas: TCanvas; ARect: TRect;
  AIsSelected: Boolean);
begin
ARect.Left:=ARect.Left+2;
ARect.Right:=ARect.Right-4;

UI.DrawCodeObject(ACanvas, ARect.Left, ARect.Top+1, UnitElement.ObjectType);
ARect.Left:=ARect.Left+20;

ACanvas.Font.Style:=[];

if not AIsSelected then ACanvas.Font.Color:=clBlue;
MyDrawText(ACanvas.Handle, UnitElement.TypeName+' in '+UnitElement.Unit_.Caption(True),
  ARect, DT_VCENTER or DT_SINGLELINE or DT_Right);

if not AIsSelected then ACanvas.Font.Color:=clBlack;
MyDrawText(ACanvas.Handle, UnitElement.Name, ARect, DT_VCENTER or DT_SINGLELINE);
end;

function VM_UnitElement.GetCaption: String;
begin
Result:=UnitElement.Name;
end;

function VM_UnitElement.UnitElement: TUnitElement;
begin
Result:=TUnitElement(CodeObject)
end;

end.
