unit uCOD_Model_UseUnit;

interface

uses uCOD_Model_Base_SingleResult, uCOD_Model_Base, uCodeEntities, SysUtils,
  Graphics, Windows, _GDI;

type VM_Unit_ForUseUnit = class(VM_Unit)
  public
    RecentIndex: Integer;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIsSelected: Boolean); override;
  end;

type m_COD_DlgResult_UseUnit = class(m_COD_DlgResult_SingleResult)
  private
    FItfSection: Boolean;
    function GetUnitName: String;
  public
    property UnitName: String read GetUnitName;
    property ItfSection: Boolean read FItfSection write FItfSection;
  end;

implementation

uses uUnits, uUI;

{ m_COD_DlgResult_UseUnit }

function m_COD_DlgResult_UseUnit.GetUnitName: String;
begin
if CodeObject.ObjectType<>coUnit then
  raise Exception.Create('Not a unit');

Result:=TBaseCodeUnit(CodeObject).Caption(False);
end;

{ VM_Unit_ForUseUnit }

procedure VM_Unit_ForUseUnit.Draw(ACanvas: TCanvas; ARect: TRect;
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

if RecentIndex=UNIT_IS_NOT_RECENT then
  ACanvas.Font.Style:=[]
else
  ACanvas.Font.Style:=[fsBold];

MyDrawText(ACanvas.Handle, UnitSource, ARect, DT_VCENTER or DT_SINGLELINE or DT_RIGHT);
MyDrawText(ACanvas.Handle, Caption, ARect, DT_VCENTER or DT_SINGLELINE);
end;

end.
