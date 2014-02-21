unit _CaptionBevel;

interface

uses Windows, Messages, Classes, Graphics, Controls;

type TCaptionBevel = class(TGraphicControl)
  protected
    procedure Paint; override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property Constraints;
    property Caption;
    property Font;
    property Color default clBtnFace;
    property ParentFont;
    property ParentColor;
  end;

procedure Register;

implementation

{$R *.DCR}

procedure Register;
begin
RegisterComponents('My components', [TCaptionBevel]);
end;

procedure TCaptionBevel.CMTextChanged(var Message: TMessage);
begin
Invalidate
end;

constructor TCaptionBevel.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
Color:=clBtnFace
end;

procedure TCaptionBevel.Paint;
var R: TRect; H: Integer;
begin
Canvas.Brush.Style:=bsClear;
Canvas.FillRect(BoundsRect);

Canvas.Font.Assign(Font);
H:=Canvas.TextHeight('A') div 2;

R:=Bounds(1, 1+H, Width-1, Height-1-H);
Canvas.Brush.Color:=clBtnHighlight;
Canvas.FrameRect(R);

R:=Bounds(0, H, Width-1, Height-1-H);
Canvas.Brush.Color:=clBtnShadow;
Canvas.FrameRect(R);

Canvas.Brush.Color:=Color;
Canvas.TextOut(6, 0, ' '+Caption+' ');
end;

end.
