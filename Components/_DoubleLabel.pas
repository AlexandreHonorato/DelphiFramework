unit _DoubleLabel;

interface

uses Controls, Graphics, Classes, SysUtils, Windows, Messages, _GDI;

type TDoubleLabel = class(TGraphicControl)
  private
    FCaption2: String;
    FFont2: TFont;
    FRightMargin: Integer;
    FLeftMargin: Integer;
    procedure SetCaption2(const Value: String);
    procedure SetFont2(const Value: TFont);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
  protected
    procedure CMTextChanged(var M: TMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align;
    property Caption;
    property Caption2: String read FCaption2 write SetCaption2;
    property Font;
    property Font2: TFont read FFont2 write SetFont2;
    property Color;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 4;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 4;
  end;

procedure Register;

implementation

procedure Register;
begin
RegisterComponents('My components', [TDoubleLabel]);
end;

{ TDoubleLabel }

procedure TDoubleLabel.CMTextChanged(var M: TMessage);
begin
inherited;
Invalidate;
end;

constructor TDoubleLabel.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FFont2:=TFont.Create;
FFont2.Assign(Font);

FLeftMargin:=4;
FRightMargin:=4;

Width:=50;
Height:=16;
end;

destructor TDoubleLabel.Destroy;
begin
if Assigned(FFont2) then FreeAndNIL(FFont2);
inherited Destroy;
end;

procedure TDoubleLabel.Paint;
var Rect: TRect;
begin
Rect:=ClientRect;

Canvas.Brush.Color:=Color;
Canvas.FillRect(Rect);

Canvas.Font.Assign(Font);
Rect.Left:=Rect.Left+FLeftMargin;
MyDrawText(Canvas.Handle, Caption, Rect, DT_VCENTER or DT_SINGLELINE);

Canvas.Font.Assign(Font2);
Rect.Right:=Rect.Right-FRightMargin;
MyDrawText(Canvas.Handle, Caption2, Rect, DT_VCENTER or DT_SINGLELINE or DT_RIGHT);
end;

procedure TDoubleLabel.SetCaption2(const Value: String);
begin
FCaption2:=Value;
Invalidate;
end;

procedure TDoubleLabel.SetFont2(const Value: TFont);
begin
FFont2.Assign(Value);
Invalidate;
end;

procedure TDoubleLabel.SetLeftMargin(const Value: Integer);
begin
FLeftMargin:=Value;
Invalidate;
end;

procedure TDoubleLabel.SetRightMargin(const Value: Integer);
begin
FRightMargin:=Value;
Invalidate;
end;

end.
