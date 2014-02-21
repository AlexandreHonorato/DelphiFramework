unit _LinkLabelMy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TLinkLabelMy = class(TCustomLabel)
  private
    FHoverFont: TFont;
    FSimpleFont: TFont;
    FMouseIn: Boolean;
    procedure SetHoverFont(const Value: TFont);
    procedure SetSimpleFont(const Value: TFont);
    procedure SimpleFontChanged(Sender: TObject);
    procedure HoverFontChanged(Sender: TObject);
  protected
    procedure CMMouseEnter(var M: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var M: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeHoverFont;
  published
    property Anchors;
    property Constraints;
    property Transparent;
    property OnClick;
    property Caption;
    property SimpleFont: TFont read FSimpleFont write SetSimpleFont;
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property Visible;
    property AutoSize;
    property Alignment;
    property Layout;
    property Hint;
    property ShowHint;
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('My components', [TLinkLabelMy]);
end;

{ TLinkLabelMy }

procedure TLinkLabelMy.CMMouseEnter(var M: TMessage);
begin
inherited;
FMouseIn:=True;
Font:=FHoverFont
end;

procedure TLinkLabelMy.CMMouseLeave(var M: TMessage);
begin
inherited;
FMouseIn:=False;
Font:=FSimpleFont
end;

constructor TLinkLabelMy.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
Cursor:=crHandPoint;

FMouseIn:=False;

FSimpleFont:=TFont.Create;
FSimpleFont.OnChange:=SimpleFontChanged;
FSimpleFont.Assign(Font);

FHoverFont:=TFont.Create;
FHoverFont.OnChange:=HoverFontChanged;
MakeHoverFont;
end;

destructor TLinkLabelMy.Destroy;
begin
FHoverFont.Free;
FSimpleFont.Free;
inherited Destroy;
end;

procedure TLinkLabelMy.HoverFontChanged(Sender: TObject);
begin
if FMouseIn then Font.Assign(FHoverFont)
end;

procedure TLinkLabelMy.MakeHoverFont;
begin
FHoverFont.Assign(FSimpleFont);
FHoverFont.Color:=clBlue;
FHoverFont.Style:=FHoverFont.Style+[fsUnderline];
end;

procedure TLinkLabelMy.SetHoverFont(const Value: TFont);
begin
FHoverFont.Assign(Value);
Invalidate
end;

procedure TLinkLabelMy.SetSimpleFont(const Value: TFont);
begin
FSimpleFont.Assign(Value);
Invalidate
end;

procedure TLinkLabelMy.SimpleFontChanged(Sender: TObject);
begin
if not FMouseIn then Font.Assign(FSimpleFont)
end;

end.
