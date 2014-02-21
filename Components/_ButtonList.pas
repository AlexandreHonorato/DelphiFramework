unit _ButtonList;

interface

uses Windows, Messages, Classes, Controls, ComCtrls, ImgList, Graphics,
  ExtCtrls, Forms, _GDI;

type
  TCustomHeaderPanel = class;

  TBGHeader = class(TComponent)
  private
    FHP: TCustomHeaderPanel;
    FVisible: Boolean;
    FText: String;
    FFont: TFont;
    FHeight: Integer;
    FCenterText: Boolean;
    FColor: TColor;
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: String);
    procedure SetVisible(const Value: Boolean);
    procedure CalcHeight;
    function GetHeight: Integer;
    procedure FontOnChange(Sender: TObject);
    procedure SetCenterText(const Value: Boolean);
    procedure SetColor(const Value: TColor);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Height: Integer read GetHeight;
  published
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write SetVisible default False;
    property Text: String read FText write SetText;
    property CenterText: Boolean read FCenterText write SetCenterText default False;
    property Color: TColor read FColor write SetColor default clHighlight;
  end;

  TCustomHeaderPanel = class(TCustomControl)
  private
    SFTop, SFBottom, SFLeft, SFRight: Integer;
    FHeader: TBGHeader;
    FSizeable: Boolean;
    FBorder: Boolean;
    procedure SetSizeable(const Value: Boolean);
    procedure SetBorder(const Value: Boolean);
  protected
    procedure WMNCHitTest(var M: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure AdjustContents; virtual;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Color;
    property Visible;
    property Header: TBGHeader read FHeader;
    property Sizeable: Boolean read FSizeable write SetSizeable default False;
    property Border: Boolean read FBorder write SetBorder default False;
  end;

type
  TButtonList = class;

  TBGButton = class(TGraphicControl)
  private
    FBG: TButtonList;
    FMouseIn: Boolean;
    FMouseDown: Boolean;
    FCaption: String;
    FImageIndex: Integer;
    FDown: Boolean;
    procedure SetCaption(const Value: String);
    function GetButtonIndex: Integer;
    function GetCaption: String;
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetDown(const Value: Boolean);
    function GetIndex: Integer;
  protected
    procedure CMMouseIn(var M: TMessage); message CM_MOUSEENTER;
    procedure CMMouseOut(var M: TMessage); message CM_MOUSELEAVE;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: String read GetCaption write SetCaption;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property ButtonIndex: Integer read GetButtonIndex; { TODO : непонятно, зачем отдельные свойства Index & ButtonIndex }
    property OnClick;

//    property Action;

    property Font;
    property ParentFont;

    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Hint;

    property Down: Boolean read FDown write SetDown;
    property Index: Integer read GetIndex;
  end;

  TGetTextEvent = procedure(Sender: TObject; Btn: TBGButton; Index: Integer; var ACaption: String; var Handled: Boolean) of object;
  TGetImageIndexEvent = procedure(Sender: TObject; Btn: TBGButton; Index: Integer; var AImageIndex: Integer; var Handled: Boolean) of object;
  TButtonClickEvent = procedure(Sender: TObject; Btn: TBGButton; Index: Integer; var Handled: Boolean) of object;
  TButtonDrawEvent = procedure(Sender: TObject; Btn: TBGButton; Index: Integer; ACanvas: TCanvas; ARect: TRect; var Handled: Boolean) of object;

  TButtonList = class(TCustomHeaderPanel)
  private
    FButtons: TList;
    FButtonHeight: Integer;
    FButtonMargin: Integer;
    FGetButtonCaption: TGetTextEvent;
    FGetButtonImageIndex: TGetImageIndexEvent;
    FImages: TImageList;
    FImageLeftMargin: Integer;
    FImageTextMargin: Integer;
    FButtonLeftMargin: Integer;
    FButtonTopMargin: Integer;
    FOnButtonClick: TButtonClickEvent;
    FFlat: Boolean;
    FDownButton: TBGButton;
    FOnButtonDraw: TButtonDrawEvent;
    function GetButtons(Index: Integer): TBGButton;
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonMargin(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    procedure SetImageLeftMargin(const Value: Integer);
    procedure SetImageTextMargin(const Value: Integer);
    procedure SetButtonLeftMargin(const Value: Integer);
    procedure SetButtonTopMargin(const Value: Integer);
    procedure SetFlat(const Value: Boolean);
    procedure SetDownButton(const Value: TBGButton);
  protected
    procedure AdjustContents; override;
    function DoGetButtonCaption(Index: Integer; var ACaption: String): Boolean;
    function DoGetButtonImageIndex(Index: Integer; var AImageIndex: Integer): Boolean;
    function DoDrawButton(Index: Integer): Boolean;
    function DoButtonClick(Index: Integer): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AdjustButtons(StartFrom: Integer = 0);
    procedure Resize; override;
  public
    function AddButton: TBGButton;
    function ButtonCount: Integer;
    property Buttons[Index: Integer]: TBGButton read GetButtons;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure DeleteButton(Index: Integer);
    procedure RemoveButton(AButton: TBGButton);
    procedure Clear;
    property DownButton: TBGButton read FDownButton write SetDownButton;
    procedure AdjustHeight;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property Flat: Boolean read FFlat write SetFlat default True;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonMargin: Integer read FButtonMargin write SetButtonMargin default 0;
    property GetButtonCaption: TGetTextEvent read FGetButtonCaption write FGetButtonCaption;
    property GetButtonImageIndex: TGetImageIndexEvent read FGetButtonImageIndex write FGetButtonImageIndex;
    property OnButtonClick: TButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnButtonDraw: TButtonDrawEvent read FOnButtonDraw write FOnButtonDraw;
    property Images: TImageList read FImages write SetImages;
    property ImageLeftMargin: Integer read FImageLeftMargin write SetImageLeftMargin default 6;
    property ImageTextMargin: Integer read FImageTextMargin write SetImageTextMargin default 4;
    property ButtonTopMargin: Integer read FButtonTopMargin write SetButtonTopMargin default 0;
    property ButtonLeftMargin: Integer read FButtonLeftMargin write SetButtonLeftMargin default 0;
  end;

type THeaderPanel = class(TCustomHeaderPanel)
  protected
    procedure AdjustContents; override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

const SizeFrameWidth = 3;

{$R *.DCR}

procedure Register;
begin
RegisterComponents('My components', [TButtonList, THeaderPanel]);
end;

{ TButtonGroup }

function TButtonList.AddButton: TBGButton;
begin
Result:=TBGButton.Create(Self);
Result.Parent:=Self;
AdjustButtons(FButtons.Add(Result));
end;

procedure TButtonList.AdjustButtons(StartFrom: Integer = 0);
var I, T: Integer; Btn: TBGButton;
begin
if StartFrom=0 then
  begin
  T:=FButtonTopMargin;
  if Header.Visible then T:=T+Header.Height;
  end
else
  T:=Buttons[StartFrom-1].Top+FButtonHeight+FButtonMargin;
T:=T+SFTop;

for i:=StartFrom to FButtons.Count-1 do
  begin
  Btn:=Buttons[i];
  Btn.SetBounds(FButtonLeftMargin+SFLeft, T, Width-2*FButtonLeftMargin-SFRight, FButtonHeight);
  T:=T+FButtonHeight+FButtonMargin;
  end;
end;

procedure TButtonList.AdjustContents;
begin
AdjustButtons;
end;

procedure TButtonList.AdjustHeight;
var h: Integer;
begin
h:=2*ButtonTopMargin+ButtonCount*ButtonHeight;
if Header.Visible then h:=h+Header.Height;
Height:=h
end;

function TButtonList.ButtonCount: Integer;
begin
Result:=FButtons.Count;
end;

procedure TButtonList.Clear;
var I: Integer;
begin
FDownButton:=NIL;
for i:=0 to FButtons.Count-1 do Buttons[i].Free;
FButtons.Clear;
end;

constructor TButtonList.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FButtons:=TList.Create;
FButtonHeight:=22;
FButtonMargin:=0;
FImageLeftMargin:=6;
FImageTextMargin:=4;
FButtonLeftMargin:=0;
FButtonTopMargin:=0;
FFlat:=True;
FDownButton:=NIL;
end;

procedure TButtonList.DeleteButton(Index: Integer);
begin
RemoveButton(Buttons[Index]);
end;

destructor TButtonList.Destroy;
var I: Integer;
begin
for i:=0 to FButtons.Count-1 do Buttons[i].Free;
FButtons.Free;
inherited Destroy;
end;

function TButtonList.DoButtonClick(Index: Integer): Boolean;
begin
if Assigned(FOnButtonClick) then
  begin
  Result:=True;
  FOnButtonClick(Self, Buttons[Index], Index, Result);
  end
else
  Result:=False;
end;

function TButtonList.DoDrawButton(Index: Integer): Boolean;
var Btn: TBGButton;
begin
if Assigned(FOnButtonDraw) then
  begin
  Result:=True;
  Btn:=Buttons[Index];
  FOnButtonDraw(Self, Btn, Index, Btn.Canvas, Btn.ClientRect, Result);
  end
else
  Result:=False;
end;

function TButtonList.DoGetButtonCaption(Index: Integer; var ACaption: String): Boolean;
begin
ACaption:='';
if Assigned(FGetButtonCaption) then
  begin
  Result:=True;
  FGetButtonCaption(Self, Buttons[Index], Index, ACaption, Result);
  end
else
  Result:=False;
end;

function TButtonList.DoGetButtonImageIndex(Index: Integer; var AImageIndex: Integer): Boolean;
begin
AImageIndex:=-1;
if Assigned(FGetButtonImageIndex) then
  begin
  Result:=True;
  FGetButtonImageIndex(Self, Buttons[Index], Index, AImageIndex, Result);
  end
else
  Result:=False;
end;

function TButtonList.GetButtons(Index: Integer): TBGButton;
begin
Result:=TBGButton(FButtons[Index]);
end;

procedure TButtonList.Notification(AComponent: TComponent; Operation: TOperation);
begin
inherited Notification(AComponent, Operation);
if (Operation=opRemove) and (AComponent=FImages) then Images:=NIL;
end;

procedure TButtonList.RemoveButton(AButton: TBGButton);
var I: Integer;
begin
I:=FButtons.IndexOf(AButton);
FButtons.Remove(AButton);
AButton.Free;
AdjustButtons(I);
end;

procedure TButtonList.Resize;
begin
inherited Resize;
AdjustButtons;
end;

procedure TButtonList.SetButtonHeight(const Value: Integer);
begin
FButtonHeight:=Value;
AdjustButtons;
end;

procedure TButtonList.SetButtonLeftMargin(const Value: Integer);
begin
FButtonLeftMargin:=Value;
AdjustButtons;
end;

procedure TButtonList.SetButtonMargin(const Value: Integer);
begin
FButtonMargin:=Value;
AdjustButtons;
end;

procedure TButtonList.SetButtonTopMargin(const Value: Integer);
begin
FButtonTopMargin:=Value;
AdjustButtons;
end;

procedure TButtonList.SetDownButton(const Value: TBGButton);
begin
if Value=FDownButton then exit;
if FDownButton<>NIL then FDownButton.Down:=False;
FDownButton:=Value;
if FDownButton<>NIL then FDownButton.Down:=True;
end;

procedure TButtonList.SetFlat(const Value: Boolean);
var I: Integer;
begin
FFlat:=Value;
for i:=0 to ButtonCount-1 do
  Buttons[i].Invalidate
end;

procedure TButtonList.SetImageLeftMargin(const Value: Integer);
begin
FImageLeftMargin:=Value;
Invalidate
end;

procedure TButtonList.SetImages(const Value: TImageList);
begin
FImages:=Value;
Invalidate;
end;

procedure TButtonList.SetImageTextMargin(const Value: Integer);
begin
FImageTextMargin:=Value;
Invalidate
end;

{ TBGButton }

procedure TBGButton.Click;
begin
try
  if not FBG.DoButtonClick(GetButtonIndex) then inherited Click
finally
  FMouseDown:=False
end; // try
end;

procedure TBGButton.CMMouseIn(var M: TMessage);
begin
inherited;
FMouseIn:=True;
Invalidate;
end;

procedure TBGButton.CMMouseOut(var M: TMessage);
begin
inherited;
FMouseIn:=False;
Invalidate;
end;

constructor TBGButton.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
//ParentFont:=True;
FBG:=TButtonList(AOwner);
FDown:=False;
FMouseIn:=False;
FMouseDown:=False;
FCaption:='';
FImageIndex:=-1;
end;

function TBGButton.GetButtonIndex: Integer;
begin
Result:=FBG.FButtons.IndexOf(Self)
end;

function TBGButton.GetCaption: String;
begin
if not FBG.DoGetButtonCaption(ButtonIndex, Result) then Result:=FCaption;
end;

function TBGButton.GetImageIndex: Integer;
begin
if not FBG.DoGetButtonImageIndex(ButtonIndex, Result) then Result:=FImageIndex;
end;

function TBGButton.GetIndex: Integer;
begin
Result:=FBG.FButtons.IndexOf(Self)
end;

procedure TBGButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button=mbLeft then
  begin
  SetCaptureControl(Self);
  FMouseDown:=True;
  Invalidate;
  end;
inherited MouseDown(Button, Shift, X, Y);
end;

procedure TBGButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
inherited MouseUp(Button, Shift, X, Y);
if FMouseDown then
  begin
  SetCaptureControl(NIL);
  FMouseDown:=False;
  Invalidate;
  end;
end;

procedure TBGButton.Paint;

  procedure DrawFlatFrame(ARect: TRect);
  var C1, C2: TColor;
  begin
  if FDown or FMouseIn or FMouseDown then
    begin
    if FDown or (FMouseIn and FMouseDown) then
      begin
      C1:=clBtnShadow;
      C2:=clBtnHighlight;
      end
    else
      begin
      C1:=clBtnHighlight;
      C2:=clBtnShadow;
      end;
    Canvas.Pen.Color:=C1;
    Canvas.MoveTo(0, 0); Canvas.LineTo(0, ARect.Bottom);
    Canvas.MoveTo(0, 0); Canvas.LineTo(ARect.Right, 0);
    Canvas.Pen.Color:=C2;
    Canvas.MoveTo(ARect.Right-1, ARect.Bottom-1); Canvas.LineTo(0, ARect.Bottom-1);
    Canvas.MoveTo(ARect.Right-1, ARect.Bottom-1); Canvas.LineTo(ARect.Right-1, 0);
    end;
  end;

  procedure DrawRegularFrame(ARect: TRect);
  var uState: Cardinal;
  begin
  uState:=DFCS_BUTTONPUSH;
  if FDown or (FMouseDown and FMouseIn) then uState:=uState or DFCS_PUSHED;
  DrawFrameControl(Canvas.Handle, ARect, DFC_BUTTON, uState)
  end;

var R: TRect; TM: Integer; Fmt: Word;
begin
R:=ClientRect;
Canvas.Brush.Color:=Color;
Canvas.FillRect(R);

if FBG.FFlat then
  DrawFlatFrame(R)
else
  DrawRegularFrame(R);

if not FBG.DoDrawButton(ButtonIndex) then
	begin
	Fmt:=DT_VCENTER or DT_SINGLELINE;
	if FBG.FImages=NIL then
	  Fmt:=Fmt or DT_CENTER
	else
	  begin
	  TM:=(Height-FBG.FImages.Height) div 2;
	  FBG.FImages.Draw(Canvas, R.Left+FBG.ImageLeftMargin, R.Top+TM, ImageIndex);
	  R.Left:=R.Left+FBG.ImageLeftMargin+FBG.FImages.Width+FBG.ImageTextMargin;
	  end;

	Canvas.Font.Assign(Font);
	MyDrawText(Canvas.Handle, Caption, R, Fmt);
	end;
end;

procedure TBGButton.SetCaption(const Value: String);
begin
FCaption:=Value;
Invalidate
end;

procedure TBGButton.SetDown(const Value: Boolean);
begin
FDown:=Value;
Invalidate;
end;

procedure TBGButton.SetImageIndex(const Value: Integer);
begin
FImageIndex:=Value;
Invalidate
end;

{ TBGHeader }

procedure TBGHeader.Assign(Source: TPersistent);
begin
if Source is TBGHeader then
  begin
  FVisible:=TBGHeader(Source).Visible;
  FText:=TBGHeader(Source).Text;
  FFont.Assign(TBGHeader(Source).Font);
  end
else
  inherited Assign(Source);
end;

procedure TBGHeader.CalcHeight;
var SaveFont: TFont;
begin
SaveFont:=TFont.Create;
SaveFont.Assign(FHP.Canvas.Font);

FHP.Canvas.Font.Assign(FFont);
FHeight:=FHP.Canvas.TextHeight('A');

FHP.Canvas.Font.Assign(SaveFont);
SaveFont.Free;
end;

constructor TBGHeader.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
SetSubComponent(True);
FHP:=TCustomHeaderPanel(AOwner);
FFont:=TFont.Create;
FFont.Color:=clHighlightText;
FFont.OnChange:=FontOnChange;
FVisible:=False;
FCenterText:=False;
FColor:=clHighlight;
FHeight:=0;
FText:='';
end;

destructor TBGHeader.Destroy;
begin
FFont.Free;
inherited Destroy;
end;

procedure TBGHeader.FontOnChange(Sender: TObject);
begin
CalcHeight;
FHP.AdjustContents;
FHP.Invalidate;
end;

function TBGHeader.GetHeight: Integer;
begin
if FHeight=0 then CalcHeight;
Result:=FHeight
end;

procedure TBGHeader.SetCenterText(const Value: Boolean);
begin
FCenterText:=Value;
FHP.Invalidate;
end;

procedure TBGHeader.SetColor(const Value: TColor);
begin
FColor:=Value;
FHP.Invalidate;
end;

procedure TBGHeader.SetFont(const Value: TFont);
begin
FFont.Assign(Value);
end;

procedure TBGHeader.SetText(const Value: String);
begin
FText:=Value;
FHP.Invalidate;
end;

procedure TBGHeader.SetVisible(const Value: Boolean);
begin
FVisible:=Value;
FHP.AdjustContents;
FHP.Invalidate;
end;

{ THeaderPanel }

procedure TCustomHeaderPanel.AdjustContents;
begin
//do nothing
end;

constructor TCustomHeaderPanel.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FSizeable:=False;
SFTop:=0;
SFBottom:=0;
SFLeft:=0;
SFRight:=0;
FBorder:=False;

FHeader:=TBGHeader.Create(Self);

ControlStyle:=[csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks, csReplicatable];
// избавляет от моргания в Run-Time, но в Design-Time не должен быть контейнером
if not (csDesigning in ComponentState) then ControlStyle:=ControlStyle+[csAcceptsControls];

Width:=185;
Height:=41;
//FFullRepaint:=True;
end;

destructor TCustomHeaderPanel.Destroy;
begin
if Assigned(FHeader) then FHeader.Free;
inherited Destroy;
end;

procedure TCustomHeaderPanel.Paint;
var R: TRect; Flags: Word;
begin
Canvas.Brush.Color:=Color;
Canvas.FillRect(ClientRect);

if not Header.Visible then exit;

R:=ClientRect;
R.Top:=R.Top+SFTop;
R.Bottom:=R.Top+Header.Height;
R.Left:=R.Left+SFLeft;
R.Right:=R.Right-SFRight;

Canvas.Brush.Color:=Header.Color;
Canvas.FillRect(R);

Flags:=DT_VCENTER or DT_SINGLELINE;
if Header.CenterText then
  Flags:=Flags or DT_Center
else
  R.Left:=R.Left+5;

Canvas.Font.Assign(Header.Font);
MyDrawText(Canvas.Handle, Header.Text, R, Flags)
end;

procedure THeaderPanel.AdjustContents;
begin
Realign;
end;

constructor THeaderPanel.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
ControlStyle:=ControlStyle+[csAcceptsControls];
//Header.Visible:=True;
end;

{ THeaderPanel }

procedure THeaderPanel.AdjustClientRect(var Rect: TRect);
begin
inherited AdjustClientRect(Rect);
Rect.Top:=Rect.Top+SFTop;
Rect.Left:=Rect.Left+SFLeft;
Rect.Bottom:=Rect.Bottom-SFBottom;
Rect.Right:=Rect.Right-SFRight;
if FHeader.Visible then Rect.Top:=Rect.Top+FHeader.Height+1;
end;

procedure TCustomHeaderPanel.SetSizeable(const Value: Boolean);
begin
FSizeable:=Value;

SFTop:=0;
SFBottom:=0;
SFLeft:=0;
SFRight:=0;

if Value then
  case Align of
  alLeft:   SFRight:=SizeFrameWidth;
  alBottom: SFTop:=SizeFrameWidth;
  alRight:  SFLeft:=SizeFrameWidth;
  alTop:    SFBottom:=SizeFrameWidth;
  end; // case

AdjustContents;
Invalidate;
end;

procedure TCustomHeaderPanel.WMNCHitTest(var M: TWMNCHitTest);
var P: TPoint;
begin
inherited;
if (not Sizeable) or (csDesigning in ComponentState) then exit;
P:=ScreenToClient(SmallPointToPoint(M.Pos));
case Align of
alLeft:   if P.X>Width-SizeFrameWidth then M.Result:=HTRIGHT;
alBottom: if P.Y<SizeFrameWidth then M.Result:=HTTOP;
alRight:  if P.X<SizeFrameWidth then M.Result:=HTLEFT;
alTop:    if P.Y>Height-SizeFrameWidth then M.Result:=HTBOTTOM;
end; // case
end;

procedure TCustomHeaderPanel.CreateParams(var Params: TCreateParams);
begin
inherited CreateParams(Params);
with Params do
  begin
  WindowClass.style:=WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  if FBorder then
    Style:=Style or WS_BORDER
  else
    Style:=Style and not WS_BORDER
  end;
end;

procedure TCustomHeaderPanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
Invalidate;
inherited;
end;

procedure TCustomHeaderPanel.SetBorder(const Value: Boolean);
begin
if FBorder<>Value then
  begin
  FBorder:=Value;
  RecreateWnd;
  end;
end;

end.


