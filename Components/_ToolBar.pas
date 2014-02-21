unit _ToolBar;

interface

uses SysUtils, Windows, Messages, Controls, Classes, Graphics,
  ActnList, _GDI, _Misc, Menus, _VCL_ZZZ;

type
  TMyEdgeBorder = (mebLeft, mebTop, mebRight, mebBottom);
  TMyEdgeBorders = set of TMyEdgeBorder;

type
  TMyControlOrientation = (mcoHorizontal, mcoVertical);

type TacToolBar = class;

  TacToolControl = class(TGraphicControl)
  private
    FToolBar: TacToolBar;
    function GetIndex: Integer;
  protected
    function CalcWidth: Integer; virtual;
    function CalcHeight: Integer; virtual;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    property Index: Integer read GetIndex;
  end;

  TacToolSeparator = class(TacToolControl)
  protected
    procedure Paint; override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
  end;

  TacToolButton = class(TacToolControl)
  private
    FMouseIn: Boolean;
    FMouseDown: Boolean;
    FImageIndex: Integer;
    FDown: Boolean;
    FShowCaption: Boolean;
    FDropDownMenu: TPopupMenu;
    FOwnsDropDownMenu: Boolean;
    procedure SetImageIndex(const Value: Integer);
    procedure SetDown(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
  protected
    procedure CMMouseIn(var M: TMessage); message CM_MOUSEENTER;
    procedure CMMouseOut(var M: TMessage); message CM_MOUSELEAVE;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    function CalcWidth: Integer; override;
    function CalcHeight: Integer; override;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property OnClick;
    property Action;
    property Caption;
    property Font;
    property ParentFont;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Hint;
    property Down: Boolean read FDown write SetDown;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property OwnsDropDownMenu: Boolean read FOwnsDropDownMenu write FOwnsDropDownMenu default False;
  end;

  TacToolBar = class(TCustomControl)
  private
    FFlat: Boolean;
    FButtons: TList;
    FImages: TImageList;
    FEdgeBorders: TMyEdgeBorders;
    FOrientation: TMyControlOrientation;
    FButtonMargin: Integer;
    function GetButtons(Index: Integer): TacToolControl;
    procedure SetImages(const Value: TImageList);
    procedure SetFlat(const Value: Boolean);
    procedure SetEdgeBorders(const Value: TMyEdgeBorders);
    procedure SetOrientation(const Value: TMyControlOrientation);
    procedure SetButtonMargin(const Value: Integer);
    procedure UpdateSize;
  public
    function AddButton: TacToolButton;
    function AddSeparator: TacToolSeparator;
    procedure AddCustom(AControl: TacToolControl);
    property Buttons[Index: Integer]: TacToolControl read GetButtons;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure RemoveButton(AButton: TacToolControl);
    procedure Paint; override;
    procedure AdjustButtons(StartFrom: Integer = 0);
  published
    property EdgeBorders: TMyEdgeBorders read FEdgeBorders write SetEdgeBorders default [];
    property Align;
    property Images: TImageList read FImages write SetImages;
    property Flat: Boolean read FFlat write SetFlat;
    property Color;
    property Orientation: TMyControlOrientation read FOrientation write SetOrientation;
    property ButtonMargin: Integer read FButtonMargin write SetButtonMargin default 6;
  end;

type
  TacToolButtonActionLink = class(TControlActionLink)
  protected
    FClient: TacToolButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

procedure Register;

implementation

{.$R *.dcr}

procedure Register;
begin
RegisterComponents('My components', [TacToolBar]);
end;

const
  SepWidth = 8;
  EdgeWidth = 2;

procedure TacToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var S: String; actn: TCustomAction;
begin
inherited ActionChange(Sender, CheckDefaults);
if (Sender is TCustomAction) then
	begin
  actn:=TCustomAction(Sender);

  S:=actn.Caption;
  if (Length(S)>3) and (copy(S, Length(S)-2, 3)='...') then
    Caption:=copy(S, 1, Length(S)-3);

	ImageIndex:=actn.ImageIndex;
	end;
end;

function TacToolButton.CalcHeight: Integer;
var B1, B2: Boolean; I1, I2: Integer;
begin
if FToolBar.Orientation=mcoHorizontal then
  Result:=Height
else
	begin
	Result:=FToolBar.FButtonMargin+FToolBar.FButtonMargin;
	B1:=(Caption<>'') and FShowCaption;
	B2:=(ImageIndex<>-1) and (FToolBar.FImages<>NIL);
	if B1 then
    I1:=Canvas.TextHeight(Caption)
  else
    I1:=0;
	if B2 then
    I2:=FToolBar.Images.Height
  else
	  I2:=0;
	Result:=Result+Max(I1, I2)
	end;
end;

function TacToolButton.CalcWidth: Integer;
var B1, B2: Boolean;
begin
Result:=FToolBar.ButtonMargin+FToolBar.ButtonMargin;
B1:=(Caption<>'') and FShowCaption;
B2:=(ImageIndex<>-1) and (FToolBar.FImages<>NIL);
if B1 then
  Result:=Result+Canvas.TextWidth(Caption);
if B2 then
  Result:=Result+FToolBar.FImages.Width;
if B1 and B2 then Result:=Result+FToolBar.ButtonMargin;
end;

procedure TacToolButton.Click;
begin
try
  if Assigned(FDropDownMenu) then
    TrackMenuAtControl(FDropDownMenu, Self)
  else
    inherited Click
except
  FMouseDown:=False;
  raise
end; // try
end;

procedure TacToolButton.CMEnabledChanged(var Message: TMessage);
begin
inherited;
if not Enabled then
  begin
  FMouseIn:=False;
  FMouseDown:=False;
  Invalidate;
  end;
end;

procedure TacToolButton.CMMouseIn(var M: TMessage);
begin
inherited;
if Enabled then
	begin
	FMouseIn:=True;
	if FToolBar.Flat then Invalidate;
	end;
end;

procedure TacToolButton.CMMouseOut(var M: TMessage);
begin
inherited;
if Enabled then
	begin
	FMouseIn:=False;
	if FToolBar.Flat then Invalidate;
	end;
end;

procedure TacToolButton.CMTextChanged(var Message: TMessage);
begin
inherited;
FToolBar.AdjustButtons(Index);
end;

constructor TacToolButton.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FDown:=False;
FMouseIn:=False;
FMouseDown:=False;
Caption:='';
FImageIndex:=-1;
FShowCaption:=True;
FOwnsDropDownMenu:=False;
Width:=FToolBar.Height;
end;

destructor TacToolButton.Destroy;
begin
if Assigned(FDropDownMenu) and FOwnsDropDownMenu then
  FreeAndNIL(FDropDownMenu); 
inherited Destroy;
end;

function TacToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
Result:=TacToolButtonActionLink;
end;

procedure TacToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button=mbLeft then
  begin
  SetCaptureControl(Self);
  FMouseDown:=True;
  Invalidate;
  end;
inherited MouseDown(Button, Shift, X, Y);
end;

procedure TacToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
inherited MouseUp(Button, Shift, X, Y);
if (Button=mbLeft) and FMouseDown then
  begin
  SetCaptureControl(NIL);
  FMouseDown:=False;
  Invalidate;
  end;
end;

procedure TacToolButton.Paint;

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

var R, R1: TRect; Fmt: Word; DownOffset: Integer;
begin
R:=ClientRect;

Canvas.Brush.Color:=Color;
Canvas.FillRect(R);

if FToolBar.FFlat then
  DrawFlatFrame(R)
else
  DrawRegularFrame(R);

if FDown or (FMouseIn and FMouseDown) then
	begin
  if FToolBar.FFlat then DownOffset:=1 else DownOffset:=2
	end
else
  DownOffset:=0;

Fmt:=DT_VCENTER or DT_SINGLELINE;
if (FToolBar.FImages<>NIL) and (FImageIndex<>-1) then
  begin
  FToolBar.FImages.Draw(Canvas, R.Left+FToolBar.ButtonMargin+DownOffset,
    R.Top+FToolBar.ButtonMargin+DownOffset, ImageIndex, Enabled);
  R.Left:=R.Left+FToolBar.ButtonMargin+FToolBar.FImages.Width+FToolBar.ButtonMargin;
  end
else
  Fmt:=Fmt or DT_CENTER;

if (Caption<>'') and FShowCaption then
	begin
	SetBkMode(Canvas.Handle, TRANSPARENT);
	Canvas.Font.Assign(Font);
	R.Left:=R.Left+DownOffset;
	R.Right:=R.Right+DownOffset;
	R.Top:=R.Top+DownOffset;
	R.Bottom:=R.Bottom+DownOffset;

	if not Enabled then
		begin
	  R1:=R;
	  GrowRect(R1, 1, 1, 1, 1);
		Canvas.Font.Color:=clBtnHighlight;
	  MyDrawText(Canvas.Handle, Caption, R1, Fmt);
	  Canvas.Font.Color:=clGrayText;
		end;

	MyDrawText(Canvas.Handle, Caption, R, Fmt);
	end;
end;

procedure TacToolButton.SetDown(const Value: Boolean);
begin
FDown:=Value;
Invalidate;
end;

procedure TacToolButton.SetImageIndex(const Value: Integer);
begin
FImageIndex:=Value;
FToolBar.AdjustButtons(Index);
end;

{ TacToolControl }

constructor TacToolControl.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
if AOwner=NIL then raise Exception.Create('Error creating TacToolControl - Owner can''t be NIL');
FToolBar:=TacToolBar(AOwner);
end;

function TacToolControl.GetIndex: Integer;
begin
Result:=FToolBar.FButtons.IndexOf(Self)
end;

function TacToolControl.CalcWidth: Integer;
begin
Result:=Height
end;

procedure TacToolControl.CMVisibleChanged(var Message: TMessage);
begin
inherited;
FToolBar.AdjustButtons(Index);
end;

function TacToolControl.CalcHeight: Integer;
begin
Result:=Height;
end;

{ TacToolBar }

function TacToolBar.AddButton: TacToolButton;
begin
Result:=TacToolButton.Create(Self);
Result.Parent:=Self;
AdjustButtons(FButtons.Add(Result))
end;

procedure TacToolBar.AddCustom(AControl: TacToolControl);
begin
AControl.Parent:=Self;
AdjustButtons(FButtons.Add(AControl))
end;

function TacToolBar.AddSeparator: TacToolSeparator;
begin
Result:=TacToolSeparator.Create(Self);
Result.Parent:=Self;
AdjustButtons(FButtons.Add(Result))
end;

procedure TacToolBar.AdjustButtons(StartFrom: Integer);
var I, L, T: Integer; Btn: TacToolControl;
begin
if FOrientation=mcoHorizontal then
	begin
	if StartFrom=0 then
	  L:=0
	else
	  begin
	  Btn:=Buttons[StartFrom-1];
	  L:=Btn.Left+Btn.Width;
	  end;
	L:=L+EdgeWidth*Integer(mebLeft in FEdgeBorders);

	T:=EdgeWidth*Integer(mebTop in FEdgeBorders);
	for i:=StartFrom to FButtons.Count-1 do
	  begin
	  Btn:=Buttons[i];
	  if Btn.Visible then
		  begin
		  Btn.Left:=L;
		  Btn.Top:=T;
	    Btn.Height:=Height-EdgeWidth*Integer(mebTop in FEdgeBorders)-EdgeWidth*Integer(mebBottom in FEdgeBorders);
      Btn.Width:=Btn.CalcWidth;
		  L:=L+Btn.Width;
		  end;
	  end;
	end
else
  begin
	if StartFrom=0 then
	  T:=0
	else
	  begin
	  Btn:=Buttons[StartFrom-1];
	  T:=Btn.Top+Btn.Height;
	  end;
	T:=T+EdgeWidth*Integer(mebTop in FEdgeBorders);

	L:=EdgeWidth*Integer(mebLeft in FEdgeBorders);
	for i:=StartFrom to FButtons.Count-1 do
	  begin
	  Btn:=Buttons[i];
	  if Btn.Visible then
		  begin
		  Btn.Left:=L;
		  Btn.Top:=T;
	    Btn.Height:=Btn.CalcHeight;
      Btn.Width:=Width-EdgeWidth*Integer(mebLeft in FEdgeBorders)-EdgeWidth*Integer(mebRight in FEdgeBorders);
		  T:=T+Btn.Height;
		  end;
	  end;
  end;
end;

procedure TacToolBar.Clear;
var I: Integer;
begin
for i:=0 to FButtons.Count-1 do Buttons[i].Free;
FButtons.Clear;
end;

constructor TacToolBar.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FButtons:=TList.Create;
FFlat:=True;
FEdgeBorders:=[];
FButtonMargin:=6;
end;

destructor TacToolBar.Destroy;
begin
Clear;
FButtons.Free;
inherited Destroy;
end;

function TacToolBar.GetButtons(Index: Integer): TacToolControl;
begin
Result:=TacToolControl(FButtons[Index])
end;

procedure TacToolBar.Paint;
begin
inherited Paint;

Canvas.Pen.Color:=clBtnHighlight;
if mebLeft in FEdgeBorders then
	begin
	Canvas.MoveTo(1, 1);
  Canvas.LineTo(1, Height);
	end;
if mebTop in FEdgeBorders then
	begin
	Canvas.MoveTo(1, 1);
  Canvas.LineTo(Width, 1);
	end;

Canvas.Pen.Color:=clBtnShadow;
if mebLeft in FEdgeBorders then
	begin
	Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height);
	end;
if mebTop in FEdgeBorders then
	begin
	Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width, 0);
	end;

if mebBottom in FEdgeBorders then
  begin
	Canvas.MoveTo(0, Height-2);
  Canvas.LineTo(Width, Height-2);
  end;

if mebRight in FEdgeBorders then
  begin
	Canvas.MoveTo(Width-2, 0);
  Canvas.LineTo(Width-2, Height);
  end;

Canvas.Pen.Color:=clBtnHighlight;

if mebBottom in FEdgeBorders then
  begin
	Canvas.MoveTo(0, Height-1);
  Canvas.LineTo(Width, Height-1);
  end;

if mebRight in FEdgeBorders then
  begin
	Canvas.MoveTo(Width-1, 0);
  Canvas.LineTo(Width-1, Height);
  end;
end;

procedure TacToolBar.RemoveButton(AButton: TacToolControl);
var I: Integer;
begin
I:=FButtons.IndexOf(AButton);
if I<>-1 then
  begin
  TacToolControl(FButtons[i]).Free;
  FButtons.Delete(i);
  AdjustButtons(i);
  end;
end;

procedure TacToolBar.SetButtonMargin(const Value: Integer);
begin
FButtonMargin:=Value;
UpdateSize
end;

procedure TacToolBar.SetEdgeBorders(const Value: TMyEdgeBorders);
begin
FEdgeBorders:=Value;
UpdateSize
end;

procedure TacToolBar.SetFlat(const Value: Boolean);
begin
FFlat:=Value;
Invalidate;
end;

procedure TacToolBar.SetImages(const Value: TImageList);
begin
FImages:=Value;
if FImages<>NIL then UpdateSize
end;

procedure TacToolBar.SetOrientation(const Value: TMyControlOrientation);
begin
FOrientation:=Value;
AdjustButtons;
end;

procedure TacToolBar.UpdateSize;
var I: Integer;
begin
if FOrientation=mcoHorizontal then
  begin
  if Assigned(FImages) then I:=FImages.Height else I:=0;
  I:=I+ButtonMargin+ButtonMargin
    +EdgeWidth*Integer(mebTop in FEdgeBorders)
    +EdgeWidth*Integer(mebBottom in FEdgeBorders);
  Height:=I;
  end
else
  begin
  if Assigned(FImages) then I:=FImages.Width else I:=0;
  I:=I+ButtonMargin+ButtonMargin
    +EdgeWidth*Integer(mebLeft in FEdgeBorders)
    +EdgeWidth*Integer(mebRight in FEdgeBorders);
  Width:=I;
  end;
AdjustButtons;
end;

{ TacToolSeparator }

function TacToolSeparator.CalcHeight: Integer;
begin
if FToolBar.Orientation=mcoVertical then
  Result:=SepWidth
else
  Result:=FToolBar.Height;
end;

function TacToolSeparator.CalcWidth: Integer;
begin
if FToolBar.Orientation=mcoHorizontal then
  Result:=SepWidth
else
  Result:=FToolBar.Width;
end;

procedure TacToolSeparator.Paint;
var I: Integer; R: TRect;
begin
R:=ClientRect;
Canvas.Brush.Color:=Color;
Canvas.FillRect(R);
if FToolBar.Orientation=mcoHorizontal then
	begin
	I:=Width div 2;
	Canvas.Pen.Color:=clBtnShadow;
	Canvas.MoveTo(I-1, R.Top+2);
	Canvas.LineTo(I-1, R.Bottom-2);
	Canvas.Pen.Color:=clBtnHighlight;
	Canvas.MoveTo(I, R.Top+3);
	Canvas.LineTo(I, R.Bottom-1);
	end
else
	begin
	I:=Height div 2;
	Canvas.Pen.Color:=clBtnShadow;
	Canvas.MoveTo(R.Left+2, I-1);
	Canvas.LineTo(R.Right-2, I-1);
	Canvas.Pen.Color:=clBtnHighlight;
	Canvas.MoveTo(R.Left+3, I);
	Canvas.LineTo(R.Right-1, I);
	end
end;

{ TacToolButtonActionLink }

procedure TacToolButtonActionLink.AssignClient(AClient: TObject);
begin
inherited AssignClient(AClient);
FClient:=AClient as TacToolButton;
end;

function TacToolButtonActionLink.IsCheckedLinked: Boolean;
begin
Result:=inherited IsCheckedLinked and (FClient.Down=(Action as TCustomAction).Checked);
end;

function TacToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
Result:=inherited IsImageIndexLinked and (FClient.ImageIndex=(Action as TCustomAction).ImageIndex);
end;

procedure TacToolButtonActionLink.SetChecked(Value: Boolean);
begin
if IsCheckedLinked then FClient.Down:=Value;
end;

procedure TacToolButtonActionLink.SetImageIndex(Value: Integer);
begin
if IsImageIndexLinked then FClient.ImageIndex:=Value;
end;

procedure TacToolButton.SetShowCaption(const Value: Boolean);
begin
FShowCaption:=Value;
FToolBar.AdjustButtons(Index);
end;

end.