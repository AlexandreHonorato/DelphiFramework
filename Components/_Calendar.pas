unit _Calendar;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Graphics, Buttons, _HintWindow,
  _DateUtils, _GDI;

const
  cColCount = 7;
  cRowCount = 6;
  cBottomMargin = 18;
  cTopMargin = 22;
  cTodayMarkSize = 14;

type
  TCalSelType = (cstFillRect, cstFocusRect);

type
  TMyCalendar = class;

  TCalCell = class
  private
    FOwner: TMyCalendar;
    FDay: Integer;
    FRect: TRect;
    FGray: Boolean;
    FToday: Boolean;
    FMonth: Integer;
    FYear: Integer;
    FDate: TDate;
    FData: Integer;
    FCurrWeek: Boolean;
    FRow: Integer;
    FCol: Integer;
    procedure SetDate(const Value: TDate);
  protected
    procedure Paint(AFullPaint: Boolean);
  public
    property Row: Integer read FRow;
    property Col: Integer read FCol;
    procedure PaintCell;
    function Selected: Boolean;
    procedure SetDate2(Y, M, D: Integer);
    procedure Invalidate;
    property Gray: Boolean read FGray write FGray;
    property Day: Integer read FDay;
    property Month: Integer read FMonth;
    property Year: Integer read FYear;
    property Date: TDate read FDate write SetDate;
    property Today: Boolean read FToday;
    property Data: Integer read FData write FData;
    property CurrWeek: Boolean read FCurrWeek write FCurrWeek;
  end;

  TCalCellClickEvent = procedure(Sender: TObject; ACell: TCalCell) of object;
  TCalCellDrawEvent = procedure(Sender: TObject; ACell: TCalCell; ARect: TRect; var DefaultDrawing: Boolean) of object;
  TCalCellGetBGColor = procedure(Sender: TObject; ACell: TCalCell; var AColor: TColor) of object;
  TCalCellGetHint = procedure(Sender: TObject; ACell: TCalCell; var S: String) of object;

  TMyCalendar = class(TCustomControl)
  private
    PrevHintCell: TCalCell;
    HW: TMyHintWindow;
    FBtnNext, FBtnPrev: TSpeedButton;
    FYear, FMonth: Integer;
    FCells: array[0..cColCount-1, 0..cRowCount-1] of TCalCell;
    lstCells: TList;
    FWorkRect, FTodayRect, FMonthYearRect: TRect;
    FCellWidth, FCellHeight: Integer;
    FMarginX, FMarginY, FTodayMargin: Integer;
    FOnCellClick: TCalCellClickEvent;
    FSelected: TCalCell;
    FOnCellDraw: TCalCellDrawEvent;
    FOnCellGetBGColor: TCalCellGetBGColor;
    FOnCellDblClick: TCalCellClickEvent;
    FOnChangeMonth: TNotifyEvent;
    FclrDayText: TColor;
    FclrSelectionBG: TColor;
    FclrGrayDayBG: TColor;
    FclrWeekHeader: TColor;
    FclrDayBG: TColor;
    FclrTodayBG: TColor;
    FclrSelectionText: TColor;
    FclrCurrWeek: TColor;
    FSelType: TCalSelType;
    FOnCellHint: TCalCellGetHint;
    FShowCellHints: Boolean;
    FRightButtonSelect: Boolean;
    FShowMonth: Boolean;
    FShowToday: Boolean;
    FSelectOnlyCurrentMonth: Boolean;
    function GetICells(Index: Integer): TCalCell;
    procedure SetSelected(const Value: TCalCell);
    function GetDate: TDate;
    procedure SetDate(ADate: TDate);

    procedure btnClick(Sender: TObject);
    function GetCells(ACol, ARow: Integer): TCalCell;
    procedure SetClr(const Index: Integer; const Value: TColor);
    procedure SetSelType(const Value: TCalSelType);
    function GetClrHint: TColor;
    procedure SetClrHint(const Value: TColor);
    procedure SetShowMonth(const Value: Boolean);
    procedure SetShowToday(const Value: Boolean);
  protected
    procedure CalcCellSizes;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustButtons;
    procedure CMMouseLeave(var M: TMessage); message CM_MOUSELEAVE;
  public
    function CellFromXY(X, Y: Integer): TCalCell;
    procedure SetMonthYear(AMonth: Integer; AYear: Integer);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property iCells[Index: Integer]: TCalCell read GetICells;
    function CellCount: Integer;
    property Selected: TCalCell read FSelected write SetSelected;
    property Canvas;
    property Cells[ACol, ARow: Integer]: TCalCell read GetCells;
    property Year: Integer read FYear;
    property Month: Integer read FMonth;
  published
    property Align;
    property PopupMenu;
    property Color;
    property ParentColor;
    property OnCellClick: TCalCellClickEvent read FOnCellClick write FOnCellClick;
    property OnCellDblClick: TCalCellClickEvent read FOnCellDblClick write FOnCellDblClick;
    property OnCellDraw: TCalCellDrawEvent read FOnCellDraw write FOnCellDraw;
    property OnCellGetBGColor: TCalCellGetBGColor read FOnCellGetBGColor write FOnCellGetBGColor;
    property OnChangeMonth: TNotifyEvent read FOnChangeMonth write FOnChangeMonth;
    property OnCellHint: TCalCellGetHint read FOnCellHint write FOnCellHint;

    property ShowCellHints: Boolean read FShowCellHints write FShowCellHints default False;
    property Date: TDate read GetDate write SetDate;

    property clrDayText: TColor index 1 read FclrDayText write SetClr default clBlack;
    property clrDayBG: TColor index 2 read FclrDayBG write SetClr default clWindow;
    property clrGrayDayBG: TColor index 3 read FclrGrayDayBG write SetClr default clBtnFace;
    property clrSelectionBG: TColor index 4 read FclrSelectionBG write SetClr default clHighlight;
    property clrTodayBG: TColor index 5 read FclrTodayBG write SetClr default $008080FF;
    property clrWeekHeader: TColor index 6 read FclrWeekHeader write SetClr default clBtnShadow;
    property clrSelectionText: TColor index 7 read FclrSelectionText write SetClr default clHighlightText;
    property clrCurrWeek: TColor index 8 read FclrCurrWeek write SetClr default $00FF8080;
    property clrHint: TColor read GetClrHint write SetClrHint;

    property SelType: TCalSelType read FSelType write SetSelType default cstFillRect;
    property RightButtonSelect: Boolean read FRightButtonSelect write FRightButtonSelect default False;
    property Visible;
    property ShowMonth: Boolean read FShowMonth write SetShowMonth default True;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property SelectOnlyCurrentMonth: Boolean read FSelectOnlyCurrentMonth write FSelectOnlyCurrentMonth default False;
  end;

procedure Register;

implementation

{$R CalendarImages\CalendarImages.res}
{$R *.DCR}

var
  bmpCalNext: TBitmap;
  bmpCalPrev: TBitmap;

procedure Register;
begin
RegisterComponents('My components', [TMyCalendar]);
end;

{ TMyCalendar }

procedure TMyCalendar.AdjustButtons;
begin
if FShowMonth then
	begin
  FBtnPrev.Visible:=True;
	FBtnPrev.SetBounds(2, 2, cTopMargin-4, cTopMargin-4);

  FBtnNext.Visible:=True;
	FBtnNext.SetBounds(Width-cTopMargin+2, 2, cTopMargin-4, cTopMargin-4);
	end
else
  begin
  FBtnPrev.Visible:=False;
  FBtnNext.Visible:=False;
  end;
end;

procedure TMyCalendar.btnClick(Sender: TObject);
var _Y, _M: Integer;
begin
if Sender=FBtnNext then
  begin
  if FMonth=12 then
    begin _M:=1; _Y:=FYear+1 end
  else
    begin _M:=FMonth+1; _Y:=FYear end
  end
else
  begin
  if FMonth=1 then
    begin _M:=12; _Y:=FYear-1 end
  else
    begin _M:=FMonth-1; _Y:=FYear end
  end;
SetMonthYear(_M, _Y);
end;

procedure TMyCalendar.CalcCellSizes;
var C, R: Integer; MonthOffset, TodayOffset: Integer;
begin
if FShowMonth then MonthOffset:=cTopMargin else MonthOffset:=0;
if FShowToday then TodayOffset:=cBottomMargin else TodayOffset:=0;

FCellWidth:=Width div cColCount;
FCellHeight:=(Height-TodayOffset-MonthOffset) div (cRowCount+1);

FMarginX:=(Width-FCellWidth*cColCount) div 2;
FMarginY:=FCellHeight+MonthOffset;

FWorkRect:=Bounds(FMarginX, FMarginY, FCellWidth*cColCount, FCellHeight*cRowCount);

for R:=0 to cRowCount-1 do
  for C:=0 to cColCount-1 do
    FCells[C, R].FRect:=Bounds(FMarginX+C*FCellWidth+1, FMarginY+R*FCellHeight+1, FCellWidth-1, FCellHeight-1);

if FShowToday then
	begin
	FTodayMargin:=2+(cBottomMargin-cTodayMarkSize) div 2;
	FTodayRect:=Bounds(0, FWorkRect.Bottom+2, Width, cBottomMargin);
	end;

if FShowMonth then
  FMonthYearRect:=Bounds(cTopMargin, 0, Width-cTopMargin*2, cTopMargin);

AdjustButtons;
end;

function TMyCalendar.CellCount: Integer;
begin
Result:=lstCells.Count
end;

function TMyCalendar.CellFromXY(X, Y: Integer): TCalCell;
var C, R: Integer;
begin
Result:=NIL;
if not PtInRect(FWorkRect, Point(X, Y)) then exit;
X:=X-FMarginX;
Y:=Y-FMarginY;
C:=X div FCellWidth;
R:=Y div FCellHeight;
Result:=FCells[C, R]
end;

procedure TMyCalendar.CMMouseLeave(var M: TMessage);
begin
inherited;
HW.HideHint;
end;

constructor TMyCalendar.Create(AOwner: TComponent);
var C, R: Integer;
begin
inherited Create(AOwner);

FSelectOnlyCurrentMonth:=False;
FRightButtonSelect:=False;
FShowMonth:=True;
FShowToday:=True;

HW:=TMyHintWindow.Create(Self);

PrevHintCell:=NIL;
FSelType:=cstFillRect;
FShowCellHints:=False;

FclrDayText:=clBlack;
FclrDayBG:=clWindow;
FclrGrayDayBG:=clBtnFace;
FclrSelectionBG:=clHighlight;
FclrTodayBG:=$008080FF;
FclrWeekHeader:=clBtnShadow;
FclrSelectionText:=clHighlightText;
FclrCurrWeek:=$00FF8080;

Width:=164;
Height:=154;

FBtnPrev:=TSpeedButton.Create(Self);
FBtnPrev.Parent:=Self;
FBtnPrev.Glyph.Assign(bmpCalPrev);
FBtnPrev.OnClick:=btnClick;

FBtnNext:=TSpeedButton.Create(Self);
FBtnNext.Parent:=Self;
FBtnNext.Glyph.Assign(bmpCalNext);
FBtnNext.OnClick:=btnClick;

FSelected:=NIL;

lstCells:=TList.Create;
for R:=0 to cRowCount-1 do
  for C:=0 to cColCount-1 do
    begin
    FCells[C, R]:=TCalCell.Create;
    FCells[C, R].FOwner:=Self;
    FCells[C, R].FRow:=R;
    FCells[C, R].FCol:=C;
    lstCells.Add(FCells[C, R]);
    end;

CalcCellSizes;
SetDate(Now);
end;

destructor TMyCalendar.Destroy;
var I: Integer;
begin
for i:=0 to lstCells.Count-1 do iCells[i].Free;
lstCells.Free;
HW.Free;
inherited Destroy;
end;

function TMyCalendar.GetCells(ACol, ARow: Integer): TCalCell;
begin
Result:=FCells[ACol, ARow]
end;

function TMyCalendar.GetClrHint: TColor;
begin
Result:=HW.Color;
end;

function TMyCalendar.GetDate: TDate;
var C: TCalCell;
begin
C:=Selected;
if C=NIL then
  Result:=0
else
  Result:=C.Date
end;

function TMyCalendar.GetICells(Index: Integer): TCalCell;
begin
Result:=TCalCell(lstCells[Index]);
end;

procedure TMyCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var C: TCalCell; CanSelect: Boolean;
begin
if (Button=mbLeft) or ((Button=mbRight) and FRightButtonSelect) then
	begin
	C:=CellFromXY(X, Y);
	if (C<>NIL) then
		begin
    CanSelect:=(FSelectOnlyCurrentMonth and (C.Month=Month)) or
      (not FSelectOnlyCurrentMonth);
    if CanSelect then
		  begin
		  Selected:=C;
		  if ssDouble in Shift then
		    begin
		    if Assigned(FOnCellDblClick) then FOnCellDblClick(Self, C);
		    end
		  else
			  begin
			  if Assigned(FOnCellClick) then FOnCellClick(Self, C);
			  end;
		  end;
		end
	else
	if (Button=mbLeft) and PtInRect(FTodayRect, Point(X, Y)) then
	  begin
	  Date:=Now;
	  if Assigned(FOnCellClick) then FOnCellClick(Self, Selected);
	  end;
	end;
end;

procedure TMyCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
var C: TCalCell; S: String; P: TPoint;
begin
if (not FShowCellHints) or (not Assigned(FOnCellHint)) then exit;

C:=CellFromXY(X, Y);
if C=PrevHintCell then exit;

PrevHintCell:=C;
HW.HideHint;
if C<>NIL then
  begin
  S:='';
  FOnCellHint(Self, C, S);
  if S<>'' then
	  begin
    P:=Point(C.FRect.Left+2, C.FRect.Bottom+2);
    P:=ClientToScreen(P);
	  HW.ShowHint(P, S);
	  end;
  end;
end;

procedure TMyCalendar.Paint;
const WDNumbers: array[0..cColCount-1] of Integer = (2, 3, 4, 5, 6, 7, 1);
var C, R, I: Integer; RC: TRect; S: String;
begin
RC.Left:=FWorkRect.Left;
RC.Bottom:=FWorkRect.Top;
RC.Top:=RC.Bottom-FCellHeight;
RC.Right:=FWorkRect.Right;
Canvas.Brush.Color:=FclrWeekHeader;
Canvas.FillRect(RC);

Canvas.Brush.Color:=FclrDayBG;
Canvas.FillRect(FWorkRect);

for R:=-1 to cRowCount do
  begin
  Canvas.MoveTo(FWorkRect.Left, FMarginY+R*FCellHeight);
  Canvas.LineTo(FWorkRect.Right, FMarginY+R*FCellHeight);
  end;

for C:=0 to cColCount do
  begin
  Canvas.MoveTo(FMarginX+C*FCellWidth, FWorkRect.Top-FCellHeight);
  Canvas.LineTo(FMarginX+C*FCellWidth, FWorkRect.Bottom+1);
  end;

for i:=0 to lstCells.Count-1 do iCells[i].Paint(False);

// Draw WeekDays header
Canvas.Font.Color:=clrDayText;
Canvas.Font.Style:=[fsBold];
for C:=0 to cColCount-1 do
  begin
  RC.Left:=FMarginX+C*FCellWidth;
  RC.Right:=RC.Left+FCellWidth;
  MyDrawText(Canvas.Handle, ShortDayNames[WDNumbers[C]], RC, DT_VCENTER or DT_SINGLELINE or DT_CENTER);
  end;

// Draw month/year
if FShowMonth then
	begin
	S:=LongMonthNames[FMonth]+' '+IntToStr(FYear)+' г.';
	MyDrawText(Canvas.Handle, S, FMonthYearRect, DT_VCENTER or DT_SINGLELINE or DT_CENTER);
	end;

Canvas.Font.Style:=[];

// Draw Today
if FShowToday then
	begin
  RC:=Bounds(FTodayMargin, FWorkRect.Bottom+FTodayMargin-1, cTodayMarkSize, cTodayMarkSize);
  Canvas.Brush.Color:=FclrTodayBG;
  Canvas.FillRect(RC);

  RC:=FTodayRect;
  RC.Left:=RC.Left+cTodayMarkSize+2*FTodayMargin;
  SetBkMode(Canvas.Handle, TRANSPARENT);
  MyDrawText(Canvas.Handle, 'Сегодня: '+DateToStr(Now)+', '+LongDayNames[DayOfWeek(Now)], RC);
  end;
end;

procedure TMyCalendar.Resize;
begin
CalcCellSizes;
inherited Resize;
Invalidate
end;

procedure TMyCalendar.SetClr(const Index: Integer; const Value: TColor);
begin
case Index of
1: FclrDayText:=Value;
2: FclrDayBG:=Value;
3: FclrGrayDayBG:=Value;
4: FclrSelectionBG:=Value;
5: FclrTodayBG:=Value;
6: FclrWeekHeader:=Value;
7: FclrSelectionText:=Value;
8: FclrCurrWeek:=Value;
end; // case
Invalidate
end;

procedure TMyCalendar.SetClrHint(const Value: TColor);
begin
HW.Color:=Value
end;

procedure TMyCalendar.SetDate(ADate: TDate);
var _Y, _M, _D: Word; I: Integer;
begin
DecodeDate(ADate, _Y, _M, _D);
SetMonthYear(_M, _Y);

ADate:=Trunc(ADate);
for i:=0 to lstCells.Count-1 do
  if iCells[i].Date=ADate then
    begin
    Selected:=iCells[i];
    break;
    end;
end;

procedure TMyCalendar.SetMonthYear(AMonth, AYear: Integer);
var D: TDate; C, R, I, ML, _M, _Y: Integer; _Today: TDate;
  CurrWeekRow: Integer;
begin
if (AMonth=FMonth) and (AYear=FYear) then exit;

if (AMonth<0) or (AMonth>12) then
  raise Exception.Create('Wrong month value');
FMonth:=AMonth;
FYear:=AYear;
FSelected:=NIL;
CurrWeekRow:=-1;

D:=EncodeDate(AYear, AMonth, 1);
C:=duWeekDay(D)-1;
R:=0;

_Today:=Trunc(Now);

// Prev Month
if AMonth=1 then
  begin _M:=12; _Y:=AYear-1 end
else
  begin _M:=AMonth-1; _Y:=AYear; end;
ML:=duMonthLength(_M, _Y);
for i:=C-1 downto 0 do
  begin
  FCells[i, R].SetDate2(_Y, _M, ML-(C-1-i));
  FCells[i, R].FToday:=FCells[i, R].Date=_Today;
  FCells[i, R].Gray:=True;
  end;

// This Month
ML:=duMonthLength(AMonth, AYear);
for i:=1 to ML do
  begin
  FCells[C, R].SetDate2(AYear, AMonth, i);
  FCells[C, R].Gray:=False;
  FCells[C, R].FToday:=FCells[C, R].Date=_Today;
  if FCells[C, R].FToday then CurrWeekRow:=R;
  Inc(C);
  if C=7 then
    begin
    C:=0;
    Inc(R);
    end;
  end;

for i:=0 to lstCells.Count-1 do iCells[i].FCurrWeek:=False;

if CurrWeekRow<>-1 then
  for i:=0 to cColCount-1 do
    FCells[i, CurrWeekRow].FCurrWeek:=True;

// Next Month
i:=1;
if AMonth=12 then
  begin _M:=1; _Y:=AYear+1 end
else
  begin _M:=AMonth+1; _Y:=AYear; end;
while True do
  begin
  if (R=cRowCount) then break;
  FCells[C, R].SetDate2(_Y, _M, i);
  FCells[C, R].Gray:=True;
  FCells[C, R].FToday:=FCells[C, R].Date=_Today;
  Inc(i);
  Inc(C);
  if C=7 then
    begin
    C:=0;
    Inc(R);
    end;
  end;

if Assigned(FOnChangeMonth) then FOnChangeMonth(Self);
  
Invalidate
end;

procedure TMyCalendar.SetSelected(const Value: TCalCell);
var Save: TCalCell;
begin
if FSelected=Value then exit;

Save:=FSelected;
FSelected:=Value;

if Save<>NIL then Save.Invalidate;
if FSelected<>NIL then FSelected.Invalidate
end;

procedure TMyCalendar.SetSelType(const Value: TCalSelType);
begin
if FSelType<>Value then
	begin
	FSelType:=Value;
	Invalidate
	end;
end;

procedure TMyCalendar.SetShowMonth(const Value: Boolean);
begin
FShowMonth:=Value;
CalcCellSizes;
Invalidate;
end;

procedure TMyCalendar.SetShowToday(const Value: Boolean);
begin
FShowToday:=Value;
CalcCellSizes;
Invalidate;
end;

{ TCalCell }

procedure TCalCell.PaintCell;
begin
SetBkMode(FOwner.Canvas.Handle, TRANSPARENT);
if Selected and (FOwner.FSelType=cstFillRect) then
  FOwner.Canvas.Font.Color:=FOwner.FclrSelectionText
else
  FOwner.Canvas.Font.Color:=FOwner.FclrDayText;

MyDrawText(FOwner.Canvas.Handle, IntToStr(Day), FRect, DT_VCENTER or DT_CENTER or DT_SINGLELINE);

if Selected and (FOwner.FSelType=cstFocusRect) then
  FOwner.Canvas.DrawFocusRect(FRect);
end;

procedure TCalCell.Invalidate;
begin
Paint(True);
end;

procedure TCalCell.Paint(AFullPaint: Boolean);
var Clr: TColor; B: Boolean;
begin
if not FOwner.HandleAllocated then exit;

if Selected and (FOwner.FSelType=cstFillRect) then
	Clr:=FOwner.FclrSelectionBG
else
	begin
  if Today then
    Clr:=FOwner.FclrTodayBG
  else
  if Gray then
    Clr:=FOwner.FclrGrayDayBG
  else
  if CurrWeek then
    Clr:=FOwner.FclrCurrWeek
  else
  	Clr:=FOwner.FclrDayBG;
	end;

if Assigned(FOwner.FOnCellGetBGColor) then FOwner.FOnCellGetBGColor(FOwner, Self, Clr);

if AFullPaint or (Clr<>FOwner.FclrDayBG) then
	begin
  FOwner.Canvas.Brush.Color:=Clr;
	FOwner.Canvas.FillRect(FRect);
	end;

B:=True;
if Assigned(FOwner.FOnCellDraw) then FOwner.FOnCellDraw(FOwner, Self, FRect, B);
if B then PaintCell;
end;

procedure TCalCell.SetDate(const Value: TDate);
var Y, M, D: Word;
begin
FDate:=Trunc(Value);
DecodeDate(Value, Y, M, D);
FYear:=Y;
FMonth:=M;
FDay:=D;
end;

procedure TCalCell.SetDate2(Y, M, D: Integer);
begin
FYear:=Y;
FMonth:=M;
FDay:=D;
FDate:=EncodeDate(Y, M, D)
end;

function TCalCell.Selected: Boolean;
begin
Result:=Self=FOwner.Selected
end;

initialization
bmpCalNext:=TBitmap.Create;
bmpCalPrev:=TBitmap.Create;
bmpCalNext.LoadFromResourceName(hInstance, 'CalNext');
bmpCalPrev.LoadFromResourceName(hInstance, 'CalPrev');

finalization
bmpCalNext.Free;
bmpCalPrev.Free;

end.
