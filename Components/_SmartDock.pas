unit _SmartDock;

interface

uses Windows, SysUtils, Messages, Classes, Controls, Graphics;

type TCreateDockManagerEvent = procedure(Sender: TObject; var ADockManager: IDockManager) of object;

type TSmartDockTree = class(TDockTree)
    protected
      procedure PaintDockFrame(Canvas: TCanvas; Control: TControl; const ARect: TRect); override;
      procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
  end;

type TSmartDock = class(TWinControl)
  private
    FSaveSize: Integer;
    FCaptions: TStringList;
    FOnCreateDockManager: TCreateDockManagerEvent;
    function GetHorizontal: Boolean;
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function AllChildsInvisible: Boolean;
    function GetCaptions(AControl: TControl): String;
    procedure SetCaptions(AControl: TControl; const Value: String);
    procedure SetOnCreateDockManager(const Value: TCreateDockManagerEvent);
  protected
    procedure CMControlListChange(var M: TMessage); message CM_CONTROLLISTCHANGE;
    procedure CMDockNotification(var M: TMessage); message CM_DOCKNOTIFICATION;
    procedure WMNCHitTest(var M: TWMNCHitTest); message WM_NCHITTEST;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure Loaded; override;
    function CreateDockManager: IDockManager; override;
  public
    property DockManager;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    property Horizontal: Boolean read GetHorizontal;
    property Size: Integer read GetSize write SetSize;
    property SaveSize: Integer read FSaveSize write FSaveSize;

    property Captions[AControl: TControl]: String read GetCaptions write SetCaptions;
  published
    property Enabled;
    property HelpContext;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Align;
    property Color;
    property OnUnDock;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnCreateDockManager: TCreateDockManagerEvent read FOnCreateDockManager write SetOnCreateDockManager;
  end;

procedure Register;

implementation

procedure Register;
begin
RegisterComponents('My components', [TSmartDock])
end;

{ TDock }

function TSmartDock.AllChildsInvisible: Boolean;
var i: integer;
begin
Result:=True;
for i:=0 to ControlCount-1 do
  if Controls[i].Visible then
    begin
    Result:=False;
    break;
    end;
end;

procedure TSmartDock.CMControlListChange(var M: TMessage);
begin
inherited;
if (Integer(True)=M.LParam) and (Size<=1) then // add control
  Size:=FSaveSize;

// при уничтожении Dock'нутого контрола CM_CONTROLLISTCHANGE может приходить как
// для самого контрола, так и контролов, для которых он является Parent'ом (если
// эти контролы разрушать вручную вызовом метода Free; если они разрушаются
// автоматически родительским контролом, то все нормально). Непонятно почему.
// Из-за этого SaveSize может устанавливаться равным 1, и при последующих доках
// наш компонент остается невидимым. Для решения проблемы не разрешаем установку
// SaveSize:=1
if (Integer(False)=M.LParam) and AllChildsInvisible then // remove control
  begin
  if Size>1 then FSaveSize:=Size;
  Size:=1;
  end;
end;

procedure TSmartDock.CMDockNotification(var M: TMessage);
var AllInvisible: Boolean;
begin
inherited;
with TCMDockNotification(M) do if (NotifyRec.ClientMsg <> CM_VISIBLECHANGED) then exit;
AllInvisible:=AllChildsInvisible;
if (Size<>1) and AllInvisible then begin FSaveSize:=Size; Size:=1; end;
if (Size=1) and (not AllInvisible) then Size:=FSaveSize;
end;

constructor TSmartDock.Create(AOwner: TComponent);
begin
inherited;
// AcceptsControls нужно для обработки бага, связанного с некорректной перерисовкой дочерних окон
FCaptions:=TStringList.Create;
ControlStyle := ControlStyle+[csAcceptsControls];
FSaveSize:=300;
DockSite:=True;
UseDockManager:=True;
Width:=50;
Height:=50;
Color:=clBtnFace;
end;

function TSmartDock.CreateDockManager: IDockManager;
begin
if (DockManager = nil) and DockSite and UseDockManager then
  begin
  if Assigned(FOnCreateDockManager) then     
    FOnCreateDockManager(Self, Result)
  else
    Result:=TSmartDockTree.Create(Self)
  end
else
  Result := DockManager;
DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

procedure TSmartDock.CreateParams(var Params: TCreateParams);
begin
inherited CreateParams(Params);
with Params do
  // нужно, чтобы дочерние элементы не мыргали при изменении размера SmartDock'a
  WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TSmartDock.CreateWnd;
begin
inherited;
DockSite:=True // Приходится писать здесь, потому что в Create и AfterConstruction не срабатывает
end;

destructor TSmartDock.Destroy;
begin
FCaptions.Free;
inherited Destroy;
end;

function TSmartDock.DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean;
begin
Result:=(NewTarget<>NIL) and (inherited DoUnDock(NewTarget, Client));
end;

function TSmartDock.GetCaptions(AControl: TControl): String;
var I: Integer;
begin
Result:='';
for i:=0 to FCaptions.Count-1 do
  if TObject(FCaptions.Objects[i])=AControl then
    begin
    Result:=FCaptions[i];
    break;
    end;
end;

function TSmartDock.GetHorizontal: Boolean;
begin
Result:=(Align=alTop) or (Align=alBottom);
end;

function TSmartDock.GetSize: Integer;
begin
if Horizontal then Result:=Height else Result:=Width;
end;

procedure TSmartDock.Loaded;
begin
inherited;
if not (csDesigning in ComponentState) and (ControlCount=0) then Size:=1;
end;

procedure TSmartDock.SetCaptions(AControl: TControl; const Value: String);
var I, Idx: Integer;
begin
Idx:=-1;
for i:=0 to FCaptions.Count-1 do
  if TObject(FCaptions.Objects[i])=AControl then
    begin
    Idx:=i;
    break;
    end;
if Idx=-1 then
  FCaptions.AddObject(Value, AControl)
else
  FCaptions[Idx]:=Value
end;

procedure TSmartDock.SetOnCreateDockManager(const Value: TCreateDockManagerEvent);
begin
  FOnCreateDockManager := Value;

  DockManager := NIL;
  DockManager := CreateDockManager;
end;

procedure TSmartDock.SetSize(const Value: Integer);
begin
if Horizontal then Height:=Value else Width:=Value;
end;

procedure TSmartDockTree.AdjustDockRect(Control: TControl;
  var ARect: TRect);
begin
inherited;
case DockSite.Align of
alLeft: if ARect.Right=DockSite.Width then ARect.Right:=ARect.Right-3;
alRight: if ARect.Left=0 then ARect.Left:=3;
alBottom: if ARect.Top=0 then ARect.Top:=3;
alTop: if ARect.Bottom=DockSite.Height then ARect.Bottom:=ARect.Bottom-3;
end; //case
end;

procedure TSmartDockTree.PaintDockFrame(Canvas: TCanvas; Control: TControl; const ARect: TRect);
const FGrabberSize=12;

  procedure DrawCloseButton(Left, Top: Integer);
  begin
  DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+FGrabberSize-2,
      Top+FGrabberSize-2), DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

var R: TRect;
begin
Canvas.Brush.Color:=clActiveCaption;
Canvas.Font.Color:=clCaptionText;

with ARect do
  if DockSite.Align in [alTop, alBottom] then
    begin
    R:=ARect; R.Left:=R.Left-1; R.Right:=R.Left+FGrabberSize+1;
    Canvas.FillRect(R);
    DrawCloseButton(Left+1, Top+1)
    end
  else
    begin
    R:=ARect; R.Top:=R.Top-1; R.Bottom:=R.Top+FGrabberSize+1;
    Canvas.FillRect(R);
    R.Right:=R.Right-FGrabberSize-2; R.Left:=R.Left+2;
    DrawText(Canvas.Handle, PChar(TSmartDock(Control.Parent).Captions[Control]), -1, R, DT_VCENTER+DT_SINGLELINE);
    DrawCloseButton(Right-FGrabberSize+1, Top+1);
    end;
end;

procedure TSmartDock.WMNCHitTest(var M: TWMNCHitTest);
var P: TPoint;
begin
if AllChildsInvisible then
  begin
  inherited;
  exit;
  end;

P:=ScreenToClient(SmallPointToPoint(M.Pos));

case Align of
alLeft: if p.x>=Width-3 then M.Result:=HTRIGHT else inherited;
alRight: if p.x<=3 then M.Result:=HTLEFT else inherited;
alBottom: if p.y<=3 then M.Result:=HTTOP else inherited;
alTop: if p.y>=Height-3 then M.Result:=HTBOTTOM else inherited;
else
inherited;
end; //case
end;

end.
