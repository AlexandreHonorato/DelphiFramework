unit _Frames;

interface

//UAL 2

// HOW TO USE:
// 1. Создать новую форму
// 2. убрать ее из AutoCreate
// 3. отнаследовать от v_VF_Base
// 4. перекрыть методы ImageIndex, DockAlign
// 5. в initialization: rcFrames.RegisterClass(VF_Frame, TfrmFrame)

// 6. Где нужно создаем список rcFrames: TRegClassesStr
// 7. Где нужно: ViewFrames:=TViewFrames.CreateEx(rcFrames, PngImageList1, [sdLeft, sdRight, sdBottom, sdMain]);

// 8. Размещаем доки, делаем обработчик OnCreateDockManager:
//    ADockManager:=TViewFramesDockTree.Create(TWinControl(Sender), 18);

uses SysUtils, Windows, Messages, Classes, Forms, Controls, Graphics, 
  _SmartDock, _SmartDockTree2, _Debug, _RegClassesLists, _MVC_Controller,
  _AsyncCalls, _MVC_Model, _GDI;

type
  TViewFrameAlign = (vfaLeft, vfaRight, vfaBottom, vfaMain,
    vfaReserved1, vfaReserved2, vfaReserved3);

type m_VF_Base = class(TMVC_Model)
  private
    FHash: Integer;
  protected
    function CanClose: Boolean; virtual;
    function ImageIndex: Integer; virtual;
  public
    constructor Create(AHash: Integer); reintroduce; virtual;
    property Hash: Integer read FHash;
  end;

type v_VF_Base = class(TDebugForm)
  private
    FModel: m_VF_Base;
  protected
    procedure WMSetText(var M: TMessage); message WM_SETTEXT;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property Model: m_VF_Base read FModel write FModel;
  end;
  v_VF_Base_Class = class of v_VF_Base;

type TSmartDockArray = array of TSmartDock;

type
  TViewFrames = class;

  c_VF_Base = class(TMVC_Controller)
    private
      FViewFrames: TViewFrames;
      function GetView: v_VF_Base;
      procedure SetView(const Value: v_VF_Base);
      function GetModel: m_VF_Base;
      procedure SetModel(const Value: m_VF_Base);
      procedure On_NEDisposeModel(AEvent: NEDisposeModel);
    protected
      class function ViewClass: v_VF_Base_Class; virtual; abstract;
      function DoCreateViewNew(AOwner: TComponent; AParent: TWinControl): TControl; override;
      function DockAlign: TViewFrameAlign; virtual;
      procedure DoStartListeningModel; override;
      procedure DoStopListeningModel; override;
      procedure DoConnectModel; override;
      procedure DoDisconnectModel; override;
    public
      class function ControllerID: String; virtual; abstract;
      constructor Create(AViewFrames: TViewFrames); reintroduce; virtual;
      property View: v_VF_Base read GetView write SetView;
      property Model: m_VF_Base read GetModel write SetModel;
    end;

  c_VF_Base_Class = class of c_VF_Base;

  TViewFrameControllers = class(TDebugList)
    private
      function GetControllers(Index: Integer): c_VF_Base;
    public
      function ByID(const AControllerID: String): c_VF_Base;
      function ByFrame(const AFrame: v_VF_Base): c_VF_Base;
      property Controllers[Index: Integer]: c_VF_Base read GetControllers; default;
    end;

  TViewFrames = class(TDebugObject)
    private
      FControllerClasses: TRegClassesStr;
      FControllers: TViewFrameControllers;
      FImages: TImageList;
      FDocks: array[TViewFrameAlign] of TSmartDock;
      procedure ClearControllers;
      procedure FreeControllerAndFrame_Async(AController: c_VF_Base);
    function GetDocks(Index: TViewFrameAlign): TSmartDock;
    protected
      function VFA2Align(AVFA: TViewFrameAlign): TAlign; virtual;
    public
      destructor Destroy; override;
      constructor Create(AControllerClasses: TRegClassesStr;
        AImages: TImageList; ADocks: TSmartDockArray); reintroduce; virtual;

      property Images: TImageList read FImages;

      function FindController(const AControllerID: String; AInstanceHash: Integer = 0;
        AModelClass: TClass = NIL): c_VF_Base; overload;
      function FindController(AModel: m_VF_Base): c_VF_Base; overload;

      function AddController(const AControllerID: String): c_VF_Base;
      function FindOrCreateController(const AControllerID: String): c_VF_Base;

      procedure FreeControllerAndFrame(AController: c_VF_Base);
      property Docks[Index: TViewFrameAlign]: TSmartDock read GetDocks;
    end;

type TViewFramesDockTree = class(TDockTree2)
  private
    FImages: TImageList;
    FViewFrames: TViewFrames;
  protected
    procedure DoCloseControl(AControl: TControl); override;
  public
    procedure PaintDockFrame(Canvas: TCanvas; Control: TControl; const ARect: TRect); override;
    constructor Create(DockSite: TWinControl; AGrabberSize: Integer; AImages: TImageList; AViewFrames: TViewFrames); reintroduce;
    property Images: TImageList read FImages;
  end;

implementation

{ v_VF_Base }

constructor v_VF_Base.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
inherited CreateNew(AOwner, Dummy);
DragMode:=dmAutomatic;
DragKind:=dkDock;
end;

procedure v_VF_Base.WMSetText(var M: TMessage);
begin
inherited;
if Parent<>NIL then Parent.Invalidate
end;

constructor TViewFrames.Create(AControllerClasses: TRegClassesStr; AImages: TImageList; ADocks: TSmartDockArray);
var I: Integer;
begin
inherited Create;
FImages:=AImages;
FControllerClasses:=AControllerClasses;

FControllers:=TViewFrameControllers.Create;

if (High(ADocks)-Low(ADocks))>(Integer(High(TViewFrameAlign))-Integer(Low(TViewFrameAlign))) then
  raise Exception.Create('Wrong ADocks array size');

FillChar(FDocks, SizeOf(FDocks), 0);
for i:=Low(ADocks) to High(ADocks) do
  FDocks[TViewFrameAlign(i)]:=ADocks[i];
end;

destructor TViewFrames.Destroy;
begin
if Assigned(FControllers) then
	begin
  ClearControllers;
	FreeAndNIL(FControllers);
	end;

inherited Destroy;
end;

constructor TViewFramesDockTree.Create(DockSite: TWinControl; AGrabberSize: Integer; AImages: TImageList; AViewFrames:
    TViewFrames);
begin
inherited Create(DockSite, AGrabberSize);
FViewFrames:=AViewFrames;
FImages:=AImages;
end;

procedure TViewFramesDockTree.DoCloseControl(AControl: TControl);
var model: m_VF_Base;
begin
model:=NIL;
if AControl is v_VF_Base then
  model:=v_VF_Base(AControl).Model;

if Assigned(model) then
  model.Dispose
else
  inherited DoCloseControl(AControl);
end;

procedure TViewFramesDockTree.PaintDockFrame(Canvas: TCanvas; Control: TControl; const ARect: TRect);
var HeaderRect: TRect; view: v_VF_Base;
begin
if Control is v_VF_Base then
	begin
  view:=v_VF_Base(Control);

	HeaderRect:=ARect;
	HeaderRect.Bottom:=HeaderRect.Top+GrabberSize;

	Canvas.Brush.Color:=clActiveCaption;
	Canvas.FillRect(HeaderRect);

  if (FImages<>NIL) and Assigned(view.Model) then
  	begin
  	FImages.Draw(Canvas, HeaderRect.Left+1, HeaderRect.Top+1, view.Model.ImageIndex);
  	HeaderRect.Left:=HeaderRect.Left+FImages.Width+2;
  	end;

	Canvas.Font.Color:=clHighlightText;
  Canvas.Font.Style:=[fsBold];
	MyDrawText(Canvas.Handle, view.Caption, HeaderRect);

	DrawFrameControl(Canvas.Handle,
	  Bounds(HeaderRect.Right-GrabberSize+2, HeaderRect.Top+2, GrabberSize-4, GrabberSize-4),
	  DFC_CAPTION, DFCS_CAPTIONCLOSE);
	end
else
  inherited PaintDockFrame(Canvas, Control, ARect);
end;

function TViewFrames.FindController(const AControllerID: String; AInstanceHash: Integer = 0;
  AModelClass: TClass = NIL): c_VF_Base;
var i: Integer; Tmp: c_VF_Base; B: Boolean;
begin
Result:=NIL;
for i:=0 to FControllers.Count-1 do
  begin
  Tmp:=FControllers[i];
  B:=Tmp.ControllerID=AControllerID;
  if AInstanceHash<>0 then B:=B and Assigned(Tmp.Model) and (Tmp.Model.Hash=AInstanceHash);
  if AModelClass<>NIL then B:=B and Assigned(Tmp.Model) and (Tmp.Model.ClassType=AModelClass);
  if B then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

function TViewFrames.AddController(const AControllerID: String): c_VF_Base;
var Cls: c_VF_Base_Class; D: TSmartDock; A: TAlign; view: v_VF_Base;
begin
Cls:=c_VF_Base_Class(FControllerClasses.ByID(AControllerID));

if Cls=NIL then raise Exception.CreateFmt('Controller class "%s" not registred', [AControllerID]);

Result:=Cls.Create(Self);
view:=v_VF_Base(Result.CreateViewNew(NIL, NIL));

D:=FDocks[Result.DockAlign];
A:=VFA2Align(Result.DockAlign);

view.Dock(D, Rect(0, 0, 0, 0));
D.DockManager.InsertControl(view, A, D);

view.Visible:=True;

FControllers.Add(Result);
end;

function TViewFrames.VFA2Align(AVFA: TViewFrameAlign): TAlign;
begin
if AVFA=vfaBottom then
  Result:=alRight
else
  Result:=alBottom;
end;

procedure TViewFrames.FreeControllerAndFrame(AController: c_VF_Base);
var view: v_VF_Base; model: m_VF_Base; CanClose: Boolean;
begin
model:=AController.Model;
if Assigned(model) then
  CanClose:=model.CanClose
else
  CanClose:=True;

if CanClose then
	begin
  AController.Model:=NIL;

	view:=AController.View;
	FreeAndNIL(view);

	FControllers.Remove(AController);
	AController.Free;
	end;
end;

procedure TViewFrames.ClearControllers;
begin
while FControllers.Count>0 do
  FreeControllerAndFrame(FControllers[0]);
end;

function TViewFrames.FindController(AModel: m_VF_Base): c_VF_Base;
var i: Integer; Tmp: c_VF_Base;
begin
Result:=NIL;
for i:=0 to FControllers.Count-1 do
  begin
  Tmp:=FControllers[i];
  if Tmp.Model=AModel then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

procedure TViewFrames.FreeControllerAndFrame_Async(AController: c_VF_Base);
begin
CallAsyncMethod(Self, @TViewFrames.FreeControllerAndFrame, AController);
end;

{ TViewFrameControllers }

function TViewFrameControllers.ByFrame(const AFrame: v_VF_Base): c_VF_Base;
var I: Integer; Tmp: c_VF_Base;
begin
Result:=NIL;

for i:=0 to Count-1 do
  begin
  Tmp:=Controllers[i];

  if Controllers[i].View=AFrame then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

function TViewFrameControllers.ByID(const AControllerID: String): c_VF_Base;
var I: Integer; Tmp: c_VF_Base;
begin
Result:=NIL;

for i:=0 to Count-1 do
  begin
  Tmp:=Controllers[i];

  if Controllers[i].ControllerID=AControllerID then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

function TViewFrameControllers.GetControllers(Index: Integer): c_VF_Base;
begin
Result:=c_VF_Base(Items[Index])
end;

constructor c_VF_Base.Create(AViewFrames: TViewFrames);
begin
inherited Create;
FViewFrames:=AViewFrames;
end;

function c_VF_Base.DockAlign: TViewFrameAlign;
begin
Result:=vfaMain
end;

procedure c_VF_Base.DoConnectModel;
begin
inherited DoConnectModel;
View.Model:=Model;
end;

function c_VF_Base.DoCreateViewNew(AOwner: TComponent; AParent: TWinControl): TControl;
begin
Result:=ViewClass.Create(AOwner);
Result.Parent:=AParent;
end;

procedure c_VF_Base.DoDisconnectModel;
begin
View.Model:=NIL;
inherited DoDisconnectModel;
end;

procedure c_VF_Base.DoStartListeningModel;
begin
inherited DoStartListeningModel;
Model.MessageBus.SignObject(Self,
  [NEDisposeModel],
  [@c_VF_Base.On_NEDisposeModel]);
end;

procedure c_VF_Base.DoStopListeningModel;
begin
Model.MessageBus.UnsignObject(Self);
inherited DoStopListeningModel;
end;

function c_VF_Base.GetModel: m_VF_Base;
begin
Result:=m_VF_Base(inherited Model)
end;

function c_VF_Base.GetView: v_VF_Base;
begin
Result:=v_VF_Base(inherited View)
end;

procedure c_VF_Base.On_NEDisposeModel(AEvent: NEDisposeModel);
begin
FViewFrames.FreeControllerAndFrame_Async(Self);
end;

procedure c_VF_Base.SetModel(const Value: m_VF_Base);
begin
inherited Model:=Value;
end;

procedure c_VF_Base.SetView(const Value: v_VF_Base);
begin
inherited View:=Value;
end;

{ m_VF_Base }

function m_VF_Base.CanClose: Boolean;
begin
Result:=True
end;

constructor m_VF_Base.Create(AHash: Integer);
begin
inherited Create;
FHash:=AHash;
end;

function m_VF_Base.ImageIndex: Integer;
begin
Result:=-1;
end;

function TViewFrames.FindOrCreateController(const AControllerID: String): c_VF_Base;
begin
Result:=FindController(AControllerID);
if Result=NIL then Result:=AddController(AControllerID);
end;

function TViewFrames.GetDocks(Index: TViewFrameAlign): TSmartDock;
begin
Result:=FDocks[Index]
end;

end.
