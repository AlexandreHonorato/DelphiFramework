unit _SmartDockTree2;

interface

uses SysUtils, Windows, Messages, Classes, Controls, Graphics, Consts,
  Forms, _GDI, _Misc;

const
  sdSplitterSize = 2;

type
  TDockTree2 = class;

  TDockZone2 = class
  private
    FChildControl: TControl;
    FChildZones: TDockZone2;
    FNextSibling: TDockZone2;
    FOrientation: TDockOrientation;
    FParentZone: TDockZone2;
    FPrevSibling: TDockZone2;
    FTree: TDockTree2;
    FZoneLimit: Integer;
    FOldSize: Integer;
    function GetChildCount: Integer;
    function GetControlName: string;
    function GetLimitBegin: Integer;
    function GetLimitSize: Integer;
    function GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
    function GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
    function GetVisible: Boolean;
    function GetVisibleChildCount: Integer;
    function GetZoneLimit: Integer;
    function SetControlName(const Value: string): Boolean;
    procedure SetZoneLimit(const Value: Integer);
  public
    constructor Create(Tree: TDockTree2);
    procedure ExpandZoneLimit(NewLimit: Integer);
    function FirstVisibleChild: TDockZone2;
    function NextVisible: TDockZone2;
    function PrevVisible: TDockZone2;
    procedure ResetChildren;
    procedure ResetZoneLimits;
    procedure Update;
    property ChildCount: Integer read GetChildCount;
    property Height: Integer index Ord(doHorizontal) read GetHeightWidth;
    property Left: Integer index Ord(doVertical) read GetTopLeft;
    property LimitBegin: Integer read GetLimitBegin;
    property LimitSize: Integer read GetLimitSize;
    property Top: Integer index Ord(doHorizontal) read GetTopLeft;
    property Visible: Boolean read GetVisible;
    property VisibleChildCount: Integer read GetVisibleChildCount;
    property Width: Integer index Ord(doVertical) read GetHeightWidth;
    property ZoneLimit: Integer read GetZoneLimit write SetZoneLimit;
  end;

  TForEachZoneProc2 = procedure(Zone: TDockZone2) of object;

  TDockTree2 = class(TInterfacedObject, IDockManager)
  private
    FBorderWidth: Integer;
    FBrush: TBrush;
    FDockSite: TWinControl;
    FOldRect: TRect;
    FOldWndProc: TWndMethod;
    FReplacementZone: TDockZone2;
    FScaleBy: Double;
    FShiftScaleOrient: TDockOrientation;
    FShiftBy: Integer;
    FSizePos: TPoint;
    FSizingDC: HDC;
    FSizingWnd: HWND;
    FSizingZone: TDockZone2;
    FTopZone: TDockZone2;
    FTopXYLimit: Integer;
    FUpdateCount: Integer;
    FVersion: Integer;
    FGrabberSize: Integer;
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean);
    procedure DrawSizeSplitter;
    function FindControlZone(Control: TControl): TDockZone2;
    procedure ForEachAt(Zone: TDockZone2; Proc: TForEachZoneProc2);
    function GetNextLimit(AZone: TDockZone2): Integer;
    procedure InsertNewParent(NewZone, SiblingZone: TDockZone2;
      ParentOrientation: TDockOrientation; InsertLast: Boolean);
    procedure InsertSibling(NewZone, SiblingZone: TDockZone2; InsertLast: Boolean);
    function InternalHitTest(const MousePos: TPoint; out HTFlag: Integer): TDockZone2;
    procedure PruneZone(Zone: TDockZone2);
    procedure RemoveZone(Zone: TDockZone2);
    procedure ScaleZone(Zone: TDockZone2);
    procedure SetNewBounds(Zone: TDockZone2);
    procedure ShiftZone(Zone: TDockZone2);
    procedure SplitterMouseDown(OnZone: TDockZone2; MousePos: TPoint);
    procedure SplitterMouseUp;
    procedure UpdateZone(Zone: TDockZone2);
    procedure WindowProc(var Message: TMessage);
  protected
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect);
    function HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl; virtual;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure PaintDockFrame(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); virtual;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); virtual;
    procedure RemoveControl(Control: TControl); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetReplacingControl(Control: TControl);
    procedure ResetBounds(Force: Boolean); virtual;
    procedure UpdateAll;
    property DockSite: TWinControl read FDockSite write FDockSite;
    procedure DoCloseControl(AControl: TControl); virtual;
  public
    constructor Create(DockSite: TWinControl; AGrabberSize: Integer); virtual;
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); virtual;
    property GrabberSize: Integer read FGrabberSize;
  end;

implementation

function NextVisibleZone(StartZone: TDockZone2): TDockZone2;
begin
  Result := StartZone;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FNextSibling;
end;

function IsOrientationSet(Zone: TDockZone2): Boolean;
begin
  Result := (Assigned(Zone.FParentZone) and
             (Zone.FParentZone.FOrientation <> doNoOrient)) or
            ((Zone.FTree.FTopZone = Zone) and (Zone.FOrientation <> doNoOrient));
end;

type TControlHack = class(TControl)
  public
    property DragKind;
  end;

type TWinControlHack = class(TWinControl)
  public
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ReloadDockedControl(const AControlName: string; var AControl: TControl); override;
    property AutoSize;
  end;

procedure TWinControlHack.AdjustClientRect(var Rect: TRect);
begin
inherited AdjustClientRect(Rect);
end;

procedure TWinControlHack.ReloadDockedControl(const AControlName: string; var AControl: TControl);
begin
ReloadDockedControl(AControlName, AControl);
end;

constructor TDockZone2.Create(Tree: TDockTree2);
begin
  FTree := Tree;
end;

function TDockZone2.GetChildCount: Integer;
var
  Zone: TDockZone2;
begin
  Result := 0;
  Zone := FChildZones;
  while Zone <> nil do
  begin
    Zone := Zone.FNextSibling;
    Inc(Result);
  end;
end;

function TDockZone2.GetVisibleChildCount: Integer;
var
  Zone: TDockZone2;
begin
  Result := 0;
  Zone := FirstVisibleChild;
  while Zone <> nil do
  begin
    Zone := Zone.NextVisible;
    Inc(Result);
  end;
end;

function TDockZone2.GetVisible: Boolean;
var
  NextChild: TDockZone2;
begin
  if Assigned(FChildControl) then
    Result := FChildControl.Visible
  else
  begin
    Result := True;
    NextChild := FirstVisibleChild;
    while Assigned(NextChild) do
    begin
      if NextChild.Visible then Exit;
      NextChild := NextChild.FNextSibling;
    end;
    Result := False;
  end;
end;

function TDockZone2.GetLimitBegin: Integer;
var
  CheckZone: TDockZone2;
begin
  if FTree.FTopZone = Self then CheckZone := Self
  else CheckZone := FParentZone;
  if CheckZone.FOrientation = doHorizontal then Result := Top
  else if CheckZone.FOrientation = doVertical then Result := Left
  else raise Exception.Create('');
end;

function TDockZone2.GetLimitSize: Integer;
var
  CheckZone: TDockZone2;
begin
  if FTree.FTopZone = Self then CheckZone := Self
  else CheckZone := FParentZone;
  if CheckZone.FOrientation = doHorizontal then Result := Height
  else if CheckZone.FOrientation = doVertical then Result := Width
  else raise Exception.Create('');
end;

function TDockZone2.GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TDockZone2;
  R: TRect;
begin
  Zone := Self;
  while Zone <> FTree.FTopZone do
  begin
    if (Zone.FParentZone.FOrientation = TDockOrientation(Orient)) and
      (Zone.FPrevSibling <> nil) then
    begin
      Result := Zone.FPrevSibling.ZoneLimit;
      Exit;
    end
    else
      Zone := Zone.FParentZone;
  end;
  R := FTree.FDockSite.ClientRect;
  TWinControlHack(FTree.FDockSite).AdjustClientRect(R);
  case TDockOrientation(Orient) of
    doVertical: Result := R.Left;
    doHorizontal: Result := R.Top;
  else
    Result := 0;
  end;
end;

function TDockZone2.GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TDockZone2;
  R: TRect;
begin
  if (Self = FTree.FTopZone) or ((FParentZone = FTree.FTopZone) and
    (FChildControl <> nil) and (FTree.FTopZone.VisibleChildCount = 1)) then
  begin
    R := FTree.FDockSite.ClientRect;
    TWinControlHack(FTree.FDockSite).AdjustClientRect(R);
    if TDockOrientation(Orient) = doHorizontal then
      Result := R.Bottom - R.Top
    else
      Result := R.Right - R.Left;
  end
  else begin
    Zone := Self;
    while Zone <> FTree.FTopZone do
    begin
      if Zone.FParentZone.FOrientation = TDockOrientation(Orient) then
      begin
        Result := Zone.ZoneLimit - Zone.LimitBegin;
        Exit;
      end
      else
        Zone := Zone.FParentZone;
    end;
    if FTree.FTopZone.FOrientation = TDockOrientation(Orient) then
      Result := FTree.FTopXYLimit
    else
      Result := FTree.FTopZone.ZoneLimit;
  end;
end;

procedure TDockZone2.ResetChildren;
var
  MaxLimit: Integer;
  NewLimit: Integer;
  ChildNode: TDockZone2;
begin
  if (VisibleChildCount = 0) or (FOrientation = doNoOrient) then Exit;
  ChildNode := FirstVisibleChild;
  case FOrientation of
    doHorizontal: MaxLimit := Height;
    doVertical: MaxLimit := Width;
  else
    MaxLimit := 0;
  end;
  NewLimit := MaxLimit div VisibleChildCount;
  while ChildNode <> nil do
  begin
    if ChildNode.FNextSibling = nil then
      ChildNode.ZoneLimit := MaxLimit
    else
      ChildNode.ZoneLimit := ChildNode.LimitBegin + NewLimit;
    ChildNode.Update;
    ChildNode := ChildNode.NextVisible;
  end;
end;

function TDockZone2.GetControlName: string;
begin
  Result := '';
  if FChildControl <> nil then
  begin
    if FChildControl.Name = '' then
      raise Exception.CreateRes(@SDockedCtlNeedsName);
    Result := FChildControl.Name;
  end;
end;

function TDockZone2.SetControlName(const Value: string): Boolean;
var
  Client: TControl;
begin
  Client := nil;
  with FTree do
  begin
    TWinControlHack(FDockSite).ReloadDockedControl(Value, Client);
    Result := Client <> nil;
    if Result then
    begin
      FReplacementZone := Self;
      try
        Client.ManualDock(FDockSite, nil, alNone);
      finally
        FReplacementZone := nil;
      end;
    end;
  end;
end;

procedure TDockZone2.Update;

  function ParentNotLast: Boolean;
  var
    Parent: TDockZone2;
  begin
    Result := False;
    Parent := FParentZone;
    while Parent <> nil do
    begin
      if Parent.NextVisible <> nil then
      begin
        Result := True;
        Exit;
      end;
      Parent := Parent.FParentZone;
    end;
  end;

var
  NewWidth, NewHeight: Integer;
  R: TRect;
begin
  if (FChildControl <> nil) and FChildControl.Visible and (FTree.FUpdateCount = 0) then
  begin
    FChildControl.DockOrientation := FParentZone.FOrientation;
    NewWidth := Width;
    NewHeight := Height;
    if ParentNotLast then
    begin
      if FParentZone.FOrientation = doHorizontal then
        Dec(NewWidth, FTree.FBorderWidth)
      else
        Dec(NewHeight, FTree.FBorderWidth);
    end;
    if (NextVisible <> nil) or ((FParentZone <> FTree.FTopZone) and
      ((FParentZone.FOrientation = FTree.FTopZone.FOrientation) and
      (ZoneLimit < FTree.FTopXYLimit)) or
      ((FParentZone.FOrientation <> FTree.FTopZone.FOrientation) and
      (ZoneLimit < FTree.FTopZone.ZoneLimit))) then
    begin
      if FParentZone.FOrientation = doHorizontal then
        Dec(NewHeight, FTree.FBorderWidth)
      else
        Dec(NewWidth, FTree.FBorderWidth);
    end;
    R := Bounds(Left, Top, NewWidth, NewHeight);
    FTree.AdjustDockRect(FChildControl, R);
    FChildControl.BoundsRect := R;
  end;
end;

function TDockZone2.GetZoneLimit: Integer;
begin
  if not Visible and IsOrientationSet(Self) then
    // LimitSize will be zero and zone will take up no space
    Result := GetLimitBegin
  else
    Result := FZoneLimit;
end;

procedure TDockZone2.SetZoneLimit(const Value: Integer);
begin
  FZoneLimit := Value;
end;

procedure TDockZone2.ExpandZoneLimit(NewLimit: Integer);

  function GetLastChildZone(Zone: TDockZone2): TDockZone2;
  begin
    { Assumes Zone has at least one child }
    Result := Zone.FChildZones;
    while Result.FNextSibling <> nil do
      Result := Result.FNextSibling;
  end;

var
  LastChild, ChildZone: TDockZone2;
begin
  ZoneLimit := NewLimit;
  ChildZone := FChildZones;
  while Assigned(ChildZone) do
  begin
    if ChildZone.ChildCount > 0 then
    begin
      LastChild := GetLastChildZone(ChildZone);
      LastChild.ExpandZoneLimit(NewLimit);
    end;
    ChildZone := ChildZone.FNextSibling;
  end;
end;

procedure TDockZone2.ResetZoneLimits;
var
  ChildZone: TDockZone2;
begin
  ChildZone := FChildZones;
  while Assigned(ChildZone) do
  begin
    { If the ZoneLimit is too big or too small then just reset all child zones }
    if (ChildZone.ZoneLimit < ChildZone.LimitBegin) or
       (ChildZone.ZoneLimit > LimitSize) then
    begin
      ResetChildren;
      FTree.ForEachAt(Self, FTree.UpdateZone);
    end;
    ChildZone.ResetZoneLimits;
    ChildZone := ChildZone.FNextSibling;
  end;
end;

function TDockZone2.NextVisible: TDockZone2;
begin
  Result := NextVisibleZone(FNextSibling);
end;

function TDockZone2.PrevVisible: TDockZone2;
begin
  Result := FPrevSibling;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FPrevSibling;
end;

function TDockZone2.FirstVisibleChild: TDockZone2;
begin
  Result := NextVisibleZone(FChildZones)
end;

constructor TDockTree2.Create(DockSite: TWinControl; AGrabberSize: Integer);
var
  I: Integer;
begin
  inherited Create;
  FBorderWidth := 4;
  FGrabberSize := AGrabberSize;
  FDockSite := DockSite;
  FVersion := $00040000;
  FTopZone := TDockZone2.Create(Self);
  FBrush := TBrush.Create;
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  // insert existing controls into tree
  BeginUpdate;
  try
    for I := 0 to DockSite.ControlCount - 1 do
      InsertControl(DockSite.Controls[I], alLeft, nil);
    FTopZone.ResetChildren;
  finally
    EndUpdate;
  end;
  if not (csDesigning in DockSite.ComponentState) then
  begin
    FOldWndProc := FDockSite.WindowProc;
    FDockSite.WindowProc := WindowProc;
  end;
end;

destructor TDockTree2.Destroy;
begin
  if @FOldWndProc <> nil then
  begin
    FDockSite.WindowProc := FOldWndProc;
    FOldWndProc := nil;
  end;
  PruneZone(FTopZone);
  FBrush.Free;
  inherited Destroy;
end;

procedure TDockTree2.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
case FDockSite.Align of
alLeft: if ARect.Right=FDockSite.Width then ARect.Right:=ARect.Right-sdSplitterSize;
alRight: if ARect.Left=0 then ARect.Left:=sdSplitterSize;
alBottom: if ARect.Top=0 then ARect.Top:=sdSplitterSize;
alTop: if ARect.Bottom=FDockSite.Height then
ARect.Bottom:=ARect.Bottom-sdSplitterSize;
end; //case

Inc(ARect.Top, FGrabberSize);
end;

procedure TDockTree2.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDockTree2.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    UpdateAll;
  end;
end;

function TDockTree2.FindControlZone(Control: TControl): TDockZone2;
var
  CtlZone: TDockZone2;

  procedure DoFindControlZone(StartZone: TDockZone2);
  begin
    if StartZone.FChildControl = Control then
      CtlZone := StartZone
    else begin
      // Recurse sibling
      if (CtlZone = nil) and (StartZone.FNextSibling <> nil) then
        DoFindControlZone(StartZone.FNextSibling);
      // Recurse child
      if (CtlZone = nil) and (StartZone.FChildZones <> nil) then
        DoFindControlZone(StartZone.FChildZones);
    end;
  end;

begin
  CtlZone := nil;
  if (Control <> nil) and (FTopZone <> nil) then DoFindControlZone(FTopZone);
  Result := CtlZone;
end;

procedure TDockTree2.ForEachAt(Zone: TDockZone2; Proc: TForEachZoneProc2);

  procedure DoForEach(Zone: TDockZone2);
  begin
    Proc(Zone);
    // Recurse sibling
    if Zone.FNextSibling <> nil then DoForEach(Zone.FNextSibling);
    // Recurse child
    if Zone.FChildZones <> nil then DoForEach(Zone.FChildZones);
  end;

begin
  if Zone = nil then Zone := FTopZone;
  DoForEach(Zone);
end;

procedure TDockTree2.GetControlBounds(Control: TControl; out CtlBounds: TRect);
var
  Z: TDockZone2;
begin
  Z := FindControlZone(Control);
  if Z = nil then
    FillChar(CtlBounds, SizeOf(CtlBounds), 0)
  else
    with Z do
      CtlBounds := Bounds(Left, Top, Width, Height);
end;

function TDockTree2.HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl;
var
  Zone: TDockZone2;
begin
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then Result := Zone.FChildControl
  else Result := nil;
end;

procedure TDockTree2.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
const
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient); { alCustom }
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False, False);  { alCustom }
var
  Sibling, Me: TDockZone2;
  InsertOrientation, CurrentOrientation: TDockOrientation;
  NewWidth, NewHeight: Integer;
  R: TRect;
begin
  if not Control.Visible then Exit;
  if FReplacementZone <> nil then
  begin
    FReplacementZone.FChildControl := Control;
    FReplacementZone.Update;
  end
  else if FTopZone.FChildZones = nil then
  begin
    // Tree is empty, so add first child
    R := FDockSite.ClientRect;
    TWinControlHack(FDockSite).AdjustClientRect(R);
    NewWidth := R.Right - R.Left;
    NewHeight := R.Bottom - R.Top;
    if TWinControlHack(FDockSite).AutoSize then
    begin
      if NewWidth = 0 then NewWidth := Control.UndockWidth;
      if NewHeight = 0 then NewHeight := Control.UndockHeight;
    end;
    R := Bounds(R.Left, R.Top, NewWidth, NewHeight);
    AdjustDockRect(Control, R);
    Control.BoundsRect := R;
    Me := TDockZone2.Create(Self);
    FTopZone.FChildZones := Me;
    Me.FParentZone := FTopZone;
    Me.FChildControl := Control;
  end
  else begin
    // Default to right-side docking
    if InsertAt in [alClient, alNone] then InsertAt := alRight;
    Me := FindControlZone(Control);
    if Me <> nil then RemoveZone(Me);
    Sibling := FindControlZone(DropCtl);
    InsertOrientation := OrientArray[InsertAt];
    if FTopZone.ChildCount = 1 then
    begin
      // Tree only has one child, and a second is being added, so orientation and
      // limits must be set up
      FTopZone.FOrientation := InsertOrientation;
      case InsertOrientation of
        doHorizontal:
          begin
            FTopZone.ZoneLimit := FTopZone.FChildZones.Width;
            FTopXYLimit := FTopZone.FChildZones.Height;
          end;
        doVertical:
          begin
            FTopZone.ZoneLimit := FTopZone.FChildZones.Height;
            FTopXYLimit := FTopZone.FChildZones.Width;
          end;
      end;
    end;
    Me := TDockZone2.Create(Self);
    Me.FChildControl := Control;
    if Sibling <> nil then CurrentOrientation := Sibling.FParentZone.FOrientation
    else CurrentOrientation := FTopZone.FOrientation;
    if InsertOrientation = doNoOrient then
      InsertOrientation := CurrentOrientation;
    // Control is being dropped into a zone with the same orientation we
    // are requesting, so we just need to add ourselves to the sibling last
    if InsertOrientation = CurrentOrientation then InsertSibling(Me, Sibling,
      MakeLast[InsertAt])
    // Control is being dropped into a zone with a different orientation than
    // we are requesting
    else InsertNewParent(Me, Sibling, InsertOrientation, MakeLast[InsertAt]);
  end;
  { Redraw client dock frames }
  FDockSite.Invalidate;
end;

procedure TDockTree2.InsertNewParent(NewZone, SiblingZone: TDockZone2;
  ParentOrientation: TDockOrientation; InsertLast: Boolean);
var
  NewParent: TDockZone2;
begin
  NewParent := TDockZone2.Create(Self);
  NewParent.FOrientation := ParentOrientation;
  if SiblingZone = nil then
  begin
    // if SiblingZone is nil, then we need to insert zone as a child of the top
    NewParent.ZoneLimit := FTopXYLimit;
    FTopXYLimit := FTopZone.ZoneLimit;
    FShiftScaleOrient := ParentOrientation;
    FScaleBy := 0.5;
    if InsertLast then
    begin
      NewParent.FChildZones := FTopZone;
      FTopZone.FParentZone := NewParent;
      FTopZone.FNextSibling := NewZone;
      NewZone.FPrevSibling := FTopZone;
      NewZone.FParentZone := NewParent;
      FTopZone := NewParent;
      ForEachAt(NewParent.FChildZones, ScaleZone);
    end
    else begin
      NewParent.FChildZones := NewZone;
      FTopZone.FParentZone := NewParent;
      FTopZone.FPrevSibling := NewZone;
      NewZone.FNextSibling := FTopZone;
      NewZone.FParentZone := NewParent;
      FTopZone := NewParent;
      ForEachAt(NewParent.FChildZones, ScaleZone);
      FShiftBy := FTopZone.ZoneLimit div 2;
      ForEachAt(NewParent.FChildZones, ShiftZone);
      NewZone.ZoneLimit := FTopZone.ZoneLimit div 2;
    end;
    ForEachAt(nil, UpdateZone);
  end
  else begin
    // if SiblingZone is not nil, we need to insert a new parent zone for me
    // and my SiblingZone
    NewParent.ZoneLimit := SiblingZone.ZoneLimit;
    NewParent.FParentZone := SiblingZone.FParentZone;
    NewParent.FPrevSibling := SiblingZone.FPrevSibling;
    if NewParent.FPrevSibling <> nil then
      NewParent.FPrevSibling.FNextSibling := NewParent;
    NewParent.FNextSibling := SiblingZone.FNextSibling;
    if NewParent.FNextSibling <> nil then
      NewParent.FNextSibling.FPrevSibling := NewParent;
    if NewParent.FParentZone.FChildZones = SiblingZone then
      NewParent.FParentZone.FChildZones := NewParent;
    NewZone.FParentZone := NewParent;
    SiblingZone.FParentZone := NewParent;
    if InsertLast then
    begin
      // insert after SiblingZone
      NewParent.FChildZones := SiblingZone;
      SiblingZone.FPrevSibling := nil;
      SiblingZone.FNextSibling := NewZone;
      NewZone.FPrevSibling := SiblingZone;
    end
    else begin
      // insert before SiblingZone
      NewParent.FChildZones := NewZone;
      SiblingZone.FPrevSibling := NewZone;
      SiblingZone.FNextSibling := nil;
      NewZone.FNextSibling := SiblingZone;
    end;
    // Set bounds of new children
  end;
  NewParent.ResetChildren;
  NewParent.ResetZoneLimits;
  ForEachAt(nil, UpdateZone);
end;

procedure TDockTree2.InsertSibling(NewZone, SiblingZone: TDockZone2;
  InsertLast: Boolean);
begin
  if SiblingZone = nil then
  begin
    // If sibling is nil then make me the a child of the top
    SiblingZone := FTopZone.FChildZones;
    if InsertLast then
      while SiblingZone.FNextSibling <> nil do
        SiblingZone := SiblingZone.FNextSibling;
  end;
  if InsertLast then
  begin
    // Insert me after sibling
    NewZone.FParentZone := SiblingZone.FParentZone;
    NewZone.FPrevSibling := SiblingZone;
    NewZone.FNextSibling := SiblingZone.FNextSibling;
    if NewZone.FNextSibling <> nil then
      NewZone.FNextSibling.FPrevSibling := NewZone;
    SiblingZone.FNextSibling := NewZone;
  end
  else begin
    // insert before sibling
    NewZone.FNextSibling := SiblingZone;
    NewZone.FPrevSibling := SiblingZone.FPrevSibling;
    if NewZone.FPrevSibling <> nil then
      NewZone.FPrevSibling.FNextSibling := NewZone;
    SiblingZone.FPrevSibling := NewZone;
    NewZone.FParentZone := SiblingZone.FParentZone;
    if NewZone.FParentZone.FChildZones = SiblingZone then
      NewZone.FParentZone.FChildZones := NewZone;
  end;
  // Set up zone limits for all siblings
  SiblingZone.FParentZone.ResetChildren;
  SiblingZone.FParentZone.ResetZoneLimits;
end;

function TDockTree2.InternalHitTest(const MousePos: TPoint; out HTFlag: Integer): TDockZone2;
var
  ResultZone: TDockZone2;

  procedure DoFindZone(Zone: TDockZone2);
  var
    ZoneTop, ZoneLeft: Integer;
  begin
    // Check for hit on bottom splitter...
    if (Zone.FParentZone.FOrientation = doHorizontal) and
      ((MousePos.Y <= Zone.ZoneLimit) and
      (MousePos.Y >= Zone.ZoneLimit - FBorderWidth)) then
    begin
      HTFlag := HTBORDER;
      ResultZone := Zone;
    end
    // Check for hit on left splitter...
    else if (Zone.FParentZone.FOrientation = doVertical) and
      ((MousePos.X <= Zone.ZoneLimit) and
      (MousePos.X >= Zone.ZoneLimit - FBorderWidth)) then
    begin
      HTFlag := HTBORDER;
      ResultZone := Zone;
    end
    // Check for hit on grabber...
    else if Zone.FChildControl <> nil then
    begin
      ZoneTop := Zone.Top;
      ZoneLeft := Zone.Left;

        if (MousePos.Y >= ZoneTop) and (MousePos.Y <= ZoneTop + FGrabberSize) and
          (MousePos.X >= ZoneLeft) and (MousePos.X <= ZoneLeft + Zone.Width) then
        begin
          ResultZone := Zone;
          with Zone.FChildControl do
            if MousePos.X > Left + Width - FGrabberSize then HTFlag := HTCLOSE
            else HTFlag := HTCAPTION;
        end;

    end;
    // Recurse to next zone...
    if (ResultZone = nil) and (Zone.NextVisible <> nil) then
      DoFindZone(Zone.NextVisible);
    if (ResultZone = nil) and (Zone.FirstVisibleChild <> nil) then
      DoFindZone(Zone.FirstVisibleChild);
  end;

  function FindControlAtPos(const Pos: TPoint): TControl;
  var
    I: Integer;
    P: TPoint;
  begin
    for I := FDockSite.ControlCount - 1 downto 0 do
    begin
      Result := FDockSite.Controls[I];
      with Result do
      begin
        { Control must be Visible and Showing }
        if not Result.Visible or ((Result is TWinControl) and
           not TWinControl(Result).Showing) then continue;
        P := Point(Pos.X - Left, Pos.Y - Top);
        if PtInRect(ClientRect, P) then Exit;
      end;
    end;
    Result := nil;
  end;

var
  CtlAtPos: TControl;
begin
  ResultZone := nil;
  HTFlag := HTNOWHERE;
  CtlAtPos := FindControlAtPos(MousePos);
  if (CtlAtPos <> nil) and (CtlAtPos.HostDockSite = FDockSite) then
  begin
    ResultZone := FindControlZone(CtlAtPos);
    if ResultZone <> nil then HTFlag := HTCLIENT;
  end
  else if (FTopZone.FirstVisibleChild <> nil) and (CtlAtPos = nil) then
    DoFindZone(FTopZone.FirstVisibleChild);
  Result := ResultZone;
end;

var
  TreeStreamEndFlag: Integer = -1;

procedure TDockTree2.LoadFromStream(Stream: TStream);

  procedure ReadControlName(var ControlName: string);
  var
    Size: Integer;
  begin
    ControlName := '';
    Stream.Read(Size, SizeOf(Size));
    if Size > 0 then
    begin
      SetLength(ControlName, Size);
      Stream.Read(Pointer(ControlName)^, Size);
    end;
  end;

var
  CompName: string;
  Client: TControl;
  Level, LastLevel, I, InVisCount: Integer;
  Zone, LastZone, NextZone: TDockZone2;
begin
  PruneZone(FTopZone);
  BeginUpdate;
  try
    // read stream version
    Stream.Read(I, SizeOf(I));
    // read invisible dock clients
    Stream.Read(InVisCount, SizeOf(InVisCount));
    for I := 0 to InVisCount - 1 do
    begin
      ReadControlName(CompName);
      if CompName <> '' then
      begin
        TWinControlHack(FDockSite).ReloadDockedControl(CompName, Client);
        if Client <> nil then
        begin
          Client.Visible := False;
          Client.ManualDock(FDockSite);
        end;
      end;
    end;
    // read top zone data
    Stream.Read(FTopXYLimit, SizeOf(FTopXYLimit));
    LastLevel := 0;
    LastZone := nil;
    // read dock zone tree
    while True do
    begin
      with Stream do
      begin
        Read(Level, SizeOf(Level));
        if Level = TreeStreamEndFlag then Break;
        Zone := TDockZone2.Create(Self);
        Read(Zone.FOrientation, SizeOf(Zone.FOrientation));
        Read(Zone.FZoneLimit, SizeOf(Zone.FZoneLimit)); 
        ReadControlName(CompName);
        if CompName <> '' then
          if not Zone.SetControlName(CompName) then
          begin
            {Remove dock zone if control cannot be found}
            Zone.Free;
            Continue;
          end;
      end;
      if Level = 0 then FTopZone := Zone
      else if Level = LastLevel then
      begin
        LastZone.FNextSibling := Zone;
        Zone.FPrevSibling := LastZone;
        Zone.FParentZone := LastZone.FParentZone;
      end
      else if Level > LastLevel then
      begin
        LastZone.FChildZones := Zone;
        Zone.FParentZone := LastZone;
      end
      else if Level < LastLevel then
      begin
        NextZone := LastZone;
        for I := 1 to LastLevel - Level do NextZone := NextZone.FParentZone;
        NextZone.FNextSibling := Zone;
        Zone.FPrevSibling := NextZone;
        Zone.FParentZone := NextZone.FParentZone;
      end;
      LastLevel := Level;
      LastZone := Zone;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TDockTree2.PaintDockFrame(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);
var HeaderRect: TRect;
begin
HeaderRect:=ARect;
HeaderRect.Bottom:=HeaderRect.Top+FGrabberSize;

Canvas.Brush.Color:=clActiveCaption;
Canvas.FillRect(HeaderRect);

Canvas.Font.Color:=clCaptionText;
MyDrawText(Canvas.Handle, TControlHack(Control).Text, HeaderRect);

DrawFrameControl(Canvas.Handle,
  Bounds(HeaderRect.Right-FGrabberSize+2, HeaderRect.Top+2, FGrabberSize-4, FGrabberSize-4),
  DFC_CAPTION, DFCS_CAPTIONCLOSE);
end;

procedure TDockTree2.PaintSite(DC: HDC);
var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
begin
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := FDockSite;
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        for I := 0 to FDockSite.ControlCount - 1 do
        begin
          Control := FDockSite.Controls[I];
          if Control.Visible and (Control.HostDockSite = FDockSite) then
          begin
            R := Control.BoundsRect;
            AdjustDockRect(Control, R);
            Dec(R.Left, 2 * (R.Left - Control.Left));
            Dec(R.Top, 2 * (R.Top - Control.Top));
            Dec(R.Right, 2 * (Control.Width - (R.Right - R.Left)));
            Dec(R.Bottom, 2 * (Control.Height - (R.Bottom - R.Top)));
            PaintDockFrame(Canvas, Control, R);
          end;
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TDockTree2.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  VisibleClients,
  NewX, NewY, NewWidth, NewHeight: Integer;
begin
  VisibleClients := FDockSite.VisibleDockClientCount;
  { When docksite has no controls in it, or 1 or less clients then the
    dockrect should only be based on the client area of the docksite }
  if (DropCtl = nil) or (DropCtl.DockOrientation = doNoOrient) or
     {(DropCtl = Client) or }(VisibleClients < 2) then
  begin
    DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    { When there is exactly 1 client we divide the docksite client area in half}
    if VisibleClients > 0 then
    with DockRect do
      case DropAlign of
        alLeft: Right := Right div 2;
        alRight: Left := Right div 2;
        alTop: Bottom := Bottom div 2;
        alBottom: Top := Bottom div 2;
      end;
  end
  else begin
  { Otherwise, if the docksite contains more than 1 client, set the coordinates
    for the dockrect based on the control under the mouse }
    NewX := DropCtl.Left;
    NewY := DropCtl.Top;
    NewWidth := DropCtl.Width;
    NewHeight := DropCtl.Height;
    if DropAlign in [alLeft, alRight] then
      NewWidth := DropCtl.Width div 2
    else if DropAlign in [alTop, alBottom] then
      NewHeight := DropCtl.Height div 2;
    case DropAlign of
      alRight: Inc(NewX, NewWidth);
      alBottom: Inc(NewY, NewHeight);
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
  end;
  MapWindowPoints(FDockSite.Handle, 0, DockRect, 2);
end;

procedure TDockTree2.PruneZone(Zone: TDockZone2);

  procedure DoPrune(Zone: TDockZone2);
  begin
    // Recurse sibling
    if Zone.FNextSibling <> nil then
      DoPrune(Zone.FNextSibling);
    // Recurse child
    if Zone.FChildZones <> nil then
      DoPrune(Zone.FChildZones);
    // Free zone
    Zone.Free;
  end;

begin
  if Zone = nil then Exit;
  // Delete children recursively
  if Zone.FChildZones <> nil then DoPrune(Zone.FChildZones);
  // Fixup all pointers to this zone
  if Zone.FPrevSibling <> nil then
    Zone.FPrevSibling.FNextSibling := Zone.FNextSibling
  else if Zone.FParentZone <> nil then
    Zone.FParentZone.FChildZones := Zone.FNextSibling;
  if Zone.FNextSibling <> nil then
    Zone.FNextSibling.FPrevSibling := Zone.FPrevSibling;
  // Free this zone
  if Zone = FTopZone then FTopZone := nil;
  Zone.Free;
end;

procedure TDockTree2.RemoveControl(Control: TControl);
var
  Z: TDockZone2;
begin
  Z := FindControlZone(Control);
  if (Z <> nil) then
  begin
    if Z = FReplacementZone then
      Z.FChildControl := nil
    else
     RemoveZone(Z);
    Control.DockOrientation := doNoOrient;
    { Redraw client dock frames }
    FDockSite.Invalidate;
  end;
end;

procedure TDockTree2.RemoveZone(Zone: TDockZone2);
var
  Sibling, LastChild: TDockZone2;
  ZoneChildCount: Integer;
begin
  if Zone = nil then
    raise Exception.Create(SDockTreeRemoveError + SDockZoneNotFound);
  if Zone.FChildControl = nil then
    raise Exception.Create(SDockTreeRemoveError + SDockZoneHasNoCtl);
  ZoneChildCount := Zone.FParentZone.ChildCount;
  if ZoneChildCount = 1 then
  begin
    FTopZone.FChildZones := nil;
    FTopZone.FOrientation := doNoOrient;
  end
  else if ZoneChildCount = 2 then
  begin
    // This zone has only one sibling zone
    if Zone.FPrevSibling = nil then Sibling := Zone.FNextSibling
    else Sibling := Zone.FPrevSibling;
    if Sibling.FChildControl <> nil then
    begin
      // Sibling is a zone with one control and no child zones
      if Zone.FParentZone = FTopZone then
      begin
        // If parent is top zone, then just remove the zone
        FTopZone.FChildZones := Sibling;
        Sibling.FPrevSibling := nil;
        Sibling.FNextSibling := nil;
        Sibling.ZoneLimit := FTopZone.LimitSize;
        Sibling.Update;
      end
      else begin
        // Otherwise, move sibling's control up into parent zone and dispose of sibling
        Zone.FParentZone.FOrientation := doNoOrient;
        Zone.FParentZone.FChildControl := Sibling.FChildControl;
        Zone.FParentZone.FChildZones := nil;
        Sibling.Free;
      end;
      ForEachAt(Zone.FParentZone, UpdateZone);
    end
    else begin
      // Sibling is a zone with child zones, so sibling must be made topmost
      // or collapsed into higher zone.
      if Zone.FParentZone = FTopZone then
      begin
        // Zone is a child of topmost zone, so sibling becomes topmost
        Sibling.ExpandZoneLimit(FTopXYLimit);
        FTopXYLimit := FTopZone.ZoneLimit;
        FTopZone.Free;
        FTopZone := Sibling;
        Sibling.FNextSibling := nil;
        Sibling.FPrevSibling := nil;
        Sibling.FParentZone := nil;
        UpdateAll;
      end
      else begin
        // Zone's parent is not the topmost zone, so child zones must be
        // collapsed into parent zone
        Sibling.FChildZones.FPrevSibling := Zone.FParentZone.FPrevSibling;
        if Sibling.FChildZones.FPrevSibling = nil then
          Zone.FParentZone.FParentZone.FChildZones := Sibling.FChildZones
        else
          Sibling.FChildZones.FPrevSibling.FNextSibling := Sibling.FChildZones;
        LastChild := Sibling.FChildZones;
        LastChild.FParentZone := Zone.FParentZone.FParentZone;
        repeat
          LastChild := LastChild.FNextSibling;
          LastChild.FParentZone := Zone.FParentZone.FParentZone;
        until LastChild.FNextSibling = nil;
        LastChild.FNextSibling := Zone.FParentZone.FNextSibling;
        if LastChild.FNextSibling <> nil then
          LastChild.FNextSibling.FPrevSibling := LastChild;
        ForEachAt(LastChild.FParentZone, UpdateZone);
        Zone.FParentZone.Free;
        Sibling.Free;
      end;
    end;
  end
  else begin
    // This zone has multiple sibling zones
    if Zone.FPrevSibling = nil then
    begin
      // First zone in parent's child list, so make next one first and remove
      // from list
      Zone.FParentZone.FChildZones := Zone.FNextSibling;
      Zone.FNextSibling.FPrevSibling := nil;
      Zone.FNextSibling.Update;
    end
    else begin
      // Not first zone in parent's child list, so remove zone from list and fix
      // up adjacent siblings
      Zone.FPrevSibling.FNextSibling := Zone.FNextSibling;
      if Zone.FNextSibling <> nil then
        Zone.FNextSibling.FPrevSibling := Zone.FPrevSibling;
      Zone.FPrevSibling.ExpandZoneLimit(Zone.ZoneLimit);
      Zone.FPrevSibling.Update;
    end;
    ForEachAt(Zone.FParentZone, UpdateZone);
  end;
  Zone.Free;
end;

procedure TDockTree2.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  if not (csLoading in FDockSite.ComponentState) and
    (FTopZone <> nil) and (FDockSite.VisibleDockClientCount > 0) then
  begin
    R := FDockSite.ClientRect;
    TWinControlHack(FDockSite).AdjustClientRect(R);
    if Force or (not CompareMem(@R, @FOldRect, SizeOf(TRect))) then
    begin
      FOldRect := R;
      case FTopZone.FOrientation of
        doHorizontal:
          begin
            FTopZone.ZoneLimit := R.Right - R.Left;
            FTopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            FTopZone.ZoneLimit := R.Bottom - R.Top;
            FTopXYLimit := R.Right - R.Left;
          end;
      end;
      if FDockSite.DockClientCount > 0 then
      begin
        SetNewBounds(nil);
        if FUpdateCount = 0 then ForEachAt(nil, UpdateZone);
      end;
    end;
  end;
end;

procedure TDockTree2.ScaleZone(Zone: TDockZone2);
begin
  if Zone = nil then Exit;
  if (Zone <> nil) and (Zone.FParentZone.FOrientation = FShiftScaleOrient) then
    with Zone do
      ZoneLimit := Integer(Round(ZoneLimit * FScaleBy));
end;

procedure TDockTree2.SaveToStream(Stream: TStream);

  procedure WriteControlName(ControlName: string);
  var
    NameLen: Integer;
  begin
    NameLen := Length(ControlName);
    Stream.Write(NameLen, SizeOf(NameLen));
    if NameLen > 0 then Stream.Write(Pointer(ControlName)^, NameLen);
  end;

  procedure DoSaveZone(Zone: TDockZone2; Level: Integer);
  begin
    with Stream do
    begin
      Write(Level, SizeOf(Level));
      Write(Zone.FOrientation, SizeOf(Zone.FOrientation));
      Write(Zone.FZoneLimit, SizeOf(Zone.FZoneLimit));
      WriteControlName(Zone.GetControlName);
    end;
    // Recurse child
    if Zone.FChildZones <> nil then
      DoSaveZone(Zone.FChildZones, Level + 1);
    // Recurse sibling
    if Zone.FNextSibling <> nil then
      DoSaveZone(Zone.FNextSibling, Level);
  end;

var
  I, NVCount: Integer;
  Ctl: TControl;
  NonVisList: TStringList;
begin
  // write stream version
  Stream.Write(FVersion, SizeOf(FVersion));
  // get list of non-visible dock clients
  NonVisList := TStringList.Create;
  try
    for I := 0 to FDockSite.DockClientCount - 1 do
    begin
      Ctl := FDockSite.DockClients[I];
      if (not Ctl.Visible) and (Ctl.Name <> '') then
        NonVisList.Add(Ctl.Name);
    end;
    // write non-visible dock client list
    NVCount := NonVisList.Count;
    Stream.Write(NVCount, SizeOf(NVCount));
    for I := 0 to NVCount - 1 do WriteControlName(NonVisList[I]);
  finally
    NonVisList.Free;
  end;
  // write top zone data
  Stream.Write(FTopXYLimit, SizeOf(FTopXYLimit));
  // write all zones from tree
  DoSaveZone(FTopZone, 0);
  Stream.Write(TreeStreamEndFlag, SizeOf(TreeStreamEndFlag));
end;

procedure TDockTree2.SetNewBounds(Zone: TDockZone2);

  procedure DoSetNewBounds(Zone: TDockZone2);
  begin
    if Zone <> nil then
    begin
      if (Zone.NextVisible = nil) and (Zone <> FTopZone) and (Zone.Visible) then
      begin
        if Zone.FParentZone = FTopZone then
          Zone.ZoneLimit := FTopXYLimit
        else
          Zone.ZoneLimit := Zone.FParentZone.FParentZone.ZoneLimit;
      end;
      DoSetNewBounds(Zone.FirstVisibleChild);
      DoSetNewBounds(Zone.NextVisible);
    end;
  end;

begin
  if Zone = nil then Zone := FTopZone.FChildZones;
  DoSetNewBounds(Zone);
  { Redraw client dock frames }
  FDockSite.Invalidate;
end;

procedure TDockTree2.SetReplacingControl(Control: TControl);
begin
  FReplacementZone := FindControlZone(Control);
end;

procedure TDockTree2.ShiftZone(Zone: TDockZone2);
begin
  if (Zone <> nil) and (Zone <> FTopZone) and
     (Zone.FParentZone.FOrientation = FShiftScaleOrient) then
    Zone.ZoneLimit := Zone.ZoneLimit + FShiftBy;
end;

procedure TDockTree2.SplitterMouseDown(OnZone: TDockZone2; MousePos: TPoint);
begin
  FSizingZone := OnZone;
  Mouse.Capture := FDockSite.Handle;
  FSizingWnd := FDockSite.Handle;
  FSizingDC := GetDCEx(FSizingWnd, 0, DCX_CACHE or DCX_CLIPSIBLINGS or
    DCX_LOCKWINDOWUPDATE);
  FSizePos := MousePos;
  DrawSizeSplitter;
end;

procedure TDockTree2.SplitterMouseUp;
begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(FSizingWnd, FSizingDC);
  if FSizingZone.FParentZone.FOrientation = doHorizontal then
    FSizingZone.ZoneLimit := FSizePos.y + (FBorderWidth div 2) else
    FSizingZone.ZoneLimit := FSizePos.x + (FBorderWidth div 2);
  SetNewBounds(FSizingZone.FParentZone);
  ForEachAt(FSizingZone.FParentZone, UpdateZone);
  FSizingZone := nil;
end;

procedure TDockTree2.UpdateAll;
begin
  if (FUpdateCount = 0) and (FDockSite.DockClientCount > 0) then
    ForEachAt(nil, UpdateZone);
end;

procedure TDockTree2.UpdateZone(Zone: TDockZone2);
begin
  if FUpdateCount = 0 then Zone.Update;
end;

procedure TDockTree2.DrawSizeSplitter;
var
  R: TRect;
  PrevBrush: HBrush;
begin
  if FSizingZone <> nil then
  begin
    with R do
    begin
      if FSizingZone.FParentZone.FOrientation = doHorizontal then
      begin
        Left := FSizingZone.Left;
        Top := FSizePos.Y - (FBorderWidth div 2);
        Right := Left + FSizingZone.Width;
        Bottom := Top + FBorderWidth;
      end
      else begin
        Left := FSizePos.X - (FBorderWidth div 2);
        Top := FSizingZone.Top;
        Right := Left + FBorderWidth;
        Bottom := Top + FSizingZone.Height;
      end;
    end;
    PrevBrush := SelectObject(FSizingDC, FBrush.Handle);
    with R do
      PatBlt(FSizingDC, Left, Top, Right - Left, Bottom - Top, PATINVERT);
    SelectObject(FSizingDC, PrevBrush);
  end;
end;

function TDockTree2.GetNextLimit(AZone: TDockZone2): Integer;
var
  LimitResult: Integer;

  procedure DoGetNextLimit(Zone: TDockZone2);
  begin
    if (Zone <> AZone) and
      (Zone.FParentZone.FOrientation = AZone.FParentZone.FOrientation) and
      (Zone.ZoneLimit > AZone.ZoneLimit) and ((Zone.FChildControl = nil) or
      ((Zone.FChildControl <> nil) and (Zone.FChildControl.Visible))) then
      LimitResult := Min(LimitResult, Zone.ZoneLimit);
    if Zone.FNextSibling <> nil then DoGetNextLimit(Zone.FNextSibling);
    if Zone.FChildZones <> nil then DoGetNextLimit(Zone.FChildZones);
  end;

begin
  if AZone.FNextSibling <> nil then
    LimitResult := AZone.FNextSibling.ZoneLimit
  else
    LimitResult := AZone.ZoneLimit + AZone.LimitSize;
  DoGetNextLimit(FTopZone.FChildZones);
  Result := LimitResult;
end;

procedure TDockTree2.ControlVisibilityChanged(Control: TControl;
  Visible: Boolean);

  function GetDockAlign(Client, DropCtl: TControl): TAlign;
  var
    CRect, DRect: TRect;
  begin
    Result := alRight;
    if DropCtl <> nil then
    begin
      CRect := Client.BoundsRect;
      DRect := DropCtl.BoundsRect;
      if (CRect.Top <= DRect.Top) and (CRect.Bottom < DRect.Bottom) and
         (CRect.Right >= DRect.Right) then
        Result := alTop
      else if (CRect.Left <= DRect.Left) and (CRect.Right < DRect.Right) and
         (CRect.Bottom >= DRect.Bottom) then
        Result := alLeft
      else if CRect.Top >= ((DRect.Top + DRect.Bottom) div 2) then
        Result := alBottom;
    end;
  end;

  procedure HideZone(const Zone: TDockZone2);
  begin
    if IsOrientationSet(Zone) then
      Zone.FOldSize := Zone.FZoneLimit - Zone.LimitBegin
    else
      Zone.FOldSize := 0;

    if Assigned(Zone.FParentZone) and not (Zone.FParentZone.Visible) then
      HideZone(Zone.FParentZone);
    { When hiding, increase ZoneLimit for the zone before us }
    if Zone.PrevVisible <> nil then
      Zone.PrevVisible.ExpandZoneLimit(Zone.FZoneLimit);
    ForEachAt(Zone.FParentZone, UpdateZone);
  end;

  procedure ShowZone(const Zone: TDockZone2);
  var
    ResetAll: Boolean;
    MinSibSize: Integer;
  begin
    if Assigned(Zone.FParentZone) and (Zone.FParentZone <> FTopZone) and
       (Zone.FParentZone.VisibleChildCount = 1) then
      ShowZone(Zone.FParentZone);
    if (Zone.FParentZone.VisibleChildCount = 1) or (Zone.FOldSize = 0) then
      ResetAll := True
    else
    begin
      ResetAll := False;
      MinSibSize := FGrabberSize + FBorderWidth + 14;
      if (Zone.PrevVisible <> nil) then
        with Zone.PrevVisible do
        begin
          if ((ZoneLimit - LimitBegin) - Zone.FOldSize) < MinSibSize then
            { Resizing the previous sibling will make it too small, resize all }
            ResetAll := True
          else
          begin
            { Make room before us as needed }
            ZoneLimit := ZoneLimit - Zone.FOldSize;
            { and adjust our own zone limit to reflect the previous size }
            Zone.ZoneLimit := ZoneLimit + Zone.FOldSize;
            Zone.PrevVisible.ResetZoneLimits;
          end;
        end
      else if (Zone.NextVisible <> nil) then
      begin
        if (Zone.NextVisible.ZoneLimit - Zone.FOldSize) < MinSibSize then
          { Resizing the next sibling will make it too small, resize all }
          ResetAll := True
        else
        begin
          { Adjust zone limit to make room for controls following this one }
          Zone.ZoneLimit := Zone.LimitBegin + Zone.FOldSize;
          Zone.NextVisible.ResetZoneLimits;
        end;
      end;
    end;
    if ResetAll then
      Zone.FParentZone.ResetChildren;
    ForEachAt(Zone.FParentZone, UpdateZone);
  end;

var
  HitTest: Integer;
  CtlZone, DropCtlZone: TDockZone2;
  DropCtl: TControl;
begin
  CtlZone := FindControlZone(Control);
  if Assigned(CtlZone) then
  begin
    if Visible then
      ShowZone(CtlZone)
    else
      HideZone(CtlZone);
    FDockSite.Invalidate;
  end
  { Existing control that was never docked, create a new dock zone for it }
  else if Visible then
  begin
    DropCtlZone := InternalHitTest(Point(Control.Left, Control.Top), HitTest);
    if DropCtlZone <> nil then
      DropCtl := DropCtlZone.FChildControl
    else
      DropCtl := nil;
    InsertControl(Control, GetDockAlign(Control, DropCtl), DropCtl);
  end;
end;

procedure TDockTree2.WindowProc(var Message: TMessage);

  procedure CalcSplitterPos;
  var
    MinWidth,
    TestLimit: Integer;
  begin
    MinWidth := FGrabberSize;
    if (FSizingZone.FParentZone.FOrientation = doHorizontal) then
    begin
      TestLimit := FSizingZone.Top + MinWidth;
      if FSizePos.y <= TestLimit then FSizePos.y := TestLimit;
      TestLimit := GetNextLimit(FSizingZone) - MinWidth;
      if FSizePos.y >= TestLimit then FSizePos.y := TestLimit;
    end
    else begin
      TestLimit := FSizingZone.Left + MinWidth;
      if FSizePos.x <= TestLimit then FSizePos.x := TestLimit;
      TestLimit := GetNextLimit(FSizingZone) - MinWidth;
      if FSizePos.x >= TestLimit then FSizePos.x := TestLimit;
    end;
  end;

const
  SizeCursors: array[TDockOrientation] of TCursor = (crDefault, crVSplit, crHSplit);
var
  TempZone: TDockZone2;
  Control: TControl;
  P: TPoint;
  R: TRect;
  HitTestValue: Integer;
  Msg: TMsg;
begin
  case Message.Msg of
    CM_DOCKNOTIFICATION:
      with TCMDockNotification(Message) do
        if (NotifyRec.ClientMsg = CM_VISIBLECHANGED) then
          ControlVisibilityChanged(Client, Boolean(NotifyRec.MsgWParam));
    WM_MOUSEMOVE:
      if FSizingZone <> nil then
      begin
        DrawSizeSplitter;
        FSizePos := SmallPointToPoint(TWMMouse(Message).Pos);
        CalcSplitterPos;
        DrawSizeSplitter;
      end;
    WM_LBUTTONDBLCLK:
      begin
        TempZone := InternalHitTest(SmallPointToPoint(TWMMouse(Message).Pos),
          HitTestValue);
        if TempZone <> nil then
          with TempZone do
            if (FChildControl <> nil) and (HitTestValue = HTCAPTION) then
            begin
              CancelDrag;
              FChildControl.ManualDock(nil, nil, alTop);
            end;
      end;
    WM_LBUTTONDOWN:
      begin
        P := SmallPointToPoint(TWMMouse(Message).Pos);
        TempZone := InternalHitTest(P, HitTestValue);
        if (TempZone <> nil) then
        begin
          if HitTestValue = HTBORDER then
            SplitterMouseDown(TempZone, P)
          else if HitTestValue = HTCAPTION then
          begin
            if (not PeekMessage(Msg, FDockSite.Handle, WM_LBUTTONDBLCLK,
               WM_LBUTTONDBLCLK, PM_NOREMOVE)) and
               (TempZone.FChildControl is TWinControl) then
              TWinControl(TempZone.FChildControl).SetFocus;
            if (TControlHack(TempZone.FChildControl).DragKind = dkDock) and
               (TControlHack(TempZone.FChildControl).DragMode = dmAutomatic)then
              TempZone.FChildControl.BeginDrag(False);
            Exit;
          end;
        end;
      end;
    WM_LBUTTONUP:
      if FSizingZone = nil then
      begin
        P := SmallPointToPoint(TWMMouse(Message).Pos);
        TempZone := InternalHitTest(P, HitTestValue);
        if (TempZone <> nil) and (HitTestValue = HTCLOSE) then
          DoCloseControl(TempZone.FChildControl);
      end
      else
        SplitterMouseUp;
    WM_SETCURSOR:
      begin
        GetCursorPos(P);
        P := FDockSite.ScreenToClient(P);
        with TWMSetCursor(Message) do
          if (Smallint(HitTest) = HTCLIENT) and (CursorWnd = FDockSite.Handle)
            and (FDockSite.VisibleDockClientCount > 0) then
          begin
            TempZone := InternalHitTest(P, HitTestValue);
            if (TempZone <> nil) and (HitTestValue = HTBORDER) then
            begin
              Windows.SetCursor(Screen.Cursors[SizeCursors[TempZone.FParentZone.FOrientation]]);
              Result := 1;
              Exit;
            end;
          end;
      end;
    CM_HINTSHOW:
      with TCMHintShow(Message) do
      begin
        FOldWndProc(Message);
        if Result = 0 then
        begin
          Control := HitTest(HintInfo^.CursorPos, HitTestValue);
          if HitTestValue = HTBORDER then
            HintInfo^.HintStr := ''
          else if (Control <> nil) and (HitTestValue in [HTCAPTION, HTCLOSE]) then
          begin
            R := Control.BoundsRect;
            AdjustDockRect(Control, R);
            Dec(R.Left, 2 * (R.Left - Control.Left));
            Dec(R.Top, 2 * (R.Top - Control.Top));
            Dec(R.Right, 2 * (Control.Width - (R.Right - R.Left)));
            Dec(R.Bottom, 2 * (Control.Height - (R.Bottom - R.Top)));
            HintInfo^.HintStr := TControlHack(Control).Caption;
            HintInfo^.CursorRect := R;
          end;
        end;
        Exit;
      end;
  end;
  if Assigned(FOldWndProc) then
    FOldWndProc(Message);
end;

procedure TDockTree2.DoCloseControl(AControl: TControl);
begin
if AControl is TCustomForm then
  TCustomForm(AControl).Close
else
  AControl.Visible:=False;
end;

end.
