unit _UFKBBanner;

interface

uses Windows, Classes, Controls, ExtCtrls, Graphics, _GDI;

type
  TBannerOption = (boCenterCaption, boMultilineCaption);
  TBannerOptions = set of TBannerOption;

type TUFKBBanner = class(TPaintBox)
  private
    bmpLogo, bmpBG: TBitmap;
    FRightPicture: TPicture;
    FOnPaint: TNotifyEvent;
    FCaption: String;
    FOptions: TBannerOptions;
    FCenterPicture: TPicture;
    procedure SetRightPicture(const Value: TPicture);
    procedure SetCaption(const Value: String);
    procedure SetOptions(const Value: TBannerOptions);
    procedure SetCenterPicture(const Value: TPicture);
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RightPicture: TPicture read FRightPicture write SetRightPicture;
    property CenterPicture: TPicture read FCenterPicture write SetCenterPicture;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Caption: String read FCaption write SetCaption;
    property Options: TBannerOptions read FOptions write SetOptions;
  end;

procedure Register;

implementation

{ TUFKBBanner }

{$R _UFKBBanner.RES}

procedure Register;
begin
RegisterComponents('My components', [TUFKBBanner]);
end;

function TUFKBBanner.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
Result:=NewHeight=64;
end;

constructor TUFKBBanner.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FOptions:=[boMultilineCaption];

Height:=64;
Width:=300;
Align:=alTop;

bmpLogo:=TBitmap.Create;
bmpLogo.LoadFromResourceName(hInstance, 'bmpUFKBBannerLogo');

bmpBG:=TBitmap.Create;
bmpBG.LoadFromResourceName(hInstance, 'bmpUFKBBannerBG');

FRightPicture:=TPicture.Create;
FCenterPicture:=TPicture.Create;
end;

destructor TUFKBBanner.Destroy;
begin
FRightPicture.Free;
FCenterPicture.Free;
bmpLogo.Free;
bmpBG.Free;
inherited Destroy;
end;

procedure TUFKBBanner.Paint;
var R, R1: TRect; H: Integer; wWorkArea, lWorkArea, lCenter, wCenter: Integer; rWorkArea: TRect;
begin
wWorkArea:=Width-bmpLogo.Width-FRightPicture.Width;
lWorkArea:=bmpLogo.Width;
rWorkArea:=Bounds(lWorkArea, 0, wWorkArea, Height);

Canvas.Draw(0, 0, bmpLogo);
if Assigned(FRightPicture.Graphic) then Canvas.Draw(rWorkArea.Right, 0, FRightPicture.Graphic);
Canvas.StretchDraw(rWorkArea, bmpBG);

if Assigned(FCenterPicture.Graphic) then
  begin
  wCenter:=FCenterPicture.Width;
  if boCenterCaption in FOptions then
    lCenter:=(Width-wCenter) div 2
  else
    lCenter:=bmpLogo.Width+(wWorkArea-wCenter) div 2;
  Canvas.Draw(lCenter, 0, FCenterPicture.Graphic);
  end;

if boCenterCaption in FOptions then
  R:=ClientRect
else
  R:=rWorkArea;

Canvas.Font.Assign(Font);
SetBkMode(Canvas.Handle, TRANSPARENT);
if boMultilineCaption in FOptions then
  begin
  R1:=R;
  H:=MyDrawText(Canvas.Handle, FCaption, R1, DT_CALCRECT or DT_WORDBREAK); // используем R1, иначе исходный Rect будет испорчен
  if H<Height then R.Top:=(R.Bottom-H) div 2;
  MyDrawText(Canvas.Handle, FCaption, R, DT_CENTER or DT_WORDBREAK);
  end
else
  MyDrawText(Canvas.Handle, FCaption, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

if Assigned(FOnPaint) then FOnPaint(Self)
end;

procedure TUFKBBanner.SetCaption(const Value: String);
begin
FCaption:=Value;
Invalidate
end;

procedure TUFKBBanner.SetCenterPicture(const Value: TPicture);
begin
if Assigned(Value) and (Value.Height<>64) then exit;
FCenterPicture.Assign(Value);
Invalidate;
end;

procedure TUFKBBanner.SetOptions(const Value: TBannerOptions);
begin
FOptions:=Value;
Invalidate
end;

procedure TUFKBBanner.SetRightPicture(const Value: TPicture);
begin
if Assigned(Value) and (Value.Height<>64) then exit;
FRightPicture.Assign(Value);
Invalidate;
end;

end.
