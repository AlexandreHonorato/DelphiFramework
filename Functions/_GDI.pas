unit _GDI;

interface

uses SysUtils, Windows, Graphics, Themes;

function MyDrawText(DC: HDC; S: String; var R: TRect; Format: Word = DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX): Integer;

procedure DrawMaskedBitmap(DC: hDC; X, Y: Integer; Bmp, BmpMask: TBitmap);

// переобъявление функции из ShellAPI
type PHICON = ^hIcon;
function ExtractIconEx(lpszFile: PChar; nIconIndex: Integer;
  phiconLarge, phiconSmall: PHICON; nIcons: UINT): UINT; stdcall; external 'shell32.dll' name 'ExtractIconExA';

function ExtractIconSize(FName: String; Index: Integer = 0; Small: Boolean = True): hIcon;

const CHECK_SIZE = 13;
procedure DrawCheck(ACanvas: TCanvas; ARect: TRect; AChecked: Boolean); overload;
procedure DrawCheck(ACanvas: TCanvas; ALeft, ATop: Integer; AChecked: Boolean); overload;

procedure RotateBitmap90Degrees(Bitmap: TBitmap; ClockWise: Boolean);

implementation

uses Classes;

procedure RotateBitmap90Degrees(Bitmap: TBitmap; ClockWise: Boolean);

type
  TRGB = record
    B, G, R: Byte;
  end;
  pRGB = ^TRGB;

  pByteArray = ^TByteArray;

var x, y, W, H, v1, v2: Integer;
    Dest: pRGB;
    VertArray: array of pByteArray;
    Bmp: TBitmap;

begin
Bitmap.PixelFormat := pf24Bit;
Bmp := TBitmap.Create;

try
  Bmp.Assign(Bitmap);
  W := Bitmap.Width - 1;
  H := Bitmap.Height - 1;

  Bitmap.Width := H + 1;
  Bitmap.Height := W + 1;
  SetLength(VertArray, H + 1);
  v1 := 0;
  v2 := 0;
  if ClockWise then v1 := H else v2 := W;
  for y := 0 to H do VertArray[y] := Bmp.ScanLine[Abs(v1 - y)];
  for x := 0 to W do
    begin
    Dest := Bitmap.ScanLine[x];
    for y := 0 to H do
      begin
      v1 := Abs(v2 - x)*3;
      with Dest^ do
        begin
        B := VertArray[y, v1];
        G := VertArray[y, v1+1];
        R := VertArray[y, v1+2];
        end;
      Inc(Dest);
      end;
    end
finally
  Bmp.Free;
end; // try
end;

procedure DrawCheck(ACanvas: TCanvas; ARect: TRect; AChecked: Boolean);
var
  DrawState: Integer;
  ElementDetails: TThemedElementDetails;
begin
if ThemeServices.ThemesEnabled then
  begin
  if AChecked then
    ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
  else
    ElementDetails := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  ThemeServices.DrawElement(ACanvas.Handle, ElementDetails, ARect);
  end
else
  begin
  if AChecked then
    DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED
  else
    DrawState := DFCS_BUTTONCHECK;
  DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DrawState);
  end;
end;

procedure DrawCheck(ACanvas: TCanvas; ALeft, ATop: Integer; AChecked: Boolean);
var rect: TRect;
begin
rect := Bounds(ALeft, ATop, CHECK_SIZE, CHECK_SIZE);
DrawCheck(ACanvas, rect, AChecked);
end;

function ExtractIconSize(FName: String; Index: Integer = 0; Small: Boolean = True): hIcon;
begin
Result:=0;
if Small then
  ExtractIconEx(PChar(FName), Index, NIL, @Result, 1)
else
  ExtractIconEx(PChar(FName), Index, @Result, NIL, 1)
end;

function MyDrawText(DC: HDC; S: String; var R: TRect; Format: Word = DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX): Integer;
begin
Result:=DrawText(DC, PChar(S), -1, R, Format);
end;

procedure DrawMaskedBitmap(DC: hDC; X, Y: Integer; Bmp, BmpMask: TBitmap);
const ROP_DstCopy = $00AA0029;
begin
MaskBlt(DC, X, Y, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0,
  BmpMask.Handle, 0, 0, MakeRop4(ROP_DstCopy, SrcCopy))
end;

end.
