unit _GDI;

interface

uses Windows, Graphics, Themes;

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

implementation

uses Classes;

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
