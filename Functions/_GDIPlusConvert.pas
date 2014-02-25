unit _GDIPlusConvert;

interface

// NOTES:
// 1. gpcConvertFaxFrameToBitmap - конвертирует TIFF'ы, полученные с факса,
//    они почему-то получаются вытяннутыми в два раза, поэтому сжимаем их
//    принудительно и сохраняем в BMP
// 2. Если модуль используется из DLL, GdiplusStartup и GdiplusShutdown
//    вызываться автоматически не будут, нужно делать это принудительно:
//      GdiplusStartup(gdiplusToken, @StartupInput, nil);
//      GdiplusShutdown(gdiplusToken);
// 3. GDI+ разумеется не выкидывает исключений, вместо них возвращает коды
//    ошибок. Данный модуль проверяет эти коды, и если они не равны Ok,
//    выбрасывает EGPCException. Следует также учесть, что при создании
//    объектов (TGPImage.Create) тоже может быть возвращен код ошибки,
//    поэтому сразу после TGPImage.Create в клиентском коде следует делать вызов
//    gpcRaiseException(GDIPlusImage.GetLastStatus);

uses SysUtils, GDIPOBJ, GDIPAPI, GDIPUTIL, _Errors, Graphics;

function gpcGetFrameCount(const AFileName: String): Integer;

procedure gpcConvertFaxFrameToBitmap(const ATiffFileName: String; AFrame: Integer; const ABitmapFileName: String); overload;
procedure gpcConvertFaxFrameToBitmap(ATiffImage: TGPImage; AFrame: Integer; const ABitmapFileName: String); overload;

const
  gpcEncoderBMP = 'image/bmp';
  gpcEncoderJPEG = 'image/jpeg';
  gpcEncoderGIF = 'image/gif';
  gpcEncoderTIFF = 'image/tiff';
  gpcEncoderPNG = 'image/png';

procedure gpcConvert(const ASourceFileName, ADestFileName, AEncoder: String);

type EGPCException = class(Exception)
  public
    Status: GpStatus;
    constructor CreateGPC(AStatus: GpStatus);
  end;

procedure gpcRaiseException(AStatus: GpStatus);
procedure gpcGetEncoderClsid(format: String; out pClsid: TGUID);

procedure gpcInit;
procedure gpcDone;

implementation

constructor EGPCException.CreateGPC(AStatus: GpStatus);
begin
Status:=AStatus;
CreateFmt('GDI+ Exception: ErrorID=%d', [Integer(AStatus)]);
end;

procedure gpcRaiseException(AStatus: GpStatus);
begin
PushError('12_001');
if AStatus<>Ok then raise EGPCException.CreateGPC(AStatus);
PopError;
end;

procedure gpcInit;
begin
PushError('12_019');
GdiplusStartup(gdiplusToken, @StartupInput, nil); { TODO : по уму тут тоже должны быть gpcRaiseException }
PopError;
end;

procedure gpcDone;
begin
PushError('12_020');
GdiplusShutdown(gdiplusToken);
PopError;
end;

function gpcGetFrameCount(const AFileName: String): Integer;
var GDIPlusImage: TGPImage;
begin
PushError('12_002');
GDIPlusImage:=TGPImage.Create(AFileName);
try
  SetError('12_003');
  gpcRaiseException(GDIPlusImage.GetLastStatus);
  SetError('12_004');
  Result:=GDIPlusImage.GetFrameCount(FrameDimensionPage);
  SetError('12_005');
  gpcRaiseException(GDIPlusImage.GetLastStatus);
finally
  GDIPlusImage.Free;
end; // try
PopError;
end;

procedure gpcConvertFaxFrameToBitmap(const ATiffFileName: String; AFrame: Integer; const ABitmapFileName: String); overload;
var TIFFImage: TGPImage;
begin
PushError('12_006');
TIFFImage:=TGPImage.Create(ATiffFileName);
try
  gpcRaiseException(TIFFImage.GetLastStatus);
  SetError('12_007');
  gpcConvertFaxFrameToBitmap(TIFFImage, AFrame, ABitmapFileName);
finally
  TIFFImage.Free;
end; // try
PopError;
end;

procedure gpcConvertFaxFrameToBitmap(ATiffImage: TGPImage; AFrame: Integer; const ABitmapFileName: String); overload;
var GDIPlusCanvas: TGPGraphics; Bmp: TBitmap;
begin
PushError('12_008');
Bmp:=TBitmap.Create;
try
  SetError('12_009');
  gpcRaiseException(ATiffImage.SelectActiveFrame(FrameDimensionPage, AFrame));

  SetError('12_010');
	Bmp.Width:=ATiffImage.GetWidth div 2;
  gpcRaiseException(ATiffImage.GetLastStatus);
  SetError('12_011');
	Bmp.Height:=ATiffImage.GetHeight;
  gpcRaiseException(ATiffImage.GetLastStatus);

  SetError('12_012');
  GDIPlusCanvas:=TGPGraphics.Create(Bmp.Canvas.Handle);
  try
    gpcRaiseException(GDIPlusCanvas.GetLastStatus);
    SetError('12_013');
	  gpcRaiseException(GDIPlusCanvas.DrawImage(ATiffImage, 0, 0));
    SetError('12_014');
	  Bmp.SaveToFile(ABitmapFileName);
  finally
    GDIPlusCanvas.Free;
  end; // try
finally
  Bmp.Free;
end; // try
PopError;
end;

procedure gpcConvert(const ASourceFileName, ADestFileName, AEncoder: String);
var GDIPlusImage: TGPImage; EncoderGUID: TGUID;
begin
PushError('12_015');
GDIPlusImage:=TGPImage.Create(ASourceFileName);
try
  gpcRaiseException(GDIPlusImage.GetLastStatus);
  SetError('12_016');
  gpcGetEncoderClsid(AEncoder, EncoderGUID);
  SetError('12_017');
  gpcRaiseException(GDIPlusImage.Save(ADestFileName, EncoderGUID));
finally
  GDIPlusImage.Free;
end; // try
PopError;
end;

procedure gpcGetEncoderClsid(format: String; out pClsid: TGUID);
begin
PushError('12_018');
if GetEncoderClsid(format, pClsid)=-1 then gpcRaiseException(InvalidParameter);
PopError;
end;

end.
