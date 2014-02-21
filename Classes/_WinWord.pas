unit _WinWord;

interface

uses Windows, Graphics, TLB_Word_XP, TLB_Office_XP, TLB_Excel_XP, Variants, Clipbrd,
  _Files, _Misc;

//procedure GenerateDoc;
//var WG: TWordGenerator; TemplateFileName, DocFileName: String;
//begin
//TemplateFileName:=|;
//DocFileName:=|;
//
//try
//  WG:=TWordGenerator.Create;
//  try
//    WG.CreateDocAsCopyOfTemplate(TemplateFileName, DocFileName, True);
//
//    |
//
//    WG.DocSaveAndClose(DocFileName);
//  finally
//    WG.Free;
//  end; // try
//except
//  on E: Exception do
//    raise Exception.Create('Ошибка при создании документа:'+#13#10#13#10+E.Message);
//end; // try
//end;

type TWordGenerator = class
  public
    App: WordApplication;
    Doc: WordDocument;

    procedure InsertText(const Text: String); // must be declared before InsertTemplate
    procedure InsertTemplate(const AFileName: String);
    procedure DocMoveRight;
    procedure TableMoveRight(ACount: Integer = 1);
    procedure TableMoveLeft(ACount: Integer = 1);
    procedure DocMoveDown;
    procedure TableCellColor(AColor: TColor);
    procedure PageBreak;
    procedure SectionBreakNextPage; // новый раздел со следующей страницы
    procedure DocSaveAndClose(const AFileName: String);
    procedure BeginOfDoc;
    procedure EndOfDoc;
    procedure TableNextRow;
    procedure GotoBookmark(const BMName: String);
    procedure TextAtBookmark(const BMName: String; const Text: String);
    procedure AddBookmark(const BMName: String);

    procedure DocNew(const AFileName: String = '');
    procedure DocOpen(const AFileName: String);
    procedure DocClose;

    procedure CreateDocAsCopyOfTemplate(const ATemplateFileName, ADocFileName: String; OpenDoc: Boolean);
    constructor Create;
    destructor Destroy; override;
  end;

type TExcelGenerator = class
  public
    App: ExcelApplication;
    Doc: ExcelWorkbook;
    procedure DocNew(const AFileName: String = '');
    procedure DocOpen(const AFileName: String);
    procedure DocSaveAndClose(const AFileName: String = '');
    procedure CreateDocAsCopyOfTemplate(const ATemplateFileName, ADocFileName: String; OpenDoc: Boolean);
    constructor Create;
    destructor Destroy; override;
    function GetRange(WS: _Worksheet; ARow, ACol, ARowCount, AColCount: Integer): ExcelRange;
    function DataAsACV(WS: _Worksheet; ARow, ACol, ARowCount, AColCount: Integer; const AData: String): ExcelRange;
    procedure ThinBorder(AIndex: OleVariant; ARange: ExcelRange);
  end;

implementation

{ TWordGenerator }

procedure TWordGenerator.TableNextRow;
var V: OleVariant;
begin
V:=wdRow;
App.Selection.EndKey(V, EmptyParam);
V:=wdCell;
App.Selection.MoveRight(V, EmptyParam, EmptyParam);
end;

procedure TWordGenerator.InsertText(const Text: String);
var V: OleVariant;
begin
V:=Text;
App.Selection.TypeText(V);
end;

procedure TWordGenerator.GotoBookmark(const BMName: String);
var V, V1: OleVariant;
begin
{$WARNINGS OFF}
V:=wdGoToBookmark;
{$WARNINGS ON}
V1:=BMName;
App.Selection.GoTo_(V, EmptyParam, EmptyParam, V1);
end;

procedure TWordGenerator.CreateDocAsCopyOfTemplate(const ATemplateFileName, ADocFileName: String; OpenDoc: Boolean);
var V: OleVariant;
begin
//Create;

MyCopyFile(ATemplateFileName, ADocFileName, False, True);

if OpenDoc then
  begin
  V:=ADocFileName;
  Doc:=App.Documents.Open(V, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
  Doc.Activate;
  end;
end;

destructor TWordGenerator.Destroy;
var V: OleVariant;
begin
if Assigned(App) then
  begin
  V:=msoFalse;
  App.Quit(V, EmptyParam, EmptyParam);
  end;
App:=NIL;

inherited Destroy;
end;

procedure TWordGenerator.DocSaveAndClose(const AFileName: String);
begin
OleVariant(Doc).SaveAs(FileName := AFileName);
Doc.Close(EmptyParam, EmptyParam, EmptyParam);
Doc:=NIL;
end;

procedure TWordGenerator.EndOfDoc;
var V: OleVariant;
begin
V:=wdStory;
App.Selection.EndKey(V, EmptyParam);
end;

procedure TWordGenerator.PageBreak;
var V: OleVariant;
begin
V:=wdPageBreak;
App.Selection.InsertBreak(V)
end;

procedure TWordGenerator.TableCellColor(AColor: TColor);
var V, V1, V2: OleVariant;
begin
V:=wdCharacter; V1:=1; V2:=wdExtend;
App.Selection.MoveRight(V, V1, V2);
App.Selection.Cells.Shading.BackgroundPatternColor:=AColor;
end;

procedure TWordGenerator.DocMoveRight;
var V, V1: OleVariant;
begin
V:=wdCharacter;
V1:=1;
App.Selection.MoveRight(V, V1, EmptyParam);
end;

procedure TWordGenerator.InsertTemplate(const AFileName: String);
var Template: WordDocument; V: OleVariant;
begin
V:=AFileName;
Template:=App.Documents.Open(V, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
  EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
  EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);

Template.Activate;
App.Selection.WholeStory;
App.Selection.Copy;
Template.Close(EmptyParam, EmptyParam, EmptyParam);

Doc.Activate;
App.Selection.PasteAndFormat(wdPasteDefault);
end;

procedure TWordGenerator.DocMoveDown;
var V, V1: OleVariant;
begin
V:=wdLine;
V1:=1;
App.Selection.MoveDown(V, V1, EmptyParam);
end;

procedure TWordGenerator.TableMoveRight(ACount: Integer = 1);
var V, V1: OleVariant;
begin
V:=wdCell;
V1:=ACount;
App.Selection.MoveRight(V, V1, EmptyParam);
end;

procedure TWordGenerator.TextAtBookmark(const BMName, Text: String);
begin
GotoBookmark(BMName);
InsertText(Text);
end;

procedure TWordGenerator.DocNew(const AFileName: String);
begin
Doc:=App.Documents.Add(EmptyParam, EmptyParam, EmptyParam, EmptyParam);
Doc.Activate;
if AFileName<>'' then OleVariant(Doc).SaveAs(FileName := AFileName);
end;

procedure TWordGenerator.DocOpen(const AFileName: String);
var V: OleVariant;
begin
V:=AFileName;
Doc:=App.Documents.Open(V, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
Doc.Activate;
end;

procedure TWordGenerator.DocClose;
begin
Doc.Close(EmptyParam, EmptyParam, EmptyParam);
Doc:=NIL;
end;

constructor TWordGenerator.Create;
begin
inherited Create;
App:=MyCreateOleObject('Word.Application') as WordApplication;
App.Visible:=False;
end;

procedure TWordGenerator.SectionBreakNextPage;
var V: OleVariant;
begin
V:=wdSectionBreakNextPage;
App.Selection.InsertBreak(V)
end;

procedure TWordGenerator.AddBookmark(const BMName: String);
var V, V1: OleVariant;
begin
V:=BMName;
V1:=App.Selection.Range;
Doc.Bookmarks.Add(V, V1);
end;

procedure TWordGenerator.TableMoveLeft(ACount: Integer);
var V, V1: OleVariant;
begin
V:=wdCell;
V1:=ACount;
App.Selection.MoveLeft(V, V1, EmptyParam);
end;

procedure TWordGenerator.BeginOfDoc;
var V: OleVariant;
begin
V:=wdStory;
App.Selection.HomeKey(V, EmptyParam);
end;

{ TExcelGenerator }

constructor TExcelGenerator.Create;
begin
inherited Create;
App:=MyCreateOleObject('Excel.Application') as ExcelApplication;
App.Visible[0]:=False;
end;

procedure TExcelGenerator.CreateDocAsCopyOfTemplate(
  const ATemplateFileName, ADocFileName: String; OpenDoc: Boolean);
var V: OleVariant;
begin
//Create;

MyCopyFile(ATemplateFileName, ADocFileName, False, True);

if OpenDoc then
  begin
  V:=ADocFileName;
  Doc:=App.Workbooks.Open(V, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, 0);
  Doc.Activate(0);
  end;
end;

procedure BufferToClipboard(Buffer: String);
var WideBuffer: WideString; BuffSize: Cardinal; Data: THandle; DataPtr: Pointer;
begin
  if Buffer <> '' then begin
    WideBuffer := Buffer;
    BuffSize := length(Buffer) * SizeOf(WideChar);
    Data := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE+GMEM_ZEROINIT, BuffSize + 2);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(PWideChar(WideBuffer)^, Pointer(Cardinal(DataPtr))^, BuffSize);
      finally
        GlobalUnlock(Data);
      end;
      Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
    except
      GlobalFree(Data);
      raise;
    end;
  end;
end;

function TExcelGenerator.DataAsACV(WS: _Worksheet; ARow, ACol, ARowCount, AColCount: Integer; const AData: String):
    ExcelRange;
begin
BufferToClipboard(AData);
Result:=GetRange(WS, ARow, ACol, ARowCount, AColCount);
OLEVariant(Result).PasteSpecial;
Clipboard.Clear;
end;

destructor TExcelGenerator.Destroy;
begin
if Assigned(App) then App.Quit;
App:=NIL;
inherited Destroy;
end;

procedure TExcelGenerator.DocNew(const AFileName: String);
begin
Doc:=App.Workbooks.Add(EmptyParam, 0);
Doc.Activate(0);
if AFileName<>'' then OleVariant(Doc).SaveAs(FileName := AFileName);
end;

procedure TExcelGenerator.DocOpen(const AFileName: String);
var V: OleVariant;
begin
V:=AFileName;
Doc:=App.Workbooks.Open(V, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
      EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, 0);
Doc.Activate(0);
end;

procedure TExcelGenerator.DocSaveAndClose(const AFileName: String = '');
begin
if AFileName='' then
  Doc.Save(0)
else
  OleVariant(Doc).SaveAs(FileName := AFileName);
Doc.Close(EmptyParam, EmptyParam, EmptyParam, 0);
Doc:=NIL;
end;

function TExcelGenerator.GetRange(WS: _Worksheet; ARow, ACol, ARowCount, AColCount: Integer): ExcelRange;
var IR1, IR2: IRange;
begin
if Doc=NIL then exit;
IDispatch(IR1):=WS.Cells.Item[ARow, ACol];
IDispatch(IR2):=WS.Cells.Item[ARow+ARowCount-1, ACol+AColCount-1];
Result:=WS.Range[IR1, IR2];
end;

procedure TExcelGenerator.ThinBorder(AIndex: OleVariant; ARange: ExcelRange);
begin
with ARange.Borders.Item[AIndex] do
  begin
  LineStyle:=xlContinuous;
  Weight:=xlThin;
  ColorIndex:=xlAutomatic;
  end;
end;

end.
