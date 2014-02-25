unit _Strings;

interface

{$I ..\CompilerVer.inc}

uses SysUtils, Classes, _IntList, Windows, ShlObj, Clipbrd;

{$IFDEF DELPHI_XE}
type TCharSet = TSysCharSet;
{$ELSE}
type TCharSet = set of char;
{$ENDIF}

type
  TWordBoundsRec = record
    Start, Len: Word;
  end;
  TWordBoundsArray = array[0..1024] of TWordBoundsRec;
  PWordBoundsArray = ^TWordBoundsArray;
  TWordBounds = record
    Count: Integer;
    Bounds: PWordBoundsArray;
  end;

function TC(const S1: string; const S2: string): Boolean;
function StrTail(const S: String; P: Integer): String;
function LeadingSymbols(const S: String; Width: Integer; LeadingChar: Char): String;
procedure ParseWords(const AString: String; AWords: TStrings; ASeparators: TCharSet);
procedure StrToClipbrd(StrValue: string);
function GetCase(Number:Integer;
  nominative,         // Страница (им.п., ед.ч.)
  genitive_singular,  // Страницы (род.п., ед.ч.)
  genitive_plural     // Страниц (род.п., мн.ч.)
  : String): String;

function AppFile(const Ext: String): String;
function AppPath: String;
function CheckSlash(Path: String): String;
function CheckSlash2(Path: String): String;
function AppDataDir: String;
function TempPath: String;

function GetCmdParam(const AParamStr: String; AParam: Char; AReturnFirstLetter: Boolean = False): String; overload;
function GetCmdParam(AParam: Char; AReturnFirstLetter: Boolean = False): String; overload;

function UserLogin: String;
function ComputerName: String;

function ExpandEnvStrings(const Value: String): String;

function IsPas(const AFileName: String): Boolean;

function Date2FileName(ADate: TDateTime): String;

implementation

function Date2FileName(ADate: TDateTime): String;
var Y, M, D: Word;
begin
DecodeDate(ADate, Y, M, D);
Result:=IntToStr(Y)+'.'+LeadingSymbols(IntToStr(M), 2, '0')+'.'+LeadingSymbols(IntToStr(D), 2, '0');
end;

function IsPas(const AFileName: String): Boolean;
begin
Result:=TC(ExtractFileExt(AFileName), '.PAS');
end;

function ExpandEnvStrings(const Value: String): String;
var Src, Dst: array[0..255] of Char;
begin
FillChar(Src, SizeOf(Src), 0);
FillChar(Dst, SizeOf(Dst), 0);
StrPCopy(Src, Value);
ExpandEnvironmentStrings(Src, Dst, SizeOf(Src));
Result:=StrPas(Dst)
end;

function TempPath: String;
var I: Integer;
begin
SetLength(Result, MAX_PATH);
i:=GetTempPath(Length(Result), PChar(Result));
SetLength(Result, i);
Result:=CheckSlash(Result);
end;

function GetCase(Number:Integer;
  nominative,         // Страница (им.п., ед.ч.)
  genitive_singular,  // Страницы (род.п., ед.ч.)
  genitive_plural     // Страниц (род.п., мн.ч.)
  : String): String;
var last_digit, last_two_digits: Integer;
begin
last_digit:=Number mod 10;
last_two_digits:=Number mod 100;
if (last_digit=1) and (last_two_digits<>11) then
  Result:=nominative
else
  if ((last_digit=2) and (last_two_digits<>12)) or ((last_digit=3)
      and (last_two_digits<>13)) or ((last_digit=4) and (last_two_digits<>14)) then
    Result:=genitive_singular
  else
    Result:=genitive_plural;
end;

function GetParamStr(P: PChar; var Param: string): PChar;
var
  i, Len: Integer;
  Start, S, Q: PChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      P := CharNext(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNext(P);
        Inc(Len, Q - P);
        P := Q;
      end;
      if P[0] <> #0 then
        P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      Inc(Len, Q - P);
      P := Q;
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      P := CharNext(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q := CharNext(P);
        while P < Q do
        begin
          S[i] := P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then P := CharNext(P);
    end
    else
    begin
      Q := CharNext(P);
      while P < Q do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result := P;
end;

function GetCmdParam(const AParamStr: String; AParam: Char; AReturnFirstLetter: Boolean = False): String;
var P: PChar; tmp: String; pos: Integer;
begin
Result:='';
P:=PChar(AParamStr);

while True do
  begin
  P:=GetParamStr(P, tmp);
  if tmp='' then break;

  if (Length(tmp)>2) and (tmp[1]='-') and (tmp[2]=AParam) then
    begin
    if AReturnFirstLetter then pos:=1 else pos:=2;
    Result:=StrTail(tmp, pos);
    break;
    end;
  end;
end;

function GetCmdParam(AParam: Char; AReturnFirstLetter: Boolean = False): String;
begin
Result:=GetCmdParam(GetCommandLine, AParam, AReturnFirstLetter);
end;

function CheckSlash2(Path: String): String;
begin
Result:=Path;
if Result='' then exit;
if Result[Length(Result)]='\' then SetLength(Result, Length(Result)-1);
end;

function CheckSlash(Path: String): String;
begin
Result:=Path;
if Path='' then exit;
if Result[Length(Result)]<>'\' then Result:=Result+'\'
end;

function AppPath: String;
begin
Result:=CheckSlash(ExtractFilePath(ParamStr(0)))
end;

function AppFile(const Ext: String): String;
begin
Result:=ChangeFileExt(ParamStr(0), Ext);
end;

function TC(const S1: string; const S2: string): Boolean;
begin
Result:=AnsiUpperCase(S1)=AnsiUpperCase(S2);
end;

function StrTail(const S: String; P: Integer): String;
begin
if P>=Length(S) then
  Result:=''
else
  Result:=copy(S, P+1, Length(S)-P);
end;

function LeadingSymbols(const S: String; Width: Integer; LeadingChar: Char): String;
var I: Integer;
begin
Result:='';
for i:=1 to Width-Length(S) do Result:=Result+LeadingChar;
Result:=Result+S;
end;

procedure ParseWords(const AString: String; AWords: TStrings; ASeparators: TCharSet);
var I, P: Integer; IL: TIntList; S, S1: String; C: Char; WB: TWordBoundsRec;
begin
if ASeparators=[] then raise Exception.Create('Separators set can''t be empty');

S:=AString;
if S='' then exit;

// если последний символ строки не является разделителем, то добавляем к строке разделитель
{$IFDEF DELPHI_XE}
if not CharInSet(S[Length(S)], ASeparators) then
{$ELSE}
if not (S[Length(S)] in ASeparators) then
{$ENDIF}
  for C:=Low(Char) to High(Char) do
{$IFDEF DELPHI_XE}
    if CharInSet(C, ASeparators) then
{$ELSE}
    if C in ASeparators then
{$ENDIF}
      begin
      S:=S+C;
      break;
      end;

AWords.Clear;

// определяем позиции, в которых находятся разделители и количество слов
IL:=TIntList.Create;
P:=1;
for i:=1 to Length(S) do
{$IFDEF DELPHI_XE}
  if CharInSet(S[i], ASeparators) then
{$ELSE}
  if S[i] in ASeparators then
{$ENDIF}
    begin
    WB.Start:=P;
    WB.Len:=i-P;
    if WB.Len<>0 then IL.Add(Integer(WB));
    P:=i+1;
    end;

// определяем слова и заполняем массив WordBounds (если пользователь принудительно прервал,
// то WordBounds останется недозаполненным)
if IL.Count<>0 then
	for i:=0 to IL.Count-1 do
	  begin
	  WB:=TWordBoundsRec(IL[i]);
	  S1:=copy(S, WB.Start, WB.Len);
    AWords.Add(S1);
	  end;

IL.Free;
end;

function AppDataDir: String;
var C: array[0..255] of char;
begin
FillChar(C, SizeOf(C), 0);
SHGetSpecialFolderPath(0, c, CSIDL_APPDATA, False);
Result:=CheckSlash(C)
end;

{$IFDEF DELPHI_XE}
procedure StrToClipbrd(StrValue: string);
begin
Clipboard.AsText:=StrValue
end;
{$ELSE}
procedure StrToClipbrd(StrValue: string);
var
  hMem: THandle;
  pMem: PChar;
begin
  hMem := GlobalAlloc(GHND or GMEM_SHARE, Length(StrValue) + 1);
  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      StrPCopy(pMem, StrValue);
      GlobalUnlock(hMem);
      if OpenClipboard(0) then
      begin
        EmptyClipboard;
        SetClipboardData(CF_TEXT, hMem);
        CloseClipboard;
      end
      else
        GlobalFree(hMem);
    end
    else
      GlobalFree(hMem);
  end;
end;
{$ENDIF}

function UserLogin: String;
var C: array[0..255] of char; DW: DWORD;
begin
FillChar(C, SizeOf(C), 0);
DW:=SizeOf(C);
GetUserName(C, DW);
Result:=C;
end;

function ComputerName: String;
var C: array[0..50] of char; DW: DWORD;
begin
FillChar(C, SizeOf(C), 0);
DW:=sizeof(C);
if GetComputerName(@C, DW) then Result:=C else Result:='';
end;

end.
