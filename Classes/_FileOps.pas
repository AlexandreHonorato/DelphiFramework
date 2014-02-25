// changes from SS 1 beta
// 1: changed interface from functions to procedures, вместо result используются exceptions

unit _FileOps;

interface

uses Windows, SysUtils, Classes, ShellAPI, _Strings;

const
  FOE_NOERROR = 0;
  FOE_ABORTED = 1;
  FOE_ERROR = 2;

type EFileOp = class(Exception)
  private
    FErrorLevel: Integer;
  public
    constructor CreateEx(const Msg: string; AErrorLevel: Integer);
    constructor CreateFmtEx(const Msg: string; const Args: array of const; AErrorLevel: Integer);
    property ErrorLevel: Integer read FErrorLevel write FErrorLevel;
  end;

procedure FileOpEx(AWnd: hWnd; AFrom: TStrings; ATo: String; AOperation: UINT;
    AOptions: Word; AProgressTitle: String; ClearFrom: Boolean); overload;

procedure FileOpEx(AWnd: hWnd; AFrom: String; ATo: String; AOperation: UINT;
    AOptions: Word; AProgressTitle: String); overload;

procedure FileOp(AFrom: TStrings; ATo: String; AOperation: UINT; AOptions: Word); overload;

procedure FileOp(AFrom: String; ATo: String; AOperation: UINT; AOptions: Word); overload;

const
  FO_MOVE           = $0001;
  FO_COPY           = $0002;
  FO_DELETE         = $0003;
  FO_RENAME         = $0004;

  FOF_MULTIDESTFILES         = $0001;
  FOF_CONFIRMMOUSE           = $0002;
  FOF_SILENT                 = $0004;  { don't create progress/report }
  FOF_RENAMEONCOLLISION      = $0008;
  FOF_NOCONFIRMATION         = $0010;  { Don't prompt the user. }
  FOF_WANTMAPPINGHANDLE      = $0020;  { Fill in SHFILEOPSTRUCT.hNameMappings
                                         Must be freed using SHFreeNameMappings }
  FOF_ALLOWUNDO              = $0040;
  FOF_FILESONLY              = $0080;  { on *.*, do only files }
  FOF_SIMPLEPROGRESS         = $0100;  { means don't show names of files }
  FOF_NOCONFIRMMKDIR         = $0200;  { don't confirm making any needed dirs }
  FOF_NOERRORUI              = $0400;  { don't put up error UI }

implementation

function StringsToPCharLen(AStrings: TStrings): Integer;
var I: Integer;
begin
Result:=0;
for i:=0 to AStrings.Count-1 do Result:=Result+Length(AStrings[i])+1;
Inc(Result);
end;

function StringsToPChar(AStrings: TStrings; var Buff: Pointer; NeedGetMem: Boolean): Integer;
var Tmp: pchar; i: integer;
begin
Result:=StringsToPCharLen(AStrings);

try
  if NeedGetMem then GetMem(Buff, Result);
  FillChar(Buff^, Result, 0);
  Tmp:=Buff;

  for i:=0 to AStrings.Count-1 do
    begin
    StrPCopy(Tmp, AStrings[i]);
    Tmp:=Tmp+Length(AStrings[i])+1;
    end;
except
  FreeMem(Buff, Result);
  Result:=0;
end; //try
end;

procedure FileOpEx(AWnd: hWnd; AFrom: TStrings; ATo: String; AOperation: UINT;
    AOptions: Word; AProgressTitle: String; ClearFrom: Boolean); overload;
var SHF: TSHFileOpStruct;
var FromLen, ARes: Integer; I: Integer;
var Buff: array [0..1023] of Char;
begin
FromLen:=0; // avoid compiler warnings
if AFrom.Count=0 then Exit;

SHF.Wnd:=AWnd;

with SHF do
  begin
  wFunc:=AOperation;
  if wFunc=FO_RENAME then for i:=AFrom.Count-1 downto 1 do AFrom.Delete(i);

  fFlags := AOptions;
  fAnyOperationsAborted := False;
  hNameMappings := nil;
  lpszProgressTitle := PChar(AProgressTitle);

  try
    FromLen:=StringsToPChar(AFrom, Pointer(SHF.pFrom), True);
    if FromLen=0 then raise Exception.Create('Ошибка при создании списка файлов');
    SHF.pTo:=PChar(ATo+#0);

    ARes:=SHFileOperation(SHF);
    if ClearFrom then AFrom.Clear;
    if (ARes <> 0) {and (ARes <> ERROR_NO_MORE_FILES) }then
      begin
      FillChar(Buff, SizeOf(Buff), 0);
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ARes, 0, Buff, SizeOf(Buff), nil);
      raise EFileOp.CreateEx(StrPas(Buff), FOE_ERROR);
      end;
    if fAnyOperationsAborted then raise EFileOp.CreateEx('Операция отменена', FOE_ABORTED);
  finally
    FreeMem(SHF.pFrom, FromLen);
    SHF.pFrom:=NIL;
  end; // try
  end; //with
end;


procedure FileOpEx(AWnd: hWnd; AFrom: String; ATo: String; AOperation: UINT;
    AOptions: Word; AProgressTitle: String); overload;
var Tmp: TStringList;
begin
Tmp:=TStringList.Create;
Tmp.Add(AFrom);
try
  FileOpEx(AWnd, Tmp, ATo, AOperation, AOptions, AProgressTitle, False);
finally
  Tmp.Free;
end; // try
end;

procedure FileOp(AFrom: TStrings; ATo: String; AOperation: UINT; AOptions: Word);
begin
FileOpEx(0, AFrom, ATo, AOperation, AOptions, '', True);
end;

procedure FileOp(AFrom: String; ATo: String; AOperation: UINT; AOptions: Word);
begin
FileOpEx(0, AFrom, ATo, AOperation, AOptions, '');
end;

{ EFileOp }

constructor EFileOp.CreateEx(const Msg: string; AErrorLevel: Integer);
begin
inherited Create(Msg);
FErrorLevel:=AErrorLevel;
end;

constructor EFileOp.CreateFmtEx(const Msg: string;
  const Args: array of const; AErrorLevel: Integer);
begin
inherited CreateFmt(Msg, Args);
FErrorLevel:=AErrorLevel;
end;

end.