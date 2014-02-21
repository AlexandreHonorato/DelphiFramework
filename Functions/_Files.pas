unit _Files;

interface

{$I ..\CompilerVer.inc}

uses Windows, SysUtils, Classes, ShellAPI, _Strings;

{$WARNINGS OFF}
const faFile = faAnyFile-faVolumeID-faDirectory;
const faFolder = faAnyFile-faVolumeID;
{$WARNINGS ON}

function Execute(const FileName: String; const Parameters: String = ''; const Directory: String = ''; ShowCmd: Integer = SW_SHOWNORMAL): HINST;
procedure Str2File(const S, AFileName: String);
function File2Str(const AFileName: String): String;

procedure MyDeleteFile(const AFileName: String);
procedure MyCopyFile(const AFrom: String; const ATo: String; AFailIfExists: BOOL; AForceToDir: Boolean);
procedure MyMoveFile(const AFrom: String; const ATo: String; AForceToDir: Boolean);

procedure f_ForceDirectories(Dir: string);
function f_FileExists(AFileName: String): Boolean;
function f_FolderExists(AFileName: String): Boolean;

function f_GetFileChangeTime(const AFileName: String): _FILETIME;
function f_GetFileSize(FName: String): Int64;
procedure f_SetFileChangeTime(const AFileName: String; AFileTime: _FILETIME);

function f_GetFileVersion(const AFileName: String): String;

implementation

function f_GetFileSize(FName: String): Int64;
var SR: TSearchRec;
begin
FillChar(SR, SizeOf(SR), 0);
if FindFirst(FName, faFile, SR)=0 then
  Result:=SR.Size
else
  Result:=-1;
FindClose(SR)
end;

function f_GetFileChangeTime(const AFileName: String): _FILETIME;
var h: Integer;
begin
if not FileExists(AFileName) then
  begin
  FillChar(Result, SizeOf(Result), 0);
  exit;
  end;

h:=FileOpen(AFileName, fmOpenRead);
try
  GetFileTime(h, NIL, NIL, @Result);
finally
  FileClose(h);
end; // try
end;

procedure f_SetFileChangeTime(const AFileName: String; AFileTime: _FILETIME);
var h: Integer;
begin
if not FileExists(AFileName) then exit;

h:=FileOpen(AFileName, fmOpenWrite);
try
  SetFileTime(h, NIL, NIL, @AFileTime);
finally
  FileClose(h);
end; // try
end;

function f_FolderExists(AFileName: String): Boolean;
var SR: TSearchRec;
begin
Result:=False;
if AFileName<>'' then
	begin
	Result:=FindFirst(CheckSlash(AFileName)+'*.*', faAnyFile, SR)=0;
	SysUtils.FindClose(SR);
	end;
end;

function f_FileExists(AFileName: String): Boolean;
var SR: TSearchRec;
begin
if AFileName='' then
  Result:=False
else
	begin
	Result:=FindFirst(AFileName, faFile, SR)=0;
	FindClose(SR)
	end;
end;

procedure f_ForceDirectories(Dir: string);
begin
if Length(Dir)=0 then raise Exception.Create('Ошибка при создании папки "'+Dir+'"');

Dir:=CheckSlash2(Dir);
if (Length(Dir)<3) or f_FolderExists(Dir) or (ExtractFilePath(Dir)=Dir) then Exit; // avoid 'xyz:\' problem.
f_ForceDirectories(ExtractFilePath(Dir));

if not CreateDir(Dir) then raise Exception.Create('Ошибка при создании папки "'+Dir+'"');
end;

procedure MyCopyFile(const AFrom: String; const ATo: String; AFailIfExists: BOOL; AForceToDir: Boolean);
begin
if AForceToDir then ForceDirectories(ExtractFilePath(ATo));
if not CopyFile(PChar(AFrom), PChar(ATo), AFailIfExists) then
  raise Exception.Create(SysErrorMessage(GetLastError));
end;

procedure MyMoveFile(const AFrom: String; const ATo: String; AForceToDir: Boolean);
begin
if AForceToDir then ForceDirectories(ExtractFilePath(ATo));
if not MoveFile(PChar(AFrom), PChar(ATo)) then
  raise Exception.CreateFmt('Ошибка при перемещении файла "%s": %s',
   [ExtractFileName(AFrom), SysErrorMessage(GetLastError)]);
end;

procedure MyDeleteFile(const AFileName: String);
begin
if not SysUtils.DeleteFile(AFileName) then
  raise Exception.Create(SysErrorMessage(GetLastError));
end;

function Execute(const FileName: String; const Parameters: String = ''; const Directory: String = ''; ShowCmd: Integer = SW_SHOWNORMAL): HINST;
begin
Result:=ShellExecute(0, NIL, PChar(FileName), PChar(Parameters), PChar(Directory), ShowCmd);
end;

{$IFDEF DELPHI_XE}
procedure Str2File(const S, AFileName: String);
var Stream: TStringStream;
begin
Stream:=TStringStream.Create(S, TEncoding.Default);
try
  Stream.SaveToFile(AFileName);
finally
  FreeAndNIL(Stream);
end; // try
end;

function File2Str(const AFileName: String): String;
var Stream: TStringStream;
begin
Stream:=TStringStream.Create('', TEncoding.Default);
try
  Stream.LoadFromFile(AFileName);
  Stream.Position:=0;
  Result:=Stream.ReadString(Stream.Size);
finally
  FreeAndNIL(Stream);
end; // try
end;
{$ELSE}
procedure Str2File(const S, AFileName: String);
var F: TFileStream;
begin
F:=TFileStream.Create(AFileName, fmCreate);
try
  F.Write(Pointer(S)^, Length(S));
finally
  F.Free;
end; // try
end;

function File2Str(const AFileName: String): String;
var F: TFileStream;
begin
F:=TFileStream.Create(AFileName, fmOpenRead);
try
  SetString(Result, NIL, F.Size);
  F.Read(Pointer(Result)^, F.Size);
finally
  F.Free;
end; // try
end;
{$ENDIF}

function f_GetFileVersion(const AFileName: String): String;
var VISize: Cardinal; VIBuff: Pointer; buffsize: Cardinal; Temp: Integer;
  FLanguageInfo: String; Str: PChar;
begin
Result:='';
if not FileExists(AFileName) then exit;
VISize:=GetFileVersionInfoSize(PChar(AFileName), buffsize);

if VISize<1 then exit;
VIBuff:=AllocMem(VISize);
GetFileVersionInfo(PChar(AFileName), Cardinal(0), VISize, VIBuff);

VerQueryValue(VIBuff, '\VarFileInfo\Translation', Pointer(Str), buffsize);
if buffsize<4 then exit;

Temp:=0;
StrLCopy(@Temp, Str, 2);
FLanguageInfo:=IntToHex(Temp, 4);
StrLCopy(@Temp, Str+2, 2);
FLanguageInfo:=FLanguageInfo+IntToHex(Temp, 4);

VerQueryValue(VIBuff, PChar('\StringFileInfo\'+FLanguageInfo+'\FileVersion'), Pointer(Str), buffsize);
if buffsize>0 then Result:=Str else Result:='';

FreeMem(VIBuff, VISize);
end;

end.
