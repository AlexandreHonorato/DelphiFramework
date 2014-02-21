unit _PTHFiles;

interface

uses SysUtils, Classes;

function PTHRead(const AFileName: String): String;
procedure PTHWrite(const AFileName, APath: String);

implementation

type TCharArray = array[0..255] of AnsiChar;

procedure EncodeString(var C: TCharArray; Cnt: Integer);
var I: Integer; B: Byte;
begin
for i:=0 to Cnt-1 do
  begin
  B:=not Byte(C[i]);
  C[i]:=AnsiChar(B);
  end;
end;

function PTHRead(const AFileName: String): String;
var F: TFileStream; C: TCharArray;
begin
F:=TFileStream.Create(AFileName, fmOpenRead);
try
  FillChar(C, SizeOf(C), 0);
  F.Read(C, F.Size);
  EncodeString(C, F.Size);
  Result:=String(C);
finally
  F.Free
end; // try
end;

procedure PTHWrite(const AFileName, APath: String);
var F: TFileStream; C: TCharArray;
begin
F:=TFileStream.Create(AFileName, fmCreate);
try
  FillChar(C, SizeOf(C), 0);
  StrPCopy(C, AnsiString(APath));
  EncodeString(C, Length(APath));
  F.Write(C, Length(APath));
finally
  F.Free
end; // try
end;

end.
