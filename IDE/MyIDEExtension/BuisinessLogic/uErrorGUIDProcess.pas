unit uErrorGUIDProcess;

interface

uses Windows, SysUtils, Classes, mwPasParser, _Strings;

function EG_ProcessFile(const AFileName: String; AStartErrorID: Integer; AForce: Boolean): Integer;

function EG_GetMaxID(const AFileName: String): Integer;

implementation

uses Math;

type TErrorIDData = record
    Position: Longint;
    Length: Integer;
  end;
  PErrorIDData = ^TErrorIDData;

const
  ErrorGUIDDigitCount = 4;
  ErrorGUIDWidth = ErrorGUIDDigitCount + 3;

function EG_GetPositions(const AFileName: String; ALst: TList; AForce: Boolean): Integer;
var EditorStream: TMemoryStream; Parser: TmPasParser; CT, T1, T2: TTokenKind;
  P: PErrorIDData;
begin
EditorStream:=TMemoryStream.Create;
try
  EditorStream.LoadFromFile(AFileName);
  Result:=EditorStream.Size;
  Parser:=TmPasParser.Create;
  Parser.Origin:=EditorStream.Memory;
  try
    while True do
      begin
      CT:=Parser.Token.ID;
      case CT of
      tkIdentifier:
        begin
        if (Parser.Token.Data='cs') then
	        begin
          Parser.NextNonJunk;
          if Parser.Token.ID=tkPoint then
            begin
            Parser.NextNonJunk;
            if (Parser.Token.Data='Push') or (Parser.Token.Data='Set_') then
              begin
              Parser.SavePos;

              Parser.NextNonJunk;
              T1:=Parser.Token.ID; // must be tkRoundOpen

              Parser.NextNonJunk;
              T2:=Parser.Token.ID; // must be tkString

              if (T1=tkRoundOpen) and (T2=tkString) then
                begin
                if AForce or (Parser.Token.Data='''''') then
                  begin
                  New(P);
                  P^.Position:=Parser.Token.Position;
                  P^.Length:=Parser.Token.Length;
                  Result:=Result+ErrorGUIDWidth-P^.Length;
                  ALst.Add(P);
                  end;
                end
              else
                Parser.RestorePos;
              end;
            end;
	        end;
        end;
      tkPoint:
        if Parser.PrevToken=tkEnd then break;
      end; // case

      Parser.NextNonJunk;
      end; // while
  finally
    FreeAndNil(Parser);
  end; // try
finally
  FreeAndNil(EditorStream);
end; // try
end;

function EG_GetMaxID(const AFileName: String): Integer;
var EditorStream: TMemoryStream; Parser: TmPasParser; CT, T1, T2: TTokenKind;
  I: Integer;
begin
EditorStream:=TMemoryStream.Create;
try
  EditorStream.LoadFromFile(AFileName);
  Result:=0;
  Parser:=TmPasParser.Create;
  Parser.Origin:=EditorStream.Memory;
  try
    while True do
      begin
      CT:=Parser.Token.ID;
      case CT of
      tkIdentifier:
        begin
        if (Parser.Token.Data='cs') then
	        begin
          Parser.NextNonJunk;
          if Parser.Token.ID=tkPoint then
            begin
            Parser.NextNonJunk;
            if (Parser.Token.Data='Push') or (Parser.Token.Data='Set_') then
              begin
              Parser.SavePos;

              Parser.NextNonJunk;
              T1:=Parser.Token.ID; // must be tkRoundOpen

              Parser.NextNonJunk;
              T2:=Parser.Token.ID; // must be tkString

              if (T1=tkRoundOpen) and (T2=tkString) then
    	          begin
                I:=0;
                if Parser.Token.Data<>'' then
                  try
                    I:=StrToInt(copy(Parser.Token.Data, 3, ErrorGUIDDigitCount));
                  except
                    I:=0;
                  end; // try
                Result:=Max(Result, I);
    	          end
              else
                Parser.RestorePos;
              end;
            end;
          end;
        end;
      tkPoint:
        if Parser.PrevToken=tkEnd then break;
      end; // case

      Parser.NextNonJunk;
      end; // while
  finally
    FreeAndNil(Parser);
  end; // try
finally
  FreeAndNil(EditorStream);
end; // try
end;

procedure EG_ProcessPositions(const AFileName: String; ADestSize: Integer; AErrorIDs: TList; AStartErrorID: Integer);
var FSource, FDest: TMemoryStream; I, Len: Integer; Rec: PErrorIDData;
  ErrorID: array[0..ErrorGUIDWidth] of Char; Tmp, FN_Backup: String;
begin
if AErrorIDs.Count<>0 then
	begin
	FSource:=TMemoryStream.Create;
	try
	  FSource.LoadFromFile(AFileName);
	  FDest:=TMemoryStream.Create;
	  try
	    FDest.Size:=ADestSize;
	    FDest.Position:=0;
	    FSource.Position:=0;
	    for i:=0 to AErrorIDs.Count-1 do
	      begin
	      Tmp:='''d'+LeadingSymbols(IntToStr(AStartErrorID+i), ErrorGUIDDigitCount, '0')+''''+#0;
	      StrPCopy(ErrorID, Tmp);

	      Rec:=PErrorIDData(AErrorIDs[i]);
	      Len:=Rec^.Position-FSource.Position;
	      FDest.CopyFrom(FSource, Len);
	      FDest.Write(ErrorID, SizeOf(ErrorID)-1);

	      FSource.Position:=Rec^.Position+Rec^.Length;
	      end;

	    FDest.CopyFrom(FSource, FSource.Size-FSource.Position);

      FN_Backup:=AFileName+'~eg';
      DeleteFile(FN_Backup);
      RenameFile(AFileName, FN_Backup);
	    FDest.SaveToFile(AFileName);
	  finally
	    FDest.Free;
	  end; // try
	finally
	  FSource.Free;
	end; // try
	end;
end;

function EG_ProcessFile(const AFileName: String; AStartErrorID: Integer; AForce: Boolean): Integer;
var lstErrorIDsPositions: TList; I: Integer; P: PErrorIDData; NewStreamSize: Integer;
begin
lstErrorIDsPositions:=TList.Create;
try
  NewStreamSize:=EG_GetPositions(AFileName, lstErrorIDsPositions, AForce);
  Result:=lstErrorIDsPositions.Count;
  EG_ProcessPositions(AFileName, NewStreamSize, lstErrorIDsPositions, AStartErrorID);
finally
  for i:=0 to lstErrorIDsPositions.Count-1 do
	  begin
    P:=PErrorIDData(lstErrorIDsPositions[i]);
	  Dispose(P);
	  end;
  lstErrorIDsPositions.Free;
end; // try
end;

(* procedure ProcessFiles;
var Cnt, Idx: Integer; F: TextFile; S: String;
begin
AssignFile(F, AppFile('.txt'));
Idx:=1;
try
  Reset(F);
  while not EOF(F) do
    begin
    Readln(F, S);
    Cnt:=ProcessFile(S, Idx);
    Idx:=Idx+Cnt;
    end;
finally
  CloseFile(F);
end; // try
end;     *)

end.
