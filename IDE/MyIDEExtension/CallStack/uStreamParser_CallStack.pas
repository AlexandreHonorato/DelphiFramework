unit uStreamParser_CallStack;

interface

uses uStreamParser, Classes, mwPasParser, uCallStack_Domain, SysUtils, uParseDirectives;

type TStreamParser_CallStack = class(TStreamParser)
  private
    FCallStackNames: TStrings;
    function ReadMethod(AParser: TmPasParser; AUnit: TcsUnit; AOuterMethod: TcsMethod): TcsMethod;
    function ReadCallStackCmd(AMethod: TcsMethod; AParser: TmPasParser): TcsCommand;
  public
    CodeUnits: TcsUnitList;
    ErrorIdIndex: TcsErrorIdIndex;
    procedure ParseStream(const AFileName: String; AStream: TMemoryStream); override;
    constructor Create(ACodeUnits: TcsUnitList; AErrorIdIndex: TcsErrorIdIndex; ACallStackNames: TStrings); reintroduce;
  end;

implementation

{ TStreamParser_CallStack }

function TStreamParser_CallStack.ReadCallStackCmd(AMethod: TcsMethod; AParser: TmPasParser): TcsCommand;

  function GetErrorID(const AText: String): String;
  begin
  if Length(AText)<=3 then
    Result:=''
  else
    Result:=Copy(AText, 2, Length(AText)-2)
  end;

begin
Result:=NIL;

if FCallStackNames.IndexOf(AParser.Token.Data)<>-1 then
	begin
  AParser.NextNonJunk;
  if AParser.Token.ID=tkPoint then
		begin
    AParser.NextNonJunk;
		if AParser.Token.Data='Push' then
		  begin
		  AParser.NextNonJunk;
		  AParser.ExpectTokens([tkRoundOpen]);
		  AParser.NextNonJunk;
		  AParser.ExpectTokens([tkString]);

		  Result:=TcsCommand.Create(AMethod, ckPush, GetErrorID(AParser.Token.Data), AParser.Token.LineNumber+1);
		  end
		else
		if AParser.Token.Data='Set_' then
		  begin
		  AParser.NextNonJunk;
		  AParser.ExpectTokens([tkRoundOpen]);
		  AParser.NextNonJunk;
		  AParser.ExpectTokens([tkString]);

		  Result:=TcsCommand.Create(AMethod, ckSet, GetErrorID(AParser.Token.Data), AParser.Token.LineNumber+1);
		  end
		else
		if AParser.Token.Data='Pop' then
		  Result:=TcsCommand.Create(AMethod, ckPop, '', AParser.Token.LineNumber+1)
		end;
	end
else
if AParser.Token.Data='exit' then
  Result:=TcsCommand.Create(AMethod, ckExit, '', AParser.Token.LineNumber+1)
end;

function TStreamParser_CallStack.ReadMethod(AParser: TmPasParser; AUnit:
    TcsUnit; AOuterMethod: TcsMethod): TcsMethod;
var tokenStack: TTokenStack; currToken: TTokenKind; roundOpen: Integer;
  lvl, recordLvl: Integer; nestedMethod: TcsMethod; cmd: TcsCommand;
begin
Result:=NIL;
if (AParser.PrevToken=tkEqual) or (AParser.PrevToken=tkColon) then exit;

// 1. —ейчас мы находимс€ после ключевого слова "procedure"/"function"/"constructor"/"destructor"
// AParser.Token.ID=tkProcedure; AParser.PrevToken=tkClass (не об€зательно, но возможно, если это class procedure)
tokenStack:=TTokenStack.Create;
tokenStack._Push(tkNull);
tokenStack._Push(AParser.Token.ID);

Result:=TcsMethod.Create(AUnit, AOuterMethod);

Result.StartPos:=AParser.RunPos-Length(AParser.Token.Data);
if AParser.PrevToken=tkClass then
  Result.StartPos:=Result.StartPos-Length('class ');
Result.StartLine:=AParser.Token.LineNumber+1;

// 2. —троим строку "[class] procedure "
Result.FullName:=AParser.Token.Data+' ';
if AParser.PrevToken=tkClass then Result.FullName:='class '+Result.FullName;

// 3. ƒалее идет либо <им€ класса>.<название метода>, либо сразу <название метода>
AParser.NextNonJunk;
Result.FullName:=Result.FullName+AParser.Token.Data;
Result.ShortName:=AParser.Token.Data;

AParser.NextNonJunk;
if AParser.Token.ID=tkPoint then // <им€ класса>.<название метода>
  begin
  AParser.NextNonJunk;
  Result.FullName:=Result.FullName+'.';
  Result.ShortName:=Result.ShortName+'.'+AParser.Token.Data;
  end;

// 4. ѕропускаем список параметров (и возможно, возвращаемое значение, если функци€)
roundOpen:=0;
while (AParser.Token.ID<>tkNull) and (not ((AParser.Token.ID=tkSemiColon) and (roundOpen=0))) do
  begin
  if not (AParser.Token.ID in [tkAnsiComment, tkBorComment, tkSlashesComment]) then // Ignore comments
	  begin
    if AParser.Token.ID in [tkCRLF, tkCRLFCo, tkSpace] then
      Result.FullName:=Result.FullName+' '
    else
      Result.FullName:=Result.FullName+AParser.Token.Data;

	  case AParser.Token.ID of
	  tkRoundOpen: Inc(roundOpen);
	  tkRoundClose: Dec(roundOpen);
	  end; // case
	  end;
  AParser.NextToken;
  end; // while
Result.FullName:=Result.FullName+';';

// 5. ¬се, FullName построен. ћы находимс€ после ";",
// начинаетс€ собственно тело процедуры, которое нам тупо надо
// пропустить и найти, где оно кончаетс€
try
  lvl:=1;
  recordLvl:=0;
  while True do
    begin
    currToken:=AParser.Token.ID;
    case currToken of
    tkExternal:
      if tokenStack._Curr in [tkProcedure, tkFunction] then
        begin
        tokenStack._Pop;
        Dec(lvl);
        end;
    tkForward:
      begin
      FreeAndNIL(Result);
      Dec(lvl);
      tokenStack._Pop;
      end;
    tkCase, tkTry:
      begin
      tokenStack._Push(currToken);
      Inc(lvl);
      end;
    tkRecord:
      begin
      Inc(recordLvl);
      tokenStack._Push(currToken);
      Inc(lvl);
      end;
    tkBegin, tkAsm:
      begin
      if not (tokenStack._Curr in [tkProcedure, tkFunction, tkConstructor, tkDestructor]) then Inc(lvl);
      tokenStack._Push(currToken);
      end;
    tkProcedure, tkFunction:
      begin
      nestedMethod:=ReadMethod(AParser, AUnit, Result);
      if Assigned(nestedMethod) then Result.NestedMethods.Add(nestedMethod);
      end;
    tkIdentifier:
      begin
      cmd:=ReadCallStackCmd(Result, AParser);
      if Assigned(cmd) then
        begin
        Result.StackCommands.Add(cmd);
        if cmd.Kind in [ckPush, ckSet] then
          ErrorIdIndex.AddErrorID(cmd);
        end;
      end;
    tkEnd:
      if recordLvl<>0 then
        while True do
          begin
          Dec(lvl);
          if (tokenStack._Pop=tkRecord) then
            begin
            Dec(recordLvl);
            break;
            end;
          end
      else
      if tokenStack._Curr in [tkBegin, tkAsm] then
        begin
        Dec(lvl);
        tokenStack._Pop;
        if tokenStack._Curr in [tkProcedure, tkFunction, tkConstructor, tkDestructor] then tokenStack._Pop;
        end
      else
        begin
        Dec(lvl);
        tokenStack._Pop;
        end;
    end; // case

    AParser.NextNonJunk; // делаем перед проверкой if lvl=0, чтоб захватить еще замыкающую ";"
    if lvl=0 then break;
    end; // while
finally
  tokenStack.Free;
end; // try

if Assigned(Result) then
  begin
  Result.EndPos:=AParser.RunPos;
  Result.EndLine:=AParser.Token.LineNumber+1;
  end;
end;

procedure TStreamParser_CallStack.ParseStream(const AFileName: String;
  AStream: TMemoryStream);
var parser: TmPasParser; currToken: TTokenKind; codeMethod: TcsMethod;
  unit_: TcsUnit; cmd, cmdParam: String; skipUnit: Boolean;
begin
unit_:=TcsUnit.Create(AFileName);
try
  parser:=TmPasParser.Create;
  try
    parser.Origin:=AStream.Memory;

    // ищем секцию implementation
    skipUnit:=False;
    while True do
      begin
      currToken:=parser.Token.ID;
      case currToken of
      tkBorComment:
        if CodeBaseDirective(parser.Token.Data, cmd, cmdParam) then
          if cmd='DONT_ANALYZE_CODE_STACK' then
            begin
            skipUnit:=True;
            break;
            end;
      tkImplementation:
        break;
      tkPoint:
        if parser.PrevToken=tkEnd then break;
      end; // case

      { TODO : дублируетс€, нужно дл€ CodeBase сделать наследника TPasParser с таким методом }
      // NextNonJunk, только не игнорирует tkBorComment
      repeat
        parser.NextToken;
      until not (parser.Token.ID in [tkAnsiComment, tkCRLF, tkCRLFCo, tkSlashesComment, tkSpace]);
      end;

    if not skipUnit then
      while True do
        begin
        currToken:=parser.Token.ID;
        case currToken of
        tkDispinterface, tkInterface:
          begin
          while parser.Token.ID<>tkEnd do
            parser.NextNonJunk;
          end;
        tkClass:
          begin
          if parser.PrevToken=tkEqual then
            begin
            parser.NextNonJunk;
            if (parser.Token.ID<>tkOf) and (parser.Token.ID<>tkSemiColon) then
              while parser.Token.ID<>tkEnd do
                parser.NextNonJunk;
            end;
          end;
        tkProcedure, tkFunction, tkConstructor, tkDestructor:
          begin
          codeMethod:=ReadMethod(parser, unit_, NIL);
          if Assigned(codeMethod) then unit_.Methods.Add(codeMethod);
          end;
        tkPoint:
          if parser.PrevToken=tkEnd then break;
        end; // case

        parser.NextNonJunk;
        end; // while
  finally
    FreeAndNil(parser);
  end; // try

  CodeUnits.Add(unit_);
except
  FreeAndNIL(unit_);
  raise
end; // try
end;

constructor TStreamParser_CallStack.Create(ACodeUnits: TcsUnitList; AErrorIdIndex: TcsErrorIdIndex; ACallStackNames:
    TStrings);
begin
inherited Create;
CodeUnits:=ACodeUnits;
ErrorIdIndex:=AErrorIdIndex;
FCallStackNames:=ACallStackNames;
end;

end.
