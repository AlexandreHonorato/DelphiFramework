unit uStreamParser_CodeUnit;

interface

uses uStreamParser, uCodeBase, uCodeEntities, Classes, SysUtils, _Strings, uParseDirectives;

type TStreamParser_CodeUnit = class(TStreamParser)
  private
    FCodeBase: TCodeBase;
    procedure DoParseStream(AStream: TMemoryStream; AUnit: TCodeUnit);
  protected
    procedure ApplyParseResults(AResultUnit: TCodeUnit); virtual; abstract;
  public
    procedure ParseStream(const AFileName: String; AStream: TMemoryStream); override;
    constructor Create(ACodeBase: TCodeBase); reintroduce;
    property CodeBase: TCodeBase read FCodeBase;
  end;

implementation

uses uParseInfo, mwPasParser;

{ TStreamParser_CodeUnit }

procedure TStreamParser_CodeUnit.DoParseStream(AStream: TMemoryStream; AUnit:
    TCodeUnit);
var parser: TmPasParser; currToken: TTokenKind; codeObjectName, parentClassName: String;
  class_: TCodeClass; lineNo: Integer; interface_: TCodeInterface;
  classParseInfo: TParseInfo_Class; metaclass_: TCodeMetaclass; isSingleton: Boolean;
  unitParseInfo: TParseInfo_Unit; unitSection: TUnitSection; lstUses: TStrings;
  cmd, cmdParam: String;
begin
isSingleton:=False;
unitSection:=usItf;
unitParseInfo:=TParseInfo_Unit(AUnit.ParseInfo);
parser:=TmPasParser.Create;
try
  parser.Origin:=AStream.Memory;

  while True do
    begin
    currToken:=parser.Token.ID;

    case currToken of
    tkClass:
      begin
      codeObjectName:=parser.PrevIdentifier;
      lineNo:=parser.Token.LineNumber+1;
      parser.NextNonJunk;

      case parser.Token.ID of
      tkProcedure, tkFunction: ; // do nothing - declaration or implementation of class method;
      tkOf:
        begin
        parser.NextNonJunk;
        parser.ExpectTokens([tkIdentifier]);

        metaclass_:=TCodeMetaclass.Create(AUnit, codeObjectName, lineNo);
        AUnit.Metaclasses.Add(metaclass_);

        SetParseInfo(metaclass_, TParseInfo_Metaclass.Create(FCodeBase, parser.Token.Data));
        end;
      tkSemiColon: ; // do nothing - forward class declaration
      tkRoundOpen:
        begin
        parser.NextNonJunk;
        parser.ExpectTokens([tkIdentifier]);

        parentClassName:=parser.Token.Data;
        classParseInfo:=TParseInfo_Class.Create(FCodeBase, parentClassName);
        class_:=TCodeClass.Create(AUnit, codeObjectName, lineNo);
        AUnit.Classes.Add(class_);

        SetParseInfo(class_, classParseInfo);

        parser.NextNonJunk;
        while parser.Token.ID<>tkRoundClose do
          begin
          parser.ExpectTokens([tkComma]);

          parser.NextNonJunk;
          parser.ExpectTokens([tkIdentifier, tkRoundClose]);
          if parser.Token.ID=tkIdentifier then
            begin
            classParseInfo.ImplementedInterfaces.Add(parser.Token.Data);
            parser.NextNonJunk;
            end;
          end;
        end
      else
        begin
        class_:=TCodeClass.Create(AUnit, codeObjectName, lineNo);
        AUnit.Classes.Add(class_);

        SetParseInfo(class_, TParseInfo_Class.Create(FCodeBase, sTObject));
        end;
      end; // case
      end;
    tkImplementation:
      begin
      AUnit.ImplementationLine:=parser.Token.LineNumber+1;
      unitSection:=usImpl;
      end;
    tkPoint:
      if parser.PrevToken=tkEnd then break;
    tkInterface:
      if parser.PrevToken=tkEqual then
        begin
        codeObjectName:=parser.PrevIdentifier;
        lineNo:=parser.Token.LineNumber+1;
        parser.NextNonJunk;

        // для интерефейсов тоже бывают forward-определиния; пока просто
        // игнорируем объявления вида ITest = interface;
        if parser.Token.ID<>tkSemiColon then
	        begin
	        interface_:=TCodeInterface.Create(AUnit, codeObjectName, lineNo);
	        AUnit.Interfaces.Add(interface_);

	        SetParseInfo(interface_, TParseInfo_Interface.Create);
	        end;
        end;
    tkBorComment:
      begin
      if CodeBaseDirective(parser.Token.Data, cmd, cmdParam) then
	      begin
	      if cmd='SINGLETON' then
	        begin
	        isSingleton:=True
	        end          
        else
        if cmd='UNIT_LAYER' then
          begin
          AUnit.Layer:=FCodeBase.Layers.ByName(cmdParam).ID;
          end
        else
        if cmd='CAN_USE' then
          begin
          unitParseInfo.CanUse[unitSection].Add(TCodeUnit.FileNameToUnitID(cmdParam))
          end;
	      end;
      end;
    tkIdentifier:
      begin
      if isSingleton then
        begin
        AUnit.Singletons.Add(TCodeSingleton.Create(AUnit, parser.Token.Data, parser.Token.LineNumber+1));
        isSingleton:=False;
        end;
      end;
    tkUses:
      begin
      lstUses:=unitParseInfo.Uses_[unitSection];

      while True do
        begin
        parser.NextNonJunk;
        if parser.Token.ID=tkSemiColon then break;
        if parser.Token.ID=tkIdentifier then lstUses.Add(parser.Token.Data);
        end;
      end;
    end; // case

    // NextNonJunk, только не игнорирует tkBorComment
    repeat
      parser.NextToken;
    until not (parser.Token.ID in [tkAnsiComment, tkCRLF, tkCRLFCo, tkSlashesComment, tkSpace]);
    end;
finally
  FreeAndNIL(parser);
end; // try
end;

constructor TStreamParser_CodeUnit.Create(ACodeBase: TCodeBase);
begin
inherited Create;
FCodeBase:=ACodeBase;
end;

procedure TStreamParser_CodeUnit.ParseStream(const AFileName: String; AStream: TMemoryStream);
var ResultUnit: TCodeUnit;
begin
ResultUnit:=TCodeUnit.Create;
ResultUnit.FileName:=AFileName;
ResultUnit.UnitSource:=usProject;
SetParseInfo(ResultUnit, TParseInfo_Unit.Create(FCodeBase));

try
  DoParseStream(AStream, ResultUnit);

  ApplyParseResults(ResultUnit);
except
  FreeAndNIL(ResultUnit);
  raise;
end; // try
end;

end.
