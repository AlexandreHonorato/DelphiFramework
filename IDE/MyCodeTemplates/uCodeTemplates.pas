unit uCodeTemplates;

interface

uses
  Classes, Sysutils, Windows, _Strings, _ActiveScript;

type TCompiledToken = class
  public
    SourceCodeLen: Integer;
    CompiledCode: String;
    SourcePos: Integer;
    CompiledPos: Integer;
  end;

type TCompiledTokens = class(TList)
  private
    FDelta: Integer;
    function GetTokens(Index: Integer): TCompiledToken;
  public
    procedure Clear; override;
    property Tokens[Index: Integer]: TCompiledToken read GetTokens; default;
    function AddToken(const ASourceCode, ACompiledCode: String; APosition: Integer): Integer;
  end;

type
  TCustomTemplateProcessor = class
  private
    FSourceCode: String;
    FCompiledLines: TStringList;
    FCompiledTokens: TCompiledTokens;
  protected
    function CompileToken(const AToken: String): String; virtual; abstract;
    function DoCompile(const ALine: String): String; virtual;
  public
    procedure Compile;
    property SourceCode: String read FSourceCode write FSourceCode;
    property CompiledLines: TStringList read FCompiledLines;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset(AFullReset: Boolean); virtual; // if AFullReset - clear SourceCode, vars, etc.
    property CompiledTokens: TCompiledTokens read FCompiledTokens;
  end;

type TTemplateVar = class
  private
    FName: String;
    FValue: String;
  public
    property Name: String read FName;
    property Value: String read FValue write FValue;
  end;

type TTemplateVars = class(TList)
  private
    function GetVars(Index: Integer): TTemplateVar;
    function GetByName(const AName: String): TTemplateVar;
  public
    property ByName[const AName: String]: TTemplateVar read GetByName;
    procedure Clear; override;
    property Vars[Index: Integer]: TTemplateVar read GetVars; default;
    function NewVar(const AName, AValue: String): TTemplateVar;
  end;

type
  TTemplateProcessor = class(TCustomTemplateProcessor)
  private
    FVars: TTemplateVars;
  protected
    function CompileToken(const AToken: String): String; override;
  public
    property Vars: TTemplateVars read FVars;
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset(AFullReset: Boolean); override;
  end;

type TScriptTemplateProcessor = class(TTemplateProcessor)
  private
    FScriptWrapper: TActiveScriptWrapper;
  protected
    function DoMacro(const AMacroName, AMacroParams: String): String;
    function CompileToken(const AToken: String): String; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ScriptWrapper: TActiveScriptWrapper read FScriptWrapper;
    procedure Reset(AFullReset: Boolean); override;
  end;

implementation

{ TCompileTemplate }

constructor TTemplateProcessor.Create;
begin
inherited Create;
FVars:=TTemplateVars.Create;
end;

destructor TTemplateProcessor.Destroy;
begin
FVars.Free;
inherited Destroy;
end;

function TCustomTemplateProcessor.DoCompile(const ALine: String): String;
var i, j: Integer; S: String; Token: TCompiledToken;
  SourcePos, CompiledPos: Integer;
begin
i:=1;
while True do
  begin
  if i>Length(ALine)-1 then break;

  if (ALine[i]='{') and (ALine[i+1]='{') then
    begin
    j:=i+2;
    while True do
      if (ALine[j]='}') and (ALine[j+1]='}') then
        begin
        S:=copy(ALine, i, j-i+2);
        break;
        end
      else
        Inc(j);

    CompiledTokens.AddToken(S, CompileToken(copy(S, 3, Length(S)-4)), i);
    i:=j+3;
    end;

  Inc(i)
  end;

if CompiledTokens.Count=0 then
  Result:=ALine
else
	begin
	SetString(Result, NIL, Length(ALine)+CompiledTokens.FDelta);
  SourcePos:=1;
  CompiledPos:=1;
	for i:=0 to FCompiledTokens.Count-1 do
	  begin
	  Token:=FCompiledTokens[i];
    if Token.CompiledPos<>CompiledPos then
      StrPCopy(@Result[CompiledPos], copy(ALine, SourcePos, Token.SourcePos-SourcePos));
    StrPCopy(@Result[Token.CompiledPos], Token.CompiledCode);

    SourcePos:=Token.SourcePos+Token.SourceCodeLen;
    CompiledPos:=Token.CompiledPos+Length(Token.CompiledCode);
	  end;
  if CompiledPos<Length(Result) then StrPCopy(@Result[CompiledPos], copy(ALine, SourcePos, Length(ALine)-SourcePos+1));

  while Pos(#1#13#10, Result)<>0 do
    Result:=StringReplace(Result, #1#13#10, '', [rfReplaceAll]);
	end;
end;

{ TCompileOut }

procedure TCustomTemplateProcessor.Compile;
begin
FCompiledLines.Text:=DoCompile(FSourceCode);
end;

procedure TTemplateProcessor.Reset(AFullReset: Boolean);
begin
inherited Reset(AFullReset);
if AFullReset then FVars.Clear;
end;

function TTemplateProcessor.CompileToken(const AToken: String): String;
begin
Result:=FVars.ByName[AToken].Value;
end;

{ TTemplateVars }

procedure TTemplateVars.Clear;
var I: Integer;
begin
for i:=0 to Count-1 do Vars[i].Free;
inherited Clear;
end;

function TTemplateVars.GetByName(const AName: String): TTemplateVar;
var I: Integer; Tmp: TTemplateVar;
begin
Result:=NIL;
for i:=0 to Count-1 do
  begin
  Tmp:=Vars[i];
  if Tmp.Name=AName then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

function TTemplateVars.GetVars(Index: Integer): TTemplateVar;
begin
Result:=TTemplateVar(Items[Index]);
end;

function TTemplateVars.NewVar(const AName, AValue: String): TTemplateVar;
begin
Result:=TTemplateVar.Create;
Result.FName:=AName;
Result.Value:=AValue;
Add(Result);
end;

constructor TCustomTemplateProcessor.Create;
begin
inherited Create;
FCompiledLines:=TStringList.Create;
FCompiledTokens:=TCompiledTokens.Create;
end;

destructor TCustomTemplateProcessor.Destroy;
begin
FCompiledTokens.Free;
FCompiledLines.Free;
inherited Destroy;
end;

procedure TCustomTemplateProcessor.Reset(AFullReset: Boolean);
begin
FCompiledLines.Clear;
FCompiledTokens.Clear;
if AFullReset then FSourceCode:='';
end;

function TCompiledTokens.AddToken(const ASourceCode, ACompiledCode: String; APosition: Integer): Integer;
var Token: TCompiledToken;
begin
Token:=TCompiledToken.Create;
Token.SourceCodeLen:=Length(ASourceCode);
Token.CompiledCode:=ACompiledCode;
Token.SourcePos:=APosition;
Token.CompiledPos:=APosition+FDelta;
Result:=Add(Token);
FDelta:=FDelta+Length(ACompiledCode)-Length(ASourceCode);
end;

procedure TCompiledTokens.Clear;
var I: Integer;
begin
FDelta:=0;
for i:=0 to Count-1 do Tokens[i].Free;
inherited Clear;
end;

function TCompiledTokens.GetTokens(Index: Integer): TCompiledToken;
begin
Result:=TCompiledToken(Items[Index])
end;

constructor TScriptTemplateProcessor.Create;
begin
inherited Create;
FScriptWrapper:=TActiveScriptWrapper.Create;
end;

destructor TScriptTemplateProcessor.Destroy;
begin
FScriptWrapper.Free;
inherited Destroy;
end;

function TScriptTemplateProcessor.DoMacro(const AMacroName, AMacroParams: String): String;
var S: String;
begin
S:=AMacroName;
if AMacroParams<>'' then S:=S+'('+AMacroParams+')';
Result:=FScriptWrapper.Eval(S);
end;

procedure TScriptTemplateProcessor.Reset(AFullReset: Boolean);
begin
inherited Reset(AFullReset);
FScriptWrapper.Reset(AFullReset);
end;

function TScriptTemplateProcessor.CompileToken(const AToken: String): String;
var V: TTemplateVar; P: Integer;
begin
V:=Vars.ByName[AToken];
if V<>NIL then
  Result:=V.Value
else
  begin
  P:=pos(' ', AToken);
  if P=0 then
    Result:=DoMacro(AToken, '')
  else
    Result:=DoMacro(copy(AToken, 1, P-1), StrTail(AToken, P))
  end;
end;

end.
