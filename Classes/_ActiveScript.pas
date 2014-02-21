unit _ActiveScript;

interface

uses SysUtils, Classes, ActiveX, TLB_MSScriptControl_10, ComObj, _Misc, _Files;

function RunCode(const ACode: String; AInclude: TStrings; const AFunctionName: String; AParams: PSafeArray): OleVariant;

type TActiveScriptWrapper = class
  private
    FCode: String;
    FItf: IScriptControl;
    FInclude: TStrings;
    procedure SetInclude(const Value: TStrings);
    procedure ItfError(const AErrorMessage: String);
  public
    constructor Create;
    destructor Destroy; override;
    property Include: TStrings read FInclude write SetInclude;
    procedure Reset(ResetInclude: Boolean = True);
    procedure AddCode(const Code: String);
    procedure AddFile(const AFileName: String);
    procedure PrepareCode;
    function RunCode(const AFunctionName: String; AParams: PSafeArray): OleVariant;
    function Eval(const AExpression: String): OleVariant;
  end;

implementation

function RunCode(const ACode: String; AInclude: TStrings; const AFunctionName: String; AParams: PSafeArray): OleVariant;
var I: Integer; Script: IScriptControl;
begin
try
  Script:=MyCreateComObject(CLASS_ScriptControl) as IScriptControl;
  Script.Language:='VBScript';

  if AInclude<>NIL then
    for i:=0 to AInclude.Count-1 do
      Script.AddCode(File2Str(AInclude[i])+#13#10);

  Script.AddCode(ACode);

  try
    Result:=Script.Run(AFunctionName, AParams);
  except
    if (Script<>NIL) and (Script.Error<>NIL) then
      raise Exception.CreateFmt('Ошибка VBScript:'+#13#10+'Сообщение: %s'+#13#10+'Строка: %d', [Script.Error.Description, Script.Error.Line])
    else
      raise;
  end; // try
finally
  Script:=NIL;
end; // try
end;


{ TActiveScriptWrapper }

procedure TActiveScriptWrapper.AddCode(const Code: String);
begin
FCode:=FCode+Code+#13#10;
end;

procedure TActiveScriptWrapper.AddFile(const AFileName: String);
begin
AddCode(File2Str(AFileName));
end;

constructor TActiveScriptWrapper.Create;
begin
inherited Create;
FInclude:=TStringList.Create;
FItf:=NIL;
end;

destructor TActiveScriptWrapper.Destroy;
begin
FItf:=NIL;
FInclude.Free;
inherited Destroy;
end;

function TActiveScriptWrapper.Eval(const AExpression: String): OleVariant;
begin
if FItf=NIL then raise Exception.Create('MS Script Control is not initialized');

try
  Result:=FItf.Eval(AExpression)
except
  on E: Exception do ItfError(E.Message);
end; // try
end;

procedure TActiveScriptWrapper.ItfError(const AErrorMessage: String);
begin
if (FItf<>NIL) and (FItf.Error<>NIL) then
  raise Exception.CreateFmt('Ошибка VBScript:'+#13#10+'Сообщение: %s'+#13#10+'Строка: %d', [FItf.Error.Description, FItf.Error.Line])
else
  raise Exception.Create(AErrorMessage);
end;

procedure TActiveScriptWrapper.PrepareCode;
var I: Integer; 
begin
if FItf=NIL then
  begin
  FItf:=MyCreateComObject(CLASS_ScriptControl) as IScriptControl;
  FItf.Language:='VBScript';
  end;

FItf.Reset;

if FInclude<>NIL then
  for i:=0 to FInclude.Count-1 do
    FItf.AddCode(File2Str(FInclude[i])+#13#10);

FItf.AddCode(FCode);
end;

procedure TActiveScriptWrapper.Reset(ResetInclude: Boolean);
begin
FItf:=NIL;
FCode:='';
if ResetInclude then FInclude.Clear;
end;

function TActiveScriptWrapper.RunCode(const AFunctionName: String; AParams: PSafeArray): OleVariant;
begin
if FItf=NIL then raise Exception.Create('MS Script Control is not initialized');

try
  Result:=FItf.Run(AFunctionName, AParams);
except
  on E: Exception do ItfError(E.Message);
end; // try
end;

procedure TActiveScriptWrapper.SetInclude(const Value: TStrings);
begin
FInclude.Assign(Value);
end;

end.
