unit uCallStack_Domain;

interface

uses _Lists, uCodeEntities, SysUtils, Classes, Windows;

type TcsErrorKind = (ekNone, ekWrongPushCount, erWrongPopCount,
  ekPushIsNotFirst, ekPopIsNotLast, ekEmptyErrorID, ekExitAfterPush);

type
  TcsCmdKind = (ckPush, ckSet, ckPop, ckExit);
  TcsCmdKinds = set of TcsCmdKind;

type
  TcsMethodList = class;
  TcsUnit = class;
  TcsMethod = class;

  TcsCommand = class
  public
    Method: TcsMethod;
    Kind: TcsCmdKind;
    ErrorID: String;
    Line: Integer;
    constructor Create(AMethod: TcsMethod; AKind: TcsCmdKind; const AErrorID: String; ALine: Integer); reintroduce;
  end;

  TcsCommandList = class(TOwningList)
  private
    function GetItems(Index: Integer): TcsCommand;
  public
    property Items[Index: Integer]: TcsCommand read GetItems; default;
  end;

  TcsMethod = class
  public
    Unit_: TcsUnit;
    OuterMethod: TcsMethod;
    StartPos: Integer;
    StartLine: Integer;
    EndPos: Integer;
    EndLine: Integer;
    ShortName: String;
    FullName: String;
    NestedMethods: TcsMethodList;
    StackCommands: TcsCommandList;
    ErrorKind: TcsErrorKind;
    constructor Create(AUnit: TcsUnit; AOuterMethod: TcsMethod); reintroduce; virtual;
    destructor Destroy; override;
    function Hash: String;
    function HasErrors(ARecursive: Boolean): Boolean;
  end;

  TcsMethodList = class(TOwningList)
  private
    function GetItems(Index: Integer): TcsMethod;
  public
    property Items[Index: Integer]: TcsMethod read GetItems; default;
  end;

  TcsUnit = class(TBaseCodeUnit)
  private
    FMethods: TcsMethodList;
  public
    HasErrors: Boolean;
    property Methods: TcsMethodList read FMethods;
    constructor Create(const AUnitFileName: String); reintroduce;
    destructor Destroy; override;
  end;

type TcsUnitList = class(TOwningList)
  private
    function GetItems(Index: Integer): TcsUnit;
  public
    property Items[Index: Integer]: TcsUnit read GetItems; default;
  end;

type TcsMethodsIndex = class(TStringList)
  private
    FFileName: String;
  public
    constructor Create(AUnit: TcsUnit); reintroduce; virtual;
    function ContainsMethod(AMethod: TcsMethod): Boolean;
    procedure AddMethod(AMethod: TcsMethod);
    procedure Save;
  end;

type CallStackRules = class
  private
    class function CheckMethod(AMethod: TcsMethod; AExcludeIndex: TcsMethodsIndex): TcsErrorKind;
    class procedure CheckMethodsRecursive(AMethods: TcsMethodList; out AHasErrors: Boolean; AExcludeIndex: TcsMethodsIndex);
  public
    class procedure CheckUnit(AUnit: TcsUnit);
  end;

type TcsErrorIdIndex = class(TStringList)
  private
    FDuplicateCommands: TcsCommandList;
  public
    property DuplicateCommands: TcsCommandList read FDuplicateCommands;
    procedure AddErrorID(ACommand: TcsCommand);
    function ByErrorID(const AErrorID: String): TcsCommand;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TcsCommand }

constructor TcsCommand.Create(AMethod: TcsMethod; AKind: TcsCmdKind; const AErrorID: String; ALine: Integer);
begin
inherited Create;
Method:=AMethod;
Kind:=AKind;
ErrorID:=AErrorID;
Line:=ALine;
end;

{ TcsCommandList }

function TcsCommandList.GetItems(Index: Integer): TcsCommand;
begin
Result:=TcsCommand(inherited Items[Index])
end;

{ TcsUnit }

constructor TcsUnit.Create(const AUnitFileName: String);
begin
inherited Create;
FMethods:=TcsMethodList.Create(True);
FileName:=AUnitFileName;
UnitSource:=usUnknown;
end;

destructor TcsUnit.Destroy;
begin
if Assigned(FMethods) then FreeAndNIL(FMethods);
inherited Destroy;
end;

{ TcsMethodList }

function TcsMethodList.GetItems(Index: Integer): TcsMethod;
begin
Result:=TcsMethod(inherited Items[Index])
end;

function TcsUnitList.GetItems(Index: Integer): TcsUnit;
begin
Result:=TcsUnit(inherited Items[Index])
end;

constructor TcsMethod.Create(AUnit: TcsUnit; AOuterMethod:
    TcsMethod);
begin
inherited Create;
Unit_:=AUnit;
OuterMethod:=AOuterMethod;
ErrorKind:=ekNone;
NestedMethods:=TcsMethodList.Create(True);
StackCommands:=TcsCommandList.Create(True);
end;

destructor TcsMethod.Destroy;
begin
if Assigned(StackCommands) then FreeAndNIL(StackCommands);
if Assigned(NestedMethods) then FreeAndNIL(NestedMethods);
inherited Destroy;
end;

{ CallStackRules }

class function CallStackRules.CheckMethod(AMethod: TcsMethod;
    AExcludeIndex: TcsMethodsIndex): TcsErrorKind;
var I: Integer; hasEmptyErrorID: Boolean; hasExit: Boolean;
  cmd: TcsCommand; pushCount, popCount: Integer;
begin
Result:=ekNone;

if Assigned(AExcludeIndex) and AExcludeIndex.ContainsMethod(AMethod) then
  begin
  AMethod.ErrorKind:=Result;
  exit;
  end;
  
hasEmptyErrorID:=False;
hasExit:=False;
pushCount:=0;
popCount:=0;
for i:=0 to AMethod.StackCommands.Count-1 do
  begin
  cmd:=AMethod.StackCommands[i];

  if (cmd.Kind in [ckPush, ckSet]) and (cmd.ErrorID='') then
    hasEmptyErrorID:=True;

  case cmd.Kind of
  ckExit:
    hasExit:=True;
  ckPush:
    begin
    Inc(pushCount);
    if cmd.ErrorID='' then hasEmptyErrorID:=True;
    end;
  ckSet:
    begin
    if cmd.ErrorID='' then hasEmptyErrorID:=True;
    end;
  ckPop:
    Inc(popCount);
  end; // case
  end;

if hasEmptyErrorID then
  Result:=ekEmptyErrorID
else
	begin
  if pushCount<>1 then
    Result:=ekWrongPushCount
  else
  if popCount<>1 then
    Result:=erWrongPopCount
  else
    begin
	  if AMethod.StackCommands[0].Kind<>ckPush then
	    Result:=ekPushIsNotFirst
	  else
	  if AMethod.StackCommands[AMethod.StackCommands.Count-1].Kind<>ckPop then
	    Result:=ekPopIsNotLast
	  end;
	end;

if Result=ekNone then
  if hasExit then // если до этого ошибки не было, то Push стоит первым, и exit, если он есть, находится после него
    Result:=ekExitAfterPush;

AMethod.ErrorKind:=Result;
end;

class procedure CallStackRules.CheckMethodsRecursive(AMethods:
    TcsMethodList; out AHasErrors: Boolean; AExcludeIndex:
    TcsMethodsIndex);
var I: Integer; method: TcsMethod;
begin
for i:=0 to AMethods.Count-1 do
  begin
  method:=AMethods[i];
  if CheckMethod(method, AExcludeIndex)<>ekNone then AHasErrors:=True;
  CheckMethodsRecursive(method.NestedMethods, AHasErrors, AExcludeIndex);
  end;
end;

function TcsMethod.Hash: String;
begin
Result:=AnsiUpperCase(ShortName);
if Assigned(OuterMethod) then
  Result:=OuterMethod.Hash+':'+Result
else
  Result:=AnsiUpperCase(Unit_.Caption(False))+':'+Result;
end;

function TcsMethod.HasErrors(ARecursive: Boolean): Boolean;
var I: Integer;
begin
Result:=ErrorKind<>ekNone;
if Result then exit;

if ARecursive then
  for i:=0 to NestedMethods.Count-1 do
    begin
    Result:=Result or NestedMethods[i].HasErrors(True);
    if Result then break;
    end;
end;

class procedure CallStackRules.CheckUnit(AUnit: TcsUnit);
var exclude: TcsMethodsIndex;
begin
AUnit.HasErrors:=False;
exclude:=TcsMethodsIndex.Create(AUnit);
try
  CheckMethodsRecursive(AUnit.FMethods, AUnit.HasErrors, exclude);
finally
  FreeAndNIL(exclude);
end; // try
end;

{ TcsMethodsIndex }

procedure TcsMethodsIndex.AddMethod(AMethod: TcsMethod);
begin
Add(AMethod.Hash);
end;

function TcsMethodsIndex.ContainsMethod(AMethod: TcsMethod): Boolean;
begin
Result:=IndexOf(AMethod.Hash)<>-1
end;

constructor TcsMethodsIndex.Create(AUnit: TcsUnit);
begin
inherited Create;
Duplicates:=dupIgnore;

FFileName:=ChangeFileExt(AUnit.FileName, '.cse');
if FileExists(FFileName) then
  LoadFromFile(FFileName);

Sorted:=True;
end;

procedure TcsMethodsIndex.Save;
begin
SaveToFile(FFileName);
end;

{ TcsErrorIdIndex }

procedure TcsErrorIdIndex.AddErrorID(ACommand: TcsCommand);
begin
try
  AddObject(AnsiUpperCase(ACommand.ErrorID), ACommand)
except
  if ACommand.ErrorID<>'' then
    FDuplicateCommands.Add(ACommand)
end; // try
end;

constructor TcsErrorIdIndex.Create;
begin
inherited Create;
Sorted:=True;
Duplicates:=dupError;

FDuplicateCommands:=TcsCommandList.Create(False);
end;

function TcsErrorIdIndex.ByErrorID(const AErrorID: String): TcsCommand;
var idx: Integer;
begin
idx:=IndexOf(AnsiUpperCase(AErrorID));
if idx=-1 then
  Result:=NIL
else
  Result:=TcsCommand(Objects[idx]);
end;

destructor TcsErrorIdIndex.Destroy;
begin
if Assigned(FDuplicateCommands) then FreeAndNIL(FDuplicateCommands);
inherited Destroy;
end;

end.
