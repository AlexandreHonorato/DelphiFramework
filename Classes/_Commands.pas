unit _Commands;

interface

uses SysUtils, _Debug, _MessageBus, _DomainEvents, _RegClassesLists,
  _AutoDestroyList, Windows, Classes, _Misc;

type
  TCommand = class;

  TSupportsMethod = TMethod;
  TExecuteMethod = TMethod;
  TConvertParamsMethod = TMethod;

  TCommand = class(TDebugObject)
  private
    FID: Integer;
    FSupportsHandler: TSupportsMethod;
    FExecuteHandler: TExecuteMethod;
    FConvertParamsMethod: TConvertParamsMethod;
  public
    constructor Create(AID: Integer; AExecuteHandler: TExecuteMethod;
      ASupportsHandler: TSupportsMethod); reintroduce; overload;
    constructor Create(AID: Integer; AExecuteHandler: TExecuteMethod;
      ASupportsHandler: TSupportsMethod;
      AConvertParamsMethod: TConvertParamsMethod); reintroduce; overload;
    property ID: Integer read FID;
    procedure Execute(AEvent: DomainEvent);
    function Supports(AContext: Pointer): Boolean;
    function ConvertParams(AEvent: DomainEvent): Pointer;
  end;

type
  TCustomCommandsRegistry = class(TDebugObject)
  private
    FCommandImplementors: TDebugList;
    FCommands: TDebugList;
    function GetByID(AID: Integer): TCommand;

    procedure InitCommandsImplementors;
    procedure DoneCommandsImplementors;
  public
    property ByID[AID: Integer]: TCommand read GetByID;
    procedure RegisterCommand(ACommand: TCommand);

    constructor Create; override;
    destructor Destroy; override;
  end;

type TCommandsRegistryClass = class of TCustomCommandsRegistry;

var CommandsRegistryClass: TCommandsRegistryClass = TCustomCommandsRegistry;

function CustomCommandsRegistry: TCustomCommandsRegistry;

var CommandImplementorsClasses: TRegClasses = NIL;

type TCommandImplementor = class(TDebugObject)
  public
    constructor Create(ACommandsRegistry: TCustomCommandsRegistry); reintroduce; virtual;
  end;

type TCommandImplementorClass = class of TCommandImplementor;

implementation

{ TCommand }

var FCommandsRegistry: TCustomCommandsRegistry;

function CustomCommandsRegistry: TCustomCommandsRegistry;
begin
if not Assigned(FCommandsRegistry) then
  begin
  FCommandsRegistry:=CommandsRegistryClass.Create;
  RegisterAutoDestroy(ADL_Domain_Framework, FCommandsRegistry);
  end;
Result:=FCommandsRegistry;
end;

{ TCustomCommandsRegistry }

constructor TCustomCommandsRegistry.Create;
begin
inherited Create;
FCommands:=TDebugList.Create;
InitCommandsImplementors;
end;

destructor TCustomCommandsRegistry.Destroy;
var I: Integer;
begin
if Assigned(FCommands) then
	begin
  for i:=0 to FCommands.Count-1 do TCommand(FCommands[i]).Free;
	FreeAndNIL(FCommands);
	end;

DoneCommandsImplementors;
inherited Destroy;
end;

procedure TCustomCommandsRegistry.DoneCommandsImplementors;
var I: Integer;
begin
if Assigned(FCommandImplementors) then
	begin
	for i:=0 to FCommandImplementors.Count-1 do
	  TCommandImplementor(FCommandImplementors[i]).Free;

  FreeAndNIL(FCommandImplementors);
	end;
end;

function TCustomCommandsRegistry.GetByID(AID: Integer): TCommand;
var I: Integer; Tmp: TCommand;
begin
Result:=NIL;
for i:=0 to FCommands.Count-1 do
  begin
  Tmp:=TCommand(FCommands[i]);
  if Tmp.ID=AID then
    begin
    Result:=Tmp;
    break
    end;
  end;

if Result=NIL then
  raise Exception.CreateFmt('Command %d is not registred', [AID]);
end;

procedure TCustomCommandsRegistry.InitCommandsImplementors;
var I: Integer; Cls: TCommandImplementorClass; Impl: TCommandImplementor;
begin
if CommandImplementorsClasses=NIL then
  raise Exception.Create('CommandImplementorsClasses variable is not assigned');

FCommandImplementors:=TDebugList.Create;

for i:=0 to CommandImplementorsClasses.Count-1 do
  begin
  Cls:=TCommandImplementorClass(CommandImplementorsClasses[i]);

  Impl:=Cls.Create(Self);
  FCommandImplementors.Add(Impl);
  end;
end;

procedure TCustomCommandsRegistry.RegisterCommand(ACommand: TCommand);
begin
FCommands.Add(ACommand);
end;

{ TCommand }

constructor TCommand.Create(AID: Integer; AExecuteHandler: TExecuteMethod;
  ASupportsHandler: TSupportsMethod);
begin
inherited Create;
FID:=AID;
FExecuteHandler:=AExecuteHandler;
FSupportsHandler:=ASupportsHandler;
end;

constructor TCommand.Create(AID: Integer;
  AExecuteHandler: TExecuteMethod; ASupportsHandler: TSupportsMethod;
  AConvertParamsMethod: TConvertParamsMethod);
begin
Create(AID, AExecuteHandler, ASupportsHandler);
FConvertParamsMethod:=AConvertParamsMethod;
end;

procedure TCommand.Execute(AEvent: DomainEvent);
type TCmd_Execute_Hanlder = procedure(ACommand: TCommand; AEvent: DomainEvent) of object;
begin
if MethodIsNull(FExecuteHandler) then
  raise Exception.Create('Execute handler is not assinged')
else
  TCmd_Execute_Hanlder(FExecuteHandler)(Self, AEvent);
end;

function TCommand.Supports(AContext: Pointer): Boolean;
type TCmd_Supports_Handler = function(ACommand: TCommand; AContext: Pointer): Boolean of object;
begin
if MethodIsNull(FSupportsHandler) then
  raise Exception.Create('Supports handler is not assinged')
else
  Result:=TCmd_Supports_Handler(FSupportsHandler)(Self, AContext);
end;

function TCommand.ConvertParams(AEvent: DomainEvent): Pointer;
type TCmd_ConvertParams_Handler = function(AEvent: DomainEvent): Pointer of object;
begin
if MethodIsNull(FConvertParamsMethod) then
  raise Exception.Create('Supports handler is not assinged')
else
  Result:=TCmd_ConvertParams_Handler(FConvertParamsMethod)(AEvent);
end;

{ TCommandImplementor }

constructor TCommandImplementor.Create(ACommandsRegistry: TCustomCommandsRegistry);
begin
inherited Create;
end;

end.
