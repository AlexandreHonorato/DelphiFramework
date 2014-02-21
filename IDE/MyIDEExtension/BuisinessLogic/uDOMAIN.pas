unit uDOMAIN;

interface

uses uCodeEntities, SysUtils, uUnits, ToolsAPI,
  _MessageBus, uFileMappers_IDE, _DomainEvents, uCodeBase, uSourceParser_CodeBase;

type DECodeBaseError = class(DomainEvent)
  public
    ex: Exception;
  end;

type TDOMAIN = class
  private
    FCodeBase: TCodeBase;
    FParseEngine: TSourceParser_CodeBase;
    FFrameWork: TFrameWork;
    FLastUsedUnits: TLastUsedUnits;

    procedure On_DECodeBaseError(AEvent: DECodeBaseError);
  public
    procedure OnParseError(AException: Exception);
    function CodeBase(ARebuild: Boolean): TCodeBase;
    property LastUsedUnits: TLastUsedUnits read FLastUsedUnits;
    property FrameWork: TFrameWork read FFrameWork;
    constructor Create;
    destructor Destroy; override;
  end;

{.SINGLETON}
var DOMAIN: TDOMAIN;

{.SINGLETON}
var mbExtension: IMessageBus;

implementation

{ TDOMAIN }

constructor TDOMAIN.Create;
begin
inherited Create;
FFrameWork:=TFrameWork.Create;
FLastUsedUnits:=TLastUsedUnits.Create;

mbExtension.SignObject(Self,
  [DECodeBaseError],
  [@TDOMAIN.On_DECodeBaseError]);
end;

destructor TDOMAIN.Destroy;
begin
mbExtension.UnsignObject(Self);

if Assigned(FLastUsedUnits) then FreeAndNIL(FLastUsedUnits);
if Assigned(FParseEngine) then FreeAndNIL(FParseEngine);
if Assigned(FCodeBase) then FreeAndNIL(FCodeBase);
if Assigned(FFrameWork) then FreeAndNIL(FFrameWork);
inherited Destroy;
end;

function TDOMAIN.CodeBase(ARebuild: Boolean): TCodeBase;
var needRebuild: Boolean;
begin
needRebuild:=ARebuild;

if not Assigned(FCodeBase) then
  begin
  FCodeBase:=TCodeBase.Create;

  FParseEngine:=TSourceParser_CodeBase.Create(FileMappers_IDE.Build, GetUnitNames_IDE(), FCodeBase);
  FParseEngine.OnParseError:=OnParseError;

  needRebuild:=True;
  end;

if needRebuild then
  FParseEngine.FullRebuild;

Result:=FCodeBase;
end;

procedure TDOMAIN.On_DECodeBaseError(AEvent: DECodeBaseError);
var svc: IOTAMessageServices;
begin
svc:=(BorlandIDEServices as IOTAMessageServices); { TODO : это событие нужно ловить не здесь, а наверное в UI }
svc.ShowMessageView(NIL);
svc.AddTitleMessage('CodeBase parse error: '+AEvent.ex.Message);
end;

procedure TDOMAIN.OnParseError(AException: Exception);
var evnt: DECodeBaseError;
begin
evnt:=DECodeBaseError.Create;
try
  evnt.ex:=AException;
  mbExtension.Notify(evnt);
finally
  FreeAndNIL(evnt);
end; // try
end;

end.
