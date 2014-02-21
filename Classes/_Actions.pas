unit _Actions;

interface

uses
  ActnList, _Debug, SysUtils, Classes, _RegClassesLists,
  _AutoDestroyList, _ActionScope, _Commands, _Misc, Messages, Menus, Forms;

const ACTN_NO_ID = -1;

var AlwaysEnabled: TNotifyEvent = NIL;

type TAction_Base = class(TDebugAction)
  private
    FID: Integer;
    FInfo: Pointer;
    FScope: TActionScope;
    FExecuteHandler: TMethod;
    FUpdateHandler: TMethod;
    FExtraInfo: Pointer;
  protected
    function GetActionInfo(AActionID: Integer): Pointer; virtual;
    procedure GetActionProperties(AActionInfo: Pointer; out ACaption: String;
      out AImageIndex: Integer; out AShortCut: Integer); virtual;
    function GetActionExtraInfo(AActionID: Integer): Pointer; virtual;
  public
    property ID: Integer read FID write FID;
    constructor Create(AID: Integer; AScope: TActionScope; AOnExecute: TMethod; AOnUpdate: TMethod); reintroduce; virtual;
    function Execute: Boolean; override;

    property Info: Pointer read FInfo;
    property ExtraInfo: Pointer read FExtraInfo;

    function Update: Boolean; override;
    property Scope: TActionScope read FScope;
  end;

type TLiteActionList = class(TDebugList)
  private
    FOwnsActions: Boolean;
    function GetActions(Index: Integer): TAction_Base;
  public
    function ByID(AActionID: Integer): TAction_Base;
    property Actions[Index: Integer]: TAction_Base read GetActions; default;
    constructor Create(AOwnsActions: Boolean); reintroduce;
    procedure Clear; override;
    function HandleShortCut(AShortCut: Integer): Boolean; overload;
    function HandleShortCut(AMsg: TWMKey): Boolean; overload;
  end;

type TActionList_Base = class(TActionList)
  private
    function GetActions(Index: Integer): TAction_Base;
  public
    function ByID(AActionID: Integer): TAction_Base;
    property Actions[Index: Integer]: TAction_Base read GetActions; default;
  end;

var ActionImplementorsClasses: TRegClasses = NIL;

type TActionImplementor = class(TDebugObject)
  public
    function SupportsList(AID: Integer): Boolean; virtual;
    procedure CreateActions(AID: Integer; AScope: TActionScope; AResultList:
        TLiteActionList); virtual;

    function SupportsAction(AID: Integer): Boolean; virtual;
    function CreateAction(AID: Integer; AScope: TActionScope): TAction_Base; virtual;
  end;

type TActionImplementorClass = class of TActionImplementor;

type TCustomActionsRegistry = class(TDebugObject)
  private
    FResultActions: TLiteActionList;
    FActionImplementors: TDebugList;
    procedure InitActionImplementors;
    procedure DoneActionImplementors;

    function GetImplementorForAction(AID: Integer): TActionImplementor;
    function GetImplementorForList(AID: Integer): TActionImplementor;
  public
    function CreateAction(AID: Integer; AScope: TActionScope): TAction_Base;
    function CreateActions(AID: Integer; AScope: TActionScope): TLiteActionList; overload;
    procedure CreateActions(AID: Integer; AScope: TActionScope; AResults: TLiteActionList); overload;

    function FindOrCreateAction(AActionID: Integer; AActionList: TLiteActionList; AScope: TActionScope): TAction_Base;

    constructor Create; override;
    destructor Destroy; override;
  end;

type
  TActionsRegistryClass = class of TCustomActionsRegistry;

var ActionsRegistryClass: TActionsRegistryClass = TCustomActionsRegistry;

function CustomActionsRegistry: TCustomActionsRegistry;

type TActionInfo = record
    ActionID: Integer;
    Caption: String;
    ImageIndex: Integer;
    ShortCut: Integer;
  end;
  PActionInfo = ^TActionInfo;

type TAction_Framework = class(TAction_Base)
  private
    function GetInfo: PActionInfo;
  protected
    function GetActionInfo(AActionID: Integer): Pointer; override;
    procedure GetActionProperties(AActionInfo: Pointer; out ACaption: String;
      out AImageIndex, AShortCut: Integer); override;
  public
    property Info: PActionInfo read GetInfo;
  end;

procedure afwRegisterActionInfo(AActionInfo: PActionInfo);

implementation

var lstAFWActions: TDebugList = NIL;

var FCustomActionsRegistry: TCustomActionsRegistry;

function CustomActionsRegistry: TCustomActionsRegistry;
begin
if not Assigned(FCustomActionsRegistry) then
  begin
  FCustomActionsRegistry:=ActionsRegistryClass.Create;
  RegisterAutoDestroy(ADL_UI_Framework, FCustomActionsRegistry);
  end;
Result:=FCustomActionsRegistry;
end;

procedure afwRegisterActionInfo(AActionInfo: PActionInfo);
begin
if lstAFWActions=NIL then
  begin
  lstAFWActions:=TDebugList.Create;
  RegisterAutoDestroy(ADL_UI_Framework, lstAFWActions);
  end;

lstAFWActions.Add(AActionInfo);
end;

function GetActionInfo__(AActionID: Integer): PActionInfo;
var I: Integer; Tmp: PActionInfo;
begin
Result:=NIL;
for i:=0 to lstAFWActions.Count-1 do
  begin
  Tmp:=PActionInfo(lstAFWActions[i]);
  if Tmp^.ActionID=AActionID then
    begin
    Result:=Tmp;
    break
    end;
  end;

if Result=NIL then
  raise Exception.CreateFmt('Info for ActionID=%d not found', [AActionID]);
end;

function TAction_Framework.GetActionInfo(AActionID: Integer): Pointer;
begin
Result:=GetActionInfo__(AActionID);
end;

procedure TAction_Framework.GetActionProperties(AActionInfo: Pointer;
  out ACaption: String; out AImageIndex, AShortCut: Integer);
var actionInfo: PActionInfo;
begin
actionInfo:=PActionInfo(AActionInfo);
ACaption:=actionInfo^.Caption;
AImageIndex:=actionInfo^.ImageIndex;
AShortCut:=actionInfo^.ShortCut;
end;

function TActionImplementor.CreateAction(AID: Integer; AScope: TActionScope): TAction_Base;
begin
Result:=NIL;
end;

procedure TActionImplementor.CreateActions(AID: Integer; AScope: TActionScope;
    AResultList: TLiteActionList);
begin
//do nothing
end;

function TActionImplementor.SupportsAction(AID: Integer): Boolean;
begin
Result:=False;
end;

function TActionImplementor.SupportsList(AID: Integer): Boolean;
begin
Result:=False;
end;

{ TCustomActionsRegistry }

constructor TCustomActionsRegistry.Create;
begin
inherited Create;
FResultActions:=TLiteActionList.Create(False);
InitActionImplementors;
end;

destructor TCustomActionsRegistry.Destroy;
begin
DoneActionImplementors;
if Assigned(FResultActions) then FreeAndNIL(FResultActions);
inherited Destroy;
end;

procedure TCustomActionsRegistry.DoneActionImplementors;
var I: Integer;
begin
if Assigned(FActionImplementors) then
	begin
	for i:=0 to FActionImplementors.Count-1 do
	  TActionImplementor(FActionImplementors[i]).Free;

  FreeAndNIL(FActionImplementors);
	end;
end;

function TCustomActionsRegistry.CreateAction(AID: Integer; AScope: TActionScope): TAction_Base;
var Implementor: TActionImplementor;
begin
Implementor:=GetImplementorForAction(AID);
Result:=Implementor.CreateAction(AID, AScope);
end;

function TCustomActionsRegistry.GetImplementorForAction(AID: Integer): TActionImplementor;
var I: Integer; Implementor: TActionImplementor;
begin
Result:=NIL;
for i:=0 to FActionImplementors.Count-1 do
  begin
  Implementor:=TActionImplementor(FActionImplementors[i]);
  if Implementor.SupportsAction(AID) then
    begin
    Result:=Implementor;
    break;
    end;
  end;

if Result=NIL then
  raise Exception.CreateFmt('No implementor for Action %d', [AID]);
end;

function TCustomActionsRegistry.GetImplementorForList(AID: Integer): TActionImplementor;
var I: Integer; Implementor: TActionImplementor;
begin
Result:=NIL;
for i:=0 to FActionImplementors.Count-1 do
  begin
  Implementor:=TActionImplementor(FActionImplementors[i]);
  if Implementor.SupportsList(AID) then
    begin
    Result:=Implementor;
    break;
    end;
  end;

if Result=NIL then
  raise Exception.CreateFmt('No implementor for ActionList %d', [AID]);
end;

function TCustomActionsRegistry.CreateActions(AID: Integer; AScope:
    TActionScope): TLiteActionList;
begin
CreateActions(AID, AScope, FResultActions);
Result:=FResultActions;
end;

function TCustomActionsRegistry.FindOrCreateAction(AActionID: Integer; AActionList: TLiteActionList; AScope:
    TActionScope): TAction_Base;
begin
Result:=AActionList.ByID(AActionID);

if not Assigned(Result) then
  begin
  Result:=CreateAction(AActionID, AScope);
  AActionList.Add(Result)
  end;
end;

procedure TCustomActionsRegistry.InitActionImplementors;
var I: Integer; Cls: TActionImplementorClass; Impl: TActionImplementor;
begin
if ActionImplementorsClasses=NIL then
  raise Exception.Create('ActionImplementorsClasses variable is not assigned');

FActionImplementors:=TDebugList.Create;

for i:=0 to ActionImplementorsClasses.Count-1 do
  begin
  Cls:=TActionImplementorClass(ActionImplementorsClasses[i]);

  Impl:=Cls.Create;
  FActionImplementors.Add(Impl);
  end;
end;

const AI_NO_ID: TActionInfo = (ActionID: ACTN_NO_ID; Caption: ''; ImageIndex: -1);

function TLiteActionList.ByID(AActionID: Integer): TAction_Base;
var I: Integer; Tmp: TAction_Base;
begin
Result:=NIL;
for i:=0 to Count-1 do
  begin
  Tmp:=Actions[i];
  if Tmp.ID=AActionID then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

procedure TLiteActionList.Clear;
var I: Integer;
begin
if FOwnsActions then
  for i:=0 to Count-1 do
    Actions[i].Free;

inherited Clear;
end;

constructor TLiteActionList.Create(AOwnsActions: Boolean);
begin
inherited Create;
FOwnsActions:=AOwnsActions;
end;

function TLiteActionList.GetActions(Index: Integer): TAction_Base;
begin
Result:=TAction_Base(Items[Index])
end;

procedure AlwaysEnabledProc(Self: TObject; Sender: TObject);
begin
with TAction(Sender) do
  begin
  Visible:=True;
  Enabled:=True;
  end;
end;

function TActionList_Base.ByID(AActionID: Integer): TAction_Base;
var I: Integer; Tmp: TAction_Base;
begin
Result:=NIL;
for i:=0 to ActionCount-1 do
  begin
  Tmp:=Actions[i];
  if Tmp.ID=AActionID then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

function TActionList_Base.GetActions(Index: Integer): TAction_Base;
begin
Result:=TAction_Base(inherited Actions[Index])
end;

procedure TCustomActionsRegistry.CreateActions(AID: Integer;
  AScope: TActionScope; AResults: TLiteActionList);
var implementor: TActionImplementor;
begin
implementor:=GetImplementorForList(AID);

AResults.Clear;
implementor.CreateActions(AID, AScope, AResults);
end;

{ TAction_Base }

constructor TAction_Base.Create(AID: Integer; AScope: TActionScope; AOnExecute: TMethod; AOnUpdate: TMethod);
var _Caption: String; _ImageIndex, _ShortCut: Integer;
begin
inherited Create(NIL);
FID:=AID;
FScope:=AScope;
FExecuteHandler:=AOnExecute;
FUpdateHandler:=AOnUpdate;

FInfo:=GetActionInfo(FID);
FExtraInfo:=GetActionExtraInfo(FID);

_Caption:='';
_ShortCut:=0;
_ImageIndex:=-1;
GetActionProperties(FInfo, _Caption, _ImageIndex, _ShortCut);

Caption:=_Caption;
ImageIndex:=_ImageIndex;
if _ShortCut<>0 then ShortCut:=_ShortCut;
end;

function TAction_Base.Execute: Boolean;
type TActn_Execute_Hanlder = procedure(AAction: TAction_Base) of object;
begin
Result:=(not MethodIsNull(FExecuteHandler)) and Enabled;
if Result then
  TActn_Execute_Hanlder(FExecuteHandler)(Self);
end;

function TAction_Base.GetActionExtraInfo(AActionID: Integer): Pointer;
begin
Result:=NIL;
end;

function TAction_Base.GetActionInfo(AActionID: Integer): Pointer;
begin
Result:=NIL;
end;

procedure TAction_Base.GetActionProperties(AActionInfo: Pointer; out ACaption:
    String; out AImageIndex: Integer; out AShortCut: Integer);
begin
// do nothing
end;

function TAction_Base.Update: Boolean;
type TActn_Update_Hanlder = procedure(AAction: TAction_Base) of object;
begin
Result:=not MethodIsNull(FUpdateHandler);
if Result then
  TActn_Update_Hanlder(FUpdateHandler)(Self);
end;

function TAction_Framework.GetInfo: PActionInfo;
begin
Result:=PActionInfo(inherited Info)
end;

var M: TMethod;

function TLiteActionList.HandleShortCut(AShortCut: Integer): Boolean;
var i: Integer; action: TAction_Base;
begin
Result := False;

for i:=0 to Count-1 do
  begin
  action:=Actions[i];
  if action.ShortCut = AShortCut then
    begin
    action.Execute;
    Result := True;
    break;
    end;
  end;
end;

function TLiteActionList.HandleShortCut(AMsg: TWMKey): Boolean;
begin
Result := HandleShortCut(Menus.ShortCut(AMsg.CharCode, KeyDataToShiftState(AMsg.KeyData)))
end;

initialization
M.Data:=NIL;
M.Code:=@AlwaysEnabledProc;
AlwaysEnabled:=TNotifyEvent(M);

afwRegisterActionInfo(@AI_NO_ID);

end.
