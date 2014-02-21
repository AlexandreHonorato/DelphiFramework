unit _MVC_Controller;

interface

uses _ActionScope, ActnList, Controls, _MVC_Model, _Debug, SysUtils,
  _Actions, _MessageBus, _NonCOMInterface, Classes, _Misc;

type TMVC_Controller = class(TNonCOMInterface)
  private
    FScope: TActionScope;
    FActionList: TLiteActionList;
    FView: TControl;
    FModel: TMVC_Model;
    procedure SetModel(Value: TMVC_Model);
    procedure SetView(const Value: TControl);
    procedure On_NEModelChanged(AEvent: NEModelChanged);
  protected
    function DoCreateView: TControl; virtual; deprecated;
    function DoCreateViewNew(AOwner: TComponent; AParent: TWinControl): TControl; virtual;
    function DoCreateActionScope: TActionScope; virtual;
    procedure DoDestroyActionScope; virtual;
    procedure DoInitActions; virtual;
    procedure DoDoneActions; virtual;
    procedure DoConnectModel; virtual;
    procedure DoDisconnectModel; virtual;
    procedure DoConnectView; virtual;
    procedure DoDisconnectView; virtual;
    procedure DoStartListeningModel; virtual;
    procedure DoStopListeningModel; virtual;
    procedure DoStartListeningView; virtual;
    procedure DoStopListeningView; virtual;
    procedure DoStartListeningDomain; virtual;
    procedure DoStopListeningDomain; virtual;
    procedure OnModelChanged(AChanges: TByteSet); virtual;
    function CreateActionList: Boolean; virtual;
  public
    class function OwnsModel: Boolean; virtual;
    class function OwnsView: Boolean; virtual;
    procedure CreateView; deprecated;
    function CreateViewNew(AOwner: TComponent; AParent: TWinControl): TControl; overload;
    property Scope: TActionScope read FScope;
    property ActionList: TLiteActionList read FActionList;
    property View: TControl read FView write SetView;
    property Model: TMVC_Model read FModel write SetModel;
    constructor Create; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TMVC_Controller }

procedure TMVC_Controller.AfterConstruction;
begin
inherited AfterConstruction;
DoStartListeningDomain;
end;

procedure TMVC_Controller.BeforeDestruction;
begin
DoStopListeningDomain;
inherited BeforeDestruction;
end;

constructor TMVC_Controller.Create;
begin
inherited Create;

if CreateActionList then
  FActionList:=TLiteActionList.Create(True);
end;

function TMVC_Controller.CreateActionList: Boolean;
begin
Result:=False;
end;

procedure TMVC_Controller.CreateView;
begin
if Assigned(FView) then
  begin
  DoStopListeningView;

  if Assigned(FActionList) then
    DoDoneActions;

  if Assigned(FScope) then
	  FreeAndNIL(FScope);

  DoDisconnectView;

  if OwnsView then
    FreeAndNIL(FView);
  end;

FView:=DoCreateView;

if Assigned(FView) then
  begin
  DoConnectView;

  FScope:=DoCreateActionScope;

  if Assigned(FActionList) then
    DoInitActions;

  DoStartListeningView
  end;
end;

function TMVC_Controller.CreateViewNew(AOwner: TComponent; AParent: TWinControl):
    TControl;
begin
if Assigned(FView) then
  begin
  DoStopListeningView;

  if Assigned(FActionList) then
    DoDoneActions;

  if Assigned(FScope) then
	  FreeAndNIL(FScope);

  DoDisconnectView;

  if OwnsView then
    FreeAndNIL(FView);
  end;

FView:=DoCreateViewNew(AOwner, AParent);
Result:=FView;

if Assigned(FView) then
  begin
  DoConnectView;

  FScope:=DoCreateActionScope;

  if Assigned(FActionList) then
    DoInitActions;

  DoStartListeningView;
  end;
end;

destructor TMVC_Controller.Destroy;
begin
Model:=NIL;
View:=NIL;

if Assigned(FActionList) then FreeAndNIL(FActionList);

inherited Destroy;
end;

procedure TMVC_Controller.DoConnectModel;
begin
//do nothing
end;

procedure TMVC_Controller.DoConnectView;
begin
//do nothing
end;

function TMVC_Controller.DoCreateActionScope: TActionScope;
begin
Result:=NIL;
end;

function TMVC_Controller.DoCreateView: TControl;
begin
Result:=NIL
end;

function TMVC_Controller.DoCreateViewNew(AOwner: TComponent; AParent:
    TWinControl): TControl;
begin
Result:=NIL
end;

procedure TMVC_Controller.DoDestroyActionScope;
begin
FreeAndNIL(FScope);
end;

procedure TMVC_Controller.DoDisconnectModel;
begin
//do nothing
end;

procedure TMVC_Controller.DoDisconnectView;
begin
//do nothing
end;

procedure TMVC_Controller.DoDoneActions;
begin
//do nothing
end;

procedure TMVC_Controller.DoInitActions;
begin
//do nothing
end;

procedure TMVC_Controller.DoStartListeningDomain;
begin
//do nothing
end;

procedure TMVC_Controller.DoStartListeningModel;
begin
FModel.MessageBus.SignObject(Self,
  [NEModelChanged],
  [@TMVC_Controller.On_NEModelChanged]);
end;

procedure TMVC_Controller.DoStartListeningView;
begin
// do nothing
end;

procedure TMVC_Controller.DoStopListeningDomain;
begin
//do nothing
end;

procedure TMVC_Controller.DoStopListeningModel;
begin
FModel.MessageBus.UnsignObject(Self);
end;

procedure TMVC_Controller.DoStopListeningView;
begin
// do nothing
end;

procedure TMVC_Controller.OnModelChanged(AChanges: TByteSet);
begin
// do nothing
end;

procedure TMVC_Controller.On_NEModelChanged(AEvent: NEModelChanged);
begin
OnModelChanged(AEvent.Changes);
end;

class function TMVC_Controller.OwnsModel: Boolean;
begin
Result:=True;
end;

class function TMVC_Controller.OwnsView: Boolean;
begin
Result:=False
end;

procedure TMVC_Controller.SetModel(Value: TMVC_Model);
begin
if Assigned(FModel) then
  begin
  FModel.StopListening;

  DoStopListeningModel;

  DoDisconnectModel;

  if OwnsModel then
	  FreeAndNIL(FModel);
  end;

FModel:=Value;

if Assigned(FModel) then
  begin
  DoConnectModel;

  DoStartListeningModel;

  FModel.StartListening;
  end;
end;

procedure TMVC_Controller.SetView(const Value: TControl);
begin
if Assigned(FView) then
  begin
  DoStopListeningView;

  if Assigned(FActionList) then
    DoDoneActions;

  if Assigned(FScope) then
	  DoDestroyActionScope;

  DoDisconnectView;

  if OwnsView then
    FreeAndNIL(FView);
  end;

FView:=Value;

if Assigned(FView) then
  begin
  DoConnectView;

  FScope:=DoCreateActionScope;

  if Assigned(FActionList) then
    DoInitActions;

  DoStartListeningView
  end;
end;

end.
