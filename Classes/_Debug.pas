unit _Debug;

interface

uses SysUtils, Windows, Messages, Classes, Forms, ActnList, _IntList, Graphics, _SortList;

var DebugWnd: hWnd;

procedure ODS(S: String);
procedure ODSClear;

type TDebugLogObjectProc = procedure(AObject: TObject; const AAction: String);

var DebugLogObject: TDebugLogObjectProc;

type TDebugObject = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type TDebugComponent = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type TDebugAction = class(TAction)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type TDebugActionList = class(TActionList)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type TDebugList = class(TList)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type TDebugSortList = class(TSortList)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type TDebugStringList = class(TStringList)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type TDebugIntList = class(TIntList)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

type TDebugBitmap = class(TBitmap)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type TDebugForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
  end;

type TDebugDataModule = class(TDataModule)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
  end;

var lstDebugObjects: TList;

function CheckDebugObjects(var S: String): Boolean;

implementation

uses _RegClassesLists;

function CheckDebugObjects(var S: String): Boolean;

  function SkipObject(AObject: TObject): Boolean;
  begin
  Result:=(AObject is TRegClasses) or (AObject is TRegClassesStr) or (AObject is TRegClassesStr2)
  end;

var I: Integer; O: TObject;
begin
Result:=True;
if lstDebugObjects.Count<>0 then
  begin
  for i:=0 to lstDebugObjects.Count-1 do
    begin
    O:=TObject(lstDebugObjects[i]);
    if not SkipObject(O) then
      begin
      Result:=False;
      break;
      end;
    end;

  if not Result then
    begin
    S:='';
    for i:=0 to lstDebugObjects.Count-1 do
      begin
      O:=TObject(lstDebugObjects[i]);
      if not SkipObject(O) then S:=S+#13#10+O.ClassName;
      end;
    end;
  end;
end;

procedure DefaultDebugLogObject(AObject: TObject; const AAction: String);
begin
ODS(AAction+' '+AObject.ClassName+' '+IntToStr(Integer(AObject)));
if AAction='Create' then
  lstDebugObjects.Add(AObject)
else
if AAction='Destroy' then
  lstDebugObjects.Remove(AObject);
end;

procedure ODSClear;
begin
SendMessage(DebugWnd, WM_USER+1, 0, 0);
end;

procedure ODS(S: String);
begin
SendMessage(DebugWnd, LB_ADDSTRING, 0, Integer(PChar(S+#0)));
end;

{ TDebugObject }

constructor TDebugObject.Create;
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create;
end;

destructor TDebugObject.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugList }

constructor TDebugList.Create;
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create;
end;

destructor TDebugList.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugStringList }

constructor TDebugStringList.Create;
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create;
end;

destructor TDebugStringList.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugForm }

constructor TDebugForm.Create(AOwner: TComponent);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create(AOwner);
end;

constructor TDebugForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'CreateNew');
inherited CreateNew(AOwner, Dummy);
end;

destructor TDebugForm.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugDataModule }

constructor TDebugDataModule.Create(AOwner: TComponent);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create(AOwner);
end;

constructor TDebugDataModule.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'CreateNew');
inherited CreateNew(AOwner, Dummy);
end;

destructor TDebugDataModule.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugComponent }

constructor TDebugComponent.Create(AOwner: TComponent);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create(AOwner);
end;

destructor TDebugComponent.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugAction }

constructor TDebugAction.Create(AOwner: TComponent);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create(AOwner);
end;

destructor TDebugAction.Destroy;
begin
try
  // TAction.Destroy может выкидывать исключени€, если не инициализированы пол€
  // FImage, FMask и другие. “ака€ ситуаци€ возможна, если в конструкторе мы
  // вызываем GetInterfaceOrRaise, и она выбрасывает исключение. ѕосле этого
  // мы попадаем в унаследованный деструктор, он тоже выбросит исключение, и,
  // если его не задавить, то мы потер€ем сообщение от GetInterfaceOrRaise
  inherited Destroy;
except
  // do nothing
end; // try
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugIntList }

constructor TDebugIntList.Create;
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create;
end;

destructor TDebugIntList.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugBitmap }

constructor TDebugBitmap.Create;
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create;
end;

destructor TDebugBitmap.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugSortList }

constructor TDebugSortList.Create;
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create;
end;

destructor TDebugSortList.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

{ TDebugActionList }

constructor TDebugActionList.Create(AOwner: TComponent);
begin
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Create');
inherited Create(AOwner);
end;

destructor TDebugActionList.Destroy;
begin
inherited Destroy;
if Assigned(DebugLogObject) then DebugLogObject(Self, 'Destroy');
end;

initialization
lstDebugObjects:=TList.Create;
DebugLogObject:=DefaultDebugLogObject;
DebugWnd:=FindWindow('TfrmDebug', 'Debug');
if DebugWnd=0 then
	begin
	WinExec('d:\Debug.exe', SW_SHOWNORMAL);
	Sleep(500);
	DebugWnd:=FindWindow('TfrmDebug', 'Debug');      
	end;
ODSClear;

finalization
lstDebugObjects.Free;
//SendMessage(DebugWnd, WM_CLOSE, 0, 0);

end.
