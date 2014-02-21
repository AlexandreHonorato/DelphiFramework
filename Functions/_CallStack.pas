unit _CallStack;

interface

{$I ..\CompilerVer.inc}

uses Windows, SysUtils {$IFDEF DELPHI_XE}, Generics.Collections{$ENDIF};

type ICallStack = class
  public
    constructor Create; virtual;

    procedure Clear(AForceClear: Boolean); virtual; abstract;

    {$IFNDEF DELPHI_XE}
    function ToString: String; virtual; abstract;
    {$ENDIF}

    function CurrentID: String; virtual; abstract;

    procedure Lock; virtual; abstract;
    procedure Unlock; virtual; abstract;

    procedure Push(const AID: String; const AExtraInfo: String = ''); virtual; abstract;
    procedure Set_(const AID: String; const AExtraInfo: String = ''); virtual; abstract;
    procedure Pop; virtual; abstract;
  end;

function CreateCallStack: ICallStack;

{$IFDEF DELPHI_XE}
type TCallStacks = class
  private
    FCallStacks: TDictionary<Cardinal, ICallStack>;
  public
    function ForCallingThread: ICallStack;
    function InitCallStack(AThreadID: Cardinal): ICallStack;
    procedure DoneCallStack(AThreadID: Cardinal);
    constructor Create;
    destructor Destroy; override;
  end;
{$ENDIF}

implementation

type
  PStackRec = ^TStackRec;
  TStackRec = record
    ID: String;
    ExtraInfo: String;
    Next: PStackRec;
    Prev: PStackRec;
    BreakPoint: Boolean;
  end;

type TCallStack = class(ICallStack)
  private
    FRoot: PStackRec;
    FCurr: PStackRec;
    function DisposeStackRec(ARec: PStackRec): PStackRec;
    function NewStackRec(const AID, AExtraInfo: String; APrev: PStackRec): PStackRec;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear(AForceClear: Boolean); override;

    function ToString: String; override;
    function CurrentID: String; override;

    procedure Lock; override;
    procedure Unlock; override;

    procedure Push(const AID: String; const AExtraInfo: String = ''); override;
    procedure Set_(const AID: String; const AExtraInfo: String = ''); override;
    procedure Pop; override;
  end;

function CreateCallStack: ICallStack;
begin
Result:=TCallStack.Create;
end;

function TCallStack.NewStackRec(const AID, AExtraInfo: String; APrev: PStackRec): PStackRec;
begin
New(Result);
Result^.ID:=AID;
Result^.ExtraInfo:=AExtraInfo;
Result^.Prev:=APrev;
Result^.Next:=NIL;
Result^.BreakPoint:=False;
if APrev<>NIL then APrev^.Next:=Result;
end;

function TCallStack.DisposeStackRec(ARec: PStackRec): PStackRec;
begin
Result:=ARec^.Prev;
if Result<>NIL then Result^.Next:=NIL;
Dispose(ARec)
end;

{ TCallStack }

procedure TCallStack.Clear(AForceClear: Boolean);
begin
while True do
	begin
  if FCurr=NIL then break;
  if (not AForceClear) and FCurr^.BreakPoint then break;

	FCurr:=DisposeStackRec(FCurr);
	end;
if FCurr=NIL then FRoot:=NIL;
end;

constructor TCallStack.Create;
begin
inherited Create;
FRoot:=NIL;
FCurr:=NIL;
end;

function TCallStack.CurrentID: String;
begin
if FCurr=NIL then
  Result:=''
else
  Result:=FCurr^.ID
end;

destructor TCallStack.Destroy;
begin
Clear(True);
inherited Destroy;
end;

procedure TCallStack.Lock;
begin
if FCurr<>NIL then FCurr^.BreakPoint:=True;
end;

procedure TCallStack.Pop;
begin
if FCurr<>NIL then
  begin
  FCurr:=DisposeStackRec(FCurr);
  if FCurr=NIL then FRoot:=NIL;
  end;
end;

procedure TCallStack.Push(const AID: String; const AExtraInfo: String = '');
begin
FCurr:=NewStackRec(AID, AExtraInfo, FCurr);
if FRoot=NIL then FRoot:=FCurr;
end;

procedure TCallStack.Set_(const AID: String; const AExtraInfo: String = '');
begin
if FCurr=NIL then
  Push(AID, AExtraInfo)
else
  begin
  FCurr^.ID:=AID;
  FCurr^.ExtraInfo:=AExtraInfo
  end;
end;

function TCallStack.ToString: String;
var tmp: PStackRec;
begin
tmp:=FRoot;
Result:='';
while tmp<>NIL do
  begin
  Result:=Result+tmp^.ID;
  if tmp^.ExtraInfo<>'' then Result:=Result+' ('+tmp^.ExtraInfo+')';
  Result:=Result+'->';
  tmp:=tmp^.Next;
  end;
if Length(Result)<>0 then SetLength(Result, Length(Result)-2);
end;

procedure TCallStack.Unlock;
begin
if FCurr<>NIL then FCurr^.BreakPoint:=False;
end;

{ ICallStack }

constructor ICallStack.Create;
begin
inherited Create;
end;

{$IFDEF DELPHI_XE}

{ TCallStacks }

constructor TCallStacks.Create;
begin
inherited Create;
FCallStacks:=TDictionary<Cardinal, ICallStack>.Create;
end;

destructor TCallStacks.Destroy;
begin
if Assigned(FCallStacks) then FreeAndNIL(FCallStacks);
inherited Destroy;
end;

procedure TCallStacks.DoneCallStack(AThreadID: Cardinal);
var callStack: ICallStack;
begin
TMonitor.Enter(FCallStacks);
try
  if FCallStacks.TryGetValue(AThreadID, callStack) then
    begin
    FreeAndNIL(callStack);
    FCallStacks.Remove(AThreadID);
    end;
finally
  TMonitor.Exit(FCallStacks);
end; // try
end;

function TCallStacks.ForCallingThread: ICallStack;
var key: Cardinal;
begin
key:=GetCurrentThreadId;
if not FCallStacks.TryGetValue(key, Result) then
  raise Exception.CreateFmt('Not found CallStack for Thread=%d', [key]);
end;

function TCallStacks.InitCallStack(AThreadID: Cardinal): ICallStack;
begin
TMonitor.Enter(FCallStacks);
try
  Result:=CreateCallStack;
  try
    FCallStacks.Add(AThreadID, Result);
  except
    FreeAndNIL(Result);
    raise
  end; // try
finally
  TMonitor.Exit(FCallStacks);
end; // try
end;
{$ENDIF}

end.
