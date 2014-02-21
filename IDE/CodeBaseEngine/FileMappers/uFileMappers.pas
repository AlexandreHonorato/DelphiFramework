unit uFileMappers;

interface

uses
  classes, _MessageBus, SysUtils;

type TMapFileDelegate = procedure(AStream: TMemoryStream; const AFileName: String;
    out AGotData: Boolean; out AStopIteration: Boolean) of object;

type TFileMappers = class(TList)
  private
    function GetItems(Index: Integer): TMapFileDelegate;
  public
    procedure Clear; override;
    property Items[Index: Integer]: TMapFileDelegate read GetItems; default;
    function AddFMSDelegate(ADelegate: TMapFileDelegate): TFileMappers; overload;
    function AddFMSDelegate(AObject: TObject; AMethodAddr: Pointer): TFileMappers; overload;
    function MapFile(const AFileName: String; AMemoryStream: TMemoryStream): Boolean;
  end;

type FileMappers_Default = class
  public
    class procedure Map_FromFile(AStream: TMemoryStream; const AFileName: String;
        out AGotData: Boolean; out AStopIteration: Boolean);
    class function Build: TFileMappers; virtual;
  end;

implementation

function TFileMappers.AddFMSDelegate(ADelegate: TMapFileDelegate): TFileMappers;
var P: PMethod;
begin
GetMem(P, SizeOf(TMethod));
P^.Code:=TMethod(ADelegate).Code;
P^.Data:=TMethod(ADelegate).Data;
Add(P);

Result:=Self
end;

function TFileMappers.AddFMSDelegate(AObject: TObject; AMethodAddr: Pointer): TFileMappers;
var P: PMethod;
begin
GetMem(P, SizeOf(TMethod));
P^.Code:=AMethodAddr;
P^.Data:=AObject;
Add(P);

Result:=Self
end;

procedure TFileMappers.Clear;
var I: Integer;
begin
for i:=0 to Count-1 do
  FreeMem(inherited Items[i], SizeOf(TMethod));
inherited Clear;
end;

function TFileMappers.GetItems(Index: Integer): TMapFileDelegate;
begin
Result:=TMapFileDelegate(inherited Items[Index]^)
end;

class function FileMappers_Default.Build: TFileMappers;
begin
Result:=TFileMappers.Create
  .AddFMSDelegate(Map_FromFile)
end;

class procedure FileMappers_Default.Map_FromFile(AStream: TMemoryStream; const
    AFileName: String; out AGotData: Boolean; out AStopIteration: Boolean);
begin
AGotData:=False;
if FileExists(AFileName) then
  begin
  AStream.LoadFromFile(AFileName);
  AGotData:=True;
  AStopIteration:=True;
  end;
end;

function TFileMappers.MapFile(const AFileName: String; AMemoryStream: TMemoryStream): Boolean;
var
  stopIteration: Boolean;
  I: Integer;
  delegate: TMapFileDelegate;
begin
Result:=False;

stopIteration:=False;
for i:=0 to Count-1 do
  begin
  delegate:=Items[i];
  delegate(AMemoryStream, AFileName, Result, stopIteration);
  if stopIteration then break;
  end;
end;

end.
