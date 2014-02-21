unit _Entity;

interface

uses _Debug, _IntList, SysUtils, _Misc, _Lists;

type TEntity = class(TDebugObject)
  private
    FID: Integer;
    FCaption: String;
  protected
    procedure SetID(Value: Integer); virtual;
  public
    property ID: Integer read FID write SetID;
    property Caption: String read FCaption write FCaption;
    property Text: String read FCaption write FCaption;
  end;

type TEntityList = class(TOwningList)
  private
    function GetItems(Index: Integer): TEntity;
  public
    property Items[Index: Integer]: TEntity read GetItems; default;
    function RemoveItem(AItemID: Integer): Boolean; virtual;
    procedure DeleteItem(AIndex: Integer); virtual;
    function ByID(ID: Integer): TEntity; overload; virtual;
    function ByID(ID: Integer; var Idx: Integer): TEntity; overload; virtual;
    function GetCaptionsStr(AByteSet: TByteSet): String;
    function GetIDsStr(AIncludeBrackets: Boolean = True): String;

    procedure ToIntList(AIntList: TIntList); overload;
    function ToIntList: TIntList; overload;
  end;

implementation

function TEntityList.ByID(ID: Integer; var Idx: Integer): TEntity;
var I: Integer; Tmp: TEntity;
begin
Result:=NIL;
Idx:=-1;
for i:=0 to Count-1 do
  begin
  Tmp:=Items[i];
  if Tmp.ID=ID then
    begin
    Result:=Tmp;
    Idx:=i;
    break;
    end;
  end;
end;

function TEntityList.ByID(ID: Integer): TEntity;
var I: Integer; Tmp: TEntity;
begin
Result:=NIL;
for i:=0 to Count-1 do
  begin
  Tmp:=Items[i];
  if Tmp.ID=ID then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

{ TEntityList }

function TEntityList.GetItems(Index: Integer): TEntity;
begin
Result:=TEntity(inherited Items[Index])
end;

function TEntityList.RemoveItem(AItemID: Integer): Boolean;
var Idx: Integer; Item: TEntity;
begin
Item:=ByID(AItemID, Idx);
Result:=Item<>NIL;
if Result then
  begin
  Delete(Idx);
  if OwnsItems then FreeAndNIL(Item);
  end;
end;

function TEntityList.GetCaptionsStr(AByteSet: TByteSet): String;
var I: Integer;
begin
Result:='';
for i:=0 to Count-1 do
  with Items[i] do
    if ID in AByteSet then
      Result:=Result+Caption+'; ';
if Result<>'' then SetLength(Result, Length(Result)-2);
end;

function TEntityList.GetIDsStr(AIncludeBrackets: Boolean = True): String;
var I: Integer;
begin
if Count=0 then raise Exception.Create('EntityList is Empty');

if AIncludeBrackets then Result:='(' else Result:='';

for i:=0 to Count-1 do
  Result:=Result+IntToStr(Items[i].ID)+', ';
SetLength(Result, Length(Result)-2);

if AIncludeBrackets then Result:=Result+')';
end;

procedure TEntityList.ToIntList(AIntList: TIntList);
var I: Integer;
begin
AIntList.Clear;
for i:=0 to Count-1 do AIntList.Add(Items[i].ID);
end;

function TEntityList.ToIntList: TIntList;
begin
Result:=TIntList.Create;
ToIntList(Result);
end;

procedure TEntityList.DeleteItem(AIndex: Integer);
var item: TEntity;
begin
item:=Items[AIndex];
Delete(AIndex);
if OwnsItems then FreeAndNIL(item);
end;

{ TEntity }

procedure TEntity.SetID(Value: Integer);
begin
FID:=Value;
end;

end.
