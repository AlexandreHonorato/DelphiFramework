unit _IntList;

interface

uses Classes, SysUtils;

resourcestring
  SIntListIndexError = 'List index out of bounds (%d)';

type
  TIntList = class;

  PIList = ^TIList;
  TIList = array[0..MaxListSize - 1] of Integer;
  TIntListSortCompare = function(Item1, Item2: Integer): Integer;

  TIntList = class(TObject)
  private
    FList: PIList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Integer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Integer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    procedure Assign(Lst: TIntList);
    destructor Destroy; override;
    function Add(Item: Integer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function IndexOf(Item: Integer): Integer;
    procedure Insert(Index: Integer; Item: Integer);
    function Remove(Item: Integer): Integer;
    procedure Sort(Compare: TIntListSortCompare);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Integer read Get write Put; default;
    function ToStr(AIncludeBrackets: Boolean = True): String;
  end;

function ILSortProc(Item1, Item2: Integer): Integer;
  
implementation

function ILSortProc(Item1, Item2: Integer): Integer;
begin
Result:=Item1-Item2
end;

{ TList }

destructor TIntList.Destroy;
begin
Clear;
end;

function TIntList.Add(Item: Integer): Integer;
begin
Result := FCount;
if Result = FCapacity then Grow;
FList^[Result] := Item;
Inc(FCount);
end;

procedure TIntList.Clear;
begin
SetCount(0);
SetCapacity(0);
end;

procedure TIntList.Delete(Index: Integer);
begin
if (Index < 0) or (Index >= FCount) then Error(@SIntListIndexError, Index);
Dec(FCount);
if Index < FCount then
  System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Integer));
end;

class procedure TIntList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
  MOV     EAX,[EBP+4]
  end;

begin
raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TIntList.Error(Msg: PResStringRec; Data: Integer);
begin
TIntList.Error(LoadResString(Msg), Data);
end;

function TIntList.Get(Index: Integer): Integer;
begin
if (Index < 0) or (Index >= FCount) then Error(@SIntListIndexError, Index);
Result := FList^[Index];
end;

procedure TIntList.Grow;
var Delta: Integer;
begin
if FCapacity > 64 then
  Delta := FCapacity div 4
else
  if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
SetCapacity(FCapacity + Delta);
end;

function TIntList.IndexOf(Item: Integer): Integer;
begin
Result := 0;
while (Result < FCount) and (FList^[Result] <> Item) do Inc(Result);
if Result = FCount then Result := -1;
end;

procedure TIntList.Insert(Index: Integer; Item: Integer);
begin
if (Index < 0) or (Index > FCount) then Error(@SIntListIndexError, Index);
if FCount = FCapacity then Grow;
if Index < FCount then
  System.Move(FList^[Index], FList^[Index + 1],
    (FCount - Index) * SizeOf(Integer));
FList^[Index] := Item;
Inc(FCount);
end;

procedure TIntList.Put(Index: Integer; Item: Integer);
begin
if (Index < 0) or (Index >= FCount) then Error(@SIntListIndexError, Index);
FList^[Index] := Item;
end;

function TIntList.Remove(Item: Integer): Integer;
begin
Result := IndexOf(Item);
if Result >= 0 then Delete(Result);
end;

procedure TIntList.SetCapacity(NewCapacity: Integer);
begin
if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then Error(@SIntListIndexError, NewCapacity);
if NewCapacity <> FCapacity then
  begin
  ReallocMem(FList, NewCapacity * SizeOf(Integer));
  FCapacity := NewCapacity;
  end;
end;

procedure TIntList.SetCount(NewCount: Integer);
var I: Integer;
begin
if (NewCount < 0) or (NewCount > MaxListSize) then Error(@SIntListIndexError, NewCount);
if NewCount > FCapacity then SetCapacity(NewCount);
if NewCount > FCount then
  FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Integer), 0)
else
  for I := FCount - 1 downto NewCount do Delete(I);
FCount := NewCount;
end;

procedure QuickSort(SortList: PIList; L, R: Integer; SCompare: TIntListSortCompare);
var
  I, J: Integer;
  P, T: Integer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TIntList.Sort(Compare: TIntListSortCompare);
begin
if (FList <> nil) and (Count > 0) then QuickSort(FList, 0, Count - 1, Compare);
end;

procedure TIntList.Assign(Lst: TIntList);
var I: Integer;
begin
Clear;
for i:=0 to Lst.Count-1 do Add(Lst[i]);
end;

function TIntList.ToStr(AIncludeBrackets: Boolean): String;
var I: Integer;
begin
if Count=0 then raise Exception.Create('IntList is Empty');

if AIncludeBrackets then Result:='(' else Result:='';

for i:=0 to Count-1 do
  Result:=Result+IntToStr(Items[i])+', ';
SetLength(Result, Length(Result)-2);

if AIncludeBrackets then Result:=Result+')';
end;

end.
