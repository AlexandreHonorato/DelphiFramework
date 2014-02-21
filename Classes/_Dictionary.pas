unit _Dictionary;

interface

uses Classes;

resourcestring
  sDictionaryIndexError = 'List index out of bounds (%d)';

type TValue = TObject;

type
  TDictionary = class;

  TDictionaryItem = record
      Key: Integer;
      Value: TValue;
    end;
  PDictionaryItem = ^TDictionaryItem;

  PDIList = ^TDIList;
  TDIList = array[0..MaxListSize - 1] of TDictionaryItem;
  TIntSortCompare = function(Item1, Item2: Integer): Integer;

  TDictionary = class(TObject)
  private
    FList: PDIList;
    FCount: Integer;
    FCapacity: Integer;
    function GetByKey(AKey: Integer): TValue;
    procedure Delete(Index: Integer);
    function IndexOf(AKey: Integer): Integer;
    function GetValues(Index: Integer): TValue;
  protected
    procedure Grow; virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(AKey: Integer; AValue: TValue): Integer;
    procedure Clear; virtual;
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function Remove(AKey: Integer): Integer;
    procedure Sort(Compare: TIntSortCompare);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property ByKey[AKey: Integer]: TValue read GetByKey; default;
    property Values[Index: Integer]: TValue read GetValues;
  end;

function ILSortProc(Item1, Item2: Integer): Integer;

implementation

function ILSortProc(Item1, Item2: Integer): Integer;
begin
Result:=Item1-Item2
end;

{ TList }

destructor TDictionary.Destroy;
begin
Clear;
end;

function TDictionary.Add(AKey: Integer; AValue: TValue): Integer;
begin
Result := FCount;
if Result = FCapacity then Grow;
FList^[Result].Key := AKey;
FList^[Result].Value := AValue;
Inc(FCount);
end;

procedure TDictionary.Clear;
begin
SetCount(0);
SetCapacity(0);
end;

procedure TDictionary.Delete(Index: Integer);
begin
if (Index < 0) or (Index >= FCount) then Error(@sDictionaryIndexError, Index);
Dec(FCount);
if Index < FCount then
  System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TDictionaryItem));
end;

class procedure TDictionary.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
  MOV     EAX,[EBP+4]
  end;

begin
raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TDictionary.Error(Msg: PResStringRec; Data: Integer);
begin
TDictionary.Error(LoadResString(Msg), Data);
end;

procedure TDictionary.Grow;
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

function TDictionary.IndexOf(AKey: Integer): Integer;
begin
Result := 0;
while (Result < FCount) and (FList^[Result].Key <> AKey) do Inc(Result);
if Result = FCount then Result := -1;
end;

function TDictionary.Remove(AKey: Integer): Integer;
begin
Result := IndexOf(AKey);
if Result >= 0 then Delete(Result);
end;

procedure TDictionary.SetCapacity(NewCapacity: Integer);
begin
if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then Error(@sDictionaryIndexError, NewCapacity);
if NewCapacity <> FCapacity then
  begin
  ReallocMem(FList, NewCapacity * SizeOf(TDictionaryItem));
  FCapacity := NewCapacity;
  end;
end;

procedure TDictionary.SetCount(NewCount: Integer);
var I: Integer;
begin
if (NewCount < 0) or (NewCount > MaxListSize) then Error(@sDictionaryIndexError, NewCount);
if NewCount > FCapacity then SetCapacity(NewCount);
if NewCount > FCount then
  FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TDictionaryItem), 0)
else
  for I := FCount - 1 downto NewCount do Delete(I);
FCount := NewCount;
end;

procedure QuickSort(SortList: PDIList; L, R: Integer; SCompare: TIntSortCompare);
var
  I, J: Integer;
  P: Integer;
  Tmp: TDictionaryItem;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1].Key;
    repeat
      while SCompare(SortList^[I].Key, P) < 0 do
        Inc(I);
      while SCompare(SortList^[J].Key, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        Tmp.Key := SortList^[I].Key;
        Tmp.Value := SortList^[I].Value;
        SortList^[I].Key := SortList^[J].Key;
        SortList^[I].Value := SortList^[J].Value;
        SortList^[J].Key := Tmp.Key;
        SortList^[J].Value := Tmp.Value;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TDictionary.Sort(Compare: TIntSortCompare);
begin
if (FList <> nil) and (Count > 0) then QuickSort(FList, 0, Count - 1, Compare);
end;

function TDictionary.GetByKey(AKey: Integer): TValue;
var I: Integer;
begin
Result:=NIL;
for i:=0 to FCount-1 do
  if FList^[i].Key=AKey then
    begin
    Result:=FList^[i].Value;
    break;
    end;
end;

function TDictionary.GetValues(Index: Integer): TValue;
begin
if (Index < 0) or (Index >= FCount) then Error(@sDictionaryIndexError, Index);
Result:=FList^[Index].Value;
end;

end.
