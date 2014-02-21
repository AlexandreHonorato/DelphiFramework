unit _SortList;

interface

uses Classes;

type TCompareItemsEvent = function(AItem1, AItem2: Pointer; AData: Integer): Integer of object;

type TSortList = class(TList)
  private
    FSortOrder: Integer;
    FOnCompare: TCompareItemsEvent;
    procedure SetSortOrder(const Value: Integer);
    procedure QuickSort2(L, R: Integer; AData: Integer);
    function DoCompareItems(AItem1, AItem2: Pointer; AData: Integer): Integer;
  protected
    function CompareItems(AItem1, AItem2: Pointer; AData: Integer): Integer; virtual;
  public
    property SortOrder: Integer read FSortOrder write SetSortOrder;
    procedure Sort2(AData: Integer);
    property OnCompare: TCompareItemsEvent read FOnCompare write FOnCompare;
  end;

implementation

{ TSortList }

function TSortList.CompareItems(AItem1, AItem2: Pointer; AData: Integer): Integer;
begin
Result:=0;
end;

function TSortList.DoCompareItems(AItem1, AItem2: Pointer; AData: Integer): Integer;
begin
if Assigned(FOnCompare) then
  Result:=FOnCompare(AItem1, AItem2, AData)
else
  Result:=CompareItems(AItem1, AItem2, AData)
end;

procedure TSortList.QuickSort2(L, R: Integer; AData: Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat
      while DoCompareItems(List^[I], P, AData) < 0 do
        Inc(I);
      while DoCompareItems(List^[J], P, AData) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := List^[I];
        List^[I] := List^[J];               
        List^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort2(L, J, AData);
    L := I;
  until I >= R;
end;

procedure TSortList.SetSortOrder(const Value: Integer);
begin
FSortOrder:=Value;
Sort2(0);
end;

procedure TSortList.Sort2(AData: Integer);
begin
if (List<>NIL) and (Count>0) then QuickSort2(0, Count-1, AData);
end;

end.
