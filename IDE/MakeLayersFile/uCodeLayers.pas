unit uCodeLayers;

interface

uses
  _Entity, Graphics;

type TUnitSection = (usItf, usImpl);
type TUsesStatus = (sError, sNotResolved, sOK, sNoUnits);

type TUnitLayer = class(TEntity)
  private
    FColor: TColor;
  public
    property Color: TColor read FColor;
    constructor Create(AID: Integer; const ACaption: String; AColor: TColor); reintroduce;
  end;

type TUnitLayerList = class(TEntityList)
  private
    function GetItems(Index: Integer): TUnitLayer;
  public
    function ByID(ID: Integer): TUnitLayer; reintroduce; overload;
    function ByID(ID: Integer; var Idx: Integer): TUnitLayer; reintroduce; overload;

    property Items[Index: Integer]: TUnitLayer read GetItems; default;
  end;


implementation

{ TUnitLayer }

constructor TUnitLayer.Create(AID: Integer; const ACaption: String; AColor: TColor);
begin
inherited Create;
ID:=AID;
Caption:=ACaption;
FColor:=AColor;
end;

{ TUnitLayerList }

function TUnitLayerList.ByID(ID: Integer): TUnitLayer;
begin
Result:=TUnitLayer(inherited ByID(ID));

if Result=NIL then
  Result:=Items[0];
end;

function TUnitLayerList.ByID(ID: Integer; var Idx: Integer): TUnitLayer;
begin
Result:=TUnitLayer(inherited ByID(ID, Idx));

if Result=NIL then
  begin
  Result:=Items[0];
  Idx:=0;
  end;
end;

function TUnitLayerList.GetItems(Index: Integer): TUnitLayer;
begin
Result:=TUnitLayer(inherited Items[Index])
end;

end.
