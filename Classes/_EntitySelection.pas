unit _EntitySelection;

interface

uses _Debug, _IntList, SysUtils, _Misc, _NonCOMInterface;

const
  IID_IEntitySelection: TGUID = '{EE4BF252-70B4-40F9-B419-2DDB75631E7E}';
  SID_IEntitySelection = '{EE4BF252-70B4-40F9-B419-2DDB75631E7E}';
type IEntitySelection = interface
  [SID_IEntitySelection]
    function Count: Integer;
    procedure GetSelectedIDs(AIDs: TIntList);
    function SingleID: Integer;
  end;

type TEntitySelectionImpl = class(TNonCOMInterface, IEntitySelection)
  protected
    function Count: Integer; virtual; abstract;
    procedure GetSelectedIDs(AIDs: TIntList); virtual; abstract;
    function SingleID: Integer; virtual; abstract;
  end;

type TEntitySelectionImpl_Null = class(TEntitySelectionImpl)
  protected
    function Count: Integer; override;
    procedure GetSelectedIDs(AIDs: TIntList); override;
    function SingleID: Integer; override;
  end;

type TEntitySelection = class(TDebugObject)
  private
    FSelectionItf: IEntitySelection;
    FSelectedIDs: TIntList;
    function GetSelCount: Integer;
    function GetSingleID: Integer;
  protected
    FSelectionImplementor: TObject;
  public
    function GetSelectedIDs: TIntList; overload;
    procedure GetSelectedIDs(AIDs: TIntList); overload;
    property SelCount: Integer read GetSelCount;
    property SingleID: Integer read GetSingleID;

    constructor Create(ASelectionImplementor: TObject); reintroduce; virtual;
    destructor Destroy; override;
  end;

type Func_Selection = function(): TEntitySelection of object;

implementation

uses _Null;

function TEntitySelection.GetSelectedIDs: TIntList;
begin
FSelectedIDs.Clear;
FSelectionItf.GetSelectedIDs(FSelectedIDs);
Result:=FSelectedIDs;
end;

constructor TEntitySelection.Create(ASelectionImplementor: TObject);
begin
inherited Create;

GetInterfaceOrRaise(ASelectionImplementor, IID_IEntitySelection, 'IEntitySelection', FSelectionItf);
FSelectionImplementor:=ASelectionImplementor;

FSelectedIDs:=TIntList.Create;
end;

destructor TEntitySelection.Destroy;
begin
if Assigned(FSelectedIDs) then FreeAndNIL(FSelectedIDs);

FSelectionItf:=NIL;
if Assigned(FSelectionImplementor) then FreeAndNIL(FSelectionImplementor);

inherited Destroy;
end;

procedure TEntitySelection.GetSelectedIDs(AIDs: TIntList);
begin
FSelectionItf.GetSelectedIDs(AIDs);
end;

function TEntitySelection.GetSelCount: Integer;
begin
Result:=FSelectionItf.Count
end;

function TEntitySelection.GetSingleID: Integer;
begin
Result:=FSelectionItf.SingleID
end;

{ TEntitySelectionImpl_Null }

function TEntitySelectionImpl_Null.Count: Integer;
begin
Result:=0;
end;

procedure TEntitySelectionImpl_Null.GetSelectedIDs(AIDs: TIntList);
begin
// do nothing
end;

function TEntitySelectionImpl_Null.SingleID: Integer;
begin
Result:=0
end;

end.
