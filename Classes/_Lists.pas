unit _Lists;

interface

uses _Debug;

type TOwningList = class(TDebugList)
  private
    FOwnsItems: Boolean;
  public
    constructor Create(AOwnsItems: Boolean); reintroduce; virtual;
    procedure Clear; override;
    property OwnsItems: Boolean read FOwnsItems;
  end;

type TOwningStringList = class(TDebugStringList)
  private
    FOwnsItems: Boolean;
  public
    constructor Create(AOwnsItems: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;
    property OwnsItems: Boolean read FOwnsItems;
  end;

implementation

constructor TOwningList.Create(AOwnsItems: Boolean);
begin
inherited Create;
FOwnsItems:=AOwnsItems;
end;

procedure TOwningList.Clear;
var I: Integer;
begin
if FOwnsItems then
  for i:=0 to Count-1 do
    TObject(Items[i]).Free;

inherited Clear;
end;

constructor TOwningStringList.Create(AOwnsItems: Boolean);
begin
inherited Create;
FOwnsItems:=AOwnsItems;
end;

procedure TOwningStringList.Clear;
var I: Integer;
begin
if FOwnsItems then
  for i:=0 to Count-1 do
    Objects[i].Free;

inherited Clear;
end;

destructor TOwningStringList.Destroy;
begin
Clear;
inherited Destroy;
end;

end.
