unit _RegClassesLists;

interface

uses SysUtils, Classes, _Debug;

type TRegClasses = class(TList)
  public
    procedure RegisterClass(AClass: TClass);
  end;

type TRegClassesStr = class(TStringList)
  public
    procedure RegisterClass(const AClassID: String; AClass: TClass);
    constructor Create;
    function ByID(const AClassID: String): TClass;
  end;

type TRegObjectsStr = class(TStringList)
  private
    FOwnsObjects: Boolean;
  public
    procedure Clear; override;
    procedure RegisterObject(const AObjectID: String; AObject: TObject);
    constructor Create(AOwnsObjects: Boolean); reintroduce;
    destructor Destroy; override;
    function ByID(const AObjectID: String): TObject;
  end;

type TRegClassesStr2 = class(TRegClassesStr)
  public
    procedure RegisterClass(AClass: TClass); overload;
    function ByName(const AClassName: String): TClass;
  end;

implementation

{ TRegClasses }

procedure TRegClasses.RegisterClass(AClass: TClass);
begin
Add(TObject(AClass));
end;

{ TRegClassesStr }

function TRegClassesStr.ByID(const AClassID: String): TClass;
var I: Integer;
begin
I:=IndexOf(AClassID);
if I=-1 then Result:=NIL else Result:=TClass(Objects[i]);
end;

constructor TRegClassesStr.Create;
begin
inherited Create;
Sorted:=True;
end;

procedure TRegClassesStr.RegisterClass(const AClassID: String; AClass: TClass);
begin
AddObject(AClassID, TObject(AClass));
end;

{ TRegClassesStr2 }

function TRegClassesStr2.ByName(const AClassName: String): TClass;
begin
Result:=ByID(AClassName);
end;

procedure TRegClassesStr2.RegisterClass(AClass: TClass);
begin
inherited RegisterClass(AClass.ClassName, AClass);
end;

{ TRegObjectsStr }

function TRegObjectsStr.ByID(const AObjectID: String): TObject;
var I: Integer;
begin
I:=IndexOf(AObjectID);
if I=-1 then Result:=NIL else Result:=Objects[i];
end;

procedure TRegObjectsStr.Clear;
var I: Integer;
begin
if FOwnsObjects then
  for i:=0 to Count-1 do
    Objects[i].Free;
inherited Clear;
end;

constructor TRegObjectsStr.Create(AOwnsObjects: Boolean);
begin
inherited Create;
Sorted:=True;
FOwnsObjects:=AOwnsObjects;
end;

destructor TRegObjectsStr.Destroy;
begin
Clear;
inherited Destroy;
end;

procedure TRegObjectsStr.RegisterObject(const AObjectID: String;
  AObject: TObject);
begin
AddObject(AObjectID, AObject);
end;

end.
