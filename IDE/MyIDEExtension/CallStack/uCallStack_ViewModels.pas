unit uCallStack_ViewModels;

interface

uses uCallStack_Domain, _Lists, Graphics;

const
  iiOK = 0;
  iiError = 1;
  iiUnit = 2;
  iiUnitRed = 3;
  iiErrorID = 4;

type VM_cs_Item = class
  public
    function Caption: String; virtual; abstract;
    function SubItem: String; virtual; abstract;
    function Indent: Integer; virtual; abstract;
    function ImageIndex: Integer; virtual; abstract;
    function FontStyle: TFontStyles; virtual; abstract;
  end;

type DS_cs_Items = class(TOwningList)
  private
    function GetItems(Index: Integer): VM_cs_Item;
  public
    property Items[Index: Integer]: VM_cs_Item read GetItems; default;
  end;

type VM_cs_Unit = class(VM_cs_Item)
  public
    Unit_: TcsUnit;
    function Caption: String; override;
    function SubItem: String; override;
    function Indent: Integer; override;
    function ImageIndex: Integer; override;
    function FontStyle: TFontStyles; override;
    constructor Create(AUnit: TcsUnit); reintroduce;
  end;

type VM_cs_Method = class(VM_cs_Item)
  private
    FIndent: Integer;
  public
    Method: TcsMethod;
    function Caption: String; override;
    function SubItem: String; override;
    function Indent: Integer; override;
    function ImageIndex: Integer; override;
    function FontStyle: TFontStyles; override;
    constructor Create(AMethod: TcsMethod); reintroduce;
  end;

implementation

{ VM_cs_Unit }

function VM_cs_Unit.Caption: String;
begin
Result:=Unit_.Caption(True)
end;

constructor VM_cs_Unit.Create(AUnit: TcsUnit);
begin
inherited Create;
Unit_:=AUnit;
end;

function VM_cs_Unit.FontStyle: TFontStyles;
begin
Result:=[fsBold]
end;

function VM_cs_Unit.ImageIndex: Integer;
begin
if Unit_.HasErrors then
  Result:=iiUnitRed
else
  Result:=iiUnit;
end;

function VM_cs_Unit.Indent: Integer;
begin
Result:=0;
end;

function VM_cs_Unit.SubItem: String;
begin
Result:=Unit_.FileName
end;

{ VM_cs_Method }

function VM_cs_Method.Caption: String;
begin
Result:=Method.ShortName;
end;

constructor VM_cs_Method.Create(AMethod: TcsMethod);
var tmp: TcsMethod;
begin
inherited Create;
Method:=AMethod;

FIndent:=1;
tmp:=Method;
while tmp.OuterMethod<>NIL do
  begin
  Inc(FIndent);
  tmp:=tmp.OuterMethod;
  end;
end;

function VM_cs_Method.FontStyle: TFontStyles;
begin
Result:=[]
end;

function VM_cs_Method.ImageIndex: Integer;
begin
if Method.ErrorKind=ekNone then
  Result:=iiOK
else
  Result:=iiError
end;

function VM_cs_Method.Indent: Integer;
begin
Result:=FIndent
end;

function VM_cs_Method.SubItem: String;
begin
Result:=Method.FullName
end;

{ DS_cs_Items }

function DS_cs_Items.GetItems(Index: Integer): VM_cs_Item;
begin
Result:=VM_cs_Item(inherited Items[Index])
end;

end.
