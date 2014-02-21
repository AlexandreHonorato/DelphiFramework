unit uTokenMenu;

interface

uses SysUtils, Menus, uCodeEntities, ToolsAPI, _ToolsAPI, _Strings, Classes;

type TUnitElementMenu = class(TPopupMenu)
  private
    FUnitElement: TUnitElement;
  private
    procedure mnuGotoUnitElement_OnClick(Sender: TObject);
    procedure mnuUseItf_OnClick(Sender: TObject);
    procedure mnuUseImpl_OnClick(Sender: TObject);
  private
    procedure Add_GotoSelf;
    procedure Add_Uses;
  protected
    function AddMenuItem(const ACaption: String; AOnClick: TNotifyEvent; AParentMenuItem: TMenuItem = NIL): TMenuItem;
  public
    function AddGotoMenuItem(AUnitElement: TUnitElement; AParentMenuItem: TMenuItem = NIL): TMenuItem;

    function UnitCaption: String;
    property UnitElement: TUnitElement read FUnitElement;
    constructor Create(AUnitElement: TUnitElement; AAddDefaultItems: Boolean); reintroduce; virtual;
  end;

type TClassMenu = class(TUnitElementMenu)
  private
    procedure Add_ImplementedIn;
    procedure Add_Implements;
    function Add_Children: Boolean;
    function Add_Parent: Boolean;

    function GetClass: TCodeClass;
  public
    property Class_: TCodeClass read GetClass;
    constructor Create(AUnitElement: TUnitElement); reintroduce;
  end;

type TInterfaceMenu = class(TUnitElementMenu)
  private
    procedure Add_ImplementedIn;
    function GetInterface: TCodeInterface;
  public
    constructor Create(AUnitElement: TUnitElement); reintroduce;
    property Interface_: TCodeInterface read GetInterface;
  end;

implementation

uses uUnits, uClasses, uUI;

{ TClassMenu }

constructor TClassMenu.Create(AUnitElement: TUnitElement);
var b1, b2: Boolean;
begin
inherited Create(AUnitElement, True);

b1:=Add_Parent;
b2:=Add_Children;

if b1 or b2 then
  Items.Add(NewLine);
  
Add_ImplementedIn;
Add_Implements;
end;

procedure TClassMenu.Add_ImplementedIn;
var I: Integer; MI: TMenuItem;
begin
if Class_.Name[1]<>'I' then exit;

MI:=TMenuItem.Create(NIL);
MI.Caption:='&Implemented in';
Items.Add(MI);

for i:=0 to Class_.Children.Count-1 do
  AddGotoMenuItem(Class_.Children[i], MI);
end;

procedure TClassMenu.Add_Implements;
var I: Integer; MI: TMenuItem;
begin
if (Class_.ImplementedInterfaces.Count=0) and (Class_.Parent.Name[1]<>'I') then exit;

MI:=TMenuItem.Create(NIL);
MI.Caption:='&Implements';
Items.Add(MI);

for i:=0 to Class_.ImplementedInterfaces.Count-1 do
  AddGotoMenuItem(Class_.ImplementedInterfaces[i], MI);

if Class_.Parent.Name[1]='I' then
  AddGotoMenuItem(Class_.Parent, MI);
end;

function TClassMenu.GetClass: TCodeClass;
begin
Result:=TCodeClass(UnitElement)
end;

function TClassMenu.Add_Children: Boolean;
var I: Integer; MI: TMenuItem;
begin
Result:=False;
if (Class_.Children.Count=0) then exit;

MI:=TMenuItem.Create(NIL);
MI.Caption:='&Descendants';
Items.Add(MI);

for i:=0 to Class_.Children.Count-1 do
  AddGotoMenuItem(Class_.Children[i], MI);

Result:=True;  
end;

function TClassMenu.Add_Parent: Boolean;
var MI: TMenuItem;
begin
Result:=Assigned(Class_.Parent.Parent);
if Result then
	begin
	MI:=AddGotoMenuItem(Class_.Parent);
  MI.Caption:='&Parent: '+MI.Caption
	end;
end;

{ TUnitElementMenu }

function TUnitElementMenu.AddMenuItem(const ACaption: String; AOnClick:
    TNotifyEvent; AParentMenuItem: TMenuItem = NIL): TMenuItem;
begin
if AParentMenuItem=NIL then AParentMenuItem:=Items;

Result:=TMenuItem.Create(nil);
Result.Caption:=ACaption;
Result.OnClick:=AOnClick;
AParentMenuItem.Add(Result);
end;

procedure TUnitElementMenu.Add_GotoSelf;
begin
AddGotoMenuItem(FUnitElement);
Items.Add(NewLine);
end;

constructor TUnitElementMenu.Create(AUnitElement: TUnitElement; AAddDefaultItems: Boolean);
begin
inherited Create(NIL);
AutoHotkeys:=maManual;
Images:=UI.il16;

FUnitElement:=AUnitElement;

if AAddDefaultItems then
	begin
	Add_GotoSelf;
	Add_Uses;
	end;
end;

procedure TUnitElementMenu.Add_Uses;
begin
AddMenuItem('Use itf. ' + UnitCaption + ' (&1)', mnuUseItf_OnClick);
AddMenuItem('Use impl. ' + UnitCaption + ' (&2)', mnuUseImpl_OnClick);
Items.Add(NewLine);
end;

procedure TUnitElementMenu.mnuGotoUnitElement_OnClick(Sender: TObject);
begin
GotoUnitElement(TUnitElement(TMenuItem(Sender).Tag))
end;

procedure TUnitElementMenu.mnuUseImpl_OnClick(Sender: TObject);
begin
UseUnit(taCurrentEditView.Buffer, UnitCaption, False);
end;

procedure TUnitElementMenu.mnuUseItf_OnClick(Sender: TObject);
begin
UseUnit(taCurrentEditView.Buffer, UnitCaption, True);
end;

function TUnitElementMenu.UnitCaption: String;
begin
Result:=FUnitElement.Unit_.Caption(False)
end;

function TUnitElementMenu.AddGotoMenuItem(AUnitElement: TUnitElement;
    AParentMenuItem: TMenuItem = NIL): TMenuItem;
begin
Result:=AddMenuItem(Format('%s: %s (%s)', [AUnitElement.Name, AUnitElement.TypeName, AUnitElement.Unit_.Caption(True)]),
  mnuGotoUnitElement_OnClick, AParentMenuItem);
Result.ImageIndex:=UI.COT2II(AUnitElement.ObjectType);
Result.Tag:=Integer(AUnitElement)
end;

{ TInterfaceMenu }

constructor TInterfaceMenu.Create(AUnitElement: TUnitElement);
begin
inherited Create(AUnitElement, True);
Add_ImplementedIn;
end;

procedure TInterfaceMenu.Add_ImplementedIn;
var I: Integer; MI: TMenuItem;
begin
if Interface_.ImplementedIn.Count=0 then exit;

MI:=TMenuItem.Create(NIL);
MI.Caption:='&Implemented in';
Items.Add(MI);

for i:=0 to Interface_.ImplementedIn.Count-1 do
  AddGotoMenuItem(Interface_.ImplementedIn[i], MI);
end;

{ TInterfaceMenu }

function TInterfaceMenu.GetInterface: TCodeInterface;
begin
Result:=TCodeInterface(UnitElement)
end;

end.
