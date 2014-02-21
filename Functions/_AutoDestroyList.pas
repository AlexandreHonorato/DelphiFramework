unit _AutoDestroyList;

interface

uses Classes, SysUtils;

const
  ADL_Repositories = 'Repositories';

  ADL_Domain_Framework = 'Domain_Framework';
  ADL_Domain_Misc = 'Domain_Misc';
  ADL_Domain_Singletons = 'Domain_Singletons';
  ADL_Domain_Null = 'ADL_Domain_Null';
  ADL_Domain_Orphan_MessageBuses = 'ADL_Domain_Orphan_MessageBuses';

  ADL_UI_Framework = 'UI_Framework';
  ADL_UI = 'UI';
  ADL_UI_Listeners = 'UI_Listeners';

  ADL_Undefined = 'Undefined';

procedure RegisterAutoDestroy(const ASection: String; AObject: TObject);
procedure DoneAutoDestroy(const ASection: String);

implementation

uses Math;

type TAutoDestroyList = class(TStringList)
  private
    function FindOrCreateSection(const ASection: String): TList;
  public
    procedure RegObject(const ASection: String; AObject: TObject);
    procedure Done(const ASection: String);
    constructor Create;
  end;

var FAutoDestroyList: TAutoDestroyList = NIL;

procedure RegisterAutoDestroy(const ASection: String; AObject: TObject);
begin
if not Assigned(FAutoDestroyList) then FAutoDestroyList:=TAutoDestroyList.Create;
FAutoDestroyList.RegObject(ASection, AObject);
end;

procedure DoneAutoDestroy(const ASection: String);
begin
if FAutoDestroyList=NIL then exit;

FAutoDestroyList.Done(ASection);
if FAutoDestroyList.Count=0 then FreeAndNil(FAutoDestroyList);
end;

{ TAutoDestroyList }

constructor TAutoDestroyList.Create;
begin
inherited Create;
Sorted:=True;
end;

procedure TAutoDestroyList.Done(const ASection: String);
var I, Idx: Integer; Section: TList;
begin
Idx:=IndexOf(ASection);
if Idx=-1 then exit;

Section:=TList(Objects[Idx]);
for i:=0 to Section.Count-1 do
  TObject(Section[i]).Free;

FreeAndNil(Section);
Delete(Idx);
end;

function TAutoDestroyList.FindOrCreateSection(const ASection: String): TList;
var Idx: Integer;
begin
Idx:=IndexOf(ASection);
if Idx=-1 then
  begin
  Result:=TList.Create;
  AddObject(ASection, Result)
  end
else
  Result:=TList(Objects[Idx]);
end;

procedure TAutoDestroyList.RegObject(const ASection: String; AObject: TObject);
var Section: TList;
begin
Section:=FindOrCreateSection(ASection);
Section.Add(AObject);
end;

end.
