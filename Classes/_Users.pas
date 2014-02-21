unit _Users;

interface

uses SysUtils, _Debug, _IntList, Classes, _Entity;

type TBaseUser = class(TEntity)
  private
    FLogin: String;
    function GetName: String;
    procedure SetName(const Value: String);
  public
    property Name: String read GetName write SetName;
    property Login: String read FLogin write FLogin;
  end;

type TBaseUserList = class(TEntityList)
  private
    function GetUsers(Index: Integer): TBaseUser;
  public
    property Users[Index: Integer]: TBaseUser read GetUsers; default;

    function AddUser(AID: Integer; AAllUsers: TBaseUserList): Integer;

    procedure ToStrings(AStrings: TStrings); overload;
    function ToStrings: TStrings; overload;

    procedure Assign(AUsers: TBaseUserList);

    procedure SortByName;
  end;

type TBaseUserListClass = class of TBaseUserList;

implementation

procedure TBaseUserList.Assign(AUsers: TBaseUserList);
var I: Integer;
begin
Clear;
for i:=0 to AUsers.Count-1 do Add(AUsers[i]);
end;

function TBaseUserList.GetUsers(Index: Integer): TBaseUser;
begin
Result:=TBaseUser(inherited Items[Index]);
end;

function SortByNameProc(Item1, Item2: Pointer): Integer;
begin
Result:=AnsiCompareText(TBaseUser(Item1).Name, TBaseUser(Item2).Name)
end;

procedure TBaseUserList.SortByName;
begin
Sort(SortByNameProc);
end;

function TBaseUserList.AddUser(AID: Integer; AAllUsers: TBaseUserList): Integer;
var user: TBaseUser;
begin
user:=TBaseUser(AAllUsers.ByID(AID));
if not Assigned(user) then
  raise Exception.CreateFmt('User ID=%d not found', [AID]);

Result:=Add(user);
end;

{ TBaseUser }

function TBaseUser.GetName: String;
begin
Result:=Text
end;

procedure TBaseUser.SetName(const Value: String);
begin
Text:=Value
end;

procedure TBaseUserList.ToStrings(AStrings: TStrings);
var I: Integer; user: TBaseUser;
begin
AStrings.Clear;
for i:=0 to Count-1 do
	begin
  user:=Users[i];
	AStrings.AddObject(user.Name, user);
	end;
end;

function TBaseUserList.ToStrings: TStrings;
begin
Result:=TStringList.Create;
ToStrings(Result);
end;

end.
