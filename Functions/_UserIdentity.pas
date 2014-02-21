unit _UserIdentity;

interface

uses _Strings, SysUtils;

type TUserIdentityType = (idUnknown, idID, idLogin);

type TUserIdentity = record
    IdentityType: TUserIdentityType;
    ID: Integer;
    Login: String;
  end;
  PUserIdentity = ^TUserIdentity;

procedure GetUserIdentity(AUserIdentity: PUserIdentity);

implementation

procedure GetUserIdentity(AUserIdentity: PUserIdentity);

  function GetCmdStr_ID: Boolean;
  var cmdParam: String;
  begin
  Result:=False;

  cmdParam:=GetCmdParam('i');

  if cmdParam<>'' then
    begin
    try
      AUserIdentity^.ID:=StrToInt(cmdParam);
      AUserIdentity^.IdentityType:=idID;
      Result:=True;
    except
      raise Exception.CreateFmt('Wrong User ID %s', [cmdParam]);
    end; // try
    end;
  end;

  function GetCmdStr_Login: Boolean;
  var cmdParam: String;
  begin
  Result:=False;

  cmdParam:=GetCmdParam('l');

  if cmdParam<>'' then
    begin
    AUserIdentity^.Login:=cmdParam;
    AUserIdentity^.IdentityType:=idLogin;
    Result:=True;
    end;
  end;

  procedure InitByLogin;
  begin
  AUserIdentity^.Login:=UserLogin;
  AUserIdentity^.IdentityType:=idLogin;
  end;

begin
AUserIdentity^.IdentityType:=idUnknown;

if not GetCmdStr_ID then
  if not GetCmdStr_Login then
	  InitByLogin;
end;

end.
