unit _NonCOMInterface;

interface

uses _Debug;

type TNonCOMInterface = class(TDebugObject, IUnknown)
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

implementation

{ TNonCOMInterface.IInterface }

function TNonCOMInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
if GetInterface(IID, Obj) then
  Result:=S_OK
else
  Result:=E_NOINTERFACE
end;

function TNonCOMInterface._AddRef: Integer;
begin
Result:=-1;
end;

function TNonCOMInterface._Release: Integer;
begin
Result:=-1;
end;

end.
