unit uParseDirectives;

interface

uses SysUtils, _Strings;

function CodeBaseDirective(const AText: String; out ACmd, ACmdParam: String): Boolean;

implementation

function CodeBaseDirective(const AText: String; out ACmd, ACmdParam: String): Boolean;
var S: String; P: Integer;
begin
Result:=(Length(AText)>2) and (copy(AText, 1, 2)='{.');
if Result then
  begin
  s:=Trim(copy(AText, 3, Length(AText)-3));
  p:=pos(' ', s);
  if p=0 then
    begin
    ACmd:=s;
    ACmdParam:=''
    end
  else
    begin
    ACmd:=copy(s, 1, p-1);
    ACmdParam:=StrTail(s, p);
    end;
  end;
end;

end.
