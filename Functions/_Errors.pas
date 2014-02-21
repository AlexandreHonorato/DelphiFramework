unit _Errors deprecated;

interface

uses Windows, SysUtils;

procedure PushError(const AErrorGUID: String; const AError: String = '');
procedure SetError(const AErrorGUID: String; const AError: String = '');
procedure PopError;
procedure ClearErrorStack(AForceClear: Boolean);

function GetErrorStack: String;

procedure ErrorStackLock;
procedure ErrorStackUnlock;

procedure ShowError(AStr: String; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0; AShowErrorGUID: Boolean = True);
procedure ShowErrorFmt(const AStr: String; AParams: array of const; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0; AShowErrorGUID: Boolean = True);

var ShowErrorParentWnd: hWnd = 0;

implementation

type
  PErrorRec = ^TErrorRec;
  TErrorRec = record
    ErrorGUID: String;
    ErrorText: String;
    Next: PErrorRec;
    Prev: PErrorRec;
    BreakPoint: Boolean;
  end;

var ErrorRoot, ErrorCurr: PErrorRec;

function NewErrorRec(const AErrorGUID, AErrorText: String; APrev: PErrorRec): PErrorRec;
begin
New(Result);
Result^.ErrorGUID:=AErrorGUID;
Result^.ErrorText:=AErrorText;
Result^.Prev:=APrev;
Result^.Next:=NIL;
Result^.BreakPoint:=False;
if APrev<>NIL then APrev^.Next:=Result;
end;

function DisposeErrorRec(ARec: PErrorRec): PErrorRec;
begin
Result:=ARec^.Prev;
if Result<>NIL then Result^.Next:=NIL;
Dispose(ARec)
end;

procedure PushError(const AErrorGUID: String; const AError: String = '');
begin
ErrorCurr:=NewErrorRec(AErrorGUID, AError, ErrorCurr);
if ErrorRoot=NIL then ErrorRoot:=ErrorCurr;
end;

procedure SetError(const AErrorGUID: String; const AError: String = '');
begin
if ErrorCurr=NIL then
  PushError(AErrorGUID, AError)
else
  begin
  ErrorCurr^.ErrorGUID:=AErrorGUID;
  ErrorCurr^.ErrorText:=AError
  end;
end;

procedure PopError;
begin
if ErrorCurr<>NIL then
  begin
  ErrorCurr:=DisposeErrorRec(ErrorCurr);
  if ErrorCurr=NIL then ErrorRoot:=NIL;
  end;
end;

procedure ClearErrorStack(AForceClear: Boolean);
begin
while True do
	begin
  if ErrorCurr=NIL then break;
  if (not AForceClear) and ErrorCurr^.BreakPoint then break;

	ErrorCurr:=DisposeErrorRec(ErrorCurr);
	end;
if ErrorCurr=NIL then ErrorRoot:=NIL;
end;

function GetErrorStack: String;
var tmp: PErrorRec;
begin
tmp:=ErrorRoot;
Result:='';
while tmp<>NIL do
  begin
  Result:=Result+tmp^.ErrorGUID;
  if tmp^.ErrorText<>'' then Result:=Result+' ('+tmp^.ErrorText+')';
  Result:=Result+'->';
  tmp:=tmp^.Next;
  end;
if Length(Result)<>0 then SetLength(Result, Length(Result)-2);
end;

procedure ErrorStackLock;
begin
if ErrorCurr<>NIL then ErrorCurr^.BreakPoint:=True;
end;

procedure ErrorStackUnlock;
begin
if ErrorCurr<>NIL then ErrorCurr^.BreakPoint:=False;
end;

procedure ShowError(AStr: String; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0; AShowErrorGUID: Boolean = True);
begin
if ErrorCurr<>NIL then
  begin
  if AShowErrorGUID then AStr:=AStr+#13#10#13#10+'ErrorID: '+ErrorCurr^.ErrorGUID;
  ClearErrorStack(False);
  end;
if AWnd=0 then AWnd:=ShowErrorParentWnd;
MessageBox(AWnd, PChar(AStr), PChar(ACaption), MB_ICONERROR);
end;

procedure ShowErrorFmt(const AStr: String; AParams: array of const; const ACaption: String = 'Ошибка'; AWnd: hWnd = 0; AShowErrorGUID: Boolean = True);
begin
ShowError(Format(AStr, AParams), ACaption, AWnd, AShowErrorGUID);
end;

initialization
ErrorRoot:=NIL;
ErrorCurr:=NIL;

finalization
ClearErrorStack(True);

end.
