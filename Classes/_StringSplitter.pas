unit _StringSplitter;

interface

{$I ..\CompilerVer.inc}

uses _Debug, Classes, SysUtils, _Strings;

type TWordDelegate = procedure(const AWord: String; AData: TObject) of object;

type TStringSplitter = class(TDebugObject)
  private
    FWords: TStringList;
    FSeparators: TCharSet;
    procedure SetSeparators(const Value: TCharSet);
    procedure DoSplit(const AString: String; ADelegate: TWordDelegate; AData: TObject);
    procedure AddWordToStrings(const AWord: String; AData: TObject);
  public
    procedure Split(const AString: String; ADelegate: TWordDelegate; AData: TObject); overload;
    procedure Split(const AString: String; AResults: TStrings; AClear: Boolean = True); overload;
    function Split(const AString: String): TStrings; overload;

    property Separators: TCharSet read FSeparators write SetSeparators;

    constructor Create(ASeparators: TCharSet); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TStringSplitter }

procedure TStringSplitter.AddWordToStrings(const AWord: String; AData: TObject);
begin
TStrings(AData).Add(AWord)
end;

constructor TStringSplitter.Create(ASeparators: TCharSet);
begin
inherited Create;
Separators:=ASeparators;
end;

destructor TStringSplitter.Destroy;
begin
if Assigned(FWords) then FreeAndNIL(FWords);
inherited Destroy;
end;

procedure TStringSplitter.DoSplit(const AString: String; ADelegate: TWordDelegate; AData: TObject);
var S, word: String; p, wordStart, wordEnd: Integer; eol: Boolean;
begin
S:=AString;

eol:=False;
p:=1;
while True do
	begin
	while True do
	  begin
	  if p>Length(S) then
	    begin
	    eol:=True;
	    break;
	    end;

    {$IFDEF DELPHI_XE}
    if CharInSet(S[p], FSeparators) then
    {$ELSE}
	  if S[p] in FSeparators then
    {$ENDIF}
	    Inc(p)
	  else
	    break;
	  end;

	if eol then break;

  wordStart:=p;

	while True do
	  begin
	  if p>Length(S) then
	    begin
	    eol:=True;
	    break;
	    end;

    {$IFDEF DELPHI_XE}
	  if not CharInSet(S[p], FSeparators) then
    {$ELSE}
    if not (S[p] in FSeparators) then
    {$ENDIF}
	    Inc(p)
	  else
	    break;
	  end;

  wordEnd:=p;
  word:=copy(S, wordStart, wordEnd-wordStart);
  ADelegate(word, AData);

  if eol then break;
	end;
end;

procedure TStringSplitter.SetSeparators(const Value: TCharSet);
begin
if Value=[] then
  raise Exception.Create('Separators set can''t be empty');

FSeparators:=Value;
end;

procedure TStringSplitter.Split(const AString: String; ADelegate: TWordDelegate; AData: TObject);
begin
DoSplit(AString, ADelegate, AData);
end;

procedure TStringSplitter.Split(const AString: String; AResults: TStrings; AClear: Boolean = True);
begin
if AClear then AResults.Clear;
DoSplit(AString, AddWordToStrings, AResults);
end;

function TStringSplitter.Split(const AString: String): TStrings;
begin
if Assigned(FWords) then
  FWords.Clear
else
  FWords:=TStringList.Create;

DoSplit(AString, AddWordToStrings, FWords);

Result:=FWords;
end;

end.
