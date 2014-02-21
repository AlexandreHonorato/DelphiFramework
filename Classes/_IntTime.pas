unit _IntTime;

interface

uses SysUtils, _Strings;

const NULL_TIME = 60*24+1;

type TIntTime = class
  private
    FValue: Word;
    FHours: Word;
    FMins: Word;
    FTime: TDateTime;
    procedure SetValue(const Value: Word);
    procedure SetTime(const Value: TDateTime);
  public
    property Value: Word read FValue write SetValue;
    property Hours: Word read FHours;
    property Mins: Word read FMins;
    function AsString: String;
    property Time: TDateTime read FTime write SetTime;
    constructor Create(AValue: Word); overload;
    constructor Create; overload;
    function EqualTo(AOther: TIntTime): Boolean; overload;
    function EqualTo(ATime: Integer): Boolean; overload;
    function IsNull: Boolean;
  end;

function HMToInt(AHours: Word; AMins: Word): Word;
procedure IntToHM(ATime: Word; var AHours: Word; var AMins: Word);

function TimeToInt(ATime: TDateTime): Word; overload;
function TimeToInt(ATime: TDateTime; var AHours: Word; var AMins: Word): Word; overload;

function IntToTime(AValue: Word): TDateTime;

function PeriodLength(AStartTime, AEndTime: Word): Word;

implementation

function IntToTime(AValue: Word): TDateTime;
var H, M, Dummy: Word;
begin
if AValue=NULL_TIME then
  begin
  H:=0;
  M:=0
  end
else
  IntToHM(AValue, H, M);

Dummy:=0;
Result:=EncodeTime(H, M, Dummy, Dummy);
end;

function PeriodLength(AStartTime, AEndTime: Word): Word;
begin
if AEndTime=NULL_TIME then
  Result:=0
else
  Result:=AEndTime-AStartTime;
end;

function TimeToInt(ATime: TDateTime): Word; overload;
var w1, w2: Word;
begin
Result:=TimeToInt(ATime, w1, w2);
end;

function TimeToInt(ATime: TDateTime; var AHours: Word; var AMins: Word): Word;
var w1, w2: Word;
begin
DecodeTime(ATime, AHours, AMins, w1, w2);
Result:=HMToInt(AHours, AMins);
end;

function HMToInt(AHours: Word; AMins: Word): Word;
begin
Result:=AHours*60+AMins;
end;

procedure IntToHM(ATime: Word; var AHours: Word; var AMins: Word);
begin
AHours:=ATime div 60;
AMins:=ATime mod 60;
end;

function TIntTime.AsString: String;
begin
if IsNull then
  Result:='(не назначено)'
else
  Result:=LeadingSymbols(IntToStr(FHours), 2, '0')+':'+LeadingSymbols(IntToStr(FMins), 2, '0')
end;

constructor TIntTime.Create(AValue: Word);
begin
inherited Create;
Value:=AValue;
end;

function TIntTime.EqualTo(AOther: TIntTime): Boolean;
begin
Result:=FValue=AOther.Value;
end;

constructor TIntTime.Create;
begin
inherited Create;
Value:=NULL_TIME;
end;

function TIntTime.EqualTo(ATime: Integer): Boolean;
begin
Result:=FValue=ATime;
end;

procedure TIntTime.SetTime(const Value: TDateTime);
begin
FTime:=Value;
FValue:=TimeToInt(FTime, FHours, FMins);
end;

procedure TIntTime.SetValue(const Value: Word);
var Dummy: Word;
begin
FValue:=Value;
IntToHM(FValue, FHours, FMins);

if FValue=NULL_TIME then
  FTime:=0
else
	begin
	Dummy:=0;
	FTime:=EncodeTime(FHours, FMins, Dummy, Dummy);
	end;
end;

function TIntTime.IsNull: Boolean;
begin
Result:=FValue=NULL_TIME;
end;

end.
