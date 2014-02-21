unit _DateUtils;

interface

uses SysUtils, DateUtils;

var
  _Today, _StartOfWeek, _EndOfWeek, _StartOfMonth, _EndOfMonth: Integer;
  _Year, _Month, _Day: Word;

const duShortDayNames: array[1..7] of string = ('Пн', 'Вт', 'Ср', 'Чт', 'Пт', 'Сб', 'Вс');

const duLongDayNames: array[1..7] of string = ('понедельник', 'вторник', 'среда', 'четверг', 'пятница', 'суббота', 'воскресенье');

// дни недели в винительном падеже. Для фраз типа "Перенести на <день недели>"
const duLongDayNamesAccusative: array[1..7] of string = ('понедельник', 'вторник', 'среду', 'четверг', 'пятницу', 'субботу', 'воскресенье');

const duLongDayNamesCapital: array[1..7] of string = ('Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье');

function duWeekDay(D: TDateTime): Integer;
function duMonthLength(M, Y: Integer): Integer;
function duDate2Int(D: TDateTime): Integer;
function duWeekOfYear(ADate: TDateTime): Word;

function duYearBegin(AYear: Integer): Integer;
function duYearEnd(AYear: Integer): Integer;

implementation

function duDate2Int(D: TDateTime): Integer;
begin
Result:=Trunc(Double(D))
end;

function duWeekDay(D: TDateTime): Integer;
begin
Result:=DayOfWeek(D)-1;
if Result=0 then Result:=7;
end;

function duMonthLength(M, Y: Integer): Integer;
begin
Result:=MonthDays[IsLeapYear(Y)][M]
end;

function duWeekOfYear(ADate: TDateTime): Word;
var Y, M, D: Word;
begin
Result:=WeekOf(ADate);

// для последней недели года возвращает 1, переделываем в 53
if Result=1 then
  begin
  DecodeDate(ADate, Y, M, D);
  if M=12 then Result:=53;
  end;
end;

function duYearBegin(AYear: Integer): Integer;
begin
Result:=duDate2Int(EncodeDate(AYear, 1, 1));
end;

function duYearEnd(AYear: Integer): Integer;
begin
Result:=duDate2Int(EncodeDate(AYear, 12, 31));
end;

procedure InitDates;
begin
_Today:=duDate2Int(Now);
_StartOfWeek:=_Today-duWeekDay(_Today)+1;
_EndOfWeek:=_StartOfWeek+6;

DecodeDate(Now, _Year, _Month, _Day);
_StartOfMonth:=duDate2Int(EncodeDate(_Year, _Month, 1));
_EndOfMonth:=duDate2Int(IncMonth(_StartOfMonth))-1;
end;

initialization
InitDates;

end.
