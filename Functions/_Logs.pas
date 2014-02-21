unit _Logs;

interface

// How to use:
// 1. При старте назначаем Logs.UserID, Logs.Options, Logs.FileName, Logs.Root
// 2. Можно ничего не назначать, по умолчанию:
//    - Logs.Root - читается из Logs.pth
//    - Logs.FileName = ComputerName+'_'+UserLogin+'.log';
// 3. В Logs.FileName можно дополнительно прописать путь к логу, например,
//    "DocFlow\Alarm\...\+DefaultLogName", тогда будет создана необходимая
//    иерархия вложенных папок
// 4. Пишем в лог вызовом Logs.Write
// 5. Можно вызвать Logs(True), тогда будет возвращена копия базового объекта
//    которую можно использовать для альтернативного логгирования (например,
//    основную - для лога ошибок, вторую - для протоколирования событий)
// 6. Есть свойство LocalFileName - в этот лог производится попытка записи,
//    если в основной не удалось. По умолчанию - AppFile('.log')

// Инициализация:
//  Logs.UserID:=UserLogin;
//  Logs.FileName:='Errors\NS_Loader\'+DefaultLogName;
//  Logs.Options:=[loDateTime, loUserID, loErrorID];

uses SysUtils, _PTHFiles, _Strings, _Files, _CallStack;

type TLogUption = (loDateTime, loUserID);
  TLogUptions = set of TLogUption;

type TFWLogs = class
  private
    FRoot: String;
    FRootInitialized: Boolean;
    FFileName: String;
    FOptions: TLogUptions;
    FUserID: String;
    FLocalFileName: String;
    function GetRoot: String;
    procedure SetRoot(const Value: String);
  public
    property Root: String read GetRoot write SetRoot;
    property FileName: String read FFileName write FFileName;
    constructor Create;
    procedure Write(const AText: String; ACallStack: ICallStack);
    property Options: TLogUptions read FOptions write FOptions;
    property UserID: String read FUserID write FUserID;
    property LocalFileName: String read FLocalFileName write FLocalFileName;
  end;

function Logs(AReturnNew: Boolean = False): TFWLogs;
var DefaultLogName: String;

implementation

var FWLogs: TFWLogs;

const FN_Logs = 'Logs.pth';

function Logs(AReturnNew: Boolean = False): TFWLogs;
begin
if AReturnNew then
  begin
  Result:=TFWLogs.Create;
  if Assigned(FWLogs) then
    begin
    Result.Root:=FWLogs.Root;
    Result.FRootInitialized:=FWLogs.FRootInitialized;
    Result.FileName:=FWLogs.FileName;
    Result.Options:=FWLogs.Options;
    Result.UserID:=FWLogs.UserID;
    end;
  end
else
	begin
	if not Assigned(FWLogs) then FWLogs:=TFWLogs.Create;
	Result:=FWLogs;
	end;
end;

{ TFWLogs }

constructor TFWLogs.Create;
begin
inherited Create;
FOptions:=[loDateTime];
FRootInitialized:=False;
FFileName:=DefaultLogName;
FLocalFileName:=AppFile('.log');
end;

function TFWLogs.GetRoot: String;
var FName: String;
begin
if (FRoot='') and (not FRootInitialized) then
  begin
  FName:=AppPath+FN_Logs;
  if FileExists(FName) then
    try
      FRoot:=CheckSlash(PTHRead(FName))
    except
      FRoot:='';
    end; // try
  end;
Result:=FRoot;
end;

procedure TFWLogs.SetRoot(const Value: String);
begin
if Value='' then
  FRoot:=''
else
  FRoot:=CheckSlash(Value);
end;

procedure TFWLogs.Write(const AText: String; ACallStack: ICallStack);

  procedure WriteLocal(const AText: String);
  var F: TextFile;
  begin
  if FLocalFileName<>'' then
    try
  	  AssignFile(F, FLocalFileName);
  	  if FileExists(FLocalFileName) then Append(F) else Rewrite(F);
  	  Writeln(F, AText);
  	  CloseFile(F);
    except
      //do nothing
    end; // try
  end;

var F: TextFile; FName: String; S: String;
begin
S:=AText;
if Assigned(ACallStack) then S:='Error="'+ACallStack.ToString+'" '+S;
if loUserID in FOptions then S:='UserID="'+FUserID+'" '+S;
if loDateTime in FOptions then S:=DateTimeToStr(Now)+' '+S;

if Root='' then
  WriteLocal(S)
else
  try
    FName:=FRoot+FFileName;
    if Pos('\', FFileName)<>0 then ForceDirectories(ExtractFilePath(FName));

    AssignFile(F, FName);
    if FileExists(FName) then Append(F) else Rewrite(F);
    Writeln(F, S);
    CloseFile(F);
  except
    WriteLocal(S)
  end; // try
end;

initialization
DefaultLogName:=ComputerName+'_'+UserLogin+'.log';

finalization
FreeAndNil(FWLogs);

end.
