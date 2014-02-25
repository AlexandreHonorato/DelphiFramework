// changes from SS 1 beta
//1: changed interface: function Exceute, ExecuteOperation => procedure Execute, ExecuteOperation + internal changes
// 2: property Wnd
// 3. property SourceFileName
// 4. func Error => proc Error, delete OnErrorProperty
// 5. in createfolder changed ShowError to raise Exception
// 6. delte property Aborted, proc Error
// 7. executeop from _FileOps

unit FileOps;

interface

uses Windows, SysUtils, Classes, Controls, Dialogs, ShellAPI, _Strings, _Files, _FileOps;

type
  TFileOpType = (fotMove, fotCopy, fotDelete, fotRename);
  TFileOpFlag = (fofAllowUndo, fofFilesOnly, fofNoConfirmation,
    fofNoConfirmMkDir, fofNoErrorUI, fofRenameCollision, fofSilent,
    fofSimpleProgress);
  TFileOpFlags = set of TFileOpFlag;

  TFileOps = class(TComponent)
  private
    FFrom: TStrings;
    FDestFolder: String;

    FProgressTitle: string;
    FOperation: TFileOpType;
    FOptions: TFileOpFlags;
    FAutoClear: Boolean;
    FWnd: hWnd;
    procedure SetFrom(const Value: TStrings);
    function GetSourceFileName: String;
    procedure SetSourceFileName(const Value: String);
  public
    procedure ExecuteOperation(AFrom: TStrings; ATo: String; AOpType: TFileOpType;
        AOptions: TFileOpFlags; AProgressTitle: String);
    procedure Execute;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Wnd: hWnd read FWnd write FWnd;


    // if AskMsg='' then create folder, named in AName;
    // else AskMsg is prompt in InputBox and AName is dir in which need to create folder.
    function CreateFolder(APath: String; AName: String = ''; ShowError: Boolean = True): String;
//    function CreateFolder(AName: String; AskMsg: String = ''; ErrorMsg: String = ''): Boolean;
  published
    property AutoClear: Boolean read FAutoClear write FAutoClear;
    property SourceFiles: TStrings read FFrom write SetFrom;
    property SourceFileName: String read GetSourceFileName write SetSourceFileName;
    property DestFolder: string read FDestFolder write FDestFolder;
    property ProgressTitle: string read FProgressTitle write FProgressTitle;
    property Operation: TFileOpType read FOperation write FOperation default fotCopy;
    property Options: TFileOpFlags read FOptions write FOptions default [];
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
RegisterComponents('My components', [TFileOps]);
end;

constructor TFileOps.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FAutoClear:=True;
FOperation := fotCopy;
FOptions := [];
FFrom := TStringList.Create;
FWnd:=0;
end;

destructor TFileOps.Destroy;
begin
FreeAndNil(FFrom);
inherited Destroy;
end;

procedure TFileOps.ExecuteOperation(AFrom: TStrings; ATo: String; AOpType: TFileOpType;
    AOptions: TFileOpFlags; AProgressTitle: String);
var Tmp: hWnd; TmpOptions: Word;
begin
if FWnd<>0 then
  Tmp:=FWnd
else
if (Owner is TWinControl) then
  Tmp:=TWinControl(Owner).Handle
else
  Tmp:=0;

TmpOptions := 0;
if fofAllowUndo in AOptions       then TmpOptions := TmpOptions or FOF_ALLOWUNDO;
if fofFilesOnly in AOptions       then TmpOptions := TmpOptions or FOF_FILESONLY;
if fofNoConfirmation in AOptions  then TmpOptions := TmpOptions or FOF_NOCONFIRMATION;
if fofNoConfirmMkDir in AOptions  then TmpOptions := TmpOptions or FOF_NOCONFIRMMKDIR;
if fofNoErrorUI in AOptions       then TmpOptions := TmpOptions or FOF_NOERRORUI;
if fofRenameCollision in AOptions then TmpOptions := TmpOptions or FOF_RENAMEONCOLLISION;
if fofSilent in AOptions          then TmpOptions := TmpOptions or FOF_SILENT;
if fofSimpleProgress in AOptions  then TmpOptions := TmpOptions or FOF_SIMPLEPROGRESS;

FileOpEx(Tmp, AFrom, ATo, Integer(AOpType)+1, TmpOptions, AProgressTitle, FAutoClear);
end;

procedure TFileOps.SetFrom(const Value: TStrings);
begin
FFrom.Assign(Value);
end;

procedure TFileOps.Execute;
begin
ExecuteOperation(FFrom, FDestFolder, FOperation, FOptions, FProgressTitle);
end;

function TFileOps.CreateFolder(APath: String; AName: String = ''; ShowError: Boolean = True): String;
begin
Result:='';

if AName='' then
  begin
  Result:=InputBox('Создание папки', 'Введите имя новой папки:', '');
  if Result='' then exit;
  end
else
  Result:=AName;

{$I-}
MkDir(CheckSlash(APath)+Result);
{$I+}
if IOResult<>0 then
  begin
  Result:='';
  raise Exception.CreateFmt('Ошибка при создании папки %s', [CheckSlash(APath)+Result])
  end;
end;

function TFileOps.GetSourceFileName: String;
begin
if FFrom.Count=0 then Result:='' else Result:=FFrom[0]
end;

procedure TFileOps.SetSourceFileName(const Value: String);
begin
FFrom.Clear;
FFrom.Add(Value);
end;

end.
