unit _EnumFiles;

interface

uses SysUtils, Classes, _Strings, _Files;

type TEFEvent = procedure(Sender: TObject; const APath: String; SR: TSearchRec; ID: Integer) of object;
type TEFEvent2 = procedure(Sender: TObject; const APath: String; ID: Integer) of object;
type TEFEvent3 = procedure(Sender: TObject; const APath: String; SR: TSearchRec; var DontIncludeToTree: Boolean; ID: Integer) of object;

type TEnumFilesException = class(Exception);

type TEnumFiles = class(TComponent)
  private
    FTree: TStringList; // пути хранятся без замыкающего слеша
    FOnFile: TEFEvent;
    FOnFolderEnd: TEFEvent2;
    FOnFolderStart: TEFEvent2;
    FMask: String;
    FOnFolderEndBT: TEFEvent;
    FOnFolderStartBT: TEFEvent3;
    FID: Integer;
    FUseIDs: Boolean;
    FIncludeDirs: Boolean;
    FData1: Integer;
    FData3: Integer;
    FData2: Integer;
    procedure SetRoot(const Value: String);
    function GetRoot: String;
    procedure InternalBuildTree(const APath: String);
  public
    Cancel: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildTree;
    procedure EnumFiles;
    property Tree: TStringList read FTree;
  published
    property Data1: Integer read FData1 write FData1;
    property Data2: Integer read FData2 write FData2;
    property Data3: Integer read FData3 write FData3;
    property OnFolderStartBT: TEFEvent3 read FOnFolderStartBT write FOnFolderStartBT;
    property OnFolderEndBT: TEFEvent read FOnFolderEndBT write FOnFolderEndBT;
    property OnFolderStart: TEFEvent2 read FOnFolderStart write FOnFolderStart;
    property OnFolderEnd: TEFEvent2 read FOnFolderEnd write FOnFolderEnd;
    property OnFile: TEFEvent read FOnFile write FOnFile;
    property Mask: String read FMask write FMask;
    property Root: String read GetRoot write SetRoot;
    property UseIDs: Boolean read FUseIDs write FUseIDs default False;
    property IncludeDirsOnEnum: Boolean read FIncludeDirs write FIncludeDirs default False;
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
RegisterComponents('My components', [TEnumFiles]);
end;

procedure TEnumFiles.InternalBuildTree(const APath: String);
var SR: TSearchRec; Status, ID: Integer; S: String; B: Boolean;
begin
if Cancel then exit;

Status:=FindFirst(APath+'*.*', faFolder, SR);
while Status=0 do
  begin
//  MyProcessMessages;
  if Cancel then
	  begin
    FindClose(SR);
	  exit;
	  end;

  if ((SR.Attr and faDirectory)<>0) then
    if (SR.Name<>'.') and (SR.Name<>'..') then
      begin
      S:=APath+SR.Name+'\';

      B:=False;
      if FUseIDs then
	      begin
        ID:=FID;
	      Inc(FID);
	      end
      else
        ID:=0;
      if Assigned(FOnFolderStartBT) then FOnFolderStartBT(Self, S, SR, B, ID);
      if not B then
	      begin
	      InternalBuildTree(S);
	      if Assigned(FOnFolderEndBT) then FOnFolderEndBT(Self, S, SR, ID);
	      FTree.AddObject(CheckSlash2(S), TObject(ID));
	      end;
      end;

  Status:=FindNext(SR);
  end;
FindClose(SR);
end;

constructor TEnumFiles.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FTree:=TStringList.Create;
FUseIDs:=False;
FIncludeDirs:=False;
end;

destructor TEnumFiles.Destroy;
begin
FTree.Free;
inherited Destroy;
end;

procedure TEnumFiles.EnumFiles;
var I, Status, ID, SearchAttr: Integer; S, S1: String; SR: TSearchRec; MaskPtr, Ptr: PChar;
begin
if Mask='' then Mask:='*.*';

if not (Assigned(FOnFolderStart) or Assigned(FOnFolderEnd) or Assigned(FOnFile)) then exit;

for i:=0 to FTree.Count-1 do
	begin
  S:=FTree[i];
  S1:=S+'\';
  if FUseIDs then
    ID:=Integer(FTree.Objects[i])
  else
    ID:=0;
  if Assigned(FOnFolderStart) then FOnFolderStart(Self, S, ID);

  MaskPtr:=PChar(FMask);
  while MaskPtr<>nil do
    begin
    Ptr:=StrScan(MaskPtr, ';');
    if Ptr<>nil then
    Ptr^:=#0;

    SearchAttr:=faFile;
    if FIncludeDirs then SearchAttr:=SearchAttr or faDirectory;
		Status:=FindFirst(S1+MaskPtr, SearchAttr, SR);
		while Status=0 do
		  begin
      if (SR.Name<>'.') and (SR.Name<>'..') then
		    begin
//		    MyProcessMessages;
		    if Cancel then begin FindClose(SR); exit end;

	      if Assigned(FOnFile) then FOnFile(Self, S1+SR.Name, SR, ID);
		    end;
	  	Status:=FindNext(SR);
		  end;
		FindClose(SR);

    if Ptr<>nil then
      begin
      Ptr^:=';';
      Inc(Ptr);
      end;
    MaskPtr:=Ptr;
		end;

  if Assigned(FOnFolderEnd) then FOnFolderEnd(Self, S, ID);
	end;
end;

procedure TEnumFiles.SetRoot(const Value: String);
begin
FTree.Clear;
FTree.AddObject(CheckSlash2(Value), TObject(-1));
end;

function TEnumFiles.GetRoot: String;
begin
if FTree.Count=0 then Result:='' else Result:=FTree[0]
end;

procedure TEnumFiles.BuildTree;
begin
if Root='' then Root:=GetCurrentDir;
if FUseIDs then FID:=1;
InternalBuildTree(Root+'\');
end;

end.
