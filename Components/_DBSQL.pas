unit _DBSQL;

interface

uses SysUtils, Classes, DB, DBTables, ADODB;

const
  esExecSQL = 0;
  esReturnCursor = 1;
  esReturnValue = 2;

type
  TDoPrepareParams = procedure(AID: Integer; AParams: array of const; AData: Pointer) of object;

type TCustomSQLMgr = class(TComponent)
  private
    FDataSet: TDataSet;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function DBSQL(AID: Integer; AReturnCursor: Byte; AParams: array of const; AData: Pointer = NIL): OleVariant; virtual; abstract;
    function ExecSQL(const ASQL: String; AReturnCursor: Integer): OleVariant; virtual; abstract;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

type TStoredSQLMgr = class(TCustomSQLMgr)
  private
    FSQLs: TStringList;
    procedure SetSQLs(const Value: TStringList);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadSQLIDs(Reader: TReader);
    procedure WriteSQLIDs(Writer: TWriter);
  public
    function IndexOfDBSQL(ID: Integer): Integer;
    function FindDBSQL(ID: Integer): String;
    procedure AddSQL(AID: Integer; const ASQL: String);
    procedure AddSQLs(ADataSet: TDataSet);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SQLs: TStringList read FSQLs write SetSQLs;
  end;

type TDataSetSQLMgr = class(TStoredSQLMgr)
  protected
    procedure DoPrepareSQL(AID: Integer; AParams: array of const; AData: Pointer = NIL); virtual; abstract;
    procedure DoPrepareParams(AID: Integer; AParams: array of const; AData: Pointer = NIL); virtual; abstract;
    procedure DoExecSQL; virtual; abstract;

    procedure DoPrepareSQL2(const ASQL: string); virtual; abstract;
  public
    function DBSQL(AID: Integer; AReturnCursor: Byte; AParams: array of const; AData: Pointer = NIL): OleVariant; override;
    function ExecSQL(const ASQL: String; AReturnCursor: Integer): OleVariant; override;
  end;

type TADO_SQLMgr = class(TDataSetSQLMgr)
  private
    FOnPrepareParams: TDoPrepareParams;
  protected
    procedure DoPrepareSQL(AID: Integer; AParams: array of const; AData: Pointer = NIL); override;
    procedure DoPrepareParams(AID: Integer; AParams: array of const; AData: Pointer = NIL); override;
    procedure DoExecSQL; override;

    procedure DoPrepareSQL2(const ASQL: string); override;
  published
    property OnPrepareParams: TDoPrepareParams read FOnPrepareParams write FOnPrepareParams;
  end;

type TBDE_SQLMgr = class(TDataSetSQLMgr)
  private
    FOnPrepareParams: TDoPrepareParams;
  protected
    procedure DoPrepareSQL(AID: Integer; AParams: array of const; AData: Pointer = NIL); override;
    procedure DoPrepareParams(AID: Integer; AParams: array of const; AData: Pointer = NIL); override;
    procedure DoExecSQL; override;

    procedure DoPrepareSQL2(const ASQL: string); override;
  published
    property OnPrepareParams: TDoPrepareParams read FOnPrepareParams write FOnPrepareParams;
  end;

procedure Register;

implementation

{$R *.DCR}

procedure Register;
begin
RegisterComponents('My components', [TADO_SQLMgr, TBDE_SQLMgr]);
end;

function TDataSetSQLMgr.DBSQL(AID: Integer; AReturnCursor: Byte; AParams: array of const; AData: Pointer = NIL): OleVariant;
begin
if FDataSet=NIL then
  begin
  Result:=varEmpty;
  exit;
  end;

DoPrepareSQL(AID, AParams, AData);
DoPrepareParams(AID, AParams, AData);

if AReturnCursor=esExecSQL then
  begin
  DoExecSQL;
  Result:=varEmpty;
  end
else
  begin
  FDataSet.Active:=True;
  FDataSet.First;

  if AReturnCursor=esReturnValue then
    Result:=FDataSet.Fields[0].Value
  else
    Result:=not FDataSet.IsEmpty;
  end;
end;

function TDataSetSQLMgr.ExecSQL(const ASQL: String; AReturnCursor: Integer): OleVariant;
begin
if FDataSet=NIL then
  begin
  Result:=varEmpty;
  exit;
  end;

DoPrepareSQL2(ASQL);

if AReturnCursor=esExecSQL then
  begin
  DoExecSQL;
  Result:=varEmpty;
  end
else
  begin
  FDataSet.Active:=True;
  FDataSet.First;

  if AReturnCursor=esReturnValue then
    Result:=FDataSet.Fields[0].Value
  else
    Result:=not FDataSet.IsEmpty;
  end;
end;

function TStoredSQLMgr.FindDBSQL(ID: Integer): String;
var I: Integer;
begin
i:=IndexOfDBSQL(ID);
if i=-1 then
  raise Exception.Create('DBSQL Manager: index '+IntToStr(ID)+' not found')
else
  Result:=FSQLs[i];

{ TODO : сделать систему сообщений об ошибках, уточнить, где будет вылетать это исключение - на клиенте или на сервере }
end;

procedure TStoredSQLMgr.AddSQL(AID: Integer; const ASQL: String);
var I: Integer;
begin
for i:=0 to FSQLs.Count-1 do
  if Integer(FSQLs.Objects[i])=AID then
    begin
    if ASQL='' then FSQLs.Delete(i) else FSQLs[i]:=ASQL;
    exit;
    end;
FSQLs.AddObject(ASQL, TObject(AID));
end;

procedure TStoredSQLMgr.AddSQLs(ADataSet: TDataSet);
begin
ADataSet.First;
while not ADataSet.EOF do
  begin
  AddSQL(ADataSet.Fields[0].AsInteger, ADataSet.Fields[1].AsString);
  ADataSet.Next
  end;
end;

constructor TStoredSQLMgr.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FSQLs:=TStringList.Create;
end;

destructor TStoredSQLMgr.Destroy;
begin
FSQLs.Free;
inherited Destroy;
end;

procedure TCustomSQLMgr.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
inherited Notification(AComponent, Operation);
if (Operation=opRemove) and (AComponent=FDataSet) then FDataSet:=NIL;
end;

{ TADO_SQLMgr }

procedure TADO_SQLMgr.DoExecSQL;
begin
TADOQuery(FDataSet).ExecSQL;
end;

procedure TADO_SQLMgr.DoPrepareParams(AID: Integer; AParams: array of const; AData: Pointer);
begin
if Assigned(FOnPrepareParams) then FOnPrepareParams(AID, AParams, AData);
end;

procedure TADO_SQLMgr.DoPrepareSQL(AID: Integer; AParams: array of const; AData: Pointer);
begin
DoPrepareSQL2(Format(FindDBSQL(AID), AParams))
end;

procedure TADO_SQLMgr.DoPrepareSQL2(const ASQL: string);
begin
with TADOQuery(FDataSet) do
  begin
  SQL.Clear;
  SQL.Text:=ASQL;
  end;
end;

{ TBDE_SQLMgr }

procedure TBDE_SQLMgr.DoExecSQL;
begin
TQuery(FDataSet).ExecSQL;
end;

procedure TBDE_SQLMgr.DoPrepareParams(AID: Integer; AParams: array of const; AData: Pointer);
begin
if Assigned(FOnPrepareParams) then FOnPrepareParams(AID, AParams, AData);
end;

procedure TBDE_SQLMgr.DoPrepareSQL(AID: Integer; AParams: array of const; AData: Pointer);
begin
DoPrepareSQL2(Format(FindDBSQL(AID), AParams))
end;

procedure TBDE_SQLMgr.DoPrepareSQL2(const ASQL: string);
begin
with TQuery(FDataSet) do
  begin
  SQL.Clear;
  SQL.Text:=ASQL;
  end;
end;

function TStoredSQLMgr.IndexOfDBSQL(ID: Integer): Integer;
begin
Result:=FSQLs.IndexOfObject(TObject(ID))
end;

procedure TStoredSQLMgr.SetSQLs(const Value: TStringList);
begin
FSQLs.Assign(Value);
end;

procedure TStoredSQLMgr.DefineProperties(Filer: TFiler);
begin
inherited DefineProperties(Filer);
Filer.DefineProperty('SQLs', ReadSQLIDs, WriteSQLIDs, True);
end;

procedure TStoredSQLMgr.ReadSQLIDs(Reader: TReader);
var ID: Integer; S: String;
begin
Reader.ReadListBegin;
while not Reader.EndOfList do
  begin
  ID:=Reader.ReadInteger;
  S:=Reader.ReadString;
  FSQLs.AddObject(S, TObject(ID));
  end;
Reader.ReadListEnd;
end;

procedure TStoredSQLMgr.WriteSQLIDs(Writer: TWriter);
var I: Integer;
begin
Writer.WriteListBegin;
for I:=0 to FSQLs.Count-1 do
  begin
  Writer.WriteInteger(Integer(FSQLs.Objects[i]));
  Writer.WriteString(FSQLs[i]);
  end;
Writer.WriteListEnd;
end;

end.

