unit _BaseDataSource_ViewModel;

interface

uses
  _ViewModel, SysUtils, _MessageBus, _Debug, _Entity, _Lists;

type TSimpleDataSource = class(TOwningList)
  private
    IsUpdating: Boolean;
    ChangedDuringUpdating: Boolean;
    FMessageBus: IMessageBus;
  protected
    class function UpdateAfterConstruction: Boolean; virtual;
    procedure UpdateData; virtual;
  public
    constructor Create; reintroduce; virtual;
    constructor CreateWithNullMessageBus;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddItem(AItem: TObject);
    procedure InsertItem(AIndex: Integer; AItem: TObject);
    function RemoveItem(AItem: TObject): Boolean;
    procedure Clear; override;

    procedure NotifyListChanged;
    procedure NotifyListChangedContents(AItemIndex: Integer = -1);
    procedure NotifyListClear;

    property MessageBus: IMessageBus read FMessageBus;
  end;

type TBaseDataSource_ViewModel = class(TEntityList)
  private
    IsUpdating: Boolean;
    ChangedDuringUpdating: Boolean;
    FMessageBus: IMessageBus;
    function GetItems(Index: Integer): TBaseViewModel;
  protected
    class function UpdateAfterConstruction: Boolean; virtual;
    procedure UpdateData; virtual;
  public
    constructor Create(AOwnsItems: Boolean = True); override;
    constructor CreateWithNullMessageBus;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddItem(AItem: TBaseViewModel);
    procedure InsertItem(AIndex: Integer; AItem: TBaseViewModel);
    function RemoveItem(AItemID: Integer): Boolean; override;
    procedure DeleteItem(AIndex: Integer); override;
    procedure Clear; override;

    procedure NotifyListChanged;
    procedure NotifyListChangedContents(AItemIndex: Integer = -1);
    procedure NotifyListClear;
    property MessageBus: IMessageBus read FMessageBus;

    property Items[Index: Integer]: TBaseViewModel read GetItems; default;
  end;

implementation

uses _Null, _DomainEventsList, Classes;

constructor TBaseDataSource_ViewModel.Create(AOwnsItems: Boolean = True);
begin
inherited Create(AOwnsItems);
IsUpdating:=False;
FMessageBus:=CreateMessageBus;
end;

constructor TBaseDataSource_ViewModel.CreateWithNullMessageBus;
begin
inherited Create(True);
IsUpdating:=False;
FMessageBus:=BaseNull._IMessageBus;
end;

destructor TBaseDataSource_ViewModel.Destroy;
begin
if BaseNull.IsNotNull(FMessageBus) then FreeMessageBus(FMessageBus); 
inherited Destroy;
end;

{ TBaseDataSource_ViewModel }

procedure TBaseDataSource_ViewModel.AddItem(AItem: TBaseViewModel);
begin
Add(AItem);
if IsUpdating then
  ChangedDuringUpdating:=True
else
  NotifyListChanged;
end;

{ TBaseDataSource_ViewModel }

procedure TBaseDataSource_ViewModel.InsertItem(AIndex: Integer; AItem:
    TBaseViewModel);
begin
Insert(AIndex, AItem);
if IsUpdating then
  ChangedDuringUpdating:=True
else
  NotifyListChanged;
end;

procedure TBaseDataSource_ViewModel.AfterConstruction;
begin
inherited AfterConstruction;

if UpdateAfterConstruction then
  UpdateData;
end;

procedure TBaseDataSource_ViewModel.BeginUpdate;
begin
if IsUpdating then
  raise Exception.Create('Twice called BeginUpdate()')
else
  begin
  ChangedDuringUpdating:=False;
  IsUpdating:=True;
  end;
end;

procedure TBaseDataSource_ViewModel.Clear;
begin
inherited Clear;

if Assigned(FMessageBus) then
  begin
  if IsUpdating then
    ChangedDuringUpdating:=True
  else
    NotifyListClear;
  end;
end;

procedure TBaseDataSource_ViewModel.EndUpdate;
begin
if not IsUpdating then
  raise Exception.Create('BeginUpdate() was not called');

try
  if ChangedDuringUpdating then NotifyListChanged;
finally
  IsUpdating:=False
end; // try
end;

function TBaseDataSource_ViewModel.GetItems(Index: Integer): TBaseViewModel;
begin
Result:=TBaseViewModel(inherited Items[Index])
end;

procedure TBaseDataSource_ViewModel.NotifyListChanged;
begin
svcNotifyList.Notify_ListChanged(FMessageBus);
end;

procedure TBaseDataSource_ViewModel.NotifyListChangedContents(AItemIndex: Integer = -1);
begin
svcNotifyList.Notify_ListChangedContents(FMessageBus, AItemIndex);
end;

procedure TBaseDataSource_ViewModel.NotifyListClear;
begin
svcNotifyList.Notify_ListClear(FMessageBus);
end;

class function TBaseDataSource_ViewModel.UpdateAfterConstruction: Boolean;
begin
Result:=True;
end;

procedure TBaseDataSource_ViewModel.UpdateData;
begin
//do nothing
end;

function TBaseDataSource_ViewModel.RemoveItem(AItemID: Integer): Boolean;
begin
Result:=inherited RemoveItem(AItemID);

if Result then
  begin
  if IsUpdating then
    ChangedDuringUpdating:=True
  else
    NotifyListChanged;
  end;
end;

constructor TSimpleDataSource.Create;
begin
inherited Create(True);
IsUpdating:=False;
FMessageBus:=CreateMessageBus;
end;

constructor TSimpleDataSource.CreateWithNullMessageBus;
begin
inherited Create(True);
IsUpdating:=False;
FMessageBus:=BaseNull._IMessageBus;
end;

destructor TSimpleDataSource.Destroy;
begin
if BaseNull.IsNotNull(FMessageBus) then FreeMessageBus(FMessageBus);
inherited Destroy;
end;

procedure TBaseDataSource_ViewModel.DeleteItem(AIndex: Integer);
begin
inherited DeleteItem(AIndex);

if IsUpdating then
  ChangedDuringUpdating:=True
else
  NotifyListChanged;
end;

{ TSimpleDataSource }

procedure TSimpleDataSource.AddItem(AItem: TObject);
begin
Add(AItem);
if IsUpdating then
  ChangedDuringUpdating:=True
else
  NotifyListChanged;
end;

procedure TSimpleDataSource.AfterConstruction;
begin
inherited AfterConstruction;

if UpdateAfterConstruction then
  UpdateData;
end;

procedure TSimpleDataSource.BeginUpdate;
begin
if IsUpdating then
  raise Exception.Create('Twice called BeginUpdate()')
else
  begin
  ChangedDuringUpdating:=False;
  IsUpdating:=True;
  end;
end;

procedure TSimpleDataSource.Clear;
begin
inherited Clear;

if Assigned(FMessageBus) then
  begin
  if IsUpdating then
    ChangedDuringUpdating:=True
  else
    NotifyListClear;
  end;
end;

procedure TSimpleDataSource.EndUpdate;
begin
if not IsUpdating then
  raise Exception.Create('BeginUpdate() was not called');

try
  if ChangedDuringUpdating then NotifyListChanged;
finally
  IsUpdating:=False
end; // try
end;

{ TSimpleDataSource }

procedure TSimpleDataSource.InsertItem(AIndex: Integer; AItem: TObject);
begin
Insert(AIndex, AItem);
if IsUpdating then
  ChangedDuringUpdating:=True
else
  NotifyListChanged;
end;

procedure TSimpleDataSource.NotifyListChanged;
begin
svcNotifyList.Notify_ListChanged(FMessageBus);
end;

procedure TSimpleDataSource.NotifyListChangedContents(AItemIndex: Integer = -1);
begin
svcNotifyList.Notify_ListChangedContents(FMessageBus, AItemIndex);
end;

procedure TSimpleDataSource.NotifyListClear;
begin
svcNotifyList.Notify_ListClear(FMessageBus);
end;

function TSimpleDataSource.RemoveItem(AItem: TObject): Boolean;
var idx: Integer;
begin
idx:=IndexOf(AItem);
Result:=idx<>-1;

if Result then
  begin
  Delete(idx);
  FreeAndNIL(AItem);

  if IsUpdating then
    ChangedDuringUpdating:=True
  else
    NotifyListChanged;
  end;
end;

class function TSimpleDataSource.UpdateAfterConstruction: Boolean;
begin
Result:=True;
end;

procedure TSimpleDataSource.UpdateData;
begin
//do nothing
end;

end.
