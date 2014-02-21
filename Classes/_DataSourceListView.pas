unit _DataSourceListView;

interface

uses
  _BaseDataSource_ViewModel, _DomainEventsList, classes, ComCtrls, Messages,
  _ListView_VCL_ZZZ, _EntitySelection, SysUtils, _IntList, _Misc;

type TDataSourceListView = class(TListView)
  private
    FDataSource: TBaseDataSource_ViewModel;
    FOnFocus: TNotifyEvent;
    FSelection: TEntitySelection;
    procedure SetDataSource(const Value: TBaseDataSource_ViewModel);
  private
    procedure On_NEListChanged(AEvent: NEListChanged);
    procedure On_NEListChangedContents(AEvent: NEListChangedContents);
    procedure On_NEListClear(AEvent: NEListClear);
    procedure On_NEListItemHighlight(AEvent: NEListItemHighlight);
  protected
    procedure ClearUI; virtual;
    procedure UpdateUI; virtual;

    function DoCreateSelection: TEntitySelection; virtual;
    function GetNullDataSource: TBaseDataSource_ViewModel; virtual;
    function GetNullSelection: TEntitySelection; virtual;

    procedure DoInitSelection; virtual;
    procedure DoDoneSelection; virtual;

    procedure StartListeningDataSource; virtual;
    procedure StopListeningDataSource; virtual;
  protected
    procedure DoSetFocus; virtual;
    procedure WMSetFocus(var M: TWMSetFocus); message WM_SETFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DataSource: TBaseDataSource_ViewModel read FDataSource write SetDataSource;
    property Selection: TEntitySelection read FSelection;
    property OnFocus: TNotifyEvent read FOnFocus write FOnFocus;
  end;

type TEntitySelectionImpl_ListView = class(TEntitySelectionImpl)
  private
    FListView: TDataSourceListView;
  protected
    function Count: Integer; override;
    procedure GetSelectedIDs(AIDs: TIntList); override;
    function SingleID: Integer; override;
  public
    property ListView: TDataSourceListView read FListView; 
    constructor Create(AListView: TDataSourceListView); reintroduce; virtual;
  end;

type TDataSourceListView_SingleColumn = class(TDataSourceListView)
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses _Null;

procedure TDataSourceListView.ClearUI;
begin
Items.Count:=0;
Invalidate;
end;

constructor TDataSourceListView.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
RowSelect:=True;
ViewStyle:=vsReport;
HideSelection:=False;
OwnerData:=True;
ReadOnly:=True;

FDataSource:=GetNullDataSource;

DoInitSelection;
end;

destructor TDataSourceListView.Destroy;
begin
DoDoneSelection;

DataSource:=NIL;
inherited Destroy;
end;

procedure TDataSourceListView.DoInitSelection;
begin
FSelection:=DoCreateSelection;
end;

procedure TDataSourceListView.DoDoneSelection;
begin
if BaseNull.IsNotNull(FSelection) then FreeAndNIL(FSelection);
FSelection:=GetNullSelection;
end;

procedure TDataSourceListView.DoSetFocus;
begin
if Assigned(FOnFocus) then FOnFocus(Self);
end;

procedure TDataSourceListView.On_NEListChanged(AEvent: NEListChanged);
begin
UpdateUI;
end;

procedure TDataSourceListView.On_NEListChangedContents(AEvent: NEListChangedContents);
begin
if AEvent.ItemIndex=-1 then
  Invalidate
else
  ListItemInvalidate(Items[AEvent.ItemIndex]);
end;

procedure TDataSourceListView.On_NEListClear(AEvent: NEListClear);
begin
ClearUI;
end;

{ TDataSourceListView }

procedure TDataSourceListView.On_NEListItemHighlight(AEvent: NEListItemHighlight);
begin
if (AEvent.Mask and IH_MASK_INDEX)<>0 then
  ShowItem2(Self, AEvent.ItemIndex);
end;

procedure TDataSourceListView.SetDataSource(const Value: TBaseDataSource_ViewModel);
begin
if not BaseNull.IsNull(DataSource) then
	begin
	ClearUI;

	StopListeningDataSource;
	end;

FDataSource:=Value;
if FDataSource=NIL then FDataSource:=GetNullDataSource;

if not BaseNull.IsNull(DataSource) then
	begin
	StartListeningDataSource;

	UpdateUI;
	end;
end;

procedure TDataSourceListView.StartListeningDataSource;
begin
FDataSource.MessageBus.SignObject(Self,
  [NEListChanged,
   NEListChangedContents,
   NEListClear,
   NEListItemHighlight],
  [@TDataSourceListView.On_NEListChanged,
   @TDataSourceListView.On_NEListChangedContents,
   @TDataSourceListView.On_NEListClear,
   @TDataSourceListView.On_NEListItemHighlight]);
end;

procedure TDataSourceListView.StopListeningDataSource;
begin
FDataSource.MessageBus.UnsignObject(Self);
end;

procedure TDataSourceListView.UpdateUI;
begin
Items.Count:=FDataSource.Count;
ClearSelection;
Invalidate;
end;

procedure TDataSourceListView.WMSetFocus(var M: TWMSetFocus);
begin
inherited;
DoSetFocus;
end;

function TDataSourceListView.GetNullDataSource: TBaseDataSource_ViewModel;
begin
Result:=BaseNull._BaseDataSource_ViewModel;
end;

function TDataSourceListView.DoCreateSelection: TEntitySelection;
begin
Result:=TEntitySelection.Create(TEntitySelectionImpl_ListView.Create(Self));
end;

function TDataSourceListView.GetNullSelection: TEntitySelection;
begin
Result:=BaseNull._TEntitySelection;
end;

{ TEntitySelectionImpl_ListView }

function TEntitySelectionImpl_ListView.Count: Integer;
begin
Result:=FListView.SelCount;
end;

constructor TEntitySelectionImpl_ListView.Create(AListView: TDataSourceListView);
begin
inherited Create;
FListView:=AListView;
end;

procedure TEntitySelectionImpl_ListView.GetSelectedIDs(AIDs: TIntList);
var I: Integer;
begin
for i:=0 to FListView.Items.Count-1 do
  if FListView.Items[i].Selected then
    AIDs.Add(FListView.DataSource[I].ID); { TODO : через итератор }
end;

function TEntitySelectionImpl_ListView.SingleID: Integer;
begin
if FListView.SelCount=1 then
  Result:=FListView.DataSource[FListView.Selected.Index].ID
else
  Result:=NullID;
end;

{ TDataSourceListView_SingleColumn }

constructor TDataSourceListView_SingleColumn.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
ShowColumnHeaders:=False;
RowSelect:=False;
Columns.Add;
end;

procedure TDataSourceListView_SingleColumn.Resize;
begin
inherited Resize;
Columns[0].Width:=Width-20;
end;

end.
