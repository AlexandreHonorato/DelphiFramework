unit _ViewModel;

interface

uses _Debug, _Entity, _Lists;

type TBaseViewModel = class(TEntity)
  end;

type TBaseViewModelList = class(TOwningList)
  private
    function GetItems(Index: Integer): TBaseViewModel;
  public
    property Items[Index: Integer]: TBaseViewModel read GetItems; default;
  end;

implementation

{ TBaseViewModelList }

function TBaseViewModelList.GetItems(Index: Integer): TBaseViewModel;
begin
Result := TBaseViewModel(inherited Items[Index])
end;

end.
