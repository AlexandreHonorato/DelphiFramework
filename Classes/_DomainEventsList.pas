unit _DomainEventsList;

interface

uses _DomainEvents, _MessageBus, SysUtils;

type NEListClear = class(DomainEvent)
  end;

type NEListChanged = class(DomainEvent)
  public
    InternalEvent: DomainEvent;
  end;

type NEListChangedContents = class(DomainEvent)
  public
    ItemIndex: Integer;
    InternalEvent: DomainEvent;
  end;

const
  IH_MASK_ID = 1;
  IH_MASK_INDEX = 2;

type NEListItemHighlight = class(DomainEvent)
  public
    ItemID: Integer;
    ItemIndex: Integer;
    Mask: Integer;
  end;

type svcNotifyList = class
  public
    class procedure Notify_ListClear(AMessageBus: IMessageBus);
    class procedure Notify_ListChanged(AMessageBus: IMessageBus; AInternalEvent: DomainEvent = NIL);
    class procedure Notify_ListChangedContents(AMessageBus: IMessageBus; AInternalEvent: DomainEvent = NIL); overload;
    class procedure Notify_ListChangedContents(AMessageBus: IMessageBus; AItemIndex: Integer; AInternalEvent: DomainEvent = NIL); overload;

    class procedure Notify_ListItemHighlight_ByID(AMessageBus: IMessageBus; AItemID: Integer);
    class procedure Notify_ListItemHighlight_ByIndex(AMessageBus: IMessageBus; AItemIndex: Integer);
  end;

implementation

{ svcNotifyList }

class procedure svcNotifyList.Notify_ListChanged(AMessageBus: IMessageBus; AInternalEvent: DomainEvent = NIL);
var Event: NEListChanged;
begin
Event:=NEListChanged.Create;
try
  Event.InternalEvent:=AInternalEvent;

  AMessageBus.Notify(Event);
finally
  FreeAndNIL(Event);
end; // try
end;

class procedure svcNotifyList.Notify_ListChangedContents(AMessageBus: IMessageBus; AInternalEvent: DomainEvent = NIL);
begin
Notify_ListChangedContents(AMessageBus, -1, AInternalEvent);
end;

class procedure svcNotifyList.Notify_ListChangedContents(AMessageBus: IMessageBus; AItemIndex: Integer; AInternalEvent:
    DomainEvent = NIL);
var Event: NEListChangedContents;
begin
Event:=NEListChangedContents.Create;
try
  Event.ItemIndex:=AItemIndex;
  Event.InternalEvent:=AInternalEvent;

  AMessageBus.Notify(Event);
finally
  FreeAndNIL(Event);
end; // try
end;

class procedure svcNotifyList.Notify_ListClear(AMessageBus: IMessageBus);
var Event: NEListClear;
begin
Event:=NEListClear.Create;
try
  AMessageBus.Notify(Event);
finally
  FreeAndNIL(Event);
end; // try
end;

class procedure svcNotifyList.Notify_ListItemHighlight_ByID(AMessageBus: IMessageBus; AItemID: Integer);
var Event: NEListItemHighlight;
begin
Event:=NEListItemHighlight.Create;
try
  Event.ItemID:=AItemID;
  Event.Mask:=IH_MASK_ID;

  AMessageBus.Notify(Event);
finally
  FreeAndNIL(Event);
end; // try
end;

class procedure svcNotifyList.Notify_ListItemHighlight_ByIndex(AMessageBus: IMessageBus; AItemIndex: Integer);
var Event: NEListItemHighlight;
begin
Event:=NEListItemHighlight.Create;
try
  Event.ItemIndex:=AItemIndex;
  Event.Mask:=IH_MASK_INDEX;

  AMessageBus.Notify(Event);
finally
  FreeAndNIL(Event);
end; // try
end;

end.
