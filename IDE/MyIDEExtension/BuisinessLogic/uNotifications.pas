unit uNotifications;

interface

uses _DomainEvents;

type NEUseUnit = class(DomainEvent)
  public
    UnitID: String;
    UnitName: String;
    IsItfSection: Boolean;
  end;

implementation

end.
