unit _ActionScope;

interface

uses _Debug, _MessageBus, SysUtils, _NonCOMInterface;

type TActionScope = class(TNonCOMInterface)
  private
    FMessageBus: IMessageBus;
  public
    property MessageBus: IMessageBus read FMessageBus;
    constructor Create; override;
    destructor Destroy; override;
  end;
  
implementation

{ TActionScope }

constructor TActionScope.Create;
begin
inherited Create;
FMessageBus:=CreateMessageBus;
end;

destructor TActionScope.Destroy;
begin
FreeMessageBus(FMessageBus); 
inherited Destroy;
end;

end.
