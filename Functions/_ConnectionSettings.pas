unit _ConnectionSettings;

interface

uses SConnect, _XML10, TLB_MSXML, _Strings;

const FN_ConnCfg = 'ConnCfg.xml';

const
  CS_ALL_FIELDS = 0;
  CS_HOST_AND_PORT_ONLY = 1;

type TConnectionSettings = record
    Host: String;
    Port: Integer;
    DataSnapServer: String;
    DBConfig: String;
  end;
  PConnectionSettings = ^TConnectionSettings;

procedure GetConnectionSettings(AConnectionSettings: PConnectionSettings;
  AFlags: Integer = CS_ALL_FIELDS;
  const AFileName: String = '';
  const ADoc: IXMLDoc = NIL);

procedure SetupSocketConnection(ASocketConnection: TSocketConnection; AConnectionSettings: PConnectionSettings);

implementation

procedure GetConnectionSettings(AConnectionSettings: PConnectionSettings;
  AFlags: Integer = CS_ALL_FIELDS;
  const AFileName: String = '';
  const ADoc: IXMLDoc = NIL);
var doc: IXMLDoc; fileName: String; node: IXMLDOMNode;
begin
if Assigned(ADoc) then
  doc:=ADoc
else
  begin
  if AFileName='' then
    fileName:=AppPath+FN_ConnCfg
  else
    fileName:=AFileName;

  xmlLoad(fileName, doc);
  end;

node:=xmlGetNode(doc.documentElement, 'Server', True);

AConnectionSettings^.Host:=xmlGetAttribute(node, 'Host');
AConnectionSettings^.Port:=xmlGetAttribute(node, 'Port');

if (AFlags and CS_HOST_AND_PORT_ONLY)=0 then
	begin
  AConnectionSettings^.DataSnapServer:=xmlGetAttribute(node, 'ServerName');
  AConnectionSettings^.DBConfig:=xmlGetAttribute(node, 'DBConfig');
	end;
end;

procedure SetupSocketConnection(ASocketConnection: TSocketConnection; AConnectionSettings: PConnectionSettings);
begin
with ASocketConnection do
  begin
  Host:=AConnectionSettings^.Host;
  Port:=AConnectionSettings^.Port;
  ServerName:=AConnectionSettings^.DataSnapServer;
  end;
end;

end.