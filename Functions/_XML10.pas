unit _XML10;

interface

uses SysUtils, Classes, TLB_MSXML, TypInfo, ActiveX, _Misc;

type IXMLDoc = IXMLDOMDocument;

const
  CLASS_DOMDoc: TGUID = '{2933BF90-7B36-11D2-B20E-00C04F983E60}';
  sFileNotFound = 'Файл "%s" не найден';

resourcestring
  sXMLFileError = 'Ошибка в документе "%s".'+#13#10;
type
  EXMLFileError = class(Exception)
    private
      procedure AppendFileNameToMessage(const AFileName: String);
    public
      constructor CreateEx(const Msg: string; const AFileName: String);
      constructor CreateFmtEx(const Msg: string; const Args: array of const; const AFileName: String);
    end;

resourcestring
  sXMLerror = 'Строка: %d'+#13#10+'Сообщение: %s';
  sXMLNodeNotFound = 'Узел "%s" не найден';

const
  E_Win1251 = 'Windows-1251';
  E_UTF8 = 'UTF8';
  E_Unicode = 'Unicode';

procedure xmlReadObject(const ANode: IXMLDOMNode; AObject: TObject; APropNames: array of string; ADataPaths: array of string);
procedure xmlSetAttribute(const ANode: IXMLDOMNode; const AName: WideString; AValue: OleVariant);
function xmlGetAttribute(const ANode: IXMLDOMNode; const AName: WideString): OleVariant;
procedure xmlAppendElement(const ANode: IXMLDOMNode; const AName: WideString; var AElement: IXMLDOMElement);
function xmlGetNode(const ANode: IXMLDOMNode; const APath: String; const Args: array of const; RaiseExceptionOnNIL: Boolean): IXMLDOMNode; overload;
function xmlGetNode(const ANode: IXMLDOMNode; const APath: String; RaiseExceptionOnNIL: Boolean): IXMLDOMNode; overload;
procedure xmlLoad(const AXMLFileName: String; var Doc: IXMLDoc);
procedure xmlReadStrings(const ANode: IXMLDOMNode; const APath: String; AStrings: TStrings);
procedure xmlCheckFileExists(const AXMLFileName, ARoot, AEncoding: String);
function xmlCreateFile(const AXMLFileName, ARoot, AEncoding: String): IXMLDoc;
function xmlStrLineBreaks(const S: String): String;
procedure xmlFindOrCreateNode(const AParentNode: IXMLDOMNode; const ANodeName: String; var AResNode: IXMLDOMNode);

type TXMLReadType = (rtText, rtNodeValue, rtNodeTypedValue);

procedure xmlReadStrings2(const ANode: IXMLDOMNode; const APath: String; AStrings: TStrings; AType: TXMLReadType);
procedure xmlReadObject2(const ANode: IXMLDOMNode; AObject: TObject; APropNames: array of string;
  ADataPaths: array of string; ARaiseOnError: array of Boolean; ATypes: array of TXMLReadType);

function xmlGetAllNodes(const ANode: IXMLDOMNode): IXMLDOMNodeList;

function xmlTextHeader(const ARoot: String; AOpenTag: Boolean; const AEncoding: String = E_Win1251): String;
function xmlProcessQuotes(const S: String): String;

implementation

procedure xmlFindOrCreateNode(const AParentNode: IXMLDOMNode;
  const ANodeName: String; var AResNode: IXMLDOMNode);
var E: IXMLDOMElement;
begin
AResNode:=xmlGetNode(AParentNode, ANodeName, False);
if AResNode=NIL then
  begin
  xmlAppendElement(AParentNode, ANodeName, E);
  AResNode:=E;
  end;
end;

function xmlProcessQuotes(const S: String): String;
begin
Result:=StringReplace(S, '"', '''', [rfReplaceAll])
end;

procedure EXMLFileError.AppendFileNameToMessage(const AFileName: String);
begin
Message:=Format(sXMLFileError, [AFileName])+Message;
end;

constructor EXMLFileError.CreateEx(const Msg: string; const AFileName: String);
begin
inherited Create(Msg);
AppendFileNameToMessage(AFileName)
end;

constructor EXMLFileError.CreateFmtEx(const Msg: string; const Args: array of const; const AFileName: String);
begin
inherited CreateFmt(Msg, Args);
AppendFileNameToMessage(AFileName)
end;

function xmlGetNode(const ANode: IXMLDOMNode; const APath: String; RaiseExceptionOnNIL: Boolean): IXMLDOMNode;
begin
Result:=ANode.selectSingleNode(APath);
if RaiseExceptionOnNIL and (Result=NIL) then
  raise EXMLFileError.CreateFmtEx(sXMLNodeNotFound, [ANode.nodeName+'/'+APath], ANode.ownerDocument.url);
end;

function xmlGetNode(const ANode: IXMLDOMNode; const APath: String; const Args: array of const; RaiseExceptionOnNIL: Boolean): IXMLDOMNode;
begin
Result:=xmlGetNode(ANode, Format(APath, Args), RaiseExceptionOnNIL)
end;

procedure xmlSetAttribute(const ANode: IXMLDOMNode; const AName: WideString; AValue: OleVariant);
var Attr: IXMLDOMNode;
begin
Attr:=ANode.attributes.getNamedItem(AName);
if Attr=NIL then
	begin
	Attr:=ANode.ownerDocument.createAttribute(AName);
	(ANode as IXMLDOMElement).setAttributeNode(Attr as IXMLDOMAttribute);
	end;
Attr.nodeValue:=AValue;
end;

function xmlGetAttribute(const ANode: IXMLDOMNode; const AName: WideString): OleVariant;
begin
Result:=ANode.attributes.getNamedItem(AName).nodeValue;
end;

procedure xmlAppendElement(const ANode: IXMLDOMNode; const AName: WideString; var AElement: IXMLDOMElement);
begin
AElement:=ANode.ownerDocument.createElement(AName);
ANode.appendChild(AElement);
end;

procedure xmlReadStrings(const ANode: IXMLDOMNode; const APath: String; AStrings: TStrings);
var I: Integer; TmpList: IXMLDOMNodeList;
begin
TmpList:=ANode.selectNodes(APath);
AStrings.Clear;
for I:=0 to TmpList.length-1 do AStrings.Add(TmpList.item[I].nodeValue{nodeTypedValue})
end;

procedure xmlReadObject(const ANode: IXMLDOMNode; AObject: TObject; APropNames: array of string; ADataPaths: array of string);
var I: Integer; V: Variant; TmpNode: IXMLDOMNode;
begin
for i:=Low(APropNames) to High(APropNames) do
  begin
  if PropIsType(AObject, APropNames[i], tkClass) and (GetObjectProp(AObject, APropNames[i]) is TStrings) then
    xmlReadStrings(ANode, ADataPaths[i], TStrings(GetObjectProp(AObject, APropNames[i])))
  else
    begin
    TmpNode:=xmlGetNode(ANode, ADataPaths[i], True);
    V:=TmpNode.nodeValue{nodeTypedValue};
    SetPropValue(AObject, APropNames[i], V);
    end;
  end;
end;

procedure xmlReadStrings2(const ANode: IXMLDOMNode; const APath: String; AStrings: TStrings; AType: TXMLReadType);
var I: Integer; TmpList: IXMLDOMNodeList;
begin
TmpList:=ANode.selectNodes(APath);
AStrings.Clear;
for I:=0 to TmpList.length-1 do
  case AType of
  rtText:            AStrings.Add(TmpList.item[I].text);
  rtNodeValue:       AStrings.Add(TmpList.item[I].nodeValue);
  rtNodeTypedValue:  AStrings.Add(TmpList.item[I].nodeTypedValue);
  end; // case
end;

procedure xmlReadObject2(const ANode: IXMLDOMNode; AObject: TObject; APropNames: array of string;
  ADataPaths: array of string; ARaiseOnError: array of Boolean; ATypes: array of TXMLReadType);
var I: Integer; V: Variant; TmpNode: IXMLDOMNode;
begin
for i:=Low(APropNames) to High(APropNames) do
  begin
  if PropIsType(AObject, APropNames[i], tkClass) and (GetObjectProp(AObject, APropNames[i]) is TStrings) then
    xmlReadStrings2(ANode, ADataPaths[i], TStrings(GetObjectProp(AObject, APropNames[i])), ATypes[i])
  else
    begin
    TmpNode:=xmlGetNode(ANode, ADataPaths[i], ARaiseOnError[i]);
    if TmpNode<>NIL then
      begin
      case ATypes[i] of
      rtText:            V:=TmpNode.text;
      rtNodeValue:       V:=TmpNode.nodeValue;
      rtNodeTypedValue:  V:=TmpNode.nodeTypedValue;
      end; // case
      SetPropValue(AObject, APropNames[i], V);
      end;
    end;
  end;
end;

procedure xmlLoad(const AXMLFileName: String; var Doc: IXMLDoc);
var perror: IXMLDOMParseError;
begin
if not FileExists(AXMLFileName) then
  raise Exception.CreateFmt(sFileNotFound, [AXMLFileName]);

doc:=MyCreateComObject(CLASS_DOMDoc) as IXMLDoc;
doc.async:=False;
doc.load(AXMLFileName);
perror := doc.parseError;

if doc.parseError.errorCode<>0 then
  raise EXMLFileError.CreateFmtEx(sXMLError, [perror.line, perror.reason], AXMLFileName);
end;

function xmlCreateFile(const AXMLFileName, ARoot, AEncoding: String): IXMLDoc;
var F: TextFile;
begin
AssignFile(F, AXMLFileName);
Rewrite(F);
Writeln(F, '<?xml version="1.0" encoding="'+AEncoding+'"?>');
Writeln(F, '<'+ARoot+'>');
Writeln(F, '</'+ARoot+'>');
CloseFile(F);
xmlLoad(AXMLFileName, Result);
end;

procedure xmlCheckFileExists(const AXMLFileName, ARoot, AEncoding: String);
begin
if not FileExists(AXMLFileName) then xmlCreateFile(AXMLFileName, ARoot, AEncoding);
end;

function xmlStrLineBreaks(const S: String): String;
begin
Result:=StringReplace(S, #$A, #$D#$A, [rfReplaceAll])
end;

function xmlGetAllNodes(const ANode: IXMLDOMNode): IXMLDOMNodeList;
begin
Result:=ANode.selectNodes('*')
end;

function xmlTextHeader(const ARoot: String; AOpenTag: Boolean; const AEncoding: String = E_Win1251): String;
begin
if AOpenTag then
  Result:=Format('<?xml version="1.0" encoding="%s"?>'+#13#10+'<%s>', [AEncoding, ARoot])
else
  Result:=Format('</%s>', [ARoot])
end;

initialization
CoInitialize(NIL);

finalization
CoUninitialize;

end.