unit uCodeTemplates_L4;

interface

//UAL 4

uses SysUtils, _XML10, TLB_MSXML;

type TCT_PL = class
  public
    procedure ReadTemplates(AFolder: TObject);
  end;

var CT_PL: TCT_PL;

procedure Init_CT_PL;
procedure Done_CT_PL;

implementation

uses uCodeTemplates_L3;

const FN_CodeTemplates = 'd:\andrey\delphi\__FrameWork\IDEConfig\CodeTemplates.xml';

procedure Init_CT_PL;
begin
CT_PL:=TCT_PL.Create;
end;

procedure Done_CT_PL;
begin
CT_PL.Free;
end;

{ TCT_PL }

procedure TCT_PL.ReadTemplates(AFolder: TObject);

  procedure ReadTemplate(ANode: IXMLDOMNode; AFolder: TCTFolder);
  var I: Integer; Node1, Node2: IXMLDOMNode; CT: TCTTemplate; Block: TTemplateBlock;
    S: String;
  begin
  CT:=AFolder.NewTemplate;
  CT.Caption:=xmlGetAttribute(ANode, 'Caption');
  try
    S:=xmlGetAttribute(ANode, 'HotKey');
    CT.HotKey:=S[1];
  except
    //do nothing
  end; // try

  try
    CT.ShortCut:=xmlGetAttribute(ANode, 'ShortCut');
    CT_BLL.ShortCuts.AddObject(CT.ShortCut, CT);
  except
    //do nothing
  end; // try

  Node1:=xmlGetNode(ANode, 'Variables', True);
  for i:=0 to Node1.childNodes.length-1 do
    begin
    Node2:=Node1.childNodes.item[i];
    CT.Vars.NewVar(xmlGetAttribute(Node2, 'Caption'), xmlGetAttribute(Node2, 'Name'),
          '', xmlGetAttribute(Node2, 'DefaultValue'));
    end;

  Node1:=xmlGetNode(ANode, 'Blocks', True);
  CT.BlockCount:=Node1.childNodes.length;
  for i:=0 to CT.BlockCount-1 do
    begin
    Node2:=Node1.childNodes.item[i];
    Block:=CT.Blocks[i];
    Block.Caption:=xmlGetAttribute(Node2, 'Caption');
    S:=Node2.text;
    if S[1]=#$A then Delete(S, 1, 1); // delete first #$A for multiline blocks
    Block.SourceCode:=StringReplace(S, #$A, #$D#$A, [rfReplaceAll]);
    end;
  end;

  procedure ReadFolder(ANode: IXMLDOMNode; AFolder: TCTFolder);
  var I: Integer; Node1: IXMLDOMNode; S: String;
  begin
  try
    AFolder.Caption:=xmlGetAttribute(ANode, 'Caption');
    S:=xmlGetAttribute(ANode, 'HotKey');
    AFolder.HotKey:=S[1];
  except
    // do nothing
  end; // try

  for i:=0 to ANode.childNodes.length-1 do
    begin
    Node1:=ANode.childNodes.item[i];
    if Node1.nodeName='Folder' then
      ReadFolder(Node1, AFolder.NewFolder)
    else
    if Node1.nodeName='Template' then
      ReadTemplate(Node1, AFolder);
    end;
  end;

var Doc: IXMLDoc; 
begin
xmlLoad(FN_CodeTemplates, Doc);
ReadFolder(Doc.documentElement, TCTFolder(AFolder));
end;

end.
