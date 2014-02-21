unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, _XML10, uCodeLayers;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var lstLayerNames, lstLayerColors: TStringList; F: TextFile; I, J: Integer;
  us: TUnitSection; usesStatus: TUsesStatus;
begin
lstLayerNames:=TStringList.Create;
lstLayerColors:=TStringList.Create;
try
  lstLayerNames.Add('UNKNOWN');             lstLayerColors.Add('CCCCCC');
  lstLayerNames.Add('UI');                  lstLayerColors.Add('CCFF33');
  lstLayerNames.Add('VIEW_MODEL');          lstLayerColors.Add('9999CC');
  lstLayerNames.Add('BLL');                 lstLayerColors.Add('FF9999');
  lstLayerNames.Add('DOMAIN');              lstLayerColors.Add('FF99FF');
  lstLayerNames.Add('APP_INFRASTRUCTURE');  lstLayerColors.Add('99CCFF');
  lstLayerNames.Add('INFRASTRUCTURE');      lstLayerColors.Add('66CCFF');
  lstLayerNames.Add('PL');                  lstLayerColors.Add('99CC99');

  AssignFile(F, 'd:\w\layers.xml');
  Rewrite(F);
  Writeln(F, xmlTextHeader('Layers', True));

  Writeln(F, '<Names>');
  for i:=0 to lstLayerNames.Count-1 do
    Writeln(F, Format(#9+'<Layer ID="%d" Name="%s" Color="%s" />',
        [i, lstLayerNames[i], lstLayerColors[i]]));
  Writeln(F, '</Names>');

  Writeln(F, '<Map>');
  for i:=0 to lstLayerNames.Count-1 do
    begin
    Writeln(F, Format(#9+'<Layer ID="%d" Name="%s">', [i, lstLayerNames[i]]));
    for us:=Low(TUnitSection) to High(TUnitSection) do
      begin
      Writeln(F, Format(#9#9+'<Section ID="%d">', [Integer(us)]));
      for j:=0 to lstLayerNames.Count-1 do
        begin
        if i=j then usesStatus:=sOK else usesStatus:=sError;
        Writeln(F, Format(#9#9#9+'<Layer ID="%d" Name="%s" UsesStatus="%d" />',
            [j, lstLayerNames[j], Integer(usesStatus)]));
        end;
      Writeln(F, #9#9+'</Section>');
      end;
    Writeln(F, #9+'</Layer>');
    end;
  Writeln(F, '</Map>');

  Writeln(F, xmlTextHeader('Layers', False));
  CloseFile(F);
finally
  FreeAndNIL(lstLayerNames);
  FreeAndNIL(lstLayerColors);
end; // try
end;

end.
