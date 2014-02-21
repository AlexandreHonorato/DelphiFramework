program LayersTest;

uses
  Forms,
  main in 'main.pas' {Form1},
  uCodeLayers in 'uCodeLayers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
