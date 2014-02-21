program CodeBase_Test;

uses
  Forms,
  main in 'main.pas' {Form1},
  uCodeEntities in '..\CodeBaseEngine\Domain\uCodeEntities.pas',
  uCodeBase in '..\CodeBaseEngine\Domain\uCodeBase.pas',
  uSourceParser_CodeBase in '..\CodeBaseEngine\SourceParsers\uSourceParser_CodeBase.pas',
  uFileMappers in '..\CodeBaseEngine\FileMappers\uFileMappers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
