program TestUsesAnalyze;

uses
  Forms,
  uUsesAnalyzeFrm in '..\MyIDEExtension\UsesAnalyze\uUsesAnalyzeFrm.pas' {frmUsesAnalyze},
  uCodeBase in '..\CodeBaseEngine\Domain\uCodeBase.pas',
  uSourceParser_CodeBase in '..\CodeBaseEngine\SourceParsers\uSourceParser_CodeBase.pas',
  uUsesAnalyzeBootstrap_Itf in '..\MyIDEExtension\UsesAnalyze\uUsesAnalyzeBootstrap_Itf.pas',
  uFileMappers in '..\CodeBaseEngine\FileMappers\uFileMappers.pas',
  uSourceParser in '..\CodeBaseEngine\SourceParsers\uSourceParser.pas',
  uUsesAnalyzeBootstrap_Test in 'uUsesAnalyzeBootstrap_Test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmUsesAnalyze, frmUsesAnalyze);
  frmUsesAnalyze.SetBootstrap(TUsesAnalyzeBootstrap_Test.Create);
  frmUsesAnalyze.UpdateContents;
  Application.Run;
end.
