program CallStackAnalyze_Test;

uses
  Forms,
  uCallStack_Domain in '..\MyIDEExtension\CallStack\uCallStack_Domain.pas',
  uCallStackAnalyzeFrm in '..\MyIDEExtension\CallStack\uCallStackAnalyzeFrm.pas' {frmCallStackAnalyze},
  uSourceParser_CallStack in '..\MyIDEExtension\CallStack\uSourceParser_CallStack.pas',
  uStreamParser_CallStack in '..\MyIDEExtension\CallStack\uStreamParser_CallStack.pas',
  uFileMappers in '..\CodeBaseEngine\FileMappers\uFileMappers.pas',
  uCodeEntities in '..\CodeBaseEngine\Domain\uCodeEntities.pas',
  uParseInfo in '..\CodeBaseEngine\Domain\uParseInfo.pas',
  uCodeBase in '..\CodeBaseEngine\Domain\uCodeBase.pas',
  uSourceParser in '..\CodeBaseEngine\SourceParsers\uSourceParser.pas',
  uStreamParser in '..\CodeBaseEngine\StreamParsers\uStreamParser.pas',
  uCallStackAnalyzeBootstrap_Itf in '..\MyIDEExtension\CallStack\uCallStackAnalyzeBootstrap_Itf.pas',
  uCallStackAnalyzeBootstrap_Test in 'uCallStackAnalyzeBootstrap_Test.pas',
  uCallStack_ViewModels in '..\MyIDEExtension\CallStack\uCallStack_ViewModels.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCallStackAnalyze, frmCallStackAnalyze);
  frmCallStackAnalyze.SetBootstrap(TCallStackAnalyzeBootstrap_Test.Create);
  frmCallStackAnalyze.UpdateContents;
  Application.Run;
end.
