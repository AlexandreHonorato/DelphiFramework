unit uCallStackAnalyzeBootstrap_Itf;

interface

uses Classes, uFileMappers, uCallStack_Domain;

type ICallStackAnalyzeBootstrap = class
  public
    function CSA_Params_FileName: String; virtual; abstract;
    procedure GetUnitNames(AUnitNames: TStrings); virtual; abstract;
    function FileMappers: TFileMappers; virtual; abstract;
    procedure OpenMethod(AMethod: TcsMethod); virtual; abstract;
    procedure OpenUnit(AUnit: TcsUnit); virtual; abstract;
    procedure OpenErrorID(ACommand: TcsCommand); virtual; abstract;
  end;

implementation

end.
