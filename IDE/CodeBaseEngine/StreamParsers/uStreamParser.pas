unit uStreamParser;

interface

uses
  classes;

type TStreamParser = class
  public
    procedure ParseStream(const AFileName: String; AStream: TMemoryStream); virtual; abstract;
  end;

implementation

end.
