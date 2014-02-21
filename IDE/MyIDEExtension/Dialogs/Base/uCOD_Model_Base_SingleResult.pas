unit uCOD_Model_Base_SingleResult;

interface

uses uCOD_Model_Base, uCodeEntities;

type m_COD_DlgResult_SingleResult = class(m_COD_DlgResult_Base)
  private
    FCodeObject: TCodeObject;
  public
    property CodeObject: TCodeObject read FCodeObject write FCodeObject;
  end;

implementation

end.
