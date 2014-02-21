unit uButtonListPropertyEditor;

interface

uses _ButtonList, DesignEditors, DesignIntf;

type TButtonListPropertyEditor = class(TClassProperty)
  public
    function GetValue: string; override;
  end;

procedure Register;

implementation

procedure Register;
begin
RegisterPropertyEditor(TypeInfo(TBGHeader), TButtonList, 'Header', TButtonListPropertyEditor)
end;

{ TButtonListPropertyEditor }

function TButtonListPropertyEditor.GetValue: string;
begin
Result:='(TBGHeader)'
end;

end.