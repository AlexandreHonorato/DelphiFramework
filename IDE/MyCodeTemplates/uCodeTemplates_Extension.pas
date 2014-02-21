unit uCodeTemplates_Extension;

interface

uses
  ToolsAPI, Menus, Classes, Windows, Controls, _ToolsAPI;

type
  TMyIDEExtension = class(TNotifierObject, IUnknown, IOTANotifier, IOTAKeyboardBinding)
  private
    procedure KB_TemplateMenu(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure MarkWordBeforeCursor(const Context: IOTAKeyContext);
  private
  public
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;

procedure Register;

implementation

uses uCodeTemplates_L3, uCodeTemplates_L2;

procedure Register;
begin
(BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TMyIDEExtension.Create);
end;

{ TShortCut }

procedure TMyIDEExtension.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
BindingServices.AddKeyBinding([ShortCut(VK_SPACE, [ssShift])],         KB_TemplateMenu, nil);
end;

function TMyIDEExtension.GetBindingType: TBindingType;
begin
Result:=btPartial
end;

function TMyIDEExtension.GetDisplayName: string;
begin
Result:='My IDE Extension'
end;

function TMyIDEExtension.GetName: string;
begin
Result:='My IDE Extension'
end;

procedure TMyIDEExtension.KB_TemplateMenu(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
var EV: IOTAEditView; P: TPoint; EB: IOTAEditBlock;
  Text: String; Idx: Integer; Template: TCTTemplate;
begin
if not Assigned(CT_ML) then Init_CT_ML;

EV:=Context.EditBuffer.TopView;
EB:=Context.EditBuffer.EditBlock;
Text:=EB.Text;
if Text='' then
  begin
  MarkWordBeforeCursor(Context);
  Text:=EB.Text;
  end;

Idx:=CT_BLL.ShortCuts.IndexOf(Text);
if Idx<>-1 then
  begin
  Template:=TCTTemplate(CT_BLL.ShortCuts.Objects[Idx]);
  Template.ResetVars(NIL);
  if Template.VarsDefined then // по идее всегда должно возвращать True, предполагается, что соотв. шаблоны имеют такую структуру (без переменных и блоков)
    begin
    // пока что просто считаем, что если у шаблона есть ShortCut, то у него нет var'ов и только один блок
    // Поэтому просто вставляем код шаблона и на этом успокаиваемся, не показывая форму Templates
    // (или не показывая шаблон, если форма уже открыта) - это дает еще дополнительный плюс,
    // заключающийся в том, что активируя шаблон с ShortCut'ом, мы не теряем данные о
    // BLL.CurrentTemplate
    CT_ML.InsertCompiledLines(EV, Template.Blocks[0]);
(*    if (Template.BlockCount>1) or (CT_BLL.CurrTemplate<>NIL {== frmCodeTemplates.IsVisible}) then
      begin
      CT_ML.ShowTemplate(Template, Lst);
      EV.GetEditWindow.Form.BringToFront;
      end;
    exit;     *)
    end;
  end
else
  begin
  P:=taGetPointForSelection(Context.EditBuffer, False);
  CT_ML.TrackTemplatesMenu(Text, P);
  end;

BindingResult:=krHandled
end;

procedure TMyIDEExtension.MarkWordBeforeCursor(const Context: IOTAKeyContext);
var EP: IOTAEditPosition; EB: IOTAEditBlock; B: Boolean;
begin
EP:=Context.EditBuffer.EditPosition;
EB:=Context.EditBuffer.EditBlock;

if EB.Text<>'' then exit;
if EP.Column=1 then exit;

if not EP.IsWordCharacter then
  begin
  EP.MoveRelative(0, -1);
  B:=EP.IsWordCharacter;
  EP.MoveRelative(0, 1);
  if not B then exit;
  end;

EP.MoveCursor(mmSkipLeft or mmSkipWord);

EB.Reset;
EB.Style:=btNonInclusive;
EB.BeginBlock;
EP.MoveCursor(mmSkipRight or mmSkipWord);
EB.EndBlock;
end;

initialization

finalization
if Assigned(CT_ML) then Done_CT_ML;

end.
