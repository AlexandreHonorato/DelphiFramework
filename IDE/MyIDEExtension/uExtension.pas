unit uExtension;

interface

uses SysUtils, Classes, Windows, Menus, ToolsAPI, uEditorOperations, Forms, uIDEOperations, uClasses,
  uCodeEntities, _MessageBus, _ToolsAPI;

type TMyIDEExtensionNew = class(TNotifierObject, IOTAKeyboardBinding)
  private
    procedure KB_IncIndent(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_DecIndent(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_CopyWord(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_CutWord(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_PasteWord(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_AddBeginEnd(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_AddBeginEndWithIndent(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_UseUnit(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_UseUnitPrev(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_OpenUnit(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_MaximizeApp(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_Comment(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_GotoCodeObject(const Context: IOTAKeyContext; KeyCode: TShortCut;
        var BindingResult: TKeyBindingResult);
    procedure KB_CloseUnit(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_AddToDo(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_RunConfig(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_Search(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_SearchIncremental(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_ShowClassContextMenu(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_GotoImplementationLine(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_InsertType(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure KB_GotoParentClass(const Context: IOTAKeyContext; KeyCode: TShortCut;
          var BindingResult: TKeyBindingResult);
    procedure KB_GotoDescendantClass(const Context: IOTAKeyContext; KeyCode: TShortCut;
          var BindingResult: TKeyBindingResult);
  protected
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  public
    constructor Create; 
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses uUnits, uUI, uDOMAIN, uRunConfigDlg;

procedure Register;
begin
(BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TMyIDEExtensionNew.Create);
end;

{ TMyIDEExtension }

procedure TMyIDEExtensionNew.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
BindingServices.AddKeyBinding([ShortCut(VK_RIGHT, [ssAlt])], KB_IncIndent, nil);
BindingServices.AddKeyBinding([ShortCut(VK_LEFT, [ssAlt])], KB_DecIndent, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('C'), [ssCtrl])], KB_CopyWord, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('X'), [ssCtrl])], KB_CutWord, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('V'), [ssCtrl])], KB_PasteWord, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('B'), [ssCtrl])], KB_AddBeginEnd, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('B'), [ssCtrl, ssShift])], KB_AddBeginEndWithIndent, nil);

BindingServices.AddKeyBinding([ShortCut(Ord('U'), [ssAlt])], KB_UseUnit, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('U'), [ssAlt, ssCtrl])], KB_UseUnitPrev, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('I'), [ssAlt, ssCtrl])], KB_GotoImplementationLine, nil);

BindingServices.AddKeyBinding([ShortCut(VK_F2, [ssAlt])], KB_OpenUnit, nil);
BindingServices.AddKeyBinding([ShortCut(VK_F2, [ssShift])], KB_CloseUnit, nil);
BindingServices.AddKeyBinding([ShortCut(VK_F2, [])], KB_GotoCodeObject, nil);
BindingServices.AddKeyBinding([ShortCut(VK_UP, [ssAlt])], KB_MaximizeApp, nil);
BindingServices.AddKeyBinding([ShortCut(VK_DOWN, [ssAlt])], KB_ShowClassContextMenu, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('T'), [ssCtrl])], KB_AddToDo, nil);

BindingServices.AddKeyBinding([ShortCut(Ord('F'), [ssAlt, ssCtrl])], KB_Search, nil);
BindingServices.AddKeyBinding([ShortCut(Ord('E'), [ssAlt, ssCtrl])], KB_SearchIncremental, nil);

BindingServices.AddKeyBinding([ShortCut(VK_F6, [])], KB_Comment, nil);
BindingServices.AddKeyBinding([ShortCut(VK_F1, [])], KB_InsertType, nil);

BindingServices.AddKeyBinding([ShortCut(VK_UP, [ssCtrl])], KB_GotoParentClass, nil);
BindingServices.AddKeyBinding([ShortCut(VK_DOWN, [ssCtrl])], KB_GotoDescendantClass, nil);

BindingServices.AddKeyBinding([ShortCut(VK_F9, [ssAlt])], KB_RunConfig, nil);
end;

constructor TMyIDEExtensionNew.Create;
begin
inherited Create;
mbExtension:=CreateMessageBus;
DOMAIN:=TDOMAIN.Create;
UI:=TUI.Create;
end;

destructor TMyIDEExtensionNew.Destroy;
begin
if Assigned(UI) then FreeAndNIL(UI);
if Assigned(DOMAIN) then FreeAndNIL(DOMAIN);
FreeMessageBus(mbExtension);
inherited Destroy;
end;

function TMyIDEExtensionNew.GetBindingType: TBindingType;
begin
Result:=btPartial
end;

function TMyIDEExtensionNew.GetDisplayName: string;
begin
Result:='My IDE Extension (new)';
end;

function TMyIDEExtensionNew.GetName: string;
begin
Result:='My IDE Extension (new)';
end;

procedure TMyIDEExtensionNew.KB_AddBeginEnd(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var EV: IOTAEditView;
begin
EV:=Context.EditBuffer.TopView;
if Assigned(EV) then SurroundWithBeginEnd(EV);

BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_AddBeginEndWithIndent(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var EV: IOTAEditView;
begin
EV:=Context.EditBuffer.TopView;
if Assigned(EV) then SurroundWithBeginEndIndent(EV);

BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_AddToDo(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
AddToDoWithDlg(Context.EditBuffer.TopView);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_CloseUnit(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
CloseUnitsWithDlg;
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_Comment(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var EV: IOTAEditView;
begin
EV:=Context.EditBuffer.TopView;
if Assigned(EV) then CommentBlockIndent(EV);

BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_RunConfig(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult:
    TKeyBindingResult);
var EV: IOTAEditView;
begin
EV:=Context.EditBuffer.TopView;
if Assigned(EV) then RunConfig(EV);

BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_CopyWord(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
CopyWord(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_CutWord(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
CutWord(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_DecIndent(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var EV: IOTAEditView;
begin
EV:=Context.EditBuffer.TopView;
if Assigned(EV) then DecBlockIndent(EV);

BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_GotoCodeObject(const Context: IOTAKeyContext;
    KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
begin
GotoCodeObjectWithDlg;
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_GotoDescendantClass(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
GotoDescendantClass(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_GotoImplementationLine(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
GotoImplementationLine(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_GotoParentClass(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
GotoParentClass(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_IncIndent(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var EV: IOTAEditView;
begin
EV:=Context.EditBuffer.TopView;
if Assigned(EV) then IncBlockIndent(EV);

BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_InsertType(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
begin
InsertTypeWithDlg(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_MaximizeApp(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
MaximizeApp;
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_OpenUnit(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
OpenUnitsWithDlg;
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_PasteWord(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
PasteWord(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_Search(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
SearchFromStartOfFile(Context.EditBuffer, False);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_SearchIncremental(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
SearchFromStartOfFile(Context.EditBuffer, True);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_ShowClassContextMenu(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
ShowTokenContextMenu(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_UseUnit(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
UseUnitWithDlg(Context.EditBuffer);
BindingResult:=krHandled;
end;

procedure TMyIDEExtensionNew.KB_UseUnitPrev(const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
UseUnitPrev(Context.EditBuffer);
BindingResult:=krHandled;
end;

end.
