unit uComponentsMenu;

interface

{$I ..\..\CompilerVer.inc}

uses SysUtils, Windows, Classes, Forms, ToolsAPI, Menus, TypInfo, Controls, StdCtrls, ExtCtrls, ComCtrls,
  DesignEditors, DesignIntf, Graphics, _ToolsAPI, _Misc, _Strings;

const FormEventsCount = 2;
const FormEventCaptions: array[0..FormEventsCount-1] of String = ('OnCreate (&C)', 'OnDestroy (&D)');
const AlignHotKeys: array[alNone..alClient] of String = (' (&N)', ' (&T)', ' (&B)', ' (&L)', ' (&R)', ' (&C)');

type TMainMenu_Components = class(TToolsAPIMenu)
  private
    mnuComponents: TMenuItem;
    mnuFormEvents: array[0..FormEventsCount-1] of TMenuItem;
    mnuSetProps: TMenuItem;
    mnuSelectForm: TMenuItem;
    mnuAdjustForm: TMenuItem;

    mnuAlignRoot: TMenuItem;
    mnuAligns: array[alNone..alClient] of TMenuItem;

    procedure mnuAlignClick(Sender: TObject);
    procedure mnuFormEventClick(Sender: TObject);
    procedure mnuSetPropsClick(Sender: TObject);
    procedure mnuSelectFormClick(Sender: TObject);
    procedure mnuAdjustFormClick(Sender: TObject);
  protected
    procedure InitMenu; override;
    procedure DoneMenu; override;
  end;

implementation

uses uUnits;

{ TMainMenu_Components }

procedure TMainMenu_Components.DoneMenu;
var I: Integer; A: TAlign;
begin
FMenuItem.Remove(mnuComponents);
for i:=0 to FormEventsCount-1 do mnuFormEvents[i].Free;
mnuSetProps.Free;
mnuSelectForm.Free;
mnuAdjustForm.Free;

for A:=alNone to alClient do mnuAligns[A].Free;
mnuAlignRoot.Free;

mnuComponents.Free;
end;

procedure TMainMenu_Components.InitMenu;
var I: Integer; A: TAlign;
begin
mnuComponents:=NewItem('Components (&Z)', 0, False, True, NIL, 0, 'mnuComponents');

for i:=0 to FormEventsCount-1 do
	begin
  mnuFormEvents[i]:=NewItem('Form '+FormEventCaptions[i], 0, False, True, mnuFormEventClick, 0, 'mnuFormEvent'+IntToStr(i));
  mnuFormEvents[i].Tag:=i;
  mnuComponents.Add(mnuFormEvents[i]);
	end;

mnuSelectForm:=NewItem('&Select Form', 0, False, True, mnuSelectFormClick, 0, 'mnuSelectForm');
mnuComponents.Add(mnuSelectForm);

mnuAdjustForm:=NewItem('Adjust Form', 0, False, True, mnuAdjustFormClick, 0, 'mnuAdjustForm');
mnuComponents.Add(mnuAdjustForm);

mnuComponents.NewBottomLine;

mnuSetProps:=NewItem('Set &Properties', 0, False, True, mnuSetPropsClick, 0, 'mnuSetProps');
mnuComponents.Add(mnuSetProps);

mnuComponents.NewBottomLine;

mnuAlignRoot:=NewItem('&Align', 0, False, True, NIL, 0, 'mnuAlignRoot');
for A:=alNone to alClient do
  begin
  I:=Integer(A);
  mnuAligns[A]:=NewItem(GetEnumName(TypeInfo(TAlign), I)+AlignHotKeys[A], 0, False, True, mnuAlignClick, 0, 'mnuAligns'+IntToStr(I));
  mnuAligns[A].Tag:=I;
  mnuAlignRoot.Add(mnuAligns[A]);
  end;
mnuComponents.Add(mnuAlignRoot);

FMenuItem.Add(mnuComponents);
end;

procedure TMainMenu_Components.mnuAdjustFormClick(Sender: TObject);
var Designer: IDesigner;
  RootComponent: TComponent; I: Integer; W, H: Integer; C: TControl;
begin
Designer:=taGetCurrentDesigner;
if Designer=NIL then exit;

RootComponent:=Designer.GetRoot;
W:=0; H:=0;
for i:=0 to RootComponent.ComponentCount-1 do
  begin
  if not (RootComponent.Components[i] is TControl) then continue;
  C:=TControl(RootComponent.Components[i]);
  if (C.Parent<>RootComponent) or (C.Align<>alNone) then continue;
  W:=Max(W, C.Left+C.Width);
  H:=Max(H, C.Top+C.Height);
  end;

if W=0 then exit;
W:=W+7;
H:=H+7;
with TForm(RootComponent) do
  begin
  ClientWidth:=W;
  ClientHeight:=H;

  ClientWidth:=W;  // нужно присваивать дважды, с первого раза почему-то иногда не срабатывает
  ClientHeight:=H;  
  end;
end;

procedure TMainMenu_Components.mnuAlignClick(Sender: TObject);
var Sel: IDesignerSelections; Designer: IDesigner; C: TComponent;
begin
Designer:=taGetCurrentDesigner;
if Designer=NIL then exit;
Sel:=CreateSelectionList;
Designer.GetSelections(Sel);
if Sel.Count=1 then
  begin
  C:=TComponent(Sel.Items[0]);
  if IsPublishedProp(C, 'Align') and PropIsType(C, 'Align', tkEnumeration) then
    begin
    SetPropValue(C, 'Align', TComponent(Sender).Tag);
    Designer.Modified;
    end;
  end;
Sel:=NIL;
end;

procedure TMainMenu_Components.mnuFormEventClick(Sender: TObject);
const FormEvents: array[0..FormEventsCount-1] of String = ('OnCreate', 'OnDestroy');
var I: Integer;
  PropCount: Integer; Props: PPropList; PropInfo: PPropInfo; Method: TMethod;
  Frm: TComponent; S, MethodName, PropName: String;
  Designer: IDesigner;
begin
Designer:=taGetCurrentDesigner;
if Designer=NIL then exit;

Frm:=Designer.GetRoot;
S:=FormEvents[TComponent(Sender).Tag];
PropCount:=GetPropList(Frm.ClassInfo, [tkMethod], nil);
GetMem(Props, PropCount*SizeOf(PPropInfo));
try
  GetPropList(Frm.ClassInfo, [tkMethod], Props);
  for I:=PropCount-1 downto 0 do
    begin
    PropInfo:=Props[I];
    {$IFDEF DELPHI_XE}
    PropName:=String(PropInfo.Name); // to avoid compiler message W1057
    {$ELSE}
    PropName:=PropInfo.Name;
    {$ENDIF}
    if TC(PropName, S) then
      begin
      Method:=GetMethodProp(Frm, PropInfo);
      MethodName:=Designer.GetMethodName(Method);
      if MethodName='' then
        begin
        Delete(S, 1, 2);
        MethodName:=Frm.Name+S;
        Method:=Designer.CreateMethod(MethodName, GetTypeData(PropInfo.PropType^));
        SetMethodProp(Frm, PropName, Method);
        Designer.Modified;
        end;
      if Designer.MethodExists(MethodName) then Designer.ShowMethod(MethodName);
      Break;
      end;
    end;
finally
  FreeMem(Props);
end; // try
end;

procedure TMainMenu_Components.mnuSelectFormClick(Sender: TObject);
var Designer: IDesigner;
  Sel: IDesignerSelections;
begin
Designer:=taGetCurrentDesigner;
if Designer=NIL then exit;

Sel:=CreateSelectionList;
Sel.Add(Designer.GetRoot);
Designer.SetSelections(Sel);
Sel:=NIL;
end;

procedure TMainMenu_Components.mnuSetPropsClick(Sender: TObject);
var I: Integer; Sel: IDesignerSelections;
  Designer: IDesigner; C: TComponent; B: Boolean;
begin
Designer:=taGetCurrentDesigner;
if Designer=NIL then exit;

Sel:=CreateSelectionList;
Designer.GetSelections(Sel);
B:=False;
for i:=0 to Sel.Count-1 do
  begin
  C:=TComponent(Sel.Items[i]);

  if C is TPanel then
    with TPanel(C) do
      begin
      Caption:='';
      BevelOuter:=bvNone;
      B:=True;
      end
  else
  if C is TListView then
    with TListView(C) do
      begin
      ViewStyle:=vsReport;
      HideSelection:=False;
      ReadOnly:=True;
      RowSelect:=True;
      B:=True;
      end
  else
  if C is TTreeView then
    with TTreeView(C) do
      begin
      HideSelection:=False;
      ReadOnly:=True;
      B:=True;
      end
  else
  if C is TButton then
    begin
    TButton(C).Width:=81;
    B:=True;
    end
  else
  if C is TToolBar then
    with TToolBar(C) do
      begin
      Flat:=True;
      AutoSize:=True;
      List:=True;
      ShowCaptions:=True;
      B:=True;
      end
  else
  if C is TLabel then
    with TLabel(C) do
      begin
      Font.Color:=clCaptionText;
      Font.Size:=12;
      Font.Name:='Arial';
      Font.Style:=[fsBold];
      Color:=clActiveCaption;
      B:=True;
      end
  else
  if C is TMemo then
    with TMemo(C) do
      begin
      ScrollBars:=ssBoth;
      WordWrap:=False;
      Lines.Clear;
      B:=True;
      end;
  end;
if B then Designer.Modified;
Sel:=NIL;
end;

end.
