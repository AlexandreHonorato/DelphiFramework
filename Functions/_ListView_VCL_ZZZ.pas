unit _ListView_VCL_ZZZ;

interface

uses ComCtrls, FlatSB, Windows;

function HorzScrollPos(LV: TCustomListView): Integer;
procedure ListItemInvalidate(LI: TListItem);
procedure ShowItem2(LV: TListView; Index: Integer); overload; // для Owner-Data ListView - LI.Parent.Parent не работает, соответственно, не работает ShowItem
procedure ShowItem2(LV: TListView; LI: TListItem); overload;
procedure ShowItem(LI: TListItem);
function LVAddItem(LV: TListView; const ACaption: String; AImageIndex: Integer; AData: Pointer; ASubItems: array of String): TListItem;

function AddColumn(AListView: TListView; const ACaption: String; AWidth: Integer): TListColumn; overload;
function AddColumn(AColumns: TListColumns; const ACaption: String; AWidth: Integer): TListColumn; overload;

implementation

function LVAddItem(LV: TListView; const ACaption: String; AImageIndex: Integer; AData: Pointer; ASubItems: array of String): TListItem;
var I: Integer;
begin
Result:=LV.Items.Add;
Result.Caption:=ACaption;
Result.ImageIndex:=AImageIndex;
Result.Data:=AData;
for i:=Low(ASubItems) to High(ASubItems) do Result.SubItems.Add(ASubItems[i])
end;

function HorzScrollPos(LV: TCustomListView): Integer;
begin
Result:=FlatSB_GetScrollPos(LV.Handle, SB_HORZ);
end;

procedure ListItemInvalidate(LI: TListItem);
var R: TRect;
begin
R:=LI.DisplayRect(drBounds);
InvalidateRect(LI.Owner.Owner.Handle, @R, TRUE);
end;

procedure ShowItem2(LV: TListView; Index: Integer);
begin
LV.Selected:=NIL;
LV.Selected:=LV.Items[Index];
LV.Selected.Focused:=True;
LV.Selected.MakeVisible(True);
//ShowItem2(LV, LV.Items[Index]) - а вот это для ODListView уже не работает. хз почему
end;

procedure ShowItem2(LV: TListView; LI: TListItem);
begin
LV.Selected:=NIL;
LV.Selected:=LI;
LV.Selected.Focused:=True;
LV.Selected.MakeVisible(True);
end;

procedure ShowItem(LI: TListItem);
begin
ShowItem2(TListView(LI.Owner.Owner), LI);
end;

function AddColumn(AListView: TListView; const ACaption: String; AWidth: Integer): TListColumn;
begin
Result := AListView.Columns.Add;
Result.Caption := ACaption;
Result.Width := AWidth;
end;

function AddColumn(AColumns: TListColumns; const ACaption: String; AWidth: Integer): TListColumn; overload;
begin
Result := AColumns.Add;
Result.Caption := ACaption;
Result.Width := AWidth;
end;

end.