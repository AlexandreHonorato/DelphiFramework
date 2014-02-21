unit _TreeView_VCL;

interface

uses ComCtrls;

procedure SetNodeImageIndex(ANode: TTreeNode; AImageIndex: Integer);

implementation

procedure SetNodeImageIndex(ANode: TTreeNode; AImageIndex: Integer);
begin
ANode.ImageIndex:=AImageIndex;
ANode.SelectedIndex:=AImageIndex;
end;

end.