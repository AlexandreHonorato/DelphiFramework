object frmSQLMgrEditor: TfrmSQLMgrEditor
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu]
  Caption = 'Edit SQLs'
  ClientHeight = 308
  ClientWidth = 495
  Color = clBtnFace
  Constraints.MinHeight = 346
  Constraints.MinWidth = 511
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = frmSQLMgrEditorCreate
  OnDestroy = frmSQLMgrEditorDestroy
  DesignSize = (
    495
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object lvMain: TListView
    Left = 8
    Top = 8
    Width = 489
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'ID'
        Tag = 1
      end
      item
        Caption = 'SQL'
        Tag = 2
        Width = 410
      end>
    GridLines = True
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    SmallImages = ImageList1
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = lvMainColumnClick
    OnDblClick = lvMainDblClick
    OnKeyDown = lvMainKeyDown
    OnResize = lvMainResize
    OnSelectItem = lvMainSelectItem
  end
  object btnAdd: TButton
    Left = 8
    Top = 232
    Width = 81
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add...'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 416
    Top = 232
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Delete'
    Enabled = False
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object btnEdit: TButton
    Left = 328
    Top = 232
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Edit...'
    Enabled = False
    TabOrder = 3
    OnClick = btnEditClick
  end
  object btnOK: TButton
    Left = 328
    Top = 288
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 416
    Top = 288
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnAddMultiple: TButton
    Left = 96
    Top = 232
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add multiple SQLs...'
    TabOrder = 6
    OnClick = btnAddMultipleClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 264
    Width = 209
    Height = 49
    Anchors = [akLeft, akBottom]
    Caption = 'Filter:'
    TabOrder = 7
    object txtFilter: TEdit
      Left = 8
      Top = 18
      Width = 121
      Height = 21
      TabOrder = 0
      OnKeyDown = txtFilterKeyDown
    end
    object btnApplyFilter: TButton
      Left = 136
      Top = 16
      Width = 65
      Height = 25
      Caption = 'Apply'
      TabOrder = 1
      OnClick = btnApplyFilterClick
    end
  end
  object ImageList1: TImageList
    Width = 1
    Left = 240
    Top = 96
  end
end
