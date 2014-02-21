object dlgCodeObjects: TdlgCodeObjects
  Left = 192
  Top = 107
  Width = 688
  Height = 500
  BorderIcons = [biSystemMenu]
  Caption = 'dlgCodeObjects'
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 688
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = dlgCodeObjectsCreate
  DesignSize = (
    680
    473)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 13
    Caption = 'Code objects:'
  end
  object lblFilter: TLabel
    Left = 8
    Top = 404
    Width = 79
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Object type filter:'
  end
  object lblCount: TLabel
    Left = 8
    Top = 380
    Width = 47
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblCount'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnOK: TButton
    Left = 496
    Top = 432
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
  end
  object Button2: TButton
    Left = 584
    Top = 432
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object txtCodeObject: TVarEdit
    Left = 8
    Top = 24
    Width = 657
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ListBox = lbCodeObjects
    OnCheckVariantsComplete = txtCodeObjectCheckVariantsComplete
  end
  object lbCodeObjects: TVarListBox
    Left = 8
    Top = 45
    Width = 657
    Height = 332
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 3
    OnDblClick = lbCodeObjectsDblClick
  end
  object cmbFilter: TComboBox
    Left = 96
    Top = 400
    Width = 569
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
    OnChange = cmbFilterChange
  end
end
