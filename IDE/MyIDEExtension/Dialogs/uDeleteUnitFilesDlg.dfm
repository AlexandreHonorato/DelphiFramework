object dlgDeleteUnitFiles: TdlgDeleteUnitFiles
  Left = 192
  Top = 107
  Width = 520
  Height = 190
  BorderIcons = [biSystemMenu]
  Caption = 'Remove unit'
  Color = clBtnFace
  Constraints.MinHeight = 190
  Constraints.MinWidth = 520
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    504
    152)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 78
    Height = 13
    Caption = 'Delete unit files?'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 24
    Height = 13
    Caption = 'Files:'
  end
  object btnOK: TButton
    Left = 328
    Top = 120
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 416
    Top = 120
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
  end
  object lbFileNames: TListBox
    Left = 8
    Top = 48
    Width = 489
    Height = 57
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
