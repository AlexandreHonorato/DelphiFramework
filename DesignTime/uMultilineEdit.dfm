object frmMultilineEdit: TfrmMultilineEdit
  Left = 192
  Top = 107
  ActiveControl = Memo1
  BorderStyle = bsDialog
  Caption = 'Multiline Text'
  ClientHeight = 119
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 184
    Top = 88
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 272
    Top = 88
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 345
    Height = 73
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
