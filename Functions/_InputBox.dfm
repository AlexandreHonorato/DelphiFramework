object frmInputBox: TfrmInputBox
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'frmInputBox'
  ClientHeight = 96
  ClientWidth = 408
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
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'lblPrompt'
  end
  object btnOK: TButton
    Left = 224
    Top = 56
    Width = 81
    Height = 25
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 312
    Top = 56
    Width = 81
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
  end
  object txtValue: TEdit
    Left = 8
    Top = 24
    Width = 385
    Height = 21
    TabOrder = 2
    OnChange = CheckBtnOK
  end
end
