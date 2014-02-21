object frmAddEditSQL: TfrmAddEditSQL
  Left = 192
  Top = 107
  Width = 519
  Height = 187
  ActiveControl = txtSQL
  BorderIcons = [biSystemMenu]
  Caption = 'frmAddEditSQL'
  Color = clBtnFace
  Constraints.MinHeight = 187
  Constraints.MinWidth = 519
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
  object Label1: TLabel
    Left = 19
    Top = 12
    Width = 14
    Height = 13
    Caption = 'ID:'
  end
  object Label2: TLabel
    Left = 9
    Top = 32
    Width = 24
    Height = 13
    Caption = 'SQL:'
  end
  object btnOK: TButton
    Left = 336
    Top = 128
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 424
    Top = 128
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object txtSQL: TMemo
    Left = 40
    Top = 32
    Width = 465
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = CheckBtnOK
    OnKeyDown = txtSQLKeyDown
  end
  object seID: TSpinEdit
    Left = 40
    Top = 8
    Width = 49
    Height = 22
    MaxValue = 999999
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
end
