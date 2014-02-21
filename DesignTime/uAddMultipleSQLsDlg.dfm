object frmAddMultipleSQLs: TfrmAddMultipleSQLs
  Left = 192
  Top = 107
  Width = 367
  Height = 307
  BorderIcons = [biSystemMenu]
  Caption = 'Add multiple SQLs'
  Color = clBtnFace
  Constraints.MinHeight = 307
  Constraints.MinWidth = 367
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
    Left = 8
    Top = 13
    Width = 39
    Height = 13
    Caption = 'Start ID:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 93
    Height = 13
    Caption = 'SQLs (one per line):'
  end
  object btnOK: TButton
    Left = 184
    Top = 248
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 272
    Top = 248
    Width = 81
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object mmSQLs: TMemo
    Left = 8
    Top = 56
    Width = 345
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
    OnChange = CheckBtnOK
  end
  object seStartID: TSpinEdit
    Left = 56
    Top = 8
    Width = 49
    Height = 22
    MaxValue = 999999
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
end
