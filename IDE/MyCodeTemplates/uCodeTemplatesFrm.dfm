object frmCodeTemplates: TfrmCodeTemplates
  Left = 102
  Top = 115
  Caption = 'Templates'
  ClientHeight = 510
  ClientWidth = 717
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001001800680300001600000028000000100000002000
    0000010018000000000040030000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000AA7C29B68124B57F20B47F1FB47F1F
    B47F1FB47F1FB47F1FB47F1FB47F1FB57F20B68124AA7C290000000000000000
    00B68124FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFB68124000000000000000000B57F20FFFFFFE6D6AEE6D7B0E7D7B1
    E7D7B1E7D7B1E7D7B1E7D7B1E6D7B0E6D6AEFFFFFFB57F200000000000000000
    00B47E1FFFFFFFE6D6AFE7D7B2E7D8B3E7D8B3E7D8B3E7D8B3E7D8B3E7D7B2E6
    D6AFFFFFFFB47E1F000000000000000000B47E1EFFFFFFE6D5ADE6D6AFE6D7B0
    E6D7B0E6D7B0E6D7B0E6D7B0E6D6AFE6D5ADFFFFFFB47E1E0000000000000000
    00B47E1EFFFFFFF8F4E7F8F4E8F8F4E9F8F4E9F8F4E9F8F4E9F8F4E9F8F4E8F8
    F4E7FFFFFFB47E1E000000000000000000B47E1EFFFFFFF4EDDDF4EDDDF4EDDE
    F4EDDEF4EDDEF4EDDEF4EDDEF4EDDDF4EDDDFFFFFFB47E1E0000000000000000
    00B47E1EFFFFFFF3EAD6F3EAD7F3EAD7F3EAD7F3EAD7F3EAD7F3EAD6F3E9D5F3
    E9D5FFFFFFB47E1E000000000000000000B47E1EFFFFFFF1E6CEF1E7CFF1E7D0
    F1E7D0F1E7CFF0E6CEF0E5CDF0E5CDF0E5CDFFFFFFB47E1E0000000000000000
    00B47E1EFFFFFFEEE3C8EFE4C9EFE4CAEFE4CAEEE3C8F6F0E1FFFFFFFFFFFFFF
    FFFFFFFFFFB47F1E000000000000000000B47E1EFFFFFFECE0C1EDE1C3EDE1C4
    EDE1C3ECDFC1FFFFFFCFAC70AD720AAC7006FFFFFFB580220000000000000000
    00B47E1EFFFFFFEADCB9EADDBCEBDDBDEADDBCE9DCB9FFFFFFAD720AFDFCF6FF
    FFFFEBDCC3B78327000000000000000000B57F1FFFFFFFE7D8B1E7D8B2E7D9B3
    E7D8B2E7D8B0FFFFFFAC7107FFFFFFEADABFB57F200000000000000000000000
    00B68124FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBDCC3B5
    7F20000000000000000000000000000000B9872DB68124B57F1FB47E1EB47E1E
    B47E1EB47E1EB47F1FB58022B78327000000000000000000000000000000FFFF
    0000800300008003000080030000800300008003000080030000800300008003
    00008003000080030000800300008003000080070000800F0000801F0000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = Form1Create
  OnDestroy = Form1Destroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 241
    Top = 0
    Height = 510
  end
  object pnlTemplate: TPanel
    Left = 244
    Top = 0
    Width = 473
    Height = 510
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = pnlTemplateResize
    object splTemplate: TSplitter
      Left = 217
      Top = 23
      Height = 487
      Color = clMedGray
      ParentColor = False
    end
    object lblTemplate: TLabel
      Left = 0
      Top = 0
      Width = 473
      Height = 23
      Align = alTop
      AutoSize = False
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
    end
    object pnlVars: TPanel
      Left = 0
      Top = 23
      Width = 217
      Height = 487
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = pnlVarsResize
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 241
    Height = 510
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 241
      Height = 23
      Align = alTop
      AutoSize = False
      Caption = ' All templates:'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
    end
    object tvMain: TTreeView
      Left = 0
      Top = 23
      Width = 241
      Height = 487
      Align = alClient
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnDblClick = tvMainDblClick
    end
  end
end
