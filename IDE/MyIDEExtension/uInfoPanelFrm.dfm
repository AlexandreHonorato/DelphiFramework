object frmInfoPanel: TfrmInfoPanel
  Left = 192
  Top = 107
  BorderStyle = bsNone
  Caption = 'frmInfoPanel'
  ClientHeight = 20
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = frmInfoPanelCreate
  OnDestroy = frmInfoPanelDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 176
    Top = 144
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 8
    Top = 3
    Width = 69
    Height = 13
    Caption = 'Last used unit:'
  end
  object lblLastUsedUnit: TLabel
    Left = 80
    Top = 3
    Width = 153
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 248
    Top = 4
    Width = 21
    Height = 13
    Caption = 'F10:'
  end
  object btnF10: TButton
    Left = 272
    Top = 0
    Width = 89
    Height = 20
    TabOrder = 0
    OnClick = btnF10Click
  end
end
