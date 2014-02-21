object frmClassTree: TfrmClassTree
  Left = 898
  Top = 182
  Width = 297
  Height = 707
  Caption = 'frmClassTree'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = frmClassTreeDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 581
    Width = 289
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object tvClasses: TTreeView
    Left = 0
    Top = 41
    Width = 289
    Height = 540
    Align = alClient
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
  end
  object mmErrors: TMemo
    Left = 0
    Top = 584
    Width = 289
    Height = 96
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object btnFullRebuild: TButton
      Left = 8
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Rebuild'
      TabOrder = 0
      OnClick = btnFullRebuildClick
    end
  end
end
