object dlgFrameWorkUnits: TdlgFrameWorkUnits
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Framework units'
  ClientHeight = 340
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = dlgFrameWorkUnitsCreate
  OnDestroy = dlgFrameWorkUnitsDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = 'Framework units:'
  end
  object lblCount: TLabel
    Left = 8
    Top = 320
    Width = 38
    Height = 13
    Caption = 'lblCount'
  end
  object btnOK: TButton
    Left = 448
    Top = 256
    Width = 145
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 448
    Top = 288
    Width = 145
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object lvUnits: TListView
    Left = 8
    Top = 24
    Width = 425
    Height = 289
    Columns = <
      item
        Caption = 'Unit'
        Width = 150
      end
      item
        Caption = 'Path'
        Width = 250
      end>
    HideSelection = False
    MultiSelect = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnCustomDrawItem = lvUnitsCustomDrawItem
    OnCustomDrawSubItem = lvUnitsCustomDrawSubItem
    OnData = lvUnitsData
  end
  object btnDeleteInvalid: TButton
    Left = 448
    Top = 64
    Width = 145
    Height = 25
    Caption = 'Delete invalid filenames'
    TabOrder = 3
    OnClick = btnDeleteInvalidClick
  end
  object btnDeleteSelected: TButton
    Left = 448
    Top = 96
    Width = 145
    Height = 25
    Caption = 'Delete selected'
    TabOrder = 4
    OnClick = btnDeleteSelectedClick
  end
  object btnAdd: TButton
    Left = 448
    Top = 24
    Width = 145
    Height = 25
    Caption = 'Add...'
    TabOrder = 5
    OnClick = btnAddClick
  end
  object dlgSelect: TOpenDialog
    DefaultExt = 'pas'
    Filter = '*.pas|*.pas|*.*|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 488
    Top = 192
  end
end
