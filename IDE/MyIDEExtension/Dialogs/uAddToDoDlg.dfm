object frmAddToDo: TfrmAddToDo
  Left = 306
  Top = 280
  ActiveControl = txtText
  BorderStyle = bsDialog
  Caption = 'Add ToDo'
  ClientHeight = 87
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Text:'
  end
  object txtText: TEdit
    Left = 8
    Top = 24
    Width = 249
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 88
    Top = 56
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 176
    Top = 56
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
