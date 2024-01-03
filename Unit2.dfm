object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 209
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object CheckBox1: TCheckBox
    Left = 24
    Top = 16
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 56
    Width = 97
    Height = 17
    Caption = 'CheckBox2'
    TabOrder = 1
  end
  object ActionList1: TActionList
    Left = 72
    Top = 24
    object actLoad: TAction
      Caption = 'actLoad'
      OnExecute = actLoadExecute
    end
    object actSave: TAction
      Caption = 'actSave'
      OnExecute = actSaveExecute
    end
  end
end
