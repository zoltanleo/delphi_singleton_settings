object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 182
  ClientWidth = 315
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = ActSaveIniMemExecute
  TextHeight = 15
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 297
    Height = 129
    Caption = 'RadioGroup1'
    ItemIndex = 1
    Items.Strings = (
      'Registry mode selected'
      'Ini file mode selected'
      'Memory based Ini file mode selected')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 149
    Width = 145
    Height = 25
    Caption = 'call Form2'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 159
    Top = 149
    Width = 148
    Height = 25
    Caption = 'call Form3'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ActionList1: TActionList
    Left = 184
    Top = 32
    object ActLoadProperty: TAction
      Caption = 'ActLoadProperty'
      OnExecute = ActLoadPropertyExecute
    end
    object ActSaveProperty: TAction
      Caption = 'ActSaveProperty'
      OnExecute = ActSavePropertyExecute
    end
    object ActLoadIniMem: TAction
      Caption = 'ActLoadIniMem'
      OnExecute = ActLoadIniMemExecute
    end
    object ActSaveIniMem: TAction
      Caption = 'ActSaveIniMem'
      OnExecute = ActSaveIniMemExecute
    end
  end
end
