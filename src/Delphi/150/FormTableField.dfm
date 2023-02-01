object TableField: TTableField
  Left = 65
  Top = 128
  Width = 341
  Height = 247
  Caption = 'TableField'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 217
    Height = 185
    Caption = 'GroupBox1'
    TabOrder = 0
    object ListBox1: TListBox
      Left = 8
      Top = 32
      Width = 121
      Height = 137
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 232
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 232
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 232
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 3
  end
end
