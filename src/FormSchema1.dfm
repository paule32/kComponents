object Form1: TForm1
  Left = 81
  Top = 126
  Width = 531
  Height = 280
  Caption = 'Form1'
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
    Width = 409
    Height = 209
    Caption = 'GroupBox1'
    TabOrder = 0
    object kListBox1: TkListBox
      Left = 8
      Top = 32
      Width = 121
      Height = 153
      ItemHeight = 13
      TabOrder = 0
      ColorEnter = clYellow
      ColorExit = clWhite
    end
    object kListBox2: TkListBox
      Left = 144
      Top = 32
      Width = 121
      Height = 153
      ItemHeight = 13
      TabOrder = 1
      ColorEnter = clYellow
      ColorExit = clWhite
    end
    object EditField1: TEditField
      Left = 280
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'EditField1'
      EditOptions = []
      EditType = efNormal
    end
    object EditField2: TEditField
      Left = 280
      Top = 160
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'EditField2'
      EditOptions = []
      EditType = efNormal
    end
  end
  object kComboBox1: TkComboBox
    Left = 288
    Top = 80
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'kComboBox1'
    ColorEnter = clYellow
    ColorExit = clWhite
  end
end
