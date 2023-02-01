object FormTableSelect: TFormTableSelect
  Left = 122
  Top = 139
  Width = 352
  Height = 237
  Caption = 'Database Tables ...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 248
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 248
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 1
    Width = 233
    Height = 184
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 81
      Height = 13
      Caption = 'Select a Table ...'
    end
    object ListBox1: TListBox
      Left = 6
      Top = 24
      Width = 217
      Height = 153
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Button3: TButton
    Left = 248
    Top = 160
    Width = 75
    Height = 25
    Caption = 'New Table'
    TabOrder = 3
    OnClick = Button3Click
  end
end
