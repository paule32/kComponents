object FormTableSchema: TFormTableSchema
  Left = 109
  Top = 109
  Width = 541
  Height = 253
  Caption = 'Table Schema ...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 5
    Width = 417
    Height = 193
    Caption = 'Table Schema: '
    TabOrder = 0
    object Label1: TLabel
      Left = 288
      Top = 16
      Width = 54
      Height = 13
      Caption = 'Field name:'
    end
    object Label2: TLabel
      Left = 288
      Top = 56
      Width = 48
      Height = 13
      Caption = 'Field type:'
    end
    object Label3: TLabel
      Left = 288
      Top = 138
      Width = 59
      Height = 13
      Caption = 'Table name:'
    end
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 35
      Height = 13
      Caption = 'Tables:'
    end
    object Label5: TLabel
      Left = 152
      Top = 16
      Width = 30
      Height = 13
      Caption = 'Fields:'
    end
    object Label6: TLabel
      Left = 288
      Top = 96
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object ListBox1: TListBox
      Left = 8
      Top = 32
      Width = 121
      Height = 145
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object ListBox2: TListBox
      Left = 152
      Top = 32
      Width = 121
      Height = 145
      ItemHeight = 13
      TabOrder = 1
      OnClick = ListBox2Click
    end
    object Edit1: TEdit
      Left = 288
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 2
      OnKeyPress = Edit1KeyPress
    end
    object Edit2: TEdit
      Left = 288
      Top = 152
      Width = 121
      Height = 21
      TabOrder = 4
      OnChange = Edit2Change
    end
    object ComboBox1: TComboBox
      Left = 288
      Top = 72
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = ComboBox1Change
      OnClick = ComboBox1Click
      OnKeyPress = ComboBox1KeyPress
      Items.Strings = (
        'INTEGER'
        'FLOAT'
        'TEXT')
    end
    object Edit3: TEdit
      Left = 288
      Top = 112
      Width = 121
      Height = 21
      TabOrder = 5
    end
  end
  object Button1: TButton
    Left = 432
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add Field'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 432
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Delete Field'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 432
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Add Table'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 433
    Top = 172
    Width = 75
    Height = 25
    Caption = 'Delete Table'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button6: TButton
    Left = 432
    Top = 97
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 5
    OnClick = Button6Click
  end
end
