object Form2: TForm2
  Left = 500
  Top = 245
  Width = 447
  Height = 349
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object kDataGrid1: TkDataGrid
    Left = 72
    Top = 48
    Width = 200
    Height = 201
    Headers = <
      item
        Color = clOlive
        Title = 'Tutzu'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Width = 0
      end>
  end
  object kDataSource1: TkDataSource
    EditDataBase = 'E:\Projekte\DelphiComponents\out\projects.db'
    Fields = <>
    Left = 16
    Top = 16
  end
end
