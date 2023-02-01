// -------------------------------------------------------------------------
// File: FormTableSelect.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit FormTableSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kDataSource, StdCtrls, FormSchema, SQLite3, SQLite3Wrap,
  EditFieldDesign;

type
  TFormTableSelect = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Label1: TLabel;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    PropertyEdit: TEditFieldTableNameProperty;
    DataBase: TSQLite3DataBase;
    DataName: String;
  end;

implementation

{$R *.dfm}

procedure TFormTableSelect.Button3Click(Sender: TObject);
var
  FTS: TFormTableSchema;
begin
  if ListBox1.ItemIndex < 0 then
  begin
    ShowMessage('Data Base must be selected.');
    exit;
  end;

  FTS := TFormTableSchema.Create(Application);
  try
    FTS.DataBase := DataBase;
    FTS.DataName := DataName;

    FTS.TableModified := true;
    FTS.ShowModal;
  finally
    FTS.Free;
  end;
end;

procedure TFormTableSelect.Button1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
  begin
    PropertyEdit.SetValue(ListBox1.Items[ListBox1.ItemIndex]);
    Close;
  end;
end;

end.
