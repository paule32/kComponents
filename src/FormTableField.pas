// -------------------------------------------------------------------------
// File: TableField.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit FormTableField;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TTableField = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    DataBase : String;
    DataTable: String;
    DataField: String;
  end;

var
  TableField: TTableField;

implementation

{$R *.dfm}

procedure TTableField.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TTableField.Button1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
  begin
    DataField := ListBox1.Items[ListBox1.ItemIndex];
    Close;
  end;
end;

end.
