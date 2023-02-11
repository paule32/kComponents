// -------------------------------------------------------------------------
// File: FormSchema.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit FormSchema;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, kListBox, EditField, kComboBox,
  SQLite3, SQLite3Wrap;

const MAX_FIELD_LENGTH = 128;

type
  TFormTableSchema = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Edit1: TEdit;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit3: TEdit;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure ComboBox1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Button6Click(Sender: TObject);
  private
    OldItemIndex1: Integer;
    OldItemIndex2: Integer;
    DataList : TStringList;
  public
    DataBase: TSQLite3DataBase;
    DataName: String;

    TableModified  : Boolean;
    MouseSelection : Boolean;
  end;

var
  FormTableSchema: TFormTableSchema;

implementation

{$R *.dfm}

procedure TFormTableSchema.Button1Click(Sender: TObject);
var
  i,p: Integer;
  s: String;
begin
  if ListBox1.ItemIndex < 0 then
  begin
    ShowMessage('table must be selected.');
    exit;
  end;

  if Length(Trim(Edit1.Text)) < 1 then
  begin
    ShowMessage('Error:' + #13#10 +
    'Datafield must be named.');
    exit;
  end;

  if ComboBox1.ItemIndex < 0 then
  begin
    ShowMessage('Error:' + #13#10 +
    'Field type must be defined.');
    exit;
  end;

  if ListBox2.Items.IndexOf('0 - ' + Edit1.Text) > -1 then
  begin
    ShowMessage('field cant be occur twice.');
    exit;
  end;
  if ListBox2.Items.IndexOf('1 - ' + Edit1.Text) > -1 then
  begin
    ShowMessage('field cant be occur twice.');
    exit;
  end;
  if ListBox2.Items.IndexOf('2 - ' + Edit1.Text) > -1 then
  begin
    ShowMessage('field cant be occur twice.');
    exit;
  end;

  ListBox2.Items.Add(
  IntToStr(ComboBox1.ItemIndex) +
  ' - ' +  Edit1.Text);

  TableModified := true;
end;

procedure TFormTableSchema.FormDestroy(Sender: TObject);
begin
  DataList.Clear;
  DataList.Free;

  ComboBox1.Clear;
  ComboBox1.Free;

  ListBox1.Clear;
  ListBox1.Free;

  ListBox2.Clear;
  ListBox1.Free;
end;

procedure TFormTableSchema.Button3Click(Sender: TObject);
begin
  if Length(Trim(Edit2.Text)) < 1 then
  begin
    ShowMessage('no table name given.');
    exit;
  end;

  if ListBox1.Items.IndexOf(Edit2.Text) > -1 then
  begin
    ShowMessage('Table already exists.');
    exit;
  end;

  ListBox1.Items.Add(Trim(Edit2.Text));
end;

procedure TFormTableSchema.Button4Click(Sender: TObject);
var
  s1: String;
  st: TSQLite3Statement;
begin
  if ListBox1.ItemIndex < 0 then
  begin
    ShowMessage('Table must be selected.');
    Edit2.Text := '';
    exit;
  end;

  if Application.MessageBox(
  PChar('Would you delete the selected Table ?'),
  PChar('Warning'),MB_YESNO) = ID_NO then
  begin
    exit;
  end else
  begin
    s1 := 'DROP TABLE "' + ListBox1.Items[ListBox1.ItemIndex] + '"';
    st := DataBase.Prepare(s1);
    st.Step;
    ListBox1.DeleteSelected;
  end;

  if ListBox1.Count > 0 then
  begin
    ListBox1.SetFocus;
    ListBox1.Selected[0] := true;
    Edit2.Text := ListBox1.Items[ListBox1.ItemIndex];
  end else
  begin
    Edit2.Text := '';
  end;
end;

procedure TFormTableSchema.Button2Click(Sender: TObject);
begin
  if ListBox1.Count < 1 then
  begin
    ShowMessage('no table data available.');
    exit;
  end;

  if ListBox2.Count < 1 then
  begin
    ShowMessage('no field data available.');
    exit;
  end;

  if ListBox1.ItemIndex < 0 then
  begin
    ShowMessage('Table must be selected.');
    exit;
  end;

  if ListBox2.ItemIndex < 0 then
  begin
    ShowMessage('field item must be selected.');
    exit;
  end;

  if Application.MessageBox(
  PChar('Would you really delete the Field ?'),
  PChar('Warning'),MB_YESNO) = ID_NO then
  exit;

  ListBox2.DeleteSelected;
  TableModified := true;
end;

procedure TFormTableSchema.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.Focused then
  begin
    ListBox2.Items[ListBox2.ItemIndex] :=
    IntToStr(ComboBox1.ItemIndex) + ' - ' +  Edit1.Text;
  end;
end;

procedure TFormTableSchema.ListBox1Click(Sender: TObject);
var
  sql,s1,s2,s3: String;
  st : TSQLite3Statement;
  rec: Integer;
  i  : Integer;
begin
  DataList.Clear;

  ListBox2.Items.Clear;
  sql := 'PRAGMA table_info ("' + ListBox1.Items[ListBox1.ItemIndex] + '")';
  st  := DataBase.Prepare(sql);
  while st.Step = SQLITE_ROW do
  begin
    s1 := UpperCase(st.ColumnText(1));
    s2 := UpperCase(st.ColumnText(2));

    if s2 = 'INTEGER' then s3 := '0 - ' + s1 else
    if s2 = 'REAL'    then s3 := '1 - ' + s1 else
    if s2 = 'TEXT'    then s3 := '2 - ' + s1 ;

    ListBox2.Items.Add(s3);
    DataList.Add(st.ColumnText(1));
  end;

  rec := 0;
  sql := 'SELECT ';
  for i := 0 to DataList.Count - 1 do
  begin
    if i = DataList.Count - 1 then
    sql := sql + DataList.Strings[i] + ' ' else
    sql := sql + DataList.Strings[i] + ', ';
  end;

  sql := sql + 'FROM ' + ListBox1.Items[ListBox1.ItemIndex];
  st  := DataBase.Prepare(sql);
  while st.Step = SQLITE_ROW do
  begin
    for i := 0 to DataList.Count-1 do
    begin
      DataList.Strings[i] :=
      DataList.Strings[i] + '@' + st.ColumnText(i);
    end;
    inc(rec);
    break;
  end;

  //DataList.Add(st.ColumnText(3));
  //ShowMessage('list:'+#13#10+DataList.Text);

  Edit2.Text := ListBox1.Items[ListBox1.ItemIndex];
  sql := 'SELECT "' + s1 + '" FROM ' + Edit2.Text;
//  showmessage(sql);

  st  := DataBase.Prepare(sql);
  st.Step;

  if s2 = 'TEXT' then
  Edit3.Text := st.ColumnText(1);

//  showmessage('==> ' + st.columntext(1));
end;

procedure TFormTableSchema.FormShow(Sender: TObject);
var
  s1,s2,s3,sql: String;
  st : TSQLite3Statement;
begin
  DataList := TStringList.Create;
  DataList.Clear;

  sql := 'SELECT * FROM sqlite_master WHERE type = "table"';
  DataBase.Open(DataName);
  st := DataBase.Prepare(sql);
  while st.Step = SQLITE_ROW do
  begin
    if Trim(st.ColumnText(1)) = '' then
    ListBox1.Items.Add('null') else
    ListBox1.Items.Add(st.ColumnText(1));
//    st.Step;
  end;

  TableModified  := false;
  OldItemIndex1  := 0;
  ListBox1.ItemIndex := 0;
  MouseSelection := false;

  if ListBox1.ItemIndex >= 0 then
  begin
    ListBox2.Items.Clear;
    sql := 'PRAGMA table_info ("' + ListBox1.Items[ListBox1.ItemIndex] + '")';
    st  := DataBase.Prepare(sql);
    while st.Step = SQLITE_ROW do
    begin
      s1 := UpperCase(st.ColumnText(1));
      s2 := UpperCase(st.ColumnText(2));

      if s2 = 'INTEGER' then s3 := '0 - ' + s1 else
      if s2 = 'REAL'    then s3 := '1 - ' + s1 else
      if s2 = 'TEXT'    then s3 := '2 - ' + s1 ;

      ListBox2.Items.Add(s3);
    end;
  end;
end;

procedure TFormTableSchema.ListBox2Click(Sender: TObject);
var
  s1,s2: String;
  i1,i2: Integer;
  st : TSQLite3Statement;
  rec: Integer;
begin
  if ListBox2.ItemIndex > -1 then
  begin
    s1 := Trim(Copy(ListBox2.Items[ListBox2.ItemIndex],5,MAX_FIELD_LENGTH));
    s2 := Edit1.Text;
    if CompareStr(s1,s2) = 0 then
    exit;
    Edit1.Text := s1;

    i1 := StrToInt(Trim(Copy(ListBox2.Items[ListBox2.ItemIndex],1,1)));
    i2 := ComboBox1.ItemIndex;
//    if i1 = i2 then
//    exit;
    ComboBox1.ItemIndex := i1;

    s2 := Trim(Copy(ListBox2.Items[ListBox2.ItemIndex],5,MAX_FIELD_LENGTH));
    s1 := ListBox1.Items[ListBox1.ItemIndex];
    s1 := 'SELECT ' + s2 + ' FROM ''' + s1 + '''';

//    showmessage('SEL: ' + s1);

    DataBase.Open(DataName);
    rec := 0;
    st  := DataBase.Prepare(s1);
    while st.Step = SQLITE_ROW do
    begin
      inc(rec);
      break;
    end;
    if rec > 0 then
    begin
      Edit3.Text := st.ColumnText(0);
    end;
  end;
end;

procedure TFormTableSchema.Edit2Change(Sender: TObject);
begin
  if not MouseSelection then
  begin
  end;
end;

procedure TFormTableSchema.ComboBox1KeyPress(Sender: TObject;
  var Key: Char);
begin
  MouseSelection := false;
  TableModified  := true;
end;

procedure TFormTableSchema.ComboBox1Click(Sender: TObject);
begin
  if ListBox2.ItemIndex > -1 then
  begin
    ListBox2.Items[ListBox2.ItemIndex] :=
    IntToStr(ComboBox1.ItemIndex) + ' - ' +  Edit1.Text;
  end;
end;

procedure TFormTableSchema.Button5Click(Sender: TObject);
var
  sql: String;
  tn : String;
  rec: Integer;
  st : TSQLite3Statement;
  i  : Integer;
  s  : String;
  s1 : String;
  s2 : String;
begin
//  DataBase.Close;
//  DataBase.Open(DataName);

(*
  tn  := ListBox1.Items[ListBox1.ItemIndex];
  sql := 'DROP TABLE IF EXISTS ' + tn;
  st  := DataBase.Prepare(sql);
  st.Step;
*)
(*
  sql := 'CREATE TABLE IF NOT EXISTS '    +
  LowerCase(ListBox1.Items[ListBox1.ItemIndex]) + ' (' + #13#10;

  for i := 0 to ListBox2.Count - 1 do
  begin
    s := ListBox2.Items[i];

    if Pos('0 -',s) > 0 then begin sql := sql + Copy(s,5,MAX_FIELD_LENGTH) + ' INTEGER '; end else
    if Pos('1 -',s) > 0 then begin sql := sql + Copy(s,5,MAX_FIELD_LENGTH) + ' REAL ';    end else
    if Pos('2 -',s) > 0 then begin sql := sql + Copy(s,5,MAX_FIELD_LENGTH) + ' TEXT ';    end ;

    if i < ListBox2.Count - 1 then
    sql := sql + ',' + #13#10;
  end;
  sql := sql + #13#10 + ')';

  ShowMessage(sql);
  st := DataBase.Prepare(sql);
  st.Step;

  sql := 'SELECT ' +
  Edit1.Text   + ' FROM ' +
  ListBox1.Items[ListBox1.ItemIndex];
  st  := DataBase.Prepare(sql);
  rec := 0;
  while st.Step = SQLITE_ROW do
  begin
    inc(rec);
    break;
  end;

  if rec > 0 then
  begin
    sql := 'UPDATE ' +
    ListBox1.Items[ListBox1.ItemIndex] + #13#10 +
    'SET ' + Edit1.Text +  ' = ''' + Edit3.Text + ''' ';
  end else
  begin
    sql := 'INSERT INTO ' +
    ListBox1.Items[ListBox1.ItemIndex] + ' (' + Copy(
    ListBox2.Items[ListBox2.ItemIndex],5,MAX_FIELD_LENGTH) +
    ') VALUES (''' + Edit3.Text + ''')';
  end;

  ShowMessage(sql);
  st := DataBase.Prepare(sql);
  st.Step;*)
end;

procedure TFormTableSchema.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #10) or (key = #13) then
  begin
    if ListBox2.Count > 0 then
    begin
      ListBox2.Items[ListBox2.ItemIndex] :=
      IntToStr(ComboBox1.ItemIndex) + ' - ' +  Edit1.Text;
    end else
    begin
      ComboBox1.ItemIndex := 0;
      ListBox2 .Items.Add('');
      OldItemIndex1 := 0;
    end;
    key := #0;
  end;
end;

procedure TFormTableSchema.Button6Click(Sender: TObject);
var
  sql: String;
  rec: Integer;
  st : TSQLite3Statement;
begin
  sql := 'SELECT ' + Edit1.Text + ' FROM ' +
  ListBox1.Items[ListBox1.ItemIndex];
  rec := 0;
  st  := DataBase.Prepare(sql);
  while st.Step = SQLITE_ROW do
  begin
    inc(rec);
  end;

  if rec > 0 then
  begin
    sql := 'UPDATE "' +
    ListBox1.Items[ListBox1.ItemIndex]  + '"'  + #13#10 + 'SET ''' +
    Edit1.Text + ''' = ''' + Edit3.Text + '''' + #13#10 + 'WHERE ' +
    Edit1.Text +   ' = (SELECT ' +
    Edit1.Text +   ' FROM "' +
    ListBox1.Items[ListBox1.ItemIndex] + '" ORDER BY '  +
    Edit1.Text +   ' LIMIT 1)';

    st := DataBase.Prepare(sql);
    st.Step;

    sql := 'SELECT * FROM ' +
    ListBox1.Items[ListBox1.ItemIndex];

    rec := 0;
    st  := DataBase.Prepare(sql);
    st.Step;

    Edit3.Text := st.ColumnText(0);
    ListBox2.ItemIndex := 0;
  end else
  begin
    sql := 'INSERT INTO ''' +
    ListBox1.Items[ListBox1.ItemIndex] + ''' (' + Copy(
    ListBox2.Items[ListBox2.ItemIndex],5,MAX_FIELD_LENGTH) +
    ') VALUES (''' + Edit3.Text + ''')';

    st := DataBase.Prepare(sql);
    st.Step;

    Edit3.Text := '';

    sql := 'seLECT ' +
    Edit1.Text   + ' FROM ' +
    ListBox1.Items[ListBox1.ItemIndex];

    st := DataBase.Prepare(sql);
    st.Step;

    Edit3.Text := st.ColumnText(1);
    ListBox2.ItemIndex := 0;
  end;
end;

end.

