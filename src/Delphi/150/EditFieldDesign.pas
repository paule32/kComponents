// -------------------------------------------------------------------------
// File: EditFieldDesign.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit EditFieldDesign;

interface

uses
  {$ifdef ver150} DesignIntf, DesignEditors,{$else}�DsgnIntf,{$endif}
  SysUtils, Classes, Controls, StdCtrls, Messages, Graphics,
  Forms, Dialogs, kDataSource, kFieldItems;

type
  TEditFieldFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

type
  TEditFieldTableNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

type
  TEditFieldFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation
uses
  EditField, SQLite3, SQLite3Wrap, FormTableSelect, FormTableField,
  kDataGrid;

  var
  db: TSQLite3DataBase;

{ TEditFieldFileNameProperty }
function TEditFieldFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;
procedure TEditFieldFileNameProperty.Edit;
begin
  with TOpenDialog.Create(Application) do
  try
    Title := GetName;
    FileName := GetValue;
    Filter := 'SQLite3 (*.db)|*.db';

    Options := Options + [
    ofShowHelp,
    ofPathMustExist,
    ofFileMustExist];

    if Execute then SetValue(FileName);
  finally
    Free
  end;
end;

{ TEditFieldTableNameProperty }
function TEditFieldTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;
procedure TEditFieldTableNameProperty.Edit;
var
  i,j: Integer;
  p: TPersistent;

  ds: TkCustomDataSource;
  st: TSQLite3Statement;

  frm: TFormTableSelect;
  sql: String;
begin
  for i := 0 to PropCount - 1 do
  begin
    if GetComponent(i).ClassName = 'TkCustomDataSource' then
    begin
      ds := TkCustomDataSource(GetComponent(i));
      if Length(Trim(ds.Base)) < 1 then
      begin
        MessageDlg(
        'Error:'   + #13#10 +
        'No database file given.', mtInformation, [mbOk], 0);
        exit;
      end;
    end;
  end;

  frm := TFormTableSelect.Create(Application);
  try
    try
      db := TSQLite3DataBase.Create;
      db.Open(ds.Base);

      frm.DataBase := db;
      frm.DataName := ds.Base;

      frm.PropertyEdit := self;

      sql := 'SELECT * FROM sqlite_master WHERE type = "table"';
      st := DB.Prepare(sql);
      while st.Step = SQLITE_ROW do
      begin
        if Trim(st.ColumnText(1)) = '' then
        frm.ListBox1.Items.Add('null') else
        frm.ListBox1.Items.Add(st.ColumnText(1)) ;
        st.Step;
      end;
      frm.ShowModal;
//      SetValue(frm.DataName);
    except
      on E: Exception do
      begin
        ShowMessage('Error:' + #13#10 +
        E.Message);
      end;
    end;
  finally
    db.Close;
    frm.Free;
  end;
end;

{ TEditFieldFieldNameProperty }
function TEditFieldFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;
procedure TEditFieldFieldNameProperty.Edit;
var
  ds: TkDataSource;
  st: TSQLite3Statement;
  db: TSQLite3DataBase;
  tc: TComponent;
  s1: String;
  s2: String;
  i: Integer;
begin
ShowMessage('1111');
  for i := 0 to TComponent(Designer.Root).ComponentCount - 1 do
  begin
    tc := TComponent(Designer.Root).Components[i];
    if CompareStr(tc.ClassName,'TkDataSource') = 0 then
    begin
      s1 := Trim(TkDataSource(tc).Data.Base);
      if Length(s1) < 1 then
      begin
        MessageDlg(
        'Error:' + #13#10 +
        'No database file given.', mtInformation, [mbOk], 0);
        exit;
      end;
    end;
  end;

  db := TSQLite3DataBase.Create;
  db.Open(s1);

  with TTableField.Create(Application) do
  try
    DataTable := s1;
    s1 := TkDataSource(tc).Data.Table;
    s1 := 'PRAGMA table_info ("' + s1 + '")';
    st := db.Prepare(s1);
//
    while st.Step = SQLITE_ROW do
    begin
      if Trim(st.ColumnText(1)) = '' then
      ListBox1.Items.Add('null') else
      ListBox1.Items.Add(st.ColumnText(1));
    end;

    ShowModal;
    SetValue(DataField);

    for i := 0 to PropCount - 1 do
    begin
      if GetComponent(i).ClassName = 'TkListItem' then
      begin
        s1 := 'SELECT ' + DataField + ' FROM ' + s2;
        st := db.Prepare(s1);
        st.Step;

        tc := TComponent(GetComponent(i));
// todo
//        TkFieldItem(tc).AsString := st.ColumnText(1);
      end;
    end;

  finally
    Free;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),
  TkCustomDataSource,'Base' ,
  TEditFieldFileNameProperty);

  RegisterPropertyEditor(TypeInfo(string),
  TkCustomDataSource,'Table',
  TEditFieldTableNameProperty);

  RegisterPropertyEditor(TypeInfo(string),
  TkCustomDataSource,'Fields' ,
  TEditFieldFieldNameProperty);

  RegisterPropertyEditor(TypeInfo(string),
  TkFieldListItem,'FieldName' ,
  TEditFieldFieldNameProperty);
end;

end.
