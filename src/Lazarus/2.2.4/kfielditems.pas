// -------------------------------------------------------------------------
// File: kFieldItems.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
{$mode delphi}{$H+}
unit kFieldItems;

interface
uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Graphics,
  Forms, Dialogs, SQLite3Conn;

type
  TkFieldItem = class(TCollectionItem)
  private
    FFieldName : String;
    FFieldType : String;
    FStr       : String;
    FValue     : Integer;
  public
    constructor Create(Collection: TCollection); override;
  published
    property FieldName: String  read FFieldName write FFieldName;
    property FieldType: String  read FFieldType write FFieldType;

    property AsString : String  read FStr       write FStr;
    property AsInteger: Integer read FValue     write FValue;
  end;

  TkFieldList = class(TCollection)
  private
    function  getItems(I: Integer): TkFieldItem;
    procedure setItems(I: Integer; Value: TkFieldItem);
  public
    constructor Create;
    function Add: TkFieldItem;
    property Items[I: Integer]: TkFieldItem read GetItems write SetItems;
  end;

implementation

constructor TkFieldList.Create;
begin
  inherited Create(TkFieldItem);
end;

function TkFieldList.getItems(I: Integer): TkFieldItem;
begin
  result := TkFieldItem(inherited Items[I]);
end;

procedure TkFieldList.SetItems(I: Integer; Value: TkFieldItem);
begin
  Items[I].Assign(Value);
end;

function TkFieldList.Add: TkFieldItem;
begin
  result := TkFieldItem(inherited Add);
end;

constructor TkFieldItem.Create(Collection: TCollection);
begin
  if Assigned(Collection) and (Collection is TkFieldList) then
  inherited Create(Collection);
  FFieldType := 'TEXT';
end;

begin
end.

