// -------------------------------------------------------------------------
// File: kDataSource.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit kDataSource;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Graphics,
  Forms, Dialogs, SQLite3, SQLite3Wrap, kFieldItems;

type TkErrorEvent = procedure (Sender: TObject; ErrorCode: Integer) of object;

type
  TkFieldListItem = class(TCollectionItem)
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

type
  TkFieldList = class(TCollection)
  private
    function  getItems(I: Integer): TkFieldListItem;
    procedure setItems(I: Integer; Value: TkFieldListItem);
  public
    constructor Create;
    function Add: TkFieldListItem;
    property Items[i: integer]: TkFieldListItem read GetItems write SetItems;
  end;

type
  TkCustomDataSource = class(TPersistent)
  private
    FDataBase  : String;
    FDataTable : String;
    FActive    : Boolean;
    FParent    : TObject;
    FFieldItems: TkFieldList;
    FDataBaseObject: TSQLite3DataBase;

    // event handler ...
    FonAfterAppend : TNotifyEvent;
    FonAfterClose  : TNotifyEvent;
    FonAfterDelete : TNotifyEvent;
    FonAfterOpen   : TNotifyEvent;
    FonAfterScroll : TNotifyEvent;

    FonBeforeAppend: TNotifyEvent;
    FonBeforeClose : TNotifyEvent;
    FonBeforeDelete: TNotifyEvent;
    FonBeforeOpen  : TNotifyEvent;
    FonBeforeScroll: TNotifyEvent;

    FonError: TkErrorEvent;
  private
    procedure SetActive(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure Close;

//    property Fields[I: Integer]: TkFieldListItem read GetItems write SetItems;
  published
    property Active: Boolean read FActive    write SetActive default false;
    property Base  : String  read FDataBase  write FDataBase;
    property Table : String  read FDataTable write FDataTable;
    property Fields: TkFieldList read FFieldItems write FFieldItems;
  // event handler's:
  published
    property OnAfterAppend : TNotifyEvent read FonAfterAppend  write FonAfterAppend;
    property OnAfterClose  : TNotifyEvent read FonAfterClose   write FonAfterClose;
    property OnAfterDelete : TNotifyEvent read FonAfterDelete  write FonAfterDelete;
    property OnAfterOpen   : TNotifyEvent read FonAfterOpen    write FonAfterOpen;
    property OnAfterScroll : TNotifyEvent read FonAfterScroll  write FonAfterScroll;

    property OnBeforeAppend: TNotifyEvent read FonBeforeAppend write FonBeforeAppend;
    property OnBeforeClose : TNotifyEvent read FonBeforeClose  write FonBeforeClose;
    property OnBeforeDelete: TNotifyEvent read FonBeforeDelete write FonBeforeDelete;
    property OnBeforeOpen  : TNotifyEvent read FonBeforeOpen   write FonBeforeOpen;
    property OnBeforeScroll: TNotifyEvent read FonBeforeScroll write FonBeforeScroll;

    property OnError: TkErrorEvent read FonError write FonError;
  end;

  TkDataSource = class(TComponent)
  private
    FData: TkCustomDataSource;
    procedure SetDataSource(AData: TkCustomDataSource);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // properties:
  published
    property Data: TkCustomDataSource read FData write SetDataSource;
  end;

procedure Register;

implementation
uses
  kDataGrid;

constructor TkFieldList.Create;
begin
  inherited Create(TkFieldListItem);
end;

function TkFieldList.Add: TkFieldListItem;
begin
  result := TkFieldListItem(inherited Add);
end;

{ TkDataSource }
constructor TkDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FData := TkCustomDataSource.Create;
  FData.FParent := AOwner;
end;
destructor TkDataSource.Destroy;
begin
  if Assigned(Data.FDataBaseObject) then
  begin
    Data.FDataBaseObject.Close;
    Data.FDataBaseObject.Free;
  end;

  inherited Destroy;
end;

procedure TkDataSource.SetDataSource(AData: TkCustomDataSource);
begin
  FData.Assign(AData);
end;

constructor TkCustomDataSource.Create;
begin
  inherited Create;
  FFieldItems := TkFieldList.Create;
end;
destructor TkCustomDataSource.Destroy;
begin
  FFieldItems.Clear;
  FFieldItems.Free;
  inherited Destroy;
end;

function TkFieldList.GetItems(i: integer): TkFieldListItem;
begin
  Result := TkFieldListItem(inherited Items[I]);
end;

procedure TkFieldList.SetItems(i: integer; Value: TkFieldListItem);
begin
  Items[I].Assign(Value);
end;

procedure TkCustomDataSource.Open;
begin
  if Assigned(FonBeforeOpen) then
  begin
    ShowMessage('before open');
    onBeforeOpen(self);
  end;

  if Length(Trim(Base)) < 1 then
  begin
    MessageDlg(
    'Error:'   + #13#10 +
    'No database file given.', mtInformation, [mbOk], 0);
    exit;
  end;

  SetActive(false);

  FDataBaseObject.Close;
  FDataBaseObject.Open(Base);

  if Assigned(FonAfterOpen) then
  begin
    ShowMessage('after open');
    onAfterOpen(self);
  end;
end;

procedure TkCustomDataSource.Close;
begin
  if FDataBaseObject <> nil then
  FDataBaseObject.Close;
end;

procedure TkCustomDataSource.SetActive(AValue: Boolean);
var
  ClassRef: TClass;
  tc: TComponent;
  i: Integer;
begin
  FActive := AValue;

  if AValue = true then
  begin
    for i := (FParent as TForm).ComponentCount - 1 downto 0 do
    begin
      tc := (FParent as TForm).Components[i];
      if tc.ClassName = 'TkDataGrid' then
      begin
        with (tc as TkDataGrid) do
        begin
          getGrid.ColCount := 5;
          getGrid.RowCount := 5;
        end;
      end;
    end;
  end else
  begin
    for i := (FParent as TForm).ComponentCount - 1 downto 0 do
    begin
      tc := (FParent as TForm).Components[i];
      if tc.ClassName = 'TkDataGrid' then
      begin
        with (tc as TkDataGrid) do
        begin
          getGrid.ColCount := 2;
          getGrid.RowCount := 2;
        end;
      end;
    end;
  end;
end;

constructor TkFieldListItem.Create(Collection: TCollection);
begin
  if Assigned(Collection) and (Collection is TkFieldList) then
  inherited Create(Collection);
  FFieldType := 'TEXT';
end;

procedure Register;
begin
  RegisterComponents('KALLUP', [
  TkDataSource]);
end;

end.

