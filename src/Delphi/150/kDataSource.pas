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
  TkDataSource = class(TComponent)
  private
    FDataBase  : String;
    FDataTable : String;
    FActive    : Boolean;

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
    FFieldItems: TkFieldList;
  protected
  public
    procedure OpenDataBase;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  // properties:
  published
    property EditDataActive : Boolean read FActive    write FActive default false;
    property EditDataBase   : String  read FDataBase  write FDataBase;
    property EditDataTable  : String  read FDataTable write FDataTable;

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

procedure Register;

implementation

{ TkDataSource }
constructor TkDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldItems := TkFieldList.Create;
end;
destructor TkDataSource.Destroy;
begin
  FFieldItems.Clear;
  FFieldItems.Free;

  if Assigned(FDataBaseObject) then
  begin
    FDataBaseObject.Close;
    FDataBaseObject.Free;
  end;

  inherited Destroy;
end;

procedure TkDataSource.OpenDataBase;
begin
  if Assigned(FonBeforeOpen) then
  begin
    ShowMessage('before open');
    onBeforeOpen(self);
  end;

  if Length(Trim(EditDataBase)) < 1 then
  begin
    MessageDlg(
    'Error:'   + #13#10 +
    'No database file given.', mtInformation, [mbOk], 0);
    exit;
  end;

  FActive := false;
  FDataBaseObject.Close;
  FDataBaseObject.Open(EditDataBase);

  if Assigned(FonAfterOpen) then
  begin
    ShowMessage('after open');
    onAfterOpen(self);
  end;
end;

procedure Register;
begin
  RegisterComponents('KALLUP', [
  TkDataSource]);
end;

end.

