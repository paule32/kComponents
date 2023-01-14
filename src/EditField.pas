// -------------------------------------------------------------------------
// File: EditField.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit EditField;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Graphics,
  Forms, Dialogs,
  DesignIntf, DesignEditors;

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

type TSQLite3ErrorEvent = procedure (Sender: TObject; ErrorCode: Integer) of object;
type
  TSQLite3DataSource = class(TComponent)
  private
    FDataBase  : String;
    FDataTable : String;
    FActive    : Boolean;

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

    FonError: TSQLite3ErrorEvent;
  public
  // properties:
  published
    property EditDataActive : Boolean read FActive    write FActive default false;
    property EditDataBase   : String  read FDataBase  write FDataBase;
    property EditDataTable  : String  read FDataTable write FDataTable;
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

    property OnError: TSQLite3ErrorEvent read FonError write FonError;
  end;

type
  TEditFieldOptionSet  = (efASCII,efNumber,efFullASCII);
  TEditFieldOptionType = (efNormal,efData);
  TEditFieldOptions = set of TEditFieldOptionSet;

  TEditField = class(TEdit)
  private
    FisChanged : Boolean;
    FOptions   : TEditFieldOptions;
    FOptionType: TEditFieldOptionType;

    FDataSource: TSQLite3DataSource;
    FDataField : String;

    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
    procedure SetPitchColor;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EditDataSource : TSQLite3DataSource   read FDataSource write FDataSource;
    property EditDataField  : String               read FDataField  write FDataField;

    property EditOptions    : TEditFieldOptions    read FOptions    write FOptions;
    property EditType       : TEditFieldOptionType read FOptionType write FOptionType;

    property Modified: Boolean read FisChanged write FisChanged default false;
  end;

procedure Register;

implementation

{$R TEditFieldDataSource.dcr}

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
begin
  with TForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

{ TEditFieldFieldNameProperty }
function TEditFieldFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;
procedure TEditFieldFieldNameProperty.Edit;
begin
  with TForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

constructor TEditField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FisChanged := false;
end;
destructor TEditField.Destroy;
begin
  inherited Destroy;
end;

procedure TEditField.SetPitchColor;
begin
end;

procedure TEditField.DoEnter;
begin
  if FisChanged then
  begin
    Color := clRed;
    Font.Color := clWhite;
  end else
  begin
    Color := clYellow;
    Font.Color := clBlack;
  end;
end;

procedure TEditField.DoExit;
begin
  Color := clWhite;
  Font.Color := clBlack;
end;

procedure TEditField.CMDialogKey(var Msg: TWMKey);
begin
  FisChanged := true;
  Msg.Result := 0;
  inherited;
end;

procedure TEditField.KeyPress(var key: Char);
begin
  FisChanged := true;
  Color := clRed;
  Font.Color := clWhite;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),
  TSQLite3DataSource,'EditDataBase' ,
  TEditFieldFileNameProperty);

  RegisterPropertyEditor(TypeInfo(string),
  TSQLite3DataSource,'EditDataTable',
  TEditFieldTableNameProperty);

  RegisterPropertyEditor(TypeInfo(string),
  TEditField,'EditDataField' ,
  TEditFieldFieldNameProperty);


  RegisterComponents('KALLUP', [
  TEditField,
  TSQLite3DataSource]);
end;

end.
