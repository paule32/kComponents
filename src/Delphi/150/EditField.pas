// -------------------------------------------------------------------------
// File: EditField.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit EditField;

interface
uses
  SysUtils, Classes, Controls, StdCtrls, Messages, Graphics,
  Forms, Dialogs, kDataSource;

type
  TEditFieldOptionSet  = (efASCII,efNumber,efFullASCII);
  TEditFieldOptionType = (efNormal,efData);
  TEditFieldOptions = set of TEditFieldOptionSet;

  TkEditField = class(TEdit)
  private
    FisChanged : Boolean;
    FOptions   : TEditFieldOptions;
    FOptionType: TEditFieldOptionType;

    FDataSource: TkDataSource;
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
    property EditDataSource : TkDataSource   read FDataSource write FDataSource;
    property EditDataField  : String               read FDataField  write FDataField;

    property EditOptions    : TEditFieldOptions    read FOptions    write FOptions;
    property EditType       : TEditFieldOptionType read FOptionType write FOptionType;

    property Modified: Boolean read FisChanged write FisChanged default false;
  end;

procedure Register;

implementation

{ TEditField }
constructor TkEditField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FisChanged := false;
end;
destructor TkEditField.Destroy;
begin
  inherited Destroy;
end;

procedure TkEditField.SetPitchColor;
begin
end;

procedure TkEditField.DoEnter;
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

procedure TkEditField.DoExit;
begin
  Color := clWhite;
  Font.Color := clBlack;
end;

procedure TkEditField.CMDialogKey(var Msg: TWMKey);
begin
  FisChanged := true;
  Msg.Result := 0;
  inherited;
end;

procedure TkEditField.KeyPress(var key: Char);
begin
  FisChanged := true;
  Color := clRed;
  Font.Color := clWhite;
end;

procedure Register;
begin
  RegisterComponents('KALLUP', [TkEditField]);
end;

end.
