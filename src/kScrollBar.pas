// -------------------------------------------------------------------------
// File: kScrollBar.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit kScrollBar;

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls;

type
  TkScrollBar = class(TScrollingWinControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;
  end;

procedure Register;
implementation

constructor TkScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Brush.Color := clYellow;
end;

destructor TkScrollBar.Destroy;
begin
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('KALLUP',[
  TkScrollBar]);
end;

end.
