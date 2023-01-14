// -------------------------------------------------------------------------
// File: kListBox.pas
// Desc: ListBox Component
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit kListBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Graphics, Types;

type
  TkListBox = class(TListBox)
  private
    FColorEnter : TColor;
    FColorExit  : TColor;

    function  getColorEnter: TColor;
    function  getColorExit : TColor;

    procedure setColorEnter(Value: TColor);
    procedure setColorExit (Value: TColor);

    procedure ListBoxEnter (Sender: TObject);
    procedure ListBoxExit  (Sender: TObject);
    procedure ListBoxDraw  (Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorEnter : TColor  read getColorEnter write setColorEnter;
    property ColorExit  : TColor  read getColorExit  write setColorExit;
  end;

procedure Register;

implementation

{ TkListBox }
constructor TkListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  FColorEnter := clYellow;
  FColorExit  := clWhite;

  onEnter     := ListBoxEnter;
  onExit      := ListBoxExit;

  OnDrawItem  := ListBoxDraw;
end;

destructor TkListBox.Destroy;
begin
  Items.Clear;
  inherited Destroy;
end;

procedure TkListBox.setColorEnter(Value: TColor);
begin
  FColorEnter := Value;
  Repaint;
end;
procedure TkListBox.setColorExit (Value: TColor);
begin
  FColorExit := Value;
  Repaint;
end;

function TkListBox.getColorEnter: TColor;
begin
  result := FColorEnter;
end;
function TkListBox.getColorExit: TColor;
begin
  result := FColorExit;
end;

procedure TkListBox.ListBoxEnter(Sender: TObject);
begin
  Color := FColorEnter;
  Repaint;
end;

procedure TkListBox.ListBoxExit(Sender: TObject);
begin
  Color := FColorExit;
  RePaint;
end;

procedure TkListBox.ListBoxDraw(
  Control: TWinControl;
  Index  : Integer;
  Rect   : TRect;
  State  : TOwnerDrawState);
var
  tr: TRect;
begin
  tr.Left   := 1;
  tr.Top    := 1;
  tr.Right  := Width  - 1;
  tr.Bottom := Height - 1;

  if Focused then
  begin
    Canvas.Brush.Color := FColorEnter;
  end else
  begin
    Canvas.Brush.Color := FColorExit;
  end;

  Canvas.FillRect(tr);
end;

procedure Register;
begin
  RegisterComponents('KALLUP', [TkListBox]);
end;

end.







