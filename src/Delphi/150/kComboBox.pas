// -------------------------------------------------------------------------
// File: kComboBox.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit kComboBox;

interface
uses
  SysUtils, Classes, Controls, StdCtrls, Graphics, Types;

type
  TkComboBox = class(TComboBox)
  private
    FColorEnter : TColor;
    FColorExit  : TColor;

    function  getColorEnter: TColor;
    function  getColorExit : TColor;

    procedure setColorEnter(Value: TColor);
    procedure setColorExit (Value: TColor);

    procedure ComboBoxEnter (Sender: TObject);
    procedure ComboBoxExit  (Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ColorEnter : TColor  read getColorEnter write setColorEnter;
    property ColorExit  : TColor  read getColorExit  write setColorExit;
  end;

procedure Register;
implementation

{ TkComboBox }
constructor TkComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  FColorEnter := clYellow;
  FColorExit  := clWhite;

  onEnter     := ComboBoxEnter;
  onExit      := ComboBoxExit;
end;

destructor TkComboBox.Destroy;
begin
  Items.Clear;
  inherited Destroy;
end;

procedure TkComboBox.setColorEnter(Value: TColor);
begin
  FColorEnter := Value;
  Repaint;
end;
procedure TkComboBox.setColorExit (Value: TColor);
begin
  FColorExit := Value;
  Repaint;
end;

function TkComboBox.getColorEnter: TColor;
begin
  result := FColorEnter;
end;
function TkComboBox.getColorExit: TColor;
begin
  result := FColorExit;
end;

procedure TkComboBox.ComboBoxEnter(Sender: TObject);
begin
  Color := FColorEnter;
  Repaint;
end;

procedure TkComboBox.ComboBoxExit(Sender: TObject);
begin
  Color := FColorExit;
  RePaint;
end;

procedure Register;
begin
  RegisterComponents('KALLUP', [TkComboBox]);
end;

end.
