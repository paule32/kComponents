// -------------------------------------------------------------------------
// File: kScrollBox.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit kScrollBox;

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms;

type
  TkScrollBox = class(TScrollBox)
  private
    FNHBitmap: TBitmap;
    FNHCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetBitmap(Value: TBitmap);
  protected
    procedure Painting;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackBitmap: TBitmap read FNHBitmap write SetBitmap;
  end;

procedure Register;
implementation

constructor TkScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNHBitmap := TBitmap.Create;
  FNHCanvas := TControlCanvas.Create;
  TControlCanvas(FNHCanvas).Control := self;
end;

destructor TkScrollBox.Destroy;
begin
  FNHBitmap.Destroy;
  FNHCanvas.Destroy;

  inherited Destroy;
end;

procedure TkScrollBox.SetBitmap(Value: TBitmap);
begin
  FNHBitmap.Assign(Value);
  Invalidate;
end;

procedure TkScrollBox.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TkScrollBox.PaintWindow(DC: HDC);
begin
  FNHCanvas.Handle := DC;
  try
    Painting;
  finally
    FNHCanvas.Handle := 0;
  end;
end;

procedure TkScrollBox.Painting;
var
  FDrawHeight, FDrawWidth: Integer;
  Row, Column, xl, xt, xw, xh: Integer;
  xdl, xdt: Integer;
  xRect: TRect;
  i: Integer;
  xhdl: Word;
begin
  if (FNHBitmap.Width <> 0) and (FNHBitmap.Height <> 0) then
  begin
    xRect := ClientRect;
    FDrawHeight := xRect.Bottom - xRect.Top;
    FDrawWidth  := xRect.Right  - xRect.Left;

    xdl := (HorzScrollBar.Position mod FNHBitmap.Width );
    xdt := (VertScrollBar.Position mod FNHBitmap.Height);

    for Row := 0 to (FDrawHeight div FNHBitmap.Height) + 1 do
    begin
      for Column := 0 to (FDrawWidth div FNHBitmap.Width) + 1 do
      begin
        xl := Column * FNHBitmap.Width  + xRect.Left - xdl;
        xt := Row    * FNHBitmap.Height + xRect.Top  - xdt;

        xw := FNHBitmap.Width;
        if    (FDrawWidth - xl + xRect.Left) < xw then
        xw := (FDrawWidth - xl + xRect.Top);

        xh := FNHBitmap.Height;
        if    (FDrawHeight - xt + xRect.Top) < xh then
        xh := (FDrawHeight - xt + xRect.Top);

        FNHCanvas.CopyRect(Rect(xl,xt,xl+xw,xt+xh),
        FNHBitmap.Canvas,Rect(0,0,xw,xh));
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('KALLUP',[
  TkScrollBox]);
end;

end.
