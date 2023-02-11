// -------------------------------------------------------------------------
// File: kDataGrid.pas
// Desc: Tools for Help authoring.
//
// Code: (c) 2023 by Jens Kallup - paule32
//       all rights reserved.
// -------------------------------------------------------------------------
unit kDataGrid;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, kScrollBox, kDataSource;

type
  TkHeaderItem = class(TCollectionItem)
  private
    FColor: TColor;
    FTitle: String;
    FFont : TFont;
    FWidth: Integer;
    procedure SetFontValue(AFont: TFont);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Color: TColor  read FColor write FColor default clBlack;
    property Title: String  read FTitle write FTitle;
    property Font : TFont   read FFont  write SetFontValue;
    property Width: Integer read FWidth write FWidth default 64;
  end;

type
  TkHeaderTitle = class(TCollection)
  private
    FCount: Integer;
    function  getItems(I: Integer): TkHeaderItem;
    procedure setItems(I: Integer; Value: TkHeaderItem);
  public
    constructor Create;
    function Add: TkHeaderItem;
    property Count: Integer read FCount;
    property Items[I: Integer]: TkHeaderItem read GetItems write SetItems;
  end;

type
  { TkCustomDataGrid }
  TkCustomDataGrid = class(TStringGrid)
  private
    FGridFixedFont: TFont;
    FGridFixedChar: WideString;

    procedure StringGrid1DrawCell(
    Sender     : TObject;
    ACol, ARow : Integer;
    Rect: TRect;
    State: TGridDrawState);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
  end;

  { TkDataGrid }
  TkDataGrid = class(TCustomControl)
  private
    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;

    FScrollBox: TkScrollBox;
    FGrid: TkCustomDataGrid;

    FDataSource: TkDataSource;

    FHeaderItems  : TkHeaderTitle;

    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure SetDataSource(ASource: TkDataSource);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function getGrid: TkCustomDataGrid;
  published
    property Headers: TkHeaderTitle read FHeaderItems write FHeaderItems;
    property DataSource: TkDataSource read FDataSource write SetDataSource;
  end;

procedure Register;
implementation

{ TkHeaderItem }
constructor TkHeaderItem.Create(Collection: TCollection);
begin
  if Assigned(Collection) and (Collection is TkHeaderTitle) then
  inherited Create(Collection);
  FFont := TFont.Create;
end;

destructor TkHeaderItem.Destroy;
begin
  FFont.    Destroy;
  inherited Destroy;
end;

procedure TkHeaderItem.SetFontValue(AFont: TFont);
begin
  FFont.Assign(AFont);
end;

{ TkHeaderTitle }
constructor TkHeaderTitle.Create;
begin
  inherited Create(TkHeaderItem);
end;

function TkHeaderTitle.getItems(I: Integer): TkHeaderItem;
begin
  result := TkHeaderItem(inherited Items[I]);
end;

procedure TkHeaderTitle.SetItems(I: Integer; Value: TkHeaderItem);
begin
  inc(FCount);
  Items[I].Assign(Value);
end;

function TkHeaderTitle.Add: TkHeaderItem;
begin
  result := TkHeaderItem(inherited Add);
end;

{ TkDataGrid }
constructor TkDataGrid.Create(AOwner: TComponent);
var
  w,h: Integer;
begin
  inherited Create(AOwner);
  ControlStyle := [
    csAcceptsControls,
    csCaptureMouse,
    csClickEvents,
    csSetCaption,
    csOpaque,
    csDoubleClicks,
    csReplicatable
  ];

  Width  := 200;
  Height := 100;

  FScrollBox := TkScrollBox.Create(self);
  FScrollBox.Parent  := self;
  FScrollBox.HorzScrollBar.Visible := false;
  FScrollBox.VertScrollBar.Visible := false;
  FScrollBox.Width   := Width;
  FScrollBox.Height  := Height;
  FScrollBox.Visible := true;

  FVScrollBar := TScrollBar.Create(FScrollBox);
  FVScrollBar.Parent  := FScrollBox;
  FVScrollBar.Visible := true;
  FVScrollBar.Kind    := sbVertical;
  FVScrollBar.Left    := FScrollBox.Width - 45;
  FVScrollBar.Top     := 0;
  FVScrollBar.Width   := 20;
  FVScrollBar.Height  := FScrollBox.Height - 45;

  FHScrollBar := TScrollBar.Create(FScrollBox);
  FHScrollBar.Parent  := FScrollBox;
  FHScrollBar.Visible := true;
  FHScrollBar.Kind    := sbHorizontal;
  FHScrollBar.Left    := 0;
  FHScrollBar.Top     := FScrollBox.Height - 24;
  FHScrollBar.Width   := FScrollBox.Width  - 24;
  FHScrollBar.Height  := 20;

  FGrid := TkCustomDataGrid.Create(FScrollBox);
  FGrid.Parent  := TWinControl(FScrollBox);
  FGrid.ScrollBars := ssNone;
  FGrid.Visible := true;
  FGrid.Top  := 0;
  FGrid.Left := 0;
  FGrid.Width  := FScrollBox.Width  - 24;
  FGrid.Height := FScrollBox.Height - 24;

  FGrid.ColWidths[0] := 24;

  FHeaderItems := TkHeaderTitle.Create;
end;

destructor TkDataGrid.Destroy;
begin
  FHeaderItems.Destroy;
  FHScrollBar .Destroy;
  FVScrollBar .Destroy;
  FGrid       .Destroy;
  FScrollBox  .Destroy;

  inherited    Destroy;
end;

procedure TkDataGrid.WMSize(var Message: TMessage);
begin
  inherited;

  FScrollBox.Width  := Width;
  FScrollBox.Height := Height;

  FGrid.Width  := Width  - 24;
  FGrid.Height := Height - 24;

  FVScrollBar.Left   := FScrollBox.Width  - 22;
  FVScrollBar.Top    := 0;
  FVScrollBar.Width  := 18;
  FVScrollBar.Height := FScrollBox.Height - 24;

  FHScrollBar.Left   := 0;
  FHScrollBar.Top    := FScrollBox.Height - 22;
  FHScrollBar.Width  := FScrollBox.Width  - 24;
  FHScrollBar.Height := 18;

  Paint;
end;

function TkDataGrid.getGrid: TkCustomDataGrid;
begin
  result := FGrid;
end;

procedure TkDataGrid.SetDataSource(ASource: TkDataSource);
begin
  FDataSource := ASource;
  FDataSource.Data.Active := ASource.Data.Active;
end;

{ TkCustomDataGrid }
constructor TkCustomDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGridFixedFont := TFont.Create;
  FGridFixedFont.Color := clBlack;
  FGridFixedFont.Size  := 14;
  FGridFixedFont.Name  := 'Times New Roman';

  FGridFixedChar := WideChar($25ba);

  Color := clGray;
  OnDrawCell := StringGrid1DrawCell;
end;

destructor TkCustomDataGrid.Destroy;
begin
  FGridFixedFont.Destroy;
  inherited Destroy;
end;

procedure TkCustomDataGrid.StringGrid1DrawCell(
  Sender     : TObject;
  ACol, ARow : Integer;
  Rect: TRect;
  State: TGridDrawState);
  var
  dg: TkDataGrid;
  ds: TkDataSource;
  tr: TRect;
begin
  dg := TkDataGrid(Parent.Parent);
  ds := dg.DataSource;
  if (Assigned(ds) or (ds <> nil)) then
  begin
    if not ds.Data.Active then
    begin
      ColCount := 2;
      RowCount := 2;
    end else
    begin
      ColCount := 5;
    end;
  end else
  begin
    ColCount := 2;
    RowCount := 2;
  end;

  if (ACol > 0) and (ARow > 0) then
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(Rect);
  end;

  if (ACol = 0) and (ARow = 1) then
  begin
    rect.Top  := rect.Top+3;
    rect.Left := 4;
    Canvas.Font.Assign(FGridFixedFont);
    DrawTextW(Canvas.Handle,PWideChar(FGridFixedChar),
    -1, Rect, DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure Register;
begin
  RegisterComponents('KALLUP', [
  TkDataGrid]);
end;

end.
