unit dummy;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, EditField, kDataSource, kDataGrid, Grids;

type
  TForm2 = class(TForm)
    kDataGrid1: TkDataGrid;
    kDataSource1: TkDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
