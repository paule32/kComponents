unit FormSchema1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, kComboBox, EditField, kListBox;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    kListBox1: TkListBox;
    kListBox2: TkListBox;
    EditField1: TEditField;
    EditField2: TEditField;
    kComboBox1: TkComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
