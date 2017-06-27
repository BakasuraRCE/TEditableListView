unit Unit1;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Controls,
  UnitEditableListView;

type

  TForm1 = class(TForm)
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Edit1: TMenuItem;
    Remove1: TMenuItem;
    Create1: TMenuItem;
    N1: TMenuItem;
    Panel1: TPanel;
    LabelTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Create1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}


procedure TForm1.Create1Click(Sender: TObject);
var
  Item: TListItem;
begin
  Item := ListView1.Items.Add;
  Item.Caption := 'a';
  Item.SubItems.Add('b');
  Item.SubItems.Add('c');
  Item.SubItems.Add('d');
  Item.SubItems.Add('e');

  Item.Selected := true;

  ListView1.Scroll(0, ListView1.Selected.Top - (ListView1.Height div 2));

end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  if ListView1.SelCount = 0 then
    Exit;

  ListView1.EditCaptionAt(ListView1.ScreenToClient(PopupMenu1.PopupPoint));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
begin

  for I := 0 to 50 do
  begin
    Item := ListView1.Items.Add;
    Item.Caption := Format('%d.%d', [I, 1]);
    Item.SubItems.Add(Format('%d.%d', [I, 2]));
    Item.SubItems.Add(Format('%d.%d', [I, 3]));
    Item.SubItems.Add(Format('%d.%d', [I, 4]));
    Item.SubItems.Add(Format('%d.%d', [I, 5]));
  end;

end;

procedure TForm1.Remove1Click(Sender: TObject);
begin
  if ListView1.SelCount = 0 then
    Exit;

  ListView1.Selected.Delete;
end;

end.
