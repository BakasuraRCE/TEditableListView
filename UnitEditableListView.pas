unit UnitEditableListView;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.ComCtrls,
  Vcl.StdCtrls;

type
  ///
  /// Based on: https://stackoverflow.com/a/10836109
  ///
  TListView = class(Vcl.ComCtrls.TListView)
  strict private
    FListViewEditor: TEdit;
    FEditorItemIndex, FEditorSubItemIndex: Integer;
    FCursorPos: TPoint;

    procedure FreeEditorItemInstance;
    procedure ResetCursorPos;

    {
      TEdit Events
    }
    procedure ListViewEditorExit(Sender: TObject);
    procedure ListViewEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewEditorKeyPress(Sender: TObject; var Key: Char);
    {
      Override Events
    }
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    {
      Windows Events
    }
    { TODO -cenhancement : Scroll edit control with listview }
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///
    /// Start edition on local position
    ///
    procedure EditCaptionAt(Point: TPoint);
  end;

implementation

uses
  Vcl.Controls,
  Winapi.CommCtrl;

{ TListView }

procedure TListView.Click;
begin
  inherited;
  // Get current point
  FCursorPos := ScreenToClient(Mouse.CursorPos);
  FreeEditorItemInstance;
end;

constructor TListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Register TEdit class
  RegisterClass(TEdit);
  // Create the TEdit and assign the OnExit event
  FListViewEditor := TEdit.Create(Self);
  with FListViewEditor do
  begin
    Parent := Self;
    OnKeyDown := ListViewEditorKeyDown;
    OnKeyPress := ListViewEditorKeyPress;
    OnExit := ListViewEditorExit;
    Visible := False;
  end;

end;

destructor TListView.Destroy;
begin
  // Free TEdit
  FListViewEditor.Free;
  // UnRegister TEdit class
  RegisterClass(TEdit);
  inherited;
end;

procedure TListView.EditCaptionAt(Point: TPoint);
var
  Rect: TRect;
  CursorPos: TPoint;
  HitTestInfo: TLVHitTestInfo;
  CurrentItem: TListItem;
begin
  // Set position to handle
  HitTestInfo.pt := Point;

  // Get item select
  if ListView_SubItemHitTest(Handle, @HitTestInfo) = -1 then
    Exit;

  with HitTestInfo do
  begin
    FEditorItemIndex := iItem;
    FEditorSubItemIndex := iSubItem;
  end;

  // Nothing?
  if (FEditorItemIndex < 0) or (FEditorItemIndex >= Items.Count) then
    Exit;

  if FEditorSubItemIndex < 0 then
    Exit;

  CurrentItem := Items[ItemIndex];

  // Get bounds
  ListView_GetSubItemRect(Handle, FEditorItemIndex, FEditorSubItemIndex, LVIR_LABEL, @Rect);

  // set the text of the Edit
  if FEditorSubItemIndex = 0 then
    FListViewEditor.Text := CurrentItem.Caption
  else
  begin
    FListViewEditor.Text := CurrentItem.SubItems[FEditorSubItemIndex - 1];
  end;
  // Set the bounds of the TEdit
  FListViewEditor.BoundsRect := Rect;
  // Show the TEdit
  FListViewEditor.Visible := True;
  // Set focus
  FListViewEditor.SetFocus;
end;

procedure TListView.ResetCursorPos;
begin
  // Free cursos pos
  FCursorPos := Point(-1, -1);
end;

procedure TListView.FreeEditorItemInstance;
begin
  FEditorItemIndex := -1;
  FEditorSubItemIndex := -1;
  FListViewEditor.Visible := False; // Hide the TEdit
end;

procedure TListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  // F2 key start edit
  if (Key = VK_F2) then
    EditCaptionAt(FCursorPos);
end;

procedure TListView.ListViewEditorExit(Sender: TObject);
begin
  // I have an instance?
  if FEditorItemIndex = -1 then
    Exit;

  // Assign the value of the TEdit to the Subitem
  if FEditorSubItemIndex = 0 then
    Items[FEditorItemIndex].Caption := FListViewEditor.Text
  else
    Items[FEditorItemIndex].SubItems[FEditorSubItemIndex - 1] := FListViewEditor.Text;
  // Free instanse
  FreeEditorItemInstance;
end;

procedure TListView.ListViewEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // ESCAPE key exit of editor
  if Key = VK_ESCAPE then
    FreeEditorItemInstance;
end;

procedure TListView.ListViewEditorKeyPress(Sender: TObject; var Key: Char);
begin
  // Update item on press ENTER
  if (Key = #$0A) or (Key = #$0D) then
    FListViewEditor.OnExit(Sender);
end;

procedure TListView.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  // Reset cursos pos
  ResetCursorPos;
  // Free instanse
  FreeEditorItemInstance;
end;

procedure TListView.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  // Reset cursos pos
  ResetCursorPos;
  // Free instanse
  FreeEditorItemInstance;
end;

procedure TListView.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  // Reset cursos pos
  ResetCursorPos;
  // Free instanse
  FreeEditorItemInstance;
end;

end.
