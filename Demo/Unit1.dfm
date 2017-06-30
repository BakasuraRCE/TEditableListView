object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Editable ListView'
  ClientHeight = 336
  ClientWidth = 844
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 25
    Width = 844
    Height = 311
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'a'
      end
      item
        AutoSize = True
        Caption = 'b'
      end
      item
        AutoSize = True
        Caption = 'c'
      end
      item
        AutoSize = True
        Caption = 'd'
      end
      item
        AutoSize = True
        Caption = 'e'
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnEdited = ListView1Edited
    OnEditing = ListView1Editing
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 844
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object LabelTitle: TLabel
      Left = 0
      Top = 12
      Width = 844
      Height = 13
      Align = alBottom
      Alignment = taCenter
      Caption = 
        'Click on field and press F2 to edit the field, or right click an' +
        'd Edit'
      ExplicitTop = 7
      ExplicitWidth = 306
    end
    object ComboBox1: TComboBox
      Left = 696
      Top = 2
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 0
      Text = 'vsReport'
      OnChange = ComboBox1Change
      Items.Strings = (
        'vsIcon'
        'vsSmallIcon'
        'vsList'
        'vsReport')
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 792
    Top = 48
    object Create1: TMenuItem
      Caption = 'Create'
      OnClick = Create1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      OnClick = Edit1Click
    end
  end
end
