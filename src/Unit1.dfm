object Form1: TForm1
  Left = 1248
  Top = 481
  Width = 412
  Height = 317
  Caption = 'Test TVHashedStringList'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 144
    Top = 210
    Width = 79
    Height = 13
    Caption = 'Set Time (msec):'
  end
  object Label2: TLabel
    Left = 256
    Top = 210
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label3: TLabel
    Left = 16
    Top = 18
    Width = 50
    Height = 13
    Caption = 'List Count:'
  end
  object Label4: TLabel
    Left = 176
    Top = 18
    Width = 51
    Height = 13
    Caption = 'Get Count:'
  end
  object Label5: TLabel
    Left = 144
    Top = 242
    Width = 80
    Height = 13
    Caption = 'Get Time (msec):'
  end
  object Label6: TLabel
    Left = 256
    Top = 242
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label7: TLabel
    Left = 16
    Top = 48
    Width = 116
    Height = 13
    Cursor = crHandPoint
    Caption = 'blog.coolsoftware.ru'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = Label7Click
  end
  object Button1: TButton
    Left = 144
    Top = 160
    Width = 121
    Height = 25
    Caption = 'Set Values'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 80
    Top = 16
    Width = 65
    Height = 21
    TabOrder = 1
    Text = '1000'
  end
  object Edit2: TEdit
    Left = 240
    Top = 16
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '5000'
  end
  object RadioButton1: TRadioButton
    Left = 144
    Top = 88
    Width = 121
    Height = 17
    Caption = 'TStringList'
    TabOrder = 3
  end
  object RadioButton2: TRadioButton
    Left = 144
    Top = 120
    Width = 121
    Height = 17
    Caption = 'TVHashedStringList'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object CheckBox1: TCheckBox
    Left = 176
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Get List Items'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
end
