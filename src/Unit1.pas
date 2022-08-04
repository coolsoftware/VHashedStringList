unit Unit1;

(*
Copyright 2012 Coolsoftware. http://blog.coolsoftware.ru/

Test project for TVHashedStringList - improved hashed string list.

You can freely use this program and code for your needs.

Please, don't remove this copyright.
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VHashedStringList;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckBox1: TCheckBox;
    Label7: TLabel;
    CheckBox2: TCheckBox;
    Label8: TLabel;
    Edit3: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox3: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
    procedure Test(nSet, nGet, nDel: Integer;
      bTVHashedStringList, bGetListItems,
      bAutoUpdateHash, bMurMurHash: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ShellAPI, murmurhash;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Test(StrToInt(Edit1.Text), StrToInt(Edit2.Text), StrToInt(Edit3.Text),
    RadioButton2.Checked, CheckBox1.Checked, CheckBox2.Checked, CheckBox3.Checked);
end;

procedure TForm1.Test(nSet, nGet, nDel: Integer;
  bTVHashedStringList, bGetListItems, bAutoUpdateHash, bMurMurHash: Boolean);
var
  i, j, n: Integer;
  s, t: String;
  lst: TStringList;
  dwTick: DWORD;
begin
  Randomize;
  SetLength(s, 8);
  SetLength(t, 16);
  if bTVHashedStringList then
  begin
    lst := TVHashedStringList.Create;
    TVHashedStringList(lst).AutoUpdateHash := bAutoUpdateHash;
    if bMurMurHash then
      TVHashedStringList(lst).HashClass := TMurMurHash;
  end else
    lst := TStringList.Create;
  dwTick := GetTickCount;
  for i := 1 to nSet do
  begin
    for j := 1 to Length(s) do
    begin
      s[j] := Chr(Ord('a')+Random(26));
    end;
    for j := 1 to Length(t) do
    begin
      t[j] := Chr(Ord('a')+Random(26));
    end;
    lst.Values[s] := t;
  end;
  dwTick := GetTickCount - dwTick;
  Label2.Caption := IntToStr(dwTick);
  dwTick := GetTickCount;
  for i := 1 to nGet do
  begin
    for j := 1 to Length(s) do
    begin
      s[j] := Chr(Ord('a')+Random(26));
    end;
    t := lst.Values[s];
  end;
  if bGetListItems then
  begin
    for i := 0 to lst.Count-1 do
    begin
      s := lst.Names[i];
      t := lst.Values[s];
      if t <> lst.ValueFromIndex[i] then
        raise Exception.Create('Error!');
    end;
  end;
  dwTick := GetTickCount - dwTick;
  Label6.Caption := IntToStr(dwTick);
  dwTick := GetTickCount;
  n := lst.Count;
  for i := 1 to nDel do
  begin
    if lst.Count = 0 then Break;
    j := Random(lst.Count);
    s := lst.Names[j];
    lst.Values[s] := '';
  end;
  if lst.Count + nDel <> n then
    raise Exception.Create('Error!');
  dwTick := GetTickCount - dwTick;
  Label10.Caption := IntToStr(dwTick);
  lst.Free;
end;

procedure TForm1.Label7Click(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open',
    PChar('http://blog.coolsoftware.ru/'),
    nil, nil, SW_SHOWNORMAL);
end;

end.
