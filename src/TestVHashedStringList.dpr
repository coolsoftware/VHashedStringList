program TestVHashedStringList;

(*
Copyright 2012 Coolsoftware. http://blog.coolsoftware.ru/

Test project for TVHashedStringList - improved hashed string list.

You can freely use this program and code for your needs.

Please, don't remove this copyright.
*)

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  VHashedStringList in 'VHashedStringList.pas',
  murmurhash in 'murmurhash.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
