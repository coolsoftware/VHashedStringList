unit VHashedStringList;

interface

(*
Copyright 2012 Coolsoftware. http://blog.coolsoftware.ru/

TVHashedStringList - improved hashed string list.

You can freely use this program and code for your needs.

Please, don't remove this copyright.
*)

uses SysUtils, Classes, Windows;

type
  PPVHashItem = ^PVHashItem;
  PVHashItem = ^TVHashItem;
  TVHashItem = packed record
    Next: PVHashItem;
    Key: String;
    Value: Integer;
    //Key: array[0..0] of Char;
  end;

  TVStringHash = class
  private
    FBuckets: array of PVHashItem;
  protected
    function Find(const Key: String): PPVHashItem;
    function HashOf(const Key: String): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: String; Value: Integer);
    procedure Clear;
//    function Modify(const Key: String; Value: Integer): Boolean;
//    procedure Remove(const Key: String);
    function Delete(const Key: String; Value: Integer): Boolean;
    function ValueOf(const Key: String): Integer;
  end;

  TVHashedStringList = class(TStringList)
  private
    FBucketsSize: Cardinal;
    FValueHash: TVStringHash;
    FNameHash: TVStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
    function GetValue(const Name: String): String;
    procedure SetValue(const Name, Value: String);
  protected
    procedure Changed; override;
    procedure InsertItem(Index: Integer; const S: String; AObject: TObject); override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(BucketsSize: Cardinal = 256);
    destructor Destroy; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override;
    property Values[const Name: String]: String read GetValue write SetValue;
  end;

implementation

uses RTLConsts;

{ TVStringHash }

procedure TVStringHash.Add(const Key: String; Value: Integer);
var
  Hash: Integer;
  Bucket: PVHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(FBuckets));
  New(Bucket);
  //GetMem(Bucket, 8 + Length(Key)+1);
  Bucket^.Key := Key;
  //StrPCopy(Bucket^.Key, Key);
  Bucket^.Value := Value;
  Bucket^.Next := FBuckets[Hash];
  FBuckets[Hash] := Bucket;
end;

procedure TVStringHash.Clear;
var
  I: Integer;
  P, N: PVHashItem;
begin
  for I := 0 to Length(FBuckets) - 1 do
  begin
    P := FBuckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      //FreeMem(P);
      P := N;
    end;
    FBuckets[I] := nil;
  end;
end;

constructor TVStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(FBuckets, Size);
end;

destructor TVStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TVStringHash.Find(const Key: String): PPVHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(FBuckets));
  Result := @FBuckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
    //if StrComp(Result^.Key, PChar(Key)) = 0 then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TVStringHash.HashOf(const Key: String): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key[I]);
end;
{
function TVStringHash.Modify(const Key: String; Value: Integer): Boolean;
var
  P: PVHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TVStringHash.Remove(const Key: String);
var
  P: PVHashItem;
  Prev: PPVHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
    //FreeMem(P);
  end;
end;
}
function TVStringHash.Delete(const Key: String; Value: Integer): Boolean;
var
  P: PVHashItem;
  Prev: PPVHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  while P <> nil do
  begin
    if (P^.Key = Key) and
       (P^.Value = Value) then
    begin
      Prev^ := P^.Next;
      Dispose(P);
      //FreeMem(P);
      Result := True;
      Exit;
    end;
    Prev := @P^.Next;
    P := Prev^;
  end;
  Result := False;
end;

function TVStringHash.ValueOf(const Key: String): Integer;
var
  P: PVHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

{ TVHashedStringList }

procedure TVHashedStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

constructor TVHashedStringList.Create(BucketsSize: Cardinal);
begin
  inherited Create;
  FBucketsSize := BucketsSize;
  FValueHash := nil;
  FNameHash := nil;
  FValueHashValid := False;
  FNameHashValid := False;
end;

procedure TVHashedStringList.Delete(Index: Integer);
begin
  inherited;
end;

destructor TVHashedStringList.Destroy;
begin
  if Assigned(FValueHash) then
    FValueHash.Free;
  if Assigned(FNameHash) then
    FNameHash.Free;
  inherited Destroy;
end;

function TVHashedStringList.GetValue(const Name: String): String;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TVHashedStringList.IndexOf(const S: String): Integer;
begin
  UpdateValueHash;
  if not CaseSensitive then
    Result :=  FValueHash.ValueOf(AnsiUpperCase(S))
  else
    Result :=  FValueHash.ValueOf(S);
end;

function TVHashedStringList.IndexOfName(const Name: String): Integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(AnsiUpperCase(Name))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure TVHashedStringList.InsertItem(Index: Integer; const S: String;
  AObject: TObject);
var
  bValueHashValid: Boolean;
  bNameHashValid: Boolean;
  Key: String;
  P: Integer;
begin
  if Index = Count then
  begin
    //if add new item then update hash
    bValueHashValid := FValueHashValid;
    bNameHashValid := FNameHashValid;
    inherited;
    if bValueHashValid then
    begin
      if FValueHash = nil then
        FValueHash := TVStringHash.Create(FBucketsSize);
      if not CaseSensitive then
        FValueHash.Add(AnsiUpperCase(Self[Index]), Index)
      else
        FValueHash.Add(Self[Index], Index);
      FValueHashValid := True;
    end;
    if bNameHashValid then
    begin
      if FNameHash = nil then
        FNameHash := TVStringHash.Create(FBucketsSize);
      Key := Get(Index);
      P := AnsiPos(NameValueSeparator, Key);
      if P <> 0 then
      begin
        if not CaseSensitive then
          Key := AnsiUpperCase(Copy(Key, 1, P - 1))
        else
          Key := Copy(Key, 1, P - 1);
        FNameHash.Add(Key, Index);
      end;
      FNameHashValid := True;
    end;
  end else
    inherited;
end;

procedure TVHashedStringList.Put(Index: Integer; const S: String);
var
  bValueHashValid: Boolean;
  bNameHashValid: Boolean;
  Key, OldValue: String;
  P: Integer;
begin
  OldValue := Get(Index);
  if OldValue = S then Exit;
  bValueHashValid := FValueHashValid;
  bNameHashValid := FNameHashValid;
  inherited Put(Index, S);
  if bValueHashValid then
  begin
    if FValueHash = nil then
      FValueHash := TVStringHash.Create(FBucketsSize);
    if not CaseSensitive then
    begin
      FValueHash.Delete(AnsiUpperCase(OldValue), Index);
      FValueHash.Add(AnsiUpperCase(Self[Index]), Index);
    end else
    begin
      FValueHash.Delete(OldValue, Index);
      FValueHash.Add(Self[Index], Index);
    end;
    FValueHashValid := True;
  end;
  if bNameHashValid then
  begin
    if FNameHash = nil then
      FNameHash := TVStringHash.Create(FBucketsSize);
    P := AnsiPos(NameValueSeparator, OldValue);
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiUpperCase(Copy(OldValue, 1, P - 1))
      else
        Key := Copy(OldValue, 1, P - 1);
      FNameHash.Delete(Key, Index);
    end;
    Key := Get(Index);
    P := AnsiPos(NameValueSeparator, Key);
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiUpperCase(Copy(Key, 1, P - 1))
      else
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, Index);
    end;
    FNameHashValid := True;
  end;
end;

procedure TVHashedStringList.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      Add(Name + NameValueSeparator + Value)
    else
      Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TVHashedStringList.UpdateNameHash;
var
  I: Integer;
  P: Integer;
  Key: String;
begin
  if FNameHashValid then Exit;
  
  if FNameHash = nil then
    FNameHash := TVStringHash.Create(FBucketsSize)
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P := AnsiPos(NameValueSeparator, Key);
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiUpperCase(Copy(Key, 1, P - 1))
      else
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure TVHashedStringList.UpdateValueHash;
var
  I: Integer;
begin
  if FValueHashValid then Exit;

  if FValueHash = nil then
    FValueHash := TVStringHash.Create(FBucketsSize)
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    if not CaseSensitive then
      FValueHash.Add(AnsiUpperCase(Self[I]), I)
    else
      FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;

end.
