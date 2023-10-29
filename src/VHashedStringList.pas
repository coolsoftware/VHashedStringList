unit VHashedStringList;

(*
Copyright 2012 Vitaly Yakovlev. http://blog.coolsoftware.ru/

TVHashedStringList - improved hashed string list.

You can freely use this program and code for your needs.

Please, don't remove this copyright.
*)

interface

uses SysUtils, Classes, Windows;

{$IF Not Declared(CompilerVersion) Or (CompilerVersion < 20)}
//Delphi version is older than 2009
//Compiler versions: https://docwiki.embarcadero.com/RADStudio/Sydney/en/Compiler_Versions
  {$DEFINE NoOwnsObjects}
{$IFEND}

type
  PPVHashItem = ^PVHashItem;
  PVHashItem = ^TVHashItem;
  TVHashItem = packed record
    Next: PVHashItem;
    Key: String;
    Value: Integer;
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
    function Delete(const Key: String; Value: Integer): Boolean;
    procedure DecValues(Value: Integer);
    procedure IncValues(Value: Integer);
    function ValueOf(const Key: String): Integer;
  end;

  TVStringHashClass = class of TVStringHash;

  TVHashedStringList = class(TStringList)
  private
    FBucketsSize: Cardinal;
    FValueHash: TVStringHash;
    FNameHash: TVStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
{$IFDEF NoOwnsObjects}
    FOwnsObjects: Boolean;
{$ENDIF}
    FAutoUpdateHash: Boolean;
    FValueHashClass: TVStringHashClass;
    FNameHashClass: TVStringHashClass;
    FKeepEmptyValues: Boolean;
    function GetValue(const Name: String): String;
    procedure SetValue(const Name, Value: String);
    function GetValueObject(const Name: String): TObject;
    procedure SetValueObject(const Name: String; AValue: TObject);
  public
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
    procedure InsertItem(Index: Integer; const S: String; AObject: TObject); override;
    procedure Put(Index: Integer; const S: String); override;
{$IFDEF NoOwnsObjects}
    procedure PutObject(Index: Integer; AObject: TObject); override;
{$ENDIF}
  public
    constructor Create(BucketsSize: Cardinal = 256); overload;
{$IFNDEF NoOwnsObjects}
    constructor Create(OwnsObjects: Boolean; BucketsSize: Cardinal = 256); overload;
{$ENDIF}
    destructor Destroy; override;
{$IFDEF NoOwnsObjects}
    procedure Clear; override;
{$ENDIF}
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: String): Integer; override;
    function IndexOfName(const Name: String): Integer; override;
    property Values[const Name: String]: String read GetValue write SetValue;
    property ValueObjects[const Name: String]: TObject read GetValueObject write SetValueObject;
{$IFDEF NoOwnsObjects}
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
{$ENDIF}
    property AutoUpdateHash: Boolean read FAutoUpdateHash write FAutoUpdateHash;
    property ValueHashClass: TVStringHashClass read FValueHashClass write FValueHashClass;
    property NameHashClass: TVStringHashClass read FNameHashClass write FNameHashClass;
    property KeepEmptyValues: Boolean read FKeepEmptyValues write FKeepEmptyValues;
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
  Bucket^.Key := Key;
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

procedure TVStringHash.DecValues(Value: Integer);
var
  P: PVHashItem;
  I: Integer;
begin
  for I := 0 to Length(FBuckets) - 1 do
  begin
    P := FBuckets[I];
    while P <> nil do
    begin
      if P^.Value > Value then
        Dec(P^.Value);
      P := P^.Next;
    end;
  end;
end;

procedure TVStringHash.IncValues(Value: Integer);
var
  P: PVHashItem;
  I: Integer;
begin
  for I := 0 to Length(FBuckets) - 1 do
  begin
    P := FBuckets[I];
    while P <> nil do
    begin
      if P^.Value > Value then
        Inc(P^.Value);
      P := P^.Next;
    end;
  end;
end;

{ TVHashedStringList }

procedure TVHashedStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

{$IFDEF NoOwnsObjects}
procedure TVHashedStringList.Clear;
var
  I: Integer;
begin
  if FOwnsObjects then
  begin
    for I := 0 to Count-1 do
      PutObject(I, nil);
  end;
  inherited Clear;
end;
{$ENDIF}

constructor TVHashedStringList.Create(BucketsSize: Cardinal);
begin
{$IFDEF NoOwnsObjects}
  inherited Create;
{$ELSE}
  Create(False, BucketsSize);
end;

constructor TVHashedStringList.Create(OwnsObjects: Boolean; BucketsSize: Cardinal);
begin
  inherited Create(OwnsObjects);
{$ENDIF}
  FBucketsSize := BucketsSize;
  FValueHash := nil;
  FNameHash := nil;
  FValueHashValid := False;
  FNameHashValid := False;
{$IFDEF NoOwnsObjects}
  FOwnsObjects := False;
{$ENDIF}
  FAutoUpdateHash := True;
  FValueHashClass := TVStringHash;
  FNameHashClass := TVStringHash;
  FKeepEmptyValues := False;
end;

procedure TVHashedStringList.Delete(Index: Integer);
var
  bValueHashValid: Boolean;
  bNameHashValid: Boolean;
  Key, OldValue: String;
  P: Integer;
begin
{$IFDEF NoOwnsObjects}
  if FOwnsObjects then
    PutObject(Index, nil);
{$ENDIF}
  if FAutoUpdateHash then
  begin
    OldValue := Get(Index);
    bValueHashValid := FValueHashValid;
    bNameHashValid := FNameHashValid;
    inherited;
    if bValueHashValid then
    begin
      if FValueHash = nil then
        FValueHash := FValueHashClass.Create(FBucketsSize);
      if not CaseSensitive then
      begin
        FValueHash.Delete(AnsiUpperCase(OldValue), Index);
      end else
      begin
        FValueHash.Delete(OldValue, Index);
      end;
      FValueHash.DecValues(Index);
      FValueHashValid := True;
    end;
    if bNameHashValid then
    begin
      if FNameHash = nil then
        FNameHash := FNameHashClass.Create(FBucketsSize);
      P := AnsiPos(NameValueSeparator, OldValue);
      if P <> 0 then
      begin
        if not CaseSensitive then
          Key := AnsiUpperCase(Copy(OldValue, 1, P - 1))
        else
          Key := Copy(OldValue, 1, P - 1);
        FNameHash.Delete(Key, Index);
        FNameHash.DecValues(Index);
      end;
      FNameHashValid := True;
    end;
  end else
    inherited;
end;

destructor TVHashedStringList.Destroy;
{$IFDEF NoOwnsObjects}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF NoOwnsObjects}
  if FOwnsObjects then
  begin
    for I := 0 to Count-1 do
      PutObject(I, nil);
  end;
{$ENDIF}
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

function TVHashedStringList.GetValueObject(const Name: String): TObject;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := GetObject(I) else
    Result := nil;
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
  if FAutoUpdateHash and (Index = Count) then
  begin
    //if add new item then update hash
    bValueHashValid := FValueHashValid;
    bNameHashValid := FNameHashValid;
    inherited;
    if bValueHashValid then
    begin
      if FValueHash = nil then
        FValueHash := FValueHashClass.Create(FBucketsSize);
      if not CaseSensitive then
        FValueHash.Add(AnsiUpperCase(Self[Index]), Index)
      else
        FValueHash.Add(Self[Index], Index);
      FValueHashValid := True;
    end;
    if bNameHashValid then
    begin
      if FNameHash = nil then
        FNameHash := FNameHashClass.Create(FBucketsSize);
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
  if FAutoUpdateHash then
  begin
    bValueHashValid := FValueHashValid;
    bNameHashValid := FNameHashValid;
    inherited Put(Index, S);
    if bValueHashValid then
    begin
      if FValueHash = nil then
        FValueHash := FValueHashClass.Create(FBucketsSize);
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
        FNameHash := FNameHashClass.Create(FBucketsSize);
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
  end else
    inherited;
end;

{$IFDEF NoOwnsObjects}
procedure TVHashedStringList.PutObject(Index: Integer; AObject: TObject);
var
  obj: TObject;
begin
  if FOwnsObjects then
  begin
    obj := GetObject(Index);
    if Assigned(obj) then obj.Free;
  end;
  inherited PutObject(Index, AObject);
end;
{$ENDIF}

procedure TVHashedStringList.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if (Value <> '') or FKeepEmptyValues then
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

procedure TVHashedStringList.SetValueObject(const Name: String;
  AValue: TObject);
var
  I: Integer;
begin
  I := IndexOf(Name);
  if AValue <> nil then
  begin
    if I < 0 then
      AddObject(Name, AValue)
    else
      PutObject(I, AValue);
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
    FNameHash := FNameHashClass.Create(FBucketsSize)
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
    FValueHash := FValueHashClass.Create(FBucketsSize)
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

