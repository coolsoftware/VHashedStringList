unit TestHashedStringListUnit;

interface
uses
  DUnitX.TestFramework, SysUtils, Classes, VHashedStringList;

type

  [TestFixture]
  TTestVHashedStringList = class(TObject)
  protected
    class procedure Warning(const Msg: String);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    [TestCase('TestTStringList','False,1000,5000,100,True,True,False')]
    [TestCase('TestTVHashedStringList','True,1000,5000,100,True,True,False')]
//    [TestCase('TestTStringList(MurMurHash)','False,1000,5000,100,True,True,True')]
//    [TestCase('TestTVHashedStringList(MurMurHash)','True,1000,5000,100,True,True,True')]
    procedure Test1(const ATestTVHashedStringList: Boolean;
      const AListCount, AGetCount, ADeleteCount: Integer;
      const ACheckListItems, AAutoUpdateHash, AMurMurHash: Boolean);
  end;

implementation

uses DateUtils, murmurhash;

procedure TTestVHashedStringList.Setup;
begin
end;

procedure TTestVHashedStringList.TearDown;
begin
end;

procedure TTestVHashedStringList.Test1(const ATestTVHashedStringList: Boolean;
  const AListCount, AGetCount, ADeleteCount: Integer; const ACheckListItems,
  AAutoUpdateHash, AMurMurHash: Boolean);
var
  i, j, n: Integer;
  s, t: String;
  lst: TStringList;
  t0, t1: TDateTime;
begin
  Randomize;
  SetLength(s, 8);
  SetLength(t, 16);
  if ATestTVHashedStringList then begin
    lst := TVHashedStringList.Create;
    TVHashedStringList(lst).AutoUpdateHash := AAutoUpdateHash;
    if AMurMurHash then begin
      TVHashedStringList(lst).NameHashClass := TMurMurHash;
    end;
  end
  else begin
    lst := TStringList.Create;
  end;
  try
    t0 := now;
    for i := 1 to AListCount do
    begin
      for j := 1 to Length(s) do begin
        s[j] := Chr(Ord('a')+Random(26));
      end;
      for j := 1 to Length(t) do begin
        t[j] := Chr(Ord('a')+Random(26));
      end;
      lst.Values[s] := t;
    end;
    t1 := now;
    Warning(Format('Fill list with %d items time: %d', [AListCount, MilliSecondsBetween(t0, t1)]));
    t0 := now;
    for i := 1 to AGetCount do begin
      for j := 1 to Length(s) do begin
        s[j] := Chr(Ord('a')+Random(26));
      end;
      t := lst.Values[s];
    end;
    t1 := now;
    Warning(Format('Get %d list items time:       %d', [AGetCount, MilliSecondsBetween(t0, t1)]));
    if ACheckListItems then begin
      t0 := now;
      for i := 0 to lst.Count-1 do begin
        s := lst.Names[i];
        t := lst.Values[s];
        Assert.AreEqual(lst.ValueFromIndex[i], t);
      end;
      t1 := now;
      Warning(Format('Check %d list items time:     %d', [lst.Count, MilliSecondsBetween(t0, t1)]));
    end;
    t0 := now;
    n := lst.Count;
    for i := 1 to ADeleteCount do begin
      if lst.Count = 0 then Break;
      j := Random(lst.Count);
      s := lst.Names[j];
      lst.Values[s] := '';
    end;
    Assert.AreEqual(n, lst.Count + ADeleteCount);
    t1 := now;
    Warning(Format('Delete %d list items time:     %d', [ADeleteCount, MilliSecondsBetween(t0, t1)]));
  finally
    lst.Free;
  end;
end;

class procedure TTestVHashedStringList.Warning(const Msg: String);
begin
  TDUnitX.CurrentRunner.Log(TLogLevel.Warning, Msg);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVHashedStringList);
end.
