unit murmurhash;

interface

uses  VHashedStringList;

type
  TMurMurHash = class(TVStringHash)
  private
    function MurmurHash2LD(Key: PChar; len, seed: Cardinal): Cardinal;
  protected
    function HashOf(const Key: String): Cardinal; override;
  end;

implementation

function TMurMurHash.HashOf(const Key: String): Cardinal;
begin
  Result := MurmurHash2LD(PChar(Key), Length(Key), 0);
end;

function TMurMurHash.MurmurHash2LD(Key: PChar; len, seed: Cardinal): Cardinal;
//--------------------------------------------------------------------
// MurmurHash2, by Austin Appleby
//
// Translated from C to Delphi by Lionel Delafosse
// Comments from Austin Appleby code
//
// Additional improvements taken from Patrick van Logchem
// and Davy Landman respective implementations
//--------------------------------------------------------------------
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5bd1e995;
  r = 24;
var
  k : cardinal;
  EndPtr : PChar;
begin
  // Initialize the hash to a 'random' value
  Result := seed xor len;
  // Mix 4 bytes at a time into the hash
  EndPtr := Key + ((Len shr 2) shl 2);
  while Key < EndPtr do begin
    k := PCardinal(key)^ * m;  // k *= m;
    k := k xor (k shr r);      // k ^= k >> r;
    Result := Result * m;      // h *= m;
    Result := Result xor (k * m);    // h ^= (k * m);
    Inc(key, 4);
  end;

  if (Len and 1) <> 0
  then begin
    // length = 1 or 3
    if (Len and 2) <> 0
    then begin  // length = 3
      Result := Result xor PWord(key)^;
      Result := Result xor (Cardinal(PByte(key+2)^) shl 16);
    end
    else        // length = 1
      Result := Result xor PByte(key)^;
    Result := Result * m;
  end
  else begin  // length = 0 or 2
    if (Len and 2) <> 0
    then begin  // length = 2
      Result := Result xor PWord(key)^;
      Result := Result * m;
    end;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  Result := Result xor (Result shr 13);       // h ^= h >> 13;
  Result := Result * m;                       // h *= m;
  Result := Result xor (Result shr 15);       // h ^= h >> 15; return h;
end;

end.

