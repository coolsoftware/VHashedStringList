{ **************************************************************************** }
{                                                                              }
{ TVIntMap - string to integer map base on TVHashedStringList                  }
{                                                                              }
{ Created by Vitaly Yakovlev                                                   }
{ Date: October 29, 2023                                                       }
{ Copyright: (c) 2023 Vitaly Yakovlev                                          }
{                                                                              }
{ License: BSD 2-Clause License.                                               }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ 1. Redistributions of source code must retain the above copyright notice,    }
{    this list of conditions and the following disclaimer.                     }
{                                                                              }
{ 2. Redistributions in binary form must reproduce the above copyright notice, }
{    this list of conditions and the following disclaimer in the documentation }
{    and/or other materials provided with the distribution.                    }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,        }
{ THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR       }
{ PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR            }
{ CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,        }
{ EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,          }
{ PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;  }
{ OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,     }
{ WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      }
{ OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF       }
{ ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                   }
{                                                                              }
{ Last edit by: Vitaly Yakovlev                                                }
{ Date: October 29, 2023                                                       }
{ Version: 1.0                                                                 }
{                                                                              }
{ **************************************************************************** }

unit VIntMap;

interface

uses SysUtils, Classes, VHashedStringList;

type
  PInteger = ^Integer;
  TIntegerList = array of Integer;

  TVIntMap = class(TVHashedStringList)
  private
    FIntList: TIntegerList;
    procedure ExchangeIntItems(Index1, Index2: Integer);
    function GetIntValue(const S: string): Integer;
    procedure SetIntValue(const S: string; const Value: Integer);
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
  protected
    function DecInt(Index: Integer; ADec: Integer): Integer; virtual;
    function GetInt(Index: Integer): Integer; virtual;
    function IncInt(Index: Integer; AInc: Integer): Integer; virtual;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
    procedure PutInt(Index: Integer; AInt: Integer); virtual;
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    function AddIntValue(const S: string; AInt: Integer): Integer; virtual;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function DecIntValue(const S: String; ADec: Integer = -1): Integer; virtual;
    function IncIntValue(const S: String; AInc: Integer = 1): Integer; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); override;
    procedure SortByIntAsc; virtual;
    procedure SortByIntDesc; virtual;
    property IntValues[const S: string]: Integer read GetIntValue write SetIntValue;
    property IntValueFromIndex[Index: Integer]: Integer read GetInt write PutInt;
  end;

implementation

uses System.RTLConsts;

{ TVIntMap }

function TVIntMap.AddIntValue(const S: string; AInt: Integer): Integer;
begin
  Result := Add(S);
  FIntList[Result] := AInt;
end;

procedure TVIntMap.CustomSort(Compare: TStringListSortCompare);
var
  LCount: Integer;
begin
  LCount := Count;
  if not Sorted and (LCount > 1) then
  begin
    Changing;
    QuickSort(0, LCount - 1, Compare);
    Changed;
  end;
end;

function TVIntMap.DecInt(Index, ADec: Integer): Integer;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FIntList[Index] - ADec;
  FIntList[Index] := Result;
end;

function TVIntMap.DecIntValue(const S: String; ADec: Integer): Integer;
var
  I: Integer;
begin
  I := IndexOf(S);
  if I < 0 then begin
    Result := ADec;
    AddIntValue(S, Result);
  end
  else
    Result := DecInt(I, ADec);
end;

procedure TVIntMap.Delete(Index: Integer);
var
  LCount: Integer;
begin
  inherited;
  LCount := Count;
  if Index < LCount then begin
    System.Move(FIntList[Index + 1], FIntList[Index],
      (LCount - Index) * SizeOf(Integer));
  end;
end;

procedure TVIntMap.Exchange(Index1, Index2: Integer);
var
  LCount: Integer;
begin
  LCount := Count;
  if (Index1 < 0) or (Index1 >= LCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= LCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  ExchangeIntItems(Index1, Index2);
  Changed;
end;

procedure TVIntMap.ExchangeIntItems(Index1, Index2: Integer);
var
  Temp: Integer;
begin
  Temp := FIntList[Index1];
  FIntList[Index1] := FIntList[Index2];
  FIntList[Index2] := Temp;
end;

function TVIntMap.GetInt(Index: Integer): Integer;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FIntList[Index];
end;

function TVIntMap.GetIntValue(const S: string): Integer;
var
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
    Result := GetInt(I) else
    Result := 0;
end;

function TVIntMap.IncInt(Index, AInc: Integer): Integer;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  Result := FIntList[Index] + AInc;
  FIntList[Index] := Result;
end;

function TVIntMap.IncIntValue(const S: String; AInc: Integer): Integer;
var
  I: Integer;
begin
  I := IndexOf(S);
  if I < 0 then begin
    Result := AInc;
    AddIntValue(S, Result);
  end
  else
    Result := IncInt(I, AInc);
end;

procedure TVIntMap.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
var
  LCount: Integer;
begin
  LCount := Count;
  inherited InsertItem(Index, S, AObject);
  if Index < LCount then
    System.Move(FIntList[Index], FIntList[Index + 1],
      (LCount - Index) * SizeOf(Integer));
  FIntList[Index] := 0;
end;

procedure TVIntMap.PutInt(Index, AInt: Integer);
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);
  FIntList[Index] := AInt;
end;

procedure TVIntMap.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then begin
          ExchangeItems(I, J);
          ExchangeIntItems(I, J);
        end;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TVIntMap.SetCapacity(NewCapacity: Integer);
var
  LOldCapacity: Integer;
begin
  LOldCapacity := Capacity;
  inherited;
  if NewCapacity <> LOldCapacity then begin
    SetLength(FIntList, NewCapacity);
  end;
end;

procedure TVIntMap.SetIntValue(const S: string; const Value: Integer);
var
  I: Integer;
begin
  I := IndexOf(S);
  if I < 0 then
    AddIntValue(S, Value)
  else
    PutInt(I, Value);
end;

function VIntMapCompareIntsAsc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := (TVIntMap(List).FIntList[Index1] - TVIntMap(List).FIntList[Index2]);
end;

function VIntMapCompareIntsDesc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := (TVIntMap(List).FIntList[Index2] - TVIntMap(List).FIntList[Index1]);
end;

procedure TVIntMap.SortByIntAsc;
begin
  CustomSort(VIntMapCompareIntsAsc);
end;

procedure TVIntMap.SortByIntDesc;
begin
  CustomSort(VIntMapCompareIntsDesc);
end;

end.
