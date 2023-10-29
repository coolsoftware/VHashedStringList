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
    procedure Grow;
    function GetIntValue(const Name: string): Integer;
    procedure SetIntValue(const Name: string; const Value: Integer);
  protected
    procedure SetCapacity(NewCapacity: Integer); override;
    function GetInt(Index: Integer): Integer;
    procedure PutInt(Index: Integer; IntValue: Integer);
  public
    constructor Create(BucketsSize: Cardinal = 256);
    function AddInt(const S: string; IntValue: Integer): Integer; virtual;
    procedure InsertInt(Index: Integer; const S: string; IntValue: Integer); virtual;
    property IntValues[const Name: string]: Integer read GetIntValue write SetIntValue;
  end;

implementation

uses System.RTLConsts;

{ TVIntMap }

function TVIntMap.AddInt(const S: string; IntValue: Integer): Integer;
begin
  if not Sorted then
    Result := Count
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertInt(Result, S, IntValue);
end;

constructor TVIntMap.Create(BucketsSize: Cardinal);
begin
  inherited Create(False {OwnsObject}, BucketsSize);
end;

function TVIntMap.GetInt(Index: Integer): Integer;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);

end;

function TVIntMap.GetIntValue(const Name: string): Integer;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := GetInt(I) else
    Result := 0;
end;

procedure TVIntMap.Grow;
var
  LCapacity, LDelta: Integer;
begin
  LCapacity := Capacity;
  if LCapacity > 64 then LDelta := LCapacity div 4 else
    if LCapacity > 8 then LDelta := 16 else
      LDelta := 4;
  SetCapacity(LCapacity + LDelta);
end;

procedure TVIntMap.InsertInt(Index: Integer; const S: string;
  IntValue: Integer);
var
  LCount: Integer;
begin
  LCount := Count;
  if LCount = Capacity then Grow;
  FIntList[LCount] := IntValue;
  InsertItem(Index, S, @FIntList[LCount]);
end;

procedure TVIntMap.PutInt(Index, IntValue: Integer);
begin
  if Cardinal(Index) >= Cardinal(Count) then
    Error(@SListIndexError, Index);

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

procedure TVIntMap.SetIntValue(const Name: string; const Value: Integer);
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I < 0 then
    AddInt(Name, Value)
  else
    PutInt(I, Value);
end;

end.
