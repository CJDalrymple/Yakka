//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2026 Christopher Crone

//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:

//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.

//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHOR OR COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.


unit OpenBook;

interface

uses
  System.SysUtils, System.Classes, System.Character, System.Generics.Collections,
  System.Math, System.StrUtils, System.Types, System.SyncObjs, GameDef, common;

type
  T_MoveSelection = (ms_None, ms_Random, ms_MostFreq, ms_BestHist, ms_BestScore);

type
  T_OpeningBook = record
    private

    class var
      Hash_Array : array[0..8703] of UInt64;
      Index_Array : array[0..8703] of UInt32;
      Move_Array : array[0..12287] of UInt64;

      Hash_Count : integer;
      Move_Count : integer;
      IsInitializedFlag : array[0..0] of integer;  // 0 = uninitialized, -1 = initialized

      class function BinarySearch(key : UInt64) : integer;  static;
      class function LoadFromResource(ResourceName : string) : boolean;  static;

    public
      class function IsInitialized : boolean;  static;
      class procedure Prepare; static;
      class function GetMove(key : UInt64; var Move : TMove; MoveSelection : T_MoveSelection) : boolean;  static;

    end;


implementation

{$CODEALIGN 16}

var
  BookLock : TCriticalSection;

function Compare(const Left, Right: UInt64): Integer;
  begin
  if Left < Right then
    Result := -1
  else if Left > Right then
    Result := 1
  else
    Result := 0;
  end;


class function T_OpeningBook.IsInitialized : boolean;
  begin
  if IsInitializedFlag[0] = -1 then
    result := true
   else
    result := false;
  end;


class procedure T_OpeningBook.Prepare;
  begin
  BookLock.Enter;
    try
    if IsInitialized = false then
      begin
        try
        if LoadFromResource('Open_Book') = false then
          Hash_Count := 0;
        except
        Hash_Count := 0;                    // book unavailable - play on without it
        Move_Count := 0;
        end;

      IsInitializedFlag[0] := -1;           // published last, under the lock
      end;
    finally
    BookLock.Leave;
    end;
  end;


class function T_OpeningBook.BinarySearch(key : UInt64) : integer;

 // returns index if key is found, else -1 if not found

  var
    L, H, mid : integer;
    cmp : integer;

  begin
  if Hash_Count = 0 then
    Exit(-1);

  Result := -1;

  L := 0;
  H := Hash_Count - 1;

  while L <= H do
    begin
    mid := L + (H - L) div 2;
    cmp := Compare(Hash_Array[mid], key);
    if cmp < 0 then
      L := mid + 1
    else
      begin
      H := mid - 1;
      if cmp = 0 then
        Result := mid;
      end;
    end;
  end;


class function T_OpeningBook.GetMove(key : UInt64; var Move : TMove; MoveSelection : T_MoveSelection) : boolean;

  var
    index, moveCount, xxx, score : integer;
    expected, adjusted, best : double;
    i, n : integer;
    PRNG : TPRNG;

  begin
  if IsInitialized = false then
    Prepare;

  Move := 0;
  result := false;

  if (Hash_Count = 0) or (MoveSelection = ms_none) then
    Exit;

  PRNG.Randomize;
  index := BinarySearch(key);

  if index >= 0 then
    begin
    result := true;
    MoveCount := (Index_Array[index] shr 24) and UInt32($FF);
    index := Index_Array[index] and UInt32($FFFFFF);

      case MoveSelection of

      ms_Random :     // selects move at random with probability in proportion to popularity
          begin
          xxx := PRNG.random(256);

          i := 0;
          move := Move_Array[index];
          score := move.HalfMoveCount;

          while (score < xxx) and (i < MoveCount - 1) do
            begin
            inc(i);
            move := Move_Array[index + i];
            score := score + move.HalfMoveCount;
            end;
          end;

      ms_MostFreq :       // selects most popular move
          begin
          i := 0;
          move := Move_Array[index];
          score := move.HalfMoveCount;

          while i < MoveCount - 1 do
            begin
            inc(i);
            if score < Move_Array[index + i].HalfMoveCount then
              begin
              move := Move_Array[index + i];
              score := move.HalfMoveCount;
              end;
            end;
          end;

      ms_BestHist :    // selects move with best expected win/loss/draw history (adjusted for sample size)
          begin
          for i := 0 to MoveCount - 1 do
            begin
            move := Move_Array[index + i];
            if getbit(move, 44) = true then
              break;
            end;
          end;

      ms_BestScore :   // selects move with best score   (in case of tie, chooses most popular of tied moves)
          begin
          i := 0;
          move := Move_Array[index];
          score := move.score;

          while i < MoveCount - 1 do
            begin
            inc(i);
            if score < Move_Array[index + i].score then
              begin
              move := Move_Array[index + i];
              score := move.score;
              end;
            end;
          end;
      end;

    end;
  end;


class function T_OpeningBook.LoadFromResource(ResourceName : string) : boolean;
  var
    Input : TStringList;
    i, len : integer;
    Stream: TResourceStream;

  begin
  result := false;
  Hash_Count := 0;
  Move_Count := 0;

  Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
    Input := TStringList.Create;

      try
      Input.LoadFromStream(Stream);

      if Input.Count < 6 then
        exit;

      len := length(Input[2]);
      Hash_Count := StrToInt(trim(RightStr(Input[2], len - 23)));

      len := length(Input[3]);
      Move_Count := StrToInt(trim(RightStr(Input[3], len - 19)));

      if (Hash_Count < 0) or (Hash_Count > Length(Hash_Array)) or
         (Move_Count < 0) or (Move_Count > Length(Move_Array)) or
         (Input.Count < 6 + Hash_Count + Move_Count) then
        begin
        Hash_Count := 0;
        Move_Count := 0;
        exit;
        end;

      for i := 0 to Hash_Count - 1 do
        begin
        Hash_Array[i] := StrToUInt64(LeftStr(Input[5 + i], 17));
        Index_Array[i] := StrToUInt(RightStr(Input[5 + i], 9));
        end;

      for i := 0 to Move_Count - 1 do
        Move_Array[i] := StrToUInt64(LeftStr(Input[6 + Hash_Count + i], 17));

      result := true;
      finally

      Input.Free;
      end;

    finally
      Stream.Free;
    end;
  end;


initialization

  BookLock := TCriticalSection.Create;

finalization

  BookLock.Free;

end.
