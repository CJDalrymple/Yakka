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


unit Bench;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, Common, GameDef, Search,CPU_Info;

procedure SpeedTest(Threads, HashSize, RunTime : integer; var PVS_Search : TSearch);


implementation

uses
  UCI;

{$CODEALIGN 16}

const
  SpeedTestGames: array[0..4] of string =
   ('e2e4 d7d5 b1c3 e7e6 g1f3 g8f6 f1c4 f8e7 e4d5 f6d5 e1g1 e8f8 d2d3 d5f6 f1e1 ' +
    'c7c5 c1f4 b8c6 c3e4 f6e4 d3e4 d8d1 a1d1 f8e8 c2c3 a7a6 c4e2 b7b5 a2a4 c5c4 a4b5 ' +
    'a6b5 b2b3 c8d7 b3c4 b5b4 f4d2 a8b8 f3d4 b4c3 d2c3 f7f6 d4b5 e7b4 c3b4 c6b4 d1d2 ' +
    'e8e7 f2f4 b4c6 g1f2 e6e5 g2g3 h8d8 e1a1 d7e6 b5a7 e6d7 a7c6 d7c6 d2d8 b8d8 f4e5 ' +
    'f6e5 a1a7 d8d7 a7a5 e7d6 c4c5 d6c7 f2e3 c7b8 e2b5 b8b7 b5d3 b7b8 a5a1 d7a7 a1f1 ' +
    'a7a3 f1f8 b8c7 e3d2 a3a2 d2e3 a2a3 e3e2 a3a2 e2f3 a2a3 f8f7 c7d8 f3e2 a3a2 e2f3 ' +
    'a2a3 f3e2 a3a2 e2e3 a2a3 e3d2 a3a2 d2c3 a2a3 c3c2 a3a2 c2b3 a2h2 f7g7 h7h6 g7g6 ' +
    'd8c7 g6g7 c7d8 g7g6 d8c7 b3b4 h2d2 b4c3 d2g2 c3b4 g2d2 b4c3 d2g2 c3c4 c6e8 g6g7',

    'd2d4 d7d5 b1c3 b8c6 c1f4 a7a6 e2e4 d5e4 d4d5 c6a7 ' +
    'd1e2 g8f6 e1c1 c8f5 h2h3 a7b5 c3b5 a6b5 e2b5 f5d7 b5b7 a8a2 c1b1 d8a8 b7a8 a2a8 ' +
    'f4c7 e7e6 d5e6 f7e6 g1e2 f8c5 e2d4 e8e7 c7e5 c5a7 f1c4 h8c8 b2b3 c8c5 f2f4 a7b8 ' +
    'e5b8 a8b8 b1c1 h7h6 h3h4 b8b6 d1e1 g7g6 g2g3 d7c8 h1g1 c8b7 g3g4 f6d7 f4f5 d7e5 ' +
    'g4g5 h6g5 f5g6 e5g6 g1g5 c5g5 h4g5 g6e5 e1h1 e4e3 h1h6 e5c4 b3c4 b6d6 c2c3 b7g2 ' +
    'c4c5 d6a6 d4f5 e7d7 f5e3 a6a1 c1d2 a1g1 d2d3 g2f3 h6g6 d7e7 d3d4 f3h1 d4e5 e7d7 ' +
    'e5f4 g1e1 g6h6 h1c6 h6h3 e1g1 h3h7 d7c8 c3c4 c6a4 h7e7 a4d7 e7g7 c8d8 g5g6 d7e8 ' +
    'e3g4 g1c1 g4e5 c1f1 f4e3 f1e1',

    'd2d4 d7d5 c2c4 e7e6 e2e4 h7h5 f1e2 g7g6 g1f3 f8g7 e1g1 g8e7 b1c3 e8g8 c4d5 ' +
    'e6d5 e4e5 c8g4 f1e1 b8c6 f3h4 d8d7 h2h3 g4e2 c3e2 e7f5 h4f5 d7f5 e2f4 f5d7 a2a4 ' +
    'c6d8 a1a3 d8e6 f4e6 d7e6 c1g5 f8e8 f2f4 f7f6 g5h4 f6e5 d4e5 e8f8 h4g5 c7c5 a3d3 ' +
    'd5d4 g2g4 h5g4 h3g4 f8f7 d3g3 a8f8 e1e4 a7a6 a4a5 f8e8 b2b3 b7b6 a5b6 e6b6 d1e2 ' +
    'f7b7 e2c2 a6a5 e4e1 b7d7 c2c4 b6e6 g1f2 e6c4 b3c4 d4d3 e1d1 e8f8 f2e1 d7d4 g3d3 ' +
    'd4c4 d3d7 c4e4 e1d2 f8b8 d2c3 e4e3 c3c4 g7f8 d7d6 a5a4 d6g6 g8f7 g6a6 b8b7 d1h1 ' +
    'b7b4 c4d5 b4d4 d5c6 f8g7 a6a7 f7g6 f4f5 g6g5 a7g7 g5f4 h1f1 f4g3 f5f6 g3g2 f6f7',

    'd2d4 d7d5 c2c4 g8f6 a2a4 b7b5 b1c3 b5a4 d1a4 c8d7 a4c2 d5c4 ' +
    'g1f3 b8c6 e2e4 c6b4 c2d1 e7e6 f1c4 f8e7 e1g1 e8g8 c1f4 a7a6 f1e1 d7b5 b2b3 c7c6 ' +
    'd1d2 d8d7 h2h3 d7b7 d2e2 f8e8 a1d1 f6d7 d1d2 e7f8 g1h1 d7b6 e1b1 a8c8 d2d1 f7f6 ' +
    'f4e3 b7f7 h1g1 h7h6 c3a2 b4a2 e2a2 c8a8 b1a1 e8b8 a2e2 f7e8 e2d3 a6a5 c4b5 c6b5 ' +
    'd4d5 a5a4 d5d6 b6d7 b3b4 e8h5 a1c1 a4a3 d3b3 a8a6 c1c7 h5e8 e3c5 a3a2 d1a1 f8d6 ' +
    'c5d6 a6d6 b3a2 b8c8 c7b7 c8b8 b7c7 b8c8 c7b7 c8b8 b7a7 e8c8 a2e2 b8b7 a7a8 b7b8',

    'f2f4 d7d5 e2e3 g8f6 b2b3 c8f5 c1b2 a7a6 b2f6 e7f6 g1f3 ' +
    'b8c6 d2d4 c6b4 f1d3 b4d3 c2d3 f8d6 b1c3 c7c6 e1g1 e8g8 d1d2 f8e8 c3e2 d8b6 e2g3 ' +
    'd6b4 d2c2 f5d7 c2f2 a8c8 e3e4 c6c5 a2a3 b4c3 d4c5 b6b3 f1b1 b3a4 a1a2 d7b5 e4d5 ' +
    'b5d3 b1c1 a4c4 d5d6 e8d8 g1h1 c4c5 f2c5 c8c5 g3e2 d3e2 a2e2 c3b4 c1d1 b4a5 e2e7 ' +
    'c5c6 e7b7 c6d6 d1d6 d8d6 g2g3 g7g6 h2h4 d6b6 b7d7 h7h5 d7d5 a5c3 d5d3 b6c6 h1g2 ' +
    'c3a5 d3d5 c6c2 g2h3 a5c7 d5d3 a6a5 f3d4 c2a2 d3c3 c7b6 d4c2 g8g7 c3c6 b6d8 f4f5 ' +
    'd8e7 g3g4 e7a3 f5g6 f7g6 g4h5 g6h5 h3g3 g7g6 g3f3 a3b2 c2e3 a5a4 c6c8 a2a1 c8g8 ' +
    'g6f7 g8a8 a4a3 f3e2 a1h1 a8a7 f7g6 a7a4 b2c1 e3d1 h1h2 d1f2 c1b2 e2e3 b2c3 a4a6 ' +
    'a3a2 a6a2 c3e1 e3d4');

   TimedPositionCount: array[0..4] of integer = (57, 52, 49, 41, 59);  // total positions = 258


type
  TSpeedTestRec = record

    ThreadCount : integer;
    TT_Size : integer;
    RunTime : integer;

    AvailableProcessors : integer;

    TotalDuration : UInt64;
    TotalNodes : UInt64;

    PositionCounter : integer;
    GameCounter : integer;
    end;


var
  TestBoard : TBoard;
  SpeedTestRec : TSpeedTestRec;
  MoveList : TGameMoveList;
  Clock : T_Timer;

  Old_OnSearchFinished : TNotifyEvent;
  Old_ThreadCount : integer;
  Old_MultiPVCount : integer;
  Old_TT_Size : integer;
  SpeedTestRunning : boolean;


procedure RestoreSearchState(PVS_Search : TSearch);
  begin
  PVS_Search.OnSearchFinished := Old_OnSearchFinished;
  PVS_Search.DoProgressUpdates := true;
  PVS_Search.SupressBestMove := false;
  PVS_Search.MultiPVCount := Old_MultiPVCount;
  PVS_Search.SetThreadCount(Old_ThreadCount);

  if PVS_Search.TransTable.Size <> Old_TT_Size then
    PVS_Search.TransTable.SetTableSize(Old_TT_Size);

  PVS_Search.NewGame;  // clears TranspositionTable etc

  SpeedTestRunning := false;
  end;


procedure TestUpdate(Sender : TObject);
  var
    MoveTime : Int64;
    MoveStr : Ansistring;
    S : PAnsiChar;
    isOK : boolean;
    LastPos : integer;

  begin
  if assigned(Sender) then
    begin
    SpeedTestRec.TotalNodes := SpeedTestRec.TotalNodes + TSearch(Sender).Search_Result.nodes;
    SpeedTestRec.TotalDuration := SpeedTestRec.TotalDuration + TSearch(Sender).Search_Result.time;

    LastPos := TimedPositionCount[SpeedTestRec.GameCounter] - 1;

    if SpeedTestRec.PositionCounter > LastPos then
      begin
      if SpeedTestRec.GameCounter < high(SpeedTestGames) then
        begin
        inc(SpeedTestRec.GameCounter);
        SpeedTestRec.PositionCounter := 0;

        LastPos := TimedPositionCount[SpeedTestRec.GameCounter] - 1;
        TSearch(Sender).NewGame;
        end
       else
        begin
        Clock.Stop;

        WriteMessage('Speed Test Finished >  ' + Clock.StopStr + #13#10);
        WriteMessage(' ===========================================');
        WriteMessage(' Version : ' + CurrentName + ' ' + CurrentVersion);
        WriteMessage(' ');
        WriteMessage(' Speedtest ' + IntToStr(SpeedTestRec.ThreadCount) + ' ' + IntToStr(SpeedTestRec.TT_Size) + ' ' + IntToStr(SpeedTestRec.RunTime));

        WriteMessage(' Thread count         : ' + IntToStr(TSearch(Sender).ThreadCount));
        WriteMessage(' TT size [MB]         : ' + IntToStr(TSearch(Sender).TransTable.TableSize div 65_536));
        WriteMessage(' Target duration      : ' + IntToStr(SpeedTestRec.Runtime) + ' (s)');
        WriteMessage(' ');
        WriteMessage(' Total nodes searched : ' + FormatFloat(',0' , SpeedTestRec.TotalNodes * 1.0));
        WriteMessage(' Total search time    : ' + FormatFloat(',0' , SpeedTestRec.TotalDuration * 1.0 ) + ' ms');
        if SpeedTestRec.TotalDuration > 0 then
          WriteMessage(' Nodes/second         : ' + FormatFloat(',0' , SpeedTestRec.TotalNodes * 1000 / SpeedTestRec.TotalDuration));
        WriteMessage(' ===========================================' + #13#10);

        RestoreSearchState((Sender as TSearch));
        exit;
        end;
      end;

    MoveStr := AnsiString(leftstr(SpeedTestGames[SpeedTestRec.GameCounter],  length(SpeedTestGames[SpeedTestRec.GameCounter]) - (LastPos - SpeedTestRec.PositionCounter) * 10));
    s := PAnsiChar(MoveStr);

    TestBoard.Reset;
    isOK := MakeMoves(TestBoard, MoveList, s);

    MoveTime := Trunc(50_000.0 / (TestBoard.TurnNumber + 15) * SpeedTestRec.RunTime / 304.92);

    inc(SpeedTestRec.PositionCounter);

    if isOK then
      begin
      TSearch(Sender).TimeLimit := MoveTime;
      TSearch(Sender).StartInBackGround(TestBoard, MoveList);
      end
     else
      begin
      RestoreSearchState(TSearch(Sender));
      end;
    end;

  end;


procedure TestSetup(Threads, HashSize, RunTime : integer; var PVS_Search : TSearch);
  var
    M : TMethod;
    version : integer;

  begin
  if assigned(PVS_Search) = false then
    PVS_Search := TSearch.Create;

  M.Code := @TestUpdate;
  M.Data := Pointer(PVS_Search);

  Old_OnSearchFinished := PVS_Search.OnSearchFinished;  // Used to reinstate after speed test is over
  Old_ThreadCount := PVS_Search.ThreadCount;
  Old_MultiPVCount := PVS_Search.MultiPVCount;
  Old_TT_Size := PVS_Search.TransTable.Size;

  PVS_Search.OnSearchFinished := TNotifyEvent(M);
  PVS_Search.DoProgressUpdates := false;
  PVS_Search.SupressBestMove := true;
  PVS_Search.OnMessage := WriteMessage;

  PVS_Search.MultiPVCount := 1;

  SpeedTestRec.PositionCounter := 0;
  SpeedTestRec.GameCounter := 0;

  SpeedTestRec.ThreadCount := Threads;
  PVS_Search.SetThreadCount(Threads);

  SpeedTestRec.TT_Size := HashSize;
  PVS_Search.SetTransTableSize(HashSize);

  SpeedTestRec.RunTime := RunTime;

  SpeedTestRec.TotalNodes := 0;
  SpeedTestRec.TotalDuration := 0;

  PVS_Search.NewGame;
  PVS_Search.Search_Result.nodes := 0;
  PVS_Search.Search_Result.time := 0;

  Clock.Start;
  WriteMessage(' ');
  WriteMessage('Speed Test Started  >  ' + Clock.StartStr);

  version := Detect_x86_64_level;

    case version of
      1  : WriteMessage('x86-64-v1 Supported >  Default x64');
      2  : WriteMessage('x86-64-v2 Supported >  SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT');
      3  : WriteMessage('x86-64-v3 Supported >  AVX, AVX2, BMI1, BMI2, FMA, LZCNT');
      4  : WriteMessage('x86-64-v4 Supported >  AVX512F, AVX512DQ, AVX512BW, AVX512VL, AVX512CD');
    end;

  if PVS_Search.Use_TB = true then
    WriteMessage('Tablebases in Use');

  WriteMessage('Expected Duration   >  ' + IntToStr(SpeedTestRec.Runtime) + ' (s)');
  TestUpdate(PVS_Search);
  end;


procedure SpeedTest(Threads, HashSize, RunTime : integer; var PVS_Search : TSearch);
  begin
  if SpeedTestRunning then
    begin
    WriteMessage('speedtest already running');
    exit;
    end;

  SpeedTestRunning := true;

  TestSetup(Threads, HashSize, RunTime, PVS_Search);
  end;


end.
