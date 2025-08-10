//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2025 Christopher Crone

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


unit Search;

interface

 {$A16}

uses
  Winapi.Windows, SysUtils, System.SyncObjs, System.Classes, Math, Threading, Diagnostics,
  Common, GameDef, GameNet, Openbook;

  {$IF defined(MSWINDOWS)}
  function GenRandom(pbBuffer :PBYTE; dwLen: DWORD):BOOL; stdcall;
  {$ENDIF}

const

  ADVAPI32    = 'advapi32.dll';

  PV_Node = 0;
  CUT_Node = 1;
  ALL_Node = -1;

  ftUnknown = 0;
  ftUpperBound = 1;
  ftLowerBound = 2;
  ftExact = 3;

  DoNull = true;
  OmitNull = false;

  Singular = true;
  NotSingular = false;

  Parallel = true;
  Sequential = false;

  MaxSearchPly = 63;

  ScoreMinValue = -32767 + MaxSearchPly * 2;         // -2^15 + 1 - margin
  ScoreMaxValue =  32767 - MaxSearchPly * 2;         //  2^15 - 1 + margin

  MateScoreCutoff = ScoreMaxValue - MaxSearchPly * 4;

  ScoreOffset = 32768;                               //  2^15

  InfiniteValue = 65535;                             // 2^16 - 1
  InvalidScore = -InfiniteValue;
  InvalidMove = 0;

  GuardValue = 31415962;
  DrawFound  = 27182818;



  TimeLimit_Max = Int64($4000000000);    // 2^38       to prevent overflow when calculating tick limit
  NodeLimit_Max = Int64($4000000000);    // 2^38

              // MVV             LVA
              // captured piece, piece

  MVV_LVA_Lookup : array[0..queen, pawn..king] of integer =
           (( 0,  0,  0,  0,  0,  0),
            ( 5,  4,  4,  3,  2,  1),
            (10,  9,  9,  8,  7,  6),
            (10,  9,  9,  8,  7,  6),
            (15, 14, 14, 13, 12, 11),
            (20, 19, 19, 18, 17, 16));

type
  TGameMoveList = record
    var
     MoveArray : array[0..639] of TMove;

    class operator Initialize(out Dest: TGameMoveList);
    function Count : integer;
    procedure Clear;
    procedure AddMove(Move : TMove);
    function RemoveLastMove : TMove;
    end;


type
  TUpdateRec = record

    BestScore : integer;
    BestMove : TMove;
    BestPV : TMoveArray;
    SelDepth : integer;
    FirstRootMoveSearched : boolean;
    end;


type
  T_ABDADA_Table = record
    const
      TableSize = $2000;             // 2^13       = 8192
      TableMask = TableSize - 1 ;    // 2^13 - 1

    var
      Table : Array[0..TableSize-1] of UInt64;

    procedure MoveSearchBegin(MoveHash : UInt64);
    procedure MoveSearchOver(MoveHash : UInt64);
    function SearchInProgress(MoveHash : UInt64) : boolean;
    procedure ClearTable;
    end;


type
  TKillerMoveTable = array[0..MaxSearchPly + 8] of TMove;

type
  TRepList = array[0..MaxSearchPly + 8] of UInt64;        // first value array[0] = number of entries

type
  THistList = array[0..MaxSearchPly + 8] of Integer;        // first value array[0] = hash value at ply = 0, etc

type
  TSignatureList = array[0..1023] of UInt64;             // first value array[0] = number of entries

type
  TTransRec = record
    UID : UInt64;
    Data : UInt64;
    end;

type
  TTransTable = Class
    const
      DefaultTableSize = UInt64($0400000);  // 2^22  entries = (2^22 x 16 bytes per entry) = 67,108,864 bytes = 64 MB

    var
      Table : array of TTransRec;

      TableSize : UInt64;
      TableMask : UInt64;

      SearchAge : Int64;  // SearchAge : [0..255]

    constructor Create;
    procedure SetTableSize(MB_Size : integer);

    function Score(Info : UInt64; ply : integer) : integer;   inline;
    function Source(Info : UInt64) : integer;                 inline;
    function Dest(Info : UInt64) : integer;                   inline;
    function Depth(Info : UInt64) : integer;                  inline;
    function Flag(Info : UInt64) : integer;                   inline;
    function Age(Info : UInt64) : integer;                    inline;
    function Priority(Info : UInt64) : integer;               inline;
    function Move(Info : UInt64) : TMove;                     inline;

    function PackInfo(const Move : TMove; Score, Depth, Flag, Ply : integer) : UInt64;

    function RetrieveData(HashCode : UInt64; var Data : UInt64) : boolean;
    procedure StoreData(HashCode : UInt64; Data : UInt64);

    function FillPermill : integer;   //   100% = 1000
    procedure ClearTable;
    procedure NewSearch;
    end;


type
  THistoryTable = record
    var
      Table : array[0..1, 1..6, 0..63] of integer;
      IsEmpty : boolean;

    procedure Update(Player : integer; const Move : TMove; Adjustment : integer);
    procedure ClearTable;
    procedure DampTable;
    end;


type
 TCounterMoveTable = record
    var
      Table : array[1..6, 0..63] of TMove;
      IsEmpty : boolean;

    function Probe(Move : TMove) : TMove;
    procedure Update(const PrevMove, CounterMove : TMove);
    procedure ClearTable;
    end;


type
  TSearchProc = reference to procedure(x : integer);


type
  T_ThreadData = record
    var
      RepList : TRepList;

      KillerMoveA : TKillerMoveTable;
      KillerMoveB : TKillerMoveTable;

      NodeCount, PrevCount : Int64;
      SearchTime : UInt64;
      selDepth : integer;

      FirstRootMoveSearched : boolean;
      ID : integer;

      EvalHist : THistList;

    procedure Reset;
    end;


type
  T_ThreadDataPtr = ^T_ThreadData;


type
 TSearchData = record
   Depth : integer;
   selDepth : integer;
   nodes : UInt64;
   time : UInt64;
   PV : TMoveArray;
   Score : integer;
   ScoreType : integer;
   MessageOut : string;
   end;


type
  TSearchFinishedProc = procedure(SearchUpdate : TSearchData);

type
  TMessageOutProc = procedure(const msg : string);

type
  TSearch = class

      TransTable : TTransTable;
      ABDADA_Table : T_ABDADA_Table;

      SignatureList : TSignatureList;

      QuietHistory : THistoryTable;

      WhiteCounterMove : TCounterMoveTable;
      BlackCounterMove : TCounterMoveTable;

      StartTick : Int64;
      TickFrequency : Int64;
      TickLimit : Int64;

      Searching : Boolean;
      Stop : Boolean;
      ShutDownUnderway : Boolean;
      ReadyToSearch :Boolean;

      ThreadPool: TThreadPool;

      // Search Information

      NodeCount : Int64;
      SearchTime_ms : Int64;

      // Search Parameters

      TimeLimit : Int64;
      BudgetTime : Int64;                      // if time control then to calculate based on remaing time and estimated remaining moves
      SoftTimeLimit : boolean;
      IsPondering : boolean;

      NodeLimit : Int64;                      // UCI:   nodes <x> search x nodes only
      DepthLimit : integer;                   // UCI:   depth <x> search x plies only
      ThreadCount : integer;                  // UCI:   option name Threads type spin default 1 min 1 max 32

      // Search Options:

      DoProgressUpdates : boolean;
      DoParallelSearch : boolean;
      DoAspirationSearch : boolean;

      UseOwnBook : boolean;
      ShowWDL : boolean;
      MultiPV : boolean;
      WillPonder : boolean;

      // Search Outcome

      Search_Result : TSearchData;
      BestMove : UInt64;

      FOnSearchProgressUpdate : TNotifyEvent;
      FOnSearchFinished : TSearchFinishedProc;
      FOnMessageOut : TMessageOutProc;

      constructor Create;
      destructor Destroy; override;

      procedure SetDefault;
      procedure NewGame;
      procedure PrepareForNextSearch;

      procedure StartInBackGround(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure FullSearch(const Board : TBoard; const GameMoveList : TGameMoveList);
      function BookMoveFound(const Board : TBoard) : boolean;

      procedure ABDADA_Search(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure ABDADA_Search_Threaded(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure ABDADA_Search_NonThreaded(const Board : TBoard; const GameMoveList : TGameMoveList);

      procedure ClearHistoryTables;
      procedure ClearCounterMoveTables;
      procedure DampHistoryTables;
      procedure SetTransTableSize(size : integer);
      procedure SetThreadCount(value : integer);
      procedure ClearTranspositionTable;
      procedure Clear_ABDADA_Table;
      procedure LoadSignatureList(const Board : TBoard; const GameMoveList : TGameMoveList);

     public
      property OnSearchFinished: TSearchFinishedProc read FOnSearchFinished write FOnSearchFinished;
      property OnProgressUpdate: TNotifyEvent read FOnSearchProgressUpdate write FOnSearchProgressUpdate;

      function GetElapsedTicks : Int64;
      function GetElapsedMilliseconds : Int64;

      procedure StopSearch;
      procedure Shutdown;

      function ResultToStr : string;

      property OnMessage : TMessageOutProc read FOnMessageOut write FOnMessageOut;

    end;

type
  TSearchPtr = ^TSearch;

procedure RateMovesQ(var moves : TMoveArray);
procedure SortMoves(var moves : TMoveArray);

function UInt64ToDouble(value : UInt64) : double;

function ExtendTime(BudgetTime, HardLimit : UInt64) : UInt64;
function AllocateTimeForSearch(MovesToGo, TimeRemaining, Increment, OpponentTimeRemaining, OpponentIncrement, MoveNumber : UInt64; WillPonder : boolean) : UInt64;

function IsMateScore(Score : integer) : boolean;


implementation

{$CODEALIGN 16}


{$IF defined(MSWINDOWS)}
function GenRandom; external ADVAPI32 name 'SystemFunction036';
{$ENDIF}


var
  Book : T_OpeningBook;


// Transposition Table ===============================================================

  //    TTData                                MoveData
  //
  //    Source Cell      :   bits 0..5     :  Source Cell
  //    Dest Cell        :   bits 6..11    :  Dest Cell
  //                     :   bits 12..15   :  Moved Piece Type
  //                     :   bits 16..19   :  Captured Piece Type
  //                     :   bits 20..23   :  Promoted Piece Type
  //    Flag             :   bits 24..29   :  Castling Rights i.e. 6 bits indicating if 4 rooks & 2 kings have moved
  //    Depth            :   bits 30..35   :  Enpassant Cell
  //    Age              :   bits 36..43   :  Half Move Count
  //                         bit  44       :  Used during search to track LMR candidates when moves are swapped
  //    Score            :   bits 48..63   :  Score


constructor TTransTable.Create;

  begin
  inherited Create;
  TableSize := DefaultTableSize;

  Setlength(Table, TableSize);
  TableMask := UInt64(TableSize-2);

  ClearTable;
  end;


procedure TTransTable.SetTableSize(MB_size : integer);
  // size is the value in MB for memory for hash tables

  // min 2^16 entries =   1,048,576 bytes =   (1 MB)
  // max 2^24 entries = 268,435,456 bytes =  (256 MB)

  // actual size is largest factor of 2 that is lesser than or equal to size
  // default is   64 MB = 2^22 entries, min is 1 MB = 2^16 entries, max is 256 MB = 2^24 entries

  begin
  TableSize := UInt64($00010000);        // min size = 2^16 entries (x 16 bytes per entry) = 1,048,576 bytes = 1 MB

  while TableSize < MB_size * 65_536 do    // 65,536 entries per MB = 1,048,576 / 16
    TableSize := TableSize * 2;

  TableSize := min(UInt64($01000000), TableSize);    // max size = 2^24 entries (x 16 bytes per entry) = 268,435,456 bytes = 256 MB

  Setlength(Table, TableSize);
  TableMask := UInt64(TableSize-2);

  ClearTable;
  end;


function TTransTable.Source(Info : UInt64) : integer;
  begin
  result := integer(Info and $3F);
  end;


function TTransTable.Dest(Info : UInt64) : integer;
  begin
  result := integer((Info shr 6) and $3F);
  end;


function TTransTable.Flag(Info : UInt64) : integer;
  begin
  result := integer((Info shr 24) and $3F);
  end;


function TTransTable.Depth(Info : UInt64) : integer;
  begin
  result := integer((Info shr 30) and $3F);
  end;


function TTransTable.Age(Info : UInt64) : integer;
  begin
  result := integer((Info shr 36) and $FF);
  end;


function TTransTable.Priority(Info : UInt64) : integer;
  begin
  result := integer((Info shr 24) and $FFFFF);
  end;


function TTransTable.Score(Info : UInt64; Ply : integer) : integer;
  begin
  result := integer(Info shr 48) - ScoreOffset;

  if result >= MateScoreCutoff then          // retrieving Mate score from TT, so subtract ply
    result := result - ply
   else if result <= -MateScoreCutoff then   // retrieving Mated score from TT, so add ply
    result := result + ply;
  end;


function TTransTable.Move(Info : UInt64) : TMove;
  begin
  result := Info and $0000000000FFFFFF;
  end;


function TTransTable.PackInfo(const Move : TMove; Score, Depth, Flag, Ply : integer) : UInt64;
  var
    StoredScore : integer;

  begin
  storedScore := Score;
  if score >= MateScoreCutoff then
    storedscore := score + ply              // saving Mate score to TT, so add ply
   else if score <= -MateScoreCutoff then
    storedscore := score - ply;             // saving Mated score to TT, so subtract ply

  storedScore := storedScore + ScoreOffset;

  result := (UInt64(StoredScore and $FFFF) shl 48) or
            (UInt64(SearchAge and $FF) shl 36) or
            (UInt64(Depth and $3F) shl 30) or
            (UInt64(Flag and $3F) shl 24) or
            (Move and $0000000000FFFFFF);
  end;


procedure TTransTable.StoreData(HashCode : UInt64; Data : UInt64);
  var
    index1, UID_1, Data_1 : UInt64;
    index2, UID_2, Data_2 : UInt64;
    Data_Score, Data1_Score, Data2_Score : integer;

  begin
  index1 := (HashCode and TableMask);     // index of first slot

  UID_1 :=  Table[Index1].UID;
  Data_1 := Table[Index1].Data;

  Data_Score :=  integer(Data shr 48) - ScoreOffset;

  if (UID_1 xor Data_1) = HashCode then
    begin
    Data1_Score := integer(Data_1 shr 48) - ScoreOffset;

    if (Data and UInt64($FFFFFF)) = 0 then             // if new Data doesn't have a move, then keep previous move
      Data := Data or (Data_1 and UInt64($FFFFFF));

    if Depth(Data_1) = Depth(Data) then
      begin
      if ((Data_Score > Data1_Score) and (Flag(Data) <> ftUpperbound)) or
         ((Data_Score < Data1_Score) and (Flag(Data) <> ftLowerbound)) or
         ((Data_Score >= MateScoreCutoff) and (Data_Score > Data1_Score)) then
            begin
            Table[index1].UID := HashCode xor Data;
            Table[index1].Data := Data;
            exit;
            end;

      if Data_Score = Data1_Score then
        begin
        if Flag(Data) <> Flag(Data_1) then
          Data := (Data and $FFFFFFFFC0FFFFFF) or (UInt64(ftExact and $3F) shl 24);
        Table[index1].UID := HashCode xor Data;
        Table[index1].Data := Data;
        exit;
        end;
      end;

    if (Priority(Data_1) <= Priority(Data)) or ((abs(Data_Score) >= MateScoreCutoff) and (Data_Score > Data1_Score)) then
      begin
      Table[index1].UID := HashCode xor Data;
      Table[index1].Data := Data;
      exit
      end;

    exit;   // don't overwrite a better entry so exit
    end;

  index2 := index1 + 1;
  UID_2 :=  Table[Index2].UID;
  Data_2 := Table[Index2].Data;

  if (UID_2 xor Data_2) = HashCode then
    begin
    Data2_Score := integer(Data_2 shr 48) - ScoreOffset;

    if (Data and UInt64($FFFFFF)) = 0 then             // if new Data doesn't have a move, then keep previous move
      Data := Data or (Data_2 and UInt64($FFFFFF));

    if Depth(Data_2) = Depth(Data) then
      begin
      if ((Data_Score > Data2_Score) and (Flag(Data) <> ftUpperbound)) or
         ((Data_Score < Data2_Score) and (Flag(Data) <> ftLowerbound)) or
         ((Data_Score >= MateScoreCutoff) and (Data_Score > Data2_Score)) then
            begin
            Table[index2].UID := HashCode xor Data;
            Table[index2].Data := Data;
            exit;
            end;

      if Data_Score = Data2_Score then
        begin
        if Flag(Data) <> Flag(Data_2) then
          Data := (Data and $FFFFFFFFC0FFFFFF) or (UInt64(ftExact and $3F) shl 24);
        Table[index2].UID := HashCode xor Data;
        Table[index2].Data := Data;
        exit;
        end;
      end;

    if (Priority(Data_2) <= Priority(Data)) or ((abs(Data_Score) >= MateScoreCutoff) and (Data_Score > Data2_Score)) then
      begin
      Table[index2].UID := HashCode xor Data;
      Table[index2].Data := Data;
      exit
      end;

    exit;   // don't overwrite a better entry so exit
    end;

  // replace the entry with lowest priority
  if Priority(Data_1) >= Priority(Data_2) then
    begin
    Table[index1].UID := HashCode xor Data;
    Table[index1].Data := Data;
    end
   else
    begin
    Table[index2].UID := HashCode xor Data;
    Table[index2].Data := Data;
    end;
  end;


function TTransTable.RetrieveData(HashCode : UInt64; var Data : UInt64) : boolean;
  var
    index : integer;
    UID : UInt64;

  begin
  result := true;
  index := (HashCode and TableMask);        // first slot

  UID := Table[index].UID;
  Data := Table[index].Data;

  if (UID xor Data) <> HashCode then
    begin
    inc(index);                          // second slot
    UID := Table[index].UID;
    Data := Table[index].Data;
    if (UID xor Data) <> HashCode then
      result := false;
    end;
  end;


procedure TTransTable.ClearTable;
  var
    i : integer;

  begin
  SearchAge := 0;

  for i := 0 to TableSize - 1 do
    begin
    Table[i].UID := InvalidBoard;
    Table[i].Data := 0;
    end;
  end;


procedure TTransTable.NewSearch;
  begin
  inc(SearchAge);
  SearchAge := SearchAge and $FF;
  end;


function TTransTable.FillPermill : integer;
  var
    i, FillCount : integer;

  begin
  FillCount := 0;

  for i := 0 to 1023 do
    if Age(Table[i].Data) = SearchAge then
      inc(FillCount);

  result := (FillCount * 1000) div 1024;
  end;


// History Table ===============================================================


procedure THistoryTable.Update(Player : integer; const Move : TMove; Adjustment : integer);
  var
    Piece, Dest : integer;

  begin
  IsEmpty := false;
  Piece := (Move shr 12) and $F;
  Dest :=  (Move shr 6) and $3F;

  Table[Player, Piece, Dest] := Table[Player, Piece, Dest] + ((32768 - abs(Table[Player, Piece, Dest])) * Adjustment) div 32768;
  end;


procedure THistoryTable.ClearTable;
  var
    Player, Piece, Dest : integer;

  begin
  IsEmpty := true;
  for Player := 0 to 1 do
    for Piece := 1 to 6 do
      for Dest := 0 to 63 do
        Table[Player, Piece, Dest] := 0;
  end;


procedure THistoryTable.DampTable;
  var
    Player, Piece, Dest : integer;

  begin
  for Player := 0 to 1 do
    for Piece := 1 to 6 do
      for Dest := 0 to 63 do
        Table[Player, Piece, Dest] := Table[Player, Piece, Dest] div 2 ;
  end;


// Countermove Table ===============================================================


procedure TCounterMoveTable.Update(const PrevMove, CounterMove : TMove);
  var
    Piece, Dest : integer;

  begin
  if PrevMove <> 0 then
    begin
    Piece := (PrevMove shr 12) and $F;
    Dest :=  (PrevMove shr 6) and $3F;

    Table[Piece, Dest] := CounterMove;
    end;
  end;


function TCounterMoveTable.Probe(Move : TMove) : TMove;
  var
    Piece, Dest : integer;

  begin
  result := 0;
  if Move <> 0 then
    begin
    Piece := (Move shr 12) and $F;
    Dest :=  (Move shr 6) and $3F;

    result := Table[Piece, Dest];
    end;
  end;


procedure TCounterMoveTable.ClearTable;
  var
    Piece, Dest : integer;

  begin
  IsEmpty := true;
  for Piece := 1 to 6 do
    for Dest := 0 to 63 do
      Table[Piece, Dest] := 0;
  end;


//  T_ABDADA_Table ===================================================================

//  Hash table to track position being searched
//  To minimize different threads searching same position


procedure T_ABDADA_Table.MoveSearchBegin(MoveHash : UInt64);
  var
    index : UInt;

  begin
  index := UInt(MoveHash and TableMask);

  if Table[index] = 0 then
    Table[index] := MoveHash;
  end;


procedure T_ABDADA_Table.MoveSearchOver(MoveHash : UInt64);
  var
    index : UInt;

  begin
  index := UInt(MoveHash and TableMask);

  if Table[index] = MoveHash then
    Table[index] := 0;
  end;


function T_ABDADA_Table.SearchInProgress(MoveHash : UInt64) : boolean;
  var
    index : UInt;

  begin
  index := UInt(MoveHash and TableMask);

  if Table[index] = MoveHash then
    exit(true);

  result := false;
  end;


procedure T_ABDADA_Table.ClearTable;
  var
    i : UInt;

  begin
  for i := 0 to TableSize - 1 do
    Table[i] := 0;
  end;


// T_ThreadData ===============================================================


procedure T_ThreadData.Reset;
  var
    i : integer;

  begin
  for i := low(KillerMoveA) to high(KillerMoveA) do
    KillerMoveA[i] := 0;
  for i := low(KillerMoveB) to high(KillerMoveB) do
    KillerMoveB[i] := 0;

  NodeCount := 0;
  PrevCount := 0;
  SearchTime := 0;
  SelDepth := 1;
  ID := 0;
  end;


// TGameMoveList ===============================================================

class operator TGameMoveList.Initialize(out Dest: TGameMoveList);
  begin
  Dest.Clear;
  end;


function TGameMoveList.Count : integer;
  begin
  result := MoveArray[0] and UInt64($FFF);
  end;


procedure TGameMoveList.Clear;
  begin
  MoveArray[0] := 0;
  end;


procedure TGameMoveList.AddMove(Move : TMove);
  var
    MoveCount : integer;

  begin
  if Move <> 0 then
    begin
    MoveCount := MoveArray[0] and UInt64($FFF);

    if MoveCount < High(MoveArray) then
      begin
      inc(MoveCount);
      MoveArray[MoveCount] := Move;
      MoveArray[0] := (MoveArray[0] and UInt64($FF000)) or MoveCount;
      end;
    end;
  end;


function TGameMoveList.RemoveLastMove : TMove;
  var
    MoveCount : UInt32;

  begin
  result := 0;
  MoveCount := MoveArray[0] and UInt64($FFF);

  if MoveCount > 0 then
    begin
    result := MoveArray[MoveCount];
    dec(MoveCount);
    MoveArray[0] := (MoveArray[0] and UInt64($FF000)) or MoveCount;
    end;
  end;


// Misc Procedures =============================================================


procedure RateMovesQ(var moves : TMoveArray);
  var
    i, score: integer;

  begin
  for i := 1 to Moves[0] do
    begin
    score:= $F000 + (Moves[i] and $F0000) - (Moves[i] and $F000);
    moves[i] := (moves[i] and UInt64($0000FFFFFFFFFFFF)) xor (UInt64(Score and $FF000) shl 36);
    end;
  end;


//  Move Prioritization:

//  $8000 +    Best Move (from hash table)
//  $7000 +    Winning Captures by most valuable victim, least valuable attacker (MVV / LVA)
//  $6000 +    Pawn Promotion to Queen
//  $5000 +    Killer Moves, CounterMove
//  $4000 +    Castling, Minor Promotion
//  $2000 +/-  Non captures (History Heuristic)
//  $1000 -    Loosing Captures


procedure RateMoves(Search : TSearchPtr; var Board : TBoard; var moves : TMoveArray);
  var
    i : integer;
    score, piece, promotionpiece, capturedpiece, SEE_value, Dest, SPACER : integer;

  begin
  for i := 1 to moves[0] do
    begin
    Piece := (Moves[i] shr 12) and $F;
    CapturedPiece := (Moves[i] shr 16) and $F;
    PromotionPiece := (Moves[i] shr 20) and $F;

    if CapturedPiece > 0 then
      begin
      score := MVV_LVA_Lookup[capturedpiece, Piece] + $7000;

      if PieceValue[Piece] > PieceValue[CapturedPiece] then
        begin
        Board.MakeMoveNoHash(Moves[i]);

        SEE_value :=  PieceValue[CapturedPiece] - Board.SEE(Moves[i]);

        Board.UndoMoveNoHash(Moves[i]);

        if SEE_value < 0 then
          score := SEE_value + $1000;      //  losing capture
        end;
      end
     else
      begin
      Dest := (moves[i] shr 6) and $3F;
      score := search.QuietHistory.Table[Board.ToPlay, Piece, Dest] div 4 + $2000;    // ensure score not -ve
      end;

    if (PromotionPiece > 0) and (score < $7000) then
      begin
      if PromotionPiece = 5 then
        score := $6000
       else
        score := $4000;
      end;

    moves[i] := moves[i] and UInt64($0000FFFFFFFFFFFF) xor (UInt64(Score and $FFFF) shl 48);
    end;
  end;


procedure SortMoves(var moves : TMoveArray);
  var
    i, j : integer;
    temp : TMove;

  begin
  //insertion sort

  for i := 2 to Moves[0] do
    begin
    temp := Moves[i];

    j := i - 1;
    while j >= 1 do
      begin
      if Moves[j] >= temp then
        break;

      Moves[j+1] := Moves[j];
      dec(j);
      end;

    Moves[j+1] := temp;
    end;
  end;


function IsMateScore(Score : integer) : boolean;
  begin
  result := Abs(score) >= MateScoreCutoff;
  end;


function PVS_Q(Search : TSearchPtr; ThreadData : T_ThreadDataPtr; ply, alpha, beta, CheckCount: Integer; var Board : TBoard; const Accumulator: TAccumulator): integer;

  var
    k : integer;
    value, SEE_value, Standpat : integer;
    Moves : TMoveArray;
    Movecount : integer;
    ChecksRemaining :integer;
    InCheck : boolean;
    Move : TMove;
    piece, capturedpiece : integer;
    Updated_Accumulator : TAccumulator;

  begin
  if Search.Stop = true then
    exit(InvalidScore);

  if (alpha + 1 <> beta) and (ply + 1 > ThreadData.SelDepth) then
    ThreadData.SelDepth := ply + 1;

  if ply = MaxSearchPly then
    exit(alpha);

  alpha := max(alpha, ScoreMinValue + ply);
  beta := min(beta, ScoreMaxValue - ply - 1);

  if alpha >= beta then
    exit(alpha);

  InCheck := Board.KingInCheck(Board.ToPlay);

  if not InCheck then
    begin
    Standpat := ScoreFromAccumulator(Accumulator, Board);

    if Standpat >= beta then
      exit(beta);

    if Standpat > alpha then
      alpha := Standpat;
    end;

  // Get Moves to investigate
  // Qsearch allows checks from quiet moves, however must set some limit (ie: to handle perpetual check).

  ChecksRemaining := CheckCount;

  if InCheck then
    movecount := Board.GetValidMoves(Board.ToPlay, Moves, All)   // When the king is in check, all moves are generated and searched.
   else
    begin
    if CheckCount = 0 then
      movecount := Board.GetValidMoves(Board.ToPlay, Moves, ViolentOnly)    // Capture and Promotion Moves only, - no quiet moves
     else
      begin
      movecount := Board.GetCapturePromotionAndCheckingMoves(Board.ToPlay, Moves);   // Capture, Promotion and Checking Moves incl Quiet Checking moves
      ChecksRemaining := CheckCount-1;
      end;
    end;

  if movecount = 0 then
    begin
    if InCheck = true then
      exit(ScoreMinValue + ply)    // checkmate
     else
      exit(alpha);                 // stalemate
    end;

  if movecount > 1 then
    begin
    RateMovesQ(Moves);
    SortMoves(Moves);
    end;


  // Search

  for k := 1 to movecount do
    begin
    Move := Moves[k];

    Board.MakeMove(Move);

    Inc(ThreadData.NodeCount);

    CapturedPiece := integer((Move shr 16) and $F);
    Piece :=  integer((Move shr 12) and $F);

    if (InCheck = false) and (CapturedPiece > 0) and (PieceValue[Piece] >= PieceValue[CapturedPiece]) then
      begin
      SEE_value :=  PieceValue[CapturedPiece] - Board.SEE(Move);

      if SEE_value < 0 then     // losing move so prune it, but only if not in check, and if it doesn't give check
        begin
        Board.UndoMove(Move);
        continue;
        end;

      // Prune neutral captures when static eval is low

      if (SEE_value = 0) and (Standpat + 200 <= alpha) then
        begin
        Board.UndoMove(Move);
        continue;
        end;
      end;

    Update_Accumulator(Accumulator, Updated_Accumulator, Move, Board);
    value := -PVS_Q(search, threadData, ply+1, -beta, -alpha, ChecksRemaining, Board, Updated_Accumulator);
    Board.UndoMove(Move);

    if value = - InvalidScore then
      exit(InvalidScore);

    if value > alpha then
      alpha := value;

    if value >= beta then
      exit(value);
    end;

  result := alpha;
  end;


function PVS_ABDADA(Search : TSearchPtr; ThreadData : T_ThreadDataPtr; ply, depth, alpha, beta, NodeType : Integer;
               var Board : TBoard; var PV : TMoveArray; const Accumulator: TAccumulator;
               NullMove : boolean; PrevMove : TMove; SingularFlag : Boolean) : integer;
  const
    R_IID = 3;
    R = 3;
    R_Null = 4;
    Rn = 3;

  var
    i, k, f : integer;
    value, TTFlag, alphaOrig, LocalEval : integer;
    Move, KillerMove, BestMove, CounterMove, TempMove, TTMove : TMove;
    Moves : TMoveArray;
    Movecount, QuietsTried : integer;
    HashCode, TT_Data, MoveHash : UInt64;
    TempEp : UInt64;
    DataExists, DataExists_A, InCheck, IsRoot, IsPV, Improving : boolean;
    PV1, PVdummy : TMoveArray;
    NewNodetype, c, m, epCell, piece : integer;
    Best, NewDepth, NullDepth : integer;
    LMR_Flag, TT_OKtoStore : boolean;
    FullDepthMoves, LMR, SEE_Value: integer;
    sDepth, sValue, sBeta, sExtension, TTScore, TTDepth  : integer;
    NowTick, ElapsedTicks : Int64;

    Updated_Accumulator : TAccumulator;

  begin
  PV[0] := 0;

  QueryPerformanceCounter(NowTick);

  ElapsedTicks := NowTick - Search.StartTick;

  if ElapsedTicks < 0 then
    ElapsedTicks := ElapsedTicks + High(Int64);

  if not Search.IsPondering then
    begin
    if ElapsedTicks > Search.TickLimit then
      begin
      Search.Stop := true;
      exit(InvalidScore);
      end;

    if Search.NodeCount >= Search.NodeLimit then
      begin
      Search.Stop := true;
      exit(InvalidScore);
      end;
    end;

  IsRoot := (ply = 0);
  IsPV := (alpha + 1 <> beta);

  // Mate distance pruning - this is important as it ensures abs(score) <= ScoreMaxValue and hence avoids
  // problem in overflowing score value when packing into move, TT etc.

  alpha := max(alpha, ScoreMinValue + ply);
  beta := min(beta, ScoreMaxValue - ply - 1);

  if not IsRoot then      // if ply = 0 then need to return a valid move so continue
    begin
    // mate distance pruning if not root

    if alpha >= beta then
      exit(alpha);

    if Board.Gamestage <= 6 then                                    // check for draw by insufficient mating material
      if Board.InsufficientMaterial then
        exit(0);

    if Board.HalfCount >= 100 then                                  // Draw by 50 move rule, return DrawFound so not stored in TT
      exit(DrawFound);

    //  Check for repetition & if so score this position as draw

    if ply >= 4 then
      begin
      m := max(ply - Board.HalfCount, 0);
      i := ply - 4;
      while i >= m do
        begin
        if Board.Hash = ThreadData.RepList[i] then                        // RepList[0] = hash of board at ply = 0
          exit(DrawFound);                                                // RepList[1] = hash of board at ply = 1 etc.
        i := i - 2;
        end;
      end;

    if (Board.HalfCount > ply) then
      begin
      m := min(Board.HalfCount - ply, search.SignatureList[0]);
      i := 1;
      while i <= m do
        begin                                                                   // search.SignatureList[0] contains count of valid historical entries in Table
        if Board.Hash = search.SignatureList[i] then                            // search.SignatureList[1] = Hash of Board at root - (1 ply)
          exit(DrawFound);
        i := i + 2;
        end;
      end;
    end;

  // protect PV from overflow

  if ply = MaxSearchPly then
    exit(ScoreFromAccumulator(Accumulator, Board));

  InCheck := Board.KingInCheck(Board.ToPlay);

  // do depth extension before probing TT to ensure consistency when setting alpha & avoid bug in search
  if (InCheck and (Depth < 2)) or (((PrevMove shr 20) and $F) <> 0) then    // PrevMove.PromotionPiece
    depth := depth + 1;

  //  Evaluate if leaf node

  if depth = 0 then
    begin
    value := PVS_Q(search, ThreadData, ply, alpha, beta, 1, Board, Accumulator);

    if Search.Stop then
      exit(InvalidScore);

    exit(Value);
    end;


  // Check transposition table

  DataExists := Search.TransTable.RetrieveData(Board.Hash, TT_Data);


  Best := InvalidScore;
  BestMove := 0;
  alphaOrig := alpha;
  BestMove := 0;

  TT_OKtoStore := true;
  TTMove := 0;

  // another thread may have searched this position already, even at root

  if DataExists = true then
    begin
    TTDepth := Search.TransTable.Depth(TT_Data);
    TTscore := Search.TransTable.Score(TT_Data, ply);
    TTMove := search.TransTable.Move(TT_Data);
    TTFlag := Search.TransTable.Flag(TT_Data);

    if not IsPV and (TTDepth >= depth) then
      begin
      if (TTFlag <> ftUpperBound) and (TTscore >= beta) then    // i.e. a lower bound or exact value above beta so can cut
        exit(TTscore)
       else if (TTFlag <> ftLowerBound) and (TTscore <= alpha) then   // i.e. an upper bound or exact value that is below alpha so can cut
        exit(TTscore);

      if (TTFlag = ftUpperBound) and (TTscore < Beta) and (depth - R_Null - depth div 4 <= TTDepth)  then
        NullMove := OmitNull;
      end;
    end;


  Improving := false;

  LocalEval := InvalidScore;
  if not InCheck then
    LocalEval := ScoreFromAccumulator(Accumulator, Board);

  ThreadData.EvalHist[ply] := LocalEval;

  if not InCheck and (ply >= 6) then
    begin
    if (ThreadData.EvalHist[ply-2] <> InvalidScore) and (ThreadData.EvalHist[ply] > ThreadData.EvalHist[ply - 2] + 20) then
      Improving := true
     else if (ThreadData.EvalHist[ply-4] <> InvalidScore) and (ThreadData.EvalHist[ply] > ThreadData.EvalHist[ply - 4] + 20) then
      Improving := true;
    end;


  if (depth <= 2) and not (IsPV or InCheck or IsRoot) then
      begin
      LocalEval := ScoreFromAccumulator(Accumulator, Board);

      // Reverse Futility Pruning
      if (depth = 1) and ((LocalEval - 160) >= Beta) then        //   160  approx value of strong pawn
        exit(LocalEval - 160);

      if (depth = 2) and ((LocalEval - 320) >= Beta) then        //    320  approx value of minor
        depth := 1;

      // razoring
      if LocalEval <= alpha - 120 * depth then                    // quiet move score likely to be less than 120 improvement
         exit(PVS_Q(search, ThreadData, ply, alpha, beta, 2, Board, Accumulator));
      end;

  movecount := Board.GetValidMoves(Board.ToPlay, Moves, All);

  // Check for immediate win or stalemate

  if movecount = 0 then
    begin
    if InCheck = true then
      exit(ScoreMinValue + ply)    // checkmate
     else
      exit(0);                     // stalemate
    end;


  // Recursive Null Move forward pruning

  if (NullMove = DoNull) and (depth > 2) and (movecount > 4) and not (InCheck or IsRoot or IsPV) then
    if not Board.PawnsOnly(Board.ToPlay) then
      begin

      //value := ScoreFromAccumulator(Accumulator, Board);

      value := LocalEval;

      if value >= beta then
        begin
        PVdummy[0] := 0;
        epCell := -1;

        if Board.Enpassant <> 0 then
          begin
          epCell := GetLowBit_Alt(Board.Enpassant);
          Board.Hash := Board.Hash xor Board.epHash[epCell];    // remove epCell from Hash, CastleFlags don't change

          tempEp := Board.Enpassant;
          Board.Enpassant := 0;
          end;

        Board.Hash:= Board.Hash xor Board.PlayerHash;           // swap player
        Board.ToPlay := 1 - Board.ToPlay;                       // swap player

        NullDepth := max(depth - R_Null - depth div 4 - ord(improving), 1);

        value := -PVS_ABDADA(search, ThreadData, ply+1, NullDepth, -beta, -beta+1, -Nodetype, Board, PVdummy, Accumulator, OmitNull, 0, SingularFlag);

        Board.ToPlay := 1 - Board.ToPlay;
        Board.Hash := Board.Hash xor Board.PlayerHash;

        if EpCell <> -1 then
          begin
          Board.Hash := Board.Hash xor Board.epHash[epCell];    // add epCell back into Hash
          Board.Enpassant := tempEp;
          end;

        if Search.Stop then
          exit(InvalidScore);

        if value = -InvalidScore then
          exit(InvalidScore);

        if value = -DrawFound then
          value := 0;

        if (value >= beta) and (abs(value) < MateScoreCutoff) then        // verify in case zugzwang, adds approx 10% to search time but avoids at least some errors
          if (depth > 4) or (depth > ply) then                            // suggestion is don't do verification at lower depths
            begin

            value := PVS_ABDADA(search, ThreadData, ply, NullDepth, beta - 1, beta, NodeType, Board, PVdummy, Accumulator, OmitNull, PrevMove, SingularFlag);

            if value = InvalidScore then
              exit(InvalidScore);

            if value = DrawFound then
              value := 0;
            end;

        if (value >= beta) and (abs(value) < MateScoreCutoff) then
          exit(beta);
        end;
      end;

  RateMoves(search, Board, Moves);

  // Killer Move Hueristic

  KillerMove := ThreadData.KillerMoveA[ply];
  if KillerMove.IsValid then
    for i := 1 to Movecount  do
      if (Moves[i] and $FFFFFF) = (KillerMove and $FFFFFF) then
        begin
        if Moves[i] < UInt64($5003000000000000) then
          Moves[i] := Moves[i] and UInt64($00000FFFFFFFFFFF) xor UInt64($5003000000000000);
        break;
        end;

  KillerMove := ThreadData.KillerMoveB[ply];
  if KillerMove.IsValid then
    for i := 1 to Movecount  do
      if (Moves[i] and $FFFFFF) = (KillerMove and $FFFFFF) then
        begin
        if Moves[i] < UInt64($5002000000000000) then
          Moves[i] := Moves[i] and UInt64($00000FFFFFFFFFFF) xor UInt64($5002000000000000);
        break;
        end;


  // Counter Move Hueristic

  if PrevMove <> 0 then
    begin
    if Board.ToPlay = White then
      CounterMove := Search.WhiteCounterMove.Probe(PrevMove)
     else
      CounterMove := Search.BlackCounterMove.Probe(PrevMove);

    if CounterMove <> 0 then
      for i := 1 to Movecount  do
        if (Moves[i] and $FFFFFF) = (CounterMove and $FFFFFF) then
          begin
          if Moves[i] < UInt64($5001000000000000) then
            Moves[i] := Moves[i] and UInt64($00000FFFFFFFFFFF) xor UInt64($5001000000000000);
          break;
          end;
    end;


  if DataExists and (TTMove <> 0) then   // identify hash move
    begin
    for i := 1 to Movecount do
      if (Moves[i] and $FFFFFF) = (TTMove and $FFFFFF) then
        begin
        Moves[i] := Moves[i] or UInt64($8000000000000000);
        break;
        end;
    end;

  SortMoves(Moves);

  // Enhanced Forward Pruning

  if (NodeType = Cut_Node) and not InCheck then
    begin
    if depth > (R+5) then
      begin
      PVdummy[0] := 0;

      c := 0;
      m := 1;

      while m <= min(4, MoveCount) do
        begin
        Move := Moves[m];                   // to avoid ruining move ordering info

        Board.MakeMove(Move);

        Inc(ThreadData.NodeCount);
        //

        NewNodetype := ALL_Node;

        DataExists_A := Search.TransTable.RetrieveData(Board.Hash, TT_Data);

        if (DataExists_A = true) and (depth - R - 1 <= Search.TransTable.Depth(TT_Data)) and (Search.TransTable.Flag(TT_Data) <> ftLowerBound) then
          value := -Search.TransTable.Score(TT_Data, ply+1)
         else
          begin
          Update_Accumulator(Accumulator, Updated_Accumulator, Move, Board);
          ThreadData.RepList[ply+1] := Board.Hash;

          value := -PVS_ABDADA(search, ThreadData, ply+1, depth-1-R, -beta, -beta+1, NewNodetype, Board, PVdummy, Updated_Accumulator, DoNull, Move, SingularFlag);
          end;

        Board.UndoMove(Move);

        if Search.Stop then
          exit(InvalidScore);

        if value = -DrawFound then
          value := 0;

        if value >= beta then
          begin
          inc(c);
          if c >= 2 then
            exit(beta);
          end;

        inc(m);
        end;
      end;
    end;


  // Enhanced Transposition Cutoff

  if (NodeType = Cut_Node) and (depth >= 2) then           // if cut node then not root so no need to test for this
    begin
    for k := 1 to min(MoveCount, 6) do           // Don't test all moves, just try first few
      begin
      Move := Moves[k];                          // to avoid ruining move ordering info

      HashCode := Board.GetMoveHash(Move);

      if search.TransTable.RetrieveData(HashCode, TT_Data) = true then
        begin
        if search.TransTable.Depth(TT_Data) = (depth - 1) then
          if search.TransTable.Flag(TT_Data) <> ftLowerBound then
            begin
            value := -search.TransTable.Score(TT_Data, ply+1);

            if (search.TransTable.Flag(TT_Data) = ftExact) and (value > alpha) and (value < beta) then
              begin
              alpha := value;
              best := value;
              bestmove := (Move and $FFFFFF);

              PV[1] := BestMove;
              PV[0] := 1;
              end;

            if value >= beta then
              exit(value);
            end;
        end;
      end;
    end;


  //Internal Iterative Deepening

  if (TTMove = 0) and IsPV and not IsRoot then
    if depth > 6 then
      depth := depth - 1;


  // Search

  PV1[0] := 0;

  QuietsTried := 0;

  if (ply > 1) and (depth > 2) and not InCheck then       // only undertake LMR for ply 2 and greater
    begin
    FullDepthMoves := 3 + GetHighBit_Alt(depth + 1);      // GetHighBit_alt is undefined when argument = 0

    for i := FullDepthMoves to MoveCount do               // flag potential moves for reduction
      if Moves[i] < UInt64($4000000000000000) then        // i.e. don't reduce winning captures, promotion, killer and countermoves
        setbit(Moves[i], 44);
    end;

  MoveHash := 0;
  if IsRoot and (SingularFlag = false) then
    ThreadData.FirstRootMoveSearched := false;

  for k := 1 to movecount do
    begin
    if (Search.ThreadCount > 1) and (depth > 2) and (k > 1) then                       // no need to do this if it is first move in list
      begin
      f := 0;
      moveHash := Moves[k] xor Board.Hash xor UInt64(depth * $4AF89F927AD5);

      while (f+k < moveCount) and Search.ABDADA_Table.SearchInProgress(movehash) do    // defer search of this move as another thread is searching it
        begin                                                                          // swap with next move in list
        inc(f);
        moveHash := Moves[k+f] xor Board.Hash xor UInt64(depth * $4AF89F927AD5);
        tempMove := Moves[k];
        moves[k] := moves[k+f];
        moves[k+f] := tempMove;
        end;
      end;

    move := Moves[k];
    Board.MakeMove(Move);

    Update_Accumulator(Accumulator, Updated_Accumulator, Move, Board);

    Inc(ThreadData.NodeCount);
    ThreadData.RepList[ply+1] := Board.Hash;

    // singular extension

    sExtension := 0;
    if (Moves[k] > UInt64($8000000000000000)) and DataExists and (TTMove <> 0) and not (IsPV or InCheck or IsRoot) and (SingularFlag = NotSingular) then
      if (TTDepth > depth - 2) and (abs(TTScore) <= MateScoreCutoff) and (TTFlag <> ftUpperbound) then
        begin
        sDepth := depth div 2;
        sBeta := TTScore - (depth - ord(improving)) * 3;

        sValue := -PVS_ABDADA(search, ThreadData, ply+1, sDepth, -sbeta, -sbeta+1, Cut_Node, Board, PVdummy, Updated_Accumulator, OmitNull, 0, Singular);

        if svalue = -DrawFound then
          svalue := 0;

        if (svalue <> -InvalidScore) and (sValue < sBeta) then
          sExtension := 1;
        end;

    LMR_Flag := Getbit(Moves[k], 44);

    if LMR_Flag = true then
      begin
      piece := (Moves[k] shr 12) and $F;
      if (Board.GameStage < 20) and ((piece = pawn) or (piece = king)) then     // treat king and pawn moves as interesting  : pawn moves has (-20%) + impact on speed of WAC_2018
        LMR_Flag := false

      else if Board.KingInCheck(Board.ToPlay) then                              // move gives check
        LMR_Flag := false;
      end;

    if NodeType = PV_Node then
      begin
      if k = 1 then
        NewNodeType := PV_Node
       else
        NewNodeType := Cut_Node;
      end
     else
      NewNodeType := - NodeType;

    if k = 1 then
      value := -PVS_ABDADA(search, ThreadData, ply+1, depth-1 + sExtension, -beta, -alpha, NewNodeType, Board, PV1, Updated_Accumulator, DoNull, Move, SingularFlag)
     else
      begin
      newDepth := depth - 1;

      if LMR_Flag = true then
        begin
        if NodeType = PV_Node then
          LMR :=  1
         else
          LMR := max((GetHighBit_Alt(k) + GetHighBit_Alt(depth) + 1) shr 1, 2);     // k = 2..32 : highbit = 1..5,  depth = 24..1 : highbit = 4..0

        newdepth := max(1, depth - LMR);
        end;

      if (moves[k] < $4000000000000000) and (moves[k] > $1000000000000000) and (alpha < MateScoreCutoff) and (depth < 8) then     // quiet move
        begin

        // late move pruning : prune late quiet moves with bad history (but don't prune any bad captures)
        if (LMR_Flag = true) and (QuietsTried > (newdepth * newdepth) div 2 + 1) and not IsPV and (moves[k] < $2000000000000000) then       // prune
          begin
          Board.UndoMove(Move);
          continue;
          end;

        // late move pruning : prune late quiet moves that lose too much material

        if LMR_Flag = true then
          begin
          SEE_Value := Board.SEE(Move);

          if SEE_Value > 15 * (7-depth) then
            begin
            Board.UndoMove(Move);
            continue;
            end;
          end;

        inc(QuietsTried);          // inc quiet moves
        end;


      if (Search.ThreadCount > 1) and (MoveHash <> 0) then
        Search.ABDADA_Table.MoveSearchBegin(MoveHash);

      value := -PVS_ABDADA(search, ThreadData, ply+1, newDepth, -alpha-1, -alpha, NewNodeType, Board, PVdummy, Updated_Accumulator, DoNull, Move, SingularFlag);

      if IsPV and (value <> -InvalidScore) then
        begin
        if value = - Drawfound then
          value := 0;

        if (alpha < value) and (value < beta)  then
          value := -PVS_ABDADA(search, ThreadData, ply+1, depth-1, -beta, -alpha, NewNodeType, Board, PV1, Updated_Accumulator, DoNull, Move, SingularFlag);
        end;

      if (Search.ThreadCount > 1) and (MoveHash <> 0) then
        Search.ABDADA_Table.MoveSearchOver(MoveHash);
      end;

    Board.UndoMove(Move);

    if value = -DrawFound then
      begin
      value := 0;

      if value > Best then
        TT_OKtoStore := false;
      end
     else if value <> -InvalidScore then
      begin
      if (value > Best) and (TT_OKtoStore = false) then
        TT_OKtoStore := true;
      end;

    if value > Alpha then
      Alpha := value;

    if IsRoot and (k = 1) and (value <> -InvalidScore) and (SingularFlag = NotSingular) then
      ThreadData.FirstRootMoveSearched := true;                     // flag that first move in root has been fully searched

    if (value <> -InvalidScore) and (value > Best)  then
      begin
      best := value;
      BestMove := (Moves[k] and $0000000000FFFFFF);

      PV[1] := BestMove;
      for i := 1 to PV1[0] do
        PV[i+1] := PV1[i];
      PV[0] := PV1[0] + 1;

      if best >= Beta then
        begin
        if BestMove.CapturedPiece = 0 then
          begin
          Search.QuietHistory.Update(Board.ToPlay, BestMove, depth*depth);
          for i := 1 to k-1 do
            Search.QuietHistory.Update(Board.ToPlay, Moves[i], -depth);

          if (BestMove and UInt64($FFFFFF)) <> (ThreadData.KillerMoveA[ply] and UInt64($FFFFFF)) then
            begin
            ThreadData.KillerMoveB[ply] := ThreadData.KillerMoveA[ply];
            ThreadData.KillerMoveA[ply] := BestMove;
            end;

          if Board.ToPlay = white then
            Search.WhiteCounterMove.Update(PrevMove, BestMove)
           else
            Search.BlackCounterMove.Update(PrevMove, BestMove);
          end;

        Break;
        end;
      end;

    if Search.Stop then
      begin
      if IsRoot and ThreadData.FirstRootMoveSearched then
        exit(Best)
       else
        exit(InvalidScore);
      end;
    end;


  if (TT_OKtoStore = false) and (((BestMove and UInt64($FF0000)) <> 0) or ((BestMove and UInt64($F000)) = UInt64($1000))) then  // if best move is capture/promotion or pawn move then irreversible position and OK to store in TT
    TT_OKtoStore := true;

  if TT_OKtoStore and (Search.Stop = false) and (SingularFlag = NotSingular) then
    begin
    TempMove := BestMove;

    if Best <= alphaOrig then
      begin
      TTFlag := ftUpperBound;       // All_Node
      TempMove := InvalidMove;      // No idea about which move is best since all
                                    // failed low, so store best move of 'zero'.
      end
     else if Best >= beta then
      begin
      TTFlag := ftLowerBound;       // Cut_Node
                                    // A good move to try first again since it caused
                                    // a cut-off, although it may not be the best move
                                    // as all moves were not searched.
      end
     else
      begin
      TTFlag := ftExact;              // PV_Node
      end;

    TT_Data := search.TransTable.PackInfo(TempMove, Best, depth, TTflag, ply);     // store adjusted score in TT
    search.TransTable.StoreData(Board.Hash, TT_Data);
    end;

  result := Best;

  if (result = 0) and (TT_OKtoStore = false) and (ply > 0) then   // draw by repetition
    result := DrawFound;
  end;


//   TSearchEngine_PVS

constructor TSearch.Create;
  begin
  inherited Create;
  TransTable := TTransTable.Create;

  setdefault;
  NewGame;

  QueryPerformanceFrequency(TickFrequency);

  Searching := false;
  ShutdownUnderway := false;
  OnSearchFinished := nil;
  OnProgressUpdate := nil;

  if ThreadPool = nil then
    begin
    ThreadPool := TThreadPool.Create;
    ThreadPool.SetMaxWorkerThreads(16);
    ThreadPool.SetMinWorkerThreads(16);
    ThreadPool.UnlimitedWorkerThreadsWhenBlocked := False;
    end;

  end;


destructor TSearch.Destroy;
  begin
  if Searching = true then
    ShutDown;

  if TransTable <> nil then
    TransTable.Free;

  if assigned(ThreadPool) then
    ThreadPool.Free;

  inherited;
  end;


procedure TSearch.ShutDown;
  begin
  if Searching = true then
    begin
    Stop := true;
    ShutDownUnderway := true;

    while searching = true do   // loop while search is being halted
      begin
      sleep(40);
      end;
    end;
  end;


procedure TSearch.StopSearch;
  begin
  Stop := true;
  end;


function TSearch.GetElapsedTicks : Int64;
  var
    NowTick : Int64;

  begin
  QueryPerformanceCounter(NowTick);

  if NowTick >= StartTick then
    result := NowTick - StartTick
   else
    result := High(Int64) - StartTick + NowTick;
  end;


function TSearch.GetElapsedMilliseconds : Int64;
  var
    NowTick, ElapsedTick : Int64;

  begin
  QueryPerformanceCounter(NowTick);

  if NowTick > StartTick then
    ElapsedTick := NowTick - StartTick
   else
    ElapsedTick := High(Int64) - StartTick + NowTick;

  result := ElapsedTick * 1000 div TickFrequency;
  end;


function TSearch.ResultToStr : string;
  var
    nps : double;

  begin
  if Search_Result.time > 0 then
    nps := Search_Result.nodes * 1000 / Search_Result.time
   else
    nps := 0.0;

  if Search_Result.score >= MateScoreCutoff then
    result := 'info depth ' + IntToStr(Search_Result.Depth) + ' seldepth ' + IntToStr(Search_Result.selDepth) + ' score mate ' + IntToStr( (ScoreMaxValue - Search_Result.score + 1) div 2)   + ' nodes ' + FormatFloat('####', Search_Result.nodes) +
                           ' nps ' + FormatFloat('####', nps) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV)

  else if Search_Result.score <= -MateScoreCutoff then
    result := 'info depth ' + IntToStr(Search_Result.Depth) + ' seldepth ' + IntToStr(Search_Result.selDepth) + ' score mate -' + IntToStr( (ScoreMaxValue + Search_Result.score + 1) div 2)  + ' nodes ' + FormatFloat('####', Search_Result.nodes) +
                           ' nps ' + FormatFloat('####', nps) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV)

  else
    result := 'info depth ' + IntToStr(Search_Result.Depth) + ' seldepth ' + IntToStr(Search_Result.selDepth) + ' score cp ' + IntToStr(Search_Result.score)  + ' nodes ' + FormatFloat('####', Search_Result.nodes) +
                           ' nps ' + FormatFloat('####', nps) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV);
  end;


procedure TSearch.NewGame;
  begin
  ClearTranspositionTable;

  PrepareForNextSearch;
  end;


procedure TSearch.PrepareForNextSearch;
  begin
  ClearHistoryTables;
  ClearCounterMoveTables;
  Clear_ABDADA_Table;

  NodeCount := 0;

  ReadyToSearch := true;
  end;


procedure TSearch.SetDefault;
  begin
  DepthLimit := MaxSearchPly;
  TimeLimit := TimeLimit_Max;
  SoftTimeLimit := false;
  NodeLimit := NodeLimit_Max;
  IsPondering := false;

  DoParallelSearch := true;
  ThreadCount := 6;

  DoAspirationSearch := true;

  DoProgressUpdates := true;
  UseOwnBook := false;

  ShowWDL := false;
  MultiPV := false;
  WillPonder := false;
  end;


procedure TSearch.ClearHistoryTables;
  begin
  QuietHistory.ClearTable;
  end;


procedure TSearch.ClearCounterMoveTables;
  begin
  WhiteCounterMove.ClearTable;
  BlackCounterMove.ClearTable;
  end;


procedure TSearch.DampHistoryTables;
  begin
  QuietHistory.DampTable;
  end;


procedure TSearch.ClearTranspositionTable;
  begin
  if TransTable <> nil then
    TransTable.ClearTable;
  end;


procedure TSearch.SetThreadCount(Value : integer);
  begin
  ThreadCount := min(max(1, value), 32);            // 0..32
  if ThreadCount = 1 then
    DoParallelSearch := false
   else
    DoParallelSearch := true;
  end;


procedure TSearch.SetTransTableSize(size : integer);
  begin
  if not Searching then
    begin
    if TransTable <> nil then
      TransTable.SetTableSize(size);

    ClearTranspositionTable;
    end;
  end;


procedure TSearch.Clear_ABDADA_Table;
  begin
  ABDADA_Table.ClearTable;
  end;


procedure TSearch.LoadSignatureList(const Board : TBoard; const GameMoveList : TGameMoveList);

  // loads hash of previous board positions (if available) into a table for repetition check during search
  // SignatureList[0] = Number of valid entries in Table
  // SignatureList[1] = Board.Hash prior to last move
  // SignatureList[2] = Board.Hash prior to second last move
  // etc

  var
    i, MoveCount : integer;
    TempBoard : TBoard;

  begin
  MoveCount := GameMoveList.Count;
  TempBoard := CopyBoard(Board);

  SignatureList[0] := MoveCount;

  for i := MoveCount downto 1 do
    begin
    TempBoard.UndoMove(GameMoveList.MoveArray[i]);
    SignatureList[MoveCount - i + 1] := TempBoard.Hash;
    end;
  end;


procedure TSearch.FullSearch(const Board : TBoard; const GameMoveList : TGameMoveList);
  begin
  ABDADA_Search(Board, GameMoveList);
  end;


procedure TSearch.StartInBackGround(const Board : TBoard; const GameMoveList : TGameMoveList);
  var
    Master : ITask;

  begin
  Master := TTask.Run(  procedure
                begin
                ABDADA_Search(Board, GameMoveList);
                end,

            ThreadPool);
  end;


procedure TSearch.ABDADA_Search(const Board : TBoard; const GameMoveList : TGameMoveList);
  begin
  if (DoParallelSearch = true) and (ThreadCount > 1) then
    ABDADA_Search_Threaded(Board, GameMoveList)
   else
    ABDADA_Search_NonThreaded(Board, GameMoveList);
  end;


function TSearch.BookMoveFound(const Board : TBoard): boolean;
  var
    i, BestScore : integer;
    Move : TMove;
    ProgressMessage : string;

  begin
  result := false;

  if Book.GetMove(Board.Hash, Move, ms_BestHist) = true then
    begin
    if move.score <> -32768 then
      BestScore := Move.score
     else
      BestScore := 0;

    Search_Result.PV[0] := 1;
    Search_Result.PV[1] := Move and UInt64($0000FFFFFFFFFFFF);

    Search_Result.Depth := 1;
    Search_Result.score := BestScore;
    Search_Result.selDepth := 1;
    Search_Result.nodes := 1;
    Search_Result.time := 0;

    if IsPondering then            // wait until ponderhit or stop
      repeat
      until stop or not IsPondering;

    ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr;

    if Assigned(FOnMessageOut) then
      FOnMessageOut(ProgressMessage);

    Searching := false;
    if Assigned(fOnSearchFinished) then
      fOnSearchFinished(Search_Result);

    exit(true);
    end;
  end;


procedure TSearch.ABDADA_Search_Threaded(const Board : TBoard; const GameMoveList : TGameMoveList);
  const
    AspirationMargin = 80;

  var
    i : integer;
    alpha, beta : integer;
    ElapsedMilliseconds : Int64;
    BestDepth, BestScore, SelDepth : integer;
    BestPV: TMoveArray;
    Lock : TCriticalSection;
    workerCount, ThreadsPerDepth, index : integer;
    workers: TArray<ITask>;
    ProgressMessage : string;
    sd : array[0..MaxSearchPly] of TUpdateRec;
    DepthToken, HighestDepth : integer;
    TimeExtension : boolean;
    ElapsedTicks, RequiredTicks : Int64;
    SupressProgressMessage : boolean;

  begin
  SupressProgressMessage := false;
  Searching := true;

  if TransTable <> nil then
    TransTable.NewSearch;        // increments search age, must do before openbook

  if UseOwnBook = true then
    if BookMoveFound(Board) then
      exit;

  for i := low(sd) to high(sd) do
    begin
    sd[i].BestScore := InvalidScore;
    sd[i].BestMove := InvalidMove;
    sd[i].BestPV[0] := 0;
    sd[i].SelDepth := 0;
    sd[i].FirstRootMoveSearched := false;
    end;

  QueryPerformanceCounter(StartTick);

  if SoftTimeLimit then
    TickLimit := (BudgetTime - (ThreadCount-1) * 2) * TickFrequency div 1000
   else
    TickLimit := (TimeLimit - (ThreadCount-1) * 2) * TickFrequency div 1000;

  TimeExtension := false;

  if ReadyToSearch = false then
    PrepareForNextSearch;              // clears all tables except Transposition Table

  LoadSignatureList(Board, GameMoveList);   // required for repetition check

  alpha := -InfiniteValue;
  beta  :=  InfiniteValue;

  if DoParallelSearch = true then
    workercount := ThreadCount
   else
    workercount := 1;

  //  Each depth only searched by a maximum of 'ThreadsPerDepth' threads i.e. workercount
  //  Starting with a depth of 2

  ThreadsPerDepth := workerCount * 2;

  DepthToken := 2 * ThreadsPerDepth;
  HighestDepth := 1;

  BestPV[0] := 0;
  BestDepth := 0;
  selDepth := 0;
  BestScore := InvalidScore;
  ElapsedMilliseconds := 0;
  BestMove := InvalidMove;

  Lock := TCriticalSection.Create;

    try
    index := -1;

    SetLength(workers, WorkerCount);
    for i := 0 to workerCount - 1 do
      workers[i] := TTask.Run(

      procedure
        var
          alpha1, beta1, ply1, SearchDepth1 : integer;
          i0, k : integer;
          score1 : integer;
          PrevMove1 : TMove;

          Board1 : TBoard;
          PV1 : TMoveArray;

          Accumulator1 : TAccumulator;
          ThreadData1 : T_ThreadData;
          UpdateResult1 : boolean;

        begin
        k := InterLockedIncrement(Index);

        ply1 := 0;
        score1 := InvalidScore;
        PrevMove1 := InvalidMove;      // Used for countermove hueristic, no previous move

        ThreadData1.Reset;
        ThreadData1.ID := k+1;

        for i0 := low(ThreadData1.RepList) to high(ThreadData1.RepList) do
          ThreadData1.RepList[i0] := 0;

        Board1 := copyBoard(Board);
        ThreadData1.RepList[ply1] := Board.Hash;

        DepthToken := InterLockedIncrement(DepthToken);
        SearchDepth1 := 2;

        while (Stop = false) and (HighestDepth < DepthLimit) do
          begin
          PV1[0] := 0;

          Refresh_Accumulator(Accumulator1, Board1);

          if DoAspirationSearch and (SearchDepth1 > 4) then
            begin
            Lock.Acquire;

            if BestScore <> InvalidScore then
              begin
              alpha1 := max(BestScore - AspirationMargin, ScoreMinValue - 1);
              beta1 := min(BestScore + AspirationMargin, ScoreMaxValue + 1);
              end
             else
              begin
              alpha1 := -InfiniteValue;
              beta1  :=  InfiniteValue;
              end;

            Lock.Release;

            ThreadData1.FirstRootMoveSearched := false;

            if not stop then
              score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);

            if (score1 >= beta1) or (score1 <= alpha1) then   //  if outside aspiration window then set ThreadData1.FirstRootMoveSearched = false;
              begin
              ThreadData1.FirstRootMoveSearched := false;

              if SoftTimeLimit and (TimeExtension = false) then
                begin
                BudgetTime := ExtendTime(BudgetTime, TimeLimit);
                TickLimit := (BudgetTime - (ThreadCount-1) * 2) * TickFrequency div 1000;
                TimeExtension := true;
                end;
              end;

            if (score1 <> InvalidScore) and not stop then
              begin
              if score1 >= beta1 then     // had cutoff, so re-search widened window
                begin
                alpha1 := max(score1 - AspirationMargin div 4, alpha);
                beta1 := beta;
                score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
                end
               else if score1 <= alpha1 then     // had cutoff, so re-search widened window
                begin
                alpha1 := max(score1 - AspirationMargin , alpha);
                beta1 := min(score1 + AspirationMargin div 4, beta);
                score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
                end;
              end;

            if (score1 >= beta1) or (score1 <= alpha1) then
              begin
              ThreadData1.FirstRootMoveSearched := false;      //   if outside aspiration window then set ThreadData1.FirstRootMoveSearched = false;
              if (score1 <> InvalidScore) and not stop then
                score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha, beta, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);

              end;
            end
           else
            begin
            ThreadData1.FirstRootMoveSearched := false;
            score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha, beta, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
            end;

          Lock.Acquire;

          NodeCount :=  NodeCount + ThreadData1.NodeCount - ThreadData1.PrevCount;
          ThreadData1.PrevCount := ThreadData1.NodeCount;

          UpdateResult1 := false;

          sd[SearchDepth1].FirstRootMoveSearched := sd[SearchDepth1].FirstRootMoveSearched or ThreadData1.FirstRootMoveSearched;

          if score1 > sd[SearchDepth1].BestScore then
            begin
            sd[SearchDepth1].BestScore := score1;
            sd[SearchDepth1].BestMove := PV1[1];
            sd[SearchDepth1].SelDepth := ThreadData1.SelDepth;
            for i0 := 0 to PV1[0] do
              sd[SearchDepth1].BestPV[i0] := PV1[i0];
            end;

          if (sd[SearchDepth1].FirstRootMoveSearched = true) and
             ((SearchDepth1 > BestDepth) or
             ((SearchDepth1 = BestDepth) and (sd[SearchDepth1].BestScore > BestScore)) or
             ((sd[SearchDepth1].BestScore >= MateScoreCutoff) and (score1 > BestScore)))  then
               updateResult1 := true;

          if not IsPondering then
            if (sd[SearchDepth1].BestPV[0] >= ScoreMaxValue - abs(sd[SearchDepth1].BestScore)) or (abs(sd[SearchDepth1].BestScore) >= MateScoreCutoff) and (SearchDepth1 >= ScoreMaxValue - abs(sd[SearchDepth1].BestScore) - 1) then
              begin
              if stop = false then
                UpdateResult1 := true;
              Stop := true;
              end;

          if UpdateResult1 = true then
            begin
            for i0 := 0 to sd[searchdepth1].BestPV[0] do
              BestPV[i0] := sd[searchdepth1].BestPV[i0];

            BestMove := sd[searchdepth1].BestMove;
            BestScore := sd[searchdepth1].BestScore;
            SelDepth := sd[searchdepth1].SelDepth;
            BestDepth := SearchDepth1;

            if (DoProgressUpdates = true) and not (stop or SupressProgressMessage) then

            // supresss progress message when looping after DepthLimit reached when pondering

              begin
              ElapsedMilliseconds := GetElapsedMilliseconds;

              if BestScore >= MateScoreCutoff then
                ProgressMessage := 'info depth ' + IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + ' score mate ' + IntToStr( (ScoreMaxValue - BestScore + 1) div 2)  + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV) + ' string : thread #' + IntToStr(k+1)

               else if BestScore <= -MateScoreCutoff then
                ProgressMessage := 'info depth ' + IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + ' score mate -' + IntToStr( (ScoreMaxValue + BestScore + 1) div 2) + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV) + ' string : thread #' + IntToStr(k+1)

               else
                ProgressMessage := 'info depth ' +  IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + ' score cp ' + IntToStr(BestScore) + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV) + ' string : thread #' + IntToStr(k+1);

              if Assigned(FOnMessageOut) then
                FOnMessageOut(ProgressMessage);
              end;
            end;

          HighestDepth := max(HighestDepth, SearchDepth1);

          if DepthToken < (HighestDepth + 1) * ThreadsPerDepth  then
            DepthToken := (HighestDepth + 1) * ThreadsPerDepth;

          DepthToken := InterLockedIncrement(DepthToken);
          SearchDepth1 := min(DepthToken div ThreadsPerDepth, DepthLimit);

          if BestDepth = DepthLimit then
            SupressProgressMessage := true;

          if IsPondering = false then      //  don't stop for time if pondering
            begin
            ElapsedTicks := GetElapsedTicks;
            if ElapsedTicks > TickLimit  then
              Stop := true;

            RequiredTicks := round(ElapsedTicks * power(2, 1.25 * Log2( UInt64ToDouble(ElapsedTicks) * 1000.0 / TickFrequency) / HighestDepth));
            if SoftTimeLimit and (RequiredTicks > TickLimit) then
              Stop := true;
            end;

          Lock.Release;
          end;
        end,

        ThreadPool);

    if workercount > 0 then
      TTask.WaitForAny(workers);

    Stop := true;                   // signal other workers to stop

    if workercount > 1 then
      TTask.WaitForAll(workers);     //  Must wait for all remaining workers to stop

    finally
    Lock.Free;
    end;

  SearchTime_ms := GetElapsedMilliseconds;

  if BestPV[0] > (ScoreMaxValue - abs(BestScore)) then
    BestPV[0] := min(BestPV[0], ScoreMaxValue - abs(BestScore));

  if BestPV[0] > 0 then
    begin
    Search_Result.Depth := BestDepth;
    Search_Result.selDepth := selDepth;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    for i := 0 to BestPV[0] do
      Search_Result.PV[i] := BestPV[i];
    Search_Result.score := BestScore;
    end
   else
    begin
    Search_Result.Depth := 0;
    Search_Result.selDepth := 0;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    Search_Result.PV[0] := 0;
    Search_Result.score := ScoreMinValue;
    end;

  if DoProgressUpdates then
    begin
    if IsPondering then               // if stop command received when pondering, only return best move
      begin
      if Search_Result.PV[0] >= 1 then
        ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr
       else if Search_Result.PV[0] = 0 then
        ProgressMessage := 'bestmove (none)';
      end
     else
      begin
      if Search_Result.PV[0] > 1 then
        ProgressMessage := ResultToStr + #13#10 + 'bestmove ' + Search_Result.PV[1].ToStr + ' ponder ' + Search_Result.PV[2].ToStr
       else if Search_Result.PV[0] = 1 then
        ProgressMessage := ResultToStr + #13#10 + 'bestmove ' + Search_Result.PV[1].ToStr
       else if Search_Result.PV[0] = 0 then
        ProgressMessage := ResultToStr + #13#10 + 'bestmove (none)';
      end;

    if Assigned(FOnMessageOut) then
      FOnMessageOut(ProgressMessage);
    end;

  IsPondering := false;
  Searching := false;
  ReadyToSearch := false;
  Stop := false;

  if Assigned(fOnSearchFinished) then
    fOnSearchFinished(Search_Result);

  PrepareForNextSearch;
  end;


procedure TSearch.ABDADA_Search_NonThreaded(const Board : TBoard; const GameMoveList : TGameMoveList);
  const
    AspirationMargin = 80;

  var
    i : integer;
    alpha, beta : integer;
    ElapsedMilliseconds : Int64;
    BestDepth, BestScore, SelDepth : integer;
    BestPV: TMoveArray;

    ProgressMessage : string;
    TimeExtension : boolean;
    ElapsedTicks, RequiredTicks : Int64;

    alpha1, beta1, ply1, SearchDepth1 : integer;
    i0 : integer;
    score1 : integer;
    PrevMove1 : TMove;

    Board1 : TBoard;
    PV1 : TMoveArray;

    Accumulator1 : TAccumulator;
    ThreadData1 : T_ThreadData;

    SupressProgressMessage : boolean;

  begin
  SupressProgressMessage := false;
  Searching := true;

  if TransTable <> nil then
    TransTable.NewSearch;        // increments search age, must do before openbook

  if UseOwnBook = true then
    if BookMoveFound(Board) then
      exit;

  QueryPerformanceCounter(StartTick);

  if SoftTimeLimit then
    TickLimit := BudgetTime * TickFrequency div 1000
   else
    TickLimit := TimeLimit * TickFrequency div 1000;

  TimeExtension := false;

  if ReadyToSearch = false then
    PrepareForNextSearch;              // clears all tables except Transposition Table

  LoadSignatureList(Board, GameMoveList);   // required for repetition check

  alpha := -InfiniteValue;
  beta  :=  InfiniteValue;

  BestPV[0] := 0;
  BestScore := InvalidScore;
  BestMove := InvalidMove;
  BestDepth := 0;
  selDepth := 0;

  ply1 := 0;
  score1 := InvalidScore;
  PrevMove1 := InvalidMove;      // Used for countermove hueristic, no previous move

  ThreadData1.Reset;
  ThreadData1.ID := 1;

  for i0 := low(ThreadData1.RepList) to high(ThreadData1.RepList) do
    ThreadData1.RepList[i0] := 0;

  Board1 := copyBoard(Board);
  ThreadData1.RepList[ply1] := Board.Hash;

  SearchDepth1 := 2;

  while (Stop = false) and (SearchDepth1 <= DepthLimit) do
    begin
    PV1[0] := 0;

    Refresh_Accumulator(Accumulator1, Board1);

    if DoAspirationSearch and (SearchDepth1 > 4) then
      begin
      if BestScore <> InvalidScore then
        begin
        alpha1 := max(BestScore - AspirationMargin, ScoreMinValue - 1);
        beta1 := min(BestScore + AspirationMargin, ScoreMaxValue + 1);
        end
       else
        begin
        alpha1 := -InfiniteValue;
        beta1  :=  InfiniteValue;
        end;

      ThreadData1.FirstRootMoveSearched := false;

      if not stop then
        score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);

      if (score1 >= beta1) or (score1 <= alpha1) then   //  if outside aspiration window then set ThreadData1.FirstRootMoveSearched = false;
        begin
        ThreadData1.FirstRootMoveSearched := false;

        if SoftTimeLimit and (TimeExtension = false) then
          begin
          BudgetTime := ExtendTime(BudgetTime, TimeLimit);
          TickLimit := (BudgetTime - (ThreadCount-1) * 2) * TickFrequency div 1000;
          TimeExtension := true;
          end;
        end;

      if (score1 <> InvalidScore) and not stop then
        begin
        if score1 >= beta1 then     // had cutoff, so re-search widened window
          begin
          alpha1 := max(score1 - AspirationMargin div 4, alpha);
          beta1 := beta;
          score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
          end
         else if score1 <= alpha1 then     // had cutoff, so re-search widened window
          begin
          alpha1 := max(score1 - AspirationMargin , alpha);
          beta1 := min(score1 + AspirationMargin div 4, beta);
          score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
          end;
        end;

      if (score1 >= beta1) or (score1 <= alpha1) then
        begin
        ThreadData1.FirstRootMoveSearched := false;      //   if outside aspiration window then set ThreadData1.FirstRootMoveSearched = false;
        if (score1 <> InvalidScore) and not stop then
          score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha, beta, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
        end;
      end
     else
      begin
      ThreadData1.FirstRootMoveSearched := false;
      score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha, beta, PV_Node, Board1, PV1, Accumulator1, DoNull, PrevMove1, NotSingular);
      end;

    NodeCount :=  NodeCount + ThreadData1.NodeCount - ThreadData1.PrevCount;
    ThreadData1.PrevCount := ThreadData1.NodeCount;

    if ThreadData1.FirstRootMoveSearched = true then
      begin
      BestScore := score1;
      BestMove := PV1[1];
      SelDepth := ThreadData1.SelDepth;

      for i0 := 0 to PV1[0] do
        BestPV[i0] := PV1[i0];
      BestDepth := SearchDepth1;
      end;

    if not IsPondering then
      if (BestPV[0] >= ScoreMaxValue - abs(BestScore)) or (SearchDepth1 >= ScoreMaxValue - abs(BestScore) - 1) then
        Stop := true;

    if (DoProgressUpdates = true) and not (stop or SupressProgressMessage) then    // supresss progress message when looping after DepthLimit reached when pondering
      begin
      ElapsedMilliseconds := GetElapsedMilliseconds;

      if BestScore >= MateScoreCutoff then
        ProgressMessage := 'info depth ' + IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + ' score mate ' + IntToStr( (ScoreMaxValue - BestScore + 1) div 2)  + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV) + ' string : thread #' + IntToStr(1)

       else if BestScore <= -MateScoreCutoff then
        ProgressMessage := 'info depth ' + IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + ' score mate -' + IntToStr( (ScoreMaxValue + BestScore + 1) div 2) + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV) + ' string : thread #' + IntToStr(1)

       else
        ProgressMessage := 'info depth ' +  IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + ' score cp ' + IntToStr(BestScore) + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV) + ' string : thread #' + IntToStr(1);

      if Assigned(FOnMessageOut) then
        FOnMessageOut(ProgressMessage);
      end;

    if not IsPondering then     //  don't stop for time if pondering
      begin
      ElapsedTicks := GetElapsedTicks;
      if ElapsedTicks > TickLimit then
        Stop := true;

      RequiredTicks := round(ElapsedTicks * power(2, 1.25 * Log2( UInt64ToDouble(ElapsedTicks) * 1000.0 / TickFrequency) / SearchDepth1));
      if SoftTimeLimit and (RequiredTicks > TickLimit) then
        Stop := true;
      end;

    inc(SearchDepth1);

    if SearchDepth1 > DepthLimit then
      SupressProgressMessage := true;

    // if pondering then continue to loop even when depth limit is reached

    if IsPondering and not Stop then
      SearchDepth1 := min(SearchDepth1, DepthLimit);
    end;

  SearchTime_ms := GetElapsedMilliseconds;

  if BestPV[0] > (ScoreMaxValue - abs(BestScore)) then
    BestPV[0] := min(BestPV[0], ScoreMaxValue - abs(BestScore));

  if BestPV[0] > 0 then
    begin
    Search_Result.Depth := BestDepth;
    Search_Result.selDepth := selDepth;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    for i := 0 to BestPV[0] do
      Search_Result.PV[i] := BestPV[i];
    Search_Result.score := BestScore;
    end
   else
    begin
    Search_Result.Depth := 0;
    Search_Result.selDepth := 0;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    Search_Result.PV[0] := 0;
    Search_Result.score := ScoreMinValue;
    end;

  if DoProgressUpdates then
    begin
    if IsPondering then               // if stop command received when pondering, only return best move
      begin
      if Search_Result.PV[0] >= 1 then
        ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr
       else if Search_Result.PV[0] = 0 then
        ProgressMessage := 'bestmove (none)';
      end
     else
      begin
      if Search_Result.PV[0] > 1 then
        ProgressMessage := ResultToStr + #13#10 + 'bestmove ' + Search_Result.PV[1].ToStr + ' ponder ' + Search_Result.PV[2].ToStr
       else if Search_Result.PV[0] = 1 then
        ProgressMessage := ResultToStr + #13#10 + 'bestmove ' + Search_Result.PV[1].ToStr
       else if Search_Result.PV[0] = 0 then
        ProgressMessage := ResultToStr + #13#10 + 'bestmove (none)';
      end;

    if Assigned(FOnMessageOut) then
      FOnMessageOut(ProgressMessage);
    end;

  IsPondering := false;
  Searching := false;
  ReadyToSearch := false;
  Stop := false;

  if Assigned(fOnSearchFinished) then
    fOnSearchFinished(Search_Result);

  PrepareForNextSearch;
  end;


function UInt64ToDouble(value : UInt64) : double;
  begin
  result := value;
  end;


function ExtendTime(BudgetTime, HardLimit : UInt64) : UInt64;
  const
    factor = 1.33;    // if outside aspiration window then allow additional search time for move

  begin
  result := min(HardLimit, UInt64(round(UInt64ToDouble(BudgetTime) * factor)));
  end;


function AllocateTimeForSearch(MovesToGo, TimeRemaining, Increment, OpponentTimeRemaining, OpponentIncrement, MoveNumber : UInt64; WillPonder : boolean) : UInt64;

  // time reserve = increment x factor

  const
    ReserveFactor = 2;        // Time safety factor
    AdditionFactor = 1.66;    // Factor to increase target time because actual time used is typically less due to insufficent time to complete another full iteration

    BackLoadStart = 0.56;       // Backload reduces target time for early moves and increases time for mid game moves
    BackLoadIncrement = 0.02;
    BackLoadLimit = 1.72;

    PonderFactor = 3;          // assumes 1 in 3 ponderhit

  var
    RemainingMoves, BackLoading : double;
    PonderIncrease : UInt64;

  begin
  PonderIncrease := 0;
  if WillPonder = true then
    begin
    if MovesToGo > 0 then
      PonderIncrease := (OpponentTimeRemaining + MovesToGo * OpponentIncrement) div PonderFactor
     else
      begin
      RemainingMoves := MAX((((((((6.3830E-12 * MoveNumber) -5.971148E-09) * MoveNumber + 2.167290653E-06) * MoveNumber) -3.866640315E-04) * MoveNumber + 3.456441656E-02) * MoveNUmber -1.44359537) * MoveNumber + 51.856, 4);
      PonderIncrease := (OpponentTimeRemaining + round(RemainingMoves) * OpponentIncrement) div PonderFactor;
      end;
    end;


  if MovesToGo > 0 then
    result := ((MovesToGo * Increment + TimeRemaining + PonderIncrease) * 112) div (128 * (max(MovesToGo, 1)))
   else
    begin
    RemainingMoves := MAX((((((((6.3830E-12 * MoveNumber) -5.971148E-09) * MoveNumber + 2.167290653E-06) * MoveNumber) -3.866640315E-04) * MoveNumber + 3.456441656E-02) * MoveNUmber -1.44359537) * MoveNumber + 51.856, 4);
    BackLoading := min(BackLoadIncrement * MoveNumber + BackLoadStart, BackLoadLimit);

    result := round( min(AdditionFactor * BackLoading * UInt64ToDouble( max(TimeRemaining + PonderIncrease - Increment * ReserveFactor, 0) ) / RemainingMoves + Increment, TimeRemaining / ReserveFactor));
    end;
  end;


initialization

  Book.LoadFromResource('Open_Book');

end.




