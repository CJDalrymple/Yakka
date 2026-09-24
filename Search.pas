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


unit Search;

interface

 {$A16}

uses
  Winapi.Windows, SysUtils, System.SyncObjs, System.Classes, Math, Threading, Diagnostics,
  Common, GameDef, GameNet, Openbook, syzygy;

  {$IF defined(MSWINDOWS)}
  function GenRandom(pbBuffer :PBYTE; dwLen: DWORD):BOOL; stdcall;
  function _aligned_malloc(size: NativeUInt; alignment: NativeUInt): Pointer; cdecl; external 'msvcrt.dll';
  procedure _aligned_free(block: Pointer); cdecl; external 'msvcrt.dll';
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

  MaxSearchPly = 62;

  ScoreMinValue = -32767 + MaxSearchPly * 2;         // -2^15 + 1 - margin
  ScoreMaxValue =  32767 - MaxSearchPly * 2;         //  2^15 - 1 + margin

  MateScoreCutoff = ScoreMaxValue - MaxSearchPly * 2;

  TB_Win = 24_000;
  TB_WinCutoff =  TB_Win - 1000;
  TB_UnknownResult = -MateScoreCutoff + 1000;

  ScoreOffset = 32768;                               //  2^15

  InfiniteValue = 65535;                             // 2^16 -1
  InvalidScore = -InfiniteValue;
  InvalidMove = 0;

  GuardValue = 31_415_962;

  TimeLimit_Max = Int64($4000000000);    // 2^38       to prevent overflow when calculating tick limit
  NodeLimit_Max = Int64($4000000000);    // 2^38

              // MVV             LVA
              // captured piece, piece

  MVV_LVA_Lookup : array[0..king, pawn..king] of integer =
           (( 0,  0,  0,  0,  0,  0),
            ( 5,  4,  4,  3,  2,  1),
            (10,  9,  9,  8,  7,  6),
            (10,  9,  9,  8,  7,  6),
            (15, 14, 14, 13, 12, 11),
            (20, 19, 19, 18, 17, 16),
            ( 0,  0,  0,  0,  0,  0));


  // Search Parameters

  AspirationMargin = 91;
  AspirationMinDepth = 5;
  QSearchPruneMargin = 205;
  ImprovingMargin = 18;
  RFP_Margin_1 = 124;
  RFP_Margin_2 = 197;
  RazoringMargin = 146;
  R_Null = 4;
  NMP_VerificationDepth = 4;
  sExtensionMargin = 8;
  LMP_SEEMargin = 94;
  LMR_FullDepthMoves = 3;
  LMR_GameStageCutoff = 12;
  LMR_QuietHistoryMargin = 540;

  // Time Management Parameters

  PonderFactor = 3.0;          // assumes 1 in 3 ponderhit
  AspirationFactor = 1.33;     // budget time increase on aspiration research
  BaseTimeFactor = 1.81;
  CarryoverFactor = 0.83;      // carryover unused move budget to next move
  StabilityStart = 1.15;       // increase for bestmove change
  StabilityDecay = 0.188;
  BackLoadStart = 0.66;
  BackLoadIncrement = 0.017;

type
  TGameMoveList = record
    var
     MoveArray : array[0..639] of TMove;

    class operator Initialize(out Dest: TGameMoveList);
    function Count : integer;
    function Size : integer;
    procedure Clear;
    procedure AddMove(Move : TMove);
    function RemoveLastMove : TMove;
    end;


type
  TUpdateRec = record

    BestScore : integer;
    BestMove : TMove;
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
  THistList = array[0..MaxSearchPly + 8] of Integer;        // first value array[0] = hash value at ply = 0, etc

type
  TRepHashList = array[0..1023] of UInt64;

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

      SearchAge : Int64;  // SearchAge : [0..1023]

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

    function RetrieveData(HashCode : UInt64; var Data : UInt64) : boolean;
    procedure StoreData(const Board : TBoard; Move : TMove; Score, Depth, Flag, Ply : integer);

    function FillPermill : integer;   //   100% = 1000
    procedure ClearTable;
    procedure NewSearch;
    function Size : integer;
    end;


type
  TQuietHistoryTable = record
    var
      Table : array[0..1, 1..6, 0..63] of integer;

    procedure Update(Player : integer; const Move : TMove; Adjustment : integer);
    procedure ClearTable;
    end;


type
  TContHistoryTable = record
    var
      Table : array[0..12*64-1, 0..12*64-1] of integer;

    procedure Update(Player : integer; const PrevMove, Move : TMove; Adjustment : integer);
    function Probe(Player : integer; const PrevMove, Move : TMove) : integer;
    procedure ClearTable;
    end;


type
 TCounterMoveTable = record
    var
      Table : array[1..6, 0..63] of TMove;

    function Probe(Move : TMove) : TMove;
    procedure Update(const PrevMove, CounterMove : TMove);
    procedure ClearTable;
    end;


type
  T_ThreadData = record
    var
      RepHashList : TRepHashList;
      HashCount : integer;

      KillerMoveA : TKillerMoveTable;
      KillerMoveB : TKillerMoveTable;

      NodeCount, PrevCount : Int64;
      SearchTime : UInt64;
      selDepth : integer;

      FirstRootMoveSearched : boolean;
      ID : integer;
      InitialDepth : integer;
      RootWhiteMaterial, RootBlackMaterial : integer;
      NullCount : integer;

      RootPV : TPVArray;
      SearchingPV : boolean;

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
   PV : TPVArray;
   Score : integer;
   ScoreType : integer;
   MessageOut : string;
   end;


type
  TMessageOutProc = procedure(const msg : AnsiString);

type
  TSearch = class

      TransTable : TTransTable;
      ABDADA_Table : T_ABDADA_Table;

      QuietHistory : TQuietHistoryTable;
      ContHistory : TContHistoryTable;

      WhiteCounterMove : TCounterMoveTable;
      BlackCounterMove : TCounterMoveTable;

      StartTick : Int64;
      TickFrequency : Int64;
      TickLimit : Int64;
      TickCarryover : Int64;

      Searching : Boolean;
      Stop : Boolean;
      ShutDownUnderway : Boolean;
      ReadyToSearch :Boolean;

      ThreadPool: TThreadPool;

      // Search Information

      NodeCount : Int64;
      SearchTime_ms : Int64;

      // Search Parameters

      TimeLimit : UInt64;
      StartBudgetTime : UInt64;
      AdjBudgetTime : UInt64;

      UseSoftTimeLimit : boolean;
      IsPondering : boolean;

      NodeLimit : Int64;
      DepthLimit : integer;
      ThreadCount : integer;
      MultiPVCount : integer;

      // Search Options:

      DoProgressUpdates : boolean;
      SupressBestMove : boolean;
      DoAspirationSearch : boolean;

      Use_TB : boolean;
      TB_MaxPieceCount : integer;
      TB_FilesPath : AnsiString;
      TB_RootHit : boolean;

      UseOwnBook : boolean;
      ShowWDL : boolean;
      WillPonder : boolean;

      RepStartBoard : TBoard;
      SearchMoveList : TMoveArray;

      Search_Result : TSearchData;
      BestMove : UInt64;

      FOnSearchProgressUpdate : TNotifyEvent;
      FOnSearchFinished : TNotifyEvent;
      FOnMessageOut : TMessageOutProc;

      constructor Create;
      destructor Destroy; override;

      procedure SetDefault;
      procedure NewGame;
      procedure PrepareForNextSearch;

      procedure StartInBackGround(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure FullSearch(const Board : TBoard; const GameMoveList : TGameMoveList);
      function BookMoveFound(const Board : TBoard) : boolean;
      procedure Probe_TB(const Board : TBoard; var Moves : TMoveArray; var TB_ResultFound, TB_BestMoveFound : boolean; HashCount : integer; const RepHashList : TRepHashList);
      procedure RateMoves(var Board : TBoard; PrevMove : TMove; var moves : TMoveArray);

      procedure ABDADA_Search(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure ABDADA_Search_MultiThread(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure ABDADA_Search_SingleThread(const Board : TBoard; const GameMoveList : TGameMoveList);

      function PVS_Q(ThreadData : T_ThreadDataPtr; ply, alpha, beta, CheckCount: Integer; var Board : TBoard; Accumulator: PAccumulator): integer;
      function PVS_ABDADA_Root(var Moves : TMoveArray; ThreadData : T_ThreadDataPtr; depth, alpha, beta : Integer; var Board : TBoard; PV_array : T_PV_array) : integer;
      function PVS_ABDADA(ThreadData : T_ThreadDataPtr; ply, depth, alpha, beta, NodeType : Integer; var Board : TBoard; var PV : TPVArray; Accumulator: PAccumulator;
               DoNullMove : boolean; PrevMove, exclMove : TMove; InCheckHint : integer) : integer;

      procedure ClearHistoryTables;
      procedure ClearCounterMoveTables;
      procedure SetTransTableSize(size : integer);
      procedure SetThreadCount(value : integer);
      procedure ClearTranspositionTable;
      procedure Clear_ABDADA_Table;

      function TicksToMilliseconds(Ticks : Int64) : UInt64;
      function MillisecondsToTicks(Milliseconds : UInt64) : Int64;
      function BudgetToTicks(Budget : UInt64) : Int64;
      procedure AllocateTimeForSearch(MovesToGo, wtime, winc, btime, binc : UInt64; const Board : TBoard);

     public
      property OnSearchFinished: TNotifyEvent read FOnSearchFinished write FOnSearchFinished;
      property OnProgressUpdate: TNotifyEvent read FOnSearchProgressUpdate write FOnSearchProgressUpdate;

      function GetElapsedTicks : Int64;
      function GetElapsedMilliseconds : Int64;

      procedure StopSearch;
      procedure ConcludeSearch; overload;
      procedure ConcludeSearch(const FinalMsg : string); overload;
      procedure Shutdown;

      function ResultMessage : string;
      function ResultToStr : AnsiString;

      property OnMessage : TMessageOutProc read FOnMessageOut write FOnMessageOut;

    end;

type
  TSearchPtr = ^TSearch;

procedure RateMovesQ(var moves : TMoveArray);
procedure SortMoves(var moves : TMoveArray);

function UInt64ToDouble(value : UInt64) : double;
function IsMateScore(Score : integer) : boolean;


implementation

{$CODEALIGN 16}

{$IF defined(MSWINDOWS)}
function GenRandom; external ADVAPI32 name 'SystemFunction036';
{$ENDIF}


//  LMR_Cands, LMR_HistHits, LMP_Cands, LMP_Prunes : Int64;

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
  //    Age              :   bits 36..45   :  Half Move Count
  //                         bit  46       :  Used during search to track LMR candidates when moves are swapped
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


function TTransTable.Size;
  begin
  result := length(Table) div 65_536;
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
  result := integer((Info shr 36) and $3FF);
  end;


function TTransTable.Priority(Info : UInt64) : integer;
  begin
  result := integer((Info shr 24) and $3FFFFF);
  end;


function TTransTable.Score(Info : UInt64; Ply : integer) : integer;
  begin
  result := integer(Info shr 48) - ScoreOffset;

  if result >= TB_WinCutoff then          // retrieving Mate score (or TB soft mate) from TT, so subtract ply
    result := result - ply
   else if result <= -TB_WinCutoff then   // retrieving Mated score (or TB soft mated) from TT, so add ply
    result := result + ply;
  end;


function TTransTable.Move(Info : UInt64) : TMove;
  begin
  result := Info and $0000000000FFFFFF;
  end;


procedure TTransTable.StoreData(const Board : TBoard; Move : TMove; Score, Depth, Flag, Ply : integer);
  var
    PackedData, HashCode, AdjustedScore : UInt64;
    index : UInt64;

  begin

  HashCode := Board.Hash;
  index := (HashCode and TableMask);

  AdjustedScore := UInt64(Score + ScoreOffset);

  if score >= TB_WinCutoff then
    AdjustedScore := AdjustedScore + ply                                        // saving Mate score to TT, so add ply
   else if score <= -TB_WinCutoff then
    AdjustedScore := AdjustedScore - ply;                                       // saving Mated score to TT, so subtract ply

  PackedData := ((AdjustedScore and $FFFF) shl 48) or
                (UInt64(SearchAge and $3FF) shl 36) or
                (UInt64(Depth and $3F) shl 30) or
                (UInt64(Flag and $3F) shl 24) or
                (Move and $0000000000FFFFFF);

  if (PackedData and $3FFFFF000000) >= (Table[Index].Data and $3FFFFF000000) then  // if priority of new entry is greater than that stored in first slot
    begin                                                                          // then replace entry in first slot
    if (move = 0) and ((Table[index].UID xor Table[index].Data) = HashCode) then   // if new Data doesn't have a move, then keep previous move
      PackedData := PackedData or (Table[index].Data and UInt64($FFFFFF));

    Table[index].UID := HashCode xor PackedData;
    Table[index].Data := PackedData;
    exit;
    end;

  if (Table[index].UID xor Table[index].Data) = HashCode then
     exit;

  inc(index);

  if (Table[index].UID xor Table[index].Data) = Hashcode then
    begin
    if (PackedData and $3FFFFF000000) < (Table[Index].Data and $3FFFFF000000) then
      exit
     else if (PackedData and UInt64($FFFFFF)) = 0 then
      PackedData := PackedData or (Table[index].Data and UInt64($FFFFFF));      // if new Data doesn't have a move, then keep previous move
    end;

  Table[index].UID := HashCode xor PackedData;                                  // always replace
  Table[index].Data := PackedData;
  exit;
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
  SearchAge := SearchAge and $3FF;
  end;





function TTransTable.FillPermill : integer;
  var
    i, FillCount : integer;

  begin
  FillCount := 0;
  if SearchAge > 0 then
    for i := 0 to 1023 do
      if Age(Table[i].Data) = SearchAge then
        inc(FillCount);

  result := (FillCount * 1000) div 1024;
  end;


// Quiet History Table ===============================================================

procedure TQuietHistoryTable.Update(Player : integer; const Move : TMove; Adjustment : integer);
  var
    Piece, Dest : integer;
  begin
  Piece := (Move shr 12) and $F;
  Dest :=  (Move shr 6) and $3F;

  Table[Player, Piece, Dest] := Table[Player, Piece, Dest] + Adjustment
                              - (Table[Player, Piece, Dest] * abs(Adjustment)) div 8192;
  end;


procedure TQuietHistoryTable.ClearTable;
  begin
  FillChar(Table, SizeOf(Table), 0);
  end;


// Continuation History Table ===============================================================

procedure TContHistoryTable.Update(Player : integer; const PrevMove, Move : TMove; Adjustment : integer);
  var
    Piece, Dest, index1, index2 : integer;

  begin
  if PrevMove = 0 then
    exit;

  Piece := ((Move shr 12) and $F) - 1;
  Dest :=  (Move shr 6) and $3F;
  index1 := ((Player*6) + Piece) * 64 + Dest;

  Piece := ((PrevMove shr 12) and $F) - 1;
  Dest :=  (PrevMove shr 6) and $3F;
  index2 := ((1-Player)*6 + piece) * 64 + Dest;

  Table[index1, index2] := Table[index1, index2] + Adjustment - (Table[index1, index2] * abs(Adjustment)) div 512;
  end;


function TContHistoryTable.Probe(Player : integer; const PrevMove, Move : TMove) : integer;
  var
    Piece, Dest, index1, index2 : integer;

  begin
  Piece := ((Move shr 12) and $F) - 1;
  Dest :=  (Move shr 6) and $3F;
  index1 := ((Player*6) + Piece) * 64 + Dest;

  Piece := ((PrevMove shr 12) and $F) - 1;
  Dest :=  (PrevMove shr 6) and $3F;
  index2 := ((1-Player)*6 + piece) * 64 + Dest;

  result := Table[index1, index2];
  end;


procedure TContHistoryTable.ClearTable;
  begin
  FillChar(Table, SizeOf(Table), 0);
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
  begin
  FillChar(Table, SizeOf(Table), 0);
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
  result := (Table[index] = MoveHash);
  end;


procedure T_ABDADA_Table.ClearTable;
  begin
  FillChar(Table, SizeOf(Table), 0);
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

  RootPV[0] := 0;
  SearchingPV := false;

  RootWhiteMaterial := 0;
  RootBlackMaterial := 0;
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


function TGameMoveList.Size : integer;
  begin
  result := High(MoveArray);
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
    i : integer;
    Move, Score : UInt64;

  begin
  for i := 1 to Moves[0] do
    begin
    Move := Moves[i];                                                 // MVV-LVA
    score:= ((Move and $FF000) xor $F000) shl 36;                     // score := (15 + CapturedPiece * 16 - piece) shl 36 ;
    moves[i] := (move and UInt64($0000FFFFFFFFFFFF)) or Score;
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


procedure TSearch.RateMoves(var Board : TBoard; PrevMove : TMove; var moves : TMoveArray);
  var
    i, temp : integer;
    score, piece, promotionpiece, capturedpiece, SEE_value, Dest : integer;
    TempBoard : TBoard;

  begin
  for i := 1 to moves[0] do
    begin
    Piece := (Moves[i] shr 12) and $F;
    CapturedPiece := (Moves[i] shr 16) and $F;
    Dest := (moves[i] shr 6) and $3F;
    PromotionPiece := (Moves[i] shr 20) and $F;

    if CapturedPiece > 0 then
      begin
      score := MVV_LVA_Lookup[capturedpiece, Piece] + $7000;

      if PieceValue[Piece] > PieceValue[CapturedPiece] then
        begin
        TempBoard := CopyBoard_asm(Board);
        TempBoard.MakeMoveNoHash(Moves[i]);

        SEE_value := PieceValue[CapturedPiece] - TempBoard.SEE(Moves[i]);

        if SEE_value < 0 then
          score := SEE_value + $1000;      //  losing capture
        end;
      end
     else
      begin
      score := QuietHistory.Table[Board.ToPlay, Piece, Dest] div 4 + $2000;    // ensure score not -ve
      if PrevMove <> 0 then
        begin
        temp := ContHistory.Probe(Board.ToPlay, PrevMove, Moves[i]);
        score := score + temp;
        end;
      end;

    if (PromotionPiece > 0) and (score < $7000) then

      case PromotionPiece of

      2 : score := $3000;
      3 : score := $1000;
      4 : score := $1000;
      5 : score := $6000;

      end;

    moves[i] := moves[i] and UInt64($0000FFFFFFFFFFFF) or (UInt64(Score and $FFFF) shl 48);
    end;
  end;


procedure SortMoves(var moves : TMoveArray);
  var
    i, j : integer;
    temp : TMove;

  begin
  // Insertion sort

  for i := 2 to Moves[0] do
    begin
    temp := Moves[i];
    j := i - 1;

    while (j >= 1) and (Moves[j] < temp) do
      begin
      Moves[j + 1] := Moves[j];
      dec(j);
      end;

    Moves[j + 1] := temp;
    end;
  end;


function IsMateScore(Score : integer) : boolean;
  begin
  result := (Abs(score) >= MateScoreCutoff);
  end;


//   TSearchEngine_PVS =======================================================

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
    ThreadPool.SetMaxWorkerThreads(TThread.ProcessorCount + 1);
    ThreadPool.SetMinWorkerThreads(TThread.ProcessorCount + 1);
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

  if syzygy.TB_Initialized and (TB_MaxPieceCount > -1) then
    TB_Free;

  inherited;
  end;


function TSearch.PVS_Q(ThreadData : T_ThreadDataPtr; ply, alpha, beta, CheckCount: Integer; var Board : TBoard; Accumulator: PAccumulator): integer;

  var
    k : integer;
    value, SEE_value, Standpat : integer;
    Moves : TMoveArray;
    Movecount : integer;
    ChecksRemaining :integer;
    InCheck : boolean;
    Move : TMove;
    piece, capturedpiece : integer;
    Alignable_Accumulator : TAlignableAccumulator;
    Updated_Accumulator : PAccumulator;

    TempBoard, TempBoard_2 : TBoard;

  begin
  if Stop = true then
    exit(InvalidScore);

  if (alpha + 1 <> beta) and (ply + 1 > ThreadData.SelDepth) then
    ThreadData.SelDepth := ply + 1;

  if ply = MaxSearchPly then
    exit(ScoreFromAccumulator(Accumulator, Board));

  alpha := max(alpha, ScoreMinValue + ply);
  beta := min(beta, ScoreMaxValue - ply - 1);

  if alpha >= beta then
    exit(alpha);

  InCheck := Board.KingInCheck(Board.ToPlay);

  if (Board.HalfCount >= 100) and not InCheck then
    exit(0);

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
    movecount := Board.GetValidMoves(Board.ToPlay, Moves, All)   // When the king is in check, all moves are searched.
   else
    begin
    if CheckCount = 0 then
      movecount := Board.GetValidMoves(Board.ToPlay, Moves, ViolentOnly)    // Capture and Promotion Moves only, - no quiet moves
     else
      begin
      movecount := Board.GetCapturePromotionCheckingMoves(Board.ToPlay, Moves);   // Capture, Promotion and Quiet Checking moves
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


  Updated_Accumulator := PAccumulator(((NativeUInt(@Alignable_Accumulator) + 63) and not 63));

  for k := 1 to movecount do
    begin
    Move := Moves[k];

    TempBoard := CopyBoard_asm(Board);
    TempBoard.MakeMove(Move);

    Inc(ThreadData.NodeCount);

    CapturedPiece := integer((Move shr 16) and $F);
    Piece :=  integer((Move shr 12) and $F);

    if (InCheck = false) and (CapturedPiece > 0) and (PieceValue[Piece] >= PieceValue[CapturedPiece]) then
      begin
      TempBoard_2 := CopyBoard_asm(TempBoard);
      SEE_value :=  PieceValue[CapturedPiece] - TempBoard_2.SEE(Move);

      if SEE_value < 0 then     // losing move so prune it, but only if not in check, and if it doesn't give check
        continue;

      // Prune neutral captures when static eval is low

      if (SEE_value = 0) and (Standpat + QSearchPruneMargin <= alpha) then
        continue;
      end;

    Update_Accumulator(Accumulator, Updated_Accumulator, Move, TempBoard);
    value := -PVS_Q(threadData, ply+1, -beta, -alpha, ChecksRemaining, TempBoard, Updated_Accumulator);

    if value = - InvalidScore then
      exit(InvalidScore);

    if value > alpha then
      alpha := value;

    if value >= beta then
      exit(value);
    end;

  result := alpha;
  end;


function TSearch.PVS_ABDADA(ThreadData : T_ThreadDataPtr; ply, depth, alpha, beta, NodeType : Integer;
               var Board : TBoard; var PV : TPVArray; Accumulator: PAccumulator;
               DoNullMove : boolean; PrevMove, exclMove : TMove; InCheckHint : integer) : integer;
  const
    R = 3;

  var
    i, k, f : integer;
    value, TTFlag, alphaOrig, StaticEval, Best : integer;
    Move, KillerMove, BestMove, CounterMove, TempMove, TTMove : TMove;
    Moves : TMoveArray;
    Movecount, QuietsTried : integer;
    HashCode, TT_Data, MoveHash : UInt64;
    TempEp : UInt64;
    DataExists, DataExists_A, InCheck, IsPV, Improving : boolean;
    PV1 : TPVArray;
    NewNodetype, c, m, epCell, piece : integer;
    NewDepth, NullDepth, FullDepth : integer;
    LMR_Flag : boolean;
    FullDepthMoves, LMR : integer;
    sDepth, sValue, sBeta, sExtension, TTScore, TTDepth : integer;
    NowTick, ElapsedTicks : Int64;
    PlayedCount, SEE_Value, trigger : integer;

    TempBoard, TempBoard_2 : TBoard;
    Alignable_Accumulator : TAlignableAccumulator;
    Updated_Accumulator : PAccumulator;

    WDL_result, TB_score, TB_bound, modifier : integer;
    TB_success : boolean;
    ChildInCheck : integer;

  begin
  PV[0] := 0;

  if Stop or (ply > MaxSearchPly) then
    exit(InvalidScore);

  if not IsPondering then
    begin
    QueryPerformanceCounter(NowTick);

    ElapsedTicks := NowTick - StartTick;

    if ElapsedTicks < 0 then
      ElapsedTicks := ElapsedTicks + High(Int64);

    if ElapsedTicks > TickLimit then
      begin
      Stop := true;
      exit(InvalidScore);
      end;

    if NodeCount >= NodeLimit then
      begin
      Stop := true;
      exit(InvalidScore);
      end;
    end;

  IsPV := (alpha + 1 <> beta);


  // Mate distance pruning

  alpha := max(alpha, ScoreMinValue + ply);
  beta := min(beta, ScoreMaxValue - ply - 1);

  if alpha >= beta then
    exit(alpha);


  // check for draw by insufficient mating material

  if Board.Gamestage <= 6 then
    if Board.InsufficientMaterial then
      exit(0);


  //  Check for repetition & if so score this position as draw

  PlayedCount := ThreadData.HashCount;
  if ThreadData.NullCount = 0 then
    begin

    m := max(PlayedCount + ply - Board.HalfCount, 0);
    i := PlayedCount + ply - 4;

    trigger := 0;
    while i >= m do
      begin
      if Board.Hash = ThreadData.RepHashList[i] then
        begin
        if i >= PlayedCount then
          trigger := trigger + 2
         else
          trigger := trigger + 1;
        if trigger >= 2 then
          exit(0);
        end;
      i := i - 2;
      end;
    end;


  // protect PV from overflow

  if ply = MaxSearchPly then
    exit(ScoreFromAccumulator(Accumulator, Board));

  if InCheckHint < 0 then
    InCheck := Board.KingInCheck(Board.ToPlay)
   else
    InCheck := (InCheckHint <> 0);

  if (Board.HalfCount >= 100) and not InCheck then
    exit(0);


  // do depth extension before probing TT to ensure consistency when setting alpha & avoid bug in search

  if (InCheck and (Depth < 2)) or (PrevMove.PromotionPiece <> 0) then
    depth := depth + 1;

  Best := InvalidScore;
  TB_score := -InvalidScore;
  BestMove := 0;
  alphaOrig := alpha;
  TTMove := 0;


  // syzygy

  if (Use_TB = true) and (TB_RootHit = false) and (bitcount(Board.WhitePegs or Board.BlackPegs) <= TB_MaxPieceCount) then
    begin
    TB_success := false;
    WDL_result := Syzygy.TB_ProbeWDL(Board, TB_success);

    if TB_success = true then
      begin
      if Board.ToPlay = White then
        modifier := (ThreadData.RootWhiteMaterial - ThreadData.RootBlackMaterial)    // modifier used to try to avoid pointless sacrifice to decend into known won position
                  - (Board.MaterialValue(white) - Board.MaterialValue(black))        // i.e. prefer won position with most material
       else
        modifier := (ThreadData.RootBlackMaterial - ThreadData.RootWhiteMaterial)
                  - (Board.MaterialValue(black) - Board.MaterialValue(white));

        case WDL_result of

        -2 :  begin                          // TB loss
              value := -TB_Win + ply - modifier;
              TB_bound := ftUpperBound;
              end;

        -1 :  begin                          // TB blessed loss
              value := -1;
              TB_bound := ftExact;
              end;

        0  :  begin                          // TB draw
              value := 0;
              TB_bound := ftExact;
              end;

         1 :  begin                          // TB cursed win
              value := 1;
              TB_bound := ftExact;
              end;

        2 :   begin                          // TB win
              value := TB_Win - ply - modifier;
              TB_bound := ftLowerBound;
              end
            else
              begin                    // probe returned something unexpected - ignore the TB hit
              TB_success := false;
              TB_bound := ftUnknown;
              value := 0;
              end;
        end;

      if (TB_bound = ftExact) or ((TB_bound = ftUpperBound) and (value <= alpha)) or ((TB_bound = ftLowerBound) and (value >= beta)) then
        exit(value);

      if TB_bound = ftLowerBound then
        begin
        best := value;
        alpha := max(alpha, value);
        end
       else
        TB_score := value;

      if depth = 0 then
        exit(value);
      end;
    end;


  //  Evaluate if leaf node

  if depth = 0 then
    begin
    value := PVS_Q(ThreadData, ply, alpha, beta, 1, Board, Accumulator);

    if Stop then
      exit(InvalidScore);

    exit(Value);
    end;

  DataExists := TransTable.RetrieveData(Board.Hash, TT_Data);


  // Check transposition table

  if DataExists = true then
    begin
    TTDepth := TransTable.Depth(TT_Data);
    TTscore := TransTable.Score(TT_Data, ply);
    TTMove := TransTable.Move(TT_Data);
    TTFlag := TransTable.Flag(TT_Data);

    if not IsPV and (TTDepth >= depth) then
      begin
      if exclMove = 0 then
        begin
        if (TTFlag <> ftUpperBound) and (TTscore >= beta) then    // i.e. a lower bound or exact value above beta so can cut
          exit(TTscore)
         else if (TTFlag <> ftLowerBound) and (TTscore <= alpha) then   // i.e. an upper bound or exact value that is below alpha so can cut
          exit(TTscore);
        end;

      if (TTFlag = ftUpperBound) and (TTscore < Beta) and (depth - R_Null - depth div 4 <= TTDepth)  then
        DoNullMove := false;
      end;
    end;

  Improving := false;
  StaticEval := InvalidScore;
  if not InCheck then
    StaticEval := ScoreFromAccumulator(Accumulator, Board);
  ThreadData.EvalHist[ply] := StaticEval;

  if not InCheck and (ply >= 6) then
    begin
    if (ThreadData.EvalHist[ply-2] <> InvalidScore) and (ThreadData.EvalHist[ply] > ThreadData.EvalHist[ply - 2] + ImprovingMargin) then
      Improving := true
     else if (ThreadData.EvalHist[ply-4] <> InvalidScore) and (ThreadData.EvalHist[ply] > ThreadData.EvalHist[ply - 4] + ImprovingMargin) then
      Improving := true;
    end;


  // Initial Pruning / Reduction

  if (depth <= 3) and not (IsPV or InCheck) and (exclMove = 0) then
      begin

      // Reverse Futility Pruning
      if (depth = 1) and ((StaticEval - RFP_Margin_1) >= Beta) then
        exit((StaticEval + Beta)  div 2);

      // razoring
      if (StaticEval <= alpha - RazoringMargin * depth) and (abs(alpha) < 800) then       // quiet move score likely to be less than 120 improvement
        begin
        value :=  PVS_Q(ThreadData, ply, alpha, beta, 1, Board, Accumulator);
        if value <= alpha then
          exit(value);
        end;

      // Reverse Futility reduction
      if (depth = 2) and ((StaticEval - RFP_Margin_2) >= Beta) then
        depth := 1;
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

  if DoNullMove and (depth > 2) and (movecount > 4) and not (InCheck or IsPV) and (exclMove = 0) then
    if not Board.PawnsOnly(Board.ToPlay) then
      begin
      value := StaticEval;

      if value >= beta then
        begin
        PV1[0] := 0;
        epCell := -1;

        if Board.Enpassant <> 0 then
          begin
          epCell := GetLowBit(Board.Enpassant);
          Board.Hash := Board.Hash xor Board.epHash[epCell];    // remove epCell from Hash, CastleFlags don't change

          tempEp := Board.Enpassant;
          Board.Enpassant := 0;
          end;

        Board.Hash:= Board.Hash xor Board.PlayerHash;           // swap player
        Board.ToPlay := 1 - Board.ToPlay;                       // swap player

        NullDepth := max(depth - R_Null - depth div 4 - ord(improving), 1);

        inc(ThreadData.NullCount);

        value := -PVS_ABDADA(ThreadData, ply+1, NullDepth, -beta, -beta+1, -NodeType, Board, PV1, Accumulator, OmitNull, 0, 0, 0);

        dec(ThreadData.NullCount);

        Board.ToPlay := 1 - Board.ToPlay;
        Board.Hash := Board.Hash xor Board.PlayerHash;

        if EpCell <> -1 then
          begin
          Board.Hash := Board.Hash xor Board.epHash[epCell];    // add epCell back into Hash
          Board.Enpassant := tempEp;
          end;

        if Stop then
          exit(InvalidScore);

        if value = -InvalidScore then
          exit(InvalidScore);

        if (value >= beta) and (abs(value) < TB_WinCutoff) then        // verify in case zugzwang, adds approx 10% to search time but avoids at least some errors
          if (depth > NMP_VerificationDepth) or (depth > ply) then     // don't do verification at lower depths
            begin
            value := PVS_ABDADA(ThreadData, ply, NullDepth, beta - 1, beta, NodeType, Board, PV1, Accumulator, OmitNull, PrevMove, 0, 0);

            if value = InvalidScore then
              exit(InvalidScore);
            end;

        if (value >= beta) and (abs(value) < TB_WinCutoff) then
          exit(beta);
        end;
      end;

  RateMoves(Board, PrevMove, Moves);


  // Killer Move Hueristic

  KillerMove := ThreadData.KillerMoveA[ply];
  if KillerMove.IsValid then
    for i := 1 to Movecount  do
      if (Moves[i] and $FFFFFF) = (KillerMove and $FFFFFF) then
        begin
        if Moves[i] < UInt64($5003000000000000) then
          Moves[i] := Moves[i] and UInt64($00000FFFFFFFFFFF) or UInt64($5003000000000000);
        break;
        end;

  KillerMove := ThreadData.KillerMoveB[ply];
  if KillerMove.IsValid then
    for i := 1 to Movecount  do
      if (Moves[i] and $FFFFFF) = (KillerMove and $FFFFFF) then
        begin
        if Moves[i] < UInt64($5002000000000000) then
          Moves[i] := Moves[i] and UInt64($00000FFFFFFFFFFF) or UInt64($5002000000000000);
        break;
        end;


  // Counter Move Hueristic

  if PrevMove <> 0 then
    begin
    if Board.ToPlay = White then
      CounterMove := WhiteCounterMove.Probe(PrevMove)
     else
      CounterMove := BlackCounterMove.Probe(PrevMove);

    if CounterMove <> 0 then
      for i := 1 to Movecount  do
        if (Moves[i] and $FFFFFF) = (CounterMove and $FFFFFF) then
          begin
          if Moves[i] < UInt64($5001000000000000) then
            Moves[i] := Moves[i] and UInt64($00000FFFFFFFFFFF) or UInt64($5001000000000000);
          break;
          end;
    end;


  if ThreadData.SearchingPV and (ThreadData.RootPV[0] > ply) then               // prioritise PV moves (important for correct funtioning of firstrootmovesearched)
    begin
    ThreadData.SearchingPV := false;
    for i := 1 to Movecount do
      if (Moves[i] and $FFFFFF) = (ThreadData.RootPV[ply+1] and $FFFFFF) then
        begin
        Moves[i] := Moves[i] or UInt64($8800000000000000);
        ThreadData.SearchingPV := true;
        break;
        end;
    end;

  if DataExists and (TTMove <> 0) then                                          // identify hash move also as priority to search
    begin
    for i := 1 to Movecount do
      if (Moves[i] and $FFFFFF) = (TTMove and $FFFFFF) then
        begin
        Moves[i] := Moves[i] or UInt64($8000000000000000);
        break;
        end;
    end;


  SortMoves(Moves);

  Updated_Accumulator := PAccumulator(((NativeUInt(@Alignable_Accumulator) + 63) and not 63));


  // Enhanced Forward Pruning

  if (NodeType = Cut_Node) and not InCheck and (exclMove = 0) then
    begin
    if depth > (R+5) then
      begin
      PV1[0] := 0;

      c := 0;
      m := 1;

      while m <= min(4, MoveCount) do
        begin
        Move := Moves[m];                   // to avoid ruining move ordering info

        TempBoard := CopyBoard_asm(Board);
        TempBoard.MakeMove(Move);

        NewNodetype := ALL_Node;

        DataExists_A := TransTable.RetrieveData(TempBoard.Hash, TT_Data);

        if (DataExists_A = true) and (depth - R - 1 <= TransTable.Depth(TT_Data)) and (TransTable.Flag(TT_Data) <> ftLowerBound) then
          value := -TransTable.Score(TT_Data, ply+1)
         else
          begin
          Inc(ThreadData.NodeCount);

          Update_Accumulator(Accumulator, Updated_Accumulator, Move, TempBoard);
          ThreadData.RepHashList[PlayedCount + ply + 1] := TempBoard.Hash;

          value := -PVS_ABDADA(ThreadData, ply+1, depth-1-R, -beta, -beta+1, NewNodetype, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, -1);
          end;

        if Stop then
          exit(InvalidScore);

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

  if (NodeType = Cut_Node) and (depth >= 2) and (exclMove = 0) then
    begin
    for k := 1 to min(MoveCount, 6) do           // Don't test all moves, just try first few
      begin
      Move := Moves[k];                          // to avoid ruining move ordering info

      HashCode := Board.GetMoveHash(Move);

      if TransTable.RetrieveData(HashCode, TT_Data) = true then
        begin
        if TransTable.Depth(TT_Data) >= (depth - 1) then
          if TransTable.Flag(TT_Data) <> ftLowerBound then
            begin
            value := -TransTable.Score(TT_Data, ply+1);

            if (TransTable.Flag(TT_Data) = ftExact) and (value > alpha) and (value < beta) then
              begin
              alpha := value;
              best := value;
              bestmove := (Move and $FFFFFF);
              bestmove.setscore(best);

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

  if (TTMove = 0) and IsPV and (exclMove = 0) then
    if depth > 6 then
      depth := depth - 2;


  // Search

  QuietsTried := 0;

  if (ply > 1) and (depth > 2) and not InCheck then       // only undertake LMR for ply 2 and greater
    begin
    FullDepthMoves := LMR_FullDepthMoves;

    for i := FullDepthMoves to MoveCount do               // flag potential moves for reduction
      if Moves[i] < UInt64($4000000000000000) then        // i.e. don't reduce winning captures, promotion, killer and countermoves
        setbit(Moves[i], 46);
    end;

  MoveHash := 0;

  for k := 1 to movecount do
    begin
    PV1[0] := 0;     // important - must be inside move loop to prevent incorrect PV

    if (exclMove <> 0) and ((Moves[k] and $FFFFFF) = (exclMove and $FFFFFF)) then
      continue;

    if (ThreadCount > 1) and (depth > 2) and (k > 1) then     // no need to do this if it is first move in list
      begin
      f := 0;
      moveHash := (Moves[k] and $FFFFFF) xor Board.Hash xor UInt64(depth * $4AF89F927AD5);

      while (f+k < moveCount) and ABDADA_Table.SearchInProgress(movehash) do    // defer search of this move as another thread is searching it
        begin                                                                          // swap with next move in list
        inc(f);
        moveHash := (Moves[k+f] and $FFFFFF) xor Board.Hash xor UInt64(depth * $4AF89F927AD5);
        tempMove := Moves[k];
        moves[k] := moves[k+f];
        moves[k+f] := tempMove;
        end;
      end;

    move := Moves[k];


    // singular extension

    sExtension := 0;

    if (depth >= 8) and DataExists and (movecount > 1) then
      if (TTMove <> 0) and ((Move and $FFFFFF) = (TTMove and $FFFFFF)) then
        if (abs(TTScore) <= TB_WinCutoff) and (TTFlag <> ftUpperbound) and (TTDepth + 3 >= depth) and (ply < ThreadData.InitialDepth * 2) then
          begin
          sDepth := depth div 2;
          sBeta := TTScore - (depth - ord(improving)) - sDepth;

          sValue := PVS_ABDADA(ThreadData, ply, sDepth, sbeta-1, sbeta, Cut_Node, Board, PV1, Accumulator, OmitNull, PrevMove, TTMove, ord(InCheck));

          if (svalue <> InvalidScore) then
            begin
            if not IsPV and (sValue < sBeta - sExtensionMargin) then
              sExtension := 2
            else if sValue < sBeta then
              sExtension := 1
             else if (sBeta >= beta) and (TTMove.CapturedPiece = 0) then     // another move failed high so position is good
              exit(sBeta)
             else if (TTScore <= alpha) or (TTScore >= Beta) then
              sExtension := -1;
            end;

          PV1[0] := 0;
          end;


    FullDepth := Max(depth - 1 + sExtension, 0);
    newDepth := FullDepth;

    TempBoard := CopyBoard_asm(Board);
    TempBoard.MakeMove(Move);

    Inc(ThreadData.NodeCount);
    ThreadData.RepHashList[PlayedCount + ply + 1] := TempBoard.Hash;
    ChildInCheck := -1;
    LMR_Flag := Getbit(Moves[k], 46);

    if LMR_Flag = true then
      begin
      piece := (Moves[k] shr 12) and $F;
      if (TempBoard.GameStage < LMR_GameStageCutoff) and ((piece = pawn) or (piece = king)) then     // treat king and pawn moves as interesting  : pawn moves has (-20%) + impact on speed of WAC_2018
        LMR_Flag := false
       else
        begin
        ChildInCheck := ord(TempBoard.KingInCheck(TempBoard.ToPlay));                    // move gives check
        if ChildInCheck = 1 then
          LMR_Flag := false;
        end;
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
      begin
      Update_Accumulator(Accumulator, Updated_Accumulator, Move, TempBoard);
      value := -PVS_ABDADA(ThreadData, ply+1, newdepth, -beta, -alpha, NewNodeType, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, -1);
      end
     else
      begin
      LMR := 0;
      if LMR_Flag = true then
        begin
        LMR := max((GetHighBit(k*3) * GetHighBit(depth + 3)) shr 2, 4) - ord(improving);

        if moves[k] >= $1000000000000000 then     // a quiet move,  LMR_Flag = true only for moves that satisfy < $4000000000000000
          begin

          if QuietHistory.Table[Board.ToPlay, Moves[k].Piece, Moves[k].Dest] > LMR_QuietHistoryMargin then
            LMR := LMR - 1;

          if (alpha < TB_WinCutoff) and (depth < 8) then
            begin

            // late move pruning : prune late quiet moves with bad history (but don't prune any bad captures)
            if (QuietsTried > (FullDepth * FullDepth) div 8 + 1) and not IsPV and (moves[k] < $2000000000000000) then     // prune
              continue;

            // late move pruning : prune late quiet moves that lose too much material
            TempBoard_2 := CopyBoard_asm(TempBoard);
            SEE_Value := TempBoard_2.SEE(Move);

            if SEE_Value > LMP_SEEMargin * depth then
              continue;
            end;
          end;

        if NodeType = PV_Node then
          LMR := LMR div 2;

        newdepth := max(1, newdepth - LMR);
        end;

      if (moves[k] < $4000000000000000) and (moves[k] >= $1000000000000000)  then  // quiet move
        inc(QuietsTried);          // inc quiet moves

      if (ThreadCount > 1) and (MoveHash <> 0) then
        ABDADA_Table.MoveSearchBegin(MoveHash);

      Update_Accumulator(Accumulator, Updated_Accumulator, Move, TempBoard);
      value := -PVS_ABDADA(ThreadData, ply+1, newDepth, -alpha-1, -alpha, NewNodeType, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, ChildInCheck);

      if (value > alpha) and (LMR > 0) then
        value := -PVS_ABDADA(ThreadData, ply+1, FullDepth, -alpha-1, -alpha, NewNodeType, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, ChildInCheck);

      if IsPV and (value <> -InvalidScore) then
        begin

        if (alpha < value) and (value < beta)  then
          value := -PVS_ABDADA(ThreadData, ply+1, FullDepth, -beta, -alpha, PV_Node, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, ChildInCheck);
        end;

      if (ThreadCount > 1) and (MoveHash <> 0) then
        ABDADA_Table.MoveSearchOver(MoveHash);
      end;

    if value <> -InvalidScore then
      begin
      if value > Alpha then
        Alpha := value;

      if value > Best  then
        begin
        best := value;
        BestMove := (Move and $0000000000FFFFFF);
        BestMove.SetScore(value);

        if IsPV then
          begin
          PV[1] := BestMove;
          for i := 1 to PV1[0] do
            PV[i+1] := PV1[i];
          PV[0] := PV1[0] + 1;
          end;

        if (best >= Beta) then
          begin
          if (BestMove.CapturedPiece = 0) and (exclMove = 0) then
            begin
            QuietHistory.Update(Board.ToPlay, BestMove, depth*depth);
            ContHistory.Update(Board.ToPlay, PrevMove, BestMove, depth);

            for i := 1 to k-1 do
              if Moves[i].CapturedPiece = 0 then
                begin
                QuietHistory.Update(Board.ToPlay, Moves[i], -depth);
                ContHistory.Update(Board.ToPlay, PrevMove, Moves[i], -depth);
                end;

            if (BestMove and UInt64($FFFFFF)) <> (ThreadData.KillerMoveA[ply] and UInt64($FFFFFF)) then
              begin
              ThreadData.KillerMoveB[ply] := ThreadData.KillerMoveA[ply];
              ThreadData.KillerMoveA[ply] := BestMove;
              end;

            if Board.ToPlay = white then
              WhiteCounterMove.Update(PrevMove, BestMove)
             else
              BlackCounterMove.Update(PrevMove, BestMove);
            end;

          Break;
          end;
        end;
      end;

    if Stop then
      exit(InvalidScore);
    end;

  if IsPV and (BestMove <> 0) then
    begin
    Best := min(Best, TB_Score);         // ensure Best is not above TB score
    BestMove.SetScore(Best);
    end;

  if (Stop = false) and (exclMove = 0) and (Best <> InvalidScore) then
    begin
    TempMove := BestMove;

    if Best <= alphaOrig then
      begin
      TTFlag := ftUpperBound;       // All_Node
      TempMove := InvalidMove;      // No idea about which move is best since all
                                    // failed low, so store best move of 'none'.
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

    TransTable.StoreData(Board, TempMove, Best, min(Depth, 63), TTFlag, Ply);
    end;

  result := Best;
  end;


function TSearch.PVS_ABDADA_Root(var Moves : TMoveArray; ThreadData : T_ThreadDataPtr; depth, alpha, beta : Integer;
               var Board : TBoard; PV_array : T_PV_array) : integer;
  var
    i, j, k, f, m : integer;
    value, TTFlag, alphaOrig, StaticEval, Best : integer;
    Move, BestMove, TempMove, TTMove : TMove;
    Movecount : integer;
    TT_Data, MoveHash : UInt64;
    DataExists, InCheck : boolean;
    PV1 : TPVArray;
    PlayedCount  : integer;

    Alignable_Accumulator_1, Alignable_Accumulator_2 : TAlignableAccumulator;
    Accumulator, Updated_Accumulator : PAccumulator;
    TempBoard : TBoard;

  begin
  for i := low(PV_array) to high(PV_array) do
    PV_array[i, 0] := 0;

  ThreadData.SearchingPV := true;

  if Stop then
    exit(InvalidScore);

  // Mate distance pruning

  alpha := max(alpha, ScoreMinValue);
  beta := min(beta, ScoreMaxValue - 1);

  InCheck := Board.KingInCheck(Board.ToPlay);

  // do depth extension before probing TT to ensure consistency when setting alpha & avoid bug in search
  if InCheck and (Depth < 2) then
    depth := depth + 1;


  // Check transposition table

  DataExists := TransTable.RetrieveData(Board.Hash, TT_Data);

  Best := InvalidScore;
  BestMove := 0;
  alphaOrig := alpha;

  TTMove := 0;

  PlayedCount := ThreadData.HashCount;

  // another thread may have searched this position already, even at root

  if DataExists = true then
    TTMove := TransTable.Move(TT_Data);

  Accumulator := PAccumulator(((NativeUInt(@Alignable_Accumulator_1) + 63) and not 63));
  Updated_Accumulator := PAccumulator(((NativeUInt(@Alignable_Accumulator_2) + 63) and not 63));

  Refresh_Accumulator(Accumulator, Board);

  StaticEval := InvalidScore;

  if not InCheck then
    StaticEval := ScoreFromAccumulator(Accumulator, Board);

  ThreadData.EvalHist[0] := StaticEval;
  movecount := Moves[0];

  RateMoves(Board, 0, Moves);
  if ThreadData.SearchingPV and (ThreadData.RootPV[0] > 0) then               // prioritise PV moves (important for correct funtioning of firstrootmovesearched)
    begin
    ThreadData.SearchingPV := false;
    for i := 1 to Movecount do
      if (Moves[i] and $FFFFFF) = (ThreadData.RootPV[1] and $FFFFFF) then
        begin
        Moves[i] := Moves[i] or UInt64($8800000000000000);
        ThreadData.SearchingPV := true;
        break;
        end;
    end;

  if DataExists and (TTMove <> 0) then                                          // identify hash move also as priority to search
    begin
    for i := 1 to Movecount do
      if (Moves[i] and $FFFFFF) = (TTMove and $FFFFFF) then
        begin
        Moves[i] := Moves[i] or UInt64($8000000000000000);
        break;
        end;
    end;

  SortMoves(Moves);

  // Search

  MoveHash := 0;

  ThreadData.FirstRootMoveSearched := false;

  for k := 1 to movecount do
    begin
    PV1[0] := 0;    // important - must be inside move loop to prevent incorrect PV

    if (ThreadCount > 1) and (depth > 2) and (k > 1) then                       // no need to do this if it is first move in list
      begin
      f := 0;
      moveHash := (Moves[k] and $FFFFFF) xor Board.Hash xor UInt64(depth * $4AF89F927AD5);

      while (f+k < moveCount) and ABDADA_Table.SearchInProgress(movehash) do    // defer search of this move as another thread is searching it
        begin                                                                   // swap with next move in list
        inc(f);
        moveHash := (Moves[k+f] and $FFFFFF) xor Board.Hash xor UInt64(depth * $4AF89F927AD5);
        tempMove := Moves[k];
        moves[k] := moves[k+f];
        moves[k+f] := tempMove;
        end;
      end;

    move := Moves[k];
    TempBoard := CopyBoard_asm(Board);
    TempBoard.MakeMove(Move);

    Update_Accumulator(Accumulator, Updated_Accumulator, Move, TempBoard);

    Inc(ThreadData.NodeCount);
    ThreadData.RepHashList[PlayedCount + 1] := TempBoard.Hash;

    if k = 1 then
      value := -PVS_ABDADA(ThreadData, 1, depth-1, -beta, -alpha, PV_Node, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, -1)
     else
      begin
      if (ThreadCount > 1) and (MoveHash <> 0) then
        ABDADA_Table.MoveSearchBegin(MoveHash);

      value := -PVS_ABDADA(ThreadData, 1, depth-1, -alpha-1, -alpha, Cut_Node, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, -1);

      if value <> -InvalidScore then
        begin
        if value > alpha then
          value := -PVS_ABDADA(ThreadData, 1, depth-1, -beta, -alpha, PV_Node, TempBoard, PV1, Updated_Accumulator, DoNull, Move, 0, -1);
        end;

      if (ThreadCount > 1) and (MoveHash <> 0) then
        ABDADA_Table.MoveSearchOver(MoveHash);
      end;

    if (k = 1) and (value <> -InvalidScore) then
      ThreadData.FirstRootMoveSearched := true;         // flag that first move in root has been fully searched

    if value <> -InvalidScore then
      begin
      for i := low(PV_array) to high(PV_array) do
        begin
        if PV_array[i, 0] = 0 then                      // vacant, so insert PV
          begin
          Move.SetScore(value);
          PV_array[i, 1] := Move;
          for j := 1 to PV1[0] do
            PV_array[i, j+1] := PV1[j];
          PV_array[i, 0] := PV1[0] + 1;
          break;
          end
         else if value > PV_array[i, 1].score then
          begin
          if i < high(PV_array) then
            for j := high(PV_array) downto i+1 do       // shuffle other PVs down
              begin
              for m := 0 to PV_array[j-1, 0] do
                PV_array[j, m] := PV_array[j-1, m];
              end;

          Move.SetScore(value);
          PV_array[i, 1] := Move;                       // insert PV
          for j := 1 to PV1[0] do
            PV_array[i, j+1] := PV1[j];
          PV_array[i, 0] := PV1[0] + 1;
          break;
          end;
        end;

      if PV_array[0, 0] > 0 then
        Best := max(Best, PV_array[0, 1].score);

      if PV_array[High(PV_array), 0] > 0 then
        Alpha := max(alpha, PV_array[High(PV_array), 1].score);

      if (Best >= Beta) and (High(PV_array) = 0) then
        Break;
      end;
    end;

  if Stop then
    begin
    if ThreadData.FirstRootMoveSearched then
      exit(Best)
     else
      exit(InvalidScore);
    end;

  if PV_array[0, 0] > 0 then
    BestMove := PV_array[0, 1];

  if (Stop = false) and (Best <> InvalidScore) then
    begin
    TempMove := BestMove;

    if Best >= beta then
      begin
      TTFlag := ftLowerBound;          // fail high: keep the refutation for the re-search
      TransTable.StoreData(Board, TempMove, Best, min(Depth, 63), TTFlag, 0);
      end
     else if alphaOrig < Best then
      begin
      TTFlag := ftExact;             // PV_Node
      TransTable.StoreData(Board, TempMove, Best, min(Depth, 63), TTFlag, 0);
      end;
    end;

  result := Best;
  end;


procedure TSearch.ShutDown;
  var
    WaitMs : integer;

  begin
  WaitMs := 0;

  if Searching = true then
    begin
    Stop := true;
    ShutDownUnderway := true;

    while (searching = true) and (WaitMs < 2000) do   // loop while search is being halted
      begin
      sleep(20);
      inc(WaitMs, 20);
      end;
    end;
  end;


procedure TSearch.StopSearch;
  begin
  Stop := true;
  end;


procedure TSearch.ConcludeSearch;
  begin
  if Assigned(FOnSearchProgressUpdate) then
    FOnSearchProgressUpdate(Self);

  ConcludeSearch(ResultMessage);
  end;


procedure TSearch.ConcludeSearch(const FinalMsg : string);

  begin
  IsPondering := false;
  Stop := false;
  ReadyToSearch := false;
  SearchMoveList[0] := 0;

  Searching := false;

  if (FinalMsg <> '')  and not SupressBestMove then
    if Assigned(FOnMessageOut) then
      FOnMessageOut(FinalMsg);

  if Assigned(fOnSearchFinished) then
    fOnSearchFinished(Self);

  PrepareForNextSearch;
  end;


function TSearch.ResultMessage : string;
  var
    InfoPrefix : string;

  begin
  result := '';

  if DoProgressUpdates then
    begin
    if Search_Result.PV[0] >= 1 then
     InfoPrefix := ResultToStr + #13#10
    else if Search_Result.score = 0 then
     InfoPrefix := 'info string stalemate' + #13#10
    else
     InfoPrefix := 'info string checkmate' + #13#10;
    end
   else
    InfoPrefix := '';

  if IsPondering then                            // if stop command received when pondering, only return best move
    begin
    if Search_Result.PV[0] >= 1 then
      result := 'bestmove ' + Search_Result.PV[1].ToStr
     else if Search_Result.PV[0] = 0 then
      result := InfoPrefix + 'bestmove 0000';
    end
   else
    begin
    if Search_Result.PV[0] > 1 then
      result := InfoPrefix + 'bestmove ' + Search_Result.PV[1].ToStr + ' ponder ' + Search_Result.PV[2].ToStr
     else if Search_Result.PV[0] = 1 then
      result := InfoPrefix + 'bestmove ' + Search_Result.PV[1].ToStr
     else if Search_Result.PV[0] = 0 then
      result := InfoPrefix + 'bestmove 0000';
    end;
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


function TSearch.ResultToStr : AnsiString;
  var
    nps : double;

  begin
  if Search_Result.time > 0 then
    nps := Search_Result.nodes * 1000 / Search_Result.time
   else
    nps := 0.0;

  if Search_Result.score >= MateScoreCutoff then
    result := 'info depth ' + IntToStr(Search_Result.Depth) + ' seldepth ' + IntToStr(Search_Result.selDepth) + ' score mate ' + IntToStr( (ScoreMaxValue - Search_Result.score + 1) div 2)   + ' nodes ' + FormatFloat('0', Search_Result.nodes) +
                           ' nps ' + FormatFloat('0', nps) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV, Search_Result.Depth)

  else if Search_Result.score <= -MateScoreCutoff then
    result := 'info depth ' + IntToStr(Search_Result.Depth) + ' seldepth ' + IntToStr(Search_Result.selDepth) + ' score mate -' + IntToStr( (ScoreMaxValue + Search_Result.score + 1) div 2)  + ' nodes ' + FormatFloat('0', Search_Result.nodes) +
                           ' nps ' + FormatFloat('0', nps) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV, Search_Result.Depth)

  else
    result := 'info depth ' + IntToStr(Search_Result.Depth) + ' seldepth ' + IntToStr(Search_Result.selDepth) + ' score cp ' + IntToStr(Search_Result.score)  + ' nodes ' + FormatFloat('0', Search_Result.nodes) +
                           ' nps ' + FormatFloat('0', nps) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV, Search_Result.Depth);
  end;


procedure TSearch.NewGame;
  var
    WaitMs : integer;

  begin
  WaitMs := 0;

  if Searching then
    begin
    Stop := true;               // signal the background search to bail
    while Searching and (WaitMs < 2000) do           // wait for the workers + driver to unwind
      begin
      sleep(5);
      inc(WaitMs, 5);
      end;
    end;

  ClearTranspositionTable;

  if syzygy.TB_Initialized then
    syzygy.TB_release;          // TB_release() will release any mmap()ed tablebase files, reducing memory
                                // usage. This called between games, for example.
  TickCarryover := 0;
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
  UseSoftTimeLimit := false;
  NodeLimit := NodeLimit_Max;
  SearchMoveList[0] := 0;
  IsPondering := false;

  RepStartBoard.Reset;

  ThreadCount := min(6, TThread.ProcessorCount);

  DoAspirationSearch := true;
  DoProgressUpdates := true;
  SupressBestMove := false;
  UseOwnBook := false;

  Use_TB := false;
  TB_FilesPath := '';
  Syzygy.TB_Initialized := false;
  TB_MaxPieceCount := -1;

  ShowWDL := false;
  MultiPVCount := 1;
  WillPonder := false;
  end;


procedure TSearch.ClearHistoryTables;
  begin
  QuietHistory.ClearTable;
  ContHistory.ClearTable;
  end;


procedure TSearch.ClearCounterMoveTables;
  begin
  WhiteCounterMove.ClearTable;
  BlackCounterMove.ClearTable;
  end;


procedure TSearch.ClearTranspositionTable;
  begin
  if TransTable <> nil then
    TransTable.ClearTable;
  end;


procedure TSearch.SetThreadCount(Value : integer);
  begin
  ThreadCount := min(max(1, value), TThread.ProcessorCount);
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


function TSearch.TicksToMilliseconds(Ticks : Int64) : UInt64;
  begin
  result := (Ticks * 1000) div TickFrequency;
  end;


function TSearch.MillisecondsToTicks(Milliseconds : UInt64) : Int64;
  begin
  result := Milliseconds * UInt64(TickFrequency) div 1000;
  end;


function TSearch.BudgetToTicks(Budget : UInt64) : Int64;
  var
    ThreadOverhead : UInt64;

  begin
  result := 0;

  ThreadOverhead := max(ThreadCount - 1, 0) * 2;

  if Budget > ThreadOverhead then
    result := Int64((Budget - ThreadOverhead) * UInt64(TickFrequency) div 1000);
  end;


procedure TSearch.AllocateTimeForSearch(MovesToGo, wtime, winc, btime, binc : UInt64; const Board : TBoard);
  var
    RemainingMoves, Draft, Backload : double;
    PonderIncrease : UInt64;
    TimeRemaining, Increment, OpponentTimeRemaining, OpponentIncrement, MoveNumber : UInt64;
    CarryoverTime : Int64;

  begin
  MoveNumber := Board.TurnNumber;
  RemainingMoves := MAX((((((((6.3830E-12 * MoveNumber) -5.971148E-09) * MoveNumber + 2.167290653E-06) * MoveNumber) -3.866640315E-04) * MoveNumber + 3.456441656E-02) * MoveNumber -1.44359537) * MoveNumber + 51.856, 4);

  if Board.ToPlay = White then
    begin
    TimeRemaining := wtime;
    OpponentTimeRemaining := btime;
    Increment := winc;
    OpponentIncrement := binc;
    end
   else
    begin
    TimeRemaining := btime;
    OpponentTimeRemaining := wtime;
    Increment := binc;
    OpponentIncrement := winc;
    end;

  if TimeRemaining > Increment then
    TimeLimit := TimeRemaining - Increment    // always keep one increment as safety buffer
   else
    TimeLimit := TimeRemaining div 2;

  if MovesToGo = 1 then
    begin
    UseSoftTimeLimit := false;
    TimeLimit := max(TimeRemaining, 2);
    exit;
    end;

  PonderIncrease := 0;

  if WillPonder = true then
    begin
    if MovesToGo > 0 then
      PonderIncrease := round((OpponentTimeRemaining + MovesToGo * OpponentIncrement) / PonderFactor)
     else
      PonderIncrease := round((OpponentTimeRemaining + RemainingMoves * OpponentIncrement) / PonderFactor);
    end;

  CarryoverTime := TicksToMilliseconds(round(TickCarryover * CarryoverFactor));
  TickCarryover := 0;

  if MovesToGo > 0 then
    StartBudgetTime := (((MovesToGo - 1) * Increment + TimeRemaining + PonderIncrease) * 112) div (128 * (max(MovesToGo, 1)))
   else
    begin
    Draft := 0;
    if TimeRemaining + PonderIncrease >  CarryoverTime + Increment then
      Draft := TimeRemaining + PonderIncrease - CarryoverTime - Increment;

    BackLoad := min(BackLoadIncrement * MoveNumber + BackLoadStart, 2.0);
    Draft := Draft * BaseTimeFactor * BackLoad;

    StartBudgetTime := min( round(Draft / RemainingMoves) + Increment + CarryoverTime, TimeRemaining - Increment);
    end;

  if (StartBudgetTime > TimeRemaining) or (StartBudgetTime < 2) then
    StartBudgetTime := max(TimeRemaining, 2);                                // keep minimum of 2 ms
  end;


procedure TSearch.FullSearch(const Board : TBoard; const GameMoveList : TGameMoveList);
  begin
  Searching := true;
  ABDADA_Search(Board, GameMoveList);
  end;


procedure TSearch.StartInBackGround(const Board : TBoard; const GameMoveList : TGameMoveList);
  var
    Master : ITask;

  begin
  Searching := true;

  Master := TTask.Run(  procedure
                begin
                ABDADA_Search(Board, GameMoveList);
                end,

            ThreadPool);
  end;


procedure TSearch.ABDADA_Search(const Board : TBoard; const GameMoveList : TGameMoveList);
  var
    WaitMs : integer;

  begin
  WaitMs := 0;
  while (ReadyToSearch = false) and (WaitMs < 1000) do
    begin
    sleep(1);
    inc(WaitMs);
    end;

  if ThreadCount > 1 then
    ABDADA_Search_MultiThread(Board, GameMoveList)
   else
    ABDADA_Search_SingleThread(Board, GameMoveList);
  end;


function TSearch.BookMoveFound(const Board : TBoard): boolean;
  var
    BestScore : integer;
    Move : TMove;

  begin
  result := false;

  if T_OpeningBook.GetMove(Board.Hash, Move, ms_BestHist) = true then
    begin
    if Board.IsMoveValid(Move) = false then
      exit;

    if move.score <> ScoreMinValue then
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

    while IsPondering and not stop do      // wait until ponderhit or stop
      sleep(1);

    if DoProgressUpdates = true then
      if Assigned(FOnMessageOut) then
        FOnMessageOut('info string opening book hit');

    ConcludeSearch;

    exit(true);
    end;
  end;


function TB_NoSpareMaterial(const Board : TBoard) : boolean;
  // true when the side to move cannot give up any single unit and still win
  var
    TempBoard : TBoard;
    pegs, SourcePeg : UInt64;
    cell, WDL : integer;
    success : boolean;

  begin
  result := true;

  if Board.ToPlay = White then
    pegs := Board.WhitePegs and not Board.Kings
   else
    pegs := Board.BlackPegs and not Board.Kings;

  while pegs <> 0 do
    begin
    cell := PopLowBit(pegs);
    SourcePeg := UInt64($1) shl cell;

    TempBoard := CopyBoard_asm(Board);

    TempBoard.Queens  := TempBoard.Queens  and not SourcePeg;
    TempBoard.Rooks   := TempBoard.Rooks   and not SourcePeg;
    TempBoard.Bishops := TempBoard.Bishops and not SourcePeg;
    TempBoard.Knights := TempBoard.Knights and not SourcePeg;
    TempBoard.Pawns   := TempBoard.Pawns   and not SourcePeg;

    if TempBoard.ToPlay = White then
      TempBoard.WhitePegs := TempBoard.WhitePegs and not SourcePeg
     else
      TempBoard.BlackPegs := TempBoard.BlackPegs and not SourcePeg;

    if TempBoard.KingInCheck(1 - TempBoard.ToPlay) then     // removing this unit exposes the enemy king
      continue;

    success := false;
    WDL := Syzygy.TB_ProbeWDL(TempBoard, success);

    if success and (WDL >= 2) then        // still winning without this unit - it is spare
      exit(false);
    end;
  end;


procedure TSearch.Probe_TB(const Board : TBoard; var Moves : TMoveArray; var TB_ResultFound, TB_BestMoveFound: boolean; HashCount : integer; const RepHashList : TRepHashList);
  var
    score, cnt50 : integer;
    k, i, m, trigger : integer;
    Move : TMove;

    tempBoard : TBoard;

    WDL_result, DTZ_result : LongInt;
    TB_success, IsZeroingMove, InCheck, IsRepetition, UnscoredMove : boolean;

    key : UInt64;

  begin
  TB_ResultFound := false;
  TB_BestMoveFound := false;

  tempBoard := copyBoard_asm(Board);
  UnscoredMove := false;

  for k := 1 to Moves[0] do
    begin
    move := Moves[k];
    tempBoard.MakeMove(Move);

    IsZeroingMove := (Move.CapturedPiece <> 0) or (Move.Piece = pawn);

    key :=  KeyFromBoard(tempBoard);

    IsRepetition := false;
    m := max(HashCount + 1 - tempBoard.HalfCount, 0);
    i := HashCount + 1 - 4;

    trigger := 0;
    while i >= m do
      begin
      if tempBoard.Hash = RepHashList[i] then
        begin
        inc(trigger);
        if trigger >= 2 then
          begin
          isRepetition := true;
          break;
          end;
        end;

      i := i - 2;
      end;

    TB_success := false;

    if IsZeroingMove then
      begin
      WDL_result := -Syzygy.TB_ProbeWDL(tempBoard, TB_success);
      DTZ_result := WDLtoDTZ[WDL_result + 2];
      end
     else
      begin
      DTZ_result := -Syzygy.TB_ProbeDTZ(tempBoard, TB_success);
      if DTZ_result > 0 then
        inc(DTZ_result)
       else if DTZ_result < 0 then
        dec(DTZ_result)
       else
        DTZ_result := 0;
      end;

    if TB_success then
      if (DTZ_result = 3) or (DTZ_result = 2) then
        if tempBoard.MoveExists(InCheck) = false then
          if InCheck = true then
            DTZ_result := 1;

    cnt50 := tempBoard.HalfCount;

    tempBoard.UndoMove(Move);

    if TB_success = false then
      begin
      UnscoredMove := true;
      Moves[k].Setscore(TB_UnknownResult);
      continue;
      end;

    clearbit(Moves[k], 46);

    if DTZ_result > 0 then
      begin
      if (DTZ_result + cnt50 <= 99) and not IsRepetition then   // win
        begin
        if TB_NoSpareMaterial(Board) = true then
          score := TB_Win - DTZ_result
         else
          score := TB_Win;
        end
       else
        score := max(1, 200 - (DTZ_result + cnt50));            // cursed win
      end
     else if DTZ_result < 0 then
      begin
      if (-DTZ_result + cnt50 < 100) then
        score := -TB_Win - DTZ_result                           // loss
      else
        score := min(-1, -200 - DTZ_result + cnt50);            // blessed loss
      end
     else
      score := 0;                                               // draw

    if IsZeroingMove = false then
      for i := low(DTM_Key) to high(DTM_Key) do
        if key =  DTM_Key[i] then
          begin
          if  Score > TB_WinCutoff then
            Score := ScoreMaxValue - DTZ_result;      // mate in score

          if  Score < -TB_WinCutoff then
            Score := ScoreMinValue - DTZ_result;      // mated in score

          TB_BestMoveFound := true;
          break;
          end;

    Moves[k].Setscore(score);
    end;

  SortMoves(Moves);

  TB_ResultFound := (Moves[1].score > TB_WinCutoff) or (UnscoredMove = false);

  if TB_ResultFound then
    begin
    i := 1;           // exclude inferior moves that don't require to be searched
    while i < Moves[0] do
      begin
      if Moves[i+1].score < Moves[i].score then
        break;
      inc(i);
      end;
    Moves[0] := i;

    if (Moves[0] = 1) then
      TB_BestMoveFound := true;

    end;
  end;


procedure SortPVs(var PVs : T_PV_array);
  var
    i, j, k : integer;
    temp : TPVArray;

  begin
  // Insertion sort - optimized version

  for i := 1 to high(PVs) do
    begin
    for k := 0 to PVs[i, 0] do
      Temp[k] := PVs[i, k];

    j := i - 1;

    while (j >= 0) and (Pvs[j, 1].score < temp[1].score) do
      begin
      for k := 0 to PVs[j, 0] do
        PVs[j+1, k] :=  PVs[j, k];
      dec(j);
      end;

    for k := 0 to temp[0] do
      PVs[j+1, k] := temp[k];
    end;
  end;


procedure TSearch.ABDADA_Search_MultiThread(const Board : TBoard; const GameMoveList : TGameMoveList);
  var
    i, j : integer;
    alpha, beta : integer;
    ElapsedMilliseconds : Int64;
    BestDepth, BestScore, SelDepth, MaxDepth : integer;
    BestPV_array : T_PV_array;

    Lock : TCriticalSection;
    workerCount, ThreadsPerDepth, index : integer;
    workers: TArray<ITask>;
    ProgressMessage : AnsiString;
    TB_FilesPath : AnsiString;
    sd : array[0..MaxSearchPly] of TUpdateRec;
    DepthToken, HighestDepth : integer;
    ElapsedTicks : Int64;
    SupressProgressMessage, InCheck : boolean;

    RepHashList : TRepHashList;
    HashCount : integer;
    TempBoard : TBoard;
    TempMove : TMove;

    RootMoves : TMoveArray;
    ValidSearchMoves : TMoveArray;
    
    Movecount : integer;
    TB_ResultFound, TB_BestMoveFound : boolean;
    TB_Moves : TMoveArray;
    TempMultiPVCount : integer;

    StableCount, LastStabilityDepth : integer;
    PrevBestMove : TMove;
    AdjFactor : double;

  label
    finished;

  begin
  SupressProgressMessage := false;
  Searching := true;

  if TransTable <> nil then
    TransTable.NewSearch;        // increments search age, must do before openbook

  if UseOwnBook = true then
    if BookMoveFound(Board) then
      exit;

  movecount := Board.GetValidMoves(Board.ToPlay, RootMoves, All);

   // Check for immediate win or stalemate

  if movecount = 0 then
    begin
    InCheck := Board.KingInCheck(Board.ToPlay);

    Search_Result.Depth := 0;
    Search_Result.selDepth := 0;
    Search_Result.nodes := 0;
    Search_Result.time := 0;
    Search_Result.PV[0] := 0;
    Search_Result.score := ScoreMinValue;

    if InCheck = true then
      begin
      Search_Result.score := ScoreMinValue;    // checkmate
      ProgressMessage := 'info string checkmate';
      end
     else
      begin
      Search_Result.score := 0;               // stalemate
      ProgressMessage := 'info string stalemate';
      end;

    goto finished;
    end;

  // If SearchMoveList is not empty then limit search to the valid legal moves
  // contained in the List. If no valid legal moves in list then search all
  // valid moves as per normal

  ValidSearchMoves[0] := 0;

  if SearchMoveList[0] > 0 then
    begin
    for i := 1 to SearchMoveList[0] do
      begin
      for j := 1 to movecount do
        if (SearchMoveList[i] and $FFFFFF) = (RootMoves[j] and $FFFFFF) then
          begin
          inc(ValidSearchMoves[0]);
          ValidSearchMoves[ValidSearchMoves[0]] := RootMoves[j];
          break;
          end;
      end;
    end;

  if ValidSearchMoves[0] > 0 then
    begin
    TempMultiPVCount := min(MultiPVCount, ValidSearchMoves[0]);

    for i := 0 to ValidSearchMoves[0] do
      RootMoves[i] := ValidSearchMoves[i];
    end
   else
    begin
    TempMultiPVCount := min(MultiPVCount, RootMoves[0]);

    for i := 0 to RootMoves[0] do
      ValidSearchMoves[i] := RootMoves[i];
    end;

  setlength(BestPV_array, TempMultiPVCount);

  // Setup RepHashList, required for repetition check in TB

  HashCount := GameMoveList.Count;
  if GameMoveList.Count > 0 then
    begin
    TempBoard := copyBoard(RepStartBoard);

    RepHashList[0] := TempBoard.Hash;

    for i := 1 to HashCount do
      begin
      TempMove := GameMoveList.MoveArray[i];
      TempBoard.MakeMove(TempMove);
      RepHashList[i] := TempBoard.Hash;
      end;
    end;

  if HashCount = 0 then
    RepHashList[0] := Board.Hash;

  TB_resultFound := false;
  TB_BestMoveFound := false;
  TB_RootHit := false;

  if (Use_TB = true) and not Syzygy.TB_initialized then
    begin
    if TB_FilesPath <> '' then
      Syzygy.TB_init(PAnsiChar(TB_FilesPath));

    TB_MaxPieceCount := min(Syzygy.TB_MaxCardinality[WDL], 6);
    end;

  if (Use_TB = true) and (bitcount(Board.WhitePegs or Board.BlackPegs) <= Syzygy.TB_MaxCardinality[WDL]) then
    begin

    Probe_TB(Board, RootMoves, TB_ResultFound, TB_BestMoveFound, HashCount, RepHashList);
    TB_RootHit := TB_ResultFound;

    if TB_BestMoveFound and (MultiPVCount = 1) then     // play best move
      begin
      Search_Result.PV[0] := 1;
      Search_Result.PV[1] := RootMoves[1];

      Search_Result.Depth := 1;
      Search_Result.score := RootMoves[1].score;
      Search_Result.selDepth := 1;
      Search_Result.nodes := 1;
      Search_Result.time := 0;

      while IsPondering and not stop do      // wait until ponderhit or stop
        sleep(1);

      if DoProgressUpdates = true then
        if Assigned(FOnMessageOut) then
          FOnMessageOut('info string tablebase hit');

      ConcludeSearch;

      exit;
      end;
    end;

  //  1. Start with AllValidMoves
  //  2. if SearchMovesCount > 0 then filter AllValidMoves to include only ValidSearchMoves
  //  3. If TB found then
  //         if MultiPVCount > TB_filtered_moves_Count then
  //           search (ValidSearchMoves)
  //          else
  //           search (TB_filtered_moves)
  //  4. report min(searched moves, MultiPVCount)

  if TB_ResultFound then                              // keep track of TB_score
    begin
    for i := 0 to RootMoves[0] do
      begin
      TB_Moves[i] := RootMoves[i];
      RootMoves[i] := (RootMoves[i] and $0000FFFFFFFFFFFF);   // clear score
      end;
    end;

  if TB_ResultFound then
    if  TempMultiPVCount > RootMoves[0] then
      for i := 0 to ValidSearchMoves[0] do
        begin
        RootMoves[i] := ValidSearchMoves[i];
        RootMoves[i] := (RootMoves[i] and $0000FFFFFFFFFFFF);   // clear score
        end;

  for i := low(BestPV_array) to high(BestPV_array) do
    BestPV_array[i, 0] := 0;

  for i := low(sd) to high(sd) do
    begin
    sd[i].BestScore := InvalidScore;
    sd[i].BestMove := InvalidMove;
    sd[i].SelDepth := 0;
    sd[i].FirstRootMoveSearched := false;
    end;

  QueryPerformanceCounter(StartTick);

  if UseSoftTimeLimit then
    TickLimit := BudgetToTicks(min(TimeLimit, StartBudgetTime))
   else
    TickLimit := BudgetToTicks(TimeLimit);

  //TimeExtension := false;

  if ReadyToSearch = false then
    PrepareForNextSearch;              // clears all tables except Transposition Table

  alpha := -InfiniteValue;
  beta  :=  InfiniteValue;

  workercount := ThreadCount;

  //  Each depth only searched by a maximum of 'ThreadsPerDepth' threads i.e. workercount
  //  Starting with a depth of 2

  ThreadsPerDepth := workerCount * 2;

  DepthToken := 2 * ThreadsPerDepth;
  HighestDepth := 0;

  BestScore := InvalidScore;
  BestMove := InvalidMove;
  BestDepth := 0;
  selDepth := 0;

  StableCount := 0;
  LastStabilityDepth := 0;
  PrevBestMove := 0;

  ElapsedMilliseconds := 0;

  MaxDepth := min(DepthLimit, MaxSearchPly);

  Lock := TCriticalSection.Create;

    try
    index := -1;

    SetLength(workers, WorkerCount);
    for i := 0 to workerCount - 1 do
      workers[i] := TTask.Run(

      procedure
        var
          alpha1, beta1, SearchDepth1 : integer;
          i0, j0, k : integer;
          score1 : integer;

          Board1 : TBoard;
          PV1_array : T_PV_array;
          MultiPVstr1 : string;
          ThreadData1 : T_ThreadData;
          UpdateResult1 : boolean;
          Moves : TMoveArray;
          Projection : double;
          WorkerMsg  : AnsiString;
          EmitMsg    : boolean;

        begin
          try
          k := InterLockedIncrement(Index);

          score1 := InvalidScore;

          ThreadData1.Reset;
          ThreadData1.ID := k+1;

          Board1 := copyBoard_asm(Board);

          ThreadData1.HashCount := HashCount;
          for i0 := 0 to HashCount do
            ThreadData1.RepHashList[i0] := RepHashList[i0];

          ThreadData1.NullCount := 0;
          ThreadData1.RootWhiteMaterial := Board1.MaterialValue(white);
          ThreadData1.RootBlackMaterial := Board1.MaterialValue(black);

          setlength(PV1_array, TempMultiPVCount);

          DepthToken := InterLockedIncrement(DepthToken);
          SearchDepth1 := 1;

          while (Stop = false) and (HighestDepth < MaxDepth) do
            begin
            ThreadData1.InitialDepth := SearchDepth1;
            EmitMsg := false;

            Lock.Acquire;

            for i0 := 0 to RootMoves[0] do
              Moves[i0] := RootMoves[i0];

            Lock.Release;

            if DoAspirationSearch and (SearchDepth1 > AspirationMinDepth) and (MultiPVCount = 1) then
              begin
              Lock.Acquire;

              if BestScore <> InvalidScore then
                begin
                alpha1 := max(BestScore - AspirationMargin, -InfiniteValue);
                beta1 := min(BestScore + AspirationMargin, InfiniteValue);
                end
               else
                begin
                alpha1 := -InfiniteValue;
                beta1  :=  InfiniteValue;
                end;

              Lock.Release;

              ThreadData1.FirstRootMoveSearched := false;

              if not stop then
                score1 := PVS_ABDADA_Root(Moves, @ThreadData1, SearchDepth1, alpha1, beta1, Board1, PV1_array);

              if (score1 >= beta1) or (score1 <= alpha1) then
                begin
                ThreadData1.FirstRootMoveSearched := false;    //  outside aspiration window, or invalid score

                if score1 <> InvalidScore then
                  begin
                  if UseSoftTimeLimit then
                    begin
                    Lock.Acquire;
                      try
                      AdjBudgetTime := min(TimeLimit, UInt64(round(StartBudgetTime * AspirationFactor)));
                      TickLimit := BudgetToTicks(AdjBudgetTime);
                      finally
                      Lock.Release;
                      end;
                    end;

                  if score1 >= beta1 then     // re-search widened window
                    begin
                    alpha1 := max(score1 - AspirationMargin div 4, alpha);
                    beta1 := beta;
                    score1 := PVS_ABDADA_Root(Moves, @ThreadData1, SearchDepth1, alpha1, beta1, Board1, PV1_array);
                    end
                   else if score1 <= alpha1 then
                    begin
                    alpha1 := max(score1 - AspirationMargin, alpha);
                    beta1 := min(score1 + AspirationMargin div 4, beta);
                    score1 := PVS_ABDADA_Root(Moves, @ThreadData1, SearchDepth1, alpha1, beta1, Board1, PV1_array);
                    end;

                  if (score1 >= beta1) or (score1 <= alpha1) then
                    begin
                    ThreadData1.FirstRootMoveSearched := false;

                    if score1 <> InvalidScore then
                      score1 := PVS_ABDADA_Root(Moves, @ThreadData1, SearchDepth1, alpha, beta, Board1, PV1_array);
                    end;
                  end;
                end;
              end
             else
              begin
              ThreadData1.FirstRootMoveSearched := false;
              score1 := PVS_ABDADA_Root(Moves, @ThreadData1, SearchDepth1, alpha, beta, Board1, PV1_array);
              end;

            Lock.Acquire;
              try
              NodeCount :=  NodeCount + ThreadData1.NodeCount - ThreadData1.PrevCount;
              ThreadData1.PrevCount := ThreadData1.NodeCount;

              UpdateResult1 := false;

              if (score1 > sd[SearchDepth1].BestScore) and ThreadData1.FirstRootMoveSearched then
                begin
                sd[SearchDepth1].FirstRootMoveSearched := true;
                sd[SearchDepth1].BestScore := score1;
                sd[SearchDepth1].BestMove := PV1_array[0, 1];
                sd[SearchDepth1].SelDepth := ThreadData1.SelDepth;
                end;

              if sd[SearchDepth1].FirstRootMoveSearched then
                updateResult1 :=  ((SearchDepth1 = HighestDepth) and (sd[SearchDepth1].BestScore > BestScore)) or         // same depth, but better score so update best
                                  ((SearchDepth1 > HighestDepth) and (sd[SearchDepth1].BestScore < MateScoreCutoff)) or   // new highest depth but not a mate score, so update best only if best is not already a mate score
                                  ((sd[SearchDepth1].BestScore >= MateScoreCutoff) and (score1 > BestScore));             // matescore, better than any previous score, so must update best


              if not IsPondering and (sd[SearchDepth1].BestScore >= MateScoreCutoff) then
                if SearchDepth1 >= ScoreMaxValue - sd[SearchDepth1].BestScore then
                  begin
                  if stop = false then
                    UpdateResult1 := true;
                  Stop := true;
                  end;

              if not IsPondering and (sd[SearchDepth1].BestScore <= -MateScoreCutoff) then
                if SearchDepth1 >= ScoreMaxValue + sd[SearchDepth1].BestScore then
                  begin
                  if stop = false then
                    UpdateResult1 := true;
                  Stop := true;
                  end;

              if UpdateResult1 = true then
                begin
                for i0 := low(PV1_array) to high(PV1_array) do
                  if PV1_array[i0, 0] > 0 then
                    begin
                    move(PV1_array[i0, 0], BestPV_array[i0, 0], length(PV1_array[i0]) * sizeof(TMove));

                    if BestPV_array[i0, 1].score > MateScoreCutoff then
                      BestPV_array[i0, 0] := min(BestPV_array[i0, 0], ScoreMaxValue - BestPV_array[i0, 1].score);

                    if BestPV_array[i0, 1].score < -MateScoreCutoff then
                      BestPV_array[i0, 0] := min(BestPV_array[i0, 0], ScoreMaxValue + BestPV_array[i0, 1].score);
                    end
                   else if i0 > low(PV1_array) then
                    BestPV_array[i0, 0] := 0;

                BestScore := score1;
                BestMove := BestPV_array[0, 1];
                SelDepth := ThreadData1.SelDepth;
                BestDepth := SearchDepth1;

                if TB_ResultFound then                              // ensure PV score reflects TB result
                  begin
                  for i0 := low(BestPV_array) to high(BestPV_array) do
                    if BestPV_array[i0, 0] > 0 then
                      for j0 := 1 to TB_Moves[0] do
                        begin
                        if (TB_Moves[j0] and $FFFFFF) = (BestPV_array[i0, 1] and $FFFFFF) then
                          begin
                          if abs(BestPV_array[i0, 1].score) < MateScoreCutoff then
                            begin
                            if TB_Moves[j0].score > TB_WinCutoff then
                              BestPV_array[i0, 1].setscore(BestPV_array[i0, 1].score + 6000)
                             else if TB_Moves[j0].score < -TB_WinCutoff then
                              BestPV_array[i0, 1].setscore(BestPV_array[i0, 1].score - 6000);
                            end;
                          break;
                          end;
                        end;
                  end;

                if (DoProgressUpdates = true) and not (stop or SupressProgressMessage) then

                // supresss progress message when looping after DepthLimit reached when pondering

                  begin
                  ElapsedMilliseconds := GetElapsedMilliseconds;

                  ProgressMessage := '';
                  for i0 := low(BestPV_array) to high(BestPV_array) do
                    if BestPV_array[i0, 0] > 0 then
                      begin
                      if i0 <> low(BestPV_array) then
                        ProgressMessage := ProgressMessage + #13#10;

                      MultiPVstr1 := '';
                      if high(BestPV_array) > 0 then
                        MultiPVstr1 :=  ' multipv ' + IntToStr(i0+1);

                      ProgressMessage := ProgressMessage + 'info depth ' + IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + MultiPVstr1;

                      if BestPV_array[i0, 1].score >= MateScoreCutoff then
                        ProgressMessage := ProgressMessage + ' score mate ' + IntToStr( (ScoreMaxValue - BestPV_array[i0, 1].score + 1) div 2)
                       else if BestPV_array[i0, 1].score <= -MateScoreCutoff then
                        ProgressMessage := ProgressMessage + ' score mate -' + IntToStr( (ScoreMaxValue + BestPV_array[i0, 1].score + 1) div 2)
                       else
                        ProgressMessage := ProgressMessage + ' score cp ' + IntToStr(BestPV_array[i0, 1].score);

                      ProgressMessage := ProgressMessage + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV_array[i0], BestDepth) + ' string : thread #' + IntToStr(k+1);
                      end;

                  WorkerMsg := ProgressMessage;
                  EmitMsg := true;
                  end;
                end;

              HighestDepth := max(HighestDepth, SearchDepth1);

              if UseSoftTimeLimit and (HighestDepth > LastStabilityDepth) and sd[HighestDepth].FirstRootMoveSearched then
                begin
                LastStabilityDepth := HighestDepth;

                if (PrevBestMove <> 0) and ((sd[HighestDepth].BestMove and $FFFFFF) = (PrevBestMove and $FFFFFF)) then
                  StableCount := min(StableCount + 1, HighestDepth)
                 else
                  StableCount := 0;

                PrevBestMove := sd[HighestDepth].BestMove;

                if HighestDepth >= 6 then
                  AdjFactor := StabilityStart - StabilityDecay * ln(StableCount + 1)
                 else
                  AdjFactor := 1.0;

                AdjBudgetTime := min(TimeLimit, UInt64(round(StartBudgetTime * AdjFactor)));

                TickLimit := BudgetToTicks(AdjBudgetTime);
                end;

              if DepthToken < (HighestDepth + 1) * ThreadsPerDepth  then
                DepthToken := (HighestDepth + 1) * ThreadsPerDepth;

              DepthToken := InterLockedIncrement(DepthToken);
              SearchDepth1 := min(DepthToken div ThreadsPerDepth, MaxDepth);

              if BestDepth = MaxDepth then
                SupressProgressMessage := true;

              if IsPondering = false then      //  don't stop for time if pondering
                begin
                ElapsedTicks := GetElapsedTicks;
                if ElapsedTicks > TickLimit  then
                  Stop := true;

                if UseSoftTimeLimit and (ElapsedTicks > 0) and (HighestDepth > 0) then
                  begin
                  Projection := power(2, 1.25 * Log2(ElapsedTicks * 1000.0 / TickFrequency) / HighestDepth);

                  if ElapsedTicks * Projection > TickLimit then
                    Stop := true;
                  end;
                end;

              for i0 := 0 to BestPV_array[0, 0] do
                ThreadData1.RootPV[i0] := BestPV_array[0, i0];

              finally
              Lock.Release;
              end;

            if EmitMsg and Assigned(FOnMessageOut) then
              FOnMessageOut(WorkerMsg);
            end;
          except
            stop := true;
          end;
        end,

        ThreadPool);
      try
      if workercount > 0 then
        TTask.WaitForAny(workers);
      except        // insurance: workers already swallow their own exceptions
      end;

    Stop := true;                      // signal other workers to stop

      try
      if workercount > 1 then
        TTask.WaitForAll(workers);     //  Must wait for all remaining workers to stop
      except
      end;

    finally
    Lock.Free;
    end;

  SearchTime_ms := GetElapsedMilliseconds;

  if UseSoftTimeLimit and not IsPondering then
    TickCarryover := max(MillisecondsToTicks(StartBudgetTime) - GetElapsedTicks, 0)
   else
    TickCarryover := 0;

  if BestPV_array[0, 0] > 0 then
    begin
    Search_Result.Depth := BestDepth;
    Search_Result.selDepth := selDepth;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    for i := 0 to BestPV_array[0, 0] do
      Search_Result.PV[i] := BestPV_array[0, i];
    Search_Result.score := BestPV_array[0, 1].score;
    end
   else if RootMoves[0] > 0 then
    begin                          // search aborted before depth 1 - play first legal move
    Search_Result.Depth := 0;
    Search_Result.selDepth := 0;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    Search_Result.PV[0] := 1;
    Search_Result.PV[1] := RootMoves[1];
    Search_Result.score := 0;
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

  finished:

  ConcludeSearch;

  end;


procedure TSearch.ABDADA_Search_SingleThread(const Board : TBoard; const GameMoveList : TGameMoveList);
  var
    i, j : integer;
    alpha, beta : integer;
    ElapsedMilliseconds : Int64;
    BestDepth, BestScore, SelDepth : integer;
    PV_array : T_PV_array;
    BestPV_array : T_PV_array;

    ProgressMessage : AnsiString;
    MultiPVstr : string;
    TB_FilesPath : AnsiString;

    ElapsedTicks : Int64;
    Projection : double;

    alpha1, beta1, SearchDepth1, MaxDepth : integer;
    score1 : integer;

    Board1 : TBoard;
    ThreadData1 : T_ThreadData;

    SupressProgressMessage : boolean;

    RepHashList : TRepHashList;
    HashCount : integer;
    TempBoard : TBoard;
    TempMove : TMove;
    PrevBestMove : TMove;

    RootMoves : TMoveArray;
    ValidSearchMoves : TMoveArray;

    Movecount : integer;
    TB_ResultFound, TB_BestMoveFound, InCheck : boolean;
    TB_Moves : TMoveArray;
    TempMultiPVCount : integer;

    StableCount : integer;
    AdjFactor : double;

  label
    finished;

  begin
  SupressProgressMessage := false;
  Searching := true;

  if TransTable <> nil then
    TransTable.NewSearch;        // increments search age, must do before openbook

  if UseOwnBook = true then
    if BookMoveFound(Board) then
      exit;

  movecount := Board.GetValidMoves(Board.ToPlay, RootMoves, All);

  if movecount = 0 then
    begin
    InCheck := Board.KingInCheck(Board.ToPlay);

    Search_Result.Depth := 0;
    Search_Result.selDepth := 0;
    Search_Result.nodes := 0;
    Search_Result.time := 0;
    Search_Result.PV[0] := 0;
    Search_Result.score := ScoreMinValue;

    if InCheck = true then
      begin
      Search_Result.score := ScoreMinValue;    // checkmate
      ProgressMessage := 'Checkmate !';
      end
     else
      begin
      Search_Result.score := 0;               // stalemate
      ProgressMessage := 'Stalemate !';
      end;

    goto finished;
    end;

  // If SearchMoveList is not empty then limit search to the valid legal moves
  // contained in the List. If no valid legal moves then search all
  // valid moves as per normal

  ValidSearchMoves[0] := 0;

  if SearchMoveList[0] > 0 then
    begin
    for i := 1 to SearchMoveList[0] do
      begin
      for j := 1 to movecount do
        if (SearchMoveList[i] and $FFFFFF) = (RootMoves[j] and $FFFFFF) then
          begin
          inc(ValidSearchMoves[0]);
          ValidSearchMoves[ValidSearchMoves[0]] := RootMoves[j];
          break;
          end;
      end;
    end;

  if ValidSearchMoves[0] > 0 then
    begin
    TempMultiPVCount := min(MultiPVCount, ValidSearchMoves[0]);

    for i := 0 to ValidSearchMoves[0] do
      RootMoves[i] := ValidSearchMoves[i];
    end
   else
    begin
    TempMultiPVCount := min(MultiPVCount, RootMoves[0]);

    for i := 0 to RootMoves[0] do
      ValidSearchMoves[i] := RootMoves[i];
    end;

  setlength(PV_array, TempMultiPVCount);
  setlength(BestPV_array, TempMultiPVCount);

  // Setup RepHashList, required for repetition check in TB

  HashCount := GameMoveList.Count;
  if GameMoveList.Count > 0 then
    begin
    TempBoard := copyBoard(RepStartBoard);

    RepHashList[0] := TempBoard.Hash;

    for i := 1 to HashCount do
      begin
      TempMove := GameMoveList.MoveArray[i];
      TempBoard.MakeMove(TempMove);
      RepHashList[i] := TempBoard.Hash;
      end;
    end;

  if HashCount = 0 then
    RepHashList[0] := Board.Hash;

  TB_ResultFound := false;
  TB_BestMoveFound := false;
  TB_RootHit := false;

  if (Use_TB = true) and not Syzygy.TB_initialized then
    begin
    if TB_FilesPath <> '' then
      Syzygy.TB_init(PAnsiChar(TB_FilesPath));

    TB_MaxPieceCount := min(Syzygy.TB_MaxCardinality[WDL], 6);
    end;

  if (Use_TB = true) and (bitcount(Board.WhitePegs or Board.BlackPegs) <= Syzygy.TB_MaxCardinality[WDL]) then
    begin
    Probe_TB(Board, RootMoves, TB_ResultFound, TB_BestMoveFound, HashCount, RepHashList);
    TB_RootHit := TB_ResultFound;

    if TB_BestMoveFound and (MultiPVCount = 1) then     // play best move
      begin
      Search_Result.PV[0] := 1;
      Search_Result.PV[1] := RootMoves[1];

      Search_Result.Depth := 1;
      Search_Result.score := RootMoves[1].score;
      Search_Result.selDepth := 1;
      Search_Result.nodes := 1;
      Search_Result.time := 0;

      while IsPondering and not stop do      // wait until ponderhit or stop
        sleep(1);

      if DoProgressUpdates = true then
        if Assigned(FOnMessageOut) then
          FOnMessageOut('info string tablebase hit');

      ConcludeSearch;

      exit;
      end;
    end;

  //  1. Start with AllValidMoves
  //  2. if SearchMovesCount > 0 then filter AllValidMoves to include only ValidSearchMoves
  //  3. If TB found then
  //         if MultiPVCount > TB_filtered_moves_Count then
  //           search (ValidSearchMoves)
  //          else
  //           search (TB_filtered_moves)
  //  4. report min(searched moves, MultiPVCount)

  if TB_ResultFound then                              // keep track of TB_score
    begin
    for i := 0 to RootMoves[0] do
      begin
      TB_Moves[i] := RootMoves[i];
      RootMoves[i] := (RootMoves[i] and $0000FFFFFFFFFFFF);   // clear score
      end;
    end;

  if TB_ResultFound then
    if  TempMultiPVCount > RootMoves[0] then
      for i := 0 to ValidSearchMoves[0] do
        begin
        RootMoves[i] := ValidSearchMoves[i];
        RootMoves[i] := (RootMoves[i] and $0000FFFFFFFFFFFF);   // clear score
        end;

  for i := low(BestPV_array) to high(BestPV_array) do
    BestPV_array[i, 0] := 0;

  QueryPerformanceCounter(StartTick);

  if UseSoftTimeLimit then
    TickLimit := MillisecondsToTicks(StartBudgetTime)
   else
    TickLimit := MillisecondsToTicks(TimeLimit);

  if ReadyToSearch = false then
    PrepareForNextSearch;              // clears all tables except Transposition Table

  alpha := -InfiniteValue;
  beta  :=  InfiniteValue;

  BestScore := InvalidScore;
  BestMove := InvalidMove;
  BestDepth := 0;
  selDepth := 0;

  StableCount := 0;
  PrevBestMove := 0;
  score1 := InvalidScore;

  ThreadData1.Reset;
  ThreadData1.ID := 1;

  Board1 := copyBoard_asm(Board);

  ThreadData1.HashCount := HashCount;          // required for repetition check
  for i := 0 to HashCount do
    ThreadData1.RepHashList[i] := RepHashList[i];

  ThreadData1.NullCount := 0;
  ThreadData1.RootWhiteMaterial := Board1.MaterialValue(white);
  ThreadData1.RootBlackMaterial := Board1.MaterialValue(black);

  SearchDepth1 := 1;

  MaxDepth := min(DepthLimit, MaxSearchPly);

    try
    while (Stop = false) and (SearchDepth1 <= MaxDepth) do
      begin
      ThreadData1.InitialDepth := SearchDepth1;

      if DoAspirationSearch and (SearchDepth1 > AspirationMinDepth) and (MultiPVCount = 1) then
        begin

        if BestScore <> InvalidScore then
          begin
          alpha1 := max(BestScore - AspirationMargin, -InfiniteValue);
          beta1 := min(BestScore + AspirationMargin, InfiniteValue);
          end
         else
          begin
          alpha1 := -InfiniteValue;
          beta1 :=   InfiniteValue;
          end;

        ThreadData1.FirstRootMoveSearched := false;

        if not stop then
          score1 := PVS_ABDADA_Root(RootMoves, @ThreadData1, SearchDepth1, alpha1, beta1, Board1, PV_Array);

        if (score1 >= beta1) or (score1 <= alpha1) then
          begin
          ThreadData1.FirstRootMoveSearched := false;

          if score1 <> InvalidScore then
            begin
            if UseSoftTimeLimit then
              begin
              AdjBudgetTime :=  min(TimeLimit, UInt64(round(StartBudgetTime * AspirationFactor)));
              TickLimit := (AdjBudgetTime * TickFrequency) div 1000;
              end;

            if score1 >= beta1 then     // had cutoff, so re-search widened window
              begin
              alpha1 := max(score1 - AspirationMargin div 4, alpha);
              beta1 := beta;
              score1 := PVS_ABDADA_Root(RootMoves, @ThreadData1, SearchDepth1, alpha1, beta1, Board1, PV_Array);
              end
             else if score1 <= alpha1 then     // had cutoff, so re-search widened window
              begin
              alpha1 := max(score1 - AspirationMargin , alpha);
              beta1 := min(score1 + AspirationMargin div 4, beta);
              score1 := PVS_ABDADA_Root(RootMoves, @ThreadData1, SearchDepth1, alpha1, beta1, Board1, PV_Array);
              end;

            if (score1 >= beta1) or (score1 <= alpha1) then
              begin
              ThreadData1.FirstRootMoveSearched := false;      //   if outside aspiration window then set ThreadData1.FirstRootMoveSearched = false;

              if score1 <> InvalidScore then
                score1 := PVS_ABDADA_Root(RootMoves, @ThreadData1, SearchDepth1, alpha, beta, Board1, PV_Array);
              end;
            end;
          end;
        end
       else
        begin
        ThreadData1.FirstRootMoveSearched := false;
        score1 := PVS_ABDADA_Root(RootMoves, @ThreadData1, SearchDepth1, alpha, beta, Board1, PV_Array);
        end;

      NodeCount := NodeCount + ThreadData1.NodeCount - ThreadData1.PrevCount;
      ThreadData1.PrevCount := ThreadData1.NodeCount;

      if ThreadData1.FirstRootMoveSearched = true then
        begin
        for i := low(PV_array) to high(PV_array) do
          if PV_array[i, 0] > 0 then
            begin
            move(PV_array[i, 0], BestPV_array[i, 0], length(BestPV_array[i]) * sizeof(TMove));    // keep a copy of best PVs so far

            if BestPV_array[i, 1].score > MateScoreCutoff then
              BestPV_array[i, 0] := min(BestPV_array[i, 0], ScoreMaxValue - BestPV_array[i, 1].score);

            if BestPV_array[i, 1].score < -MateScoreCutoff then
              BestPV_array[i, 0] := min(BestPV_array[i, 0], ScoreMaxValue + BestPV_array[i, 1].score);
            end
           else if i > low(PV_array) then
            BestPV_array[i, 0] := 0;

        BestScore := score1;
        BestMove := BestPV_array[0, 1];
        SelDepth := ThreadData1.SelDepth;
        BestDepth := SearchDepth1;

        if TB_ResultFound then                              // ensure PV score reflects TB result
          begin
          for i := low(BestPV_array) to high(BestPV_array) do
            if BestPV_array[i, 0] > 0 then
              for j := 1 to TB_Moves[0] do
                begin
                if (TB_Moves[j] and $FFFFFF) = (BestPV_array[i, 1] and $FFFFFF) then
                  begin
                  if abs(BestPV_array[i, 1].score) < MateScoreCutoff then
                    begin
                    if TB_Moves[j].score > TB_WinCutoff then
                      BestPV_array[i, 1].setscore(BestPV_array[i, 1].score + 6000)
                     else if TB_Moves[j].score < -TB_WinCutoff then
                      BestPV_array[i, 1].setscore(BestPV_array[i, 1].score - 6000);
                    end;
                  break;
                  end;
                end;
          end;

        if UseSoftTimeLimit and ThreadData1.FirstRootMoveSearched then
          begin
          if (PrevBestMove <> 0) and ((BestMove and $FFFFFF) = (PrevBestMove and $FFFFFF)) then
            StableCount := min(StableCount + 1, SearchDepth1)
           else
            StableCount := 0;

          PrevBestMove := BestMove;

          if SearchDepth1 >= 6 then
            AdjFactor := StabilityStart - StabilityDecay * ln(StableCount + 1)
           else
            AdjFactor := 1.0;

          AdjBudgetTime := min(TimeLimit, UInt64(round(StartBudgetTime * AdjFactor)));
          TickLimit := (AdjBudgetTime * TickFrequency) div 1000;
          end;

        for i := 0 to BestPV_array[0, 0] do
          ThreadData1.RootPV[i] := BestPV_array[0, i];
        end;

      if not IsPondering and (BestScore >= MateScoreCutoff) then
        if SearchDepth1 >= ScoreMaxValue - BestScore then
          Stop := true;

      if not IsPondering and (BestScore <= -MateScoreCutoff) then
        if SearchDepth1 >= ScoreMaxValue + BestScore then
          Stop := true;

      if (DoProgressUpdates = true) and not (stop or SupressProgressMessage) then
        begin
        ElapsedMilliseconds := GetElapsedMilliseconds;

        ProgressMessage := '';
        for i := low(BestPV_array) to high(BestPV_array) do
          if BestPV_array[i, 0] > 0 then
            begin
            if i <> low(BestPV_array) then
              ProgressMessage := ProgressMessage + #13#10;

            MultiPVstr := '';
            if high(BestPV_array) > 0 then
              MultiPVstr :=  ' multipv ' + IntToStr(i+1);

            ProgressMessage := ProgressMessage + 'info depth ' + IntToStr(BestDepth) + ' seldepth ' + IntToStr(SelDepth) + MultiPVstr;

            if BestPV_array[i, 1].score >= MateScoreCutoff then
              ProgressMessage := ProgressMessage + ' score mate ' + IntToStr( (ScoreMaxValue - BestPV_array[i, 1].score + 1) div 2)
             else if BestPV_array[i, 1].score <= -MateScoreCutoff then
              ProgressMessage := ProgressMessage + ' score mate -' + IntToStr( (ScoreMaxValue + BestPV_array[i, 1].score + 1) div 2)
             else
              ProgressMessage := ProgressMessage + ' score cp ' + IntToStr(BestPV_array[i, 1].score);

            ProgressMessage := ProgressMessage + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(BestPV_array[i], BestDepth);
            end;

        if Assigned(FOnMessageOut) then
          FOnMessageOut(ProgressMessage);
        end;

      if not IsPondering then     //  don't stop for time if pondering
        begin
        ElapsedTicks := GetElapsedTicks;
        if ElapsedTicks > TickLimit then
          Stop := true;

        if UseSoftTimeLimit and (ElapsedTicks > 0) and (SearchDepth1 > 0) then
          begin
          Projection := power(2, 1.25 * Log2(ElapsedTicks * 1000.0 / TickFrequency) / SearchDepth1);
          if ElapsedTicks * Projection > TickLimit then
            Stop := true;
          end;
        end;

      inc(SearchDepth1);

      if SearchDepth1 > MaxDepth then
        SupressProgressMessage := true;

      // if pondering then continue to loop even when depth limit is reached

      if IsPondering and not Stop then
        SearchDepth1 := min(SearchDepth1, MaxDepth);  // if IsPondering then must keep looping, even if DepthLimit is reasched until 'Ponderhit' or 'Stop'
      end;
    except
    Stop := true;   // fall through to finished: and emit best move so far
    end;

  SearchTime_ms := GetElapsedMilliseconds;

  if UseSoftTimeLimit and not IsPondering then
    TickCarryover := max(MillisecondsToTicks(StartBudgetTime) - GetElapsedTicks, 0)
   else
    TickCarryover := 0;

  if BestPV_array[0, 0] > 0 then
    begin
    Search_Result.Depth := BestDepth;
    Search_Result.selDepth := selDepth;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    for i := 0 to BestPV_array[0, 0] do
      Search_Result.PV[i] := BestPV_array[0, i];
    Search_Result.score := BestPV_array[0, 1].score;
    end
   else if RootMoves[0] > 0 then
    begin                                  // search aborted before depth 1 - play first legal move
    Search_Result.Depth := 0;
    Search_Result.selDepth := 0;
    Search_Result.nodes := NodeCount;
    Search_Result.time := SearchTime_ms;
    Search_Result.PV[0] := 1;
    Search_Result.PV[1] := RootMoves[1];
    Search_Result.score := 0;
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

  finished:

  ConcludeSearch;

  end;


function UInt64ToDouble(value : UInt64) : double;
  begin
  result := value;
  end;

end.




