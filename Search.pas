//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2024 Christopher Crone

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

uses
  Winapi.Windows, SysUtils, System.SyncObjs, System.Classes, Math, Threading, Diagnostics,
  Common, GameDef, Eval, Openbook;

  {$IF defined(MSWINDOWS)}
  function GenRandom(pbBuffer :PBYTE; dwLen: DWORD):BOOL; stdcall;
  {$ENDIF}

const

  ThreadDebugInfo = false;

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

  Parallel = true;
  Sequential = false;

  MaxSearchPly = 128;

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
  TPVRec = record
    UID : UInt64;
    MoveList : TMoveArray;
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
  TKillerMoveTable = array[0..MaxSearchPly - 1] of TMove;

type
  TRepList = array[0..MaxSearchPly - 1] of UInt64;        // first value array[0] = number of entries

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

      Hit, Miss, exact : Int64;

      SearchAge : Int64;  // SearchAge : [0..63]

    constructor Create;
    procedure SetTableSize(MB_Size : integer);

    function Score(Info : UInt64) : integer;   inline;
    function Source(Info : UInt64) : integer;  inline;
    function Dest(Info : UInt64) : integer;    inline;
    function Depth(Info : UInt64) : integer;   inline;
    function Flag(Info : UInt64) : integer;    inline;
    function Age(Info : UInt64) : integer;     inline;
    function Move(Info : UInt64) : TMove;      inline;

    function PackInfo(const Move : TMove; Depth, Flag : integer) : UInt64;

    function RetrieveData(HashCode : UInt64; var Data : UInt64) : boolean;
    procedure StoreData(HashCode : UInt64; Data : UInt64);

    procedure Prefetch(HashCode : UInt64);
    function FillPermill : integer;   //   100% = 1000
    procedure ClearTable;
    procedure NewSearch;
    end;


type
  THistoryTable = record
    var
      Table : array[1..6, 0..63] of integer;
      CheckTable : array[1..6, 0..63] of integer;

      IsEmpty : boolean;

    procedure Update(const Move : TMove; Adjustment : integer);
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
      KillerMoveA : TKillerMoveTable;
      KillerMoveB : TKillerMoveTable;

      NodeCount, PrevCount : Int64;
      SearchTime : UInt64;
      selDepth : integer;

      FirstRootMoveSearched : boolean;
      ID : integer;

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
  TSearchFinishedProc = procedure(SearchUpdate : TSearchData);

type
  TSearch = class

      TransTable : TTransTable;
      ABDADA_Table : T_ABDADA_Table;

      RepTable : TRepList;
      SignatureList : TSignatureList;

      WhiteHistory : THistoryTable;
      BlackHistory : THistoryTable;

      WhiteCounterMove : TCounterMoveTable;
      BlackCounterMove : TCounterMoveTable;

      KillerMoveA : TKillerMoveTable;
      KillerMoveB : TKillerMoveTable;

      StartTickCount : UInt64;

      Searching : Boolean;
      Stop : Boolean;
      ShutDownUnderway : Boolean;
      ReadyToSearch :Boolean;

      // Search Information

      NodeCount : Int64;
      QNodeCount : Int64;
      SearchTime : UInt64;
      selDepth : integer;

      // Search Parameters

      TimeRemaining_White : UInt64;           // UCI:   wtime <x> white has x msec left on the clock
      TimeRemaining_Black : UInt64;           // UCI:   btime <x> black has x msec left on the clock
      TimeIncrementPerMove_White : UInt64;    // UCI:   winc <x> white time increment per move in msec if x>0
      TimeIncrementPerMove_Black : UInt64;    // UCI:   binc <x> black time increment per move in msec if x>0
      MovesToGo :  UInt64;                    // UCI:   movestogo <x> therre are x moves until the next time control if x>0 (only sent if x>0)

      TimeLimit : UInt64;                     // if time control then to calculate based on remaing time and estimated remaining moves
      NodeLimit : UInt64;                     // UCI:   nodes <x> search x nodes only
      DepthLimit : integer;                   // UCI:   depth <x> search x plies only
      ThreadCount : integer;                  // UCI:   option name Threads type spin default 1 min 1 max 32

      // Search Options:

      DoProgressUpdates : boolean;
      DoParallelSearch : boolean;
      DoAspirationSearch : boolean;
      DoSearchExtension : boolean;
      UseOwnBook : boolean;

      // Search Outcome

      Search_Result : TSearchData;
      BestMove : UInt64;
      FOnSearchProgressUpdate : TNotifyEvent;
      FOnSearchFinished : TSearchFinishedProc;

      constructor Create;
      destructor Destroy; override;

      procedure SetDefault;

      procedure NewGame;
      procedure PrepareForNextSearch;

      procedure StartInBackGround(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure FullSearch(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure ABDADA_Search(const Board : TBoard; const GameMoveList : TGameMoveList);

      procedure ClearHistoryTables;
      procedure ClearCounterMoveTables;
      procedure DampHistoryTables;
      procedure ClearKillerMoves;
      procedure SetTransTableSize(size : integer);
      procedure SetThreadCount(value : integer);
      procedure ClearTranspositionTable;
      procedure Clear_ABDADA_Table;
      procedure LoadSignatureList(const Board : TBoard; const GameMoveList : TGameMoveList);
      procedure CalcTimeLimit(const Board : TBoard);

     public
      property OnSearchFinished: TSearchFinishedProc read FOnSearchFinished write FOnSearchFinished;
      property OnProgressUpdate: TNotifyEvent read FOnSearchProgressUpdate write FOnSearchProgressUpdate;

      procedure StopSearch;
      procedure Shutdown;

      function ResultToStr : string;

    end;

type
  TSearchPtr = ^TSearch;

procedure RateMovesQ(MoveCount : integer; var moves : TMoveArray);
procedure SortMoves(count : integer; var moves : TMoveArray);

implementation

{$CODEALIGN 16}

{$IF defined(MSWINDOWS)}
function GenRandom; external ADVAPI32 name 'SystemFunction036';
{$ENDIF}

var
  Pool: TThreadPool;
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
  //    Age              :   bits 30..35   :  Enpassant Cell
  //    Depth            :   bits 36..42   :  Half Move Count
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


function TTransTable.Age(Info : UInt64) : integer;
  begin
  result := integer((Info shr 30) and $3F);
  end;


function TTransTable.Depth(Info : UInt64) : integer;
  begin
  result := integer((Info shr 36) and $FF);
  end;


function TTransTable.Score(Info : UInt64) : integer;
  begin
  result := integer(Info shr 48) - ScoreOffset;
  end;


function TTransTable.Move(Info : UInt64) : TMove;
  begin
  result := Info and $FFFF000000FFFFFF;
  end;


function TTransTable.PackInfo(const Move : TMove; Depth, Flag : integer) : UInt64;
  begin
  result := (Move and $FFFF000000FFFFFF) or (UInt64(Flag and $3F) shl 24) or (UInt64(SearchAge) shl 30) or (UInt64(Depth and $FF) shl 36);
  end;


procedure TTransTable.StoreData(HashCode : UInt64; Data : UInt64);
  var
    index, UID_1, Data_1 : UInt64;
    AgeAdj : integer;

  begin
  index := (HashCode and TableMask);     // index of first slot : depth preferred

  UID_1 :=  Table[Index].UID;
  Data_1 := Table[Index].Data;

  if (UID_1 xor Data_1) = HashCode then
    begin
    if (Data and UInt64($FFFFFF)) = 0 then             // if Data does't have move then keep previous move
      Data := Data or (Data_1 and UInt64($FFFFFF));

    if Depth(Data_1) = Depth(Data) then
      begin
      if ((Score(Data) > Score(Data_1)) and (Flag(Data) <> ftUpperbound)) or
         ((Score(Data) < Score(Data_1)) and (Flag(Data) <> ftLowerbound)) or
         ((Score(Data) >= MateScore - MaxSearchPly) and (Score(Data) > Score(Data_1))) then

        begin
        Table[index].UID := HashCode xor Data;
        Table[index].Data := Data;
        exit;
        end;

      if Score(Data) = Score(Data_1) then
        begin
        if Flag(Data) <> Flag(Data_1) then
          Data := (Data and $FFFFFFFFC0FFFFFF) or (UInt64(ftExact and $3F) shl 24);
        Table[index].UID := HashCode xor Data;
        Table[index].Data := Data;
        exit;
        end;
      end;
    end;

  AgeAdj := Age(Data) - Age(Data_1);
  AgeAdj := AgeAdj + (AgeAdj and $40);       // always > 0 if Data younger than Data_1, even with wrap-around of age from 63 back to 0

  if (Depth(Data_1) <= Depth(Data) + 4 * AgeAdj) or ((Score(Data) >= MateScore - MaxSearchPly) and (Score(Data) > Score(Data_1))) then
    begin
    Table[index].UID := HashCode xor Data;
    Table[index].Data := Data;
    exit
    end;

  inc(index);         // index of second slot : always replace

  Table[index].UID := HashCode xor Data;
  Table[index].Data := Data;
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

  Miss := 0;
  Hit := 0;
  Exact := 0;
  end;


procedure TTransTable.NewSearch;
  begin
  inc(SearchAge);
  SearchAge := SearchAge and $3F;
  end;


procedure TTransTable.Prefetch(HashCode : UInt64);
  asm
  and rdx, self.TableMask
  lea rcx, self.Table
  mov rcx, [rcx]
  add rdx, rdx
  lea rax, [rcx + rdx*8]

  PrefetchT1 byte ptr [rax]
  end;


function TTransTable.FillPermill : integer;
  var
    i, FillCount : integer;

  begin
  FillCount := 0;

  for i := 0 to 2047 do
    if Age(Table[i].Data) = SearchAge then
      inc(FillCount);

  result := (FillCount * 1000) div 2048;
  end;


// History Table ===============================================================


procedure THistoryTable.Update(const Move : TMove; Adjustment : integer);
  var
    Piece, Dest : integer;

  begin
  IsEmpty := false;
  Piece := (Move shr 12) and $F;
  Dest :=  (Move shr 6) and $3F;

  Table[Piece, Dest] := Table[Piece, Dest] + ((32768 - abs(Table[Piece, Dest])) * Adjustment) div 32768;
  end;


procedure THistoryTable.ClearTable;
  var
    Piece, Dest : integer;

  begin
  IsEmpty := true;
  for Piece := 1 to 6 do
    for Dest := 0 to 63 do
      Table[Piece, Dest] := 0;
  end;


procedure THistoryTable.DampTable;
  var
    Piece, Dest : integer;

  begin
  for Piece := 1 to 6 do
    for Dest := 0 to 63 do
      Table[Piece, Dest] := Table[Piece, Dest] div 2 ;
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
  SelDepth := 0;
  ID := 0;

  FirstRootMoveSearched := false;
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

procedure WriteConsoleString(const S: string);
  var
    temp: Cardinal;
    tempstr : string;

  begin
  if S <> '' then
    begin
    tempstr := S + #13#10;
    WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), @tempstr[1], Length(tempstr),
      temp, nil);
    end;
  end;


procedure RateMovesQ(MoveCount : integer; var moves : TMoveArray);
  var
    i : integer;
    score, capturedpiece, piece : integer;

  begin
  for i := 1 to MoveCount do
    begin
    Piece := (Moves[i] shr 12) and $F;
    CapturedPiece := (Moves[i] shr 16) and $F;

    score:= 8 + CapturedPiece * 8 - piece;
    moves[i] := moves[i] and UInt64($0000FFFFFFFFFFFF) xor UInt64(Score and $FFFF) shl 48;
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


procedure RateMoves(Search : TSearchPtr; var Board : TBoard; Player, MoveCount : integer; var moves : TMoveArray);
  var
    i : integer;
    score, piece, promotionpiece, capturedpiece, temp, Dest : integer;

  begin
  if Player = white then
    for i := 1 to MoveCount do
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
          temp :=  PieceValue[CapturedPiece] - Board.SEE(Moves[i]);
          Board.UndoMoveNoHash(Moves[i]);

          if temp < 0 then
            score := temp + $1000;      //  losing capture
          end;
        end
       else
        begin
        Dest := (moves[i] shr 6) and $3F;
        score := search.WhiteHistory.Table[Piece, Dest] div 4 + $2000;    // ensure score not -ve
        end;

      if (PromotionPiece > 0) and (score < $7000) then
        begin
        if PromotionPiece = 5 then
          score := $6000
         else
          score := $4000;
        end;

      moves[i] := moves[i] and UInt64($0000FFFFFFFFFFFF) xor (UInt64(Score and $FFFF) shl 48);
      end
   else
    for i := 1 to MoveCount do
      begin
      Piece := (Moves[i] shr 12) and $F;
      CapturedPiece := (Moves[i] shr 16) and $F;
      PromotionPiece := (Moves[i] shr 20) and $F;

      if CapturedPiece > 0 then
        begin
        score := MVV_LVA_Lookup[Capturedpiece, Piece] + $7000;

        if PieceValue[Piece] > PieceValue[CapturedPiece] then
          begin
          Board.MakeMoveNoHash(Moves[i]);
          temp :=  PieceValue[CapturedPiece] - Board.SEE(Moves[i]);
          Board.UndoMoveNoHash(Moves[i]);

          if temp < 0 then
            score := temp + $1000;     //  losing capture
          end;
        end
       else
        begin
        Dest := (moves[i] shr 6) and $3F;
        score := search.BlackHistory.Table[piece, Dest] div 4 + $2000;     // ensure score not -ve
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


procedure SortMoves(count : integer; var moves : TMoveArray);

  //insertion sort

  var
    i, j : integer;
    temp : TMove;

  begin
  for i := 2 to count do
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



function PVS_Q(Search : TSearchPtr; ply, alpha, beta, CheckCount: Integer; PrevInCheck : boolean; var Board : TBoard; PrevMove : TMove): integer;
  var
    k : integer;
    value  : integer;
    Moves : TMoveArray;
    Movecount : integer;
    temp : integer;
    ChecksRemaining :integer;
    InCheckFlag : boolean;

    piece, capturedpiece : integer;

  begin
  if (GetTickCount64 - Search.StartTickCount) >= (Search.TimeLimit - 8) then
    Search.Stop := true;

  if Search.Stop = true then
    exit(InvalidScore);

  if ply > Search.SelDepth then
    Search.SelDepth := ply;

  InCheckFlag := Board.KingInCheck(Board.ToPlay);

  if not InCheckFlag then        // StandPat TODO : check if not in zugzwang
    begin
    value := ScoreFromBoard(Board);

    if value >= beta then
      exit(value);

    CapturedPiece := integer((PrevMove shr 16) and $F);
    Piece :=  integer((PrevMove shr 12) and $F);

    if alpha < value then
      alpha := value;

    if (PrevInCheck = false) and (Piece > CapturedPiece) and (CapturedPiece > 0) then
      begin
      temp :=  PieceValue[CapturedPiece] - Board.SEE(PrevMove);

      if temp < 0 then
        exit(beta);         // losing move so prune it, but only if not in check, and if it doesn't give check
      end;
    end;

  // Get Moves to investigate

  // Qsearch allows checks from quiet moves, however must set some limit (ie: to handle perpetual check).

  ChecksRemaining := CheckCount;

  if InCheckFlag = true then
    movecount := Board.GetAllValidMoves(Board.ToPlay, Moves, false)   // When the king is in check, all moves are generated and searched.
   else
    begin
    if CheckCount = 0 then
      movecount := Board.GetAllValidMoves(Board.ToPlay, Moves, true)    // Capture and Promotion Moves only, - no quiet moves
     else
      begin
      movecount := Board.GetCapturePromotionAndCheckingMoves(Board.ToPlay, Moves);   // Capture, Promotion and Checking Moves incl Quiet Checking moves
      ChecksRemaining := CheckCount-1;
      end;
    end;

  //  Evaluate

  if movecount = 0 then
    begin
    if InCheckFlag = true then    // this position is checkmate, but may not be a forced mate if quiet moves were not considered in one or more ancestor nodes
      exit(-10_000 + ply)
     else
      exit(alpha);     // no more violent moves left
    end;

  if movecount > 1 then
    begin
    RateMovesQ(movecount, Moves);
    SortMoves(movecount, Moves);
    end;

  // Search

  for k := 1 to movecount do
    begin
    Board.MakeMove(Moves[k]);
    value := -PVS_Q(search, ply+1, -beta, -alpha, ChecksRemaining, InCheckFlag, Board, Moves[k]);
    Board.UndoMove(Moves[k]);

    if value = - InvalidScore then
      exit(InvalidScore);

    if value >= beta then
      exit(value);

    if value > alpha then
     alpha := value;
    end;

  result := alpha;
  end;


function PVS_ABDADA(Search : TSearchPtr; ThreadData : T_ThreadDataPtr; ply, depth, alpha, beta, NodeType : Integer;
               var Board : TBoard; var PV : TPVArray;
               var RepList : TRepList; NullMove : boolean; PrevMove : TMove) : integer;
  const
    R_IID = 3;
    R = 3;
    R_Null = 3;
    Rn = 3;
    CC = 2;
    MM = 6;

    ETC_Limit = 6;

  var
    i, j, k, f : integer;
    value, Flag, alphaOrig, temp, score : integer;
    Move, KillerMove, BestMove, CounterMove, TempMove, TTMove : TMove;
    Moves : TMoveArray;
    Movecount : integer;
    HashCode, TT_Data, MoveHash : UInt64;
    TempEp : UInt64;
    DataExists, HashMove, InCheck : boolean;
    PV1, PVdummy : TPVArray;
    NewNodetype, c, m, epCell : integer;
    Best, NewDepth : integer;
    HalfCount : integer;
    LMR_Flag, ThreatDetected, TT_OKtoStore : boolean;
    ReductionLimit, FullDepthMoves, LMR : integer;

  begin
  if (GetTickCount64 - Search.StartTickCount) >= (Search.TimeLimit - 8) then
    Search.Stop := true;

  if Search.NodeCount >= Search.NodeLimit then
    Search.Stop := true;

  if Search.Stop then
    exit(InvalidScore);

  Inc(ThreadData.NodeCount);

  // Check for immediate win or stalemate

  if Board.MoveExists(InCheck) = false then         // also sets value of InCheck for use later in this routine
    begin
    PV[0] := 0;
    if InCheck = true then
      exit(-MateScore + ply)       // checkmate
     else
      exit(0);                     // stalemate
    end;


  if (Board.HalfCount >= 100) and (ply > 0) then    // if Board.HalfCount = 100 and ply = 0 then need to return a valid move so continue
    begin
    PV[0] := 0;
    exit(RepetitionFound);                          // Draw by 50 move rule, return RepetitionFound so not stored in TT
    end;

  if depth <> 0 then
    DataExists := Search.TransTable.RetrieveData(Board.Hash, TT_Data);


  //  Check for repetition & if so score this position as draw

  HalfCount := Board.HalfCount;

  if ply >= 4 then
    begin
    for i := (ply - 4) downto max(ply - HalfCount, 0) do
      if Board.Hash = RepList[i] then                           // RepList[0] = hash of board at ply = 0
        begin                                                   // RepList[1] = hash of board at ply = 1 etc.
        PV[0] := 0;
        exit(RepetitionFound);
        end;
    end;

  if (HalfCount > ply) and (ply > 0) then                               // if ply = 0 then need to return a valid move so continue
    for i := 1 to min(HalfCount - ply, search.SignatureList[0]) do      // search.SignatureList[0] contains count of valid historical entries in Table
      if Board.Hash = search.SignatureList[i] then                      // search.SignatureList[1] = Hash of Board at ply = -1
        begin
        PV[0] := 0;
        exit(RepetitionFound);
        end;


  //  Evaluate if leaf node

  if depth = 0 then
    begin
    value := PVS_Q(search, ply, alpha, beta, 2, InCheck, Board, PrevMove);

    if ply > ThreadData.SelDepth then
      ThreadData.SelDepth := ply;

    if Search.Stop then
      exit(InvalidScore);

    PV[0] := 0;
    exit(Value) ;
    end;


  // Check transposition table

  Best := ScoreMinValue;
  BestMove := 0;
  alphaOrig := alpha;
  BestMove := 0;

  TT_OKtoStore := true;
  TTMove := 0;

  // another thread may have searched this position already, even at root

  if DataExists = true then
    begin
    TTMove := search.TransTable.Move(TT_Data);
    if Search.TransTable.Depth(TT_Data) >= depth then
      begin
      PV[0] := 0;
      Flag := Search.TransTable.Flag(TT_Data);
      value := Search.TransTable.Score(TT_Data);

      if value > (MateScore - MaxSearchPly) then
          value := value - ply                          // retrieving from TT, so subtract ply
       else if value < -(MateScore - MaxSearchPly) then
          value := value + ply;                          // retrieving from TT, so add ply

      if (Flag = ftLowerBound) and (value >= beta) then
        exit(value)
       else if (Flag = ftUpperBound) and (value <= alpha) then
        exit(value)
       else if (Flag = ftExact) and (value < beta) and (value > alpha) then
        alpha := value;

      if (flag = ftUpperBound) and (depth - R_Null - depth div 4 <= Search.TransTable.Depth(TT_Data)) and (value < Beta) then
        NullMove := OmitNull;
      end;
    end;


  // Reverse Futility Pruning

  // if your score is so good you can take a big hit and still get the beta cutoff, go for it

  if (depth = 1) and (NodeType <> PV_Node) and (not InCheck) and (ply > 0) then
    begin
    temp := ScoreFromBoard(Board);

    if (temp - 160) >= Beta then         //   160  approx value of strong pawn
      begin
      PV[0] := 0;
      exit(temp - 160);
      end;
    end;


  if (depth = 2) and (NodeType <> PV_Node) and (not InCheck) and (ply > 0) then
    begin
    temp := ScoreFromBoard(Board);

    if (temp - 320) >= Beta then     //    320  approx value of minor
      depth := 1;                    //    i.e. depth := depth - 1
    end;

  movecount := Board.GetAllValidMoves(Board.ToPlay, Moves, false);


  // Recursive Null Move forward pruning

  ThreatDetected := false;

  if (NullMove = DoNull) and (depth > 2) and (Beta < MateScore - ply) and (movecount > 4) and (ply > 0) then
    if not InCheck and (NodeType <> PV_Node) and not Board.OnlyPawns then
      begin
      value := ScoreFromBoard(Board);

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
        Board.PawnHash:= Board.PawnHash xor Board.PlayerHash;
        Board.ToPlay := 1 - Board.ToPlay;                       // swap player

        if NodeType = CUT_Node then
          NewNodetype := ALL_Node
         else
          NewNodetype := CUT_Node;

        value := -PVS_ABDADA(search, ThreadData, ply+1, max(depth - R_Null - depth div 4, 1), -beta, -beta+1, NewNodetype, Board, PVdummy, RepList, OmitNull, 0);

        if Search.Stop then
          exit(InvalidScore);

        if value = -InvalidScore then
          exit(InvalidScore);

        if value = -RepetitionFound then
          value := 0;

        if value < beta  then
          if -PVS_ABDADA(search, ThreadData, ply, max(depth - R_Null - depth div 4,  1), -beta, -alpha, Cut_Node, Board, PVdummy, RepList, OmitNull, PrevMove) + 320 < alpha then
            ThreatDetected := true;

        Board.ToPlay := 1 - Board.ToPlay;
        Board.Hash := Board.Hash xor Board.PlayerHash;
        Board.PawnHash := Board.PawnHash xor Board.PlayerHash;

        if EpCell <> -1 then
          begin
          Board.Hash := Board.Hash xor Board.epHash[epCell];    // add epCell back into Hash
          Board.Enpassant := tempEp;
          end;

        if value >= beta then                        // verify in case zugzwang, adds approx 10% to search time but avoids at least some errors
          if (depth > 4) or (depth > ply) then       // suggestion is don't do verification at lower depths
            begin

            value := PVS_ABDADA(search, ThreadData, ply, max(depth - R_Null - depth div 4,  1), beta - 1, beta, Cut_Node, Board, PVdummy, RepList, OmitNull, PrevMove);

            if value = InvalidScore then
              exit(InvalidScore);

            if (value <> RepetitionFound) and (value >= beta) then
              begin
              Move := PrevMove;

              if value > (MateScore - MaxSearchPly) then
                  score := value + ply                                 // saving to TT, so add ply
               else if value < -(MateScore - MaxSearchPly) then
                  score := value - ply;                                // saving to TT, so subtract ply

              Move := UInt64(Move and $0000FFFFFFFFFFFF) + UInt64((score + ScoreOffset) and $FFFF) shl 48;    //  Move.SetScore(value);

              TT_Data := search.TransTable.PackInfo(Move, max(depth - R_Null - depth div 4, 1), ftLowerBound);

              search.TransTable.StoreData(Board.Hash, TT_Data);
              end;

            if value = RepetitionFound then
              value := 0;
            end;

        if value >= beta then
          begin
          PV[0] := 0;
          exit(value);
          end;

        end;
      end;

  RateMoves(search, Board, Board.ToPlay, movecount, Moves);


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

  HashMove := false;

  if TTMove.IsValid then         // identify hash move
    begin
    for i := 1 to Movecount do
      if (Moves[i] and $FFF) = (TTMove and $FFF) then
        begin
        Moves[i] := Moves[i] or UInt64($8000000000000000);
        HashMove := true;
        break;
        end;
    end;

  SortMoves(movecount, Moves);


  // Enhanced Forward Pruning

  if (NodeType = Cut_Node) and not InCheck and (ply > 0) then
    begin
    if depth > (R+5) then
      begin
      PVdummy[0] := 0;

      c := 0;
      m := 1;

      while m <= min(MM, MoveCount) do
        begin
        Move := Moves[m];                   // to avoid ruining move ordering info
        Board.MakeMove(Move);               // perhaps this why move picker did not provide an improvement as hoped
        RepList[ply+1] := Board.Hash;

        NewNodetype := ALL_Node;

        DataExists := Search.TransTable.RetrieveData(Board.Hash, TT_Data);

        if (DataExists = true) and (depth - R - 1 <= Search.TransTable.Depth(TT_Data)) and (Search.TransTable.Flag(TT_Data) <> ftLowerBound) then
          begin
          value := Search.TransTable.Score(TT_Data);

          if value > (MateScore - MaxSearchPly) then
            value := value - ply                             // retrieving from TT, so subtract ply
           else if value < -(MateScore - MaxSearchPly) then
            value := value + ply;                           // retrieving from TT, so add ply
          end
         else
          value := -PVS_ABDADA(search, ThreadData, ply+1, depth-1-R, -beta, -beta+1, NewNodetype, Board, PVdummy, RepList, DoNull, Move);

        Board.UndoMove(Move);

        if Search.Stop then
          exit(InvalidScore);

        if value = -RepetitionFound then
          value := 0;

        if value >= beta then
          begin
          inc(c);
          if c >= CC then
            begin
            depth := depth - 2;
            break;
            end;
          end;

        inc(m);
        end;
      end;
    end;


  // depth extension must be done before ETC to ensure consistency when setting alpha

  if Search.DoSearchExtension then
    if (depth <= 2) and (InCheck or (PrevMove.PromotionPiece <> 0) or (MoveCount < 4)) then       // search extension
      depth := depth + 1;


  // Enhanced Transposition Cutoff

  if (NodeType <> All_Node) and (ply > 0)  then
    if depth >= 2 then
      begin
      for k := 1 to min(MoveCount, ETC_Limit) do   // Don't test all moves, just try first few
        begin
        Move := Moves[k];                          // to avoid ruining move ordering info

        HashCode := Board.GetMoveHash(Move);

        if search.TransTable.RetrieveData(HashCode, TT_Data) = true then
          begin
          if search.TransTable.Depth(TT_Data) = (depth - 1) then
            if search.TransTable.Flag(TT_Data) <> ftLowerBound then
              begin
              value := -search.TransTable.Score(TT_Data);

              if value > (MateScore - MaxSearchPly) then
                value := value - ply - 1                                // retrieving from TT, so subtract (ply + 1)
               else if value < -(MateScore - MaxSearchPly) then
                value := value + ply + 1;                               // retrieving from TT, so add (ply + 1)

              if value >= beta then
                begin
                PV[0] := 0;
                exit(value);
                end;

              if (search.TransTable.Flag(TT_Data) = ftExact) and (value > alpha) then
                begin
                alpha := value;
                best := value;
                bestmove := Move;
                BestMove := UInt64(BestMove and $0000FFFFFFFFFFFF) + UInt64((value + ScoreOffset) and $FFFF) shl 48;  // set score

                PV[1] := BestMove;
                PV[0] := 1;
                end;
              end;
          end;
        end;
      end;


  //Internal Iterative Deepening

  if HashMove = false then
    if (depth - R_IID > 3) and (NullMove <> OmitNull) then
      begin
      PVdummy[0] := 0;
      value := PVS_ABDADA(search, ThreadData, ply, depth - R_IID, alpha, beta, NodeType, Board, PVdummy, RepList, DoNull, PrevMove);

      if Search.Stop then
        exit(InvalidScore);

      if value = RepetitionFound then
        value := 0;

      if (PVDummy[0] <> 0) and (value > alpha) then
        begin
        Move := PVdummy[1];
        if Move.IsValid then              // put this move at start of move list
          for i := 1 to Movecount do
            if (Moves[i] and $FFFFFF) = (Move and $FFFFFF) then
              begin
              Move := Moves[i];
              for j := i downto 2 do
                Moves[j] := Moves[j-1];
              Moves[1] := Move;
              break;
              end;
        end;
      end;


  // Search

  PV1[0] := 0;
  ReductionLimit := 3;

  if (ply > 1) and (depth > 2) and (ThreatDetected = false) then  // only undertake LMR for ply 2 and greater
    begin
    FullDepthMoves := 3 + GetHighBit_Alt(ply + 1);        // GetHighBit_alt is undefined when argument = 0

    if NodeType = PV_Node then                            // PV_Node - increase number of full depth moves
      FullDepthMoves := FullDepthMoves + 4;               // FullDepthMoves + 12;

    for i := FullDepthMoves to MoveCount do               // flag potential moves for reduction
      if Moves[i] < UInt64($4000000000000000) then        // i.e. don't reduce winning captures, promotion, killer and countermoves
        setbit(Moves[i], 44);
    end;

  MoveHash := 0;

  for k := 1 to movecount do
    begin

    if alpha = (MateScore - ply - 1) then         // mate distance pruning
      exit(alpha);

    if (k = 1) and (ply = 0) then
      ThreadData.FirstRootMoveSearched := false;

    if (depth > 2) and (k > 1) then                       // no need to do this if it is first move in list
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

    RepList[ply+1] := Board.Hash;

    LMR_Flag := Getbit(Moves[k], 44);

    if LMR_Flag = true then
      begin
      if InCheck then
        LMR_Flag := false

      else if (Board.GameStage < 20) and ((Moves[k].Piece = pawn) or (Moves[k].Piece = king)) then     // treat king and pawn moves as interesting  : pawn moves has (-20%) + impact on speed of WAC_2018
        LMR_Flag := false

      else if Board.KingInCheck(Board.ToPlay) then                        // move gives check
        LMR_Flag := false;                                                // expensive, so only call if all other conditions to do LMR are true
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
      value := -PVS_ABDADA(search, ThreadData, ply+1, depth-1, -beta, -alpha, NewNodeType, Board, PV1, RepList, DoNull, Moves[k])
     else
      begin
      newDepth := depth - 1;

      if LMR_Flag = true then
        begin
        LMR := max((GetHighBit_Alt(k) + GetHighBit_Alt(depth) + 1) shr 1, 2);     // k = 2..32 : highbit = 1..5,  depth = 24..1 : highbit = 4..0

        if NodeType = PV_Node then
          newDepth := depth - 2       // i.e. reduce by 1 ply
         else
          newDepth := depth - LMR;    // i.e. reduce by 1..5 ply depending on number of moves checked and depth

        newdepth := max(1, newdepth);
        end;

      if MoveHash <> 0 then
        Search.ABDADA_Table.MoveSearchBegin(MoveHash);

      value := -PVS_ABDADA(search, ThreadData, ply+1, newDepth, -alpha-1, -alpha, NewNodeType, Board, PVdummy, RepList, DoNull, Moves[k]);

      if value <> -InvalidScore then
        if ((value = -RepetitionFound) and (0 > alpha) and (0 < beta)) or ((value <> -RepetitionFound) and (value > alpha) and (value < beta)) then
          value := -PVS_ABDADA(search, ThreadData, ply+1, newDepth, -beta, -alpha, NewNodeType, Board, PV1, RepList, DoNull, Moves[k]);

      if (newdepth < depth - 1) and (value > alpha) then         // if reduced depth search returns score > alpha then research at full depth
        value := -PVS_ABDADA(search, ThreadData, ply+1, depth-1, -beta, -alpha, PV_Node, Board, PV1, RepList, DoNull, Moves[k]);

      if MoveHash <> 0 then
        Search.ABDADA_Table.MoveSearchOver(MoveHash);
      end;

    Board.UndoMove(Move);

    if value = -RepetitionFound then
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

    if (ply = 0) and (k = 1) and (value <> -InvalidScore) then
      ThreadData.FirstRootMoveSearched := true;                     // flag that first move in root has been fully searched

    if (value <> -InvalidScore) and (value > Best)  then
      begin
      best := value;
      BestMove := Moves[k];
      BestMove := UInt64(BestMove and $0000FFFFFFFFFFFF) + UInt64((value + ScoreOffset) and $FFFF) shl 48;

      PV[1] := BestMove;
      for i := 1 to PV1[0] do
        PV[i+1] := PV1[i];
      PV[0] := PV1[0] + 1;

      if best >= Beta then
        begin
        if BestMove.CapturedPiece = 0 then
          begin
          if Board.ToPlay = white then
            begin
            Search.WhiteHistory.Update(BestMove, depth*depth);
            for i := 1 to k-1 do
              Search.WhiteHistory.Update(Moves[i], -depth);
            end
           else
            begin
            Search.BlackHistory.Update(BestMove, depth * depth);
            for i := 1 to k-1 do
              Search.BlackHistory.Update(Moves[i], -depth);
            end;

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

    if (Search.Stop and (ply > 0)) or (Best = ScoreMinValue) or (Search.Stop and (ply = 0) and (ThreadData.FirstRootMoveSearched = false)) then
      exit(InvalidScore);

    if (Search.Stop) and (ply = 0) then
      exit(Best);
    end;

  if (TT_OKtoStore = false) and (((PrevMove and UInt64($FF0000)) <> 0) or ((PrevMove and UInt64($F000)) = UInt64($1000))) then  // if prev move is capture/promotion or pawn move then irreversible position and OK to store in TT
    TT_OKtoStore := true;

  if  TT_OKtoStore and (Search.Stop = false) then
    begin
    TempMove := BestMove;
    if Best > MateScore - MaxSearchPly then
      TempMove.SetScore(Best + ply)                    // saving to TT, so add ply
     else if Best < -MateScore + MaxSearchPly then
      TempMove.SetScore(Best - ply);                   // saving to TT, so subtract ply

    if Best <= alphaOrig then
      begin
      Flag := ftUpperBound;                                    // All_Node
      TempMove := TempMove and UInt64($FFFF000000000000);      // No idea about which move is best since all
                                                               // failed low, so store best move of 'zero'.
      end
     else if Best >= beta then
      begin
      Flag := ftLowerBound;         // Cut_Node
                                    // A good move to try first again since it caused
                                    // a cut-off, although it may not be the best move
                                    // as all moves were not searched.
      end
     else
      begin
      Flag := ftExact;              // PV_Node
      end;

    TT_Data := search.TransTable.PackInfo(TempMove, depth, flag);     // store adjusted score in TT
    search.TransTable.StoreData(Board.Hash, TT_Data);
    end;

  result := Best;

  if (result = 0) and (TT_OKtoStore = false) and (ply > 0) then   // draw by repetition
    result := RepetitionFound;
  end;


//   TSearchEngine_PVS

constructor TSearch.Create;
  begin

  inherited Create;

  TransTable := TTransTable.Create;

  setdefault;
  NewGame;

  Searching := false;
  ShutdownUnderway := false;
  OnSearchFinished := nil;
  OnProgressUpdate := nil;
  end;


destructor TSearch.Destroy;
  begin
  if Searching = true then
    ShutDown;

  if TransTable <> nil then
    TransTable.Free;

  // Always call the parent destructor after running your own code
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
      sleep(50);
      end;
    end;
  end;


procedure TSearch.StopSearch;
  begin
  Stop := true;
  end;


function TSearch.ResultToStr : string;
  var
    nps : double;
    HashFill : integer;

  begin
  if Search_Result.time > 0 then
    nps := Search_Result.nodes * 1000 / Search_Result.time
   else
    nps := 0.0;

  if TransTable <> nil then
    hashfill := TransTable.FillPermill;

  if Search_Result.score = -MateScore then
    result := 'info depth 0 ' {+ ' seldepth ' + IntToStr(Search_Result.selDepth)} + 'score mate nodes ' + FormatFloat('#,###', Search_Result.nodes) {+ IntToStr(Search_Result.nodes)} +
                            ' time ' + IntToStr(Search_Result.time)

  else if Search_Result.score >= MateScore - MaxSearchPly then
    result := 'info depth ' + IntToStr(Search_Result.Depth) {+ ' seldepth ' + IntToStr(Search_Result.selDepth)} + ' score mate ' + IntToStr( (MateScore - Search_Result.score + 1) div 2)  + ' nodes ' + FormatFloat('#,###', Search_Result.nodes) {+ IntToStr(Search_Result.nodes)} +
                           ' nps ' + FormatFloat('#,###', nps) + ' hashfull ' + IntToStr(hashfill) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV)

  else if Search_Result.score <= -MateScore + MaxSearchPly then
    result := 'info depth ' + IntToStr(Search_Result.Depth) {+ ' seldepth ' + IntToStr(Search_Result.selDepth)} + ' score mate -' + IntToStr( (MateScore + Search_Result.score + 1) div 2) + ' nodes ' + FormatFloat('#,###', Search_Result.nodes) {+ IntToStr(Search_Result.nodes)} +
                           ' nps ' + FormatFloat('#,###', nps) + ' hashfull ' + IntToStr(hashfill) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV)

  else
    result := 'info depth ' + IntToStr(Search_Result.Depth) {+ ' seldepth ' + IntToStr(Search_Result.selDepth)} + ' score cp ' + IntToStr(Search_Result.score) + ' nodes ' + FormatFloat('#,###', Search_Result.nodes) {+ IntToStr(Search_Result.nodes)} +
                           ' nps ' + FormatFloat('#,###', nps) + ' hashfull ' + IntToStr(hashfill) + ' time ' + IntToStr(Search_Result.time) + ' pv ' + PVToStr(Search_Result.PV);
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
  ClearKillerMoves;

  NodeCount := 0;
  QNodeCount := 0;
  SearchTime := 0;
  SelDepth := 0;

  Stop := false;
  ShutdownUnderway := false;

  ReadyToSearch := true;
  end;


procedure TSearch.SetDefault;
  begin
  DepthLimit := 128;
  TimeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue
  NodeLimit := UInt64(18_446_744_073_709_551_615);    // UInt64.MaxValue

  DoParallelSearch := true;
  ThreadCount := 6;

  DoAspirationSearch := true;
  DoSearchExtension := true;

  DoProgressUpdates := true;
  UseOwnBook := true;
  end;


procedure TSearch.ClearHistoryTables;
  begin
  WhiteHistory.ClearTable;
  BlackHistory.ClearTable;
  end;


procedure TSearch.ClearCounterMoveTables;
  begin
  WhiteCounterMove.ClearTable;
  BlackCounterMove.ClearTable;
  end;


procedure TSearch.DampHistoryTables;
  begin
  WhiteHistory.DampTable;
  BlackHistory.DampTable;
  end;


procedure TSearch.ClearKillerMoves;
  var
    ply : integer;

  begin
  for ply := low(KillerMoveA) to high(KillerMoveA) do
    KillerMoveA[ply] := 0;
  for ply := low(KillerMoveB) to high(KillerMoveB) do
    KillerMoveB[ply] := 0;
  end;


procedure TSearch.ClearTranspositionTable;
  begin
  if TransTable <> nil then
    TransTable.ClearTable;
  end;


procedure TSearch.SetThreadCount(Value : integer);
  begin
  ThreadCount := min(max(1, value), 32);            // 0..32
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


procedure TSearch.CalcTimeLimit(const Board : TBoard);
  begin
  if Board.ToPlay = white then
    TimeLimit := ((MovesToGo * TimeIncrementPerMove_White + TimeRemaining_White) * 112) div (128 * (max(MovesToGo, 1)))
   else
    TimeLimit := ((MovesToGo * TimeIncrementPerMove_Black + TimeRemaining_Black) * 112) div (128 * (max(MovesToGo, 1)));
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
    MasterThread : TThread;

  begin
  MasterThread := TThread.CreateAnonymousThread(
      procedure
        begin
        ABDADA_Search(Board, GameMoveList);
        end);

  MasterThread.Start;
  end;


procedure TSearch.ABDADA_Search(const Board : TBoard; const GameMoveList : TGameMoveList);
  const
    AspirationWindow = 80;

  var
    i : integer;
    alpha, beta, MaxDepth : integer;
    ElapsedMilliseconds : Int64;
    BestPV: TPVArray;
    Lock : TCriticalSection;
    workerCount, ThreadsPerDepth, index : integer;
    workers: TArray<ITask>;
    ProgressMessage : string;
    Move : TMove;

    DepthToken, HighestDepth : integer;

  begin
  if TransTable <> nil then
    TransTable.NewSearch;        // increments search age, must do before openbook

  if UseOwnBook = true then
    begin
    if Book.GetMove(Board.Hash, Move, ms_BestHist) = true then
      begin
      BestPV[0] := 1;
      BestPV[1] := Move and UInt64($0000FFFFFFFFFFFF);

      Search_Result.Depth := 1;
      Search_Result.selDepth := 0;
      Search_Result.nodes := 1;
      Search_Result.time := 0;
      Search_Result.PV := BestPV;
      if move.score <> -32768 then
        Search_Result.score := Move.score
       else
        Search_Result.score := 0;

      if DoProgressUpdates = true then
        begin
        ProgressMessage := ResultToStr;

        writeln(output, AnsiString(' '));
        writeln(output, AnsiString(ProgressMessage + #13#10));

        if Search_Result.PV[0] > 1 then
          ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr + ' ponder ' + Search_Result.PV[2].ToStr
         else if Search_Result.PV[0] = 1 then
          ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr
         else if Search_Result.PV[0] = 0 then
          ProgressMessage := 'bestmove (none)';

        writeln(output, AnsiString(ProgressMessage + #13#10));
        flush(output);
        end;

      Searching := false;
      ReadyToSearch := false;

      if Assigned(fOnSearchFinished) then
        fOnSearchFinished(Search_Result);

      exit;
      end;
    end;

  StartTickCount := GetTickCount64;

  Searching := true;

  if ReadyToSearch = false then
    PrepareForNextSearch;              // clears all tables except Transposition Table

  LoadSignatureList(Board, GameMoveList);   // required for repetition check

  MaxDepth := DepthLimit;

  alpha := ScoreMinValue+1;
  beta  :=  ScoreMaxValue-1;

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
  ElapsedMilliseconds := 0;
  BestMove := 0;

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
          PV1 : TPVArray;
          RepList1 : TRepList;
          ThreadData1 : T_ThreadData;
          UpdateResult1 : boolean;
          LoopCount : integer;

        begin
        k := InterLockedIncrement(Index);

        ply1 := 0;
        score1 := 0;
        PrevMove1 := 0;      // Used for countermove hueristic, no previous move

        ThreadData1.Reset;
        ThreadData1.ID := k+1;

        for i0 := low(RepList1) to high(RepList1) do
          RepList1[i0] := 0;

        Board1 := copyBoard(Board);
        RepList1[ply1] := Board.Hash;

        DepthToken := InterLockedIncrement(DepthToken);
        SearchDepth1 := DepthToken div ThreadsPerDepth;
        LoopCount := 0;

        while (HighestDepth < MaxDepth) and (score1 <= MateScore - SearchDepth1) and (score1 >= -MateScore + SearchDepth1) and (Stop = false) and (LoopCount < MaxDepth) do        // can stop search if mate is found
          begin
          inc(LoopCount);
          PV1[0] := 0;

          if DoAspirationSearch and (SearchDepth1 > 4) then
            begin    // Do aspiration search
            Lock.Acquire;

            if BestPV[0] > 0 then
              begin
              alpha1 := max(BestPV[1].score - AspirationWindow, ScoreMinValue);
              beta1 := min(BestPV[1].score + AspirationWindow, ScoreMaxValue);
              end
             else
              begin
              alpha1 := ScoreMinValue+1;
              beta1 := ScoreMaxValue-1;
              end;

            Lock.Release;

            score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha1, beta1, PV_Node, Board1, PV1, RepList1, DoNull, PrevMove1);

            if (score1 <> InvalidScore) and (score1 <> MateScore) then
              if (score1 >= beta1) or (score1 <= alpha1) then     // had cutoff, so research full window
                begin
                //   if outside aspiration window then set ThreadData1.FirstRootMoveSearched = false;
                ThreadData1.FirstRootMoveSearched := false;

                score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha, beta, PV_Node, Board1, PV1, RepList1, DoNull, PrevMove1);
                end
            end
           else
            score1 := PVS_ABDADA(@self, @ThreadData1, ply1, SearchDepth1, alpha, beta, PV_Node, Board1, PV1, RepList1, DoNull, PrevMove1);

          NodeCount :=  NodeCount + ThreadData1.NodeCount - ThreadData1.PrevCount;
          ThreadData1.PrevCount := ThreadData1.NodeCount;

          if (score1 > ScoreMinValue) and (score1 <> InvalidScore) and (score1 <> -MateScore) and (PV1[0] > 0) then
            begin
            Lock.Acquire;
            UpdateResult1 := false;

            if BestPV[0] = 0 then      // no result yet, so save something to get started
              UpdateResult1 := true
             else
              begin

              // 1) Found a winning mate move and new score is higher than best found so far, so it is clearly best move even if search depth is less

              if (PV1[1].score >= MateScore - MaxSearchPly) and (PV1[1].score > BestPV[1].score) then
                UpdateResult1 := true;

              // 2) Searched to same depth as old best move, and score is better, so this becomes new best move

              if (PV1[0] = BestPV[0]) and (PV1[1].score > BestPV[1].score) then
                UpdateResult1 := true;

              // 3) found a move searched at a greater depth
              //    All root moves may not have been completely searched at this new depth if search truncated due to time, so this is a lower bound
              //    Don't consider as best unless first root move at this depth has been completely searched by at least one thread as well and the previous best is not a mate score
              //    If first root move not completely searched, then the found best move is sometimes much inferior to the first root move
              //    which was best move at the previous lower depth

              if (PV1[0] > BestPV[0]) and ThreadData1.FirstRootMoveSearched and (BestPV[1].score < MateScore - MaxSearchPly) then
                UpdateResult1 := true;

              if (PV1[1].score >= MateScore - MaxSearchPly) and (PV1[0] = MateScore - PV1[1].score) then    // lowest posible mate, so stop further search
                Stop := true;

              ElapsedMilliseconds := GetTickCount64 - StartTickCount;
              end;

            if UpdateResult1 = true then
              begin
              for i0 := 0 to PV1[0] do
                BestPV[i0] := PV1[i0];

              BestMove := BestPV[1];

              if DoProgressUpdates = true then
                begin
                ElapsedMilliseconds := GetTickCount64 - StartTickCount;

                if PV1[1].score >= MateScore - MaxSearchPly then
                  ProgressMessage := 'info depth ' + IntToStr(PV1[0]) {+ ' seldepth ' + IntToStr(Search_Result.selDepth)} + ' score mate ' + IntToStr( (MateScore - PV1[1].score + 1) div 2)  + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(PV1) + ' string : thread #' + IntToStr(k+1)

                 else if PV1[1].score <= -MateScore + MaxSearchPly then
                  ProgressMessage := 'info depth ' + IntToStr(PV1[0]) {+ ' seldepth ' + IntToStr(Search_Result.selDepth)} + ' score mate -' + IntToStr( (MateScore + PV1[1].score + 1) div 2) + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(PV1) + ' string : thread #' + IntToStr(k+1)

                 else
                  ProgressMessage := 'info depth ' +  IntToStr(PV1[0]) + ' score cp ' + IntToStr(PV1[1].score) + ' nodes ' + IntToStr(NodeCount) + ' time ' + IntToStr(ElapsedMilliseconds) + ' pv ' + PVToStr(PV1) + ' string : thread #' + IntToStr(k+1);

                writeln(output, AnsiString(ProgressMessage));
                flush(output);
                end;
              end;

            Lock.Release;
            end;

          Lock.Acquire;

          HighestDepth := max(HighestDepth, SearchDepth1);

          if DepthToken < (HighestDepth + 1) * ThreadsPerDepth  then
            DepthToken := (HighestDepth + 1) * ThreadsPerDepth;

          DepthToken := InterLockedIncrement(DepthToken);
          SearchDepth1 := DepthToken div ThreadsPerDepth;

          Lock.Release;

          if GetTickCount64 - StartTickCount >= TimeLimit - 2 then
            Stop := true;
          end;
        end,

        Pool);

    if workercount > 0 then
      TTask.WaitForAny(workers);

    Stop := true;                   // signal other workers to stop

    if workercount > 1 then
      TTask.WaitForAll(workers);     //  Must wait for all remaining workers to stop

    ElapsedMilliseconds := GetTickCount64 - StartTickCount;
    SearchTime := ElapsedMilliseconds;

    if BestPV[0] > 0 then
      begin
      if (BestPV[1].score >= MateScore - MaxSearchPly) or (BestPV[1].score <= -MateScore + MaxSearchPly) then
        Search_Result.Depth := MateScore - BestPV[1].score
       else
        Search_Result.Depth := BestPV[0];

      Search_Result.selDepth := selDepth;
      Search_Result.nodes := NodeCount;
      Search_Result.time := ElapsedMilliseconds;
      Search_Result.PV := BestPV;
      Search_Result.score := BestPV[1].score;
      end
     else
      begin
      Search_Result.selDepth := 0;
      Search_Result.nodes := NodeCount;
      Search_Result.time := ElapsedMilliseconds;
      Search_Result.PV := BestPV;
      Search_Result.score := -MateScore;
      end;

    if DoProgressUpdates = true then
      begin
      ProgressMessage := ResultToStr;

      writeln(output, AnsiString(' '));
      writeln(output, AnsiString(ProgressMessage + #13#10));

      if Search_Result.PV[0] > 1 then
        ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr + ' ponder ' + Search_Result.PV[2].ToStr
       else if Search_Result.PV[0] = 1 then
        ProgressMessage := 'bestmove ' + Search_Result.PV[1].ToStr
       else if Search_Result.PV[0] = 0 then
        ProgressMessage := 'bestmove (none)';

      writeln(output, AnsiString(ProgressMessage + #13#10));
      flush(output);
      end;

    Searching := false;
    ReadyToSearch := false;

    if Assigned(fOnSearchFinished) then
      fOnSearchFinished(Search_Result);

    finally
    Lock.Free;
    end;

  PrepareForNextSearch;
  end;


initialization

  if Pool = nil then
    begin
    Pool := TThreadPool.Create;
    Pool.SetMaxWorkerThreads(16);
    Pool.SetMinWorkerThreads(16);
    end;

  Book.LoadFromResource('Open_Book');

finalization

  if assigned(Pool) then
    Pool.Free;

end.




