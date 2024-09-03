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


unit GameDef;

interface

uses
  SysUtils, Classes, Math, Common;


const
  noPiece = 0;

  Pawn = 1;
  Knight = 2;
  Bishop = 3;
  Rook = 4;
  Queen = 5;
  King = 6;

  PieceValue : array[0..6] of integer =  (0, 100, 300, 300, 500, 900, 2400);

  White = 0;
  Black = 1;
  none = 2;

  InvalidBoard = UInt64($FFFFFFFFFFFFFFFF);

  ScoreMinValue = -32767;        // -2^15+1
  ScoreMaxValue = 32767;         //  2^15-1

  InvalidScore = ScoreMinValue - 1;

  RepetitionFound = 27182818;      // e
  ScoreOffset = 32768;

  whitesqr  = UInt64($AA55AA55AA55AA55);
  blacksqr = UInt64($55AA55AA55AA55AA);

  pieceNames : array[0..king] of string = ('', 'Pawn', 'Knight', 'Bishop', 'Rook', 'Queen', 'King');
  colourNamesShort : array[White..Black] of string = ('W', 'B');
  colourNamesLong : array[White..Black] of string = ('White', 'Black');

type
 THashTable = array[White..Black, Pawn..King, 0..63] of UInt64;       // 12 x 64

type
 TMove = UInt64;

type
  TPVArray = array[0..255] of TMove;

type
  TMoveArray = array[0..255] of TMove;

  // array[0]      bits 0..7   : Move Count
  // array[0]      bit 12      : unknown Result
  // array[0]      bit 13      : White Win
  // array[0]      bit 14      : Draw
  // array[0]      bit 15      : Black Win
  // array[0]      bit 16      : KingIsInCheck

  // array[1] ...   Move 1
  // array[2] ...   Move 2  etc

  //   Move information stored in UInt64 as follows:

  //   bits 0..5     : Source Cell
  //   bits 6..11    : Dest Cell
  //   bits 12..15   : Moved Piece Type
  //   bits 16..19   : Captured Piece Type
  //   bits 20..23   : Promoted Piece Type
  //   bits 24..29   : Castling Rights i.e. 6 bits indicating if 4 rooks & 2 kings have moved
  //   bits 30..35   : Enpassant Cell
  //   bits 36..43   : Half Move Count  i.e. in range [0..255]
  //   bit  44       : LMR flag used during search
  //   bits 48..63   : 16 bits available for Move value or other uses


type
  TMoveViewer = record
    FMove : Uint64;
    movstr : string;
    Piece : string;
    CapturedPiece : string;
    PromotionPiece : string;
    Score : integer;

    procedure Import(Move : TMove);
    procedure Clear;
    end;


type
  TMoveHelper = record helper for TMove
    function Source : integer;             inline;
    procedure SetSource(Cell : integer);
    function Dest : integer;               inline;
    procedure SetDest(Cell : integer);
    function Piece : integer;              inline;
    procedure SetPiece(Piece : integer);
    function CapturedPiece : integer;      inline;
    procedure SetCapturedPiece(CapturedPiece : integer);
    function PromotionPiece : integer;     inline;
    procedure SetPromotionPiece(PromotionPiece : integer);
    function MovedPieces : UInt64;
    procedure SetCastleFlags(MovedPieces : UInt64);
    function EnpassantCell : integer;      inline;
    procedure SetEnpassantCell(Cell : integer);
    function HalfMoveCount : integer;
    procedure SetHalfMoveCount(Count : integer);
    function Score : integer;              inline;
    procedure SetScore(Score : integer);
    function IsValid : boolean;            inline;
    function ToStr : string;
    end;


  {Board cells:

                    Black

  +----+----+----+----+----+----+----+----+
  |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8
  +----+----+----+----+----+----+----+----+
  |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 |  7
  +----+----+----+----+----+----+----+----+
  | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 |  6
  +----+----+----+----+----+----+----+----+
  | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 |  5
  +----+----+----+----+----+----+----+----+
  | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 |  4
  +----+----+----+----+----+----+----+----+
  | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 |  3
  +----+----+----+----+----+----+----+----+
  | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 |  2
  +----+----+----+----+----+----+----+----+
  | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 |  1
  +----+----+----+----+----+----+----+----+
     a    b    c    d    e    f    g    h

                   White                        }


type

  TBoard = record

  const
    HashTableSeed_0 = $5A5DC759;     //  best
    HashTableSeed_1 = $c2b2aeC7;     //  best

  var
    WhitePegs : UInt64;
    BlackPegs : UInt64;

    AllPegs : UInt64;        // volatile - not copied when copying board, used to speed up some functions
    PinnedPieces : UInt64;   // volatile - not copied when copying board, used to keep track of absolute pins for use in evaluation
    DefendCheck : UInt64;    // volatile - not copied when copying board, used for staged move generation
    BlockCheck : UInt64;     // volatile - not copied when copying board, used for staged move generation

    Kings : UInt64;
    Queens : UInt64;
    Rooks : UInt64;
    Bishops : UInt64;
    Knights : UInt64;
    Pawns : UInt64;

    EnPassant : UInt64;

    ToPlay : integer;

    MovedPieces : UInt64;   // used to keep track of castling rights
    GameStage : integer;

    TurnNumber : integer;
    HalfCount : integer;

    Hash : UInt64;
    PawnHash : UInt64;

  class var
    HashTable : THashTable;
    PlayerHash : UInt64;
    CastleHash : array[0..63] of UInt64;
    epHash : array[0..63] of UInt64;

    RookHorzMask : array[0..63] of UInt64;
    RookVertMask : array[0..63] of UInt64;
    RookLookup : array[0..7, 0..127] of byte;
    RookMask : array[0..63] of UInt64;

    BishopUpMask : array[0..63] of UInt64;
    BishopDownMask : array[0..63] of UInt64;
    BishopUpLookup : array[0..63, 0..127] of byte;
    BishopDownLookup : array[0..63, 0..127] of byte;
    BishopMask : array[0..63] of UInt64;

    KnightMask : array[0..63] of UInt64;
    KingMask : array[0..63] of UInt64;

    WhitePawnMoves : array[0..63] of UInt64;
    WhitePawnCaptures : array[0..63] of UInt64;
    BlackPawnMoves : array[0..63] of UInt64;
    BlackPawnCaptures : array[0..63] of UInt64;

    IsInitialized : array[0..0] of integer;       // 0 = uninitialized, -1 = initialized

  procedure Reset;
  procedure ClearBoard;
  procedure CalcHash;
  procedure CalcPawnHash;
  procedure CalcGameStage;

  function MakeMove(var Move : UInt64) : boolean;
  function UndoMove(const Move : UInt64) : boolean;

  function MakeMoveNoHash(var Move : UInt64) : boolean;
  function UndoMoveNoHash(const Move : UInt64) : boolean;

  function GetMoveHash(var Move : UInt64) : UInt64;

  function GetAllValidMoves(Player : integer; var moves : TMoveArray; ViolentOnly : boolean) : integer;
  function GetCapturePromotionAndCheckingMoves(Player : integer; var moves : TMoveArray) : integer;
  function GetCaptureMove(TargetCell : integer) : TMove;

  function GetCellValidMoves(Player : integer; CellIndex : integer) : UInt64;
  function GetPinnedPegs(PlayerPegs : UInt64) : UInt64;
  procedure DiscardPinnedMoves(CellIndex : integer; var CaptureMoves, QuietMoves : UInt64);
  function NonPinnedMoves(CellIndex : integer) : UInt64;

  function CountValidMoves(Player : integer) : integer;

  procedure FillHashTables;
  procedure MakeRookMasks;
  procedure MakeBishopMasks;
  procedure MakeKnightMasks;
  procedure MakeKingMasks;
  procedure MakePawnMasks;
  procedure MakeRookLookUp;
  procedure MakeBishopLookUp;

  function RookAttack(Cell : integer) : UInt64;
  function RookAttack_asm(Cell : integer): UInt64;
  function BishopAttack(Cell : integer) : UInt64;
  function BishopAttack_asm(Cell : integer) : UInt64;

  function QueenAttack(Cell : integer) : UInt64;
  function KnightAttack(Cell : integer) : UInt64;
  function PawnAttack(Cell : integer) : UInt64;
  function KingAttack(Cell : integer) : UInt64;

  function MobilityBoard(Player : integer) : UInt64;

  function RookMobility(Player : integer; MobBoard : UInt64) : integer;
  function BishopMobility(Player : integer; MobBoard : UInt64) : integer;
  function QueenMobility(Player : integer; MobBoard : UInt64) : integer;
  function KnightMobility(Player : integer; MobBoard : UInt64) : integer;
  function KingMobility(Player : integer; MobBoard : UInt64) : integer;

  function KingInCheck(Player : integer; var CheckCaptureMask, CheckBlockMask : UInt64) : boolean;    overload;
  function KingInCheck(Player : integer) : boolean;  overload;
  function AdjacentKings : boolean;
  function IsMoveLegal(Move : TMove) : boolean;

  function MoveExists(var InCheck : boolean) : boolean;
  function KingOnly(Player : integer) : boolean;
  function PawnsOnly(Player : integer) : boolean;
  function OnlyPawns : boolean;

  function CellAttacked(Player, Cell : integer) : boolean;
  function EnpassantLegal(ToPlay : integer; Dest, Source : UInt64) : boolean;

  function SEE(Move : TMove) : integer;

  function LargestPieceValue(Player : integer) : integer;

  procedure RookMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  procedure BishopMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  procedure QueenMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  procedure KnightMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  procedure KingMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  procedure KingMoves_InCheck(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  procedure PawnMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);

  function CastleKingSide(Player : integer) : boolean;
  function CastleQueenSide(Player : integer) : boolean;

  function GetColor(CellIndex : integer) : integer;

  function GetPiece(CellIndex : integer) : integer;
  function GetPiece_asm(CellIndex : integer) : integer;

  procedure FlipColor;

  end;

procedure BoardFromFEN(const FEN : string; var Board : TBoard);
procedure BoardToFen(var FEN : string; const Board : TBoard);

function MovesToStr(moves : TMoveArray) : string;
function MoveToStr(Move : UInt64) : string;
function PVToStr(PV : TMoveArray) : string;

function CopyBoard(const Board : TBoard) : TBoard;
function IsBoardLegal(const Board : TBoard) : boolean;

function ItemCount(const MoveArray : TMoveArray) : integer;
procedure ClearList(var MoveArray : TMoveArray);
procedure AddToList(var MoveArray : TMoveArray; Move : TMove);
function RemoveFromList(var MoveArray : TMoveArray) : TMove;


implementation

uses
  Eval;

{$CODEALIGN 16}

procedure TMoveViewer.Import(Move : TMove);
  begin
  FMove := Move;
  movstr := Move.ToStr;
  piece := PieceNames[Move.Piece];
  capturedPiece := PieceNames[Move.CapturedPiece];
  PromotionPiece := PieceNames[Move.PromotionPiece];
  Score := Move.Score;
  end;

procedure TMoveViewer.Clear;
  begin
  FMove := 0;
  movstr := '';
  piece := '';
  capturedPiece := '';
  PromotionPiece := '';
  Score := 0;
  end;

function TMoveHelper.Source : integer;
  begin
  result := integer(self and $3F);
  end;


procedure TMoveHelper.SetSource(Cell : integer);
  begin
  self := UInt64(self and UInt64($FFFFFFFFFFFFFFC0)) + UInt64(Cell and $3F);
  end;


function TMoveHelper.Dest : integer;
  begin
  result := integer((self shr 6) and $3F);
  end;


procedure TMoveHelper.SetDest(Cell : integer);
  begin
  self := UInt64(self and $FFFFFFFFFFFFF03F) + UInt64(Cell and $3F) shl 6;
  end;


function TMoveHelper.Piece : integer;
  begin
  result := integer((self shr 12) and $F);
  end;


procedure TMoveHelper.SetPiece(Piece : integer);
  begin
  self := UInt64(self and $FFFFFFFFFFFF0FFF) + UInt64(Piece and $F) shl 12;
  end;


function TMoveHelper.CapturedPiece : integer;
  begin
  result := integer((self shr 16) and $F);
  end;


procedure TMoveHelper.SetCapturedPiece(CapturedPiece : integer);
  begin
  self := UInt64(self and $FFFFFFFFFFF0FFFF) + UInt64(CapturedPiece and $F) shl 16;
  end;


function TMoveHelper.PromotionPiece : integer;
  begin
  result := integer((self shr 20) and $F);
  end;


procedure TMoveHelper.SetPromotionPiece(PromotionPiece : integer);
  begin
  self := UInt64(self and $FFFFFFFFFF0FFFFF) + UInt64(PromotionPiece and $F) shl 20;
  end;


function TMoveHelper.MovedPieces : UInt64;
  var
    Moved : UInt64;

  begin
  Moved := (self shr 24) and $3F;
  result := PDEP(Moved, UInt64($9100000000000091));
  end;


procedure TMoveHelper.SetCastleFlags(MovedPieces : UInt64);
  var
    Moved : UInt64;

  begin
  Moved := PEXT(MovedPieces, UInt64($9100000000000091));
  self := UInt64(self and $FFFFFFFFC0FFFFFF) + (Moved and $3F) shl 24;
  end;


function TMoveHelper.EnpassantCell : integer;
  begin
  result := integer(UInt64(self shr 30) and $3F);
  end;


procedure TMoveHelper.SetEnpassantCell(Cell : integer);
  begin
  self := UInt64(self and $FFFFFFF03FFFFFFF) + UInt64(Cell and $3F) shl 30;
  end;


function TMoveHelper.HalfMoveCount : integer;
  begin
  result := integer(UInt64(self shr 36) and $FF);
  end;


procedure TMoveHelper.SetHalfMoveCount(Count : integer);
  begin
  self := UInt64(self and $FFFFF00FFFFFFFFF) + UInt64(Count and $FF) shl 36;
  end;


function TMoveHelper.Score : integer;
  begin
  result := integer(UInt64(self shr 48) and $FFFF) - ScoreOffset;
  end;


procedure TMoveHelper.SetScore(Score : integer);
  begin
  Score := Score + ScoreOffset;
  self := UInt64(self and $0000FFFFFFFFFFFF) + UInt64(Score and $FFFF) shl 48;
  end;


function TMoveHelper.IsValid : boolean;
  begin
  result := UInt64(self and $3F) <> (UInt64(self shr 6) and $3F);
  end;


function TMoveHelper.ToStr : string;
  var
    source, Dest, FileNo, Rank, PromotionPiece : integer;

  begin
  Source := Self and $3F;
  FileNo := Source mod 8;
  Rank := 7 - (Source div 8);

  result := Char(FileNo + Ord(Char('a'))) + IntToSTr(Rank + 1);

  Dest :=  (Self shr 6) and $3F;
  FileNo := Dest mod 8;
  Rank := 7 - (Dest div 8);

  result :=   result + Char(FileNo + Ord(Char('a'))) + IntToSTr(Rank + 1);

  PromotionPiece := Self.PromotionPiece;
    case PromotionPiece of
    Knight : result :=  result + 'n';
    Bishop : result :=  result + 'b';
    Rook : result :=  result + 'r';
    Queen : result :=  result + 'q';
    end;
  end;


function ItemCount(const MoveArray : TMoveArray) : integer;
  begin
  result := MoveArray[0] and $FF;       // higher bits may be used for other purposes
  end;


procedure ClearList(var MoveArray : TMoveArray);
  begin
  MoveArray[0] := 0;
  end;


procedure AddToList(var MoveArray : TMoveArray; Move : TMove);
  var
    MoveCount, i: integer;

  begin
  MoveCount := MoveArray[0] and $FF;    // max number of moves is 255
  inc(MoveCount);

  if MoveCount > 255 then     // max number of moves is 255
    begin
    for i := 1 to 254 do
      MoveArray[i] := MoveArray[i+1];

    MoveCount := min(MoveCount, 255);
    end;

  MoveArray[MoveCount] := Move;
  MoveArray[0] := (MoveArray[0] and $FF00) or MoveCount;
  end;


function RemoveFromList(var MoveArray : TMoveArray) : TMove;
  var
    MoveCount : UInt32;

  begin
  result := 0;
  MoveCount := MoveArray[0] and $FF;

  if MoveCount > 0 then
    begin
    result := MoveArray[MoveCount];
    dec(MoveCount);
    MoveArray[0] := (MoveArray[0] and $FF00) or MoveCount;
    end;
  end;


procedure TBoard.CalcGameStage;
  begin
  GameStage := min(64, BitCount(Queens)*10 + BitCount(Rooks)*5 + BitCount(Bishops)*3 + BitCount(Knights)*3);
  end;


function TBoard.GetColor(CellIndex : integer) : integer;
  begin
  if GetBit(WhitePegs, CellIndex) = true then
    exit(White);
  if GetBit(BlackPegs, CellIndex) = true then
    exit(Black);
  Result := none;
  end;


procedure TBoard.FlipColor;
  var
    Temp : UInt64;

  begin
  Kings := Flip(Kings);
  Queens := Flip(Queens);
  Rooks := Flip(Rooks);
  Bishops := Flip(Bishops);
  Knights := Flip(Knights);
  Pawns := Flip(Pawns);

  Temp := WhitePegs;
  WhitePegs := Flip(BlackPegs);
  BlackPegs := Flip(Temp);

  ToPlay := (1 - ToPlay);

  EnPassant := Flip(EnPassant);
  MovedPieces := Flip(MovedPieces);

  PawnHash :=  PawnHash xor PlayerHash;    // needed to ensure pawn evaluation remains consistent
  end;


function TBoard.GetPiece(CellIndex : integer) : integer;
  var
    t : UInt64;

  begin
  t := Uint64($1) shl CellIndex;

  if (t and pawns) <> 0 then
    exit(pawn);

  if (t and Rooks) <> 0 then
    exit(Rook);

  if (t and Bishops) <> 0 then
    exit(Bishop);

  if (t and Knights) <> 0 then
    exit(Knight);

  if (t and Queens) <> 0 then
    exit(Queen);

  if (t and Kings) <> 0 then
    exit(King);

  result := 0;
  end;


function TBoard.GetPiece_asm(CellIndex : integer) : integer;
  asm
  xor rax, rax
  mov r9, $1
  shlx r8, r9, rdx

  mov eax, Pawn
  test Self.Pawns, r8
  JNZ @exit

  mov eax, Rook
  test Self.Rooks, r8
  JNZ @exit

  mov eax, Bishop
  test Self.Bishops, r8
  JNZ @exit

  mov eax, Knight
  test Self.Knights, r8
  JNZ @exit

  mov eax, Queen
  test Self.Queens, r8
  JNZ @exit

  mov eax, King
  test Self.Kings, r8
  JNZ @exit

  xor eax, eax

  @exit:
  end;


function TBoard.CountValidMoves(Player : integer) : integer;        // primariliy used for perft testing
  var
    SourcePegs, PlayerPegs, QuietMoves, CaptureMoves, DefendCheckMask, BlockCheckMask, DestPeg, EnpassantCapture, AllMoves : UInt64;
    sourceCell, destCell, CapturedPiece : integer;
    QuietMoveCount, CaptureMoveCount : integer  ;
    MoveValidFlag, KingInCheckFlag : boolean;

  begin
  QuietMoveCount := 0;
  CaptureMoveCount := 0;

  AllPegs := WhitePegs or BlackPegs;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  KingInCheckFlag := KingInCheck(Player, DefendCheckMask, BlockCheckMask);
  if KinginCheckFlag = true then
    begin
    SourceCell := GetLowBit_Alt(Kings and PlayerPegs);
    KingMoves_InCheck(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);    // Get king moves

    CaptureMoveCount := CaptureMoveCount + BitCount(CaptureMoves);
    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves);

    if BitCount(DefendCheckMask) > 1 then         // This is double check, therefore only legal moves are king moves
      exit(CaptureMoveCount + QuietMoveCount);
    end;

  // find pin information

  PinnedPieces := GetPinnedPegs(PlayerPegs);             // Stores result in Board.PinnedPieces field, may be used later during eval i.e. pinned piece penalty

  SourcePegs := Rooks and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    RookMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoveCount := CaptureMoveCount + BitCount(CaptureMoves and DefendCheckMask);
    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves and BlockCheckMask);
    end;

  SourcePegs := Bishops and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    BishopMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoveCount := CaptureMoveCount + BitCount(CaptureMoves and DefendCheckMask);
    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves and BlockCheckMask);
    end;

  SourcePegs := Knights and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    AllMoves :=  KnightMask[SourceCell];
    QuietMoves := AllMoves and not (AllPegs);
    CaptureMoves := AllMoves and (AllPegs xor PlayerPegs);

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoveCount := CaptureMoveCount + BitCount(CaptureMoves and DefendCheckMask);
    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves and BlockCheckMask);
    end;

  SourcePegs := Queens and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    QueenMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoveCount := CaptureMoveCount + BitCount(CaptureMoves and DefendCheckMask);
    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves and BlockCheckMask);
    end;

  if KingInCheckFlag = false then
    begin
    SourcePegs := Kings and PlayerPegs;
    SourceCell := PopLowBit_Alt(SourcePegs);

    KingMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    CaptureMoveCount := CaptureMoveCount + BitCount(CaptureMoves and DefendCheckMask);
    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves and BlockCheckMask);
    end;

  SourcePegs := Pawns and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    PawnMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    if CaptureMoves <> 0 then   // Must test for case where enpassant capture eliminates the checking pawn
      begin
      EnpassantCapture := CaptureMoves and not AllPegs;

      if EnpassantCapture <> 0 then
        begin
        if Player = White then
          begin
          if (EnPassantCapture shl 8) and DefendCheckMask <> 0 then   // Capture Moves for Pawn enpassant move does not match the DefendCheckMask
            DefendCheckMask := DefendCheckMask or EnpassantCapture;
          end
         else
          begin
          if (EnPassantCapture shr 8) and DefendCheckMask <> 0 then
            DefendCheckMask := DefendCheckMask or EnpassantCapture;
          end;
        end;
      end;

    CaptureMoves := CaptureMoves and DefendCheckMask;

    DestPeg := CaptureMoves and UInt64($FF000000000000FF);
    while DestPeg <> 0 do                        // Pawn Promotion with Capture
      begin
      DestCell := PopLowBit_Alt(DestPeg);
      CaptureMoveCount := CaptureMoveCount + 4;
      ClearBit(CaptureMoves, DestCell);
      end;

    while CaptureMoves <> 0 do
      begin
      MoveValidFlag := true;
      DestCell := PopLowBit_Alt(CaptureMoves);
      CapturedPiece := GetPiece_asm(DestCell);
      if CapturedPiece = 0 then          // handle enpassant capture
        MoveValidFlag := EnpassantLegal(Player, DestCell, SourceCell);   // check enpassant move is legal & not putting king in check

      if MoveValidFlag = true then
        inc(CaptureMoveCount);
      end;

    QuietMoves := QuietMoves and BlockCheckMask;

    DestPeg := QuietMoves and UInt64($FF000000000000FF);
    if DestPeg <> 0 then                         // Pawn Promotion without Capture
      begin                                      // Generate all 4 alternative moves
      DestCell := PopLowBit_Alt(DestPeg);
      QuietMoveCount := QuietMoveCount + 4;
      ClearBit(QuietMoves, DestCell);
      end;

    QuietMoveCount := QuietMoveCount + BitCount(QuietMoves);
    end;

  result := CaptureMoveCount + QuietMoveCount;
  end;

// =====================================================================================================================================

function TBoard.GetCapturePromotionAndCheckingMoves(Player : integer; var moves : TMoveArray) : integer;
  // result is number of valid moves, all valid moves are contained in 'moves' array

  var
    SourcePegs, PlayerPegs, QuietMoves, CaptureMoves, DefendCheckMask, BlockCheckMask, DestPeg, EnpassantCapture, AllMoves : UInt64;
    sourceCell, destCell, temp, i, CapturedPiece, OpponentKingCell : integer;
    MoveValidFlag, InCheckFlag : boolean;
    DiscoveredCheckPegs : UInt64;

  begin
  result := 1;

  AllPegs := WhitePegs or BlackPegs;

  if Player = White then
    begin
    PlayerPegs := WhitePegs;
    OpponentKingCell := GetLowBit_Alt(Kings and BlackPegs);
    end
   else
    begin
    PlayerPegs := BlackPegs;
    OpponentKingCell := GetLowBit_Alt(Kings and WhitePegs);
    end;

  InCheckFlag := KingInCheck(Player, DefendCheckMask, BlockCheckMask);
  if InCheckFlag = true then
    begin
    SourceCell := GetLowBit_Alt(Kings and PlayerPegs);
    KingMoves_InCheck(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);    // Get king moves

    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12);
      inc(result);
      end;

    if BitCount(DefendCheckMask) > 1 then         // This is double check, therefore only legal moves are king moves
      begin
      dec(result);
      Moves[0] := result;
      exit;
      end;
    end
   else
    begin
    DefendCheckMask := UInt64($FFFFFFFFFFFFFFFF);
    BlockCheckMask := UInt64($FFFFFFFFFFFFFFFF);
    end;

  // find pin information

  PinnedPieces := GetPinnedPegs(PlayerPegs);        // Stores result in Board.PinnedPieces field, may be used later during eval i.e. pinned piece penalty

  DiscoveredCheckPegs := GetPinnedPegs(PlayerPegs xor AllPegs) and PlayerPegs;

  SourcePegs := Queens and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);

    QueenMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoves := CaptureMoves and DefendCheckMask;
    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Queen) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    if DiscoveredCheckPegs and (UInt64($1) shl sourceCell) = 0 then
      QuietMoves := (RookAttack_asm(OpponentKingCell) or BishopAttack_asm(OpponentKingCell)) and QuietMoves and BlockCheckMask;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Queen) shl 12);
      inc(result);
      end;
    end;

  SourcePegs := Rooks and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    RookMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoves := CaptureMoves and DefendCheckMask;
    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Rook) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    if DiscoveredCheckPegs and (UInt64($1) shl sourceCell) = 0 then
      QuietMoves := RookAttack_asm(OpponentKingCell) and QuietMoves and BlockCheckMask;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Rook) shl 12);
      inc(result);
      end;
    end;

  SourcePegs := Bishops and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    BishopMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    CaptureMoves := CaptureMoves and DefendCheckMask;
    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Bishop) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    if DiscoveredCheckPegs and (UInt64($1) shl sourceCell) = 0 then
      QuietMoves := BishopAttack_asm(OpponentKingCell) and QuietMoves and BlockCheckMask;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Bishop) shl 12);
      inc(result);
      end;
    end;

  SourcePegs := Knights and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    AllMoves :=  KnightMask[SourceCell];
    QuietMoves := AllMoves and not (AllPegs);
    CaptureMoves := AllMoves and (AllPegs xor PlayerPegs);

    if PinnedPieces and (UInt64($1) shl sourceCell) = 0 then    // can only move knight if not pinned
      begin
      CaptureMoves := CaptureMoves and DefendCheckMask;
      while CaptureMoves <> 0 do
        begin
        DestCell := PopLowBit_Alt(CaptureMoves);
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Knight) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
        inc(result);
        end;

      if DiscoveredCheckPegs and (UInt64($1) shl sourceCell) = 0 then
        QuietMoves := KnightAttack(OpponentKingCell) and QuietMoves and BlockCheckMask;

      while QuietMoves <> 0 do
        begin
        DestCell := PopLowBit_Alt(QuietMoves);
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Knight) shl 12);
        inc(result);
        end;
      end;
    end;

  if InCheckFlag = false then
    begin
    SourcePegs := Kings and PlayerPegs;
    SourceCell := PopLowBit_Alt(SourcePegs);

    KingMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;
    end;

  SourcePegs := Pawns and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    PawnMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    if CaptureMoves <> 0 then   // Must test for case where enpassant capture eliminates the checking pawn
      begin
      EnpassantCapture := CaptureMoves and not AllPegs;

      if EnpassantCapture <> 0 then
        begin
        if Player = White then
          begin
          if (EnPassantCapture shl 8) and DefendCheckMask <> 0 then   // Capture Moves for Pawn enpassant move does not match the DefendCheckMask
            DefendCheckMask := DefendCheckMask or EnpassantCapture;
          end
         else
          begin
          if (EnPassantCapture shr 8) and DefendCheckMask <> 0 then
            DefendCheckMask := DefendCheckMask or EnpassantCapture;
          end;
        end;
      end;

    CaptureMoves := CaptureMoves and DefendCheckMask;

    DestPeg := CaptureMoves and UInt64($FF000000000000FF);
    while DestPeg <> 0 do                        // Pawn Promotion with Capture
      begin
      DestCell := PopLowBit_Alt(DestPeg);
      temp := GetPiece_asm(DestCell);
      for i := 2 to 5 do                         // Generate all 4 alternative moves
        begin
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(temp) shl 16) or (UInt64(i) shl 20);
        inc(result);
        end;

      ClearBit(CaptureMoves, DestCell);
      end;

    while CaptureMoves <> 0 do
      begin
      MoveValidFlag := true;
      DestCell := PopLowBit_Alt(CaptureMoves);
      CapturedPiece := GetPiece_asm(DestCell);

      if CapturedPiece = 0 then            // handle enpassant capture
        begin
        CapturedPiece := Pawn;
        MoveValidFlag := EnpassantLegal(Player, DestCell, SourceCell);   // check enpassant move is legal & not putting king in check
        end;

      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(CapturedPiece) shl 16);
      if MoveValidFlag = true then
        inc(result);
      end;

    QuietMoves := QuietMoves and BlockCheckMask;

    DestPeg := QuietMoves and UInt64($FF000000000000FF);
    if DestPeg <> 0 then                         // Pawn Promotion without Capture
      begin                                      // Generate all 4 alternative moves
      DestCell := PopLowBit_Alt(DestPeg);
      for i := Queen downto Knight do
        begin
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(i) shl 20);
        inc(result);
        end;

      ClearBit(QuietMoves, DestCell);
      end;

    if DiscoveredCheckPegs and (UInt64($1) shl sourceCell) = 0 then
      QuietMoves := PawnAttack(OpponentKingCell) and QuietMoves;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12);
      inc(result);
      end;

    end;

  dec(result);

  Moves[0] := result;          // stores movecount in Moves[0], first move is Moves[1]
  end;

//=======================================================================================================

function TBoard.GetAllValidMoves(Player : integer; var moves : TMoveArray; ViolentOnly : boolean) : integer;
  // result is number of valid moves, all valid moves are contained in 'moves' array

  var
    SourcePegs, PlayerPegs, QuietMoves, CaptureMoves, DestPeg, EnpassantCapture, AllMoves : UInt64;
    sourceCell, destCell, temp, i, CapturedPiece : integer;
    MoveValidFlag, InCheckFlag : boolean;
    OpponentPegs : UInt64;

  begin
  result := 1;

  AllPegs := WhitePegs or BlackPegs;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  OpponentPegs := PlayerPegs xor AllPegs;

  InCheckFlag := KingInCheck(Player, DefendCheck, BlockCheck);
  if InCheckFlag = true then
    begin
    SourceCell := GetLowBit_Alt(Kings and PlayerPegs);
    KingMoves_InCheck(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);    // Get king moves

    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12);
      inc(result);
      end;

    if BitCount(DefendCheck) > 1 then         // This is double check, therefore only legal moves are king moves
      begin
      dec(result);
      Moves[0] := result;
      exit;
      end;
    end
   else
    begin
    DefendCheck := UInt64($FFFFFFFFFFFFFFFF);
    if ViolentOnly = true then
      BlockCheck := UInt64(0)
     else
      BlockCheck := UInt64($FFFFFFFFFFFFFFFF);
    end;

  // find pin information

  PinnedPieces := GetPinnedPegs(PlayerPegs);       // Stores result in Board.PinnedPieces field

  SourcePegs := Queens and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);

    AllMoves := RookAttack_asm(SourceCell) or BishopAttack_asm(SourceCell);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      AllMoves := AllMoves and NonPinnedMoves(SourceCell);

    CaptureMoves := AllMoves and OpponentPegs and DefendCheck;
    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Queen) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    QuietMoves := AllMoves and not AllPegs and BlockCheck;
    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Queen) shl 12);
      inc(result);
      end;
    end;


  SourcePegs := Rooks and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);

    AllMoves := RookAttack_asm(SourceCell);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      AllMoves := AllMoves and NonPinnedMoves(SourceCell);

    CaptureMoves := AllMoves and OpponentPegs and DefendCheck;
    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Rook) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    QuietMoves := AllMoves and not AllPegs and BlockCheck;
    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Rook) shl 12);
      inc(result);
      end;
    end;


  SourcePegs := Bishops and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);

    AllMoves :=   BishopAttack_asm(SourceCell);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      AllMoves := AllMoves and NonPinnedMoves(SourceCell);

    CaptureMoves := AllMoves and OpponentPegs and DefendCheck;
    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Bishop) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    QuietMoves := AllMoves and not AllPegs and BlockCheck;
    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Bishop) shl 12);
      inc(result);
      end;
    end;


  SourcePegs := Knights and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    if PinnedPieces and (UInt64($1) shl sourceCell) = 0 then    // can only move knight if not pinned
      begin
      AllMoves :=  KnightMask[SourceCell];

      CaptureMoves := AllMoves and OpponentPegs and DefendCheck;
      while CaptureMoves <> 0 do
        begin
        DestCell := PopLowBit_Alt(CaptureMoves);
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Knight) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
        inc(result);
        end;

      QuietMoves := AllMoves and not (AllPegs) and BlockCheck;
      while QuietMoves <> 0 do
        begin
        DestCell := PopLowBit_Alt(QuietMoves);
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Knight) shl 12);
        inc(result);
        end;
      end;
    end;


  if InCheckFlag = false then
    begin
    SourcePegs := Kings and PlayerPegs;
    SourceCell := PopLowBit_Alt(SourcePegs);

    KingMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12) or (UInt64(GetPiece_asm(DestCell)) shl 16);
      inc(result);
      end;

    QuietMoves := QuietMoves and BlockCheck;
    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12);
      inc(result);
      end;
    end;


  SourcePegs := Pawns and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    PawnMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    if CaptureMoves <> 0 then   // Must test for case where enpassant capture eliminates the checking pawn
      begin
      EnpassantCapture := CaptureMoves and not AllPegs;

      if EnpassantCapture <> 0 then
        begin
        if Player = White then
          begin
          if (EnPassantCapture shl 8) and DefendCheck <> 0 then   // Capture Moves for Pawn enpassant move does not match the DefendCheckMask
            DefendCheck := DefendCheck or EnpassantCapture;
          end
         else
          begin
          if (EnPassantCapture shr 8) and DefendCheck <> 0 then
            DefendCheck := DefendCheck or EnpassantCapture;
          end;
        end;
      end;

    CaptureMoves := CaptureMoves and DefendCheck;

    DestPeg := CaptureMoves and UInt64($FF000000000000FF);
    while DestPeg <> 0 do                        // Pawn Promotion with Capture
      begin
      DestCell := PopLowBit_Alt(DestPeg);
      temp := GetPiece_asm(DestCell);
      for i := 2 to 5 do                         // Generate all 4 alternative moves
        begin
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(temp) shl 16) or (UInt64(i) shl 20);
        inc(result);
        end;

      ClearBit(CaptureMoves, DestCell);
      end;

    while CaptureMoves <> 0 do
      begin
      MoveValidFlag := true;
      DestCell := PopLowBit_Alt(CaptureMoves);
      CapturedPiece := GetPiece_asm(DestCell);

      if CapturedPiece = 0 then            // handle enpassant capture
        begin
        CapturedPiece := Pawn;
        MoveValidFlag := EnpassantLegal(Player, DestCell, SourceCell);   // check enpassant move is legal & not putting king in check
        end;

      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(CapturedPiece) shl 16);
      if MoveValidFlag = true then
        inc(result);
      end;

    if InCheckFlag = true then                     // Note this test to ensure promotions included in Quiecent Search
      QuietMoves := QuietMoves and BlockCheck;

    DestPeg := QuietMoves and UInt64($FF000000000000FF);
    if DestPeg <> 0 then                         // Pawn Promotion without Capture
      begin                                      // Generate all 4 alternative moves
      DestCell := PopLowBit_Alt(DestPeg);
      for i := Queen downto Knight do
        begin
        Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(i) shl 20);
        inc(result);
        end;

      ClearBit(QuietMoves, DestCell);
      end;

    QuietMoves := QuietMoves and BlockCheck;

    while QuietMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(QuietMoves);
      Moves[result] := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(Pawn) shl 12);
      inc(result);
      end;

    end;

  dec(result);

  Moves[0] := result;          // stores movecount in Moves[0], first move is Moves[1]
  end;


function TBoard.GetCaptureMove(TargetCell : integer) : TMove;
  // return a move capturing a piece on the target cell by the lowest attacking piece; 0 if no capture possible

  var
    SourcePegs, PlayerPegs, QuietMoves, CaptureMoves, DefendCheckMask, BlockCheckMask, DestPeg, EnpassantCapture : UInt64;
    sourceCell, destCell, temp, CapturedPiece : integer;
    MoveValidFlag, InCheckFlag : boolean;
    TargetPeg  : UInt64;

  begin
  AllPegs := WhitePegs or BlackPegs;

  if ToPlay = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  CapturedPiece := GetPiece_asm(TargetCell);
  TargetPeg := (UInt64($1) shl TargetCell);

  InCheckFlag := KingInCheck(ToPlay, DefendCheckMask, BlockCheckMask);
  if InCheckFlag = true then
    begin
    SourceCell := GetLowBit_Alt(Kings and PlayerPegs);
    KingMoves_InCheck(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);    // Get king moves

    CaptureMoves := CaptureMoves and TargetPeg;

    while CaptureMoves <> 0 do
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      result := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12) or (UInt64(CapturedPiece) shl 16);
      exit(result);
      end;
    end
   else
    DefendCheckMask := UInt64($FFFFFFFFFFFFFFFF);

  // find pin information

  PinnedPieces := GetPinnedPegs(PlayerPegs);

  // try pawn first

  if PlayerPegs = WhitePegs then
    SourcePegs := Pawns and PlayerPegs and BlackPawnCaptures[TargetCell]
   else
    SourcePegs := Pawns and PlayerPegs and WhitePawnCaptures[TargetCell];

  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    CaptureMoves := TargetPeg and DefendCheckMask;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      CaptureMoves := CaptureMoves and NonPinnedMoves(SourceCell);

    if CaptureMoves and UInt64($FF000000000000FF) <> 0 then    // Pawn Promotion with Capture
      begin                                                    // assume promotion piece is queen
      result := UInt64(SourceCell) or (UInt64(TargetCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(CapturedPiece) shl 16) or (UInt64(Queen) shl 20);
      exit(result);
      end;

    if CaptureMoves <> 0 then
      begin
      result := UInt64(SourceCell) or (UInt64(TargetCell) shl 6) or (UInt64(Pawn) shl 12) or (UInt64(CapturedPiece) shl 16);
      exit(result);
      end;
    end;


  SourcePegs := Knights and PlayerPegs and KnightMask[TargetCell];
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    if PinnedPieces and (UInt64($1) shl sourceCell) = 0 then    // can only move knight if not pinned
      begin
      if  (DefendCheckMask and TargetPeg) <> 0 then
        begin
        result := UInt64(SourceCell) or (UInt64(TargetCell) shl 6) or (UInt64(Knight) shl 12) or (CapturedPiece shl 16);
        exit(result);
        end;
      end;
    end;

  SourcePegs := Bishops and PlayerPegs and BishopAttack_asm(TargetCell);
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    CaptureMoves := TargetPeg and DefendCheckMask;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      CaptureMoves := CaptureMoves and NonPinnedMoves(SourceCell);

    if CaptureMoves <> 0 then
      begin
      result := UInt64(SourceCell) or (UInt64(TargetCell) shl 6) or (UInt64(Bishop) shl 12) or (CapturedPiece shl 16);
      exit(result);
      end;
    end;

  SourcePegs := Rooks and PlayerPegs and RookAttack_asm(TargetCell);
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    CaptureMoves := TargetPeg and DefendCheckMask;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      CaptureMoves := CaptureMoves and NonPinnedMoves(SourceCell);

    if CaptureMoves <> 0 then
      begin
      result := UInt64(SourceCell) or (UInt64(TargetCell) shl 6) or (UInt64(Rook) shl 12) or (CapturedPiece shl 16);
      exit(result);
      end;
    end;

  SourcePegs := Queens and PlayerPegs and (BishopAttack_asm(TargetCell) or RookAttack_asm(TargetCell));
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    CaptureMoves := TargetPeg and DefendCheckMask;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      CaptureMoves := CaptureMoves and NonPinnedMoves(SourceCell);

    if CaptureMoves <> 0 then
      begin
      result := UInt64(SourceCell) or (UInt64(TargetCell) shl 6) or (UInt64(Queen) shl 12) or (CapturedPiece shl 16);
      exit(result);
      end;
    end;


  // try king moves last

  if InCheckFlag = false then
    begin
    SourcePegs := Kings and PlayerPegs;
    SourceCell := PopLowBit_Alt(SourcePegs);

    KingMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
    CaptureMoves := CaptureMoves and TargetPeg;

    if CaptureMoves <> 0 then
      begin
      DestCell := PopLowBit_Alt(CaptureMoves);
      result := UInt64(SourceCell) or (UInt64(DestCell) shl 6) or (UInt64(King) shl 12) or (UInt64(CapturedPiece) shl 16);
      exit(result);
      end;
    end;

  result := 0;
  end;


function TBoard.MoveExists(var InCheck : boolean) : boolean;
  var
    SourcePegs, PlayerPegs, QuietMoves, CaptureMoves, DefendCheckMask, BlockCheckMask, EnpassantCapture, AllMoves : UInt64;
    sourceCell, destCell, CapturedPiece : integer;
    MoveValidFlag : boolean;

  begin
  AllPegs := WhitePegs or BlackPegs;

  if ToPlay = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  InCheck := KingInCheck(ToPlay, DefendCheckMask, BlockCheckMask);
  if InCheck = true then
    begin
    SourceCell := GetLowBit_Alt(Kings and PlayerPegs);
    KingMoves_InCheck(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);    // Get king moves

    if CaptureMoves <> 0 then
      exit(true);

    if QuietMoves <> 0 then
      exit(true);

    if BitCount(DefendCheckMask) > 1 then         // This is double check, therefore the only legal moves are king moves
      exit(false);
    end
   else
    begin
    DefendCheckMask := UInt64($FFFFFFFFFFFFFFFF);
    BlockCheckMask := UInt64($FFFFFFFFFFFFFFFF);
    end;

  // find pin information

  PinnedPieces := GetPinnedPegs(PlayerPegs);             // Stores result in Board.PinnedPieces field

  SourcePegs := Knights and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    AllMoves :=  KnightMask[SourceCell] and not PlayerPegs;

    if PinnedPieces and (UInt64($1) shl sourceCell) = 0 then    // can only move knight if not pinned
      if AllMoves and (DefendCheckMask or BlockCheckMask) <> 0  then
        exit(true);
    end;


  SourcePegs := Pawns and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    PawnMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      DiscardPinnedMoves(SourceCell, CaptureMoves, QuietMoves);

    if CaptureMoves <> 0 then   // Must test for case where enpassant capture eliminates the checking pawn
      begin
      EnpassantCapture := CaptureMoves and not AllPegs;

      if EnpassantCapture <> 0 then
        begin
        if ToPlay = White then
          begin
          if (EnPassantCapture shl 8) and DefendCheckMask <> 0 then   // Capture Moves for Pawn enpassant move does not match the DefendCheckMask
            DefendCheckMask := DefendCheckMask or EnpassantCapture;
          end
         else
          begin
          if (EnPassantCapture shr 8) and DefendCheckMask <> 0 then
            DefendCheckMask := DefendCheckMask or EnpassantCapture;
          end;
        end;
      end;

    CaptureMoves := CaptureMoves and DefendCheckMask;

    while CaptureMoves <> 0 do
      begin
      MoveValidFlag := true;
      DestCell := PopLowBit_Alt(CaptureMoves);
      CapturedPiece := GetPiece_asm(DestCell);

      if CapturedPiece = 0 then            // handle enpassant capture
        MoveValidFlag := EnpassantLegal(ToPlay, DestCell, SourceCell);   // check enpassant move is legal & not putting king in check

      if MoveValidFlag = true then
        exit(true);
      end;

    QuietMoves := QuietMoves and BlockCheckMask;
    if QuietMoves <> 0 then
      exit(true);
    end;

  SourcePegs := Rooks and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    sourceCell := PopLowBit_Alt(SourcePegs);
    AllMoves := RookAttack_asm(SourceCell) and not PlayerPegs;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      AllMoves := AllMoves and NonPinnedMoves(SourceCell);

    if AllMoves and (DefendCheckMask or BlockCheckMask) <> 0  then
      exit(true);
    end;

  SourcePegs := Bishops and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    AllMoves := BishopAttack_asm(SourceCell) and not PlayerPegs;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      AllMoves := AllMoves and NonPinnedMoves(SourceCell);

    if AllMoves and (DefendCheckMask or BlockCheckMask) <> 0  then
      exit(true);
    end;

  SourcePegs := Queens and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    AllMoves := (RookAttack_asm(SourceCell) or BishopAttack_asm(SourceCell)) and not PlayerPegs;

    if PinnedPieces and (UInt64($1) shl sourceCell) <> 0 then
      AllMoves := AllMoves and NonPinnedMoves(SourceCell);

    if AllMoves and (DefendCheckMask or BlockCheckMask) <> 0  then
      exit(true);

    end;

  if InCheck = false then
    begin
    SourceCell := GetLowBit_Alt(Kings and PlayerPegs);
    KingMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);

    if CaptureMoves <> 0 then
      exit(true);

    QuietMoves := QuietMoves and BlockCheckMask;
    if QuietMoves <> 0 then
      exit(true);
    end;

  result := false;
  end;


function TBoard.GetPinnedPegs(PlayerPegs : UInt64) : UInt64;
  var
    KingCell, source : integer;
    Opponent, Pinners : UInt64;

  begin
  result := 0;
  Opponent := (WhitePegs or BlackPegs) xor PlayerPegs;

  KingCell := GetLowBit_Alt(Kings and PlayerPegs);
  Pinners := RookMask[KingCell] and (Rooks or Queens) and Opponent;
  while Pinners <> 0 do
    begin
    Source := PopLowBit_Alt(Pinners);
    result := result or (RookAttack_asm(source) and RookAttack_asm(KingCell));
    end;

  Pinners := BishopMask[KingCell] and (Bishops or Queens) and Opponent;
  while Pinners <> 0 do
    begin
    Source := PopLowBit_Alt(Pinners);
    result := result or (BishopAttack_asm(source) and BishopAttack_asm(KingCell));
    end;
  end;


procedure TBoard.DiscardPinnedMoves(CellIndex : integer; var CaptureMoves, QuietMoves : UInt64);
  var
    PlayerPegs, OpponentPegs, CellPeg, tempPegs, tempPegs2, ValidMoves, Pinners : UInt64;
    KingCell, Source : integer;

  begin
  CellPeg := UInt64($1) shl CellIndex;

  if ToPlay = White then
    begin
    PlayerPegs := WhitePegs;
    OpponentPegs := BlackPegs;
    end
   else
    begin
    PlayerPegs := BlackPegs;
    OpponentPegs := WhitePegs;
    end;

  // 1) remove piece from board

  if ToPlay = White then
    WhitePegs := WhitePegs xor CellPeg
   else
    BlackPegs := BlackPegs xor CellPeg;

  // 2) get pinner and attacked cells

  KingCell := GetLowBit_Alt(Kings and PlayerPegs);

  ValidMoves := UInt64($FFFFFFFFFFFFFFFF);

  tempPegs := RookAttack_asm(KingCell);
  Pinners := tempPegs and (Rooks or Queens) and OpponentPegs;
  while Pinners <> 0 do
    begin
    Source := PopLowBit_Alt(Pinners);
    tempPegs2 := RookAttack_asm(Source);
    if (CellPeg and tempPegs2) <> 0 then
      ValidMoves := ValidMoves and ((UInt64($1) shl source) or (tempPegs2 and tempPegs));
    end;

  tempPegs := BishopAttack_asm(KingCell);
  Pinners := tempPegs and (Bishops or Queens) and OpponentPegs;
  while Pinners <> 0 do
    begin
    Source := PopLowBit_Alt(Pinners);
    tempPegs2 := BishopAttack_asm(Source);
    if (CellPeg and tempPegs2) <> 0 then
      ValidMoves := ValidMoves and ((UInt64($1) shl source) or (tempPegs2 and tempPegs));
    end;

  // 3) discard moves not in this range

  CaptureMoves := CaptureMoves and ValidMoves;
  QuietMoves := QuietMoves and ValidMoves;

  // 4) Put back piece on board

  if ToPlay = White then
    WhitePegs := WhitePegs xor CellPeg
   else
    BlackPegs := BlackPegs xor CellPeg;
  end;


function TBoard.NonPinnedMoves(CellIndex : integer) : UInt64;
  var
    PlayerPegs, OpponentPegs, CellPeg, tempPegs, tempPegs2,  Pinners : UInt64;
    KingCell, Source : integer;

  begin
  CellPeg := UInt64($1) shl CellIndex;

  if ToPlay = White then
    begin
    PlayerPegs := WhitePegs;
    OpponentPegs := BlackPegs;
    end
   else
    begin
    PlayerPegs := BlackPegs;
    OpponentPegs := WhitePegs;
    end;

  // 1) remove piece from board

  if ToPlay = White then
    WhitePegs := WhitePegs xor CellPeg
   else
    BlackPegs := BlackPegs xor CellPeg;

  // 2) get pinner and attacked cells

  KingCell := GetLowBit_Alt(Kings and PlayerPegs);

  Result := UInt64($FFFFFFFFFFFFFFFF);

  tempPegs := RookAttack_asm(KingCell);
  Pinners := tempPegs and (Rooks or Queens) and OpponentPegs;
  while Pinners <> 0 do
    begin
    Source := PopLowBit_Alt(Pinners);
    tempPegs2 := RookAttack_asm(Source);
    if (CellPeg and tempPegs2) <> 0 then
      Result := Result and ((UInt64($1) shl source) or (tempPegs2 and tempPegs));
    end;

  tempPegs := BishopAttack_asm(KingCell);
  Pinners := tempPegs and (Bishops or Queens) and OpponentPegs;
  while Pinners <> 0 do
    begin
    Source := PopLowBit_Alt(Pinners);
    tempPegs2 := BishopAttack_asm(Source);
    if (CellPeg and tempPegs2) <> 0 then
      Result := Result and ((UInt64($1) shl source) or (tempPegs2 and tempPegs));
    end;

  // 3) Put back piece on board

  if ToPlay = White then
    WhitePegs := WhitePegs xor CellPeg
   else
    BlackPegs := BlackPegs xor CellPeg;
  end;


function TBoard.MakeMove(var Move : UInt64) : boolean;
  var
    sourcePeg, DestPeg, tempPeg, CastleFlags : UInt64;
    Source, Dest, tempDest, Piece, CapturedPiece, PromotionPiece, epCell, Old_epCell : integer;

  begin
  Move := Move and $FFFFFF;  // important to clear move ordering value

  Source :=  Move and $3F;
  Dest :=  (Move shr 6) and $3F;
  Piece := (Move shr 12) and $F;
  CapturedPiece := (Move shr 16) and $F;
  PromotionPiece := (Move shr 20) and $F;

  SourcePeg := UInt64($1) shl Source;
  DestPeg := UInt64($1) shl Dest;

  Old_epCell := 0;

  CastleFlags := PEXT(MovedPieces, UInt64($9100000000000091));
  if Enpassant <> 0 then
    begin
    Old_epCell := GetLowBit_Alt(Enpassant);
    Hash := Hash xor epHash[Old_epCell];             // remove old epCell from Hash
    Move := Move or (CastleFlags shl 24) or (UInt64(Old_epCell) shl 30) or (UInt64(HalfCount) shl 36);
    end
   else
    Move := Move or (CastleFlags shl 24) or (UInt64(HalfCount) shl 36);

  Hash := Hash xor CastleHash[CastleFlags];      // remove old CastleFlags from Hash

  inc(HalfCount);
  EnPassant := 0;

    case Piece of
    King :    begin
              Kings := Kings xor SourcePeg xor DestPeg;

              Hash := Hash xor HashTable[ToPlay, King, Source] xor HashTable[ToPlay, King, Dest];
   //           PawnHash := PawnHash xor HashTable[ToPlay, King, Source] xor HashTable[ToPlay, King, Dest];

              MovedPieces := MovedPieces or SourcePeg;

              if Dest - Source = 2 then        // King Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($A000000000000000);
                  WhitePegs:= WhitePegs xor UInt64($A000000000000000);
                  Hash := Hash xor HashTable[White, Rook, 63] xor HashTable[White, Rook, 61];
                  end
                 else
                  begin
                  Rooks := Rooks xor $A0;
                  BlackPegs:= BlackPegs xor $A0;
                  Hash := Hash xor HashTable[Black, Rook, 7] xor HashTable[Black, Rook, 5];
                  end;

              if Dest - Source = -2 then       // Queen Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($900000000000000);
                  WhitePegs:= WhitePegs xor UInt64($900000000000000);
                  Hash := Hash xor HashTable[White, Rook, 56] xor HashTable[White, Rook, 59];
                  end
                 else
                  begin
                  Rooks := Rooks xor $9;
                  BlackPegs:= BlackPegs xor $9;
                  Hash := Hash xor HashTable[Black, Rook, 0] xor HashTable[Black, Rook, 3];
                  end;
              end;

    Queen :   begin
              Queens := Queens xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Queen, Source] xor HashTable[ToPlay, Queen, Dest];
              end;

    Rook :    begin
              Rooks := Rooks xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Rook, Source] xor HashTable[ToPlay, Rook, Dest];
              MovedPieces := MovedPieces or SourcePeg;
              end;

    Bishop :  begin
              Bishops := Bishops xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Bishop, Source] xor HashTable[ToPlay, Bishop, Dest];
              end;

    Knight :  begin
              Knights := Knights xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Knight, Source] xor HashTable[ToPlay, Knight, Dest];
              end;

    Pawn :    begin
              Pawns := Pawns xor SourcePeg xor DestPeg;

              Hash := Hash xor HashTable[ToPlay, Pawn, Source] xor HashTable[ToPlay, Pawn, Dest];
              PawnHash := PawnHash xor HashTable[ToPlay, Pawn, Source] xor HashTable[ToPlay, Pawn, Dest];

              if abs(Dest - Source) = 16 then
                begin
                epCell := (Dest + Source) shr 1;
                EnPassant := UInt64($1) shl epCell;
                if ToPlay = White then
                  begin
                  if (WhitePawnCaptures[epCell] and Pawns and BlackPegs) <> 0 then
                    Hash := Hash xor epHash[epCell]     // add new epCell to Hash
                   else
                    EnPassant := 0;
                  end
                 else
                  begin
                  if (BlackPawnCaptures[epCell] and Pawns and WhitePegs) <> 0 then
                    Hash := Hash xor epHash[epCell]     // add new epCell to Hash
                   else
                    EnPassant := 0;
                  end;
                end;
              HalfCount := 0;
              end;
    end;

  if ToPlay = White then
    WhitePegs := WhitePegs xor SourcePeg xor DestPeg
   else
    begin
    BlackPegs := BlackPegs xor SourcePeg xor DestPeg;
    inc(TurnNumber);
    end;

  if CapturedPiece <> 0 then
    begin
    HalfCount := 0;
    tempPeg := DestPeg;
    tempDest := Dest;

      case CapturedPiece of
      Queen :   begin
                Queens := Queens xor tempPeg;
                GameStage := GameStage - 10;
                end;
      Rook :    begin
                Rooks := Rooks xor tempPeg;
                MovedPieces := MovedPieces or tempPeg;   // Fixes bug > ensures cannot castle if rook is captured before moving at all
                GameStage := GameStage - 5;
                end;
      Bishop :  begin
                Bishops := Bishops xor tempPeg;
                GameStage := GameStage - 3;
                end;
      Knight :  begin
                Knights := Knights xor tempPeg;
                GameStage := GameStage - 3;
                end;
      Pawn :    begin
                if Dest = Old_epCell then  // enpassant Capture
                  begin
                  if ToPlay = White then
                    begin
                    tempPeg := DestPeg shl 8;
                    Pawns := Pawns xor tempPeg;
                    tempDest := Dest + 8;
                    end
                   else
                    begin
                    tempPeg := DestPeg shr 8;
                    Pawns := Pawns xor tempPeg;
                    tempDest := Dest - 8;
                    end;
                  end
                 else
                  Pawns := Pawns xor tempPeg;

                PawnHash := PawnHash xor HashTable[1-ToPlay, Pawn, tempDest];  // Remove pawn from PawnHash
                end;
      end;

    Hash := Hash xor HashTable[1-ToPlay, CapturedPiece, tempDest];         // Remove captured piece from Hash

    if ToPlay = White then
      BlackPegs := BlackPegs xor tempPeg
     else
      WhitePegs := WhitePegs xor tempPeg;
    end;

  CastleFlags := PEXT(MovedPieces, UInt64($9100000000000091));
  Hash := Hash xor CastleHash[CastleFlags];   // add CastleFlags back into Hash

  if PromotionPiece <> 0 then         // follow after capture
    begin
    Pawns := Pawns xor DestPeg;
    Hash := Hash xor HashTable[ToPlay, Pawn, Dest];            // remove pawn from Hash
    PawnHash := PawnHash xor HashTable[ToPlay, Pawn, Dest];    // remove pawn from PawnHash

      case PromotionPiece of
      Queen :   begin
                Queens := Queens xor DestPeg;
                GameStage := GameStage + 10;
                end;
      Rook :    begin
                Rooks := Rooks xor DestPeg;
                GameStage := GameStage + 5;
                end;
      Bishop :  begin
                Bishops := Bishops xor DestPeg;
                GameStage := GameStage + 3;
                end;
      Knight :  begin
                Knights := Knights xor DestPeg;
                GameStage := GameStage + 3;
                end;
      end;

    Hash := Hash xor HashTable[ToPlay, PromotionPiece, Dest];    // add promoted piece to Hash
    end;

  Hash := Hash xor PlayerHash;
  PawnHash :=  PawnHash xor PlayerHash;

  ToPlay := 1 - ToPlay;

  result := true;
  end;


function TBoard.MakeMoveNoHash(var Move : UInt64) : boolean;
  var
    sourcePeg, DestPeg, tempPeg, CastleFlags : UInt64;
    Source, Dest, Piece, CapturedPiece, PromotionPiece, epCell, Old_epCell : integer;

  begin
  Move := Move and $FFFFFF;  // important to clear move ordering value

  Source :=  Move and $3F;
  Dest :=  (Move shr 6) and $3F;
  Piece := (Move shr 12) and $F;
  CapturedPiece := (Move shr 16) and $F;
  PromotionPiece := (Move shr 20) and $F;

  SourcePeg := UInt64($1) shl Source;
  DestPeg := UInt64($1) shl Dest;

  Old_epCell := 0;

  CastleFlags := PEXT(MovedPieces, UInt64($9100000000000091));
  if Enpassant <> 0 then
    begin
    Old_epCell := GetLowBit_Alt(Enpassant);
    Move := Move or (CastleFlags shl 24) or (UInt64(Old_epCell) shl 30) or (UInt64(HalfCount) shl 36);
    end
   else
    Move := Move or (CastleFlags shl 24) or (UInt64(HalfCount) shl 36);

  inc(HalfCount);
  EnPassant := 0;

    case Piece of
    King :    begin
              Kings := Kings xor SourcePeg xor DestPeg;
              MovedPieces := MovedPieces or SourcePeg;

              if Dest - Source = 2 then        // King Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($A000000000000000);
                  WhitePegs:= WhitePegs xor UInt64($A000000000000000);
                  end
                 else
                  begin
                  Rooks := Rooks xor $A0;
                  BlackPegs:= BlackPegs xor $A0;
                  end;

              if Dest - Source = -2 then       // Queen Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($900000000000000);
                  WhitePegs:= WhitePegs xor UInt64($900000000000000);
                  end
                 else
                  begin
                  Rooks := Rooks xor $9;
                  BlackPegs:= BlackPegs xor $9;
                  end;
              end;

    Queen :   Queens := Queens xor SourcePeg xor DestPeg;

    Rook :    begin
              Rooks := Rooks xor SourcePeg xor DestPeg;
              MovedPieces := MovedPieces or SourcePeg;
              end;

    Bishop :  Bishops := Bishops xor SourcePeg xor DestPeg;

    Knight :  Knights := Knights xor SourcePeg xor DestPeg;

    Pawn :    begin
              Pawns := Pawns xor SourcePeg xor DestPeg;

              if abs(Dest - Source) = 16 then
                begin
                epCell := (Dest + Source) shr 1;
                EnPassant := UInt64($1) shl epCell;
                if ToPlay = White then
                  begin
                  if (WhitePawnCaptures[epCell] and Pawns and BlackPegs) = 0 then
                    EnPassant := 0;
                  end
                 else
                  begin
                  if (BlackPawnCaptures[epCell] and Pawns and WhitePegs) = 0 then
                    EnPassant := 0;
                  end;
                end;
              HalfCount := 0;
              end;
    end;

  if ToPlay = White then
    WhitePegs := WhitePegs xor SourcePeg xor DestPeg
   else
    begin
    BlackPegs := BlackPegs xor SourcePeg xor DestPeg;
    inc(TurnNumber);
    end;

  if CapturedPiece <> 0 then
    begin
    HalfCount := 0;
    tempPeg := DestPeg;

      case CapturedPiece of
      Queen :   Queens := Queens xor tempPeg;

      Rook :    begin
                Rooks := Rooks xor tempPeg;
                MovedPieces := MovedPieces or tempPeg;   // Fixes bug > ensures cannot castle if rook is captured before moving at all
                end;

      Bishop :  Bishops := Bishops xor tempPeg;

      Knight :  Knights := Knights xor tempPeg;

      Pawn :    begin
                if Dest = Old_epCell then  // enpassant Capture
                  begin
                  if ToPlay = White then
                    begin
                    tempPeg := DestPeg shl 8;
                    Pawns := Pawns xor tempPeg;
                    end
                   else
                    begin
                    tempPeg := DestPeg shr 8;
                    Pawns := Pawns xor tempPeg;
                    end;
                  end
                 else
                  Pawns := Pawns xor tempPeg;
                end;
      end;

    if ToPlay = White then
      BlackPegs := BlackPegs xor tempPeg
     else
      WhitePegs := WhitePegs xor tempPeg;
    end;

  if PromotionPiece <> 0 then         // follow after capture
    begin
    Pawns := Pawns xor DestPeg;

      case PromotionPiece of
      Queen :  Queens := Queens xor DestPeg;

      Rook :   Rooks := Rooks xor DestPeg;

      Bishop : Bishops := Bishops xor DestPeg;

      Knight : Knights := Knights xor DestPeg;
      end;
    end;

  ToPlay := 1 - ToPlay;

  result := true;
  end;


function TBoard.GetMoveHash(var Move : UInt64) : UInt64;
  var
    sourcePeg, DestPeg, tempPeg, CastleFlags, TempMovedPieces : UInt64;
    Source, Dest, tempDest, Piece, CapturedPiece, PromotionPiece, epCell, Old_epCell : integer;

  begin
  result := Hash;
  TempMovedPieces := MovedPieces;

  Source :=  Move and $3F;
  Dest :=  (Move shr 6) and $3F;
  Piece := (Move shr 12) and $F;
  CapturedPiece := (Move shr 16) and $F;
  PromotionPiece := (Move shr 20) and $F;

  SourcePeg := UInt64($1) shl Source;
  DestPeg := UInt64($1) shl Dest;

  Old_epCell := 0;

  CastleFlags := PEXT(TempMovedPieces, UInt64($9100000000000091));
  if Enpassant <> 0 then
    begin
    Old_epCell := GetLowBit_Alt(Enpassant);
    result := result xor epHash[Old_epCell];             // remove old epCell from Hash
    end;

  result := result xor CastleHash[CastleFlags];      // remove old CastleFlags from Hash

    case Piece of
    King :    begin
              result := result xor HashTable[ToPlay, King, Source] xor HashTable[ToPlay, King, Dest];
              TempMovedPieces := TempMovedPieces or SourcePeg;

              if Dest - Source = 2 then        // King Side Castle
                if ToPlay = White then
                  result := result xor HashTable[White, Rook, 63] xor HashTable[White, Rook, 61]
                 else
                  result := result xor HashTable[Black, Rook, 7] xor HashTable[Black, Rook, 5];

              if Dest - Source = -2 then       // Queen Side Castle
                if ToPlay = White then
                  result := result xor HashTable[White, Rook, 56] xor HashTable[White, Rook, 59]
                 else
                  result := result xor HashTable[Black, Rook, 0] xor HashTable[Black, Rook, 3];
              end;

    Queen :   result := result xor HashTable[ToPlay, Queen, Source] xor HashTable[ToPlay, Queen, Dest];

    Rook :    begin
              result := result xor HashTable[ToPlay, Rook, Source] xor HashTable[ToPlay, Rook, Dest];
              TempMovedPieces := TempMovedPieces or SourcePeg;
              end;

    Bishop :  result := result xor HashTable[ToPlay, Bishop, Source] xor HashTable[ToPlay, Bishop, Dest];

    Knight :  result := result xor HashTable[ToPlay, Knight, Source] xor HashTable[ToPlay, Knight, Dest];

    Pawn :    begin
              result := result xor HashTable[ToPlay, Pawn, Source] xor HashTable[ToPlay, Pawn, Dest];

              if abs(Dest - Source) = 16 then
                begin
                epCell := (Dest + Source) shr 1;
                if ToPlay = White then
                  begin
                  if (WhitePawnCaptures[epCell] and Pawns and BlackPegs) <> 0 then
                    result := result xor epHash[epCell]     // add new epCell to Hash
                  end
                 else
                  begin
                  if (BlackPawnCaptures[epCell] and Pawns and WhitePegs) <> 0 then
                    result := result xor epHash[epCell]     // add new epCell to Hash
                  end;
                end;
              end;
    end;

  if CapturedPiece <> 0 then
    begin
    tempPeg := DestPeg;
    tempDest := Dest;

      case CapturedPiece of

      Rook :    TempMovedPieces := TempMovedPieces or tempPeg;   // Fixes bug > ensures cannot castle if rook is captured before moving at all

      Pawn :    begin
                if Dest = Old_epCell then  // enpassant Capture
                  begin
                  if ToPlay = White then
                    tempDest := Dest + 8
                   else
                    tempDest := Dest - 8;
                  end;
                end;
      end;

    result := result xor HashTable[1-ToPlay, CapturedPiece, tempDest];         // Remove captured piece from Hash
    end;

  CastleFlags := PEXT(TempMovedPieces, UInt64($9100000000000091));
  result := result xor CastleHash[CastleFlags];   // add CastleFlags back into Hash

  if PromotionPiece <> 0 then         // follow after capture
    begin
    result := result xor HashTable[ToPlay, Pawn, Dest];              // remove pawn from Hash
    result := result xor HashTable[ToPlay, PromotionPiece, Dest];    // add promoted piece to Hash
    end;

  result := result xor PlayerHash;
  end;


function TBoard.UndoMove(const Move : UInt64) : boolean;
  var
    sourcePeg, DestPeg, tempPeg, CastleFlags : UInt64;
    Source, Dest, tempDest, Piece, CapturedPiece, PromotionPiece, epCell : integer;

  begin
  Source := Move and $3F;
  Dest :=  (Move shr 6) and $3F;
  Piece := (Move shr 12) and $F;
  CapturedPiece := (Move shr 16) and $F;
  PromotionPiece := (Move shr 20) and $F;

  SourcePeg := UInt64($1) shl Source;
  DestPeg := UInt64($1) shl Dest;

  ToPlay := 1 - ToPlay;

  PawnHash := PawnHash xor PlayerHash;
  Hash := Hash xor PlayerHash;

  CastleFlags := PEXT(MovedPieces, UInt64($9100000000000091));
  Hash := Hash xor CastleHash[CastleFlags];      // remove CastleFlags from Hash

  if Enpassant <> 0 then
    begin
    epCell := GetLowBit_Alt(Enpassant);
    Hash := Hash xor epHash[epCell];             // remove last epCell from Hash
    end;

  HalfCount := (Move shr 36) and $FF;
  CastleFlags := (Move shr 24) and $3F;
  epCell := (Move shr 30) and $3F;

  Hash := Hash xor CastleHash[CastleFlags];      // add CastleFlags to Hash
  MovedPieces := PDEP(CastleFlags, UInt64($9100000000000091));

  Enpassant := 0;
  if epCell <> 0 then
    begin
    Hash := Hash xor epHash[epCell];             // add last epCell to Hash
    Enpassant := UInt64(1) shl epCell;
    end;

  if PromotionPiece <> 0 then
    begin
    Pawns := Pawns xor DestPeg;

    Hash := Hash xor HashTable[ToPlay, Pawn, Dest];    // add pawn to Hash
    PawnHash := PawnHash xor HashTable[ToPlay, Pawn, Dest];    // add pawn to PawnHash

      case PromotionPiece of
      Queen :   begin
                Queens := Queens xor DestPeg;
                GameStage := GameStage - 10;
                end;
      Rook :    begin
                Rooks := Rooks xor DestPeg;
                GameStage := GameStage - 5;
                end;
      Bishop :  begin
                Bishops := Bishops xor DestPeg;
                GameStage := GameStage - 3;
                end;
      Knight :  begin
                Knights := Knights xor DestPeg;
                GameStage := GameStage - 3;
                end;
      end;

    Hash := Hash xor HashTable[ToPlay, PromotionPiece, Dest];    // remove promoted piece from Hash
    end;

  if CapturedPiece <> 0 then
    begin
    tempPeg := DestPeg;
    tempDest := Dest;

      case CapturedPiece of
      Queen :   begin
                Queens := Queens xor tempPeg;
                GameStage := GameStage + 10;
                end;
      Rook :    begin
                Rooks := Rooks xor tempPeg;
                GameStage := GameStage + 5;
                end;
      Bishop :  begin
                Bishops := Bishops xor tempPeg;
                GameStage := GameStage + 3;
                end;
      Knight :  begin
                Knights := Knights xor tempPeg;
                GameStage := GameStage + 3;
                end;
      Pawn :    begin
                if Dest = epCell then
                  begin
                  if ToPlay = White then
                    begin
                    tempPeg := (DestPeg shl 8);
                    Pawns := Pawns xor tempPeg;
                    tempDest := Dest + 8;
                    end
                   else
                    begin
                    tempPeg := (DestPeg shr 8);
                    Pawns := Pawns xor tempPeg;
                    tempDest := Dest - 8;
                    end;
                  end
                 else
                  Pawns := Pawns xor tempPeg;

                PawnHash := PawnHash xor HashTable[1-ToPlay, Pawn, tempDest];   // Add captured pawn to PawnHash
                end;
      end;

    Hash := Hash xor HashTable[1-ToPlay, CapturedPiece, tempDest];    // Add captured piece to Hash

    if ToPlay = White then
      BlackPegs := BlackPegs xor tempPeg
     else
      WhitePegs := WhitePegs xor tempPeg;
    end;

    case Piece of
    King :    begin
              Kings := Kings xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, King, Source] xor HashTable[ToPlay, King, Dest];
      //        PawnHash := PawnHash xor HashTable[ToPlay, King, Source] xor HashTable[ToPlay, King, Dest];

              if Dest - Source = 2 then    // King Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($A000000000000000);
                  WhitePegs:= WhitePegs xor UInt64($A000000000000000);
                  Hash := Hash xor HashTable[White, Rook, 63] xor HashTable[White, Rook, 61];
                  end
                 else
                  begin
                  Rooks := Rooks xor $A0;
                  BlackPegs:= BlackPegs xor $A0;
                  Hash := Hash xor HashTable[Black, Rook, 7] xor HashTable[Black, Rook, 5];
                  end;

              if Dest - Source = -2 then    // Queen Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($900000000000000);
                  WhitePegs:= WhitePegs xor UInt64($900000000000000);
                  Hash := Hash xor HashTable[White, Rook, 56] xor HashTable[White, Rook, 59];
                  end
                 else
                  begin
                  Rooks := Rooks xor $9;
                  BlackPegs:= BlackPegs xor $9;
                  Hash := Hash xor HashTable[Black, Rook, 0] xor HashTable[Black, Rook, 3];
                  end;
              end;

    Queen :   begin
              Queens := Queens xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Queen, Source] xor HashTable[ToPlay, Queen, Dest];
              end;

    Rook :    begin
              Rooks := Rooks xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Rook, Source] xor HashTable[ToPlay, Rook, Dest];
              end;

    Bishop :  begin
              Bishops := Bishops xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Bishop, Source] xor HashTable[ToPlay, Bishop, Dest];
              end;

    Knight :  begin
              Knights := Knights xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Knight, Source] xor HashTable[ToPlay, Knight, Dest];
              end;

    Pawn :    begin
              Pawns := Pawns xor SourcePeg xor DestPeg;
              Hash := Hash xor HashTable[ToPlay, Pawn, Source] xor HashTable[ToPlay, Pawn, Dest];
              PawnHash := PawnHash xor HashTable[ToPlay, Pawn, Source] xor HashTable[ToPlay, Pawn, Dest];
              end;
    end;


  if ToPlay = White then
    WhitePegs := WhitePegs xor SourcePeg xor DestPeg
   else
    begin
    BlackPegs := BlackPegs xor SourcePeg xor DestPeg;
    dec(TurnNumber);
    end;

  result := true;
  end;


function TBoard.UndoMoveNoHash(const Move : UInt64) : boolean;
  var
    sourcePeg, DestPeg, tempPeg, CastleFlags : UInt64;
    Source, Dest, Piece, CapturedPiece, PromotionPiece, epCell : integer;

  begin
  Source := Move and $3F;
  Dest :=  (Move shr 6) and $3F;
  Piece := (Move shr 12) and $F;
  CapturedPiece := (Move shr 16) and $F;
  PromotionPiece := (Move shr 20) and $F;

  SourcePeg := UInt64($1) shl Source;
  DestPeg := UInt64($1) shl Dest;

  ToPlay := 1 - ToPlay;

  HalfCount := (Move shr 36) and $FF;
  CastleFlags := (Move shr 24) and $3F;
  epCell := (Move shr 30) and $3F;

  MovedPieces := PDEP(CastleFlags, UInt64($9100000000000091));

  Enpassant := 0;
  if epCell <> 0 then
    Enpassant := UInt64(1) shl epCell;

  if PromotionPiece <> 0 then
    begin
    Pawns := Pawns xor DestPeg;

      case PromotionPiece of
      Queen :  Queens := Queens xor DestPeg;

      Rook :   Rooks := Rooks xor DestPeg;

      Bishop : Bishops := Bishops xor DestPeg;

      Knight : Knights := Knights xor DestPeg;
      end;
    end;

  if CapturedPiece <> 0 then
    begin
    tempPeg := DestPeg;

      case CapturedPiece of
      Queen :   Queens := Queens xor tempPeg;

      Rook :    Rooks := Rooks xor tempPeg;

      Bishop :  Bishops := Bishops xor tempPeg;

      Knight :  Knights := Knights xor tempPeg;

      Pawn :    begin
                if Dest = epCell then
                  begin
                  if ToPlay = White then
                    begin
                    tempPeg := (DestPeg shl 8);
                    Pawns := Pawns xor tempPeg;
                    end
                   else
                    begin
                    tempPeg := (DestPeg shr 8);
                    Pawns := Pawns xor tempPeg;
                    end;
                  end
                 else
                  Pawns := Pawns xor tempPeg;
                end;
      end;

    if ToPlay = White then
      BlackPegs := BlackPegs xor tempPeg
     else
      WhitePegs := WhitePegs xor tempPeg;
    end;

    case Piece of
    King :    begin
              Kings := Kings xor SourcePeg xor DestPeg;

              if Dest - Source = 2 then    // King Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($A000000000000000);
                  WhitePegs:= WhitePegs xor UInt64($A000000000000000);
                  end
                 else
                  begin
                  Rooks := Rooks xor $A0;
                  BlackPegs:= BlackPegs xor $A0;
                  end;

              if Dest - Source = -2 then    // Queen Side Castle
                if ToPlay = White then
                  begin
                  Rooks := Rooks xor UInt64($900000000000000);
                  WhitePegs:= WhitePegs xor UInt64($900000000000000);
                  end
                 else
                  begin
                  Rooks := Rooks xor $9;
                  BlackPegs:= BlackPegs xor $9;
                  end;
              end;

    Queen :   Queens := Queens xor SourcePeg xor DestPeg;

    Rook :    Rooks := Rooks xor SourcePeg xor DestPeg;

    Bishop :  Bishops := Bishops xor SourcePeg xor DestPeg;

    Knight :  Knights := Knights xor SourcePeg xor DestPeg;

    Pawn :    Pawns := Pawns xor SourcePeg xor DestPeg;
    end;


  if ToPlay = White then
    WhitePegs := WhitePegs xor SourcePeg xor DestPeg
   else
    begin
    BlackPegs := BlackPegs xor SourcePeg xor DestPeg;
    dec(TurnNumber);
    end;

  result := true;
  end;


function TBoard.GetCellValidMoves(Player : integer; CellIndex : integer) : UInt64;
  var
    Moves :  TMoveArray;
    i, MoveCount : integer;

  begin
  MoveCount := GetAllValidMoves(Player, moves, false);
  Result := 0;

  for i := 1 to MoveCount do
    begin
    if (Moves[i] and $3F) = CellIndex then
      SetBit(result, (Moves[i] shr 6) and $3F);
    end;
  end;


function TBoard.KingInCheck(Player : integer; var CheckCaptureMask, CheckBlockMask : UInt64) : boolean;
  var
    cell : integer;
    Opponent, tempPegs, RookCapture, RookBlock, BishopCapture, BishopBlock : UInt64;

  begin
  if Player = White then
    begin
    Opponent := BlackPegs;
    Cell := GetLowBit_Alt(Kings and WhitePegs);
    CheckCaptureMask := WhitePawnCaptures[Cell] and Pawns and Opponent;
    end
   else
    begin
    Opponent := WhitePegs;
    Cell := GetLowBit_Alt(Kings and BlackPegs);
    CheckCaptureMask := BlackPawnCaptures[Cell] and Pawns and Opponent;
    end;

  tempPegs := RookAttack_asm(cell);
  RookCapture := tempPegs and (Rooks or Queens) and Opponent;
  RookBlock := 0;
  if RookCapture <> 0 then
    begin
    RookBlock := RookAttack_asm(GetLowBit_Alt(RookCapture)) and tempPegs;
    CheckCaptureMask := CheckCaptureMask or RookCapture;
    end;

  tempPegs := BishopAttack_asm(cell);
  BishopCapture := tempPegs and (Bishops or Queens) and Opponent;
  BishopBlock := 0;
  if BishopCapture <> 0 then
    begin
    BishopBlock := BishopAttack_asm(GetLowBit_Alt(BishopCapture)) and tempPegs;
    CheckCaptureMask := CheckCaptureMask or BishopCapture;
    end;

  CheckBlockMask := BishopBlock or RookBlock;
  CheckCaptureMask := CheckCaptureMask or (KnightMask[Cell] and Knights and Opponent);

  result := (CheckCaptureMask <> 0);

  if result = false then
    begin
    CheckCaptureMask := UInt64($FFFFFFFFFFFFFFFF);
    CheckBlockMask := UInt64($FFFFFFFFFFFFFFFF);
    end;
  end;


function TBoard.KingInCheck(Player : integer) : boolean;
  var
    Opponent, t : UInt64;
    KingCell : integer;

  begin
  if Player = White then
    begin
    Opponent := BlackPegs;
    KingCell := GetLowBit_Alt(Kings and WhitePegs);
    end
   else
    begin
    Opponent := WhitePegs;
    KingCell := GetLowBit_Alt(Kings and BlackPegs);
    end;

  if RookAttack_asm(KingCell) and (Rooks or Queens) and Opponent <> 0 then
    exit(true);

  if BishopAttack_asm(KingCell) and (Bishops or Queens) and Opponent <> 0 then
    exit(true);

  if KnightMask[KingCell] and Knights and Opponent <> 0 then
    exit(true);

  t := ((WhitePawnCaptures[KingCell] and BlackPegs) or (BlackPawnCaptures[KingCell] and WhitePegs)) and Pawns and Opponent;

  result := (t <> 0);
  end;


function TBoard.AdjacentKings : boolean;
  var
    KingCell : integer;

  begin
  KingCell := GetLowBit_Alt(Kings and WhitePegs);
  result := KingMask[KingCell] and Kings and BlackPegs <> 0
  end;


function TBoard.IsMoveLegal(Move : TMove) : boolean;
  var
    Flag : boolean;

  begin
  if GetColor(Move.Source) <> ToPlay then
    exit(false);

  if GetPiece_asm(Move.Source) <> Move.Piece then
    exit(false);

  if GetPiece_asm(Move.Dest) <> Move.CapturedPiece then
    exit(false);

  if (Move.CapturedPiece <> 0) and (GetColor(Move.Dest) <> (1-ToPLay)) then
    exit(false);

  makeMoveNoHash(Move);
  flag := KingInCheck(1-ToPlay);
  UndoMoveNoHash(Move);

  if flag = true then
    exit(false);

  result := true;
  end;


function TBoard.KingOnly(Player : integer) : boolean;
  begin
  if Player = White then
    result := (WhitePegs and not Kings) = 0
   else
    result := (BlackPegs and not Kings) = 0
  end;


function TBoard.PawnsOnly(Player : integer) : boolean;
  begin
  if Player = White then
    result := (WhitePegs and (Queens or Rooks or Knights or Bishops)) = 0
   else
    result := (BlackPegs and (Queens or Rooks or Knights or Bishops)) = 0;
  end;


function TBoard.OnlyPawns : boolean;
  begin
  result := (Queens or Rooks or Knights or Bishops) = 0;
  end;



function TBoard.CellAttacked(Player, cell : integer) : boolean;
  var
    Opponent, t : UInt64;

  begin
  if Player = White then
    Opponent := BlackPegs
   else
    Opponent := WhitePegs;

  t := RookAttack_asm(cell) and (Rooks or Queens) and Opponent;
  if t <> 0 then
    exit(true);

  t := BishopAttack_asm(cell) and (Bishops or Queens) and Opponent;
  if t <> 0 then
    exit(true);

  t := KnightMask[Cell] and Knights and Opponent;
  if t <> 0 then
    exit(true);

  t := KingMask[Cell] and Kings and Opponent;
  if t <> 0 then
    exit(true);

  t := ((WhitePawnCaptures[Cell] and BlackPegs) or (BlackPawnCaptures[Cell] and WhitePegs)) and Pawns and Opponent;

  result := (t <> 0);
  end;


function TBoard.SEE(Move : TMove) : integer;
  var
    AttackMove : TMove;
    Dest, CapturedPiece, PromotionPiece : integer;

  begin
  Dest := (Move shr 6) and $3F;

  if not CellAttacked(1-ToPlay, Dest) then
    exit(0);

  AttackMove := GetCaptureMove(Dest);

  if AttackMove = 0 then
    exit(0);

  MakeMoveNoHash(AttackMove);

  CapturedPiece := (AttackMove shr 16) and $F;
  PromotionPiece := (AttackMove shr 20) and $F;

  result := PieceValue[CapturedPiece] - SEE(AttackMove);

  if PromotionPiece <> 0 then
    result := result + PieceValue[PromotionPiece] - PieceValue[Pawn];

  result := max(0, result);

  UndoMoveNoHash(AttackMove);
  end;


function TBoard.LargestPieceValue(Player : integer) : integer;
  var
    PlayerPegs : UInt64;

  begin
  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  if (PlayerPegs and Queens) <> 0 then
    exit(870);

  if (PlayerPegs and Rooks) <> 0 then
    exit(470);

  if (PlayerPegs and Bishops) <> 0 then
    exit(305);

  if (PlayerPegs and Knights) <> 0 then
    exit(295);

  if (PlayerPegs and Pawns) <> 0 then
    exit(95);

  result := 0;
  end;


function TBoard.EnpassantLegal(ToPlay : integer; Dest, Source : UInt64) : boolean;
  var
    tempPegs, KingPeg : UInt64;
    SourcePeg, DestPeg : UInt64;
    KingCell : integer;

  begin
  result := true;
  SourcePeg := UInt64($1) shl Source;
  DestPeg := UInt64($1) shl Dest;

  if  ToPlay = White then                    // test if discovered check makes enpassant move illegal
    begin
    KingPeg := Kings and WhitePegs;

    if (KingPeg and UInt64($FF000000) <> 0) and (((Queens or Rooks) and BlackPegs) and UInt64($FF000000) <> 0) then
      begin
      // remove pawns and check king for rook attack

      WhitePegs := WhitePegs xor SourcePeg;
      BlackPegs := BlackPegs xor (DestPeg shl 8);

      KingCell := GetLowBit_Alt(KingPeg);
      tempPegs := RookAttack_asm(KingCell);
      if (tempPegs and (Rooks or Queens) and BlackPegs) <> 0 then
        result := false;

      WhitePegs := WhitePegs xor SourcePeg;
      BlackPegs := BlackPegs xor (DestPeg shl 8);
      end;

    exit;
    end
   else
    begin
    KingPeg := Kings and BlackPegs;

    if (KingPeg and UInt64($FF00000000) <> 0) and (((Queens or Rooks) and WhitePegs) and UInt64($FF00000000) <> 0) then
      begin
      // remove pawns and check king for rook attack

      BlackPegs := BlackPegs xor SourcePeg;
      WhitePegs := WhitePegs xor (DestPeg shr 8);

      KingCell := GetLowBit_Alt(KingPeg);
      tempPegs := RookAttack_asm(KingCell);
      if (tempPegs and (Rooks or Queens) and WhitePegs) <> 0 then
        result := false;

      BlackPegs := BlackPegs xor SourcePeg;
      WhitePegs := WhitePegs xor (DestPeg shr 8);
      end;

    exit;
    end
  end;


function TBoard.RookAttack(Cell : integer): UInt64;
  var
    Index : integer;
    Moves, Mask, AllPegs : UInt64;

  begin
  AllPegs := WhitePegs or BlackPegs;

  Mask :=  RookHorzMask[Cell];
  Index := PEXT(AllPegs, Mask);
  Moves := PDEP(RookLookup[Cell and $7, Index], Mask);

  Mask :=  RookVertMask[Cell];
  Index := PEXT(AllPegs, Mask);
  result := Moves or PDEP(RookLookup[Cell shr 3, Index], Mask);
  end;


function TBoard.RookAttack_asm(Cell : integer): UInt64;
  asm
  mov r8, Self.WhitePegs                      //  r8 =>     AllPegs := WhitePegs or BlackPegs;
  or r8, Self.BlackPegs

  lea r9, qword ptr TBoard.RookHorzMask       //  r9 =>     Mask :=  RookHorzMask[Cell];
  mov r9, [r9 + rdx * $8]
  PEXT rax, r8, r9                            //  rax =>    Index := PEXT(AllPegs, Mask);

  mov r10, rdx                                // r10 =>     cell and $7
  and r10, $07
  shl r10, $04

  lea r11, qword ptr TBoard.RookLookup
  lea r11, [r11 + r10 * $8]
  movzx r11, byte ptr [r11 + rax]             // r11 =>     RookLookup[cell and $7, Index]

  PDEP r12, r11, r9                           // r12 =>     Moves := PDEP(RookLookup[Cell and $7, Index], Mask);

  lea r9, qword ptr Tboard.RookVertMask       //  r9 =>     Mask :=  RookVertMask[Cell];
  mov r9, [r9 + rdx * $8]

  PEXT rax, r8, r9                            //  rax =>    Index := PEXT(AllPegs, Mask);

  mov r10, rdx                                //  r10 =>    cell shr 3
  shr r10, $03
  shl r10, $04

  lea r11, qword ptr TBoard.RookLookup
  lea r11, [r11 + r10 * $8]
  movzx r11, byte ptr [r11 + rax]             // r11 =>     RookLookup[Cell shr 3, Index];

  PDEP rax, r11, r9                           // rax =>     PDEP(RookLookup[Cell shr 3, Index], Mask)

  or rax, r12                                 // result :=  Moves or PDEP(RookLookup[Cell shr 3, Index], Mask);
  end;


function TBoard.RookMobility(Player : integer; MobBoard : UInt64) : integer;
  var
    SourcePegs, PlayerPegs, Moves : UInt64;
    SourceCell : integer;

  begin
  result := 0;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  SourcePegs := Rooks and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    Moves := RookAttack_asm(SourceCell) and MobBoard;
    result := result + BitCount(Moves);
    end;
  end;


function TBoard.BishopAttack(Cell : integer): UInt64;
  var
    Index : integer;
    Moves, Mask, AllPegs : UInt64;

  begin
  AllPegs := WhitePegs or BlackPegs;

  Mask :=  BishopUpMask[Cell];
  Index := PEXT(AllPegs, Mask);
  Moves := PDEP(BishopUpLookup[Cell, Index], Mask);

  Mask :=  BishopDownMask[Cell];
  Index := PEXT(AllPegs, Mask);
  result := Moves or PDEP(BishopDownLookup[Cell, Index], Mask);
  end;


function TBoard.BishopAttack_asm(Cell : integer): UInt64;
  asm
  mov r8, Self.WhitePegs                      //  r8 =>     AllPegs := WhitePegs or BlackPegs;
  or r8, Self.BlackPegs

  lea r9, qword ptr TBoard.BishopUpMask       //  r9 =>     Mask :=  BishopUpMask[Cell];
  mov r9, [r9 + rdx * $8]
  PEXT rax, r8, r9                            //  rax =>    Index := PEXT(AllPegs, Mask);

  mov r10, rdx                                // r10 =>     cell
  shl r10, $04

  lea r11, qword ptr TBoard.BishopUpLookup
  lea r11, [r11 + r10 * $8]
  movzx r11, byte ptr [r11 + rax]             // r11 =>     BishopUpLookup[cell, Index]

  PDEP r12, r11, r9                           // r12 =>     Moves := PDEP(BishopUpLookup[Cell, Index], Mask);

  lea r9, qword ptr Tboard.BishopDownMask     //  r9 =>     Mask :=  BishopDownMask[Cell];
  mov r9, [r9 + rdx * $8]

  PEXT rax, r8, r9                            //  rax =>    Index := PEXT(AllPegs, Mask);

  mov r10, rdx                                //  r10 =>    cell
  shl r10, $04

  lea r11, qword ptr TBoard.BishopDownLookup
  lea r11, [r11 + r10 * $8]
  movzx r11, byte ptr [r11 + rax]             // r11 =>     BishopDownLookup[Cell, Index];

  PDEP rax, r11, r9                           // rax =>     PDEP(BishopDownLookup[Cell, Index], Mask)

  or rax, r12                                 // result :=  Moves or PDEP(BishopDownLookup[Cell, Index], Mask);
  end;


function TBoard.BishopMobility(Player : integer; MobBoard : UInt64) : integer;
  var
    SourcePegs, PlayerPegs, Moves : UInt64;
    SourceCell : integer;

  begin
  result := 0;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  SourcePegs := Bishops and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    Moves := BishopAttack_asm(SourceCell) and MobBoard;
    result := result + BitCount(Moves);
    end;
  end;


function TBoard.QueenAttack(Cell : integer) : UInt64;
  begin
  Result := RookAttack_asm(Cell) or BishopAttack_asm(Cell);
  end;


function TBoard.QueenMobility(Player : integer; MobBoard : UInt64) : integer;
  var
    SourcePegs, PlayerPegs, Moves : UInt64;
    SourceCell : integer;

  begin
  result := 0;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  SourcePegs := Queens and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    Moves := QueenAttack(SourceCell) and MobBoard;
    result := result + BitCount(Moves);
    end;
  end;


function TBoard.KnightAttack(Cell : integer): UInt64;
  begin
  Result :=  KnightMask[Cell];
  end;


function TBoard.KnightMobility(Player : integer; MobBoard : UInt64) : integer;
  var
    SourcePegs, PlayerPegs, Moves : UInt64;
    SourceCell : integer;

  begin
  result := 0;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  SourcePegs := Knights and PlayerPegs;
  while SourcePegs <> 0 do
    begin
    SourceCell := PopLowBit_Alt(SourcePegs);
    Moves := KnightMask[SourceCell] and MobBoard;
    result := result + BitCount(Moves);
    end;
  end;


function TBoard.KingMobility(Player : integer; MobBoard : UInt64) : integer;
  var
    SourcePegs, PlayerPegs, QuietMoves, CaptureMoves : UInt64;
    SourceCell : integer;

  begin
  result := 0;

  if Player = White then
    PlayerPegs := WhitePegs
   else
    PlayerPegs := BlackPegs;

  SourcePegs := Kings and PlayerPegs;
  SourceCell := PopLowBit_Alt(SourcePegs);
  KingMoves(SourceCell, PlayerPegs, CaptureMoves, QuietMoves);
  result := result + BitCount(CaptureMoves or QuietMoves);
  end;


function TBoard.MobilityBoard(Player : integer) : UInt64;
  var
    SourcePegs, PlayerPegs : Uint64;

  begin
  result := 0;

  if Player = Black then
    begin
    PlayerPegs := BlackPegs;
    SourcePegs := WhitePegs and Pawns;
    result := ((SourcePegs and not Board_RightEdge) shr 7) or ((SourcePegs and not Board_LeftEdge) shr 9);
    end
   else
    begin
    PlayerPegs := WhitePegs;
    SourcePegs := BlackPegs and Pawns;
    result := ((SourcePegs and not Board_RightEdge) shl 9) or ((SourcePegs and not Board_LeftEdge) shl 7);
    end;

  // mobility board excludes squares attacked by enemy pawns and excludes squares occupied by own pawns and square occupied by own king

  result := not (result or (PlayerPegs and (Pawns or Kings)));
  end;


function TBoard.KingAttack(Cell : integer) : UInt64;
  begin
  Result := KingMask[Cell];
  end;


function TBoard.PawnAttack(Cell : integer) : UInt64;
  begin
  result := (WhitePawnCaptures[Cell] and BlackPegs) or (BlackPawnCaptures[Cell] and WhitePegs);
  end;


procedure TBoard.RookMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  var
    Moves : UInt64;

  begin
  Moves :=   RookAttack_asm(Cell);
  QuietMoves := Moves and not AllPegs;
  CaptureMoves := Moves and (AllPegs xor PlayerPegs);
  end;


procedure TBoard.BishopMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  var
    Moves : UInt64;

  begin
  Moves :=   BishopAttack_asm(Cell);
  QuietMoves := Moves and not AllPegs;
  CaptureMoves := Moves and (AllPegs xor PlayerPegs);
  end;


procedure TBoard.QueenMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  var
    Moves : UInt64;

  begin
  Moves :=   BishopAttack_asm(Cell) or RookAttack_asm(Cell);
  QuietMoves := Moves and not AllPegs;
  CaptureMoves := Moves and (AllPegs xor PlayerPegs);
  end;


procedure TBoard.KnightMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  var
    Moves : UInt64;

  begin
  Moves :=  KnightMask[Cell];
  QuietMoves := Moves and not (AllPegs);
  CaptureMoves := Moves and (AllPegs xor PlayerPegs);
  end;


procedure TBoard.KingMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  var
    tempPegs : UInt64;
    dest, Player : integer;

  begin
  AllPegs := WhitePegs or BlackPegs;

  QuietMoves := KingMask[Cell] and not AllPegs;
  CaptureMoves := KingMask[Cell] and (AllPegs xor PlayerPegs);

  if PlayerPegs = WhitePegs then
    begin
    if CastleKingSide(White) = true then
      QuietMoves := QuietMoves or UInt64($4000000000000000);
    if CastleQueenSide(White) = true then
      QuietMoves := QuietMoves or UInt64($0400000000000000);
    Player := White;
    end
   else
    begin
    if CastleKingSide(Black) = true then
      QuietMoves := QuietMoves or $40;
    if CastleQueenSide(Black) = true then
      QuietMoves := QuietMoves or $04;
    Player := Black;
    end;

  ClearBit(Kings, Cell);            // to test cell behind king in sliding attack in direction towards king

  if Player = White then
    ClearBit(WhitePegs, Cell)
   else
    ClearBit(BlackPegs, Cell);

  tempPegs := QuietMoves;
  while tempPegs <> 0 do
    begin
    Dest := PopLowBit_Alt(tempPegs);
    if CellAttacked(Player, Dest) then
      ClearBit(QuietMoves, Dest);
    end;

  tempPegs := CaptureMoves;
  while tempPegs <> 0 do
    begin
    Dest := PopLowBit_Alt(tempPegs);
    if CellAttacked(Player, Dest) then
      ClearBit(CaptureMoves, Dest);
    end;

  if Player = White then
    WhitePegs := WhitePegs or (UInt64($1) shl Cell)       // Slightly faster than calling setbit
   else
    BlackPegs := BlackPegs or (UInt64($1) shl Cell);

  Kings := Kings or (UInt64($1) shl Cell);
  end;


procedure TBoard.KingMoves_InCheck(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
// No need to test for castle move if in check

  var
    tempPegs : UInt64;
    dest, Player : integer;

  begin
  AllPegs := WhitePegs or BlackPegs;

  QuietMoves := KingMask[Cell] and not AllPegs;
  CaptureMoves := KingMask[Cell] and (AllPegs xor PlayerPegs);

  ClearBit(Kings, Cell);            // to test cell behind king in sliding attack in direction towards king
  if PlayerPegs = WhitePegs then
    begin
    Player := White;
    ClearBit(WhitePegs, Cell);
    end
   else
    begin
    Player := Black;
    ClearBit(BlackPegs, Cell);
    end;

  tempPegs := QuietMoves;
  while tempPegs <> 0 do
    begin
    Dest := PopLowBit_Alt(tempPegs);
    if CellAttacked(Player, Dest) then
      ClearBit(QuietMoves, Dest);
    end;

  tempPegs := CaptureMoves;
  while tempPegs <> 0 do
    begin
    Dest := PopLowBit_Alt(tempPegs);
    if CellAttacked(Player, Dest) then
      ClearBit(CaptureMoves, Dest);
    end;

  if Player = White then
    WhitePegs := WhitePegs or (UInt64($1) shl Cell)       // Slightly faster than calling setbit
   else
    BlackPegs := BlackPegs or (UInt64($1) shl Cell);

  Kings := Kings or (UInt64($1) shl Cell);
  end;


procedure TBoard.PawnMoves(Cell : integer; PlayerPegs : UInt64; var CaptureMoves, QuietMoves : UInt64);
  var
    Mask, Peg : UInt64;

  begin
  Peg := UInt64($1) shl cell;

  if PlayerPegs = WhitePegs then
    begin
    Mask := AllPegs and (Peg shr 8);
    if Mask = 0 then
      QuietMoves := (WhitePawnMoves[Cell] and not AllPegs)
     else
      QuietMoves := 0;

    CaptureMoves := WhitePawnCaptures[Cell] and (BlackPegs or EnPassant);
    end
   else
    begin
    Mask := AllPegs and (Peg shl 8);
    if Mask = 0 then
      QuietMoves := (BlackPawnMoves[Cell] and not AllPegs)
     else
      QuietMoves := 0;
    CaptureMoves := BlackPawnCaptures[Cell] and (WhitePegs or EnPassant);
    end;
  end;


function TBoard.CastleKingSide(Player : integer) : boolean;
  // only called if king is not in check so no need to test if king is attacked here

  begin
  if Player = White then
    begin
    if MovedPieces and UInt64($9000000000000000) <> 0 then             // King or KingSide rook has moved or KingSide rook has been captured before moving
      exit(false);

    if AllPegs and UInt64($6000000000000000) <> 0 then                 // Intermediate squares are occupied
      exit(false);

    if CellAttacked(Player, 61) or CellAttacked(Player, 62) then     //   Intermediate squares are attacked
     exit(false);
    end
   else
    begin
    if MovedPieces and $90 <> 0 then    // King or Kingside rook has moved or KingSide rook has been captured before moving
      exit(false);

    if AllPegs and $60 <> 0 then        // Intermediate squares are occupied
      exit(false);

    if CellAttacked(Player, 5) or CellAttacked(Player, 6) then   //   Intermediate squares are attacked
     exit(false);
    end;

  result := true;
  end;


function TBoard.CastleQueenSide(Player : integer) : boolean;
  // only called if king is not in check so no need to test if king is attacked here

  begin
  if Player = White then
    begin
    if MovedPieces and UInt64($1100000000000000) <> 0 then             // King or QueenSide rook has moved or Queenside rook has been captured before moving
      exit(false);

    if AllPegs and UInt64($0E00000000000000) <> 0 then                  // Intermediate squares are occupied
      exit(false);

    if CellAttacked(Player, 58) or CellAttacked(Player, 59) then     //   Intermediate squares are attacked
     exit(false);
    end
   else
    begin
    if MovedPieces and $11 <> 0 then    // King or QueenSide rook has moved or QueenSide rook has been captured before moving
      exit(false);

    if AllPegs and $E <> 0 then        // Intermediate squares are occupied
      exit(false);

    if CellAttacked(Player, 2) or CellAttacked(Player, 3) then   //   Intermediate squares are attacked
     exit(false);
    end;

  result := true;
  end;


procedure TBoard.ClearBoard;
  begin
  Knights := 0;
  Rooks := 0;
  Bishops := 0;
  Queens := 0;
  Pawns := 0;
  Kings := 0;

  WhitePegs := 0;
  BlackPegs := 0;

  EnPassant := 0;
  MovedPieces := 0;

  ToPlay := White;
  TurnNumber := 1;
  HalfCount := 0;
  Hash := 0;
  PawnHash := 0;
  GameStage := 0;
  end;


procedure TBoard.Reset;
  begin
  if IsInitialized[0] = 0 then
    begin
    FillHashTables;
    MakeKnightMasks;
    MakeKingMasks;
    MakeBishopMasks;
    MakeRookMasks;
    MakePawnMasks;
    MakeRookLookUp;
    MakeBishopLookUp;

    IsInitialized[0] := -1;
    end;

  BoardFromFEN('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1', Self);
  end;


procedure TBoard.CalcHash;
  var
    k, epCell : integer;
    pegs, CastleFlags : UInt64;

  begin
  Hash := 0;

  // white pegs

  pegs := WhitePegs and Kings;
  k := PopLowBit_Alt(pegs);
  Hash := Hash xor HashTable[White, King, k];

  pegs := WhitePegs and Queens;
  if pegs <> 0 then
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[White, Queen, k];
    end;

  pegs := WhitePegs and Rooks;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[White, Rook, k];
    end;

  pegs := WhitePegs and Bishops;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[White, Bishop, k];
    end;

  pegs := WhitePegs and Knights;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[White, Knight, k];
    end;

  pegs := WhitePegs and Pawns;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[White, Pawn, k];
    end;

  // black pegs

  pegs := BlackPegs and Kings;
  k := PopLowBit_Alt(pegs);
  Hash := Hash xor HashTable[Black, King, k];

  pegs := BlackPegs and Queens;
  if pegs <> 0 then
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[Black, Queen, k];
    end;

  pegs := BlackPegs and Rooks;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[Black, Rook, k];
    end;

  pegs := BlackPegs and Bishops;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[Black, Bishop, k];
    end;

  pegs := BlackPegs and Knights;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[Black, Knight, k];
    end;

  pegs := BlackPegs and Pawns;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    Hash := Hash xor HashTable[Black, Pawn, k];
    end;

  if ToPlay = black then
    Hash := Hash xor PlayerHash;

  CastleFlags := PEXT(MovedPieces, UInt64($9100000000000091));
  Hash := Hash xor CastleHash[CastleFlags];      // add CastleFlags to Hash

  if Enpassant <> 0 then
    begin
    epCell := GetLowBit_Alt(Enpassant);
    Hash := Hash xor epHash[epCell];             // add last epCell to Hash
    end;
  end;


procedure TBoard.CalcPawnHash;
  var
    k : integer;
    pegs : UInt64;

  begin
  PawnHash := 0;

  // white pegs

 { pegs := WhitePegs and Kings;
  k := PopLowBit_Alt(pegs);
  PawnHash := PawnHash xor HashTable[White, King, k];   }

  pegs := WhitePegs and Pawns;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    PawnHash := PawnHash xor HashTable[White, Pawn, k];
    end;

  // black pegs

{  pegs := BlackPegs and Kings;
  k := PopLowBit_Alt(pegs);
  PawnHash := PawnHash xor HashTable[Black, King, k];  }

  pegs := BlackPegs and Pawns;
  while pegs <> 0 do
    begin
    k := PopLowBit_Alt(pegs);
    PawnHash := PawnHash xor HashTable[Black, Pawn, k];
    end;

  if ToPlay = black then
    PawnHash := PawnHash xor PlayerHash;
  end;


procedure TBoard.FillHashTables;
  var
    i, j, k : integer;
    PRNG : TPRNG;

  begin
  // PRNG_Randomize;
  PRNG.Seed(HashTableSeed_0, HashTableSeed_1);

  for i := White to Black do
    for j := Pawn to King do
      for k := 0 to 63 do
        TBoard.HashTable[i, j, k] := PRNG.Rand64;

  TBoard.PlayerHash := PRNG.Rand64;

  for i := 0 to 63 do
    TBoard.CastleHash[i] := PRNG.Rand64;

  for i := 0 to 63 do
    TBoard.epHash[i] := PRNG.Rand64;
  end;


procedure TBoard.MakeKnightMasks;
  var
    i : integer;

  begin
  for i := 0 to 63 do
    KnightMask[i] := 0;

  for i := 0 to 63 do
    begin
    if (i + 17 < 64) and ( (i + 17) mod 8 > i mod 8) then
      setbit(KnightMask[i], i + 17);

    if (i + 10 < 64) and ( (i + 10) mod 8 > i mod 8) then
      setbit(KnightMask[i], i + 10);

    if (i - 6 >= 0) and ( (i - 6) mod 8 > i mod 8) then
      setbit(KnightMask[i], i - 6);

    if (i - 15 >= 0) and ( (i - 15) mod 8 > i mod 8) then
      setbit(KnightMask[i], i - 15);

    if (i - 17 >= 0) and ( (i - 17) mod 8 < i mod 8) then
      setbit(KnightMask[i], i - 17);

    if (i - 10 >= 0) and ( (i - 10) mod 8 < i mod 8) then
      setbit(KnightMask[i], i - 10);

    if (i + 6 < 64) and ( (i + 6) mod 8 < i mod 8) then
      setbit(KnightMask[i], i + 6);

    if (i + 15 < 64) and ( (i + 15) mod 8 < i mod 8) then
      setbit(KnightMask[i], i + 15);
    end;
  end;


procedure TBoard.MakeKingMasks;
  var
    i : integer;

  begin
  for i := 0 to 63 do
    KingMask[i] := 0;

  for i := 0 to 63 do
    begin
    if (i + 9 < 64) and (i mod 8 <> 7) then
      setbit(KingMask[i], i+9);

    if i mod 8 <> 7 then
      setbit(KingMask[i], i+1);

    if (i - 7 >= 0) and (i mod 8 <> 7) then
      setbit(KingMask[i], i-7);

    if (i - 8 >= 0) then
      setbit(KingMask[i], i-8);

    if (i - 9 >= 0) and (i mod 8 <> 0) then
      setbit(KingMask[i], i-9);

    if i mod 8 <> 0 then
      setbit(KingMask[i], i-1);

    if (i + 7 < 64) and (i mod 8 <> 0) then
      setbit(KingMask[i], i+7);

    if i + 8 < 64 then
      setbit(KingMask[i], i+8);
    end;
  end;


procedure TBoard.MakeBishopMasks;
  var
    i, k : integer;

  begin
  for i := 0 to 63 do
    BishopUpMask[i] := 0;

  for i := 0 to 63 do
    begin
    for k := -7 to -1 do
      if (i + k*9 >= 0) and ((i + k*9) mod 8 < i mod 8) then
         setbit(BishopUpMask[i], i + k*9);

    for k := 1 to 7 do
      if (i + k*9 < 64) and  ((i + k*9) mod 8 > i mod 8) then
         setbit(BishopUpMask[i], i + k*9);
    end;

  for i := 0 to 63 do
    BishopDownMask[i] := 0;

  for i := 0 to 63 do
    begin
    for k := -7 to -1 do
      if (i + k*7 >= 0) and ((i + k*7) mod 8 > i mod 8) then
         setbit(BishopDownMask[i], i + k*7);

    for k := 1 to 7 do
      if (i + k*7 < 64) and  ((i + k*7) mod 8 < i mod 8) then
         setbit(BishopDownMask[i], i + k*7);
    end;

  for i := 0 to 63 do
    BishopMask[i] := BishopUpMask[i] or BishopDownMask[i];
  end;


procedure TBoard.MakeRookMasks;
  var
    i, k : integer;

  begin
  for i := 0 to 63 do
    RookHorzMask[i] := 0;

  for i := 0 to 63 do
    begin
    for k := -7 to -1 do
      if (i + k >= 0) and ((i + k) mod 8 < i mod 8) then
         setbit(RookHorzMask[i], i + k);

    for k := 1 to 7 do
      if (i + k < 64) and ((i + k) mod 8 > i mod 8)then
         setbit(RookHorzMask[i], i + k);
    end;

  for i := 0 to 63 do
    RookVertMask[i] := 0;

  for i := 0 to 63 do
    begin
    for k := -7 to -1 do
      if i + k*8 >= 0 then
         setbit(RookVertMask[i], i + k*8);

    for k := 1 to 7 do
      if i + k*8 < 64 then
         setbit(RookVertMask[i], i + k*8);
    end;

  for i := 0 to 63 do
    RookMask[i] := RookHorzMask[i] or RookVertMask[i];
  end;


procedure TBoard.MakePawnMasks;
  var
    i : integer;

  begin
  for i := 0 to 63 do
    BlackPawnMoves[i] := 0;

  for i := 8 to 55 do
    setbit(BlackPawnMoves[i], i+8);

  for i := 8 to 15 do
    setbit(BlackPawnMoves[i], i+16);


  for i := 0 to 63 do
    WhitePawnMoves[i] := 0;

  for i := 8 to 55 do
    setbit(WhitePawnMoves[i], i-8);

  for i := 48 to 55 do
    setbit(WhitePawnMoves[i], i-16);


  for i := 0 to 63 do
    BlackPawnCaptures[i] := 0;

  for i := 0 to 55 do                 // need coverage of cells 0..7 as these may be used during test for King check
    begin
    if i mod 8 <> 0 then
      setbit(BlackPawnCaptures[i], i+7);
    if i mod 8 <> 7 then
      setbit(BlackPawnCaptures[i], i+9);
    end;

  for i := 0 to 63 do
    WhitePawnCaptures[i] := 0;

  for i := 8 to 63 do                // need coverage of cells 56..63 as these may be used during test for King check
    begin
    if i mod 8 <> 0 then
      setbit(WhitePawnCaptures[i], i-9);
    if i mod 8 <> 7 then
      setbit(WhitePawnCaptures[i], i-7);
    end;
  end;


procedure TBoard.MakeRookLookUp;
  var
    cell, j, k : integer;
    Mask, Temp, Ans : UInt64;

  begin
  for cell := 0 to 7 do
    for j := 0 to 127 do
      RookLookUp[cell, j] := 0;

  for cell := 0 to 7 do
    begin
    Mask :=  RookHorzMask[cell];
    for j := 0 to 127 do
      begin
      Temp := PDEP(j, Mask);
      Ans := 0;
      for k := -1 downto -7 do
        if cell + k >= 0 then
           begin
           setbit(Ans, cell + k);
           if GetBit(Temp, cell + k) = true then break;
           end;

      for k := 1 to 7 do
        if cell + k < 8 then
           begin
           setbit(Ans, cell + k);
           if GetBit(Temp, cell + k) = true then break;
           end;

      RookLookUp[cell, j] := byte(PEXT(Ans, Mask));
      end;
    end;
  end;


procedure TBoard.MakeBishopLookUp;
  var
    cell, j, k : integer;
    Mask, Temp, Ans : UInt64;

  begin
  for cell := 0 to 63 do
    for j := 0 to 127 do
      BishopUpLookUp[cell, j] := 0;

  for cell := 0 to 63 do
    begin
    Mask :=  BishopUpMask[cell];
    for j := 0 to 127 do
      begin
      Temp := PDEP(j, Mask);
      Ans := 0;
      for k := -1 downto -7 do
        if (cell + k*9 >= 0) and ((cell + k*9) mod 8 < cell mod 8) then
           begin
           setbit(Ans, cell + k*9);
           if GetBit(Temp, cell + k*9) = true then break;
           end;

      for k := 1 to 7 do
        if (cell + k*9 < 64) and  ((cell + k*9) mod 8 > cell mod 8) then
           begin
           setbit(Ans, cell + k*9);
           if GetBit(Temp, cell + k*9) = true then break;
           end;
      BishopUpLookUp[cell, j] := byte(PEXT(Ans, Mask));
      end;
    end;

  for cell := 0 to 63 do
    for j := 0 to 127 do
      BishopDownLookUp[cell, j] := 0;

  for cell := 0 to 63 do
    begin
    Mask :=  BishopDownMask[cell];
    for j := 0 to 127 do
      begin
      Temp := PDEP(j, Mask);
      Ans := 0;
      for k := -1 downto -7 do
        if (cell + k*7 >= 0) and ((cell + k*7) mod 8 > cell mod 8) then
           begin
           setbit(Ans, cell + k*7);
           if GetBit(Temp, cell + k*7) = true then break;
           end;

      for k := 1 to 7 do
        if (cell + k*7 < 64) and  ((cell + k*7) mod 8 < cell mod 8) then
           begin
           setbit(Ans, cell + k*7);
           if GetBit(Temp, cell + k*7) = true then break;
           end;
      BishopDownLookUp[cell, j] := byte(PEXT(Ans, Mask));
      end;
    end;
  end;


function IsBoardLegal(const Board : TBoard) : boolean;
  begin
  result := true;

  if (Board.KingInCheck(White)) and (Board.ToPlay = Black) then
    exit(false);

  if (Board.KingInCheck(Black)) and (Board.ToPlay = White) then
    exit(false);

  if Board.AdjacentKings then
    exit(false);

  if (Board.KingInCheck(White) = true) and (Board.KingInCheck(Black) = true) then
    exit(false);

  if Board.WhitePegs and Board.BlackPegs <> 0 then
    exit(false);

  if Bitcount(Board.Kings) <> 2 then
    exit(false);

  if Bitcount(Board.Kings and Board.WhitePegs) <> 1 then
    exit(false);

  if Bitcount(Board.Kings and Board.BlackPegs) <> 1 then
    exit(false);

  if (Board.WhitePegs or Board.BlackPegs) <> (Board.Kings or Board.Queens or Board.Rooks or Board.Bishops or Board.Knights or Board.Pawns) then
    exit(false);

  if Board.Kings and Board.Queens and Board.Rooks and Board.Bishops and Board.Knights and Board.Pawns <> 0 then
    exit(false);


  if Board.Pawns and (Board.Kings or Board.Queens or Board.Rooks or Board.Bishops or Board.Knights) <> 0 then
    exit(false);

  if Board.Knights and (Board.Kings or Board.Queens or Board.Rooks or Board.Bishops or Board.Pawns) <> 0 then
    exit(false);

  if Board.Bishops and (Board.Kings or Board.Queens or Board.Rooks or Board.Knights or Board.Pawns) <> 0 then
    exit(false);

  if Board.Rooks and (Board.Kings or Board.Queens or Board.Bishops or Board.Knights or Board.Pawns) <> 0 then
    exit(false);

  if Board.Queens and (Board.Kings or Board.Rooks or Board.Bishops or Board.Knights or Board.Pawns) <> 0 then
    exit(false);

  if Board.Kings and (Board.Queens or Board.Rooks or Board.Bishops or Board.Knights or Board.Pawns) <> 0 then
    exit(false);


  if (Board.Pawns and Board.WhitePegs and $FF) <> 0 then
    exit(false);

  if (Board.Pawns and Board.WhitePegs and $FF00000000000000) <> 0 then
    exit(false);

  if (Board.Pawns and Board.BlackPegs and $FF00000000000000) <> 0 then
    exit(false);

  if (Board.Pawns and Board.BlackPegs and $FF) <> 0 then
    exit(false);


  if ((Board.MovedPieces and $80) = 0) and ((Board.BlackPegs and Board.Rooks and $80) = 0) then
    exit(false);

  if ((Board.MovedPieces and $10) = 0) and ((Board.BlackPegs and Board.Kings and $10) = 0) then
    exit(false);

  if ((Board.MovedPieces and $1) = 0) and ((Board.BlackPegs and Board.Rooks and $1) = 0) then
    exit(false);

  if ((Board.MovedPieces and $8000000000000000) = 0) and ((Board.WhitePegs and Board.Rooks and $8000000000000000) = 0) then
    exit(false);

  if ((Board.MovedPieces and $1000000000000000) = 0) and ((Board.WhitePegs and Board.Kings and $1000000000000000) = 0) then
    exit(false);

  if ((Board.MovedPieces and $100000000000000) = 0) and ((Board.WhitePegs and Board.Rooks and $100000000000000) = 0) then
    exit(false);
  end;


procedure ValidateBoard(Board : TBoard);
  var
    OutputMessage : string;

  begin
  OutputMessage := '';

  if (Board.KingInCheck(White)) and (Board.ToPlay = Black) then
    OutputMessage := 'Board Invalid - White King in check and Black is to play';

  if (Board.KingInCheck(Black)) and (Board.ToPlay = White) then
    OutputMessage := 'Board Invalid - Black King in check and White is to play';

  if Board.AdjacentKings = true then
    OutputMessage := 'Board Invalid - Black King adjacent to White King';

  if (Board.KingInCheck(White) = true) and (Board.KingInCheck(Black) = true) then
    OutputMessage := 'Board Invalid - Both Kings in check';

  if Bitcount(Board.Kings) <> 2 then
    OutputMessage := 'Board Invalid - Number of Kings incorrect';

  if Bitcount(Board.Kings and Board.WhitePegs) <> 1 then
    OutputMessage := 'Board Invalid - One White King required';

  if Bitcount(Board.Kings and Board.BlackPegs) <> 1 then
    OutputMessage := 'Board Invalid - One Black King required';

  if Board.WhitePegs and Board.BlackPegs <> 0 then
    OutputMessage := 'Board Invalid - Black piece and White piece share same square';

  if (Board.WhitePegs or Board.BlackPegs) <> (Board.Kings or Board.Queens or Board.Rooks or Board.Bishops or Board.Knights or Board.Pawns) then
    OutputMessage := 'Board Invalid - WhitePegs & BlackPegs inconsistent with Piece Pegs';

  if Board.Kings and Board.Queens and Board.Rooks and Board.Bishops and Board.Knights and Board.Pawns <> 0 then
    OutputMessage := 'Board Invalid - Inconsistent Piece Pegs';


  if (Board.Pawns and Board.WhitePegs and $FF) <> 0 then
    OutputMessage := 'Board Invalid - White pawn on Rank #8';

  if (Board.Pawns and Board.WhitePegs and $FF00000000000000) <> 0 then
   OutputMessage := 'Board Invalid - White pawn on Rank #1';

  if (Board.Pawns and Board.BlackPegs and $FF00000000000000) <> 0 then
    OutputMessage := 'Board Invalid - Black pawn on Rank #1';

  if (Board.Pawns and Board.BlackPegs and $FF) <> 0 then
    OutputMessage := 'Board Invalid - Black pawn on Rank #8';


  if ((Board.MovedPieces and $80) = 0) and ((Board.BlackPegs and Board.Rooks and $80) = 0) then
    OutputMessage := 'Board Invalid - Castling rights inconsistent with empty Black Kings-Rook Square';

  if ((Board.MovedPieces and $10) = 0) and ((Board.BlackPegs and Board.Kings and $10) = 0) then
    OutputMessage := 'Board Invalid - Castling rights inconsistent with empty Black King Square';

  if ((Board.MovedPieces and $1) = 0) and ((Board.BlackPegs and Board.Rooks and $1) = 0) then
    OutputMessage := 'Board Invalid - Castling rights inconsistent with empty Black Queens-Rook Square';

  if ((Board.MovedPieces and $8000000000000000) = 0) and ((Board.WhitePegs and Board.Rooks and $8000000000000000) = 0) then
    OutputMessage := 'Board Invalid - Castling rights inconsistent with empty White Kings-Rook Square';

  if ((Board.MovedPieces and $1000000000000000) = 0) and ((Board.WhitePegs and Board.Kings and $1000000000000000) = 0) then
    OutputMessage := 'Board Invalid - Castling rights inconsistent with empty White King Square';

  if ((Board.MovedPieces and $100000000000000) = 0) and ((Board.WhitePegs and Board.Rooks and $100000000000000) = 0) then
    OutputMessage := 'Board Invalid - Castling rights inconsistent with empty White Queens-Rook Square';
  end;


procedure BoardFromFEN(const FEN : string; var Board : TBoard);
  var
  RankNo, FileNo, cursor, cell : integer;
  c : Char;
  tempstr : string;

  label
    Last;

  begin
  Board.ClearBoard;

  // Piece postions

  RankNo := 0;
  FileNo := 0;
  cursor := 1;

    repeat
    c := Char(Fen[cursor]);
    cell := RankNo*8 + FileNo;
      case c of
      Char('K') :  begin SetBit(Board.Kings, cell); SetBit(Board.WhitePegs, cell); end;
      Char('Q') :  begin SetBit(Board.Queens, cell); SetBit(Board.WhitePegs, cell); end;
      Char('R') :  begin SetBit(Board.Rooks, cell); SetBit(Board.WhitePegs, cell); end;
      Char('B') :  begin SetBit(Board.Bishops, cell); SetBit(Board.WhitePegs, cell); end;
      Char('N') :  begin SetBit(Board.Knights, cell); SetBit(Board.WhitePegs, cell); end;
      Char('P') :  begin SetBit(Board.Pawns, cell); SetBit(Board.WhitePegs, cell); end;
      Char('k') :  begin SetBit(Board.Kings, cell); SetBit(Board.BlackPegs, cell); end;
      Char('q') :  begin SetBit(Board.Queens, cell); SetBit(Board.BlackPegs, cell); end;
      Char('r') :  begin SetBit(Board.Rooks, cell); SetBit(Board.BlackPegs, cell); end;
      Char('b') :  begin SetBit(Board.Bishops, cell); SetBit(Board.BlackPegs, cell); end;
      Char('n') :  begin SetBit(Board.Knights, cell); SetBit(Board.BlackPegs, cell); end;
      Char('p') :  begin SetBit(Board.Pawns, cell); SetBit(Board.BlackPegs, cell); end;

      Char('2') :  inc(FileNo, 1);
      Char('3') :  inc(FileNo, 2);
      Char('4') :  inc(FileNo, 3);
      Char('5') :  inc(FileNo, 4);
      Char('6') :  inc(FileNo, 5);
      Char('7') :  inc(FileNo, 6);
      Char('/') :  begin FileNo := -1; inc(RankNo); end;
      end;
    inc(FileNo);
    inc(cursor);
    until c = Char(' ');

  // To play

  c := Char(Fen[cursor]);
  if c = 'w' then
    Board.ToPlay := White
   else
    Board.ToPlay := Black;
  inc(cursor, 2);

  // Castling rights

  Board.MovedPieces := UInt64($9100000000000091);  // assume no castling
    repeat
    c := Char(Fen[cursor]);
      case c of
      Char('k') :  begin ClearBit(Board.MovedPieces, 7); ClearBit(Board.MovedPieces, 4); end;
      Char('q') :  begin ClearBit(Board.MovedPieces, 0); ClearBit(Board.MovedPieces, 4); end;
      Char('K') :  begin ClearBit(Board.MovedPieces, 63); ClearBit(Board.MovedPieces, 60); end;
      Char('Q') :  begin ClearBit(Board.MovedPieces, 56); ClearBit(Board.MovedPieces, 60); end;
      Char('-') :  begin inc(cursor, 2); break; end;
      end;
    inc(cursor);
    until c = Char(' ');

  // Enpassant

  if cursor >= length(Fen) then
    GoTo Last;

  c := Char(Fen[cursor]);
  if c <> Char('-') then
    begin
    FileNo := Ord(c) - Ord(Char('a'));

    inc(cursor);
    c := Char(Fen[cursor]);

    if c = '3' then
      SetBit(Board.EnPassant, 40 + FileNo);

    if c = '6' then
      SetBit(Board.EnPassant, 16 + FileNo);
    end;

  inc(cursor, 2);

  if cursor >= length(Fen) then
    GoTo Last;

  // HalfCount

  c := Char(Fen[cursor]);
  if c = '-' then
    begin
    Board.HalfCount := 0;
    inc(cursor);
    end
   else
    begin
    tempstr := '';
    while c <> ' ' do
      begin
      tempstr :=  tempstr + c;
      inc(cursor);
      c := Char(Fen[cursor]);
      end;
    Board.HalfCount := StrToInt(tempstr);
    end;

  inc(Cursor);

  // TurnNumber

  if cursor >= length(Fen) then
    GoTo Last;

  c := Char(Fen[cursor]);
  if c = '-' then Board.TurnNumber := 1
   else
    begin
    tempstr := c;
    while (cursor < length(Fen)) and (Fen[cursor+1] <> Char(' ')) do
      begin
      inc(cursor);
      c := Char(Fen[cursor]);
      tempstr :=  tempstr + c;
      end;
    Board.TurnNumber := StrToInt(tempstr);
    end;

  Last:

  Board.CalcHash;
  Board.CalcPawnHash;
  Board.CalcGameStage;

  ValidateBoard(Board);
  end;


procedure BoardToFen(var FEN : string; const Board : TBoard);
  var
    cell, RankNo, FileNo : integer;
    piece, color, blank, t : integer;
    tempstr : string;

  begin
  FEN := '';
  blank := 0;

  RankNo := 0;
  FileNo := 0;

  while rankNo <= 7 do
    begin
    cell := RankNo*8 + FileNo;
    piece := Board.GetPiece_asm(cell);
    color := Board.GetColor(cell);
    if color = none then
      color := 0;
    t := color * 6 + piece;

      case t of
       0 : inc(blank);
       1 : tempstr := 'P';
       2 : tempstr := 'N';
       3 : tempstr := 'B';
       4 : tempstr := 'R';
       5 : tempstr := 'Q';
       6 : tempstr := 'K';
       7 : tempstr := 'p';
       8 : tempstr := 'n';
       9 : tempstr := 'b';
      10 : tempstr := 'r';
      11 : tempstr := 'q';
      12 : tempstr := 'k';
      end;

    inc(FileNo);

    if blank = 0 then
      FEN := FEN + tempstr;

    if (t > 0) and (blank > 0) then
      begin
      FEN := FEN + IntToStr(blank) + tempstr;
      blank := 0;
      end;

    if FileNo = 8 then
      begin
      if Blank <> 0 then
        FEN := FEN + IntToStr(blank);
      Blank := 0;
      if RankNo <> 7 then
        FEN := FEN + '/';
      FileNo := 0;
      inc(RankNo);
      end;
    end;

  // To play

  if Board.ToPlay = white then
    FEN := FEN + ' w'
   else
    FEN := FEN + ' b';

  // Castling rights

  tempstr := ' ';
  if ((Board.MovedPieces and UInt64($9000000000000000)) = 0) and ((Board.Rooks and Board.WhitePegs and UInt64($8000000000000000)) <> 0) then
    tempstr := tempstr + 'K';
  if ((Board.MovedPieces and UInt64($1100000000000000)) = 0) and ((Board.Rooks and Board.WhitePegs and UInt64($0100000000000000)) <> 0) then
    tempstr := tempstr + 'Q';
  if ((Board.MovedPieces and $90) = 0) and ((Board.Rooks and Board.BlackPegs and $80) <> 0) then
    tempstr := tempstr + 'k';
  if ((Board.MovedPieces and $11) = 0) and ((Board.Rooks and Board.BlackPegs and $1) <> 0) then
    tempstr := tempstr + 'q';
  if tempstr = ' ' then
    tempstr := ' -';
  FEN := FEN + tempstr;

  // Enpassant

  if Board.EnPassant <> 0 then
    begin
    cell := GetLowBit_Alt(Board.EnPassant);
    RankNo := 7 - (cell shr 3);
    FileNo := cell and $7;
    FEN := FEN + ' ' + Char( Ord('a') + FileNo) + Char( Ord('1') + RankNo);
    end
   else
    FEN := FEN + ' -' ;

  // HalfCount

  FEN := FEN + ' ' + IntToStr(Board.HalfCount);

  // TurnNumber

  FEN := FEN + ' ' + IntToStr(Board.TurnNumber);
  end;


function MoveToStr(Move : UInt64) : string;
  var
    source, Dest : integer;

  begin
  Source := Move and $3F;
  Dest :=  (Move shr 6) and $3F;

  result := '(' + IntToSTr(Source) + '-' + IntToStr(Dest) + '), ';
  end;


function PVToStr(PV : TMoveArray) : string;
  var
    i : integer;

  begin
  result := '';

  if PV[0] > 1 then
    for i := 1 to PV[0]-1 do
      result := result + PV[i].ToStr + ' ';

  if PV[0] <> 0 then
    result := result  + PV[PV[0]].ToStr;
  end;


function MovesToStr(moves : TMoveArray) : string;
  var
    i : integer;

  begin
  result := '';

  if moves[0] > 0 then
    for i := 1 to moves[0]-1 do
      result := result + moves[i].ToStr + {':' + IntToStr(PV[i].score) +} ' ';

  if moves[0] <> 0 then
    result := result  + moves[moves[0]].ToStr {+ ':' + IntToStr(PV[i].score)};
  end;


function PVToStrExt(PV : TPVArray) : string;
  var
    i : integer;

  begin
  result := '';
  for i := 1 to PV[0]-1 do
    result := result + PV[i].ToStr + ', ';

  if PV[0] <> 0 then
    result := result + PV[PV[0]].ToStr;
  end;


function CopyBoard(const Board: TBoard) : TBoard;
  begin
  Result.Knights := Board.Knights;
  Result.Rooks := Board.Rooks;
  Result.Bishops := Board.Bishops;
  Result.Queens := Board.Queens;
  Result.Pawns := Board.Pawns;
  Result.Kings := Board.Kings;

  Result.WhitePegs := Board.WhitePegs;
  Result.BlackPegs := Board.BlackPegs;

  Result.EnPassant := Board.EnPassant;
  Result.MovedPieces := Board.MovedPieces;

  Result.ToPlay := Board.ToPlay;
  Result.TurnNumber := Board.TurnNumber;
  Result.HalfCount := Board.HalfCount;

  Result.Hash := Board.Hash;
  Result.PawnHash := Board.PawnHash;
  Result.GameStage := Board.GameStage;
  end;

end.




