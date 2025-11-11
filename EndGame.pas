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


unit EndGame;

interface

uses
  System.Math, Winapi.Windows, GameDef, Common, System.Classes;

const

  WhiteKeySqrMask : array[0..63] of UInt64 =
   ($0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000,
    $0000000000000000, $0000000000000507, $0000000000000A0E, $000000000000141C, $0000000000002838, $0000000000005070, $000000000000A0E0, $0000000000000000,
    $0000000000000000, $0000000000000707, $0000000000000E0E, $0000000000001C1C, $0000000000003838, $0000000000007070, $000000000000E0E0, $0000000000000000,
    $0000000000000202, $0000000000070700, $00000000000E0E00, $00000000001C1C00, $0000000000383800, $0000000000707000, $0000000000E0E000, $0000000000004040,
    $0000000000000202, $0000000000070000, $00000000000E0000, $00000000001C0000, $0000000000380000, $0000000000700000, $0000000000E00000, $0000000000004040,
    $0000000000000202, $0000000007000000, $000000000E000000, $000000001C000000, $0000000038000000, $0000000070000000, $00000000E0000000, $0000000000004040,
    $0000000000000202, $0000000700000000, $0000000E00000000, $0000001C00000000, $0000003800000000, $0000007000000000, $000000E000000000, $0000000000004040,
    $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000);

  BlackKeySqrMask : array[0..63] of UInt64 =
   ($0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000,
    $0202000000000000, $0000000007000000, $000000000E000000, $000000001C000000, $0000000038000000, $0000000070000000, $00000000E0000000, $4040000000000000,
    $0202000000000000, $0000000700000000, $0000000E00000000, $0000001C00000000, $0000003800000000, $0000007000000000, $000000E000000000, $4040000000000000,
    $0202000000000000, $0000070000000000, $00000E0000000000, $00001C0000000000, $0000380000000000, $0000700000000000, $0000E00000000000, $4040000000000000,
    $0202000000000000, $0007070000000000, $000E0E0000000000, $001C1C0000000000, $0038380000000000, $0070700000000000, $00E0E00000000000, $4040000000000000,
    $0000000000000000, $0707000000000000, $0E0E000000000000, $1C1C000000000000, $3838000000000000, $7070000000000000, $E0E0000000000000, $0000000000000000,
    $0000000000000000, $0705000000000000, $0E0A000000000000, $1C14000000000000, $3828000000000000, $7050000000000000, $E0A0000000000000, $0000000000000000,
    $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000, $0000000000000000);


  FlipLookup : array[0..63] of integer =
      (56, 57, 58, 59, 60, 61, 62, 63,
       48, 49, 50, 51, 52, 53, 54, 55,
       40, 41, 42, 43, 44, 45, 46, 47,
       32, 33, 34, 35, 36, 37, 38, 39,
       24, 25, 26, 27, 28, 29, 30, 31,
       16, 17, 18, 19, 20, 21, 22, 23,
        8,  9, 10, 11, 12, 13, 14, 15,
        0,  1,  2,  3,  4,  5,  6,  7);


type
  TEvalFunction = function(const Board : TBoard; Eval : integer) : integer;

function IndexFromBoard(const Board : TBoard) : UInt64;

var
  EndGameLookup : Array[0..1330] of TEvalFunction;

implementation

uses
  Search;

function Distance(Cell1, Cell2 : integer) : integer;
  begin
  result := max( abs(Cell1 shr 3 - Cell2 shr 3), abs(Cell1 and $7 - Cell2 and $7));
  end;


function ManhattanDistance(Cell1, Cell2 : integer) : integer;
  begin
  result := abs(Cell1 shr 3 - Cell2 shr 3) + abs(Cell1 and $7 - Cell2 and $7);
  end;


function ShiftRight(Pegs : UInt64) : UInt64;
  begin
  result := (Pegs shl 1) and $FEFEFEFEFEFEFEFE;
  end;

function ShiftLeft(Pegs : UInt64) : UInt64;
  begin
  result := (Pegs shr 1) and $7F7F7F7F7F7F7F7F;
  end;

function ShiftUp(Pegs : UInt64) : UInt64;
  begin
  result := (Pegs shr 8);
  end;

function ShiftDown(Pegs : UInt64) : UInt64;
  begin
  result := (Pegs shl 8);
  end;

function KingRange(Cell : integer; Mask : UInt64) : UInt64;
  var
    Fill, temp : UInt64;

  begin
  Fill := UInt64($1) shl cell;
    repeat
    temp := Fill;
    Fill := Fill or ShiftUp(Fill);
    Fill := Fill or ShiftDown(Fill);
    Fill := Fill or ShiftRight(Fill);
    Fill := Fill or ShiftLeft(Fill);
    Fill := Fill and not Mask;
    until
    temp = Fill;

  result := fill;
  end;


function KingOnKeySquare(const Board : TBoard; Player, Cell : integer) : boolean;

  begin
  if Player = white then
    result := ((WhiteKeySqrMask[Cell] and Board.WhitePegs and Board.Kings) <> 0)
   else
    result := ((BlackKeySqrMask[Cell] and Board.BlackPegs and Board.Kings) <> 0);
  end;


function IndexFromBoard(const Board : TBoard) : UInt64;
  var
    index, cell, piece : integer;
    pegs : UInt64;

  begin
  result := 0;
  Pegs := Board.WhitePegs and not(Board.Kings);
  while pegs <> 0 do
    begin
    result := result * 11;
    Cell := PopLowBit_Alt(Pegs);
    Piece := Board.GetPiece_asm(Cell);

    result := result + piece;
    end;

  Pegs := Board.BlackPegs and not(Board.Kings);
  while pegs <> 0 do
    begin
    result := result * 11;
    Cell := PopLowBit_Alt(Pegs);
    Piece := Board.GetPiece_asm(Cell);

    result := result + piece + 5;
    end;
  end;


function Evaluate_kp_K(const Board : TBoard; Eval : integer) : integer;
  var
    PawnCell, PromotionCell, KingCell, OpponentKingCell : integer;

  begin
  PawnCell := GetLowBit_Alt(Board.Pawns);

  if (Board.Pawns and Board.WhitePegs) <> 0 then  // white winning
    begin
    OpponentKingCell := GetLowBit_Alt(Board.Kings and Board.BlackPegs);
    KingCell := GetLowBit_Alt(Board.Kings and Board.WhitePegs);
    PromotionCell := PawnCell and $7;

    if min(5, Distance(PawnCell, PromotionCell)) < Distance(OpponentKingCell, PromotionCell) - Board.ToPlay then   // win for white
      exit(Eval);

    if distance(PawnCell, OpponentKingCell) < Distance(PawnCell, KingCell) - Board.ToPlay then   // draw : pawn can be captured
      exit(Eval div 64);

    if  KingOnKeySquare(Board, white, PawnCell) then             // win for white
      exit(Eval);
    end
   else
    begin      // black winning
    OpponentKingCell := GetLowBit_Alt(Board.Kings and Board.WhitePegs);
    KingCell := GetLowBit_Alt(Board.Kings and Board.BlackPegs);
    PromotionCell := 56 + (PawnCell and $7);

    if min(5, Distance(PawnCell, PromotionCell)) < Distance(OpponentKingCell, PromotionCell) - ( 1- Board.ToPlay) then   // win for black
      exit(Eval);

    if distance(PawnCell, OpponentKingCell) < Distance(PawnCell, KingCell) - (1 - Board.ToPlay) then    // draw : pawn can be captured
      exit(Eval div 64);

    if  KingOnKeySquare(Board, black, PawnCell) then           // win for black
      exit(Eval);
    end;

  result := Eval div 64;       // draw
  end;


function Evaluate_Draw(const Board : TBoard; Eval : integer) : integer;
  begin
  result := Eval div 64;
  end;


function WhiteBestCaseDraw(const Board : TBoard; Eval : integer) : integer;
  begin
  result := min(Eval, Eval div 64);
  end;

function BlackBestCaseDraw(const Board : TBoard; Eval : integer) : integer;
  begin
  result := max(Eval, Eval div 64);
  end;


function Evaluate_MostlyDraw(const Board : TBoard; Eval : integer) : integer;
  begin
  result := Eval div 32;
  end;


function Evaluate_BN(const Board : TBoard; Eval : integer) : integer;

   // Mate with KBN vs K. Drive the defending king towards an edge,
   // and then to a corner square with same color as the bishop
   // In tables below, target corner square has lowest value
   // i.e penalty is least for these squares

  const

    EdgePenalty : array[0..63] of integer =

     (  0,   0,   0,   0,   0,   0,   0,  0,
        0,  16,  16,  16,  16,  16,  16,  0,
        0,  16,  25,  25,  25,  25,  16,  0,
        0,  16,  25,  29,  29,  25,  16,  0,
        0,  16,  25,  29,  29,  25,  16,  0,
        0,  16,  25,  25,  25,  25,  16,  0,
        0,  16,  16,  16,  16,  16,  16,  0,
        0,   0,   0,   0,   0,   0,   0,  0);


     CornerDistance_W : array[0..63] of integer =

     (  0,  1,  2,  3,  4,  5,  6,  7,
        1,  1,  2,  3,  4,  5,  6,  6,
        2,  2,  2,  3,  4,  5,  5,  5,
        3,  3,  3,  3,  4,  4,  4,  4,
        4,  4,  4,  4,  3,  3,  3,  3,
        5,  5,  5,  4,  3,  2,  2,  2,
        6,  6,  5,  4,  3,  2,  1,  1,
        7,  6,  5,  4,  3,  2,  1,  0);


     CornerDistance_B : array[0..63] of integer =

     (  7,  6,  5,  4,  3,  2,  1,  0,
        6,  6,  5,  4,  3,  2,  1,  1,
        5,  5,  5,  4,  3,  2,  2,  2,
        4,  4,  4,  4,  3,  3,  3,  3,
        3,  3,  3,  3,  4,  4,  4,  4,
        2,  2,  2,  3,  4,  5,  5,  5,
        1,  1,  2,  3,  4,  5,  6,  6,
        0,  1,  2,  3,  4,  5,  6,  7);


  var
    blackKing, whiteKing, AttackingKing, DefendingKing, penalty, BishopCell, KnightCell : integer;
    knightDist, cornerDist, kingDist : integer;
    Mask, cornerMask, kingPrison : UInt64;

  begin
  BlackKing :=  GetLowBit_Alt(Board.Kings and Board.BlackPegs);
  WhiteKing :=  GetLowBit_Alt(Board.Kings and Board.WhitePegs);

  knightCell := GetLowBit_Alt(Board.Knights);
  bishopCell := GetLowBit_Alt(Board.Bishops);

  if (Board.Knights and Board.WhitePegs) <> 0 then
    begin
    DefendingKing := BlackKing; // white winning
    AttackingKing := WhiteKing;
    end
   else
    begin
    DefendingKing := WhiteKing;  // black winning
    AttackingKing := BlackKing;
    end;

  kingDist := Distance(WhiteKing, BlackKing);

  penalty :=  kingDist + ManhattanDistance(WhiteKing, BlackKing);     // goal : keep kings close
  penalty :=  penalty + EdgePenalty[DefendingKing];                   // goal : push defending king to edge of the board

  knightDist := Distance(DefendingKing, knightCell);
  penalty := penalty + max(knightDist - 3, 0) * 3;                    // goal : bring knight closer to the action

  if (Board.Bishops and whitesqr) <> 0 then
    begin
    cornerDist := CornerDistance_W[DefendingKing];
    cornerMask := $8000000000000001;
    end
   else
    begin
    cornerDist := CornerDistance_B[DefendingKing];
    cornerMask := $100000000000080;
    end;

  penalty := penalty + cornerDist * 19;                               // goal : push defending king towards mate corner

  Mask := Board.KnightMask[knightCell];
  Mask := Mask or Board.BishopAttack_asm(bishopCell);
  Mask := Mask or Board.KingMask[AttackingKing];

  kingPrison := KingRange(DefendingKing, Mask);                       // confine defending king and squeeze
  penalty := penalty + min(bitcount(kingPrison), 18);

  if (Board.Bishops and Board.BlackPegs) <> 0 then
    result := -Eval + penalty   // black is winning, so white is down on material and larger penalty is better for white
   else
    result := Eval - penalty;   // white is winning, so white is up material and smaller penalty is better for white
  end;


function Evaluate_EasyMate(const Board : TBoard; Eval : integer) : integer;

  const
    EdgeBonus : array[0..63] of integer =
           (36, 25, 16,  9,  9, 16, 25, 36,
            25, 16,  9,  4,  4,  9, 16, 25,
            16,  9,  4,  1,  1,  4,  9, 16,
             9,  4,  1,  0,  0,  1,  4,  9,
             9,  4,  1,  0,  0,  1,  4,  9,
            16,  9,  4,  1,  1,  4,  9, 16,
            25, 16,  9,  4,  4,  9, 16, 25,
            36, 25, 16,  9,  9, 16, 25, 36);

  var
    BlackKingCell, WhiteKingCell, DefeatedKingCell, index, material, bonus : integer;
    WhiteWinning : boolean;

 // Mate with K vs kxx.
 // Returns score with bonus from white perspective
 // Bonus to drive the black king towards the edge of board,
 // keep white king towards the centre,
 // and keep white king close to black king.

  begin

  BlackKingCell :=  GetLowBit_Alt(Board.Kings and Board.BlackPegs);
  WhiteKingCell :=  GetLowBit_Alt(Board.Kings and Board.WhitePegs);

  WhiteWinning := (bitcount(Board.WhitePegs) > 1);

  if WhiteWinning then         // white winning
    bonus := 8 * (EdgeBonus[BlackKingCell] - Distance(WhiteKingCell, BlackKingCell) * Distance(WhiteKingCell, BlackKingCell))
   else
    bonus := 8 * (EdgeBonus[WHiteKingCell] - Distance(WhiteKingCell, BlackKingCell) * Distance(WhiteKingCell, BlackKingCell));

  if ((board.ToPlay = White) and WhiteWinning) or ((board.ToPlay = black) and not WhiteWinning) then
    result := Eval + bonus
   else
    result := Eval - bonus;
  end;


procedure InitializeEndGameLookup;
  var i, index : integer;

  begin
  for i := low(EndGameLookup) to high(EndGameLookup) do
    EndGameLookup[i] := nil;

  // 2-man eval

  // k v K  : Draw
  index := 0;
  EndGameLookup[index] := Evaluate_Draw;


  // 3-man eval

  // KP v k
  index := pawn;
  EndGameLookup[index] := Evaluate_kp_K;

  // K v kp  :
  index := (pawn + 5);
  EndGameLookup[index] := Evaluate_kp_K;

  // K v kn  : Draw
  index := (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KN v k  : Draw
  index := knight;
  EndGameLookup[index] := Evaluate_Draw;

  // K v kb  : Draw
  index := (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KB v k  : Draw
  index := bishop;
  EndGameLookup[index] := Evaluate_Draw;

  // K v kr  : Simple Mate by Black
  index := (rook + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KR v k  : Simple Mate
  index := rook;
  EndGameLookup[index] := Evaluate_EasyMate;

  // K v kq  : Simple Mate
  index := (queen + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KQ v k  : Simple Mate
  index := queen;
  EndGameLookup[index] := Evaluate_EasyMate;


  // 4-man eval

  // KN v kn   : Draw
  index := knight * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KB v kb   : Draw
  index := bishop * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KB v kn  : Draw
  index := bishop * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KN v kb   : Draw
  index := knight * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KR v kr    : Draw
  index := rook * 11 + (rook + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KQ v kq    : Draw
  index := queen * 11 + (queen + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // K v knn    : Draw
  index := (knight + 5) * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KNN v k    : Draw
  index := knight * 11 + knight;
  EndGameLookup[index] := Evaluate_Draw;

  // K v kbb   : Simple Mate by Black
  index := (bishop + 5) * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KBB v k   : Simple Mate by White
  index := bishop * 11 + bishop;
  EndGameLookup[index] := Evaluate_EasyMate;

  // KR v kq   : Mate by Black
  index := rook * 11 + (queen + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KQ v kr   : Mate by White
  index := queen * 11 + (rook + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KB v kq   : Mate by Black
  index := bishop * 11 + (queen + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KQ v kb   : Mate by White
  index := queen * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KN v kq   : Mate by Black
  index := knight * 11 + (queen + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // KQ v kn   : Mate by White
  index := queen * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_EasyMate;

  // K v knb    : Mate
  index := (knight + 5) * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_BN;
  index := (bishop + 5) * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_BN;

  // KNB v k    : Mate
  index := knight * 11 + bishop;
  EndGameLookup[index] := Evaluate_BN;
  index := bishop * 11 + knight;
  EndGameLookup[index] := Evaluate_BN;

  // 5-man eval

  // KBB v kb  : Draw
  index := bishop * 121 + bishop * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KB v kbb   : Draw
  index := bishop * 121 + (bishop + 5) * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KBB v kr   : Draw
  //            : white win 8.8%  draw 90.5%  black win 0.7%
  index := bishop * 121 + bishop * 11 + (rook + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KR v kbb   : Draw
  //            : white win 0.7%  draw 90.5%  black win 8.8%
  index := rook * 121 + (bishop + 5) * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KN v knn   : Draw
  index := knight * 121 + (knight + 5) * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KNN v kn   : Draw
  index := knight * 121 + knight * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KB v knn   : Draw
  index := bishop * 121 + (knight + 5) * 11 + (knight + 5);
  EndGameLookup[index] := Evaluate_Draw;

  // KNN v kb   : Draw
  index := knight * 121 + knight * 11 + (bishop + 5);
  EndGameLookup[index] := Evaluate_Draw;

  end;


initialization

  InitializeEndGameLookup;


end.
