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


unit EndGame;

interface

uses
  System.Math, Winapi.Windows, GameDef, Common, System.Classes;

const
  MaterialHash : array[white..black, 0..29] of UInt64 =

  (($38F4CE22B412B425, $F594DB19D0B7C845, $A56F9F4CB8CB3FAB, $032A0BD7A806E72C, $4ACB4E1BE879E207, $21AF65E947D4F69F, $9AC14B0FBFBC5609, $A80259739D092042, $F1AA3172AC31FB6F,   // white pawn 0..8
    $6DCBBBA917A57843, $2AC80EDF8B9C2471, $7797D297A84147FE, $3709009FFB55342B,      // white knight     0, 1, 2, >2
    $7A38C076953B97E6, $C42062C86032D767, $14558239DECB0F4D, $652EDF916C4BE504,      // white bishop(w)  0, 1, 2, >2
    $E161CAAE4F177394, $AB985BD53993CB91, $94985E477AB8C825, $5499EE29285CE543,      // white bishop(b)  0, 1, 2, >2
    $0F43FE5038EAD367, $5510B9D904A371B2, $BAD83E3E71C380CE, $ACDEB7D186B4851F,      // white rook       0, 1, 2, >2
    $8A01CE3A2C9A7A61, $10A2861D684519B1, $3FE167DB101E0EDD, $137A5FE7A93F4649,      // white queen      0, 1, 2, >2
    $1B7DD93B5EE030A8),                                                              // white king       1

   ($B72CB9A01EC4EE88, $4D5398BA37ED58E3, $9CF7785716E94402, $AC9731B8AF0E3066, $80FDD21DC621AA1F, $C7C3B13CEB19663D, $65057F886B8BC518, $5765795CBA9F9BF2, $C8DD294CCA56E8FE,   // black pawn 0..8
    $BAADACC2EF1A84F8, $76070BCE1CC86256, $81E83EDB8CD483B9, $A73236E5DE9A10CA,      // black knight     0, 1, 2, >2
    $570330EF9C3B6CBE, $030181F0831F92A8, $E64921367CDCFA69, $52BA8419A1D75502,      // black bishop(w)  0, 1, 2, >2
    $221D070A21A59A5A, $5AD7A2DFBEA88C5D, $3E2002294879A896, $A52BCB76AD7E053C,      // black bishop(b)  0, 1, 2, >2
    $CAA18F7957DF1524, $420CD65B8D8BE7CA, $88B6450D4DF2A40C, $B788A3FE9E84BFAA,      // black rook       0, 1, 2, >2
    $75CC2C3D68AFC23C, $BDCB012CE31AB73B, $786F9A2754978132, $9BD988B855C923B1,      // black queen      0, 1, 2, >2
    $DACBB869272A058E));                                                             // black king       1

type
  TEvalFunction = function(const Board : TBoard; Eval : integer) : integer;

type
  TEndGameRec = record
    ID : UInt64;
    EvalFunction : TEvalFunction;
  end;

type
  T_EndGame_Table = record
    const
      TableSize = $1000;             // 2^12       = 4096
      TableMask = TableSize - 1 ;    // 2^12 - 1
      MaxLink = 4;

    var
      Table : Array[0..TableSize-1] of TEndGameRec;

    class operator Initialize (out Dest: T_EndGame_Table);

    procedure AddCase(MaterialHash : UInt64; EvalFunction : TEvalFunction);
    function RetrieveEval(MaterialHash : UInt64) : TEvalFunction;
    end;

function MaterialHashFromBoard(const Board : TBoard) : UInt64;
function WhiteMaterialHashFromBoard(const Board : TBoard) : UInt64;
function BlackMaterialHashFromBoard(const Board : TBoard) : UInt64;

var
  EndGameTable : T_Endgame_Table;

implementation

uses
  Search, Eval;

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


function WhiteMaterialHashFromBoard(const Board : TBoard) : UInt64;
  var
    index : integer;
    pegs : UInt64;

  begin
  result := MaterialHash[White, 29];

  Pegs := Board.WhitePegs and Board.Pawns;
  index := BitCount(pegs);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Knights;
  index := 9 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Bishops and whitesqr;
  index := 13 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Bishops and blacksqr;
  index := 17 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Rooks;
  index := 21 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Queens;
  index := 25 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];
  end;


function BlackMaterialHashFromBoard(const Board : TBoard) : UInt64;
  var
    index : integer;
    pegs : UInt64;

  begin
  result := MaterialHash[Black, 29];

  Pegs := Board.BlackPegs and Board.Pawns;
  index := BitCount(pegs);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Knights;
  index := 9 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Bishops and whitesqr;
  index := 13 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Bishops and blacksqr;
  index := 17 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Rooks;
  index := 21 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Queens;
  index := 25 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];
  end;


function MaterialHashFromBoard(const Board : TBoard) : UInt64;
  var
    index : integer;
    pegs : UInt64;

  begin
  result := MaterialHash[White, 29] xor MaterialHash[Black, 29];

  Pegs := Board.WhitePegs and Board.Pawns;
  index := BitCount(pegs);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Knights;
  index := 9 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Bishops and whitesqr;
  index := 13 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Bishops and blacksqr;
  index := 17 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Rooks;
  index := 21 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];

  Pegs := Board.WhitePegs and Board.Queens;
  index := 25 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[White, index];


  Pegs := Board.BlackPegs and Board.Pawns;
  index := BitCount(pegs);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Knights;
  index := 9 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Bishops and whitesqr;
  index := 13 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Bishops and blacksqr;
  index := 17 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Rooks;
  index := 21 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];

  Pegs := Board.BlackPegs and Board.Queens;
  index := 25 + min(BitCount(pegs), 3);
  Result := Result xor MaterialHash[Black, index];
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
      exit(max(Eval, 850));

    if distance(PawnCell, OpponentKingCell) < Distance(PawnCell, KingCell) - Board.ToPlay then   // draw : pawn can be captured
      exit(Eval div 64);

    if  KingOnKeySquare(Board, white, PawnCell) then             // win for white
      exit(max(Eval, 850));
    end
   else
    begin      // black winning
    OpponentKingCell := GetLowBit_Alt(Board.Kings and Board.WhitePegs);
    KingCell := GetLowBit_Alt(Board.Kings and Board.BlackPegs);
    PromotionCell := 56 + (PawnCell and $7);

    if min(5, Distance(PawnCell, PromotionCell)) < Distance(OpponentKingCell, PromotionCell) - ( 1- Board.ToPlay) then   // win for black
      exit(min(-850, Eval));

    if distance(PawnCell, OpponentKingCell) < Distance(PawnCell, KingCell) - (1 - Board.ToPlay) then    // draw : pawn can be captured
      exit(Eval div 64);

    if  KingOnKeySquare(Board, black, PawnCell) then           // win for black
      exit(min(-850, Eval));
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
    result := -Material_Table[4, 0] - 165 + penalty   // black is winning, so white is down on material and larger penalty is better for white
   else
    result := Material_Table[4, 0] + 165 - penalty;   // white is winning, so white is up material and smaller penalty is better for white
  end;


function Evaluate_EasyMate(const Board : TBoard; Eval : integer) : integer;

  const
    EdgeBonus : array[0..63] of integer =
       (17,	 10,	  5,	  2,	  2,	  5,	 10,	 17,
        10,	  3,   -2,   -5,   -5,   -2,    3,	 10,
         5,  -4,   -7, 	-10,  -10, 	 -7,   -2,	  5,
         2,	 -5,  -10,  -13,  -13,  -10,   -5,	  2,
         2,	 -5,  -10,  -13,  -13,  -10,   -5,	  2,
         5,  -2,   -7, 	-10,  -10, 	 -7,   -2,	  5,
        10,	  3,   -2,   -5,   -5,   -2,    3,	 10,
        17,	 10,	  5,	  2,	  2,	  5,	 10,	 17);

  var
    BlackKingCell, WhiteKingCell, FlipCell, index, material : integer;

 // Mate with K vs kxx.
 // Returns score with bonus from white perspective
 // Bonus to drive the black king towards the edge of board,
 // keep white king towards the centre,
 // and keep white king close to black king.

  begin
  BlackKingCell :=  GetLowBit_Alt(Board.Kings and Board.BlackPegs);
  WhiteKingCell :=  GetLowBit_Alt(Board.Kings and Board.WhitePegs);
  FlipCell := FlipLookup[BlackKingCell];

  index := bitcount(Board.knights) + bitcount(Board.bishops)*3 + bitcount(Board.rooks)*9 + min(bitcount(Board.queens), 1) * 21;
  Material := Material_Table[index, 0];
  if bitcount(Board.queens) > 1 then
    Material := material + SecondQueenValue;

  if Eval > 0 then
    begin    // white winning
    result :=  (300 + Material) + EdgeBonus[BlackKingCell] - EdgeBonus[WhiteKingCell] ;
    result := result - Distance(WhiteKingCell, BlackKingCell) - ManhattanDistance(WhiteKingCell, BlackKingCell);        // white winning : closer together is better for white
    end
   else
    begin    // black winning
    result :=  -(300 + Material) + EdgeBonus[BlackKingCell] - EdgeBonus[WhiteKingCell];
    result := result + Distance(WhiteKingCell, BlackKingCell) + ManhattanDistance(WhiteKingCell, BlackKingCell);        // black winning : further apart is better for white
    end;
  end;


class operator T_EndGame_Table.Initialize (out Dest: T_EndGame_Table);
  var
    i : integer;
    Hash : UInt64;
    Board : TBoard;

  begin
  for i := 0 to TableSize - 1 do
    begin
    Dest.Table[i].ID := 0;
    Dest.Table[i].EvalFunction := nil;
    end;


  // 2-man eval

  // k v K  : Draw
  BoardFromFEN('4k3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // 3-man eval

  // kp v K :
  BoardFromFEN('4k3/8/8/4p3/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_kp_K);

  // k v KP
  BoardFromFEN('4k3/8/8/4P3/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_kp_K);


  // kn v K  : Draw
  BoardFromFEN('1n2k3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // k v KN  : Draw
  BoardFromFEN('4k3/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb(w) v K  : Draw
  BoardFromFEN('2b1k3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb(b) v K  : Draw
  BoardFromFEN('4kb3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // k v KB(w)  : Draw
  BoardFromFEN('4k3/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // k v KB(b)  : Draw
  BoardFromFEN('4k3/8/8/8/8/8/8/2B1K4 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // kr v K  : Simple Mate by Black
  BoardFromFEN('r3k3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // k v KR  : Simple Mate
  BoardFromFEN('4k3/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kq v K  : Simple Mate
  BoardFromFEN('3qk3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // k v KQ  : Simple Mate
  BoardFromFEN('4k3/8/8/8/8/8/8/3QK3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // b" = bishop white squares,  b' = bishop black squares

  // 4-man eval

  // kn v KN  : Draw
  BoardFromFEN('1n2k3/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb" v KB"  : Draw
  BoardFromFEN('2b1k3/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb" v KB' : Draw
  BoardFromFEN('2b1k3/8/8/8/8/8/8/2B1K4 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb' v KB' : Draw
  BoardFromFEN('4kb3/8/8/8/8/8/8/2B1K4 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb' v KB" : Draw
  BoardFromFEN('4kb3/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kn v KB"  : Draw
  BoardFromFEN('1n2k3/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kn v KB'  : Draw
  BoardFromFEN('1n2k3/8/8/8/8/8/8/2B1K4 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb" v KN  : Draw
  BoardFromFEN('2b1k3/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb' v KN  : Draw
  BoardFromFEN('4kb3/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // kr v KR   : Draw
  BoardFromFEN('r3k3/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kq v KQ   : Draw
  BoardFromFEN('3qk3/8/8/8/8/8/8/3QK3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // knn v K   : Draw
  BoardFromFEN('1n2k1n1/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // k v KNN   : Draw
  BoardFromFEN('4k3/8/8/8/8/8/8/1N2K1N1 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // kr v KN   : Mostly draw
  //           : white win 0.0%  draw 71.8%  black win 28.2%   drawish
  BoardFromFEN('r3k3/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kn v KR   : Mostly draw
  //           : white win 28.2%  draw 71.8%  black win 0.0%   drawish
  BoardFromFEN('1n2k3/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // kr v KB"   : Mostly draw
  //           : white win 0.0%  draw 81.6%  black win 18.4%   drawish
  BoardFromFEN('r3k3/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kr v KB'   : Mostly draw
  //           : white win 0.0%  draw 81.6%  black win 18.4%   drawish
  BoardFromFEN('r3k3/8/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb" v KR   : Mostly draw
  //            : white win 18.4%  draw 81.6%  black win 0.0%   drawish
  BoardFromFEN('2b1k3/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb' v KR   : Mostly draw
  //            : white win 18.4%  draw 81.6%  black win 0.0%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // kbb v K  : Simple Mate by Black
  BoardFromFEN('2b1kb2/8/8/8/8/8/8/3K4 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // k v KBB  : Simple Mate by White
  BoardFromFEN('4k3/8/8/8/8/8/8/2B1KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kq v KR  : Mate by Black
  BoardFromFEN('3qk3/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kr v KQ  : Mate by White
  BoardFromFEN('r3k3/8/8/8/8/8/8/3QK3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kq v KB'  : Mate by Black
  BoardFromFEN('3qk3/8/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kq v KB"  : Mate by Black
  BoardFromFEN('3qk3/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kb" v KQ  : Mate by White
  BoardFromFEN('2b1k3/8/8/8/8/8/8/3QK3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);


  // kb' v KQ  : Mate by White
  BoardFromFEN('4kb2/8/8/8/8/8/8/3QK3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kq v KN  : Mate by Black
  BoardFromFEN('3qk3/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // kn v KQ  : Mate by White
  BoardFromFEN('1n2k3/8/8/8/8/8/8/3QK3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_EasyMate);

  // knb" v K   : Mate
  BoardFromFEN('1nb1k3/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_BN);

  // knb' v K   : Mate
  BoardFromFEN('1n2kb2/8/8/8/8/8/8/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_BN);

  // k v KNB"   : Mate
  BoardFromFEN('4k3/8/8/8/8/8/8/1N2KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_BN);

  // k v KNB'   : Mate
  BoardFromFEN('4k3/8/8/8/8/8/8/1NB1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_BN);


  // kp v KB"  : drawish : don't want to exchange into this position
  //           : white win  0.0%  draw 85.1%  black win 14.6%   drawish
  BoardFromFEN('4k3/p7/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kp v KB'  : drawish : don't want to exchange into this position
  //           : white win  0.0%  draw 85.1%  black win 14.6%   drawish
  BoardFromFEN('4k3/p7/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb" v KP  : drawish : don't want to exchange into this position
  //           : white win  14.6%  draw 85.1%  black win 0.0%   drawish
  BoardFromFEN('2b1k3/8/8/8/8/8/P7/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb' v KP  : drawish : don't want to exchange into this position
  //           : white win  14.6%  draw 85.1%  black win 0.0%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/P7/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // kp v KN  : drawish : don't want to exchange into this position
  //          : white win  0.0%  draw 77.0%  black win 23.0%   drawish
  BoardFromFEN('4k3/p7/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kn v KP  : drawish : don't want to exchange into this position
  //          : white win 23.0%  draw 77.0%  black win 0.0%   drawish
  BoardFromFEN('1n2k3/8/8/8/8/8/P7/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);



  // 5-man eval

  // kb" v KB'B"  : Draw
  BoardFromFEN('2b1k3/8/8/8/8/8/8/2B1KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb' v KB'B"  : Draw
  BoardFromFEN('4kb2/8/8/8/8/8/8/2B1KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb'b" v KB"  : Draw
  BoardFromFEN('2b1kb2/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb'b" v KB'  : Draw
  BoardFromFEN('2b1kb2/8/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // kr v KB'B"  : Draw
  //             : white win 8.8%  draw 90.5%  black win 0.7%
  BoardFromFEN('r3k3/8/8/8/8/8/8/2B1KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb'b" v KR  : Draw
  //             : white win 0.7%  draw 90.5%  black win 8.8%
  BoardFromFEN('2b1kb2/8/8/8/8/8/8/R4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // kr v KB"N  : drawish : don't want to exchange into this position
  //              : white win 13.8%  draw 84.1%  black win 2.0%
  BoardFromFEN('r3k3/8/8/8/8/8/8/1N2KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kr v KB'N  : drawish : don't want to exchange into this position
  //              : white win 13.8%  draw 84.1%  black win 2.0%
  BoardFromFEN('r3k3/8/8/8/8/8/8/1NB1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // kb'n v KR  : drawish : don't want to exchange into this position
  //            : white win 2.0%  draw 84.1%  black win 13.8%
  BoardFromFEN('1n2kb2/8/8/8/8/8/8/R4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb"n v KR  : drawish : don't want to exchange into this position
  //            : white win 2.0%  draw 84.1%  black win 13.8%
  BoardFromFEN('1nb1k3/8/8/8/8/8/8/R4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // knn v KN  : Draw
  BoardFromFEN('1n2k1n1/8/8/8/8/8/8/1N2K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kn v KNN  : Draw
  BoardFromFEN('1n2k3/8/8/8/8/8/8/1N2K1N1 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // knn v KB'  : Draw
  BoardFromFEN('1n2k1n1/8/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // knn v KB"  : Draw
  BoardFromFEN('1n2k1n1/8/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb" v KNN  : Draw
  BoardFromFEN('2b1k3/8/8/8/8/8/8/1N2K1N1 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kb' v KNN  : Draw
  BoardFromFEN('4kb2/8/8/8/8/8/8/1N2K1N1 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);


  // knn v KR  : Draw
  //           : white win 3.2%  draw 96.8%  black win 0.0%
  BoardFromFEN('1n2k1n1/8/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);

  // kr v KNN  : Draw
  //           : white win 0.0%  draw 96.8%  black win 3.2%
  BoardFromFEN('r3k3/8/8/8/8/8/8/1N2K1N1 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_Draw);



  // TODO expand this with dedicated evaluation

  // krp v KR  : winable but dificult
  //           : white win 10.4%  draw 43.9%  black win 45.8%
  BoardFromFEN('r3k3/p7/8/8/8/8/8/R3K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kr v KRP  : winable but dificult
  //           : white win 45.8%  draw 43.9%  black win 10.4%
  BoardFromFEN('r3k3/8/8/8/8/8/P7/R4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);



  // kb"p v KB"  : drawish : don't want to exchange into this position
  //             : white win  0.0%  draw 73.0%  black win 27.0%   drawish
  BoardFromFEN('2b1k3/p7/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb"p v KB' : drawish : don't want to exchange into this position
  //                : white win  0.0%  draw 73.0%  black win 27.0%   drawish
  BoardFromFEN('2b1k3/p7/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb'p v KB' : drawish : don't want to exchange into this position
  //            : white win  0.0%  draw 73.0%  black win 27.0%   drawish
  BoardFromFEN('4kb2/p7/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb'p v KB" : drawish : don't want to exchange into this position
  //            : white win  0.0%  draw 73.0%  black win 27.0%   drawish
  BoardFromFEN('4kb2/p7/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // kb" v KB"P  : drawish : don't want to exchange into this position
  //             : white win 27.0%  draw 73.0%  black win 0.0%   drawish
  BoardFromFEN('2b1k3/8/8/8/8/8/P7/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb' v KB"P : drawish : don't want to exchange into this position
  //            : white win 27.0%  draw 73.0%  black win 0.0%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/P7/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb" v KB'P : drawish : don't want to exchange into this position
  //            : white win 27.0%  draw 73.0%  black win 0.0%   drawish
  BoardFromFEN('2b1k3/8/8/8/8/8/P7/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb' v KB'P : drawish : don't want to exchange into this position
  //            : white win 27.0%  draw 73.0%  black win 0.0%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/P7/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);



  // kb' v KPP  : drawish : don't want to exchange into this position
  //            : white win 40.1%  draw 59.9%  black win 0.0%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/PP6/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kb" v KPP  : drawish : don't want to exchange into this position
  //            : white win 40.1%  draw 59.9%  black win 0.0%   drawish
  BoardFromFEN('2b1k3/8/8/8/8/8/PP6/4K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kpp v KB'  : drawish : don't want to exchange into this position
  //            : white win 0.0%  draw 59.9%  black win 40.1%  drawish
  BoardFromFEN('4k3/pp6/8/8/8/8/8/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kpp v KB"  : drawish : don't want to exchange into this position
  //            : white win 0.0%  draw 59.9%  black win 40.1%  drawish
  BoardFromFEN('4k3/pp6/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // kn v KPP  : drawish : don't want to exchange into this position
  //           : white win 51.3%  draw 48.7%  black win 0.0%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/PP6/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kpp v KN  : drawish : don't want to exchange into this position
  //           : white win 0.0%  draw 48.7%  black win 51.3%   drawish
  BoardFromFEN('4k3/pp6/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);


  // knn v KP  : drawish : don't want to exchange into this position
  //           : white win 7.9%  draw 70.3%  black win 14.6%   drawish
  BoardFromFEN('4kb2/8/8/8/8/8/PP6/2B1K3 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);

  // kp v KNN  : drawish : don't want to exchange into this position
  //           : white win 14.6%  draw 70.3%  black win 7.9%   drawish
  BoardFromFEN('4k3/pp6/8/8/8/8/8/4KB2 w - - 0 1', Board);
  Hash := MaterialHashFromBoard(Board);
  Dest.AddCase(Hash, Evaluate_MostlyDraw);
  end;


 procedure T_EndGame_Table.AddCase(MaterialHash : UInt64; EvalFunction : TEvalFunction);
  var
    index, stop : integer;

  begin
  index := UInt(MaterialHash and TableMask);
  stop := (index + MaxLink) and TableMask;

    repeat
    if (Table[index].ID = 0) or (Table[index].ID = MaterialHash) then
      begin
      Table[index].ID := MaterialHash;
      Table[index].EvalFunction := EvalFunction;
      exit;
      end;

    index := (index + 1) and TableMask;
    until index = stop;
  end;


function T_EndGame_Table.RetrieveEval(MaterialHash : UInt64) : TEvalFunction;
  var
    index, stop : integer;

  begin
  index := UInt(MaterialHash and TableMask);
  stop := (index + MaxLink) and TableMask;

    repeat
    if Table[index].ID = MaterialHash then
      begin
      result := Table[index].EvalFunction;
      exit;
      end;

    index := (index + 1) and TableMask;
    until index = stop;

  result := nil;
  end;


end.
