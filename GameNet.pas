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


unit GameNet;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Math, Common, GameDef, EndGame;

type
  TNetArchType = (nt_unknown, _768x128_v1, _768x128_x2, _1536x128_v1);

const
  NetType = _768x128_x2;


type
  TEvalRec = record
    UID : UInt64;
    Data : UInt64;
    end;


type
  T_EvalHash_Table = record
    const
      TableSize = $40000;             // 2^18       = 262,144 slots
      TableMask = TableSize - 1 ;     // 2^18 - 1

    var
    Table : Array[0..TableSize-1] of TEvalRec;

    function RetrieveScore(HashCode : UInt64; var score : integer) : boolean;
    procedure StoreScore(HashCode : UInt64; score : integer);
    procedure ClearTable;
    end;


type
  TAccumulator_128 = record
    values : array[0..255] of int16;

    procedure LoadBias_asm(sourceW, sourceB : PSmallInt);
    procedure Load_asm;
    procedure Save_asm;
    procedure Add_asm(sourceW, sourceB : PSmallInt);
    procedure Sub_asm(sourceW, sourceB : PSmallInt);
    procedure Zero;
    function GetEval_asm(weights : PSmallInt; colour : integer) : int16;
    end;


type
  TAccumulator = TAccumulator_128;


type
  TGameNet = record

  const
    InputLayerSize = 768;
    OutputLayerSize = sizeof(TAccumulator) div 2;

  var

    Weights : array[0..InputLayerSize*OutputLayerSize - 1] of int16;
    Bias : array[0..OutputLayerSize - 1] of int16;

    OutputWeights : array[0..OutputLayerSize - 1] of int16;
    OutputBias : int16;

  public
    function LoadFromResource(const ResourceIdentifier : string) : boolean;
    function LoadFromFile(const FileName : string) : boolean;

    procedure RefreshAccumulator(var Accumulator : TAccumulator; const Board : TBoard);
    procedure UpdateAccumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
    function  ScoreFromAccumulator(const Accumulator : TAccumulator; colour : integer) : integer;
    function  Score(const Board : TBoard) : integer;
    end;


function ScoreFromAccumulator(const Accumulator : TAccumulator; const Board : TBoard) : integer;
procedure Update_Accumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
procedure Refresh_Accumulator(var Accummulator : TAccumulator; const Board : TBoard);

implementation

uses
  Search;

var
  Game_Net : TGameNet;
  EvalHashTable : T_EvalHash_Table;


// ==================  TAccumulator_128  ===========================================

procedure TAccumulator_128.LoadBias_asm(sourceW, sourceB : PSmallInt);
  asm
  .NOFRAME

	vmovdqu ymm0, [rdx]       //  Load W0
  vmovdqu ymm1, [rdx+32]    //  Load W1
  vmovdqu ymm2, [rdx+64]    //  Load W2
  vmovdqu ymm3, [rdx+96]    //  Load W3
  vmovdqu ymm4, [rdx+128]   //  Load W4
  vmovdqu ymm5, [rdx+160]   //  Load W5
  vmovdqu ymm6, [rdx+192]   //  Load W6
  vmovdqu ymm7, [rdx+224]   //  Load W7

	vmovdqu ymm8, [r8]        //  Load B0
  vmovdqu ymm9, [r8+32]     //  Load B1
	vmovdqu ymm10, [r8+64]    //  Load B2
  vmovdqu ymm11, [r8+96]    //  Load B3
  vmovdqu ymm12, [r8+128]   //  Load B4
  vmovdqu ymm13, [r8+160]   //  Load B5
  vmovdqu ymm14, [r8+192]   //  Load B6
  vmovdqu ymm15, [r8+224]   //  Load B7
  end;


procedure TAccumulator_128.Load_asm;
  asm
  .NOFRAME
  mov r8, rcx
  add r8, 256

	vmovdqu ymm0, [rcx]       //  Load W0
  vmovdqu ymm1, [rcx+32]    //  Load W1
	vmovdqu ymm2, [rcx+64]    //  Load W2
  vmovdqu ymm3, [rcx+96]    //  Load W3
	vmovdqu ymm4, [rcx+128]   //  Load W4
	vmovdqu ymm5, [rcx+160]   //  Load W5
	vmovdqu ymm6, [rcx+192]   //  Load W6
	vmovdqu ymm7, [rcx+224]   //  Load W7

  vmovdqu ymm8, [r8]        //  Load B0
	vmovdqu ymm9, [r8+32]     //  Load B1
  vmovdqu ymm10, [r8+64]    //  Load B2
	vmovdqu ymm11, [r8+96]    //  Load B3
	vmovdqu ymm12, [r8+128]   //  Load B4
	vmovdqu ymm13, [r8+160]   //  Load B5
	vmovdqu ymm14, [r8+192]   //  Load B6
	vmovdqu ymm15, [r8+224]   //  Load B7
  end;


procedure TAccumulator_128.Save_asm;
  asm
  .NOFRAME
  mov r8, rcx
  add r8, 256

	vmovdqu [rcx], ymm0        //  Save A0
  vmovdqu [rcx+32], ymm1     //  Save A1
	vmovdqu [rcx+64], ymm2     //  Save A2
  vmovdqu [rcx+96], ymm3     //  Save A3
	vmovdqu [rcx+128], ymm4    //  Save A4
	vmovdqu [rcx+160], ymm5    //  Save A5
	vmovdqu [rcx+192], ymm6    //  Save A6
	vmovdqu [rcx+224], ymm7    //  Save A7

  vmovdqu [r8], ymm8        //  Save B0
	vmovdqu [r8+32], ymm9     //  Save B1
  vmovdqu [r8+64], ymm10    //  Save B2
	vmovdqu [r8+96], ymm11    //  Save B3
  vmovdqu [r8+128], ymm12   //  Save B4
  vmovdqu [r8+160], ymm13   //  Save B5
  vmovdqu [r8+192], ymm14   //  Save B6
  vmovdqu [r8+224], ymm15   //  Save B7
  vzeroall
  end;


procedure TAccumulator_128.Add_asm(sourceW, sourceB : PSmallInt);
  asm
  .NOFRAME
	vpaddsw ymm0, ymm0,   [rdx]       //  Add W0
	vpaddsw ymm8, ymm8,   [r8]        //  Add B0
	vpaddsw ymm1, ymm1,   [rdx+32]    //  Add W1
	vpaddsw ymm9, ymm9,   [r8+32]     //  Add B1
	vpaddsw ymm2, ymm2,   [rdx+64]    //  Add W2
	vpaddsw ymm10, ymm10, [r8+64]     //  Add B2
	vpaddsw ymm3, ymm3,   [rdx+96]    //  Add W3
	vpaddsw ymm11, ymm11, [r8+96]     //  Add B3
	vpaddsw ymm4, ymm4,   [rdx+128]   //  Add W4
	vpaddsw ymm12, ymm12, [r8+128]    //  Add B4
	vpaddsw ymm5, ymm5,   [rdx+160]   //  Add W5
	vpaddsw ymm13, ymm13, [r8+160]    //  Add B5
	vpaddsw ymm6, ymm6,   [rdx+192]   //  Add W6
	vpaddsw ymm14, ymm14, [r8+192]    //  Add B6
	vpaddsw ymm7, ymm7,   [rdx+224]   //  Add W7
	vpaddsw ymm15, ymm15, [r8+224]    //  Add B7
  end;


procedure TAccumulator_128.Sub_asm(sourceW, sourceB : PSmallInt);
  asm
	vpsubsw ymm0, ymm0,   [rdx]       //  Sub W0
	vpsubsw ymm8, ymm8,   [r8]        //  Sub B0
	vpsubsw ymm1, ymm1,   [rdx+32]    //  Sub W1
	vpsubsw ymm9, ymm9,   [r8+32]     //  Sub B1
	vpsubsw ymm2, ymm2,   [rdx+64]    //  Sub W2
	vpsubsw ymm10, ymm10, [r8+64]     //  Sub B2
	vpsubsw ymm3, ymm3,   [rdx+96]    //  Sub W3
	vpsubsw ymm11, ymm11, [r8+96]     //  Sub B3
	vpsubsw ymm4, ymm4,   [rdx+128]   //  Sub W4
	vpsubsw ymm12, ymm12, [r8+128]    //  Sub B4
	vpsubsw ymm5, ymm5,   [rdx+160]   //  Sub W5
	vpsubsw ymm13, ymm13, [r8+160]    //  Sub B5
	vpsubsw ymm6, ymm6,   [rdx+192]   //  Sub W6
	vpsubsw ymm14, ymm14, [r8+192]    //  Sub B6
	vpsubsw ymm7, ymm7,   [rdx+224]   //  Sub W7
	vpsubsw ymm15, ymm15, [r8+224]    //  Sub B7
  end;


procedure TAccumulator_128.Zero;
  asm
  vzeroall
  end;


function TAccumulator_128.GetEval_asm(weights : PSmallInt; colour : integer) : int16;
  asm
  test r8, r8
  JNZ @BTP

  @WTP:
  mov r9, rcx
  add r9, 256
  mov r10, rdx
  add r10, 256
  JMP @start

  @BTP:
  mov r9, rcx
  add rcx, 256
  mov r10, rdx
  add r10, 256

  @start:
  mov r8, 120
  vzeroall                           // zero the accummulator

  @loop1:

  vpmovsxwd ymm1, [rcx+r8*2]             //  mov 8 int16 values B into ymm1 and sign extend to int32
  vpmovsxwd ymm2, [rdx+r8*2]             //  mov 8 int16 values C into ymm2 and sign extend to int32

  vpmaxsw ymm1, ymm3, ymm1               //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time

  vpmovsxwd ymm5, [r9+r8*2]              //  mov 8 int16 values B into ymm5 and sign extend to int32
  vpmovsxwd ymm6, [r10+r8*2]             //  mov 8 int16 values C into ymm6 and sign extend to int32
  vpmaxsw ymm5, ymm3, ymm5               //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time

  vpmulld ymm4, ymm1, ymm2               //  Calculate D = low(B.C), 8 int32 at a time
  vpaddd ymm0, ymm0, ymm4

  vpmulld ymm7, ymm5, ymm6               //  Calculate D = low(B.C), 8 int32 at a time
  vpaddd ymm0, ymm0, ymm7

	sub r8, 8
	jge @loop1

  vextractf128 xmm1, ymm0, $1
  vzeroupper

  paddd xmm0, xmm1
  pshufd xmm1, xmm0, $4e
  paddd  xmm0, xmm1

  pshufd xmm1, xmm0, $11
  paddd xmm0, xmm1

  pextrw eax, xmm0, $1                  //  extract the high word = equivalent to division by 256 x 256
                                        //  if positive then result rounded down towards zero
  test eax, $8000                       //  if negative then shift rounds down towards negative infinity, so add one if negative to round result up towards zero
  jz @end
  add eax, $1
  @end:

  vzeroall
  end;


// ================= Eval Hash ==================================================

// score [-32767..32767]

procedure T_EvalHash_Table.StoreScore(HashCode : UInt64; Score : integer);
  var
    index, Data : UInt64;

  begin
  index := UInt64(HashCode and TableMask);

  Data := UInt64(Score + 65536);

  Table[index].UID := HashCode xor Data;
  Table[index].Data := Data;
  end;


function T_EvalHash_Table.RetrieveScore(HashCode : UInt64; var score : integer) : boolean;
  var
    index, UID, Data : UInt64;

  begin
  result := false;
  index := (HashCode and TableMask);

  UID := Table[index].UID;
  Data := Table[index].Data;

  if (UID xor Data) = HashCode then
    begin
    score := integer(Data) - 65536;
    exit(true);
    end;
  end;


procedure T_EvalHash_Table.ClearTable;
  var
    i : integer;

  begin
  for i := 0 to TableSize - 1 do
    begin
    Table[i].UID := 0;
    Table[i].Data := 0;
    end;
  end;


// ================== TGame_Net ================================================


function TGameNet.LoadFromResource(const ResourceIdentifier : string) : boolean;
  var
    Stream: TResourceStream;

  begin
  Stream := TResourceStream.Create(HInstance, ResourceIdentifier, RT_RCDATA);

    try
    stream.Read(weights[0], InputLayerSize * OutputLayerSize * sizeof(int16));
    stream.Read(Bias[0], OutputLayerSize * sizeof(int16));

    stream.Read(OutputWeights[0], OutputLayerSize * sizeof(int16));
    stream.Read(OutputBias, sizeof(int16));

    finally
    Stream.Free;
    end
  end;


function TGameNet.LoadFromFile(const Filename : string) : boolean;
  var
    Stream: TMemoryStream;

  begin
  Stream := TMemoryStream.Create;
    try
    Stream.LoadFromFile(Filename);

    stream.ReadBuffer(weights[0], InputLayerSize * OutputLayerSize * sizeof(int16));
    stream.ReadBuffer(Bias[0], OutputLayerSize * sizeof(int16));

    stream.ReadBuffer(OutputWeights[0], OutputLayerSize * sizeof(int16));
    stream.ReadBuffer(OutputBias, sizeof(int16));

    finally
    Stream.Free;
    end
  end;


procedure TGameNet.RefreshAccumulator(var Accumulator : TAccumulator; const Board : TBoard);
  var
    index_W, index_B : int64;
    Pegs : UInt64;
    Cell : integer;

  begin
  Accumulator.LoadBias_asm(@Bias[0], @Bias[0]);

  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (pawn-1) * 64 + Cell) * OutputLayerSize;
    index_B := (Black * 384 + (pawn-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (knight-1) * 64 + Cell) * OutputLayerSize;
    index_B := (Black * 384 + (knight-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (bishop-1) * 64 + Cell) * OutputLayerSize;
    index_B := (Black * 384 + (bishop-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (rook-1) * 64 + Cell) * OutputLayerSize;
    index_B := (Black * 384 + (rook-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (queen-1) * 64 + Cell) * OutputLayerSize;
    index_B := (Black * 384 + (queen-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (king-1) * 64 + Cell) * OutputLayerSize;
    index_B := (Black * 384 + (king-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (pawn-1) * 64 + Cell) * OutputLayerSize;
    index_B := (              (pawn-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (knight-1) * 64 + Cell) * OutputLayerSize;
    index_B := (              (knight-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (bishop-1) * 64 + Cell) * OutputLayerSize;
    index_B := (              (bishop-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (rook-1) * 64 + Cell) * OutputLayerSize;
    index_B := (              (rook-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (queen-1) * 64 + Cell) * OutputLayerSize;
    index_B := (              (queen-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (king-1) * 64 + Cell) * OutputLayerSize;
    index_B := (              (king-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  Accumulator.Save_asm;
  end;


procedure TGameNet.UpdateAccumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
  var
    index_W, index_B : integer;
    Source, Dest, Piece, CapturedPiece, PromotionPiece, epCell, TempSource, TempDest : integer;
    ColourOffset, PieceOffset : integer;

  begin
  Piece := (Move shr 12) and $F;              // Piece := Move.Piece
  Source :=  Move and $3F;                    // Source := Move.Source
  Dest :=  (Move shr 6) and $3F;              // Dest := Move.Dest

  CapturedPiece := (Move shr 16) and $F;      // CapturedPiece := Move.CapturedPiece
  PromotionPiece := (Move shr 20) and $F;     // PromotionPiece := Move.PromotionPiece
  epCell := (Move shr 30) and $3F;

  ColourOffset :=  (1-Board.ToPlay) * 384;
  PieceOffset := (Piece - 1) * 64;

  SourceAccumulator.Load_asm;

  // source
  index_W := (ColourOffset + PieceOffset + source) * OutputLayerSize;
  index_B := ((384 - ColourOffset) + PieceOffset + (source xor 56)) * OutputLayerSize;
  SourceAccumulator.Sub_asm(@Weights[index_W], @Weights[index_B]);

  // dest
  index_W := (ColourOffset + PieceOffset + dest) * OutputLayerSize;
  index_B := ((384 - ColourOffset) + PieceOffset + (dest xor 56)) * OutputLayerSize;
  SourceAccumulator.Add_asm(@Weights[index_W], @Weights[index_B]);

  // captured piece

  if CapturedPiece <> 0 then  // deducts captured piece values from accumulator
    begin
    TempDest := Dest;

    if (CapturedPiece = pawn) and (epCell = Dest) then
      begin
      if Board.ToPlay = Black then
        TempDest := Dest + 8
       else
        TempDest := Dest - 8;
      end;

    PieceOffset := (CapturedPiece - 1) * 64;

    index_W := ((384 - ColourOffset) + PieceOffset + TempDest) * OutputLayerSize;
    index_B := (ColourOffset + PieceOffset + (TempDest xor 56)) * OutputLayerSize;
    SourceAccumulator.Sub_asm(@Weights[index_W], @Weights[index_B]);
    end;

  // promotion piece

  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin
    PieceOffset := (Pawn - 1) * 64;
    index_W := (ColourOffset + PieceOffset + Dest) * OutputLayerSize;
    index_B := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * OutputLayerSize;
    SourceAccumulator.Sub_asm(@Weights[index_W], @Weights[index_B]);

    PieceOffset := (PromotionPiece - 1) * 64;
    index_W := (ColourOffset + PieceOffset + Dest) * OutputLayerSize;
    index_B := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * OutputLayerSize;
    SourceAccumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
    end;

  // castling move

  if (Piece = King) then
    begin
    if (Dest - Source = 2) then    // King Side Castle, adjust rook
      begin
      TempDest := Dest - 1;
      TempSource := Source + 3;
      PieceOffset := (Rook - 1) * 64;

      // source
      index_W := (ColourOffset + PieceOffset + TempSource) * OutputLayerSize;
      index_B := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index_W], @Weights[index_B]);

      // dest
      index_W := (ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
      index_B := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
      end;

    if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      // source
      index_W := (ColourOffset + PieceOffset + TempSource) * OutputLayerSize;
      index_B := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index_W], @Weights[index_B]);

      // dest
      index_W := (ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
      index_B := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index_W], @Weights[index_B]);
      end;
    end;

  DestAccumulator.Save_asm;
  end;



function  TGameNet.ScoreFromAccumulator(const Accumulator : TAccumulator; colour : integer) : integer;
  begin
  result := Accumulator.GetEval_asm(@OutputWeights[0], colour) + OutputBias;     // result = score from P.O.V. of side to play
  end;


function TGameNet.Score(const Board : TBoard) : integer;
  var
    Accumulator : TAccumulator;

  begin
  RefreshAccumulator(Accumulator, Board);
  result := ScoreFromAccumulator(Accumulator, Board.ToPlay);        // result = score in cp, from P.O.V. of side to play
  end;


function ScoreFromAccumulator(const Accumulator : TAccumulator; const Board : TBoard) : integer;
  var
    EndGameIndex : integer;
    EvalFunction : TEvalFunction;

  begin
  if EvalHashTable.RetrieveScore(Board.Hash, result) = false then
    begin
    result := Game_Net.ScoreFromAccumulator(Accumulator, Board.ToPlay);

    if bitcount(Board.WhitePegs or Board.BlackPegs) <= 5 then
      begin
      EndGameIndex := IndexFromBoard(Board);
      EvalFunction := EndGameLookup[EndGameIndex];

      if assigned(EvalFunction) then
        result := EvalFunction(Board, result);
      end;

    result := min(result, MateScoreCutoff - 1);
    result := max(result, -MateScoreCutoff + 1);

    EvalHashTable.StoreScore(Board.Hash, result);
    end;

  if Board.HalfCount > 36 then
    result := result * (165 - Board.HalfCount) div 128;
  end;


procedure Update_Accumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
  begin
  Game_Net.UpdateAccumulator(SourceAccumulator, DestAccumulator, Move, Board);
  end;


procedure Refresh_Accumulator(var Accummulator : TAccumulator; const Board : TBoard);
  begin
  Game_Net.RefreshAccumulator(Accummulator, Board);
  end;


// ====================== NNUE functions =======================================

procedure Load_GameNet;
  var
    FileName, FilePath : string;

  begin
  Game_Net.LoadFromResource('_768x128_x2_gen5');    // 'NNUE 768x128_x2 151190269c +70+ gen5.net'
  end;


Initialization

  EvalHashTable.ClearTable;
  Load_GameNet;

end.
