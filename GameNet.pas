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
  TNetArchType = (nt_unknown, _768x128_x2, _768x256_x2);

const
  NetType = _768x256_x2;

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
  TAccumulator_256 = record
    values : array[0..511] of int16;

    procedure LoadBias_asm(source : PSmallInt);
    procedure Load_asm(colour : Integer);
    procedure Save_asm(colour : Integer);
    procedure Add_asm(source : PSmallInt);
    procedure Sub_asm(source : PSmallInt);
    procedure Zero;
    function GetEval_asm(weights : PSmallInt; colour : integer) : int16;
    end;

type
  TAccumulator = TAccumulator_256;


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

var
  EarlyNet : TGameNet;
  MidNet : TGameNet;
  LateNet : TGameNet;

  Game_Net : ^TGameNet;


implementation

uses
  Search;

var
  EvalHashTable : T_EvalHash_Table;


// ==================  TAccumulator_256  ===========================================


procedure TAccumulator_256.LoadBias_asm(source : PSmallInt);
  asm
  .NOFRAME
  mov r8, rdx
  add r8, 256

	vmovdqu ymm0, [rdx]       //  Load A0
  vmovdqu ymm8, [r8]        //  Load A8

  vmovdqu ymm1, [rdx+32]    //  Load A1
  vmovdqu ymm9, [r8+32]     //  Load A9

  vmovdqu ymm2, [rdx+64]    //  Load A2
	vmovdqu ymm10, [r8+64]    //  Load A10

  vmovdqu ymm3, [rdx+96]    //  Load A3
  vmovdqu ymm11, [r8+96]    //  Load A11

  vmovdqu ymm4, [rdx+128]   //  Load A4
  vmovdqu ymm12, [r8+128]   //  Load A12

  vmovdqu ymm5, [rdx+160]   //  Load A5
  vmovdqu ymm13, [r8+160]   //  Load A13

  vmovdqu ymm6, [rdx+192]   //  Load A6
  vmovdqu ymm14, [r8+192]   //  Load A14

  vmovdqu ymm7, [rdx+224]   //  Load A7
  vmovdqu ymm15, [r8+224]   //  Load A15
  end;


procedure TAccumulator_256.Load_asm(colour : integer);
  asm
  .NOFRAME
  imul rdx, 512             // if colour = black (1) then r8 = 512, else if white (0) then r8 = 0
  add rcx, rdx

  mov r8, rcx
  add r8, 256

	vmovdqu ymm0, [rcx]       //  Load A0
  vmovdqu ymm8, [r8]        //  Load A8

  vmovdqu ymm1, [rcx+32]    //  Load A1
  vmovdqu ymm9, [r8+32]     //  Load A9

	vmovdqu ymm2, [rcx+64]    //  Load A2
  vmovdqu ymm10, [r8+64]    //  Load A10

  vmovdqu ymm3, [rcx+96]    //  Load A3
	vmovdqu ymm11, [r8+96]    //  Load A11

	vmovdqu ymm4, [rcx+128]   //  Load A4
	vmovdqu ymm12, [r8+128]   //  Load A12

	vmovdqu ymm5, [rcx+160]   //  Load A5
	vmovdqu ymm13, [r8+160]   //  Load A13

	vmovdqu ymm6, [rcx+192]   //  Load A6
	vmovdqu ymm14, [r8+192]   //  Load A14

	vmovdqu ymm7, [rcx+224]   //  Load AW7
	vmovdqu ymm15, [r8+224]   //  Load A15
  end;


procedure TAccumulator_256.Save_asm(colour : integer);
  asm
  .NOFRAME
  imul rdx, 512             // if colour = black (1) then r8 = 512, else if white (0) then r8 = 0
  add rcx, rdx

  mov r8, rcx
  add r8, 256

	vmovdqu [rcx], ymm0        //  Save A0
  vmovdqu [r8], ymm8         //  Save A8

  vmovdqu [rcx+32], ymm1     //  Save A1
	vmovdqu [r8+32], ymm9      //  Save A9

	vmovdqu [rcx+64], ymm2     //  Save A2
  vmovdqu [r8+64], ymm10     //  Save A10

  vmovdqu [rcx+96], ymm3     //  Save A3
	vmovdqu [r8+96], ymm11     //  Save A11

	vmovdqu [rcx+128], ymm4    //  Save A4
  vmovdqu [r8+128], ymm12    //  Save A12

	vmovdqu [rcx+160], ymm5    //  Save A5
  vmovdqu [r8+160], ymm13    //  Save A13

	vmovdqu [rcx+192], ymm6    //  Save A6
  vmovdqu [r8+192], ymm14    //  Save A14

	vmovdqu [rcx+224], ymm7    //  Save A7
  vmovdqu [r8+224], ymm15    //  Save A15

  vzeroall
  end;


procedure TAccumulator_256.Add_asm(source : PSmallInt);
  asm
  .NOFRAME
  mov r8, rdx
  add r8, 256

	vpaddsw ymm0, ymm0,   [rdx]       //  Add W0
	vpaddsw ymm8, ymm8,   [r8]        //  Add W8
	vpaddsw ymm1, ymm1,   [rdx+32]    //  Add W1
	vpaddsw ymm9, ymm9,   [r8+32]     //  Add W9
	vpaddsw ymm2, ymm2,   [rdx+64]    //  Add W2
	vpaddsw ymm10, ymm10, [r8+64]     //  Add W10
	vpaddsw ymm3, ymm3,   [rdx+96]    //  Add W3
	vpaddsw ymm11, ymm11, [r8+96]     //  Add W11
	vpaddsw ymm4, ymm4,   [rdx+128]   //  Add W4
	vpaddsw ymm12, ymm12, [r8+128]    //  Add W12
	vpaddsw ymm5, ymm5,   [rdx+160]   //  Add W5
	vpaddsw ymm13, ymm13, [r8+160]    //  Add W13
	vpaddsw ymm6, ymm6,   [rdx+192]   //  Add W6
	vpaddsw ymm14, ymm14, [r8+192]    //  Add W14
	vpaddsw ymm7, ymm7,   [rdx+224]   //  Add W7
	vpaddsw ymm15, ymm15, [r8+224]    //  Add W15
  end;


procedure TAccumulator_256.Sub_asm(source : PSmallInt);
  asm
  .NOFRAME
  mov r8, rdx
  add r8, 256

	vpsubsw ymm0, ymm0,   [rdx]       //  Sub W0
	vpsubsw ymm8, ymm8,   [r8]        //  Sub W8
	vpsubsw ymm1, ymm1,   [rdx+32]    //  Sub W1
	vpsubsw ymm9, ymm9,   [r8+32]     //  Sub W9
	vpsubsw ymm2, ymm2,   [rdx+64]    //  Sub W2
	vpsubsw ymm10, ymm10, [r8+64]     //  Sub W10
	vpsubsw ymm3, ymm3,   [rdx+96]    //  Sub W3
	vpsubsw ymm11, ymm11, [r8+96]     //  Sub W11
	vpsubsw ymm4, ymm4,   [rdx+128]   //  Sub W4
	vpsubsw ymm12, ymm12, [r8+128]    //  Sub W12
	vpsubsw ymm5, ymm5,   [rdx+160]   //  Sub W5
	vpsubsw ymm13, ymm13, [r8+160]    //  Sub W13
	vpsubsw ymm6, ymm6,   [rdx+192]   //  Sub W6
	vpsubsw ymm14, ymm14, [r8+192]    //  Sub W14
	vpsubsw ymm7, ymm7,   [rdx+224]   //  Sub W7
	vpsubsw ymm15, ymm15, [r8+224]    //  Sub W15
  end;


procedure TAccumulator_256.Zero;
  asm
  vzeroall
  end;


function TAccumulator_256.GetEval_asm(weights : PSmallInt; colour : integer) : int16;
  asm
  test r8, r8
  JNZ @BTP

  @WTP:
  mov r9, rcx
  add r9, 512
  mov r10, rdx
  add r10, 512
  JMP @start

  @BTP:
  mov r9, rcx
  add rcx, 512
  mov r10, rdx
  add r10, 512

  @start:
  mov r8, 248
  vzeroall                               // zero the accummulator

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
  Accumulator.LoadBias_asm(@Bias[0]);

  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (pawn-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (knight-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (bishop-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (rook-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (queen-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (              (king-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (pawn-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (knight-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (bishop-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (rook-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (queen-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (king-1) * 64 + Cell) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_W]);
    end;

  Accumulator.Save_asm(white);


  Accumulator.LoadBias_asm(@Bias[0]);

  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (pawn-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (knight-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (bishop-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (rook-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (queen-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (king-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (              (pawn-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (              (knight-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (              (bishop-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (              (rook-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (              (queen-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (              (king-1) * 64 + (Cell xor 56)) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index_B]);
    end;

  Accumulator.Save_asm(black);
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


  SourceAccumulator.Load_asm(white);

  // source
  index_W := (ColourOffset + PieceOffset + source) * OutputLayerSize;
  SourceAccumulator.Sub_asm(@Weights[index_W]);

  // dest
  index_W := (ColourOffset + PieceOffset + dest) * OutputLayerSize;
  SourceAccumulator.Add_asm(@Weights[index_W]);

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
    SourceAccumulator.Sub_asm(@Weights[index_W]);
    end;

  // promotion piece

  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin
    PieceOffset := (Pawn - 1) * 64;
    index_W := (ColourOffset + PieceOffset + Dest) * OutputLayerSize;
    SourceAccumulator.Sub_asm(@Weights[index_W]);

    PieceOffset := (PromotionPiece - 1) * 64;
    index_W := (ColourOffset + PieceOffset + Dest) * OutputLayerSize;
    SourceAccumulator.Add_asm(@Weights[index_W]);
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
      SourceAccumulator.Sub_asm(@Weights[index_W]);

      // dest
      index_W := (ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index_W]);
      end;

    if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      // source
      index_W := (ColourOffset + PieceOffset + TempSource) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index_W]);

      // dest
      index_W := (ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index_W]);
      end;
    end;

  DestAccumulator.Save_asm(white);

  SourceAccumulator.Load_asm(black);

  PieceOffset := (Piece - 1) * 64;

  // source
  index_B := ((384 - ColourOffset) + PieceOffset + (source xor 56)) * OutputLayerSize;
  SourceAccumulator.Sub_asm(@Weights[index_B]);

  // dest
  index_B := ((384 - ColourOffset) + PieceOffset + (dest xor 56)) * OutputLayerSize;
  SourceAccumulator.Add_asm(@Weights[index_B]);

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

    index_B := (ColourOffset + PieceOffset + (TempDest xor 56)) * OutputLayerSize;
    SourceAccumulator.Sub_asm(@Weights[index_B]);
    end;

  // promotion piece

  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin
    PieceOffset := (Pawn - 1) * 64;
    index_B := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * OutputLayerSize;
    SourceAccumulator.Sub_asm(@Weights[index_B]);

    PieceOffset := (PromotionPiece - 1) * 64;
    index_B := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * OutputLayerSize;
    SourceAccumulator.Add_asm(@Weights[index_B]);
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
      index_B := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index_B]);

      // dest
      index_B := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index_B]);
      end;

    if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      // source
      index_B := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index_B]);

      // dest
      index_B := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index_B]);
      end;
    end;

  DestAccumulator.Save_asm(black);
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

procedure Load_GameNets;
  var
    FileName, FilePath : string;

  begin

  // Yakka 1.4

  EarlyNet.LoadFromResource('_768x256_x2_gen7_early');  //  NNUE 768x256_x2 31222017c gen7 early.net
  MidNet.LoadFromResource('_768x256_x2_gen7_mid');      //  NNUE 768x256_x2 85639857c gen7 mid.net
  LateNet.LoadFromResource('_768x256_x2_gen7_late');    //  NNUE 768x256_x2 169081011c gen7 late.net

  end;


Initialization

  EvalHashTable.ClearTable;
  Load_GameNets;

end.
