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


unit GameNet;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Math, Common, GameDef, EndGame, CPU_Info;

type
  TNetArchType = (nt_unknown, _768→512x2→1);

const
  NetArchName : array[0..1] of string = ('Unknown', '768→512x2→1');

const

  NetType = _768→512x2→1;

  Attribute_Count = 768;
  InputLayerWidth = 512;
  Accumulator_Width = 1024;

  Buckets = 6;
  BucketLookup : array[0..16] of integer = (0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5);

type
  TAccumulator = array[0..Accumulator_Width-1] of int32;

  T_GetBias_asm = procedure(dest, IndexAdd : PInteger; colour : integer);
  T_Add_asm = procedure(dest, IndexAdd : PInteger; colour : integer);

  T_UpdateMove_asm = procedure(source, dest, IndexAdd, IndexSub : PInteger; colour : integer);
  T_UpdateCapture_asm = procedure(source, dest, IndexAdd, IndexSub1, IndexSub2 : PInteger; colour : integer);
  T_UpdateSelf_asm = procedure(source, IndexAdd, IndexSub : PInteger; colour : integer);

  T_GetEval_asm = function(source, weights : PInteger; colour : integer) : int16;


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
    procedure Clear;
    end;


type
  TGameNet = record

  var
    Weights : array[0..Attribute_Count * InputLayerWidth - 1] of int32;
    Bias : array[0..InputLayerWidth - 1] of int32;

    OutputWeights : array[0..buckets * Accumulator_Width - 1] of int32;
    OutputBias : array[0..buckets - 1] of int32;

    Hash : UInt64;

  public
    class operator Initialize (out x: TGameNet);
    function LoadFromResource(const ResourceIdentifier : string) : boolean;
    function LoadFromFile(const FileName : string) : boolean;

    procedure RefreshAccumulator(var Accumulator : TAccumulator; const Board : TBoard);
    procedure UpdateAccumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);

    function  ScoreFromAccumulator(const Accumulator : TAccumulator; colour, pieceCount : integer) : integer;
    function  Score(const Board : TBoard) : integer;
    end;


function ScoreFromAccumulator(const Accumulator : TAccumulator; const Board : TBoard) : integer;
procedure Update_Accumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
procedure Refresh_Accumulator(var Accummulator : TAccumulator; const Board : TBoard);


var
  Net : TGameNet;
  Game_Net : ^TGameNet;

  GetBias_asm : T_GetBias_asm;
  Add_asm : T_Add_asm;

  UpdateMove_asm : T_UpdateMove_asm;
  UpdateCapture_asm : T_UpdateCapture_asm;
  UpdateSelf_asm : T_UpdateSelf_asm;

  GetEval_asm : T_GetEval_asm;

  EvalHashTable : T_EvalHash_Table;

implementation

uses
  Search;


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


procedure T_EvalHash_Table.Clear;
  var
    i : integer;

  begin
  for i := 0 to TableSize - 1 do
    begin
    Table[i].UID := 0;
    Table[i].Data := 0;
    end;
  end;


// ========================= AVX2 ==============================================

procedure GetBias_asm_AVX2(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu ymm0, [rdx+r11]           //  Load Bias
  vmovdqu [rcx+r11], ymm0           //  Save to Self

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure Add_asm_AVX2(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu ymm0, [rcx+r11]           //  Load Self
  vpaddd ymm0, ymm0, [rdx+r11]      //  Add
  vmovdqu [rcx+r11], ymm0           //  Save Self

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure UpdateMove_asm_AVX2(source, dest, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  .NOFRAME

  mov r10d, colour

  imul r10, InputLayerWidth * 4     // if colour = black (1) then r10 = InputLayerWidth * 4, else if white (0) then r10 = 0
  add rcx, r10
  add rdx, r10
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu ymm0, [rcx+r11]           //  Load from Self as source
  vpaddd ymm0, ymm0, [r8+r11]       //  Add
  vpsubd ymm0, ymm0, [r9+r11]       //  Sub
  vmovdqu [rdx+r11], ymm0           //  Save to Dest

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure UpdateCapture_asm_AVX2(source, dest, IndexAdd, IndexSub1, IndexSub2 : PInteger; colour : integer);
  asm
  .NOFRAME

  mov r11d, colour
  mov r10, IndexSub2

  imul r11, InputLayerWidth * 4     // if colour = black (1) then r11 = InputLayerWidth * 4, else if white (0) then r11 = 0
  add rcx, r11
  add rdx, r11
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu ymm0, [rcx+r11]           //  Load from Self as source
  vpaddd ymm0, ymm0, [r8+r11]       //  Add
  vpsubd ymm0, ymm0, [r9+r11]       //  Sub
  vpsubd ymm0, ymm0, [r10+r11]      //  Sub
  vmovdqu [rdx+r11], ymm0           //  Save to Dest

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure UpdateSelf_asm_AVX2(source, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  .NOFRAME

  imul r9, InputLayerWidth * 4      // if colour = black (1) then r9 = InputLayerWidth * 4, else if white (0) then r9 = 0
  add rcx, r9
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu ymm0, [rcx+r11]           //  Load Self
  vpaddd ymm0, ymm0, [rdx+r11]      //  Add
  vpsubd ymm0, ymm0, [r8+r11]       //  Sub
  vmovdqu [rcx+r11], ymm0           //  Save Self

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


function GetEval_asm_AVX2(source, weights : PInteger; colour : integer) : int16;
  const
    clamp : integer = 46340;

  asm
  .NOFRAME

  test r8, r8
  JNZ @BTP

  @WTP:
  mov r9, rcx
  add r9, InputLayerWidth * 4
  mov r10, rdx
  add r10, InputLayerWidth * 4
  JMP @start

  @BTP:
  mov r9, rcx
  add rcx, InputLayerWidth * 4
  mov r10, rdx
  add r10, InputLayerWidth * 4

  @start:
  xor r8, r8                        // r8 = 0

  vpxor ymm0, ymm0, ymm0            // zero the accummulator
  vpxor ymm9, ymm9, ymm9            // zero the accummulator
  vpxor ymm3, ymm3, ymm3            // zero the floor

  vpbroadcastd ymm8, clamp

  @loop1:
                                    //  STP accumulator
  vmovdqu ymm1, [rcx+r8]            //  mov 8 int32 values B into ymm1
  vmovdqu ymm2, [rdx+r8]            //  mov 8 int32 values C into ymm2

  vpmaxsd ymm1, ymm3, ymm1          //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time
  vpminsd ymm1, ymm8, ymm1          //  clamp to sqrt(2^31)
  vpmulld ymm1, ymm1, ymm1          //  square the result   i.e. scReLU activation
  vpsrad ymm1, ymm1, 8              //  divide by 256

                                    //  OTP accumulator
  vmovdqu ymm5, [r9+r8]             //  mov 8 int32 values B into ymm5
  vmovdqu ymm6, [r10+r8]            //  mov 8 int32 values C into ymm6

  vpmaxsd ymm5, ymm3, ymm5          //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time
  vpminsd ymm5, ymm8, ymm5          //  clamp to sqrt(2^31)
  vpmulld ymm5, ymm5, ymm5          //  square the result   i.e. scReLU activation
  vpsrad ymm5, ymm5, 8              //  divide by 256

  vpmulld ymm4, ymm1, ymm2          //  Calculate D = B x C, 8 int32 at a time
  vpaddd ymm0, ymm0, ymm4

  vpmulld ymm7, ymm5, ymm6          //  Calculate D = B x C, 8 int32 at a time
  vpaddd ymm9, ymm9, ymm7

  add r8, 32
  cmp r8, InputLayerWidth * 4
  jl  @loop1

  vpaddd ymm0, ymm0, ymm9

  vextractf128 xmm1, ymm0, $1
  vzeroupper

  paddd xmm0, xmm1
  pshufd xmm1, xmm0, $4e
  paddd  xmm0, xmm1

  pshufd xmm1, xmm0, $11
  paddd xmm0, xmm1

  movd         eax, xmm0            // move low 32 bits to eax
  mov          edx, eax
  and          edx, $FFFF           //  edx = remainder
  sar          eax, 16              //  eax = floor(S / 2^16)
  test         eax, eax
  jge          @end                 //  if result >= 0, then result rounded down towards zero so no adjustment
  test         edx, edx             //  if remainder == 0, already exact, so no adjustment
  jz           @end

  add eax, $1                       //  adjust toward zero for negative S
  @end:
  end;


// ========================= AVX512 ==============================================

procedure GetBias_asm_AVX512(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu32 zmm0, [rdx+r11]         //  Load Bias
  vmovdqu32 [rcx+r11], zmm0         //  Save to Self

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure Add_asm_AVX512(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu32 zmm0, [rcx+r11]         //  Load Self
  vpaddd zmm0, zmm0,   [rdx+r11]    //  Add
  vmovdqu32 [rcx+r11], zmm0         //  Save Self

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure UpdateMove_asm_AVX512(source, dest, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  mov r10d, colour

  imul r10, InputLayerWidth * 4     // if colour = black (1) then r10 = InputLayerWidth * 4, else if white (0) then r10 = 0
  add rcx, r10
  add rdx, r10
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu32 zmm0, [rcx+r11]         //  Load from Self as source
  vpaddd zmm0, zmm0,   [r8+r11]     //  Add
  vpsubd zmm0, zmm0,   [r9+r11]     //  Sub
  vmovdqu32 [rdx+r11], zmm0         //  Save to Dest

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure UpdateCapture_asm_AVX512(source, dest, IndexAdd, IndexSub1, IndexSub2 : PInteger; colour : integer);
  asm
  mov r11d, colour
  mov r10, IndexSub2

  imul r11, InputLayerWidth * 4     // if colour = black (1) then r11 = InputLayerWidth * 4, else if white (0) then r11 = 0
  add rcx, r11
  add rdx, r11
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqu32 zmm0, [rcx+r11]         //  Load from Self as source
  vpaddd zmm0, zmm0,   [r8+r11]     //  Add
  vpsubd zmm0, zmm0,   [r9+r11]     //  Sub
  vpsubd zmm0, zmm0,   [r10+r11]    //  Sub
  vmovdqu32 [rdx+r11], zmm0         //  Save to Dest

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


procedure UpdateSelf_asm_AVX512(source, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  .NOFRAME

  imul r9, InputLayerWidth * 4       // if colour = black (1) then r11 = InputLayerWidth * 4, else if white (0) then r11 = 0
  add rcx, r9
  xor r11, r11                       // r11 = 0

  @loop1:
  vmovdqu32 zmm0, [rcx+r11]          //  Load Self
  vpaddd zmm0, zmm0,   [rdx+r11]     //  Add
  vpsubd zmm0, zmm0,   [r8+r11]      //  Sub
  vmovdqu32 [rcx+r11], zmm0          //  Save Self

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1
  end;


function GetEval_asm_AVX512(source, weights : PInteger; colour : integer) : int16;
  const
    clamp : integer = 46340;

  asm
  test r8, r8
  JNZ @BTP

  @WTP:
  mov r9, rcx
  add r9, InputLayerWidth * 4
  mov r10, rdx
  add r10, InputLayerWidth * 4
  JMP @start

  @BTP:
  mov r9, rcx
  add rcx, InputLayerWidth * 4
  mov r10, rdx
  add r10, InputLayerWidth * 4

  @start:
  xor r8, r8                         // r8 = 0

  vxorps zmm0, zmm0, zmm0            // zero the accummulator
  vxorps zmm9, zmm9, zmm9            // zero the accummulator
  vxorps zmm3, zmm3, zmm3            // zero the floor

  vpbroadcastd zmm8, clamp

  @loop1:
                                         //  STP accumulator
  vmovdqu32 zmm1, [rcx+r8]               //  mov 16 int32 values B into ymm1
  vmovdqu32 zmm2, [rdx+r8]               //  mov 16 int32 values C into ymm2

  vpmaxsd zmm1, zmm3, zmm1               //  perform ReLU  i.e. if B < 0 then B = 0, 16 int32 at a time
  vpminsd zmm1, zmm8, zmm1               //  clamp to sqrt(2^31)
  vpmulld zmm1, zmm1, zmm1               //  square the result i.e. scReLU activation
  vpsrad zmm1, zmm1, 8                   //  divide by 256

                                         //  OTP accumulator
  vmovdqu32 zmm5, [r9+r8]                //  mov 16 int32 values B into ymm5
  vmovdqu32 zmm6, [r10+r8]               //  mov 16 int32 values C into ymm6

  vpmaxsd zmm5, zmm3, zmm5               //  perform ReLU  i.e. if B < 0 then B = 0, 16 int32 at a time
  vpminsd zmm5, zmm8, zmm5               //  clamp to sqrt(2^31)
  vpmulld zmm5, zmm5, zmm5               //  square the result i.e. scReLU activation
  vpsrad zmm5, zmm5, 8                   //  divide by 256

  vpmulld zmm4, zmm1, zmm2               //  Calculate D = B x C, 16 int32 at a time
  vpaddd zmm0, zmm0, zmm4

  vpmulld zmm7, zmm5, zmm6               //  Calculate D = B x C, 16 int32 at a time
  vpaddd zmm9, zmm9, zmm7

  add r8, 64
  cmp r8, InputLayerWidth * 4
  jl  @loop1

  vpaddd zmm0, zmm0, zmm9

  vextracti32x8   ymm1, zmm0, 1          // ymm1 = upper 8 dwords
  vpaddd          ymm0, ymm0, ymm1       // reduce 16 -> 8 (sum pairs: i += i+8)

  vperm2i128      ymm1, ymm0, ymm0, $1   // swap 128-bit halves
  vpaddd          ymm0, ymm0, ymm1       // reduce 8 -> 4 (i += i+4 within lane)

  vpshufd         ymm1, ymm0, $B1        // shuffle  [1,0,3,2] per 128-bit lane
  vpaddd          ymm0, ymm0, ymm1       // reduce 4 -> 2 (i += neighbor)

  vpshufd         ymm1, ymm0, $4E        // shuffle  [2,3,0,1] per 128-bit lane
  vpaddd          ymm0, ymm0, ymm1       // reduce 2 -> 1 (final sum replicated in all dwords)

  vextracti32x4   xmm0, ymm0, 0          // get low 128 bits
  movd            eax, xmm0              // move low 32 bits to eax
  mov          edx, eax
  and          edx, $FFFF                //  edx = remainder
  sar          eax, 16                   //  eax = floor(S / 2^16)
  test         eax, eax
  jge          @end                      //  if result >= 0, then result rounded down towards zero so no adjustment
  test         edx, edx                  //  if remainder == 0, already exact, so no adjustment
  jz           @end

  add eax, $1                            //  adjust toward zero for negative S

  @end:
  end;


// ================== TGame_Net ================================================

class operator TGameNet.Initialize(out x: TGameNet);
  var
    PRNG : TPRNG;

  begin
  x.Hash := PRNG.Rand64;
  end;


function TGameNet.LoadFromResource(const ResourceIdentifier : string) : boolean;
  var
    Stream: TResourceStream;

  begin
  result := false;
  Stream := TResourceStream.Create(HInstance, ResourceIdentifier, RT_RCDATA);

    try
    stream.Read(weights[0], length(weights) * sizeof(int32));
    stream.Read(Bias[0], length(Bias) * sizeof(int32));

    stream.Read(OutputWeights[0], length(OutputWeights) * sizeof(int32));
    stream.Read(OutputBias[0], length(OutputBias) * sizeof(int32));
    result := true;

    finally
    Stream.Free;
    end
  end;


function TGameNet.LoadFromFile(const Filename : string) : boolean;
  var
    Stream: TMemoryStream;

  begin
  result := false;
  Stream := TMemoryStream.Create;
    try
    Stream.LoadFromFile(Filename);

    stream.ReadBuffer(weights[0], length(weights) * sizeof(int32));
    stream.ReadBuffer(Bias[0], length(Bias) * sizeof(int32));

    stream.ReadBuffer(OutputWeights[0], length(OutputWeights) * sizeof(int32));
    stream.ReadBuffer(OutputBias[0], length(OutputBias) * sizeof(int32));
    result := true;

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
  GetBias_asm(@Accumulator, @Bias[0], white);
  GetBias_asm(@Accumulator, @Bias[0], black);

  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := ( (pawn-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := ( (knight-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := ( (bishop-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := ( (rook-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := ( (queen-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := ( (king-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (pawn-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (knight-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (bishop-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (rook-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (queen-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_W := (Black * 384 + (king-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_W], white);
    end;


  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (pawn-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (knight-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (bishop-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (rook-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (queen-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := (Black * 384 + (king-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := ( (pawn-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := ( (knight-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := ( (bishop-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := ( (rook-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := ( (queen-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit_Alt(Pegs);
    index_B := ( (king-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator, @Weights[index_B], black);
    end;
  end;


procedure TGameNet.UpdateAccumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
  var
    index_Add1, index_Sub1, index_Sub2, dummy : integer;
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

  index_Sub1 := (ColourOffset + PieceOffset + source) * InputLayerWidth;                  // source
  index_Add1 := (ColourOffset + PieceOffset + dest) * InputLayerWidth;                    // dest

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
    index_Sub2 := ((384 - ColourOffset) + PieceOffset + TempDest) * InputLayerWidth;

    UpdateCapture_asm(@SourceAccumulator, @DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], @Weights[index_Sub2], white);
    end
   else
    UpdateMove_asm(@SourceAccumulator, @DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], white);

  // promotion piece
  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin
    PieceOffset := (Pawn - 1) * 64;
    index_Sub1 := (ColourOffset + PieceOffset + Dest) * InputLayerWidth;

    PieceOffset := (PromotionPiece - 1) * 64;
    index_Add1 := (ColourOffset + PieceOffset + Dest) * InputLayerWidth;

    UpdateSelf_asm(@DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], white);
    end;

  // castling move
  if (Piece = King) then
    begin
    if (Dest - Source = 2) then    // King Side Castle, adjust rook
      begin
      TempDest := Dest - 1;
      TempSource := Source + 3;
      PieceOffset := (Rook - 1) * 64;

      index_Sub1 := (ColourOffset + PieceOffset + TempSource) * InputLayerWidth;              // source
      index_Add1 := (ColourOffset + PieceOffset + TempDest) * InputLayerWidth;                // dest
      UpdateSelf_asm(@DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], white);
      end

    else if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      index_Sub1 := (ColourOffset + PieceOffset + TempSource) * InputLayerWidth;              // source
      index_Add1 := (ColourOffset + PieceOffset + TempDest) * InputLayerWidth;                // dest
      UpdateSelf_asm(@DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], white);
      end;
    end;

  PieceOffset := (Piece - 1) * 64;

  index_Sub1 := ((384 - ColourOffset) + PieceOffset + (source xor 56)) * InputLayerWidth;     // source
  index_Add1 := ((384 - ColourOffset) + PieceOffset + (dest xor 56)) * InputLayerWidth;       // dest

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
    index_Sub2 := (ColourOffset + PieceOffset + (TempDest xor 56)) * InputLayerWidth;

    UpdateCapture_asm(@SourceAccumulator, @DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], @Weights[index_Sub2], black);
    end
   else
     UpdateMove_asm(@SourceAccumulator, @DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], black);

  // promotion piece
  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin
    PieceOffset := (Pawn - 1) * 64;
    index_Sub1 := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * InputLayerWidth;

    PieceOffset := (PromotionPiece - 1) * 64;
    index_Add1 := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * InputLayerWidth;

    UpdateSelf_asm(@DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], black);
    end;

  // castling move
  if (Piece = King) then
    begin
    if (Dest - Source = 2) then    // King Side Castle, adjust rook
      begin
      TempDest := Dest - 1;
      TempSource := Source + 3;
      PieceOffset := (Rook - 1) * 64;

      index_Sub1 := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * InputLayerWidth;      // source
      index_Add1 := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * InputLayerWidth;        // dest
      UpdateSelf_asm(@DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], black);
      end
     else if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      index_Sub1 := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * InputLayerWidth;      // source
      index_Add1 := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * InputLayerWidth;        // dest
      UpdateSelf_asm(@DestAccumulator, @Weights[index_Add1], @Weights[index_Sub1], black);
      end;
    end;
  end;


function  TGameNet.ScoreFromAccumulator(const Accumulator : TAccumulator; colour, piececount : integer) : integer;
  var
    bucket, index : integer;

  begin
  bucket := bucketLookup[pieceCount];
  index := bucket * Accumulator_Width;

  result := GetEval_asm(@Accumulator, @OutputWeights[index], colour) + OutputBias[bucket];     // result = score from P.O.V. of side to play

  result := min(result, MateScoreCutoff - 1);
  result := max(result, -MateScoreCutoff + 1);
  end;


function TGameNet.Score(const Board : TBoard) : integer;
  var
    Accumulator : TAccumulator;
    pieceCount : integer;

  begin
  RefreshAccumulator(Accumulator, Board);
  pieceCount := bitcount(Board.Queens or Board.Rooks or Board.Bishops or Board.Knights);
  result := ScoreFromAccumulator(Accumulator, Board.ToPlay, pieceCount);                    // result = score in cp, from P.O.V. of side to play
  end;


function ScoreFromAccumulator(const Accumulator : TAccumulator; const Board : TBoard) : integer;
  var
    EndGameIndex : integer;
    EvalFunction : TEvalFunction;
    pieceCount : integer;

  begin
  if EvalHashTable.RetrieveScore(Board.Hash, result) = false then
    begin
    pieceCount := bitcount(Board.Queens or Board.Rooks or Board.Bishops or Board.Knights);
    result := Game_Net.ScoreFromAccumulator(Accumulator, Board.ToPlay, pieceCount);

    if bitcount(Board.WhitePegs or Board.BlackPegs) <= 5 then
      begin
      EndGameIndex := IndexFromBoard(Board);
      EvalFunction := EndGameLookup[EndGameIndex];

      if assigned(EvalFunction) then
        result := EvalFunction(Board, result);
      end;

    result := min(result, MateScoreCutoff - MaxSearchPly * 2);
    result := max(result, -MateScoreCutoff + MaxSearchPly * 2);

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
  begin
  Net.LoadFromResource('_768x512_x2_gen8_b6');
  Game_Net := @Net;
  end;


Initialization

  EvalHashTable.Clear;
  Load_GameNets;

  if AVX512f_Supported = true then
    begin
    GetBias_asm := GetBias_asm_AVX512;
    Add_asm := Add_asm_AVX512;

    UpdateMove_asm := UpdateMove_asm_AVX512;
    UpdateCapture_asm := UpdateCapture_asm_AVX512;
    UpdateSelf_asm := UpdateSelf_asm_AVX512;

    GetEval_asm := GetEval_asm_AVX512;
    end
   else {if AVX2_Supported = true then }
    begin
    GetBias_asm := GetBias_asm_AVX2;
    Add_asm := Add_asm_AVX2;

    UpdateMove_asm := UpdateMove_asm_AVX2;
    UpdateCapture_asm := UpdateCapture_asm_AVX2;
    UpdateSelf_asm := UpdateSelf_asm_AVX2;

    GetEval_asm := GetEval_asm_AVX2;
    end;

end.
