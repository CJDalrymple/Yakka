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
  Winapi.Windows, System.Classes, System.SysUtils, System.Math, Common, GameDef, EndGame, CPU_Info;

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
  BucketLookup : array[0..31] of integer = (0, 0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
                                            5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);

type
  TAccumulator = array[0..Accumulator_Width-1] of int32;
  PAccumulator = ^TAccumulator;

  TAlignableAccumulator = array[0..Accumulator_Width-1 + 63] of int32;

  T_GetBias_asm = procedure(dest, IndexAdd : PInteger; colour : integer);
  T_Add_asm = procedure(dest, IndexAdd : PInteger; colour : integer);

  T_UpdateMove_asm = procedure(source, dest, IndexAdd, IndexSub : PInteger; colour : integer);
  T_UpdateCapture_asm = procedure(source, dest, IndexAdd, IndexSub1, IndexSub2 : PInteger; colour : integer);
  T_UpdateSelf_asm = procedure(source, IndexAdd, IndexSub : PInteger; colour : integer);

  T_GetEval_asm = function(source, weights : PInteger; colour : integer) : int16;


type
  T_EvalHash_Table = record
    const
      TableSize = $40000;             // 2^18       = 262,144 slots
      TableMask = TableSize - 1 ;     // 2^18 - 1
      TagMask   = UInt64($FFFFFFFFFFFF0000);

    var
      Table : Array[0..TableSize-1] of UInt64;

    function RetrieveScore(HashCode : UInt64; var score : integer) : boolean;
    procedure StoreScore(HashCode : UInt64; score : integer);
    procedure Clear;
    end;


type
  TGameNet = record

  type
    TWeights = array[0..Attribute_Count * InputLayerWidth - 1] of int32;
    PWeights = ^TWeights;

    TBias = array[0..InputLayerWidth - 1] of int32;
    PBias = ^TBias;

    TOutputWeights = array[0..buckets * Accumulator_Width - 1] of int32;
    POutputWeights = ^TOutputWeights;

    TOutputBias = array[0..buckets - 1] of int32;
    POutputBias = ^TOutputBias;

  var
    Weights : PWeights;
    Bias : PBias;

    OutputWeights : POutputWeights;
    OutputBias : POutputBias;

    Hash : UInt64;

  public
    class operator Initialize (out x: TGameNet);
    class operator Finalize(var x: TGameNet);

    function LoadFromResource(const ResourceIdentifier : string) : boolean;
    function LoadFromFile(const FileName : string) : boolean;

    procedure RefreshAccumulator(Accumulator : PAccumulator; const Board : TBoard);
    procedure UpdateAccumulator(SourceAccumulator, DestAccumulator : PAccumulator; Move : TMove; const Board : TBoard);

    function  ScoreFromAccumulator(Accumulator : PAccumulator; colour, pieceCount : integer) : integer;
    function  Score(const Board : TBoard) : integer;
    end;


function ScoreFromAccumulator(Accumulator : PAccumulator; const Board : TBoard) : integer;
procedure Update_Accumulator(SourceAccumulator, DestAccumulator : PAccumulator; Move : TMove; const Board : TBoard);
procedure Refresh_Accumulator(Accummulator : PAccumulator; const Board : TBoard);


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

{$CODEALIGN 16}

uses
  Search;


// ================= Eval Hash ==================================================

// score [-32767..32767]


procedure T_EvalHash_Table.StoreScore(HashCode : UInt64; Score : integer);
  begin
  Table[HashCode and TableMask] := (HashCode and TagMask) or UInt64(Score + 32768);
  end;


function T_EvalHash_Table.RetrieveScore(HashCode : UInt64; var score : integer) : boolean;
  var
    entry : UInt64;

  begin
  entry := Table[HashCode and TableMask];
  result := ((entry xor HashCode) and TagMask) = 0;
  if result then
    score := integer(entry and $FFFF) - 32768;
  end;


procedure T_EvalHash_Table.Clear;
  begin
  FillChar(Table, SizeOf(Table), 0);
  end;


// ========================= AVX2 ==============================================

procedure GetBias_asm_AVX2(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  movzx r8, r8b
  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa ymm0, [rdx+r11]           //  Load Bias
  vmovdqa [rcx+r11], ymm0           //  Save to Self

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


procedure Add_asm_AVX2(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  movzx r8, r8b
  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa ymm0, [rcx+r11]           //  Load Self
  vpaddd ymm0, ymm0, [rdx+r11]      //  Add
  vmovdqa [rcx+r11], ymm0           //  Save Self

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
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
  vmovdqa ymm0, [rcx+r11]           //  Load from Self as source
  vpaddd ymm0, ymm0, [r8+r11]       //  Add
  vpsubd ymm0, ymm0, [r9+r11]       //  Sub
  vmovdqa [rdx+r11], ymm0           //  Save to Dest

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
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
  vmovdqa ymm0, [rcx+r11]           //  Load from Self as source
  vpaddd ymm0, ymm0, [r8+r11]       //  Add
  vpsubd ymm0, ymm0, [r9+r11]       //  Sub
  vpsubd ymm0, ymm0, [r10+r11]      //  Sub
  vmovdqa [rdx+r11], ymm0           //  Save to Dest

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


procedure UpdateSelf_asm_AVX2(source, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  .NOFRAME

  movzx r9, r9b
  imul r9, InputLayerWidth * 4      // if colour = black (1) then r9 = InputLayerWidth * 4, else if white (0) then r9 = 0
  add rcx, r9
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa ymm0, [rcx+r11]           //  Load Self
  vpaddd ymm0, ymm0, [rdx+r11]      //  Add
  vpsubd ymm0, ymm0, [r8+r11]       //  Sub
  vmovdqa [rcx+r11], ymm0           //  Save Self

  add r11, 32
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


function GetEval_asm_AVX2(source, weights : PInteger; colour : integer) : int16;
  const
    clamp : integer = 46340;

  asm
  .NOFRAME

  test r8d, r8d
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
  vpxor ymm5, ymm5, ymm5            // zero the accummulator
  vpxor ymm3, ymm3, ymm3            // zero the floor

  vpbroadcastd ymm4, clamp

  @loop1:
                                    //  STP accumulator
  vmovdqa ymm1, [rcx+r8]            //  mov 8 int32 values B into ymm1
  vpmaxsd ymm1, ymm3, ymm1          //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time
  vpminsd ymm1, ymm4, ymm1          //  clamp to sqrt(2^31)
  vpmulld ymm1, ymm1, ymm1          //  square the result   i.e. scReLU activation
  vpsrad  ymm1, ymm1, 8             //  divide by 256
  vpmulld ymm1, ymm1, [rdx+r8]      //  Calculate D = B x C, 8 int32 at a time
  vpaddd  ymm0, ymm0, ymm1          //  sum result

                                    //  OTP accumulator
  vmovdqa ymm2, [r9+r8]             //  mov 8 int32 values B into ymm5
  vpmaxsd ymm2, ymm3, ymm2          //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time
  vpminsd ymm2, ymm4, ymm2          //  clamp to sqrt(2^31)
  vpmulld ymm2, ymm2, ymm2          //  square the result   i.e. scReLU activation
  vpsrad  ymm2, ymm2, 8             //  divide by 256
  vpmulld ymm2, ymm2, [r10+r8]      //  Calculate D = B x C, 8 int32 at a time
  vpaddd  ymm5, ymm5, ymm2          //  sum result

  add r8, 32
  cmp r8, InputLayerWidth * 4
  jl  @loop1

  vpaddd ymm0, ymm0, ymm5

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

  movzx r8, r8b
  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa32 zmm0, [rdx+r11]         //  Load Bias
  vmovdqa32 [rcx+r11], zmm0         //  Save to Self

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


procedure Add_asm_AVX512(dest, IndexAdd : PInteger; colour : integer);
  asm
  .NOFRAME

  movzx r8, r8b
  imul r8, InputLayerWidth * 4      // if colour = black (1) then r8 = InputLayerWidth * 4, else if white (0) then r8 = 0
  add rcx, r8
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa32 zmm0, [rcx+r11]         //  Load Self
  vpaddd zmm0, zmm0,   [rdx+r11]    //  Add
  vmovdqa32 [rcx+r11], zmm0         //  Save Self

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


procedure UpdateMove_asm_AVX512(source, dest, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  .NOFRAME

  mov r10d, colour
  imul r10, InputLayerWidth * 4     // if colour = black (1) then r10 = InputLayerWidth * 4, else if white (0) then r10 = 0
  add rcx, r10
  add rdx, r10
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa32 zmm0, [rcx+r11]         //  Load from Self as source
  vpaddd zmm0, zmm0,   [r8+r11]     //  Add
  vpsubd zmm0, zmm0,   [r9+r11]     //  Sub
  vmovdqa32 [rdx+r11], zmm0         //  Save to Dest

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


procedure UpdateCapture_asm_AVX512(source, dest, IndexAdd, IndexSub1, IndexSub2 : PInteger; colour : integer);
  asm
  .NOFRAME

  mov r11d, colour
  mov r10, IndexSub2

  imul r11, InputLayerWidth * 4     // if colour = black (1) then r11 = InputLayerWidth * 4, else if white (0) then r11 = 0
  add rcx, r11
  add rdx, r11
  xor r11, r11                      // r11 = 0

  @loop1:
  vmovdqa32 zmm0, [rcx+r11]         //  Load from Self as source
  vpaddd zmm0, zmm0,   [r8+r11]     //  Add
  vpsubd zmm0, zmm0,   [r9+r11]     //  Sub
  vpsubd zmm0, zmm0,   [r10+r11]    //  Sub
  vmovdqa32 [rdx+r11], zmm0         //  Save to Dest

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


procedure UpdateSelf_asm_AVX512(source, IndexAdd, IndexSub : PInteger; colour : integer);
  asm
  .NOFRAME

  movzx r9, r9b
  imul r9, InputLayerWidth * 4       // if colour = black (1) then r11 = InputLayerWidth * 4, else if white (0) then r11 = 0
  add rcx, r9
  xor r11, r11                       // r11 = 0

  @loop1:
  vmovdqa32 zmm0, [rcx+r11]          //  Load Self
  vpaddd zmm0, zmm0,   [rdx+r11]     //  Add
  vpsubd zmm0, zmm0,   [r8+r11]      //  Sub
  vmovdqa32 [rcx+r11], zmm0          //  Save Self

  add r11, 64
  cmp r11, InputLayerWidth * 4
  jl  @loop1

  vzeroupper
  end;


function GetEval_asm_AVX512(source, weights : PInteger; colour : integer) : int16;
  const
    clamp : integer = 46340;

  asm
  .NOFRAME

  test r8d, r8d
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

  vpxord zmm0, zmm0, zmm0            // zero the accummulator
  vpxord zmm5, zmm5, zmm5            // zero the accummulator
  vpxord zmm3, zmm3, zmm3            // zero the floor

  vpbroadcastd zmm4, clamp

  @loop1:
                                       //  STP accumulator
  vmovdqa32 zmm1, [rcx+r8]             //  mov 16 int32 values B into ymm1
  vpmaxsd   zmm1, zmm3, zmm1           //  perform ReLU  i.e. if B < 0 then B = 0, 16 int32 at a time
  vpminsd   zmm1, zmm4, zmm1           //  clamp to sqrt(2^31)
  vpmulld   zmm1, zmm1, zmm1           //  square the result i.e. scReLU activation
  vpsrad    zmm1, zmm1, 8              //  divide by 256
  vpmulld   zmm1, zmm1, [rdx+r8]       //  Calculate D = B x C, 16 int32 at a time
  vpaddd    zmm0, zmm0, zmm1

                                       //  OTP accumulator
  vmovdqa32 zmm2, [r9+r8]              //  mov 16 int32 values B into ymm5
  vpmaxsd   zmm2, zmm2, zmm3           //  perform ReLU  i.e. if B < 0 then B = 0, 16 int32 at a time
  vpminsd   zmm2, zmm4, zmm2           //  clamp to sqrt(2^31)
  vpmulld   zmm2, zmm2, zmm2           //  square the result i.e. scReLU activation
  vpsrad    zmm2, zmm2, 8              //  divide by 256
  vpmulld   zmm2, zmm2, [r10+r8]       //  Calculate D = B x C, 16 int32 at a time
  vpaddd    zmm5, zmm5, zmm2

  add r8, 64
  cmp r8, InputLayerWidth * 4
  jl  @loop1

  vpaddd zmm0, zmm0, zmm5

  vextracti64x4   ymm1, zmm0, 1          // ymm1 = upper 8 dwords
  vpaddd          ymm0, ymm0, ymm1       // reduce 16 -> 8 (sum pairs: i += i+8)

  vperm2i128      ymm1, ymm0, ymm0, $1   // swap 128-bit halves
  vpaddd          ymm0, ymm0, ymm1       // reduce 8 -> 4 (i += i+4 within lane)

  vpshufd         ymm1, ymm0, $B1        // shuffle  [1,0,3,2] per 128-bit lane
  vpaddd          ymm0, ymm0, ymm1       // reduce 4 -> 2 (i += neighbor)

  vpshufd         ymm1, ymm0, $4E        // shuffle  [2,3,0,1] per 128-bit lane
  vpaddd          ymm0, ymm0, ymm1       // reduce 2 -> 1 (final sum replicated in all dwords)

  vzeroupper

  //vextracti32x4   xmm0, ymm0, 0          // get low 128 bits
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

  x.Weights := PWeights(_aligned_malloc(SizeOf(TWeights), 64));
  x.Bias := PBias(_aligned_malloc(SizeOf(TBias), 64));
  x.OutputWeights := POutputWeights(_aligned_malloc(SizeOf(TOutputWeights), 64));
  x.OutputBias := POutputBias(_aligned_malloc(SizeOf(TOutputBias), 64));
  end;


class operator TGameNet.Finalize(var x: TGameNet);
  begin
  _aligned_free(x.Weights);
  _aligned_free(x.Bias);
  _aligned_free(x.OutputWeights);
  _aligned_free(x.OutputBias);
  end;


function TGameNet.LoadFromResource(const ResourceIdentifier : string) : boolean;
  var
    Stream: TResourceStream;

  begin
  result := false;
  Stream := TResourceStream.Create(HInstance, ResourceIdentifier, RT_RCDATA);

    try
    if Stream.Size <> SizeOf(TWeights) + SizeOf(TBias) + SizeOf(TOutputWeights) + SizeOf(TOutputBias) then
      exit;

    stream.Read(weights[0], length(weights^) * sizeof(int32));
    stream.Read(Bias[0], length(Bias^) * sizeof(int32));

    stream.Read(OutputWeights[0], length(OutputWeights^) * sizeof(int32));
    stream.Read(OutputBias[0], length(OutputBias^) * sizeof(int32));
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

    if Stream.Size <> SizeOf(TWeights) + SizeOf(TBias) + SizeOf(TOutputWeights) + SizeOf(TOutputBias) then
      exit;

    stream.ReadBuffer(weights[0], length(weights^) * sizeof(int32));
    stream.ReadBuffer(Bias[0], length(Bias^) * sizeof(int32));

    stream.ReadBuffer(OutputWeights[0], length(OutputWeights^) * sizeof(int32));
    stream.ReadBuffer(OutputBias[0], length(OutputBias^) * sizeof(int32));
    result := true;

    finally
    Stream.Free;
    end
  end;


procedure TGameNet.RefreshAccumulator(Accumulator : PAccumulator; const Board : TBoard);
  var
    index_W, index_B : int64;
    Pegs : UInt64;
    Cell : integer;

  begin
  GetBias_asm(@Accumulator[0], @Bias[0], white);
  GetBias_asm(@Accumulator[0], @Bias[0], black);

  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := ( (pawn-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := ( (knight-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := ( (bishop-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := ( (rook-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := ( (queen-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := ( (king-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := (Black * 384 + (pawn-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := (Black * 384 + (knight-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := (Black * 384 + (bishop-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := (Black * 384 + (rook-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := (Black * 384 + (queen-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_W := (Black * 384 + (king-1) * 64 + Cell) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_W], white);
    end;


  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := (Black * 384 + (pawn-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := (Black * 384 + (knight-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := (Black * 384 + (bishop-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := (Black * 384 + (rook-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := (Black * 384 + (queen-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := (Black * 384 + (king-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := ( (pawn-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := ( (knight-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := ( (bishop-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := ( (rook-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := ( (queen-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    Cell := PopLowBit(Pegs);
    index_B := ( (king-1) * 64 + (Cell xor 56)) * InputLayerWidth;
    Add_asm(@Accumulator[0], @Weights[index_B], black);
    end;
  end;


procedure TGameNet.UpdateAccumulator(SourceAccumulator, DestAccumulator : PAccumulator; Move : TMove; const Board : TBoard);
  var
    index_Add1, index_Sub1, index_Sub2, dummy : integer;
    Source, Dest, Piece, CapturedPiece, PromotionPiece, epCell, TempSource, TempDest : integer;
    ColourOffset, PieceOffset, CapturedOffset, PromotionOffset : integer;

  begin
  Piece := (Move shr 12) and $F;              // Piece := Move.Piece
  Source :=  Move and $3F;                    // Source := Move.Source
  Dest :=  (Move shr 6) and $3F;              // Dest := Move.Dest

  CapturedPiece := (Move shr 16) and $F;      // CapturedPiece := Move.CapturedPiece
  PromotionPiece := (Move shr 20) and $F;     // PromotionPiece := Move.PromotionPiece
  epCell := (Move shr 30) and $3F;

  ColourOffset :=  (1-Board.ToPlay) * 384;

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

    PieceOffset := (Piece - 1) * 64;
    CapturedOffset := (CapturedPiece - 1) * 64;

    index_Sub1 := (ColourOffset + PieceOffset + source) * InputLayerWidth;                      // source
    index_Add1 := (ColourOffset + PieceOffset + dest) * InputLayerWidth;                        // dest
    index_Sub2 := ((384 - ColourOffset) + CapturedOffset + TempDest) * InputLayerWidth;
    UpdateCapture_asm(@SourceAccumulator[0], @DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], @Weights[index_Sub2], white);

    index_Sub1 := ((384 - ColourOffset) + PieceOffset + (source xor 56)) * InputLayerWidth;     // source
    index_Add1 := ((384 - ColourOffset) + PieceOffset + (dest xor 56)) * InputLayerWidth;       // dest
    index_Sub2 := (ColourOffset + CapturedOffset + (TempDest xor 56)) * InputLayerWidth;
    UpdateCapture_asm(@SourceAccumulator[0], @DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], @Weights[index_Sub2], black);
    end
   else
    begin
    PieceOffset := (Piece - 1) * 64;

    index_Sub1 := (ColourOffset + PieceOffset + source) * InputLayerWidth;                      // source
    index_Add1 := (ColourOffset + PieceOffset + dest) * InputLayerWidth;                        // dest
    UpdateMove_asm(@SourceAccumulator[0], @DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], white);

    index_Sub1 := ((384 - ColourOffset) + PieceOffset + (source xor 56)) * InputLayerWidth;     // source
    index_Add1 := ((384 - ColourOffset) + PieceOffset + (dest xor 56)) * InputLayerWidth;       // dest
    UpdateMove_asm(@SourceAccumulator[0], @DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], black);
    end;

  // promotion piece
  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin
    PieceOffset := (Pawn - 1) * 64;
    PromotionOffset := (PromotionPiece - 1) * 64;

    index_Sub1 := (ColourOffset + PieceOffset + Dest) * InputLayerWidth;
    index_Add1 := (ColourOffset + PromotionOffset + Dest) * InputLayerWidth;
    UpdateSelf_asm(@DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], white);

    index_Sub1 := ((384 - ColourOffset) + PieceOffset + (Dest xor 56)) * InputLayerWidth;
    index_Add1 := ((384 - ColourOffset) + PromotionOffset + (Dest xor 56)) * InputLayerWidth;
    UpdateSelf_asm(@DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], black);
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
      UpdateSelf_asm(@DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], white);

      index_Sub1 := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * InputLayerWidth;      // source
      index_Add1 := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * InputLayerWidth;        // dest
      UpdateSelf_asm(@DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], black);
      end

    else if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      index_Sub1 := (ColourOffset + PieceOffset + TempSource) * InputLayerWidth;              // source
      index_Add1 := (ColourOffset + PieceOffset + TempDest) * InputLayerWidth;                // dest
      UpdateSelf_asm(@DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], white);

      index_Sub1 := ((384 - ColourOffset) + PieceOffset + (TempSource xor 56)) * InputLayerWidth;      // source
      index_Add1 := ((384 - ColourOffset) + PieceOffset + (TempDest xor 56)) * InputLayerWidth;        // dest
      UpdateSelf_asm(@DestAccumulator[0], @Weights[index_Add1], @Weights[index_Sub1], black);
      end;
    end;
  end;


function  TGameNet.ScoreFromAccumulator(Accumulator : PAccumulator; colour, piececount : integer) : integer;
  var
    bucket, index : integer;

  begin
  bucket := bucketLookup[pieceCount];
  index := bucket * Accumulator_Width;

  result := GetEval_asm(PInteger(Accumulator), @OutputWeights[index], colour) + OutputBias[bucket];     // result = score from P.O.V. of side to play

  result := min(result, TB_WinCutoff - MaxSearchPly * 2);
  result := max(result, -TB_WinCutoff + MaxSearchPly * 2);

  end;


function TGameNet.Score(const Board : TBoard) : integer;
  var
    Alignable_Accumulator : TAlignableAccumulator;
    Accumulator : PAccumulator;
    pieceCount : integer;

  begin
  Accumulator := PAccumulator(((NativeUInt(@Alignable_Accumulator) + 63) and not 63));

  RefreshAccumulator(Accumulator, Board);
  pieceCount := bitcount(Board.Queens or Board.Rooks or Board.Bishops or Board.Knights);
  result := ScoreFromAccumulator(Accumulator, Board.ToPlay, pieceCount);                    // result = score in cp, from P.O.V. of side to play
  end;


function ScoreFromAccumulator(Accumulator : PAccumulator; const Board : TBoard) : integer;
  var
    EndGameIndex : integer;
    EvalFunction : TEvalFunction;
    pieceCount : integer;
    Hash : UInt64;

  begin
  Hash := Board.Hash xor Board.HalfMoveHash;

  if EvalHashTable.RetrieveScore(Hash, result) = false then
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

    if Board.HalfCount > Board.HalfMoveReductionCutoff then
      result := result * max (0, 65 + Board.HalfMoveReductionCutoff - Board.HalfCount) div 64;     //  HalfMoveReductionCutoff = 64

    result := min(result, TB_WinCutoff - MaxSearchPly * 2);
    result := max(result, -TB_WinCutoff + MaxSearchPly * 2);

    EvalHashTable.StoreScore(Hash, result);
    end;

  end;


procedure Update_Accumulator(SourceAccumulator, DestAccumulator : PAccumulator; Move : TMove; const Board : TBoard);
  begin
  Game_Net.UpdateAccumulator(SourceAccumulator, DestAccumulator, Move, Board);
  end;


procedure Refresh_Accumulator(Accummulator : PAccumulator; const Board : TBoard);
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
