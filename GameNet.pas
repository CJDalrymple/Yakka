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
  Winapi.Windows, System.Classes, System.SysUtils, Common, GameDef, EndGame;

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
  TAccumulator_64 = record
    values : array[0..127] of int16;

    procedure LoadBias_asm(sourceW, sourceB : PSmallInt);
    procedure Load_asm;
    procedure Save_asm;
    procedure Add_asm(sourceW, sourceB : PSmallInt);
    procedure Sub_asm(sourceW, sourceB : PSmallInt);
    function GetEval_asm(weights : PSmallInt; colour : integer) : int16;
    end;

type
  TAccumulator = TAccumulator_64;

type
  TGameNet = record

   // net = [768x2]>64>1

  const
    InputLayerSize = 1536;
    OutputLayerSize = 64;

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


// ==================  TAccumulator  ===========================================

procedure TAccumulator_64.LoadBias_asm(sourceW, sourceB : PSmallInt);
  asm
  .NOFRAME
	vmovdqu ymm8, [rdx]       //  Load W0
  vmovdqu ymm9, [rdx+32]    //  Load W1
  vmovdqu ymm10, [rdx+64]   //  Load W2
  vmovdqu ymm11, [rdx+96]   //  Load W3.

	vmovdqu ymm12, [r8]      //  Load B0
  vmovdqu ymm13, [r8+32]   //  Load B1
	vmovdqu ymm14, [r8+64]   //  Load B2
  vmovdqu ymm15, [r8+96]   //  Load B3.
  end;


procedure TAccumulator_64.Load_asm;
  asm
  .NOFRAME
	vmovdqu ymm8, [rcx]       //  Load W0
  vmovdqu ymm9, [rcx+32]    //  Load W1
	vmovdqu ymm10, [rcx+64]   //  Load W2
  vmovdqu ymm11, [rcx+96]   //  Load W3.
	vmovdqu ymm12, [rcx+128]   //  Load B0
  vmovdqu ymm13, [rcx+160]   //  Load B1
	vmovdqu ymm14, [rcx+192]   //  Load B2
  vmovdqu ymm15, [rcx+224]   //  Load B3.
  end;


procedure TAccumulator_64.Save_asm;
  asm
  .NOFRAME
	vmovdqu [rcx], ymm8        //  Save A0
  vmovdqu [rcx+32], ymm9     //  Save A1
	vmovdqu [rcx+64], ymm10    //  Save A2
  vmovdqu [rcx+96], ymm11    //  Save A3
	vmovdqu [rcx+128], ymm12   //  Save B0
  vmovdqu [rcx+160], ymm13   //  Save B1
	vmovdqu [rcx+192], ymm14   //  Save B2
  vmovdqu [rcx+224], ymm15   //  Save B3
  end;


procedure TAccumulator_64.Add_asm(sourceW, sourceB : PSmallInt);
  asm
  .NOFRAME
	vpaddw ymm8, ymm8, [rdx]        //  Add W0
	vpaddw ymm12, ymm12, [r8]       //  Add B0
	vpaddw ymm9, ymm9, [rdx+32]     //  Add W1
	vpaddw ymm13, ymm13, [r8+32]    //  Add B1
	vpaddw ymm10, ymm10, [rdx+64]   //  Add W2
	vpaddw ymm14, ymm14, [r8+64]    //  Add B2
	vpaddw ymm11, ymm11, [rdx+96]   //  Add W3
	vpaddw ymm15, ymm15, [r8+96]    //  Add B3
  end;


procedure TAccumulator_64.Sub_asm(sourceW, sourceB : PSmallInt);
  asm
  .NOFRAME
	vpsubw ymm8, ymm8, [rdx]        //  Sub W0
	vpsubw ymm12, ymm12, [r8]       //  Sub B0
	vpsubw ymm9, ymm9, [rdx+32]     //  Sub W1
	vpsubw ymm13, ymm13, [r8+32]    //  Sub B1
	vpsubw ymm10, ymm10, [rdx+64]   //  Sub W2
	vpsubw ymm14, ymm14, [r8+64]    //  Sub B2
	vpsubw ymm11, ymm11, [rdx+96]   //  Sub W3
	vpsubw ymm15, ymm15, [r8+96]    //  Sub B3
  end;


function TAccumulator_64.GetEval_asm(weights : PSmallInt; colour : integer) : int16;
  asm
  .NOFRAME
  imul r8, 64*2       // if colour = black (1) then r8 = 128, else if white (0) then r8 = 0
  add rcx, r8

  mov r8, 56;

  vxorps ymm3, ymm3, ymm3          // zero the ReLU zero reference
  vxorps ymm0, ymm0, ymm0          // zero the accummulator

  @loop1:

  vpmovsxwd ymm1, [rcx+r8*2]             //  mov 8 int16 values B into ymm1 and sign extend to int32
  vpmovsxwd ymm2, [rdx+r8*2]             //  mov 8 int16 values C into ymm2 and sign extend to int32
  vpmaxsw ymm1, ymm3, ymm1               //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time

  vpmovsxwd ymm5, [rcx+r8*2-16]          //  mov 8 int16 values B into ymm1 and sign extend to int32
  vpmovsxwd ymm6, [rdx+r8*2-16]          //  mov 8 int16 values C into ymm2 and sign extend to int32
  vpmaxsw ymm5, ymm3, ymm5               //  perform ReLU  i.e. if B < 0 then B = 0, 8 int32 at a time

  vpmulld ymm4, ymm1, ymm2               //  Calculate D = low(B.C), 8 int32 at a time
  vpaddd ymm0, ymm0, ymm4

  vpmulld ymm7, ymm5, ymm6               //  Calculate D = low(B.C), 8 int32 at a time
  vpaddd ymm0, ymm0, ymm7

	sub r8, 16
	jge @loop1

  vextractf128 xmm1, ymm0, $1
  paddd xmm0, xmm1

  pshufd xmm1, xmm0, $4e
  paddd  xmm0, xmm1

  pshufd xmm1, xmm0, $11
  paddd xmm0, xmm1

  pextrw eax, xmm0, $1                  //  extract the high word = equivalent to division by 256 x 256
                                        //  if positive then result rounded down towards zero
  test eax, eax                         //  if negative then shift rounds down towards negative infinity, so add one if negative to round result up towards zero
  jns @end
  add eax, $1
  @end:

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
    index : int64;
    Pegs : UInt64;

  begin
  Accumulator.LoadBias_asm(@Bias, @Bias);

  Pegs := Board.WhitePegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    index := PopLowBit_Alt(Pegs) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.WhitePegs and Board.Knights;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 64) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.WhitePegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 128) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.WhitePegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 192) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.WhitePegs and Board.Queens;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 256) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.WhitePegs and Board.Kings;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 320) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;


  Pegs := Board.BlackPegs and Board.Pawns;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 384) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.BlackPegs and Board.Knights;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 448) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.BlackPegs and Board.Bishops;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 512) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.BlackPegs and Board.Rooks;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 576) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.BlackPegs and Board.Queens;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 640) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Pegs := Board.BlackPegs and Board.Kings;
  while Pegs <> 0 do
    begin
    index := (PopLowBit_Alt(Pegs) + 704) * OutputLayerSize;
    Accumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  Accumulator.Save_asm;
  end;


procedure TGameNet.UpdateAccumulator(const SourceAccumulator : TAccumulator; var DestAccumulator : TAccumulator; Move : TMove; const Board : TBoard);
  var
    index : integer;
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

  index := (ColourOffset + PieceOffset + source) * OutputLayerSize;
  SourceAccumulator.Sub_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);

  // dest

  index := (ColourOffset + PieceOffset + Dest) * OutputLayerSize;
  SourceAccumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);

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
    index := (384 - ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
    SourceAccumulator.Sub_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
    end;

  // promotion piece

  if PromotionPiece <> 0 then // add promotion piece & deduct pawn from accummulator
    begin

    PieceOffset := (Pawn - 1) * 64;

    index := (ColourOffset + PieceOffset + Dest) * OutputLayerSize;         // removed pawn
    SourceAccumulator.Sub_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);

    PieceOffset := (PromotionPiece - 1) * 64;
    index :=(ColourOffset + PieceOffset + Dest) * OutputLayerSize;         // promoted piece
    SourceAccumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
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

      index := (ColourOffset + PieceOffset + TempSource) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);

      // dest

      index := (ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
      end;

    if (Dest - Source = -2) then   // Queen Side Castle, adjust rook
      begin
      TempDest := Dest + 1;
      TempSource := Source - 4;
      PieceOffset := (Rook - 1) * 64;

      // source

      index := (ColourOffset + PieceOffset + TempSource) * OutputLayerSize;
      SourceAccumulator.Sub_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);

      // dest

      index := (ColourOffset + PieceOffset + TempDest) * OutputLayerSize;
      SourceAccumulator.Add_asm(@Weights[index], @Weights[index + 768 * OutputLayerSize]);
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

    if abs(result) < MateScoreCutoff then
      if bitcount(Board.WhitePegs or Board.BlackPegs) <= 5 then
        begin
        EndGameIndex := IndexFromBoard(Board);
        EvalFunction := EndGameLookup[EndGameIndex];

        if assigned(EvalFunction) then
          result := EvalFunction(Board, result);
        end;

    EvalHashTable.StoreScore(Board.Hash, result);
    end;
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
  //  FilePath := ExtractFileDir(ParamStr(0)) + '\..\..\';
  //  FileName := '\NNUE 1536x64 rev 50.net';
  //  Game_Net.LoadFromFile(FilePath + '\' + FileName);

  Game_Net.LoadFromResource('_1536x64_v1');        //  NNUE 1536x64 rev 50.net
  end;


Initialization

  EvalHashTable.ClearTable;
  Load_GameNet;

end.
