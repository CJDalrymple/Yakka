//  The MIT License (MIT)

//  Chess Engine Yakka
//  Copyright (c) 2026 Christopher Crone

//  Syzygy Tablebase Code
//  Copyright (c) 2025 Ronald de Man

//  The Syzygy tablebases are created by Ronald de Man.
//  The Syzygy tablebase access code is a Delphi translation of the C code
//  written by Ronald de Man, and published at https://github.com/syzygy1/probetool,
//  with modifications as required to interface with Yakka code.

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


unit Syzygy;

interface

uses
  SysUtils, Classes, WinApi.Windows, System.SyncObjs, System.StrUtils, Common, GameDef;

{$ALIGN 8}

const
  TB_PIECES = 7;
  TB_HASHBITS = 12;
  TB_MAX_PIECE = 650;
  TB_MAX_PAWN = 861;

  Suffix : array[0..1] of Ansistring = ('.rtbw', '.rtbz');

  Magic: array[0..1] of UInt32 = ($5D23E871, $A50C66D7);

  PieceToAnsiChar : array[0..7] of AnsiChar = ('0', 'P', 'N', 'B', 'R', 'Q', 'K', '0');

  FileToFile: array[0..7] of Byte = (0, 1, 2, 3, 3, 2, 1, 0);

  WdlToMap: array[0..4] of Integer = (1, 3, 0, 2, 0);

  PAFlags: array[0..4] of Byte = (8, 0, 0, 0, 4);

  WdlToDtz: array[0..4] of Integer = (-1, -101, 0, 101, 1);

  matKey : array[0..15] of UInt64 =
     ($0000000000000000,
      $D8C54B6242CC4658,
      $B84CD5FD6ADF1A60,
      $0C8FA4E03DA65E01,
      $A16591BE7916B4AC,
      $6E4682F9525CC4C4,
      $0000000000000000,
      $0000000000000000,

      $0000000000000000,
      $60ADC383AFAC9D1B,
      $97BBDD24AFA0B2D1,
      $298CEFB2F9BFAC89,
      $6B16A6BC4040B7C2,
      $F7153C5390F198AC,
      $0000000000000000,
      $0000000000000000);

  WDL = 0;
  DTZ = 1;

  PIECE_ENC = 0;
  FILE_ENC = 1;

  FAIL = 0;                     // Probe failed (missing file table)
  OK = 1;                       // Probe successful
  CHANGE_STM = -1;              // DTZ should check the other side
  ZEROING_IS_BEST = 2;          // Best move zeroes DTZ (capture or pawn move)

  FD_ERR = INVALID_HANDLE_VALUE;

  OffDiag : array[0..63] of int8 =
    (0, -1, -1, -1, -1, -1, -1, -1,
     1,  0, -1, -1, -1, -1, -1, -1,
     1,  1,  0, -1, -1, -1, -1, -1,
     1,  1,  1,  0, -1, -1, -1, -1,
     1,  1,  1,  1,  0, -1, -1, -1,
     1,  1,  1,  1,  1,  0, -1, -1,
     1,  1,  1,  1,  1,  1,  0, -1,
     1,  1,  1,  1,  1,  1,  1,  0);

  Triangle : array[0..63] of byte =
    (6,  0,  1,  2,  2,  1,  0,  6,
     0,  7,  3,  4,  4,  3,  7,  0,
     1,  3,  8,  5,  5,  8,  3,  1,
     2,  4,  5,  9,  9,  5,  4,  2,
     2,  4,  5,  9,  9,  5,  4,  2,
     1,  3,  8,  5,  5,  8,  3,  1,
     0,  7,  3,  4,  4,  3,  7,  0,
     6,  0,  1,  2,  2,  1,  0,  6);

  FlipDiag : array[0..63] of byte =
    (0,  8, 16, 24, 32, 40, 48, 56,
     1,  9, 17, 25, 33, 41, 49, 57,
     2, 10, 18, 26, 34, 42, 50, 58,
     3, 11, 19, 27, 35, 43, 51, 59,
     4, 12, 20, 28, 36, 44, 52, 60,
     5, 13, 21, 29, 37, 45, 53, 61,
     6, 14, 22, 30, 38, 46, 54, 62,
     7, 15, 23, 31, 39, 47, 55, 63);

  Lower : array[0..63] of byte =
   (28,  0,  1,  2,  3,  4,  5,  6,
     0, 29,  7,  8,  9, 10, 11, 12,
     1,  7, 30, 13, 14, 15, 16, 17,
     2,  8, 13, 31, 18, 19, 20, 21,
     3,  9, 14, 18, 32, 22, 23, 24,
     4, 10, 15, 19, 22, 33, 25, 26,
     5, 11, 16, 20, 23, 25, 34, 27,
     6, 12, 17, 21, 24, 26, 27, 35);

  Diag : array[0..63] of byte =
    (0,  0,  0,  0,  0,  0,  0,  8,
     0,  1,  0,  0,  0,  0,  9,  0,
     0,  0,  2,  0,  0, 10,  0,  0,
     0,  0,  0,  3, 11,  0,  0,  0,
     0,  0,  0, 12,  4,  0,  0,  0,
     0,  0, 13,  0,  0,  5,  0,  0,
     0, 14,  0,  0,  0,  0,  6,  0,
    15,  0,  0,  0,  0,  0,  0,  7);

  Flap : array[0..1, 0..63] of byte =
   ((0,  0,  0,  0,  0,  0,  0,  0,
     0,  6, 12, 18, 18, 12,  6,  0,
     1,  7, 13, 19, 19, 13,  7,  1,
     2,  8, 14, 20, 20, 14,  8,  2,
     3,  9, 15, 21, 21, 15,  9,  3,
     4, 10, 16, 22, 22, 16, 10,  4,
     5, 11, 17, 23, 23, 17, 11,  5,
     0,  0,  0,  0,  0,  0,  0,  0),
    (0,  0,  0,  0,  0,  0,  0,  0,
     0,  1,  2,  3,  3,  2,  1,  0,
     4,  5,  6,  7,  7,  6,  5,  4,
     8,  9, 10, 11, 11, 10,  9,  8,
    12, 13, 14, 15, 15, 14, 13, 12,
    16, 17, 18, 19, 19, 18, 17, 16,
    20, 21, 22, 23, 23, 22, 21, 20,
     0,  0,  0,  0,  0,  0,  0,  0));

  PawnTwist : array[0..1, 0..63] of byte =
   ((0,  0,  0,  0,  0,  0,  0,  0,
    47, 35, 23, 11, 10, 22, 34, 46,
    45, 33, 21,  9,  8, 20, 32, 44,
    43, 31, 19,  7,  6, 18, 30, 42,
    41, 29, 17,  5,  4, 16, 28, 40,
    39, 27, 15,  3,  2, 14, 26, 38,
    37, 25, 13,  1,  0, 12, 24, 36,
     0,  0,  0,  0,  0,  0,  0,  0),
    (0,  0,  0,  0,  0,  0,  0,  0,
    47, 45, 43, 41, 40, 42, 44, 46,
    39, 37, 35, 33, 32, 34, 36, 38,
    31, 29, 27, 25, 24, 26, 28, 30,
    23, 21, 19, 17, 16, 18, 20, 22,
    15, 13, 11,  9,  8, 10, 12, 14,
     7,  5,  3,  1,  0,  2,  4,  6,
     0,  0,  0,  0,  0,  0,  0,  0));

  KKIdx : array[0..9, 0..63] of int16 =
  ((-1, -1, -1,  0,  1,  2,  3,  4,
    -1, -1, -1,  5,  6,  7,  8,  9,
    10, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33,
    34, 35, 36, 37, 38, 39, 40, 41,
    42, 43, 44, 45, 46, 47, 48, 49,
    50, 51, 52, 53, 54, 55, 56, 57),
   (58, -1, -1, -1, 59, 60, 61, 62,
    63, -1, -1, -1, 64, 65, 66, 67,
    68, 69, 70, 71, 72, 73, 74, 75,
    76, 77, 78, 79, 80, 81, 82, 83,
    84, 85, 86, 87, 88, 89, 90, 91,
    92, 93, 94, 95, 96, 97, 98, 99,
   100,101,102,103,104,105,106,107,
   108,109,110,111,112,113,114,115),
  (116,117, -1, -1, -1,118,119,120,
   121,122, -1, -1, -1,123,124,125,
   126,127,128,129,130,131,132,133,
   134,135,136,137,138,139,140,141,
   142,143,144,145,146,147,148,149,
   150,151,152,153,154,155,156,157,
   158,159,160,161,162,163,164,165,
   166,167,168,169,170,171,172,173),
  (174, -1, -1, -1,175,176,177,178,
   179, -1, -1, -1,180,181,182,183,
   184, -1, -1, -1,185,186,187,188,
   189,190,191,192,193,194,195,196,
   197,198,199,200,201,202,203,204,
   205,206,207,208,209,210,211,212,
   213,214,215,216,217,218,219,220,
   221,222,223,224,225,226,227,228),
  (229,230, -1, -1, -1,231,232,233,
   234,235, -1, -1, -1,236,237,238,
   239,240, -1, -1, -1,241,242,243,
   244,245,246,247,248,249,250,251,
   252,253,254,255,256,257,258,259,
   260,261,262,263,264,265,266,267,
   268,269,270,271,272,273,274,275,
   276,277,278,279,280,281,282,283),
  (284,285,286,287,288,289,290,291,
   292,293, -1, -1, -1,294,295,296,
   297,298, -1, -1, -1,299,300,301,
   302,303, -1, -1, -1,304,305,306,
   307,308,309,310,311,312,313,314,
   315,316,317,318,319,320,321,322,
   323,324,325,326,327,328,329,330,
   331,332,333,334,335,336,337,338),
   (-1, -1,339,340,341,342,343,344,
    -1, -1,345,346,347,348,349,350,
    -1, -1,441,351,352,353,354,355,
    -1, -1, -1,442,356,357,358,359,
    -1, -1, -1, -1,443,360,361,362,
    -1, -1, -1, -1, -1,444,363,364,
    -1, -1, -1, -1, -1, -1,445,365,
    -1, -1, -1, -1, -1, -1, -1,446),
   (-1, -1, -1,366,367,368,369,370,
    -1, -1, -1,371,372,373,374,375,
    -1, -1, -1,376,377,378,379,380,
    -1, -1, -1,447,381,382,383,384,
    -1, -1, -1, -1,448,385,386,387,
    -1, -1, -1, -1, -1,449,388,389,
    -1, -1, -1, -1, -1, -1,450,390,
    -1, -1, -1, -1, -1, -1, -1,451),
  (452,391,392,393,394,395,396,397,
    -1, -1, -1, -1,398,399,400,401,
    -1, -1, -1, -1,402,403,404,405,
    -1, -1, -1, -1,406,407,408,409,
    -1, -1, -1, -1,453,410,411,412,
    -1, -1, -1, -1, -1,454,413,414,
    -1, -1, -1, -1, -1, -1,455,415,
    -1, -1, -1, -1, -1, -1, -1,456),
  (457,416,417,418,419,420,421,422,
    -1,458,423,424,425,426,427,428,
    -1, -1, -1, -1, -1,429,430,431,
    -1, -1, -1, -1, -1,432,433,434,
    -1, -1, -1, -1, -1,435,436,437,
    -1, -1, -1, -1, -1,459,438,439,
    -1, -1, -1, -1, -1, -1,460,440,
    -1, -1, -1, -1, -1, -1, -1,461));


 DTM_Key : array[0..9] of UInt64 =         // for these cases DTZ = DTM

    ($6E4682F9525CC4C4,                    // KQ vs K      Mate ≤ 10
     $F7153C5390F198AC,                    // K vs KQ

     $A16591BE7916B4AC,                    // KR vs K      Mate ≤ 16
     $6B16A6BC4040B7C2,                    // K vs KR

     $191F49C07B4CBC02,                    // KBB vs K     Mate ≤ 19
     $5319DF65F37F5912,                    // K vs KBB

     $C4DC7ADDA8857861,                    // KBN vs K     Mate ≤ 33
     $C148CCD7A9605F5A,                    // K vs KBN

     $28E681F8409D4F20,                    // KNNN vs K    Mate <= 42
     $C733976E0EE21873);                   // K vs KNNN


type
  PCharArray = array of PAnsiChar;

type
  TPieceArray = array[0..15] of Integer;
  PPieceArray = ^TPieceArray;

type
  PWordArr  = ^TWordArray;
  TWordArr  = array[0..Int16.maxvalue] of PWord;

type
  PUInt64Array = ^TUInt64Array;
  TUInt64Array = array[-4096..4096] of UInt64;

type
  PByteArray  = ^TByteArray;
  TByteArray  = array[0..Int16.maxvalue] of Byte;

type
  PPairsData = ^TPairsData;
  TPairsData = record
    indexTable : PByte;                   // const uint8_t*
    sizeTable  : PWord;                   // const uint16_t*
    data       : PByte;                   // const uint8_t*
    offset     : PWord;                   // const uint16_t*
    symLen     : PByte;                   // uint8_t*
    symPat     : PByte;                   // const uint8_t*
    blockSize  : Byte;                    // uint8_t
    idxBits    : Byte;                    // uint8_t
    minLen     : Byte;                    // uint8_t
    constValue : array[0..1] of Byte;     // uint8_t[2]
    base       : PUInt64;                 // uint64_t base[]
  end;

type
  PEncInfo = ^TEncInfo;
  TEncInfo = record
    precomp : PPairsData;                            // struct PairsData*
    factor  : array[0..TB_PIECES-1] of NativeUInt;   // size_t factor[]
    pieces  : array[0..TB_PIECES-1] of Byte;         // uint8_t pieces[]
    norm    : array[0..TB_PIECES-1] of Byte;         // uint8_t norm[]
    end;

type
  PBaseEntry = ^TBaseEntry;
  TBaseEntry = record
    key         : UInt64;
    data        : array[0..1] of PByte;       // const uint8_t *data[3]
    mapping     : array[0..1] of THandle;     // map_t mapping[3]
    ready       : array[0..1] of Boolean;     // atomic_bool ready[3]
    num         : Byte;                       // uint8_t num
    symmetric   : Boolean;
    hasPawns    : Boolean;
    hasDtz      : Boolean;

    case Integer of
      0: (kk_enc : Boolean);                  // union { bool kk_enc; ... }
      1: (pawns  : array[0..1] of Byte);      // union { uint8_t pawns[2]; }
    end;

type
  PPieceEntry = ^TPieceEntry;
  TPieceEntry = record
    be        : TBaseEntry;
    ei        : array[0..4] of TEncInfo;     // EncInfo ei[5]
    dtzMap    : Pointer;                     // const void*
    dtzMapIdx : array[0..3] of Word;         // uint16_t[4]
    dtzFlags  : Byte;                        // uint8_t
    end;

type
  PPawnEntry = ^TPawnEntry;
  TPawnEntry = record
    be         : TBaseEntry;
    ei         : array[0..23] of TEncInfo;          // EncInfo ei[24]
    dtzMap     : Pointer;                           // const void*
    dtzMapIdx  : array[0..3, 0..3] of Word;         // uint16_t[4][4]
    dtzFlags   : array[0..3] of Byte;               // uint8_t[4]
    end;

type
  THashEntry = record
    key : Uint64;
    BaseEntry : pointer;
    end;

type
  PIntArr = ^TIntArr;
  TIntArr = array[0..TB_PIECES-1] of integer;

type
  PEncInfoArr = ^TEncInfoArr;
  TEncInfoArr = array[0..23] of TEncInfo;

type
  PDTZMapIdx = ^TDTZMapIdx;
  TDTZMapIdx = array[0..5, 0..3] of Word;

type
  TtempArr = array[0..2] of UInt64;

type
  PTB_Position = ^TTB_Position;
  TTB_Position = record
    occ: UInt64;               // uint64_t
    sq: array[0..7] of Byte;   // uint8_t[8]
    pt: array[0..7] of Byte;   // uint8_t[8]
    num: LongInt;              // int
    stm: LongInt;              // int
    idx: LongInt;              // int
    end;


function TB_MaterialKeyFromCounts(const Counts : TPieceArray): UInt64;
function TB_MaterialKeyFromCounts2(const Counts : TPieceArray): UInt64;
procedure InitIndices;
function ReadLE16(p: Pointer): UInt16;

function TB_init(const pathList: AnsiString) : boolean;
function TB_ProbeWDL(var Board : TBoard; var success: Boolean): Integer;
function TB_ProbeDTZ(var Board : TBoard; var success: Boolean): Integer;
procedure TB_Release;
procedure TB_Free;

procedure TB_TableCount(var WDLCount, DTZCount: integer);
function KeyFromBoard(Board : TBoard) : UInt64;

var
  TB_initialized: boolean = False;

  pathString : string;
  paths : array of Ansistring;

  numPaths: Integer = 0;

  TB_NumTables : array[0..1] of integer;
  TB_MaxCardinality : array[0..1] of integer;
  TB_ProbeCount : array[0..1] of UInt64;

  numPiece, numPawn: Integer;

  PieceEntry : array[0..TB_MAX_PIECE-1] of TPieceEntry;
  PawnEntry : array[0..TB_MAX_PAWN-1] of TPawnEntry;

  Binomial : array[0..6, 0..63] of UInt64;
  PawnIdx : array[0..1, 0..5, 0..23] of UInt64;
  PawnFactorFile : array[0..5, 0..3] of UInt64;

  tbHash : array[0..4095] of THashEntry;       // 2^12 - 1
  mutex : TCriticalSection;


implementation

{$CODEALIGN 16}

// code for dealing with files

function open_file(FileName: PAnsiChar): THandle;
  begin
  Result := CreateFileA(
    FileName,                 // LPCSTR lpFileName
    GENERIC_READ,             // DWORD dwDesiredAccess
    FILE_SHARE_READ,          // DWORD dwShareMode
    nil,                      // LPSECURITY_ATTRIBUTES lpSecurityAttributes
    OPEN_EXISTING,            // DWORD dwCreationDisposition
    FILE_FLAG_RANDOM_ACCESS,  // DWORD dwFlagsAndAttributes
    0 );                      // HANDLE hTemplateFile
  end;


procedure close_file(H: THandle);
  begin
  if H <> INVALID_HANDLE_VALUE then
    CloseHandle(H);
  end;


function file_size(H: THandle): UInt64;
  var
    SizeLow, SizeHigh: DWORD;

  begin
  SizeLow := GetFileSize(H, @SizeHigh);

  if (SizeLow = INVALID_FILE_SIZE) and (GetLastError <> NO_ERROR) then
    RaiseLastOSError;

  Result := (UInt64(SizeHigh) shl 32) or SizeLow;
  end;


function MapFile(fd: THandle; var mapping: THandle): Pointer;
  var
    sizeLow, sizeHigh: DWORD;
  begin
  Result  := nil;
  mapping := 0;

  sizeLow := GetFileSize(fd, @sizeHigh);

  mapping := CreateFileMapping(fd, nil, PAGE_READONLY, sizeHigh, sizeLow, nil);
  if mapping = 0 then
    Exit;

  Result := MapViewOfFile(mapping, FILE_MAP_READ, 0, 0, 0);

  if Result = nil then
    begin
    CloseHandle(mapping);
    mapping := 0;
    end;
  end;


procedure UnmapFile(data: Pointer; var mapping: THandle);
  begin
  if data <> nil then
    UnmapViewOfFile(data);

  if mapping <> 0 then
    begin
    CloseHandle(mapping);
    mapping := 0;
    end;
  end;


//  Functions for dealing with endianness

function IsLittleEndian: Boolean;
  var
    num: UInt32;
  begin
  num := 1;
  Result := PByte(@num)^ = 1;
  end;


function FromBE64(v: UInt64): UInt64;
  asm
  .NOFRAME
  BSWAP rcx
  MOV rax, rcx
  end;


function FromBE32(v: UInt32): UInt32;
  asm
  .NOFRAME
  BSWAP ecx
  MOV eax, ecx
  end;


function FromBE16(v: UInt16): UInt16;
  asm
  .NOFRAME
  movzx ecx, cx      // zero‑extend 16‑bit input
  rol   cx, 8        // rotate low 16 bits
  mov   ax, cx
  end;


function FromLE32(v: UInt32): UInt32;
  begin
  if IsLittleEndian then
    Result := v
   else
    Result := FromBE32(v);
  end;


function FromLE16(v: UInt16): UInt16;
  begin
  if IsLittleEndian then
    Result := v
   else
    Result := FromBE16(v);
  end;


function ReadLE32(p: Pointer): Cardinal;
  begin
  Move(p^, Result, SizeOf(Result));
  Result := FromLE32(Result);
  end;


function ReadLE16(p: Pointer): Word;
  begin
  Move(p^, Result, SizeOf(Result));
  Result := FromLE16(Result);
  end;


// TB initialization and probing code

function open_tb(nameStr, suffixStr: AnsiString): THandle;
  var
    filename : Ansistring;
    i: Integer;

  begin
  filename := '';

  for i := 0 to numPaths - 1 do
    begin
    filename := AnsiString(paths[i] + '\' + nameStr + suffixStr);
    Result := open_file(PAnsiChar(filename));

    if Result <> INVALID_HANDLE_VALUE then
      Exit;
    end;

  Result := INVALID_HANDLE_VALUE;
  end;


function test_tb(const NameStr, suffix: AnsiString): Boolean;
  var
    fd : THandle;
    size : UInt64;

  begin
  fd := open_tb(NameStr, suffix);
  if fd <> INVALID_HANDLE_VALUE then
    begin
    size := file_size(fd);
    close_file(fd);

    if (size and 63) <> 16 then
      fd := INVALID_HANDLE_VALUE;
    end;

  Result := fd <> INVALID_HANDLE_VALUE;
  end;


function MapTB(const nameStr, suffix: AnsiString; var mapping: THandle): Pointer;
  var
    fd: THandle;
    data: Pointer;
  begin
  Result  := nil;
  mapping := 0;

  fd := open_tb(nameStr, suffix);
  if fd = FD_ERR then
    Exit;

  data := MapFile(fd, mapping);
  close_file(fd);

  if data = nil then
    Exit;

  Result := data;
  end;


procedure AddToHash(ptr: PBaseEntry; key: UInt64);
  var
    idx: UInt64;

  begin
  idx := key shr (64 - TB_HASHBITS);

  while tbHash[idx].BaseEntry <> nil do
    idx := (idx + 1) and ((1 shl TB_HASHBITS) - 1);

  tbHash[idx].key := key;
  tbHash[idx].BaseEntry := ptr;
  end;


procedure Swap_byte(var a, b: byte);
  var
    tmp: byte;

  begin
  tmp := a;
  a := b;
  b := tmp;
  end;


procedure Swap_int(var a, b: integer);
  var
    tmp: integer;

  begin
  tmp := a;
  a := b;
  b := tmp;
  end;


procedure detect_tb(NameStr: AnsiString);
  var
    pcs: TPieceArray;
    i, j, k, l, m: Integer;
    s: PAnsiChar;
    colour: Integer;
    key, key2: UInt64;
    hasPawns: Boolean;
    be: PBaseEntry;

  begin
  if not test_tb(NameStr, suffix[WDL]) then
    Exit;

  // zero pcs[]
  for i := 0 to 15 do
    pcs[i] := 0;

  colour := 0;

  // parse material string
  s := PAnsiChar(NameStr);
  while s^ <> #0 do
    begin
    if s^ = AnsiChar('v') then
      colour := 8
    else
     for i := 1 to 6 do
       if s^ = PieceToAnsiChar[i] then
         begin
         Inc(pcs[i or colour]);
         Break;
         end;
    Inc(s);
    end;

  key  := TB_MaterialKeyFromCounts(pcs);     //  whitepcs, blackpcs
  key2 := TB_MaterialKeyFromCounts2(pcs);    //  blackpcs, whitepcs

  hasPawns := (pcs[1] <> 0) or (pcs[9] <> 0);

  if hasPawns then
    begin
    be := @PawnEntry[numPawn].be;
    Inc(numPawn);
    end
  else
    begin
    be := @pieceEntry[numPiece].be;
    Inc(numPiece);
    end;

  be^.hasPawns := hasPawns;
  be^.key := key;
  be^.symmetric := (key = key2);

  // count total pieces
  be^.num := 0;
  for i := 0 to 15 do
    Inc(be^.num, pcs[i]);

  // update table counts
  Inc(TB_NumTables[WDL]);

  be^.hasDtz := test_tb(NameStr, suffix[DTZ]);
  if be^.hasDtz then
    Inc(TB_NumTables[DTZ]);

  // update max cardinality
  if be^.num > TB_MaxCardinality[WDL] then
    TB_MaxCardinality[WDL] := be^.num;

  if be^.hasDtz and (be^.num > TB_MaxCardinality[DTZ]) then
    TB_MaxCardinality[DTZ] := be^.num;

  // initialize ready[] flags
  for i := 0 to 1 do
    be^.ready[i] := False;

  // handle pawn vs non-pawn encoding
  if not be^.hasPawns then
    begin
    j := 0;
    for i := 0 to 15 do
      if pcs[i] = 1 then
        Inc(j);
    be^.kk_enc := (j = 2);
    end
  else
    begin
    be^.pawns[0] := pcs[1];  // white pawns
    be^.pawns[1] := pcs[9];  // black pawns

    if (pcs[9] <> 0) and ((pcs[1] = 0) or (pcs[1] > pcs[9])) then
      Swap_byte(be^.pawns[0], be^.pawns[1]);
    end;

  AddToHash(be, key);
  if key <> key2 then
    AddToHash(be, key2);
  end;


function PCE_E(x: Pointer): PPieceEntry; inline;
  begin
  Result := PPieceEntry(x);
  end;


function PWN_E(x: Pointer): PPawnEntry; inline;
  begin
  Result := PPawnEntry(x);
  end;


function NumTables(be: PBaseEntry; t: Integer): Integer; inline;
  begin
  if be^.hasPawns then
    Result := 4
   else
    Result := 1;
  end;


function FirstEI(be: PBaseEntry; t: Integer): PEncInfo; inline;
  var
    index: Integer;
  begin

  if be^.hasPawns then
    begin
    // Pawn-entry encoding offsets
    if t = WDL then
      index := 0
     else
      index := 20;

    Result := @PWN_E(be)^.ei[index];
    end
  else
    begin
    // Piece-entry encoding offsets
    if t = WDL then
      index := 0
    else
      index := 4;

    Result := @PCE_E(be)^.ei[index];
    end;
  end;


procedure FreeTBEntry(be: PBaseEntry);
  var
    t, i, num: Integer;
    ei: PEncInfoArr;

  begin
  for t := 0 to 1 do              // removed DTM
    begin
    if be^.ready[t] then
      begin
      UnmapFile(be^.data[t], be^.mapping[t]);
      be^.data[t] := nil;

      num := NumTables(be, t);
      ei := PEncInfoArr(FirstEI(be, t));

      for i := 0 to num - 1 do
        begin
        FreeMem(ei[i].precomp);

        if t <> DTZ then
          FreeMem(ei[num + i].precomp);
        end;

      be^.ready[t] := False;
      end;
    end;
  end;


procedure TB_Release;
  var
    i: Integer;

  begin
  for i := 0 to numPiece - 1 do
    FreeTBEntry(@pieceEntry[i]);

  for i := 0 to numPawn - 1 do
    FreeTBEntry(@pawnEntry[i]);
  end;


procedure TB_TableCount(var WDLCount, DTZCount: integer);
  begin
  WDLCount := TB_NumTables[WDL];
  DTZCount := TB_NumTables[DTZ];
  end;


function TB_init(const pathList: AnsiString) : boolean;  // returns true if TB found
  var
    i, j, k, l, m: Integer;
    WDLCount, DTZCount: integer;

    NameStr : Ansistring;
    len: integer;
    tempList: TStringList;

  label
    finish;

  begin
  result := false;

  if not TB_initialized then
    begin
    InitIndices;
    TB_initialized := True;
    end;

  pathString := '';
  for i := low(paths) to high(paths) do
    paths[i] := '';

  TB_release;

  if assigned(mutex) then
    FreeAndNil(mutex);

  for i := 0 to 1 do
    begin
    TB_NumTables[i] := 0;
    TB_MaxCardinality[i] := 0;
    end;

  PathString := string(PathList);

  if length(PathString) = 0 then
    Exit;

  numPiece := 0;
  numPawn := 0;

  tempList := TStringList.Create;

    try
    tempList.Delimiter := ';';
    tempList.DelimitedText := PathString;
    numPaths := tempList.Count;

    SetLength(paths, numPaths);
    for i := 0 to numPaths - 1 do
      paths[i] := AnsiString(tempList[i]);

    finally
    tempList.Free;
    end;

  if assigned(mutex) = false then
    mutex := TCriticalSection.Create;

  for i := Low(tbHash) to High(tbHash) do
    FillChar(tbHash[i], SizeOf(THashEntry), 0);

  WDLCount := 0;
  DTZCount := 0;

  for i := 0 to 4 do
    begin
    NameStr := 'K' + PieceToAnsiChar[5-i] + 'vK';
    detect_tb(NameStr);
    end;

  for i := 0 to 4 do
    for j := i to 4 do
      begin
      NameStr := 'K' + PieceToAnsiChar[5-i] + 'vK' + PieceToAnsiChar[5-j];
      detect_tb(NameStr);
      end;

  for i := 0 to 4 do
    for j := i to 4 do
      begin
      NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + 'vK';
      detect_tb(NameStr);
      end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := 0 to 4 do
        begin
        NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + 'vK' + PieceToAnsiChar[5-k];
        detect_tb(NameStr);
        end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := j to 4 do
        begin
        NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + PieceToAnsiChar[5-k] + 'vK';
        detect_tb(NameStr);
        end;

  if SizeOf(NativeUInt) < 8 then    // 6- and 7-piece TBs make sense only with a 64-bit address space
    goto finish;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := i to 4 do
        begin
        for l := (if i = k then j else k) to 4 do
          begin
          NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + 'vK' + PieceToAnsiChar[5-k] + PieceToAnsiChar[5-l];
          detect_tb(NameStr);
          end;
        end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := j to 4 do
        for l := 0 to 4 do
          begin
          NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + PieceToAnsiChar[5-k] + 'vK' + PieceToAnsiChar[5-l];
          detect_tb(NameStr);
          end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := j to 4 do
        for l := k to 4 do
          begin
          NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + PieceToAnsiChar[5-k] + PieceToAnsiChar[5-l] + 'vK';
          detect_tb(NameStr);
          end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := j to 4 do
        for l := k to 4 do
          for m := l to 4 do
            begin
            NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + PieceToAnsiChar[5-k] + PieceToAnsiChar[5-l] + PieceToAnsiChar[5-m] + 'vK';
            detect_tb(NameStr);
            end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := j to 4 do
        for l := k to 4 do
          for m := 0 to 4 do
            begin
            NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + PieceToAnsiChar[5-k] + PieceToAnsiChar[5-l] + 'vK' + PieceToAnsiChar[5-m];
            detect_tb(NameStr);
            end;

  for i := 0 to 4 do
    for j := i to 4 do
      for k := j to 4 do
        for l := 0 to 4 do
          for m := l to 4 do
            begin
            NameStr := 'K' + PieceToAnsiChar[5-i] + PieceToAnsiChar[5-j] + PieceToAnsiChar[5-k] + 'vK' + PieceToAnsiChar[5-l] + PieceToAnsiChar[5-m];
            detect_tb(NameStr);
            end;

  finish:

  TB_TableCount(WDLCount, DTZCount);

  if WDLCount > 0 then
    result := true
   else
    pathString := '';      // no tablebase files found
  end;


procedure TB_Free;
  begin
  TB_release;

  if assigned(mutex) then
    FreeAndNil(mutex);
  end;

 // Disable overflow checking and range checking for the following code
 // addition of the, matkey can result in overflow,
 // UInt64 addition overflow results in a wrapping of the value

{$IFOPT R+}
{$DEFINE HasRangeChecks}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE HasOverflowChecks}
{$ENDIF}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

function TB_MaterialKeyFromCounts(const Counts : TPieceArray): UInt64;
  var
    i: Integer;

  begin
  Result := 0;

  for i := PAWN to QUEEN do
    Inc(Result, UInt64(Counts[i]) * matKey[i] + UInt64(Counts[i+8]) * matKey[i + 8]);
  end;

function TB_MaterialKeyFromCounts2(const Counts : TPieceArray): UInt64;
  var
    i: Integer;

  begin
  Result := 0;

  for i := PAWN to QUEEN do
    Inc(Result, UInt64(Counts[i+8]) * matKey[i] + UInt64(Counts[i]) * matKey[i + 8]);
  end;

{$IFDEF HasRangeChecks}
{$RANGECHECKS ON}
{$ENDIF}
{$IFDEF HasOverflowChecks}
{$OVERFLOWCHECKS ON}
{$ENDIF}


procedure InitIndices;
  var
    i, j: Integer;
    s: UInt64;

  begin
  // Binomial[k][n] = C(n, k)
  for j := 0 to 63 do
    Binomial[0][j] := 1;

  for i := 1 to 6 do
    for j := 1 to 63 do
      Binomial[i][j] := Binomial[i - 1][j - 1] + Binomial[i][j - 1];

  // PawnIdx[0], PawnFactorFile
  for i := 0 to 5 do
    begin
    s := 0;
    for j := 0 to 23 do
      begin
      PawnIdx[0][i][j] := s;

      s := s + Binomial[i][ PawnTwist[0][ (1 + (j mod 6)) * 8 + (j div 6) ] ];

      if ((j + 1) mod 6 = 0) then
        begin
        PawnFactorFile[i][j div 6] := s;
        s := 0;
        end;
      end;
    end;
  end;


function LeadingPawn(var p: TIntArr; be: PBaseEntry; enc: integer): integer;
  var
    i: integer;

  begin
  // Find the pawn with the "leading" Flap value and move it to p[0]
  for i := 1 to be^.pawns[0] - 1 do
    if Flap[enc - 1][p[0]] > Flap[enc - 1][p[i]] then
      Swap_int(p[0], p[i]);

  // Return file or rank depending on encoding
  if enc = FILE_ENC then
    Result := FileToFile[p[0] and 7]
  else
    Result := (p[0] - 8) shr 3;
  end;


function Encode(p: TIntArr; ei: PEncInfo; be: PBaseEntry; enc: Integer): UInt64;
  var
    n, i, j, k, t, sq, skips, s1, s2: Integer;
    idx, s: UInt64;

  begin
  n := be^.num;

  // Mirror horizontally if needed
  if (p[0] and $04) <> 0 then
    for i := 0 to n - 1 do
      p[i] := p[i] xor $07;

  if enc = PIECE_ENC then
    begin
    // Mirror vertically if needed
    if (p[0] and $20) <> 0 then
      for i := 0 to n - 1 do
        p[i] := p[i] xor $38;

    // Diagonal flip if needed
    for i := 0 to n - 1 do
      begin
      if OffDiag[p[i]] <> 0 then
        begin
        if (OffDiag[p[i]] > 0) and (i < If (be^.kk_enc = true) then 2 else 3) then
          for j := 0 to n - 1 do
            p[j] := FlipDiag[p[j]];
        Break;
        end;
      end;

    // KK or KXY encoding
    if be^.kk_enc = true then
      begin
      idx := KKIdx[Triangle[p[0]]][p[1]];
      k := 2;
      end
    else
      begin
      s1 := Ord(p[1] > p[0]);
      s2 := Ord(p[2] > p[0]) + Ord(p[2] > p[1]);

      if OffDiag[p[0]] <> 0 then
        idx := UInt64(Triangle[p[0]]) * (63 * 62)
             + UInt64(p[1] - s1) * 62
             + UInt64(p[2] - s2)
      else if OffDiag[p[1]] <> 0 then
        idx := UInt64(6 * 63 * 62)
             + UInt64(Diag[p[0]]) * (28 * 62)
             + UInt64(Lower[p[1]]) * 62
             + UInt64(p[2] - s2)
      else if OffDiag[p[2]] <> 0 then
        idx := UInt64(6 * 63 * 62)
             + UInt64(4 * 28 * 62)
             + UInt64(Diag[p[0]]) * (7 * 28)
             + UInt64(Diag[p[1]] - s1) * 28
             + UInt64(Lower[p[2]])
      else
        idx := UInt64(6 * 63 * 62)
             + UInt64(4 * 28 * 62)
             + UInt64(4 * 7 * 28)
             + UInt64(Diag[p[0]]) * (7 * 6)
             + UInt64(Diag[p[1]] - s1) * 6
             + UInt64(Diag[p[2]] - s2);

      k := 3;
      end;

    idx := idx * ei^.factor[0];
    end
   else
    begin
    // Pawn encoding (same color)
    for i := 1 to be^.pawns[0] - 1 do
      for j := i + 1 to be^.pawns[0] - 1 do
        if PawnTwist[enc - 1][p[i]] < PawnTwist[enc - 1][p[j]] then
          Swap_int(p[i], p[j]);

    k := be^.pawns[0];
    idx := PawnIdx[enc - 1][k - 1][Flap[enc - 1][p[0]]];

    for i := 1 to k - 1 do
      idx := idx + Binomial[k - i][PawnTwist[enc - 1][p[i]]];

    idx := idx * ei^.factor[0];

    // Pawns of the other color
    if be^.pawns[1] <> 0 then
      begin
      t := k + be^.pawns[1];

      for i := k to t - 1 do
        for j := i + 1 to t - 1 do
          if p[i] > p[j] then
            Swap_int(p[i], p[j]);

      s := 0;
      for i := k to t - 1 do
        begin
        sq := p[i];
        skips := 0;

        for j := 0 to k - 1 do
          if sq > p[j] then
            Inc(skips);

        s := s + Binomial[i - k + 1][sq - skips - 8];
        end;

      idx := idx + s * ei^.factor[k];
      k := t;
      end;
    end;

  // Remaining pieces (non‑pawns)
  while k < n do
    begin
    t := k + ei^.norm[k];

    for i := k to t - 1 do
      for j := i + 1 to t - 1 do
        if p[i] > p[j] then
          Swap_int(p[i], p[j]);

    s := 0;
    for i := k to t - 1 do
      begin
      sq := p[i];
      skips := 0;

      for j := 0 to k - 1 do
        if sq > p[j] then
          Inc(skips);

      s := s + Binomial[i - k + 1][sq - skips];
      end;

    idx := idx + s * ei^.factor[k];
    k := t;
    end;

  Result := idx;
  end;


function EncodePiece(var p: TIntArr; ei: PEncInfo; be: PBaseEntry): UInt64;
  begin
  Result := Encode(p, ei, be, PIECE_ENC);
  end;


function EncodePawnF(var p: TIntArr; ei: PEncInfo; be: PBaseEntry): UInt64;
  begin
  Result := Encode(p, ei, be, FILE_ENC);
  end;


function SubFactor(k, n: UInt64): UInt64;
  var
    i: UInt64;
    f: UInt64;

  begin
  f := n;

  for i := 1 to k - 1 do
    f := (f * (n - i)) div (i + 1);

  Result := f;
  end;


function InitEncInfo(ei: PEncInfo; be: PBaseEntry; tb: PByte; shift, t, enc: Integer): UInt64;
  var
    morePawns: Boolean;
    i, j, k, order, order2, n: Integer;
    f: UInt64;

  begin
  morePawns := (enc <> PIECE_ENC) and (be^.pawns[1] > 0);

  // Decode piece types
  for i := 0 to be^.num - 1 do
    begin
    ei^.pieces[i] := (tb[i + 1 + Ord(morePawns)] shr shift) and $0F;
    ei^.norm[i] := 0;
    end;

  order  := (tb[0] shr shift) and $0F;
  order2 := if morePawns then ((tb[1] shr shift) and $0F) else $0F;

  // Initial group size
  if enc <> PIECE_ENC then
    k := be^.pawns[0]
   else if be^.kk_enc = true then
    k := 2
   else
    k := 3;

  ei^.norm[0] := k;

  // Opponent pawns
  if morePawns then
    begin
    ei^.norm[k] := be^.pawns[1];
    Inc(k, ei^.norm[k]);
    end;

  // Group remaining pieces by identical type
  i := k;
  while i < be^.num do
    begin
    j := i;
    while (j < be^.num) and (ei^.pieces[j] = ei^.pieces[i]) do
      begin
      Inc(ei^.norm[i]);
      Inc(j);
      end;
    Inc(i, ei^.norm[i]);
    end;

  // Compute factors
  n := 64 - k;
  f := 1;

  i := 0;
  while (k < be^.num) or (i = order) or (i = order2) do
    begin
    if i = order then
      begin
      ei^.factor[0] := f;

      if enc = FILE_ENC then
        f := f * PawnFactorFile[ei^.norm[0] - 1][t]
       else if be^.kk_enc = true then
        f := f * 462
       else
        f := f * 31332;
      end
     else if i = order2 then
      begin
      ei^.factor[ei^.norm[0]] := f;
      f := f * SubFactor(ei^.norm[ei^.norm[0]], 48 - ei^.norm[0]);
      end
     else
      begin
      ei^.factor[k] := f;
      f := f * SubFactor(ei^.norm[k], n);
      n := n - ei^.norm[k];
      k := k + ei^.norm[k];
      end;

    Inc(i);
    end;

  Result := f;
  end;

procedure CalcSymLen(d: PPairsData; s: UInt32; tmp: PByte);
  var
    w: PByte;
    s1, s2: UInt32;

  begin
  // w points to the 3-byte pattern for symbol s
  w := @d^.symPat[s * 3];

  // Extract s2 = (w[2] << 4) | (w[1] >> 4)
  s2 := (UInt32(w[2]) shl 4) or (UInt32(w[1]) shr 4);

  if s2 = $0FFF then
    begin
    d^.symLen[s] := 0;
    end
  else
    begin
    // Extract s1 = ((w[1] & 0xF) << 8) | w[0]
    s1 := ((UInt32(w[1]) and $0F) shl 8) or UInt32(w[0]);

    if tmp[s1] = 0 then
      CalcSymLen(d, s1, tmp);

    if tmp[s2] = 0 then
      CalcSymLen(d, s2, tmp);

    d^.symLen[s] := d^.symLen[s1] + d^.symLen[s2] + 1;
    end;

  tmp[s] := 1;
  end;


function SetupPairs(var ptr: PByte; tb_size: UInt64; var size: TTempArr; flags: PByte; ptype: Integer): PPairsData;
  var
    d: PPairsData;
    data: PByte;
    blockSize, idxBits: Byte;
    realNumBlocks, numBlocks: UInt32;
    maxLen, minLen, h: Integer;
    numSyms: UInt32;
    numIndices: UInt64;
    tmp: array[0..4095] of Byte;
    s: UInt32;
    i: Integer;

    Ptr_1, Ptr_2 : PUInt64;
    tempPtr : PByte;

  begin
  data := ptr;
  flags^ := data[0];

  // Case: constant-value table
  if (data[0] and $80) <> 0 then
    begin
    d := AllocMem(SizeOf(TPairsData));
    d^.idxBits := 0;
    d^.constValue[0] := If ptype = WDL then data[1] else 0;
    d^.constValue[1] := 0;

    ptr := data + 2;
    size[0] := 0;
    size[1] := 0;
    size[2] := 0;

    Result := d;
    Exit;
    end;

  // Normal table
  blockSize := data[1];
  idxBits   := data[2];
  realNumBlocks := ReadLE32(data + 4);
  numBlocks := realNumBlocks + data[3];

  maxLen := data[8];
  minLen := data[9];
  h := maxLen - minLen + 1;

  numSyms := ReadLE16(data + 10 + 2 * h);

  // Allocate PairsData + base[] + symLen[]
  d := AllocMem(SizeOf(TPairsData) + h * SizeOf(UInt64) + numSyms);

  d^.blockSize := blockSize;
  d^.idxBits   := idxBits;
  d^.offset := PWord(data + 10);
  d^.symLen := PByte(NativeUInt(d) + SizeOf(TPairsData) + h * SizeOf(UInt64));
  d^.symPat := data + 12 + 2 * h;
  d^.minLen := minLen;

  d^.base := Pointer(d);
  inc(d^.base, SizeOf(TPairsData) div 8);

  ptr := data + 12 + 2 * h + 3 * numSyms + (numSyms and 1);

  // Compute sizes
  numIndices := (tb_size + (UInt64(1) shl idxBits) - 1) shr idxBits;

  size[0] := 6 * numIndices;
  size[1] := 2 * numBlocks;
  size[2] := UInt64(realNumBlocks) shl blockSize;

  // Compute symbol lengths
  FillChar(tmp, numSyms, 0);

  for s := 0 to numSyms - 1 do
    if tmp[s] = 0 then
      CalcSymLen(d, s, @tmp[0]);

  // Compute base[]

  Ptr_1 := d^.base;
  inc(Ptr_1, h - 1);
  Ptr_1^ := 0;

  Ptr_2 := Ptr_1;
  for i := h - 2 downto 0 do
    begin
    dec(Ptr_1);

    Ptr_1^ :=
      ( Ptr_2^
        + UInt64(ReadLE16(PByte(NativeUInt(d^.offset) + i * SizeOf(Word))))
        - UInt64(ReadLE16(PByte(NativeUInt(d^.offset) + (i + 1) * SizeOf(Word))))
      ) div 2;

    Ptr_2 := Ptr_1;
    end;

  Ptr_1 := d^.base;

  for i := 0 to h - 1 do
    begin
    Ptr_1^ := Ptr_1^ shl (64 - (minLen + i));
    inc(Ptr_1);
    end;

  // Adjust offset pointer
  d^.offset := PWord(NativeUInt(d^.offset) - UInt64(d^.minLen) * SizeOf(Word));

  Result := d;
  end;


function InitTable(be: PBaseEntry; const Namestr: AnsiString; atype: Integer): Boolean;
  var
    data, mapPtr: PByte;
    split: Boolean;
    tb_sizeArr: array[0..5, 0..1] of UInt64;
    sizeArr: array[0..5, 0..1] of TTempArr;

    num, t, i: Integer;
    ei: PEncInfoArr;
    enc: Integer;
    flagsByte: Byte;
    map16: PWord;
    mapIdxDTZ: PDTZMapIdx;

    flagsDTZ: PByte;
    count: TPieceArray;

  begin
  data := MapTb(Namestr, suffix[atype], be^.mapping[atype]);
  if data = nil then
    begin
    Result := False;
    Exit;
    end;

  if ReadLE32(data) <> magic[atype] then
    begin
    // fprintf(stderr, "Corrupted table.\n");

    unmapfile(data, be^.mapping[atype]);
    be^.data[atype] := nil;
    Result := False;
    Exit;
    end;

  be^.data[atype] := data;

  split := (atype <> DTZ) and ((data[4] and $01) <> 0);

  Inc(data, 5);

  num := NumTables(be, atype);
  ei := PEncInfoArr(FirstEI(be, atype));

  if not be^.hasPawns then
    enc := PIECE_ENC
   else
    enc := FILE_ENC;

  // Encoding info
  for t := 0 to num - 1 do
    begin
    tb_sizeArr[t, 0] := InitEncInfo(@ei[t], be, data, 0, t, enc);

    if split then
      tb_sizeArr[t, 1] := InitEncInfo(@ei[num + t], be, data, 4, t, enc);

    Inc(data, be^.num + 1 + Ord(be^.hasPawns and (be^.pawns[1] <> 0)));
    end;

  Inc(data, NativeUInt(data) and 1);      // aligns to even number

  // PairsData setup
  for t := 0 to num - 1 do
    begin
    flagsByte := 0;
    ei[t].precomp := SetupPairs(data, tb_sizeArr[t, 0], sizeArr[t, 0], @flagsByte, atype);
    if atype = DTZ then
      begin
      if not be^.hasPawns then
        PCE_E(be)^.dtzFlags := flagsByte
      else
        PWN_E(be)^.dtzFlags[t] := flagsByte;
      end;

    if split then
      ei[num + t].precomp := SetupPairs(data, tb_sizeArr[t, 1], sizeArr[t, 1], @flagsByte, atype)
    else if atype <> DTZ then
      ei[num + t].precomp := nil;
    end;

  // DTZ map
  if atype = DTZ then
    begin
    mapPtr := data;
    if be^.hasPawns then
      begin
      PWN_E(be)^.dtzMap := mapPtr;
      mapIdxDTZ := @PWN_E(be)^.dtzMapIdx[0];
      flagsDTZ := @PWN_E(be)^.dtzFlags[0];
      end
     else
      begin
      PCE_E(be)^.dtzMap := mapPtr;
      mapIdxDTZ := @PCE_E(be)^.dtzMapIdx;
      flagsDTZ := @PCE_E(be)^.dtzFlags;
      end;

    for t := 0 to num - 1 do
      begin
      if (flagsDTZ[t] and 2) <> 0 then
        begin
        if (flagsDTZ[t] and 16) = 0 then
          begin
          for i := 0 to 3 do
            begin
            mapIdxDTZ^[t][i] := data + 1 - mapPtr;
            Inc(data, 1 + data[0]);
            end;
          end
         else
          begin
          Inc(data, NativeUInt(data) and 1);
          for i := 0 to 3 do
            begin
            mapIdxDTZ^[t][i] := (NativeUInt(PWord(data)) + 2 - NativeUInt(mapPtr)) div 2;
            Inc(data, 2 + 2 * ReadLE16(data));
            end;
          end;
        end;
      end;

    Inc(data, NativeUInt(data) and 1);
    end;

  // indexTable
  for t := 0 to num - 1 do
    begin
    ei[t].precomp^.indexTable := data;
    Inc(data, sizeArr[t, 0, 0]);
    if split then
      begin
      ei[num + t].precomp^.indexTable := data;
      Inc(data, sizeArr[t, 1, 0]);
      end;
    end;

  // sizeTable
  for t := 0 to num - 1 do
    begin
    ei[t].precomp^.sizeTable := PWord(data);
    Inc(data, sizeArr[t, 0, 1]);
    if split then
      begin
      ei[num + t].precomp^.sizeTable := PWord(data);
      Inc(data, sizeArr[t, 1, 1]);
      end;
    end;

  // data blocks (aligned to 64 bytes)
  for t := 0 to num - 1 do
    begin
    data := PByte((NativeUInt(data) + $3F) and not NativeUInt($3F));    //   aligns to 64 bytes
    ei[t].precomp^.data := data;
    Inc(data, sizeArr[t, 0, 2]);
    if split then
      begin
      data := PByte((NativeUInt(data) + $3F) and not NativeUInt($3F));   //   aligns to 64 bytes
      ei[num + t].precomp^.data := data;
      Inc(data, sizeArr[t, 1, 2]);
      end;
    end;

  Result := True;
  end;


function DecompressPairs(d: PPairsData; idx: UInt64): PByte;
  var
    mainIdx: UInt32;
    litIdx: Integer;
    block: UInt32;
    idxOffset: UInt16;
    ptr: PUInt32;
    m, l: Integer;
    offset_Ptr: Pword;
    symLen_Ptr: PByte;
    sym, bitCnt: UInt32;
    code: UInt64;
    w: UInt32;
    s1: Integer;
    symPat_Ptr: PByte;

    Base_Ptr : PUInt64;
    Block_Ptr : PWord;

  begin
  // Constant-value table
  if d^.idxBits = 0 then
    begin
    Result := @d^.constValue[0];
    Exit;
    end;

  // Main index and literal index
  mainIdx := idx shr d^.idxBits;
  litIdx  := Integer(idx and ((UInt64(1) shl d^.idxBits) - 1))
             - Integer(UInt64(1) shl (d^.idxBits - 1));

  // Read block index
  Move((d^.indexTable + 6 * mainIdx)^, block, SizeOf(block));
  block := FromLE32(block);

  // Read literal offset
  Move((d^.indexTable + 6 * mainIdx + 4)^, idxOffset, SizeOf(idxOffset));
  litIdx := litIdx + FromLE16(idxOffset);


  // Adjust block based on literal index

  Block_Ptr := d^.sizeTable;
  inc(Block_Ptr, block);
  if litIdx < 0 then
    begin
    while litIdx < 0 do
      begin
      Dec(block);
      Dec(Block_Ptr);

      litIdx := litIdx + Block_Ptr^ + 1;
      end;
    end
  else
    begin
    while litIdx > Block_Ptr^ do
      begin
      litIdx := litIdx - (Block_Ptr^ + 1);

      Inc(block);
      inc(Block_Ptr);
      end;
    end;

  // Pointer to compressed block
  ptr := PUInt32(d^.data + (UInt64(block) shl d^.blockSize));
  m := d^.minLen;
  symLen_Ptr := d^.symLen;

  // Load first 64 bits (big-endian)
  code := FromBE64(PUInt64(ptr)^);
  Inc(ptr, 2);

  bitCnt := 0;

  // Decode symbol
  while True do
    begin
    Offset_Ptr := d^.offset;
    Base_Ptr := d^.base;

    l := m;

    while code < Base_Ptr^ do
      begin
      Inc(Base_Ptr);
      inc(l);
      end;

    inc(Offset_Ptr, l);
    sym := FromLE16(Offset_Ptr^);
    sym := sym + UInt32((code - Base_Ptr^) shr (64 - l));

    if litIdx < symLen_Ptr[sym] + 1 then
      Break;

    litIdx := litIdx - (symLen_Ptr[sym] + 1);
    code := code shl l;
    bitCnt := bitCnt + l;

    if bitCnt >= 32 then
      begin
      bitCnt := bitCnt - 32;
      var tmp := FromBE32(ptr^);
      Inc(ptr);
      code := code or (UInt64(tmp) shl bitCnt);
      end;
    end;

  // Walk the symbol tree
  symPat_Ptr := d^.symPat;

  while symLen_Ptr[sym] <> 0 do
    begin
    w := ReadLE32(symPat_Ptr + 3 * sym);
    s1 := w and $0FFF;

    if litIdx < symLen_Ptr[s1] + 1 then
      sym := s1
    else
      begin
      litIdx := litIdx - (symLen_Ptr[s1] + 1);
      sym := (w shr 12) and $0FFF;
      end;
    end;

  Result := symPat_Ptr + 3 * sym;
  end;


function SyzygyPositionFromBoard(const Board:TBoard; Position : PTB_Position) : boolean;
  var
    cell : integer;
    i, cursor : integer;
    sourcePegs : UInt64;
    TempBoard : TBoard;

  begin
  TempBoard := CopyBoard(Board);

  TempBoard.WhitePegs := Flip(TempBoard.WhitePegs);
  TempBoard.BlackPegs := Flip(TempBoard.BlackPegs);
  TempBoard.Kings := Flip(TempBoard.Kings);
  TempBoard.Queens := Flip(TempBoard.Queens);
  TempBoard.Rooks := Flip(TempBoard.Rooks);
  TempBoard.Bishops := Flip(TempBoard.Bishops);
  TempBoard.Knights := Flip(TempBoard.Knights);
  TempBoard.Pawns := Flip(TempBoard.Pawns);

  Position.occ := (TempBoard.WhitePegs or TempBoard.BlackPegs);
  Position.num := bitcount(Position.occ);

  if Position.num > 7 then
    exit(false);

  for i := low(Position.pt) to high(Position.pt) do
    Position.pt[i] := 0;
  for i := low(Position.sq) to high(Position.sq) do
    Position.sq[i] := 0;

  Position.pt[0] := King;               // white king  =  6
  Position.pt[1] := King + 8;           // black king  = 14

  cell := getlowBit(TempBoard.Kings and TempBoard.WhitePegs);
  Position.sq[0] := cell;

  cell := getlowBit(TempBoard.Kings and TempBoard.BlackPegs);
  Position.sq[1] := cell;

  cursor := 2;

  sourcePegs := TempBoard.WhitePegs and TempBoard.Queens;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Queen;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.BlackPegs and TempBoard.Queens;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Queen + 8;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.WhitePegs and TempBoard.Rooks;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Rook;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.BlackPegs and TempBoard.Rooks;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Rook + 8;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.WhitePegs and TempBoard.Bishops;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Bishop;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.BlackPegs and TempBoard.Bishops;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Bishop + 8;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.WhitePegs and TempBoard.Knights;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Knight;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.BlackPegs and TempBoard.Knights;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Knight + 8;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.WhitePegs and TempBoard.Pawns;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Pawn;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  sourcePegs := TempBoard.BlackPegs and TempBoard.Pawns;
  while SourcePegs <> 0 do
    begin
    Cell := PopLowBit(SourcePegs);
    Position.pt[cursor] := Pawn + 8;
    Position.sq[cursor] := Cell;
    inc(cursor);
    end;

  Position.stm := TempBoard.ToPlay;
  Position.idx := 0;

  result := true;
  end;


function TB_BareKings(position : PTB_Position): Boolean;
  begin
  Result := position^.num = 2;
  end;


{$IFOPT R+}
{$DEFINE HasRangeChecks}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE HasOverflowChecks}
{$ENDIF}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

function TB_MaterialKey(position: PTB_Position): UInt64;
  var
    i: Integer;

  begin
  Result := 0;

  // Skip the two kings (indices 0 and 1)
  for i := 2 to position^.num - 1 do
    Result := (Result + matKey[position^.pt[i]]);
  end;

{$IFDEF HasRangeChecks}
{$RANGECHECKS ON}
{$ENDIF}
{$IFDEF HasOverflowChecks}
{$OVERFLOWCHECKS ON}
{$ENDIF}


function KeyFromBoard(Board : TBoard) : UInt64;
  var
    position : TTB_Position;

  begin
  result := 0;
  if SyzygyPositionFromBoard(Board, @Position) = true then
    result := TB_MaterialKey(@position);
  end;


procedure TB_MaterialString(pos: PTB_Position; var str: AnsiString);
  var
    cnt : array[0..15] of Integer;
    i : Integer;

  begin
  // Zero counts
  for i := 0 to 15 do
    cnt[i] := 0;

  // Count all pieces
  for i := 0 to pos^.num - 1 do
    Inc(cnt[pos^.pt[i]]);

  str := '';

  // White side: KING down to PAWN
  for i := KING downto PAWN do
    while cnt[i] > 0 do
      begin
      str := str + PieceToAnsiChar[i];
      Dec(cnt[i]);
      end;

  // Separator
  str :=str + AnsiChar('v');

  // Black side: KING down to PAWN
  for i := KING downto PAWN do
    while cnt[8 + i] > 0 do
      begin
      str := str + PieceToAnsiChar[i];
      Dec(cnt[8 + i]);
      end;
  end;


function TB_WhiteToMove(pos: PTB_Position): Boolean;
  begin
  Result := pos^.stm = WHITE;
  end;


procedure TB_ListSquares(pos: PTB_Position; pt : array of byte; flip: Boolean; var p: TIntArr);
  var
    i, j, t: Integer;
    mask: Integer;

  begin
  i := 0;
  mask := If flip then $38 else 0;

  while i < pos^.num do
    begin
    t := pt[i] xor (Ord(flip) shl 3);

    // find the piece with matching type
    for j := 0 to pos^.num - 1 do
      if pos^.pt[j] = t then
        begin
        p[i] := pos^.sq[j] xor mask;
        Inc(i);
        end;
    end;
  end;


function ProbeTable(position : PTB_Position; const s : Integer; var resultCode : Integer; atype : Integer) : Integer;
  var
    key      : UInt64;
    hashIdx  : Integer;
    be       : PBaseEntry;
    bside    : Boolean;
    flip     : Boolean;
    ei       : PEncInfo;
    p        : TIntArr;
    idx      : UInt64;
    t        : Integer;
    flags    : Byte;
    w        : PByte;
    v        : Integer;
    m        : Integer;

    str, str2: AnsiString;
    vpos, len : integer;

  begin
  // KvK special case
  if (atype = WDL) and TB_BareKings(position) then
    begin
    Result := 0;
    Exit;
    end;

  // Material key
  key := TB_MaterialKey(position);

  // Hash lookup
  hashIdx := key shr (64 - TB_HASHBITS);
  while (tbHash[hashIdx].key <> 0) and (tbHash[hashIdx].key <> key) do
    hashIdx := (hashIdx + 1) and ((1 shl TB_HASHBITS) - 1);

  if tbHash[hashIdx].BaseEntry = nil then
    begin
    resultCode := FAIL;
    Result := 0;
    Exit;
    end;

  be := tbHash[hashIdx].BaseEntry;

  if (atype = DTZ) and not be^.hasDtz then
    begin
    resultCode := FAIL;
    Result := 0;
    Exit;
    end;

  // Double-checked init
  if not be^.ready[atype] then
    begin
    mutex.acquire;
      try
      if not be^.ready[atype] then
        begin
        TB_MaterialString(position, str);

        if be^.key <> key then
          begin
          VPos := pos('v', Str);
          len := length(Str);

          str2 :=  RightStr(Str,  len - Vpos) + 'v' + LeftStr(Str,  Vpos-1);
          end;

        if not InitTable(be, If be^.key = key then str else str2, atype) then
          begin
          tbHash[hashIdx].BaseEntry := nil;
          resultCode := FAIL;
          Result := 0;
          Exit;
          end;

        TInterlocked.Exchange(be^.ready[atype], True);
        end;
      finally
      mutex.release;
      end;
    end;

  // Side / flip logic
  if not be^.symmetric then
    begin
    flip  := key <> be^.key;
    bside := (TB_WhiteToMove(position) = flip);
    end
   else
    begin
    flip  := not TB_WhiteToMove(position);
    bside := False;
    end;

  ei := FirstEI(be, atype);

  if not be^.hasPawns then
    begin
    if atype = DTZ then
      begin
      flags := PCE_E(be)^.dtzFlags;
      if ((flags and 1) <> Ord(bside)) and not be^.symmetric then
        begin
        resultCode := CHANGE_STM;
        Result := 0;
        Exit;
        end;
      end;

    if atype <> DTZ then
      Inc(ei, Ord(bside));

    TB_ListSquares(position, ei^.pieces, flip, p);
    idx := EncodePiece(p, ei, be);
    end
   else
    begin
    TB_ListSquares(position, ei^.pieces, flip, p);
    t := LeadingPawn(p, be, FILE_ENC);

    if atype = DTZ then
      begin
      flags := PWN_E(be)^.dtzFlags[t];
      if ((flags and 1) <> Ord(bside)) and not be^.symmetric then
        begin
        resultCode := CHANGE_STM;
        Result := 0;
        Exit;
        end;
      end;

    if atype = WDL then
      Inc(ei, t + 4 * Ord(bside))
     else
      Inc(ei, t);

    TB_ListSquares(position, ei^.pieces, flip, p);
    LeadingPawn(p, be, FILE_ENC);

    idx := EncodePawnF(p, ei, be)
    end;

  Inc(TB_ProbeCount[atype]);

  w := DecompressPairs(ei^.precomp, idx);

  if atype = WDL then
    begin
    Result := Integer(w[0]) - 2;
    Exit;
    end;

  v := Integer(w[0]) + ((Integer(w[1]) and $0F) shl 8);

  if (flags and 2) <> 0 then
    begin
    m := WdlToMap[s + 2];
    if (flags and 16) = 0 then
      begin
      if be^.hasPawns then
        v := PByteArray(PWN_E(be)^.dtzMap)^[PWN_E(be)^.dtzMapIdx[t][m] + v]
       else
        v := PByteArray(PCE_E(be)^.dtzMap)^[PCE_E(be)^.dtzMapIdx[m] + v];
      end
     else
      begin
      if be^.hasPawns then
        v := FromLE16(PWordArr(PWN_E(be)^.dtzMap)^[PWN_E(be)^.dtzMapIdx[t][m] + v])
       else
        v := FromLE16(PWordArr(PCE_E(be)^.dtzMap)^[PCE_E(be)^.dtzMapIdx[m] + v]);
      end;

    end;

  if ((flags and PAFlags[s + 2]) = 0) or ((s and 1) <> 0) then
    v := v * 2;

  Result := v;
  end;


function ProbeWDLTable(position : PTB_Position; var resultCode : Integer) : Integer;
  begin
  Result := ProbeTable(position, 0, resultCode, WDL);
  end;


function ProbeDTZTable(position : PTB_Position; wdl : Integer; var resultCode : Integer) : Integer;
  begin
  Result := ProbeTable(position, wdl, resultCode, DTZ);
  end;


function ProbeAB(var Board : TBoard; alpha, beta : Integer; var resultCode : Integer) : Integer;
  var
    movecount, k, v: Integer;
    Moves : TMoveArray;
    Move : TMove;
    Position : TTB_Position;

  begin
  v := alpha;

  // probe_ab() is never called for positions with en passant captures.

  if Board.EnPassant <> 0 then
    begin
    resultCode := FAIL;
    Exit(0);
    end;

  // Generate all captures
  movecount := Board.GetValidMoves(Board.ToPlay, Moves, ViolentOnly);  // includes some quiet moves

  for k := 1 to movecount do
    begin
    Move := Moves[k];

    if move.CapturedPiece = 0 then
      continue;

    Board.MakeMove(Move);

    v := -ProbeAB(Board, -beta, -alpha, resultCode);

    Board.UndoMove(Move);

    if resultCode = FAIL then
      Exit(0);

    if v > alpha then
      alpha := v;

    if alpha >= beta then
      Exit(alpha);
    end;

  // No capture improved alpha — fall back to WDL probe

  if SyzygyPositionFromBoard(Board, @Position) = true then
    v := ProbeWDLTable(@Position, resultCode);

  if v > alpha then
    alpha := v;

  Result := alpha;
  end;


function ProbeWDL(var Board : TBoard; var resultCode : Integer) : Integer;
  var
    movecount, k, v: Integer;
    bestCap, bestEp: Integer;
    legalCaps: Boolean;
    Moves : TMoveArray;
    Move : TMove;
    Position : TTB_Position;

  label
    no_stalemate;

  begin
  resultCode := OK;

  movecount := Board.GetValidMoves(Board.ToPlay, Moves, ViolentOnly);

  bestCap := -3;
  bestEp  := -3;

  for k := 1 to movecount do
    begin
    Move := Moves[k];

    if move.CapturedPiece = 0 then
      continue;

    Board.MakeMove(Move);

    v := -ProbeAB(Board, -2, -bestCap, resultCode);

    Board.UndoMove(Move);

    if resultCode = FAIL then
      Exit(0);

    if v > bestCap then
      begin
      if v = 2 then
        begin
        resultCode := ZEROING_IS_BEST;
        Exit(2);
        end;

      if not (Move.EnpassantCell <> 0) then
        bestCap := v
      else
        if v > bestEp then
          bestEp := v;
      end;
    end;

  // No winning capture → probe table

  if not SyzygyPositionFromBoard(Board, @Position) then
    begin
    resultCode := FAIL;
    Exit(0);
    end;

  v := ProbeWDLTable(@Position, resultCode);

  if resultCode = FAIL then
    Exit(0);

  // Combine capture and table results
  legalCaps := bestCap > -3;

  if bestEp > bestCap then
    begin
    if bestEp > v then
      begin
      resultCode := ZEROING_IS_BEST;
      Exit(bestEp);
      end;
    bestCap := bestEp;
    end;

  // Now max(v, bestCap) is correct unless stalemate case applies
  if bestCap >= v then
    begin
    if bestCap > 0 then
      resultCode := ZEROING_IS_BEST;
    Exit(bestCap);
    end;

  // Handle stalemate case
  if (bestEp > -3) and (v = 0) and (not legalCaps) then
    begin
    // No legal non-ep captures; check if position without ep rights is stalemate

    if Board.KingInCheck(Board.ToPlay) then
      goto no_stalemate;

    if movecount > 0 then
      goto no_stalemate;

    // Stalemate detected → ep capture is best
    resultCode := ZEROING_IS_BEST;
    Exit(bestEp);
    end;

  no_stalemate:
    Result := v;
  end;

  // Probe the WDL table for a particular position.

  // The return value is from the point of view of the side to move:
  // -2 : loss
  // -1 : loss, but draw under 50-move rule
  //  0 : draw
  //  1 : win, but draw under 50-move rule
  //  2 : win

function TB_ProbeWDL(var Board : TBoard; var success: Boolean): Integer;
  var
    resultCode: Integer;

  begin
  Result := ProbeWDL(Board, resultCode);
  success := resultCode <> FAIL;
  end;

  // Probe the DTZ table for a particular position.
  // If *success == true, the probe was successful.
  // The return value is from the point of view of the side to move:
  //         n < -100 : loss, but draw under 50-move rule
  // -100 <= n < -1   : loss in n ply (assuming 50-move counter == 0)
  //         0        : draw
  //     1 < n <= 100 : win in n ply (assuming 50-move counter == 0)
  //   100 < n        : win, but draw under 50-move rule
  //
  // If the position mate, -1 is returned instead of 0.
  //
  // The return value n can be off by 1: a return value -n can mean a loss
  // in n+1 ply and a return value +n can mean a win in n+1 ply. This
  // cannot happen for tables with positions exactly on the "edge" of
  // the 50-move rule.
  //
  // This means that if dtz > 0 is returned, the position is certainly
  // a win if dtz + 50-move-counter <= 99. Care must be taken that the engine
  // picks moves that preserve dtz + 50-move-counter <= 99.
  //
  // If n = 100 immediately after a capture or pawn move, then the position
  // is also certainly a win, and during the whole phase until the next
  // capture or pawn move, the inequality to be preserved is
  // dtz + 50-movecounter <= 100.
  //
  // In short, if a move is available resulting in dtz + 50-move-counter <= 99,
  // then do not accept moves leading to dtz + 50-move-counter == 100.

function TB_ProbeDTZ(var Board : TBoard; var success : Boolean) : Integer;
  var
    resultCode: Integer;
    wdl, movecount, k, v, dtz, best: Integer;
    Moves : TMoveArray;
    Move : TMove;
    Position : TTB_Position;
    MoveExists, InCheck : boolean;

  label
    failure;

  begin
  resultCode := OK;
  success := True;

  // Probe WDL first
  wdl := ProbeWDL(Board, resultCode);
  if resultCode = FAIL then
    goto failure;

  // Draw → dtz = 0
  if wdl = 0 then
    Exit(0);

  // Zeroing move (winning capture or EP capture)
  if resultCode = ZEROING_IS_BEST then
    Exit(WdlToDtz[wdl + 2]);

  movecount := Board.GetValidMoves(Board.ToPlay, Moves, All);

  // If winning, check for winning pawn move
  if wdl > 0 then
    begin

    for k := 1 to movecount do
      begin
       Move := Moves[k];

      if Move.Piece <> pawn then
        continue;

      if Move.CapturedPiece <> 0 then
        continue;

      Board.MakeMove(Move);

      v := -ProbeWDL(Board, resultCode);

      Board.UndoMove(Move);

      if resultCode = FAIL then
        goto failure;

      if v = wdl then
        Exit(WdlToDtz[wdl + 2]);
      end;
    end;

  // Safe to probe DTZ table

  if not SyzygyPositionFromBoard(Board, @Position) then
    begin
    resultCode := FAIL;
    goto failure;
    end;

  dtz := ProbeDTZTable(@position, wdl, resultCode);

  if resultCode = FAIL then
    goto failure;

  if resultCode <> CHANGE_STM then
    Exit(WdlToDtz[wdl + 2] + if wdl > 0 then dtz else -dtz);

  // CHANGE_STM: need to probe DTZ for the other side
  best := High(Integer);

  if wdl < 0 then
    best := WdlToDtz[wdl + 2];

  for k := 1 to movecount do
    begin
    Move := Moves[k];

    if Move.Piece = pawn then
      continue;

    if Move.CapturedPiece <> 0 then
      continue;

    Board.MakeMove(Move);

    v := -TB_ProbeDTZ(Board, success);

    MoveExists := Board.MoveExists(InCheck);

    if (v = 1) and InCheck and MoveExists then
      best := 1
     else if wdl > 0 then
      begin
      if (v > 0) and (v + 1 < best) then
        best := v + 1;
      end
     else
      begin
      if (v - 1 < best) then
        best := v - 1;
      end;

    Board.UndoMove(Move);

    if not success then
      Break;
    end;

  if best <> High(Integer) then
    Exit(best);

  failure:
  success := False;
  Result := 0;
  end;


end.
