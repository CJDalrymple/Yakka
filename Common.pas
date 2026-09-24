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


unit Common;

interface

uses
  System.SysUtils, Winapi.Windows, System.Classes, System.SyncObjs;

  {$IF defined(MSWINDOWS)}
  function GenRandom(pbBuffer :PBYTE; dwLen: DWORD):BOOL; stdcall;
  {$ENDIF}


type
  TAnonThread = class(TThread)
  protected
    class var
      fCountdown: TCountdownEvent;
    class constructor ClassCreate;
    class destructor ClassDestroy;
  public
    class procedure Shutdown; static;
    class function WaitForAll(Timeout: Cardinal = INFINITE): TWaitResult; static;
  protected
    fProc: TProc;
    procedure Execute; override;
  public
    constructor Create(const aProc: TProc);
  end;


type
  TPRNG = record
    private
      PRNG_lo : UInt64;   //  PRNG state
      PRNG_hi : UInt64;   //  PRNG state

    public
      class operator Initialize(out Dest: TPRNG);
      function Rand64 : UInt64;
      function Random(const ARange: Integer): Integer;
      procedure Randomize;
      procedure Seed(x1, x2 : UInt64);
    end;


type
  T_Timer = record
    private
      StartTime : TDateTime;
      StopTime : TDateTime;

      Running : boolean;

      StartTickCount : Int64;
      StopTickCount : Int64;
      TotalTicks : Int64;
      ElapsedHours, ElapsedMinutes, ElapsedSeconds : Int64;
      Frequency : Int64;

    public

      class operator Initialize(out Dest: T_Timer);
      procedure Start;
      procedure Stop;
      function StartStr : string;
      function StopStr : string;
      function DurationStr : string;
      function ElapsedMilliseconds : Int64;
    end;


procedure Flipbit(var x : UInt64; Index : UInt64);
procedure ClearBit(var x : UInt64; Index : UInt64);
procedure SetBit(var x : UInt64; Index : UInt64);
function GetBit(const x : UInt64; Index : UInt64) : boolean;

function GetLowBit(const x : UInt64) : UInt64;
function GetHighBit(const x : UInt64) : UInt64;
function PopLowBit(var x : UInt64) : UInt64;

function BitCount(const x : UInt64) : Int64;

function PEXT(const source, mask : UInt64) : UInt64;
function PDEP(const source, Mask : UInt64) : UInt64;

function Flip(Pegs : UInt64) : UInt64;


implementation

{$CODEALIGN 16}

{$IF defined(MSWINDOWS)}
function GenRandom; external ADVAPI32 name 'SystemFunction036';
{$ENDIF}


class constructor TAnonThread.ClassCreate;
  begin
  fCountdown := TCountdownEvent.Create(1);
  end;

class destructor TAnonThread.ClassDestroy;
  begin
  fCountdown.Free;
  end;

class procedure TAnonThread.Shutdown;
  begin
  fCountdown.Signal;
  end;

class function TAnonThread.WaitForAll(Timeout: Cardinal): TWaitResult;
  begin
  Result := fCountdown.WaitFor(Timeout);
  end;

constructor TAnonThread.Create(const aProc: TProc);
  begin
  inherited Create(True);
  fProc := aProc;
  FreeOnTerminate := True;
  end;

procedure TAnonThread.Execute;
  begin
  if fCountdown.TryAddCount then
    try
      fProc;
    finally
      fCountdown.Signal;
    end;
  end;


// TPRNG

class operator TPRNG.Initialize(out Dest: TPRNG);
  begin
  Dest.Randomize;
  end;


function TPRNG.Rand64 : UInt64;
  asm
  .ALIGN 16

  mov rax, Self.PRNG_lo
  mov r10, $a4c1f32680f70c55
  mov r11, $2d99787926d46932
  mul r10
  mov r9, rax
  mov r8, rdx
  mov rax, Self.PRNG_lo
  mul r11
  add r8, rax

  mov rax, Self.PRNG_hi
  mul r10
  add rax, r8
  add r9, r10
  adc rax, r11
  mov Self.PRNG_lo, r9
  mov Self.PRNG_hi, rax
  end;


function TPRNG.Random(const ARange: Integer): Integer;
  var
    temp : UInt64;

  begin
  Temp := Rand64 and $FFFFFFFF;
  Result := (UInt64(UInt32(ARange)) * Temp) shr 32;
  end;


procedure TPRNG.Randomize;

{$IF defined(MSWINDOWS)}
  begin
  GenRandom(@PRNG_lo, 8);
  GenRandom(@PRNG_hi, 8);
  end;
{$ENDIF}
{$IF defined(LINUX) or defined(ANDROID) or defined(MACOS)}
  begin
  System.Randomize;
  PRNG_lo := UInt64(System.Random(MaxInt)) shl 32 + UInt64(System.Random(MaxInt));
  PRNG_hi := UInt64(System.Random(MaxInt)) shl 32 + UInt64(System.Random(MaxInt));
  end;
{$ENDIF}


procedure TPRNG.Seed(x1, x2 : UInt64);
  begin
  PRNG_lo := UInt64(x1);
  PRNG_hi := UInt64(x2);
  end;


// T_Timer

class operator T_Timer.Initialize(out Dest: T_Timer);
  begin
  QueryPerformanceFrequency(Dest.Frequency);
  end;


procedure T_Timer.Start;
  begin
  StartTime := now;
  QueryPerformanceCounter(StartTickCount);
  QueryPerformanceFrequency(Frequency);
  running := true;
  end;


procedure T_Timer.Stop;
  begin
  StopTime := now;
  QueryPerformanceCounter(StopTickCount);
  running := false;
  end;


function T_Timer.StartStr : string;
  begin
  result := DateToStr(StartTime) + '  ' + TimeToStr(StartTime);
  end;


function T_Timer.StopStr : string;
  begin
  result := DateToStr(StopTime) + '  ' + TimeToStr(StopTime);
  end;


function T_Timer.DurationStr : string;
  begin
  if running = true then
    QueryPerformanceCounter(StopTickCount);

  TotalTicks := StopTickCount - StartTickCount;
  ElapsedHours := TotalTicks div (Frequency * 60 * 60);
  ElapsedMinutes := TotalTicks div (Frequency * 60) - ElapsedHours * 60;
  ElapsedSeconds := TotalTicks div Frequency - ElapsedHours * 60 * 60 - ElapsedMinutes * 60;

  result := IntToStr(ElapsedHours) + ':' + FormatFloat('00', ElapsedMinutes * 1.0)  + ':' + FormatFloat('00', ElapsedSeconds * 1.0) + ' [h:m:s]'
  end;


function T_Timer.ElapsedMilliseconds : Int64;
  begin
  if running = true then
    QueryPerformanceCounter(StopTickCount);

  TotalTicks := StopTickCount - StartTickCount;

  result := (TotalTicks * 1000) div Frequency;
  end;


// Utility Bit functions

// index : [0..63]

procedure Flipbit(var x : UInt64; Index : UInt64);
  asm
  .ALIGN 16

  MOV rax, [rcx]
  BTC rax, rdx
  MOV [rcx], rax
  end;


// index : [0..63]

procedure ClearBit(var x : UInt64; Index : UInt64);
  asm
  .ALIGN 16

  MOV rax, [rcx]
  BTR rax, rdx
  MOV [rcx], rax
  end;


// index : [0..63]

procedure SetBit(var x : UInt64; Index : UInt64);
  asm
  .ALIGN 16

  MOV rax, [rcx]
  BTS rax, rdx
  MOV [rcx], rax
  end;


// index : [0..63]

function GetBit(const x : UInt64; Index : UInt64) : boolean;
  asm
  .ALIGN 16

  BT rcx, rdx
  SETC al
  end;


procedure SetAllBits(var x : UInt64);
  asm
  .ALIGN 16

  MOV [rcx], -1
  end;


function GetLowBit(const x : UInt64) : UInt64;
  // returns the index of the lowest significant set bit
  // at least one bit must be set

  asm
  .ALIGN 16

  BSF rax, rcx
  end;


function GetHighBit(const x : UInt64) : UInt64;
  // returns the index of the highest significant set bit
  // at least one bit must be set

  asm
  .ALIGN 16

  BSR rax, rcx
  end;


function PopLowBit(var x : UInt64) : UInt64;
  // clears the lowest significant set bit and returns it's index
  // at least one bit must be set

  asm
  .ALIGN 16

  BLSI rdx, [rcx]
  XOR [rcx], rdx          // clears bit in x
  BSF rax, rdx            // gets index of lowest bit
  end;


function BitCount(const x : UInt64) : Int64;
  asm
  .ALIGN 16

  POPCNT  rax, rcx
  end;


function PEXT(const source, mask : UInt64) : UInt64;
  asm
  .ALIGN 16

  PEXT rax, rcx, rdx
  end;


function PDEP(const source, Mask : UInt64) : UInt64;
  asm
  .ALIGN 16

  PDEP rax, rcx, rdx
  end;


function Flip(Pegs : UInt64) : UInt64;
  asm
  .ALIGN 16

  BSWAP rcx
  MOV rax, rcx
  end;


end.
