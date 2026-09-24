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
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.


unit CPU_Info;

interface

function PopCnt_Supported : boolean;
function BMI2_Supported : boolean;
function SSE_Supported : boolean;
function SSE2_Supported : boolean;
function AVX_Supported : boolean;
function AVX2_Supported : boolean;
function AVX512f_Supported : boolean;
function AVX512fp16_Supported : boolean;
function Detect_x86_64_level : integer;

implementation

{$CODEALIGN 16}

// popcnt instruction  used in unit : Common

function PopCnt_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  SHR ECX, 23     // bit 23 of ECX
  AND RCX, $1
  MOV RAX, RCX

  POP RBX
  end;

// BMI2 required for PEXT, PDEP instructions used in units : GameDef, Common

function BMI2_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $7
  XOR ECX, ECX
  CPUID
  SHR EBX, 8     // bit 8 of EBX
  AND RBX, $1
  MOV RAX, RBX

  POP RBX
  end;


function SSE_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  SHR EDX, 25     // bit 25 of EDX
  AND RDX, $1
  MOV RAX, RDX

  POP RBX
  end;


// SEE2 required for CVTSD2SI instruction used in unit : Net
// Convert Scalar Double-Precision FP Value to Integer

function SSE2_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  SHR EDX, 26     // bit 26 of EDX
  AND RDX, $1
  MOV RAX, RDX

  POP RBX
  end;


// FMA required for vfmadd231ps instruction used in unit : NNUE

function FMA_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  SHR ECX, 12     // bit 12 of ECX
  AND RCX, $1
  MOV RAX, RCX

  POP RBX
  end;


// AVX required for vmovups, vmaxps, vextractf128 ....  instructions used in unit : NNUE

function AVX_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  SHR ECX, 28    // bit 28 of ECX
  AND RCX, $1
  MOV RAX, RCX

  POP RBX
  end;


function AVX2_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $7
  XOR ECX, ECX
  CPUID
  SHR EBX, 5       // bit 5 of EBX
  AND RBX, $1
  MOV RAX, RBX

  POP RBX
  end;


function AVX512f_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $7
  XOR ECX, ECX
  CPUID
  SHR EBX, 16      // bit 16 of EBX
  AND RBX, $1
  MOV RAX, RBX

  POP RBX
  end;


function AVX512fp16_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $7
  XOR ECX, ECX
  CPUID
  SHR EDX, 23      // bit 23 of EDX
  AND RDX, $1
  MOV RAX, RBX

  POP RBX
  end;


function Detect_x86_64_level : integer;
  // Returns:  1, 2, 3, or 4

  asm
  push rbx
  push rsi
  push rdi

  // default: x86-64-v1

  mov     r11d, 1

  // -------------------------------------------------
  //  CPUID leaf 1: basic features
  // -------------------------------------------------

  mov     eax, 1
  cpuid               // EAX, EBX, ECX, EDX updated
  mov     r8d, ecx    // save ECX(1)
  mov     r9d, edx    // save EDX(1)


  // ------------- check x86-64-v2  -------------------------
  //
  // need: SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, CX16, LAHF_LM
  // --------------------------------------------------------


  bt      r8d, 0                // SSE3 (ECX bit 0)
  jnc     @finish

  bt      r8d, 9               // SSSE3 (ECX bit 9)
  jnc     @finish

  bt      r8d, 19              // SSE4.1 (ECX bit 19)
  jnc     @finish

  bt      r8d, 20              // SSE4.2 (ECX bit 20)
  jnc     @finish

  bt      r8d, 23              // POPCNT (ECX bit 23)
  jnc     @finish

  bt      r8d, 13              // CX16 (ECX bit 13)
  jnc     @finish

  // LAHF_LM from CPUID.(EAX=80000001).ECX bit 0
  mov     eax, $80000000
  cpuid
  cmp     eax, $80000001
  jb      @finish              // no extended leaf -> not v2

  mov     eax, $80000001
  cpuid                        // ECX, EDX updated
  bt      ecx, 0               // LAHF_LM
  jnc     @finish

  mov     r11d, 2              // all v2 features present


  // --------------- check x86-64-v3----------------------
  //
  // need: AVX, AVX2, BMI1, BMI2, F16C, FMA, LZCNT, MOVBE
  // -----------------------------------------------------

  // we still have r8d = ECX(1), r9d = EDX(1) from leaf 1
  // but we overwrote ECX with 0x80000001 above, so reload leaf 1

  mov     eax, 1
  cpuid
  mov     r8d, ecx             // ECX(1)
  mov     r9d, edx             // EDX(1)

  bt      r8d, 27              // OSXSAVE (ECX bit 27)
  jnc     @finish              // stay at v2

  xor     ecx, ecx
  xgetbv
  and     eax, 6
  cmp     eax, 6
  jne     @finish              // OS does not support AVX

  mov     eax, 7
  xor     ecx, ecx
  cpuid
  mov     r10d, ebx

  bt      r8d, 28              // AVX (ECX bit 28)
  jnc     @finish              // stay at v2

  bt      r8d, 12              // FMA (ECX bit 12)
  jnc     @finish

  bt      r8d, 29              // F16C (ECX bit 29)
  jnc     @finish

  bt      r8d, 22              // MOVBE (ECX bit 22)
  jnc     @finish

  mov     eax, $80000001       // LZCNT from CPUID.(EAX=80000001).ECX bit 5
  cpuid
  bt      ecx, 5
  jnc     @finish

  mov     eax, 7               // AVX2, BMI1, BMI2 from CPUID.(EAX=7, ECX=0)
  xor     ecx, ecx
  cpuid                        // EBX, ECX, EDX updated
  mov     r10d, ebx            // EBX(7,0)

  bt      r10d, 3              // BMI1 (EBX bit 3)
  jnc     @finish

  bt      r10d, 5              // AVX2 (EBX bit 5)
  jnc     @finish

  bt      r10d, 8              // BMI2 (EBX bit 8)
  jnc     @finish

  mov     r11d, 3              // all v3 features present


  // ----------------  check x86-64-v4----------------------
  //
  // need: AVX512F, AVX512DQ, AVX512BW, AVX512VL, AVX512CD
  // -------------------------------------------------------

  // from CPUID.(EAX=7, ECX=0)
  // we already have EBX, EDX from leaf 7 above
  // EBX in r10d, EDX in edx

  bt      r10d, 16             // AVX512F (EBX bit 16)
  jnc     @finish              // stay at v3

  bt      r10d, 17             // AVX512DQ (EBX bit 17)
  jnc     @finish

  bt      r10d, 30             // AVX512BW (EBX bit 30)
  jnc     @finish

  bt      r10d, 31             // AVX512VL (EBX bit 31)
  jnc     @finish

  bt      edx, 28              // AVX512CD (EDX bit 28)
  jnc     @finish

  mov     r11d, 4              // all v4 features present

  @finish:

  mov     eax, r11d

  pop rdi
  pop rsi
  pop rbx

  end;


end.
