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

implementation


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



end.
