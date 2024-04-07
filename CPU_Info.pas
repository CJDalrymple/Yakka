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


implementation


// popcnt instruction  used in unit : Common

function PopCnt_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  AND ECX, $800000             // bit 23 of ECX
  MOV RAX, RCX
  SHR RAX, $17
  POP RBX
  end;

// BMI2 required for PEXT, PDEP instructions used in units : GameDef, Common

function BMI2_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $7
  XOR ECX, ECX
  CPUID
  AND EBX, $100               // bit 8 of EBX
  MOV RAX, RBX
  SHR RAX, $8
  POP RBX
  end;


function SSE_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  AND EDX, $2000000          // bit 25 of EDX
  MOV RAX, RCX
  SHR RAX, $19
  POP RBX
  end;


function SSE2_Supported : boolean;
  asm
  PUSH RBX
  MOV EAX, $1
  CPUID
  AND EDX, $4000000          // bit 26 of EDX
  MOV RAX, RDX
  SHR RAX, $1A
  POP RBX
  end;

end.
