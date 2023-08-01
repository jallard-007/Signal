#pragma once

#include <map>
#include "../nodes.hpp"
#include "../tokenizer/tokenizer.hpp"

struct CodeGen {
  Program &program;
  std::vector<Tokenizer> &tokenizers;
  std::map<std::string, int> g;
  Tokenizer *tk;
  CodeGen(Program&, std::vector<Tokenizer>&);

  // expression gen
  void generateExpression(const Expression &);
  void generateExpressionArrAccess(const ArrayAccess &);
  void generateExpressionArrOrStructLit(const ArrayOrStructLiteral &);
  void generateExpressionBinOp(const BinOp &);
  void generateExpressionFunctionCall(const FunctionCall &);
  void generateExpressionUnOp(const UnOp &);
  int generateExpressionValue(const Token &);

  int getStackOffset(const std::string &);
  int allocateRegister();
  void getRegister();
  void freeRegister(int);
};

/*

registers:

layout of general purpose registers
--------
    ----
      --
       -
8 byte registers:
rax, rcx, rdx, rbx, rsi, rdi, rsp, rbp, r8, r9, r10, r11, r12, r13, r14, r15
4 bytes registers:
eax, ecx, edx, ebx, esi, edi, esp, edp, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d
2 byte registers:
ax, cx, dx, bx, si, di, sp, dp, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w
1 byte registers:
al, cl, dl, bl, sil, dil, spl, dpl, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b


expression

binary op:
suffixes for operand size:
q: quad word. 8 bytes
l: double word. 4 bytes
w: word. 2 bytes
b: 1 byte

  evaluate left, eval right


linux x86_64 calling convention:
return value goes in rax
arguments that can fit in a register go in rdi, rsi, rdx, rcx, r8, r9, in that order
larger types go on the stack.

lets keep it simple to start. max 6 parameters, and only built in ones (no structs)

*/