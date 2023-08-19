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

*/