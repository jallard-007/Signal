#pragma once

#include <map>
#include <array>
#include <cstdint>
#include "../nodes.hpp"
#include "../tokenizer/tokenizer.hpp"
#include "../bytecodeDesign.hpp"

struct RegisterInfo {
  uint32_t stackOffset;
  bool inUse {false};
  bool changed {false};
};

struct CodeGen {
  std::map<std::string, uint32_t> variableNameToRegister;
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  Program &program;
  std::vector<Tokenizer> &tokenizers;
  Tokenizer *tk;

  CodeGen(Program&, std::vector<Tokenizer>&);

  // expression gen
  uint32_t generateExpression(const Expression &);
  uint32_t generateExpressionArrAccess(const ArrayAccess &);
  uint32_t generateExpressionArrOrStructLit(const ArrayOrStructLiteral &);
  uint32_t generateExpressionBinOp(const BinOp &);
  uint32_t generateExpressionFunctionCall(const FunctionCall &);
  uint32_t generateExpressionUnOp(const UnOp &);
  uint64_t loadValue(const Token &);

  int getStackOffset(const std::string &);
  uint32_t allocateRegister();
  void freeRegister(uint32_t);
};

/*

*/