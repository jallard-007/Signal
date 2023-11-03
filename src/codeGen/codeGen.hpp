#pragma once

#include <map>
#include <array>
#include <cstdint>
#include "../nodes.hpp"
#include "../tokenizer/tokenizer.hpp"
#include "../bytecodeDesign.hpp"

struct RegisterInfo {
  uint32_t stackOffset{0};
  bool inUse {false};
  bool changed {false};
};

struct ExpressionResult {
  bool isReg {false};
  uint64_t val{0};
  ExpressionResult() = default;
  ExpressionResult(bool, uint64_t);
};

struct CodeGen {
  std::map<std::string, uint32_t> variableNameToRegister;
  std::vector<unsigned char> byteCode;
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  Tokenizer *tk{nullptr};
  Program &program;
  std::vector<Tokenizer> &tokenizers;

  CodeGen(Program&, std::vector<Tokenizer>&);

  // expression gen
  ExpressionResult generateExpression(const Expression &);
  ExpressionResult generateExpressionArrAccess(const ArrayAccess &);
  ExpressionResult generateExpressionArrOrStructLit(const ArrayOrStructLiteral &);
  ExpressionResult generateExpressionBinOp(const BinOp &);
  ExpressionResult generateExpressionFunctionCall(const FunctionCall &);
  ExpressionResult generateExpressionUnOp(const UnOp &);
  ExpressionResult loadValue(const Token &);
  
  void addByte(OpCodes);
  void addByte(unsigned char);
  void addBytes(const std::vector<unsigned char>&);
  void alignForImm(uint32_t);
  void moveImmToReg(uint8_t, uint64_t);
  ExpressionResult mathematicalBinOp(const BinOp&, OpCodes, OpCodes);
  int getStackOffset(const std::string &);
  unsigned char allocateRegister();
  void freeRegister(unsigned char);
};

/*

*/