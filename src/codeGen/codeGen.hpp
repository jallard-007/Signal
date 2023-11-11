#pragma once

#include <map>
#include <array>
#include <cstdint>
#include "../nodes.hpp"
#include "../tokenizer/tokenizer.hpp"
#include "../bytecodeDesign.hpp"
#include "../checker/checker.hpp"

struct RegisterInfo {
  uint32_t stackOffset{0};
  bool inUse {false};
  bool changed {false};
};

struct ExpressionResult {
  uint64_t val{0};
  OpCodes jumpOp {OpCodes::NOP};
  bool isReg {false};
  bool isTemp {false};
  bool isStruct {false};
  ExpressionResult() = default;
  ExpressionResult(bool, bool, uint64_t);
};

struct StructInformation {
  int32_t size = -1;
  StructInformation() = default;
};

enum class JumpMarkerType: uint8_t {
  NONE,
  START_IF,
  IF_STATEMENT,
  END_IF,
  LOOP,
  SWITCH,
  BREAK,
  CONTINUE,
};

struct JumpMarker {
  uint64_t index;
  JumpMarkerType type;
  JumpMarker() = delete;
  JumpMarker(uint64_t, JumpMarkerType);
  bool operator==(const JumpMarker) const;
};

struct CodeGen {
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  std::map<std::string, StructInformation> structNameToInfoMap;
  std::vector<unsigned char> byteCode;
  std::vector<JumpMarker> jumpMarkers;
  Tokenizer *tk{nullptr};
  Program &program;
  Checker &checker;
  std::vector<Tokenizer> &tokenizers;

  CodeGen(Program&, std::vector<Tokenizer>&, Checker&);

  // expression gen
  ExpressionResult generateExpression(const Expression&, bool = false);
  ExpressionResult generateExpressionArrAccess(const ArrayAccess&);
  ExpressionResult generateExpressionArrOrStructLit(const ArrayOrStructLiteral&);
  ExpressionResult generateExpressionBinOp(const BinOp&, bool = false);
  ExpressionResult generateExpressionFunctionCall(const FunctionCall&);
  ExpressionResult generateExpressionUnOp(const UnOp&);
  ExpressionResult loadValue(const Token&);

  uint32_t generateVariableDeclaration(const VariableDec&, bool = true);
  uint32_t generateVariableDeclarationStructType(const VariableDec&, bool);
  uint32_t sizeOfStruct(StructDecList *);

  void generateStatement(const Statement&);
  void generateControlFlowStatement(const ControlFlowStatement&);
  void generateIfStatement(const IfStatement&);
  void updateJumpOp(JumpMarkerType, JumpMarkerType = JumpMarkerType::NONE);
  void addMarker(JumpMarkerType);

  void generateScope(const Scope&);

  void addByte(OpCodes);
  void addByte(unsigned char);
  void addBytes(const std::vector<unsigned char>&);
  void alignForImm(uint32_t, uint32_t);
  void moveImmToReg(uint8_t, uint64_t);
  ExpressionResult mathematicalBinOp(const BinOp&, OpCodes, OpCodes);
  ExpressionResult assignmentBinOp(const BinOp&, OpCodes, OpCodes);
  ExpressionResult logicalBinOp(const BinOp&, OpCodes, OpCodes, OpCodes, bool = false);
  int getStackOffset(const std::string&);
  unsigned char allocateRegister();
  void freeRegister(unsigned char);
};

/*

*/