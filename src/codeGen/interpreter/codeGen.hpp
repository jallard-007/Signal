#pragma once

#include <map>
#include <array>
#include <cstdint>
#include "../../nodes.hpp"
#include "../../tokenizer/tokenizer.hpp"
#include "../../bytecodeDesign.hpp"
#include "../../checker/checker.hpp"

struct RegisterInfo {
  bool inUse {false};
  bool changed {false};
};

struct ExpressionResult {
  uint64_t val{0};
  OpCodes jumpOp {OpCodes::NOP};
  bool isReg {false};
  bool isTemp {false};
};

struct StructInformation {
  std::map<std::string, uint32_t> offsetMap;
  int32_t size = -1;
  uint32_t alignTo = 0;
  StructInformation() = default;
};

enum class MarkerType: uint8_t {
  NONE,
  START_IF,
  IF_STATEMENT,
  END_IF,
  START_LOOP,
  SWITCH,
  BREAK,
  CONTINUE,
  SHORT_CIRCUIT,
};

struct Marker {
  uint64_t index;
  MarkerType type;
  Marker() = delete;
  Marker(uint64_t, MarkerType);
  bool operator==(const Marker) const;
};

enum class StackMarkerType: uint8_t {
  NONE,
  HARD_SCOPE_START,
  SOFT_SCOPE_START,
};

struct StackVariable {
  const VariableDec &varDec;
  uint32_t offset = 0;
  unsigned char reg = 0;
};

enum class StackItemType: uint8_t {
  NONE,
  MARKER,
  VARIABLE,
  RETURN_ADDRESS,
  RETURN_VALUE,
  BASE_POINTER
};

struct StackItem {
  union {
    StackVariable variable;
    StackMarkerType marker;
    uint32_t offset;
  };
  StackItemType type = StackItemType::NONE;
};

struct CodeGen {
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  std::map<std::string, StructInformation> structNameToInfoMap;
  std::map<std::string, uint32_t> nameToStackItemIndex;
  std::vector<unsigned char> byteCode;
  std::vector<Marker> jumpMarkers;
  std::vector<StackItem> stack;
  Tokenizer *tk{nullptr};
  Program &program;
  std::map<std::string, GeneralDec *>& lookUp;
  std::vector<Tokenizer> &tokenizers;

  CodeGen(Program&, std::vector<Tokenizer>&, std::map<std::string, GeneralDec *>&);

  // expression gen
  ExpressionResult generateExpression(const Expression&, bool = false);
  ExpressionResult generateExpressionArrAccess(const ArrayAccess&);
  ExpressionResult generateExpressionArrOrStructLit(const ArrayOrStructLiteral&);
  ExpressionResult generateExpressionFunctionCall(const FunctionCall&);
  ExpressionResult generateExpressionUnOp(const UnOp&);

  // binary ops
  ExpressionResult generateExpressionBinOp(const BinOp&, bool = false);
  ExpressionResult mathematicalBinOp(const BinOp&, OpCodes, OpCodes);
  ExpressionResult assignmentBinOp(const BinOp&, OpCodes, OpCodes);
  ExpressionResult booleanBinOp(const BinOp&, OpCodes, OpCodes, OpCodes, bool = false);

  ExpressionResult loadValue(const Token&);

  bool generate();
  bool generateGeneralDeclaration(const GeneralDec&);
  bool generateFunctionDeclaration(const FunctionDec&);
  bool generateVariableDeclaration(const VariableDec&, bool = true);
  void generateVariableDeclarationStructType(const VariableDec&, bool);
  StructInformation& getStructInfo(const std::string&);

  void generateStatement(const Statement&);
  void generateControlFlowStatement(const ControlFlowStatement&);
  void generateIfStatement(const IfStatement&);
  void updateJumpOpTo(uint64_t, MarkerType, MarkerType = MarkerType::NONE);
  uint64_t addMarker(MarkerType);

  // scopes
  void generateScope(const Scope&);
  void addFunctionSignatureToVirtualStack(const FunctionDec&);
  void startSoftScope();
  void endSoftScope();
  void startFunctionScope(const FunctionDec&);
  void endFunctionScope(const FunctionDec&);

  // adding to the bytecode
  void addByteOp(OpCodes);
  void addByte(unsigned char);
  void addBytes(const std::vector<unsigned char>&);
  void add2ByteNum(const uint16_t);
  void add4ByteNum(const uint32_t);
  void add8ByteNum(const uint64_t);
  void addBytesBasedOnEndianess(const void *, uint64_t);
  void addPointer();


  void alignForImm(uint32_t, uint32_t);
  void moveImmToReg(uint8_t, uint64_t);

  // stack stuff
  uint32_t getCurrStackPointerOffset();
  void makeRoomOnVirtualStack(Token, uint32_t = 0);
  StackVariable& addVarDecToVirtualStack(const VariableDec&);
  void addExpressionResToStack(const ExpressionResult&);
  void addTokenToStack(Token);
  uint32_t getOffsetPushingItemToStack(uint32_t, Token, uint32_t = 0);
  int getStackOffset(const std::string&);

  unsigned char allocateRegister();
  void freeRegister(unsigned char);
  uint32_t sizeOfType(Token);
  Token getTypeFromTokenList(const TokenList&);
};

OpCodes getLoadOpForSize(unsigned char);


/*
hard scope change: starting a new function (variables can take on any identifier not in the global scope)
soft scope change: scope within a function (variables within cannot take the same identifier as variables outside)

*/