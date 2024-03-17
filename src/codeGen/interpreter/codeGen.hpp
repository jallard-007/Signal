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

enum class JumpMarkerType: uint8_t {
  NONE,
  
  // if/elif/else statements
  BRANCH_START, // marks start of branching statement
  TO_BRANCH_END, // go to end of branching statement (condition was false)
  IF_CHAIN_START, // marks start of if/elif/else chain
  TO_IF_CHAIN_END, // to go end if if/elif/else chain

  // loops
  LOOP_START,
  TO_LOOP_START,
  TO_LOOP_END,

  // short circuit logical binary ops
  LOGICAL_BIN_OP_START,
  TO_LOGICAL_BIN_OP_END,
};

struct JumpMarker {
  uint64_t start;
  uint64_t destination;
  JumpMarkerType type;
  JumpMarker() = delete;
  JumpMarker(uint64_t, JumpMarkerType);
  // bool operator==(const Marker) const;
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

enum class BranchStatementResult: uint8_t {
  ADDED_JUMP,
  ALWAYS_TRUE,
  ALWAYS_FALSE,
};

struct CodeGen {
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  std::map<std::string, StructInformation> structNameToInfoMap;
  std::map<std::string, uint32_t> nameToStackItemIndex;
  std::vector<unsigned char> byteCode;
  std::vector<JumpMarker> jumpMarkers;
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
  BranchStatementResult generateBranchStatement(const BranchStatement&);
  void updateJumpMarkersTo(uint64_t, JumpMarkerType, JumpMarkerType = JumpMarkerType::NONE, bool = false);
  uint64_t addJumpMarker(JumpMarkerType);

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
  void addNumBytes(const void *, uint64_t);
  void add2ByteNum(const uint16_t);
  void add4ByteNum(const uint32_t);
  void add8ByteNum(const uint64_t);
  void addPointer();
  void addJumpOp(OpCodes);

  void updateJumpOpTo(uint64_t, uint64_t);


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