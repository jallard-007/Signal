#pragma once

#include <map>
#include <array>
#include <span>
#include <cstdint>
#include "nodes.hpp"
#include "tokenizer/tokenizer.hpp"
#include "bytecodeDesign/bytecodeDesign.hpp"
#include "checker/checker.hpp"

struct RegisterInfo {
  bool inUse {false};
  bool changed {false};
};

struct ExpressionResult {
  private:
  bytecode_t data [SIZE_OF_REGISTER] {0};
  public:
  const TokenList *type {nullptr};
  OpCode jumpOp {OpCode::NOP};
  bool isReg {false};
  bool isTemp {false};
  inline const void *getData() const { return (void *)data; }
  void setData(const void *, uint8_t);
  inline void setReg(bytecode_t reg) { data[0] = reg; }
  inline bytecode_t getReg() const { return data[0]; }
};

struct StructInformation {
  std::map<std::string, uint32_t> offsetMap;
  int32_t size = -1; // do we want to allow structs without any member variables? if not, change this to unsigned, make checker show error
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
  uint32_t positionOnStack = 0;
  bytecode_t reg = 0;
};

enum class StackItemType: uint8_t {
  NONE,
  MARKER,
  VARIABLE,
  RETURN_ADDRESS,
  RETURN_VALUE,
};

struct StackItem {
  union {
    StackVariable variable;
    StackMarkerType marker;
    uint32_t positionOnStack;
  };
  StackItemType type = StackItemType::NONE;
};

enum class BranchStatementResult: uint8_t {
  ADDED_JUMP,
  ALWAYS_TRUE,
  ALWAYS_FALSE,
};

enum class DataSectionEntryType {
  STRING_LITERAL,
  STATIC_DATA,
};

struct DataSectionEntry {
  DataSectionEntryType type;
  uint32_t indexInDataSection;
  DataSectionEntry() = delete;
  DataSectionEntry(DataSectionEntryType type, uint32_t indexInDataSection): type{type}, indexInDataSection{indexInDataSection} {}
};

struct CodeGen {
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  std::map<std::string, StructInformation> structNameToInfoMap;
  std::map<std::string, uint32_t> nameToStackItemIndex;
  std::vector<unsigned char> byteCode;
  std::vector<unsigned char> dataSection;
  std::vector<DataSectionEntry> dataSectionEntries;
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

  const DataSectionEntry& addToDataSection(DataSectionEntryType, void *data, uint32_t n);

  // binary ops
  ExpressionResult generateExpressionBinOp(const BinOp&, bool = false);
  ExpressionResult mathematicalBinOp(const BinOp&, OpCode, OpCode);
  ExpressionResult assignmentBinOp(const BinOp&, OpCode, OpCode);
  ExpressionResult booleanBinOp(const BinOp&, OpCode, OpCode, OpCode, bool = false);

  uint32_t getVarOffsetFromSP(const StackVariable &);
  ExpressionResult loadValue(const Token&);
  ExpressionResult getAddressOfExpression(const Expression&);

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
  void addByteOp(OpCode);
  void addByte(bytecode_t);
  void addBytes(const std::span<const bytecode_t>);
  void addNumBytes(const void *, uint64_t);
  void add2ByteNum(const uint16_t);
  void add4ByteNum(const uint32_t);
  void add8ByteNum(const uint64_t);
  void addPointer();
  void addJumpOp(OpCode);
  ExpressionResult expressionResWithOp(OpCode, OpCode, const ExpressionResult&, const ExpressionResult&);

  void updateJumpOpTo(uint64_t, uint64_t);


  void alignForImm(uint32_t, uint32_t);
  void moveImmToReg(uint8_t, ExpressionResult&);

  // stack stuff
  uint32_t getCurrStackPointerPosition();
  void makeRoomOnVirtualStack(Token, uint32_t = 0);
  StackVariable& addVarDecToVirtualStack(const VariableDec&);
  void addExpressionResToStack(const ExpressionResult&);
  void addTokenToStack(Token);
  uint32_t getPositionPushingItemToStack(Token, uint32_t = 0);
  int getStackOffset(const std::string&);

  bytecode_t allocateRegister();
  void freeRegister(bytecode_t);
  uint32_t sizeOfType(Token);
  Token getTypeFromTokenList(const TokenList&);
};

OpCode getLoadOpForSize(unsigned char);


/*
hard scope change: starting a new function (variables can take on any identifier not in the global scope)
soft scope change: scope within a function (variables within cannot take the same identifier as variables outside)

*/