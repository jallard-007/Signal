#pragma once

#include <map>
#include <array>
#include <span>
#include <cstdint>
#include <cassert>
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
  bool isPointerToValue {false};
  inline const void *getData() const { return (void *)data; }

  template <class T> void set(T) = delete;
  void set(char);
  void set(uint32_t);
  void set(uint64_t);
  void set(int32_t);
  void set(int64_t);
  void set(FILE *);
  void set(double);
  void set(bool);

  template <class T> void setUntyped(T) requires (std::integral<T> || std::floating_point<T>);

  inline void setReg(bytecode_t reg) { data[0] = reg; isReg = true; }
  inline bytecode_t getReg() const { assert(isReg); return data[0]; }
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

struct TempStackValue {
  uint32_t positionOnStack = 0;
};

enum class StackItemType: uint8_t {
  NONE,
  MARKER,
  VARIABLE,
  RETURN_ADDRESS,
  RETURN_VALUE,
  TEMP_VALUE,
};

struct StackItem {
  union {
    TempStackValue tempValue;
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
  STDIN,
  STDOUT,
  STDERR,
  STRING_LITERAL,
  STATIC_DATA,
};

struct DataSectionEntry {
  DataSectionEntryType type;
  uint32_t indexInDataSection;
  DataSectionEntry() = delete;
  DataSectionEntry(DataSectionEntryType type, uint32_t indexInDataSection): type{type}, indexInDataSection{indexInDataSection} {}
};

#define STACK_RETURN_VALUE_IDENTIFIER "-rv"
#define STACK_RETURN_ADDRESS_IDENTIFIER "-ra"

struct CodeGen {
  std::array<RegisterInfo, NUM_REGISTERS> registers;
  std::map<std::string, StructInformation> structNameToInfoMap;
  std::map<std::string, uint32_t> nameToStackItemIndex;
  std::vector<unsigned char> byteCode;
  std::vector<unsigned char> dataSection;
  std::vector<DataSectionEntry> dataSectionEntries;
  std::vector<JumpMarker> jumpMarkers;
  std::vector<StackItem> stackItems;
  Tokenizer *tk{nullptr};
  Program &program;
  std::map<std::string, GeneralDec *>& lookUp;
  std::vector<Tokenizer> &tokenizers;

  CodeGen(Program&, std::vector<Tokenizer>&, std::map<std::string, GeneralDec *>&);

  // main driver
  void generate();

  const DataSectionEntry& addToDataSection(DataSectionEntryType, void *data, uint32_t n);

// ADDING TO BYTECODE
  void addByte(bytecode_t);
  void addByteOp(OpCode);
  void addBytes(const std::span<const bytecode_t>);
  void addNumBytes(const void *, uint64_t);
  void add2ByteNum(const uint16_t);
  void add4ByteNum(const uint32_t);
  void add8ByteNum(const uint64_t);
  void addPointer();
  void addJumpOp(OpCode);
  void alignForImm(uint32_t, uint32_t);
  void moveImmToReg(uint8_t, ExpressionResult&);

// JUMP MARKERS
  uint64_t addJumpMarker(JumpMarkerType);
  void updateJumpMarkersTo(uint64_t, JumpMarkerType, JumpMarkerType = JumpMarkerType::NONE, bool = false);

// DECLARATIONS
  void generateGeneralDeclaration(const GeneralDec&);
  void generateVariableDeclaration(const VariableDec&, bool = true);

// GENERAL EXPRESSIONS
  ExpressionResult generateExpression(const Expression&, bool = false);
  ExpressionResult generateExpressionArrAccess(const ArrayAccess&);
  ExpressionResult generateExpressionArrOrStructLit(const ArrayOrStructLiteral&);
  ExpressionResult generateExpressionFunctionCall(const FunctionCall&);
  void expressionResWithOp(OpCode, OpCode, const ExpressionResult&, const ExpressionResult&);
  ExpressionResult getAddressOfExpression(const Expression&);
  ExpressionResult loadValue(const Token&);

// BINARY EXPRESSIONS
  ExpressionResult mathematicalBinOp(const BinOp&, OpCode, OpCode);
  ExpressionResult assignmentBinOp(const BinOp&, OpCode, OpCode);
  ExpressionResult booleanBinOp(const BinOp&, OpCode, OpCode, OpCode, bool = false);
  ExpressionResult generateExpressionBinOp(const BinOp&, bool = false);

// UNARY EXPRESSIONS
  ExpressionResult generateExpressionUnOp(const UnOp&);

// STRUCTS
  StructInformation& getStructInfo(const std::string&);

// SCOPES
  void generateScope(const Scope&);
  void generateStatementList(const StatementList*);
  void startSoftScope();
  void endSoftScope();

// FUNCTIONS
  void generateFunctionDeclaration(const FunctionDec&);

// STATEMENTS
  void generateStatement(const Statement&);
  void generateControlFlowStatement(const ControlFlowStatement&);
  BranchStatementResult generateBranchStatement(const BranchStatement&);
  void generateReturnStatement(const ReturnStatement&);

// STACK MANAGEMENT
  uint32_t getPositionPushingTypeToStack(Token, uint32_t = 0);
  uint32_t getPositionPushingStuctToStack(const StructInformation&);
  uint32_t getPositionOnStack(uint32_t);
  uint32_t getPositionPushingToStack(uint32_t, uint32_t);
  uint32_t getPaddingNeededPushingToStack(uint32_t, uint32_t, uint32_t);
  void addSpaceToStack(uint32_t);
  StackVariable& addVarDecToVirtualStack(const VariableDec&);
  uint32_t getCurrStackPointerPosition();
  void makeRoomOnVirtualStack(Token, uint32_t = 0);
  uint32_t addExpressionResToStack(ExpressionResult&);
  void addFunctionSignatureToVirtualStack(const FunctionDec&);

  void fakeClearStackFromTo(uint32_t, uint32_t);
  uint32_t getVarOffsetFromSP(const StackVariable &);
  uint32_t getOffsetFromSP(uint32_t);


// OTHER
  bytecode_t allocateRegister();
  void freeRegister(bytecode_t);
  uint32_t getSizeOfType(Token);

};

Token getTypeFromTokenList(const TokenList&);
const TokenList* getNextFromTokenList(const TokenList&);
constexpr OpCode getLoadOpForSize(uint32_t);
constexpr OpCode getStoreOpForSize(uint32_t);
constexpr OpCode getPopOpForSize(uint32_t);
constexpr OpCode getPushOpForSize(uint32_t);
// ExpressionResult evaluateUnaryOpImmExpression(TokenType, ExpressionResult&);
// ExpressionResult evaluateBinOpImmExpression(TokenType, ExpressionResult&, ExpressionResult&);

/*
hard scope change: starting a new function (variables can take on any identifier not in the global scope)
soft scope change: scope within a function (variables within cannot take the same identifier as variables outside)

*/



// ========================================
// PRINTING
// ========================================



std::ostream& operator<<(std::ostream& os, const StackVariable& obj);
std::ostream& operator<<(std::ostream& os, const StackMarkerType& obj);

std::ostream& operator<<(std::ostream& os, const StackItem& obj);
std::ostream& operator<<(std::ostream& os, const std::vector<StackItem>& obj);




// ========================================
// COMPARING
// ========================================


