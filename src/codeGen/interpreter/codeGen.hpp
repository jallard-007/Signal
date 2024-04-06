#pragma once

#include <unordered_map>
#include <array>
#include <list>
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

    inline void setReg(bytecode_t reg) { data[0] = reg; isReg = true;  /* setting all values as temporary for now to force reloading */ isTemp = true; }
    inline bytecode_t getReg() const { assert(isReg); return data[0]; }
};

enum class JumpMarkerType: uint8_t {
    NONE,
    
    // marks actual jump statements, a jump op is at the paired index 
    TO_BRANCH_END, // go to end of branching statement (condition was false)
    TO_IF_CHAIN_END, // to go end if if/elif/else chain
    TO_LOOP_START,
    TO_LOOP_END,
    TO_LOGICAL_BIN_OP_END,

    // other
    FUNCTION_CALL,

    SET, // the jump marker has been set
};

bool jumpMarkerTypeHasJump(JumpMarkerType);

struct JumpMarker {
    uint64_t start;
    union {
        uint64_t destination;
        const FunctionDec *funcDec;
    };
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
    // bytecode_t reg = 0;
};

struct SavedTempValue {
    const TokenList& type;
    uint32_t positionOnStack = 0;
    bytecode_t reg = 0;
};

enum class StackItemType: uint8_t {
    NONE,
    MARKER,
    VARIABLE,
    RETURN_ADDRESS,
    RETURN_VALUE_SPACE_POINTER,
    RETURNED_VALUE_SPACE,
    ARGUMENT,
    SAVED_TEMPORARY,
    PADDING,
};

struct StackItem {
    union {
        SavedTempValue savedTempValue;
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

// #define STACK_RETURN_VALUE_IDENTIFIER "-rvp"
#define STACK_RETURN_ADDRESS_IDENTIFIER "-ra"

struct CodeGen {
    std::array<RegisterInfo, NUM_REGISTERS> registers;
    std::unordered_map<std::string, uint32_t> nameToStackItemIndex;
    std::unordered_map<const FunctionDec*, uint32_t> functionDecPointerToIndex;
    std::vector<unsigned char> byteCode;
    std::vector<unsigned char> dataSection;
    std::vector<DataSectionEntry> dataSectionEntries;
    std::list<JumpMarker> jumpMarkers;
    std::vector<StackItem> stackItems;
    Tokenizer *tk{nullptr};
    Program &program;
    std::vector<Tokenizer> &tokenizers;
    std::unordered_map<std::string, GeneralDec *>& lookUp;
    std::unordered_map<StructDec *, StructInformation>& structLookUp;

    CodeGen(
        Program&,
        std::vector<Tokenizer>&,
        std::unordered_map<std::string, GeneralDec *>&,
        std::unordered_map<StructDec *, StructInformation>&
    );

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
    JumpMarker& addJumpMarker(JumpMarkerType);
    void updateJumpMarkersTo(uint64_t, JumpMarkerType, uint32_t);
    void writeLocalJumpOffsets();
    void writeFunctionJumpOffsets();

// DECLARATIONS
    void generateGeneralDeclaration(const GeneralDec&);
    void generateVariableDeclaration(const VariableDec&, bool = true);

// GENERAL EXPRESSIONS
    [[nodiscard]] ExpressionResult generateExpression(const Expression&, bool = false);
    [[nodiscard]] ExpressionResult generateExpressionArrAccess(const ArrayAccess&);
    [[nodiscard]] ExpressionResult generateExpressionArrOrStructLit(const ArrayOrStructLiteral&);
    [[nodiscard]] ExpressionResult generateExpressionFunctionCall(const FunctionCall&);
    void expressionResWithOp(OpCode, OpCode, const ExpressionResult&, const ExpressionResult&);
    [[nodiscard]] ExpressionResult getAddressOfExpression(const Expression&);
    [[nodiscard]] ExpressionResult loadValue(const Expression&);
    [[nodiscard]] ExpressionResult loadValueFromPointer(const ExpressionResult &, bytecode_t);
    void storeValueToPointer(const ExpressionResult &, ExpressionResult &);
    void doPointerIndex(const ExpressionResult &, ExpressionResult&);
    void copyValue(ExpressionResult &, ExpressionResult &);


// BINARY EXPRESSIONS
    [[nodiscard]] ExpressionResult mathematicalBinOp(const BinOp&, OpCode, OpCode);
    [[nodiscard]] ExpressionResult assignmentBinOp(const BinOp&, OpCode, OpCode);
    [[nodiscard]] ExpressionResult booleanBinOp(const BinOp&, OpCode, OpCode, OpCode, bool = false);
    [[nodiscard]] ExpressionResult generateExpressionBinOp(const BinOp&, bool = false);

// UNARY EXPRESSIONS
    [[nodiscard]] ExpressionResult generateExpressionUnOp(const UnOp&);
    [[nodiscard]] ExpressionResult defaultUnOp(const UnOp&, OpCode);
    [[nodiscard]] ExpressionResult incrementOrDecrement(const UnOp&, TokenType);

// STRUCTS

// SCOPES
    void generateScope(const Scope&);
    void generateStatementList(const StatementList*);
    void startSoftScope();
    void endSoftScope();

// FUNCTIONS
    void generateFunctionDeclaration(const FunctionDec&);
    bool sizeRequiresReturnValuePointer(uint32_t);

// STATEMENTS
    void generateStatement(const Statement&);
    void generateControlFlowStatement(const ControlFlowStatement&);
    BranchStatementResult generateBranchStatement(const BranchStatement&);
    void generateReturnStatement(const ReturnStatement&);

// STACK MANAGEMENT
    uint32_t getPositionOnStack(uint32_t);
    void addSpaceToStack(uint32_t);
    void addPaddingToStack(uint32_t, bool = false);
    uint32_t addPaddingAndSpaceToStack(uint32_t, uint32_t);
    StackVariable& addVarDecToStack(const VariableDec&, uint32_t = 0, bool = false);
    StackVariable& addItemToStack(uint32_t, uint32_t, bool = false);

    uint32_t getCurrStackPointerPosition();
    StackItem& addExpressionResToStack(ExpressionResult&, StackItemType, uint32_t = 0);
    void addFunctionSignatureToVirtualStack(const FunctionDec&);

    void fakeClearStackFromTo(uint32_t, int32_t);
    uint32_t getVarOffsetFromSP(const StackVariable &);
    uint32_t getOffsetFromSP(uint32_t);

    void popValue(const TokenList&, bytecode_t);
    void postFunctionCall();

// OTHER
    bytecode_t allocateRegister();
    void freeRegister(bytecode_t);
    uint32_t getSizeOfType(Token);
    void efficientImmAddOrSub(bytecode_t, uint64_t, OpCode);
};

OpCode getLoadOpForSize(uint32_t);
OpCode getStoreOpForSize(uint32_t);
OpCode getPopOpForSize(uint32_t);
OpCode getPushOpForSize(uint32_t);
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


