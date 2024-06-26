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
#include "compTime/compTime.hpp"

struct RegisterInfo {
    bool inUse {false};
    bool changed {false};
};

struct ExpressionResult {
    LiteralValue value;
    OpCode jumpOp {OpCode::NOP};
    OpCode getOp {OpCode::NOP};
    bool isReg {false};
    bool isTemp {false};
    bool isPointerToValue {false};

    inline void setReg(bytecode_t reg) { value.data[0] = reg; isReg = true;  /* setting all values as temporary for now to force reloading */ isTemp = true; }
    inline bytecode_t getReg() const { assert(isReg); return value.data[0]; }
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

/**
 * Used to store information regarding where a jump is and where it wants to go
*/
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
    VariableDec &varDec;
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

#define STACK_RETURN_ADDRESS_IDENTIFIER "-ra"

struct CodeGen {
    /**
     * List of register info
    */
    std::array<RegisterInfo, NUM_REGISTERS> registers;

    /**
     * The generated bytecode
    */
    std::vector<unsigned char> byteCode;

    /**
     * Stores items placed on the stack
    */
    std::vector<StackItem> stackItems;

    /**
     * maps name to index within the stackItems vector 
    */
    std::unordered_map<std::string, uint32_t> nameToStackItemIndex;

    /**
     * Maps FunctionDec * to an index in the bytecode. This allows looking up where a function is
    */
    std::unordered_map<const FunctionDec*, uint32_t> functionDecPointerToIndex;

    /**
     * The data section of the bytecode. This should contain things such as string literals, space for items that have global scope
    */
    std::vector<unsigned char> dataSection;

    /**
     * A list of DataSectionEntry for things that are in the dataSection. 
    */
    std::vector<DataSectionEntry> dataSectionEntries;

    /**
     * A list of jump markers 
    */
    std::list<JumpMarker> jumpMarkers;

    /**
     * A pointer to a tokenizer object.
     * This is not a list of tokenizers, but just a pointer to the active tokenizer.
     * When switching between GeneralDecs you must switch to the correct tokenizer for that dec
    */
    Tokenizer *tk{nullptr};

    /**
     * A reference to a program object (source code AST)
    */
    Program &program;

    /**
     * A reference to the list of all tokenizers used to create the program
    */
    std::vector<Tokenizer> &tokenizers;

    /**
     * A reference to the lookUp map made by the Checker
    */
    std::unordered_map<std::string, const GeneralDec *>& lookUp;

    /**
     * A reference to the structLookUp map made by the Checker
    */
    std::unordered_map<const StructDec *, StructInformation>& structLookUp;

    CodeGen(
        Program&,
        std::vector<Tokenizer>&,
        std::unordered_map<std::string, const GeneralDec *>&,
        std::unordered_map<const StructDec *, StructInformation>&
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

    /**
     * Used to add a jump op. Adds both the jump op and extra byte for the offset
     * 
     * \param jumpOp the jump op to add. only RS_JUMP* ops are valid as parameters
    */
    void addJumpOp(OpCode jumpOp);

    /**
     * Align for 'size' byte immediate with 'offset' from current position within the bytecode.
     * Use before any instruction that has an immediate value.
     * \param offset number of bytes before immediate
     * \param size size in bytes of immediate
     * \example when trying to add a ADD_I op, since ADD_I takes a dest reg and then a 2 byte immediate
     * offset = 2 ; offset until the immediate is 2 (ADD_I is 1 byte, dest reg is another byte)
     * size = 2 ; immediate size is 2
     * you would call this function with alignForImm(2, 2)
    */
    void alignForImm(uint32_t offset, uint32_t size);

    /**
     * Moves an immediate value within an ExpressionResult into a register
     * Updates expRes by setting isReg and isTemp to true, as well as setting the register to reg parameter
     * \param reg the register to move the immediate into. typically you would use allocateRegister()
     * \param expRes the expRes object
    */
    void moveImmToReg(uint8_t reg, ExpressionResult& expRes);

// JUMP MARKERS

    /**
     * \param type the type of jump marker
     * \returns the index of the jump marker within the jump markers array
    */
    JumpMarker& addJumpMarker(JumpMarkerType type);

    /**
     * Updates jump markers with destination. Sets type of each updated marker to JumpMarkerType::SET
     * \param destination the destination
     * \param type type of jump marker to update
     * \param from update markers from this index within the jumper markers container until the end
    */
    void updateJumpMarkersTo(uint64_t destination, JumpMarkerType type, uint32_t from);
    
    void writeLocalJumpOffsets();
    
    void writeFunctionJumpOffsets();

// DECLARATIONS

    void generateGeneralDeclaration(const GeneralDec&);

    /**
    */
    void generateVariableDeclaration(VariableDec&);

    void generateArrayVariableDeclaration(VariableDec&);

    void generateContainerLiteralArray(const ExpressionList *exp, TokenList *containerType, ExpressionResult& pointerExp, uint32_t&, uint32_t);

    void generateStructDeclaration(const StructDec&);

    void generateStructVariableDeclaration(VariableDec& varDec);

    void generateContainerLiteralStruct(const ExpressionList *exp, TokenList *containerType, ExpressionResult& pointerExp, uint32_t&, uint32_t);


    void generateFunctionDeclaration(const FunctionDec&);


// GENERAL EXPRESSIONS

    /**
     * 
     * \note if the expression is a string literal, the returned object has
     * memory allocated using new within the field value.type (TokenList)
    */
    [[nodiscard]] ExpressionResult generateExpression(const Expression&);

    /**
     * 
    */
    void postExpression(ExpressionResult& expRes, bool loadImmediate, int16_t reg = -1);

    BranchStatementResult postExpressionControlFlow(ExpressionResult& expRes);

    [[nodiscard]] ExpressionResult generateExpressionArrAccess(const ArrayAccess&);

    /**
     * Generates a function call expression
     * \param functionCall the function call node
     * \returns an ExpressionResult object
    */
    [[nodiscard]] ExpressionResult generateExpressionFunctionCall(const FunctionCall&);

    /**
     * Uses the misc register
    */
    void expressionResWithOp(OpCode, OpCode, const ExpressionResult&, const ExpressionResult&);

    /**
     * Generates the expression and returns the address of the result.
     * the expression must be of a type that has a resulting address.
     * Valid expression types: value (value must be an identifier, which is a valid variable), array access, binary op member access, unary op dereference
     * \returns an ExpressionResult object. isTemp is true if isReg is true.
     * if isReg is set to false, it means there is something wrong with the expression. this should have been caught in the checker stage
    */
    [[nodiscard]] ExpressionResult getAddressOfExpression(const Expression&);

    [[nodiscard]] ExpressionResult handleStringLiteral(ExpressionResult&);

    [[nodiscard]] ExpressionResult loadValue(const Expression&);

    /**
     * Loads the data pointed at by expRes into a register
     * \param expRes the expression result with the pointer expression
     * \param reg the register to load the value into
    */
    [[nodiscard]] ExpressionResult loadValueFromPointer(const ExpressionResult& expRes, bytecode_t reg);

    /**
     * Stores data
    */
    void storeValueToPointer(const ExpressionResult &, ExpressionResult &);

    void copyValue(ExpressionResult &, ExpressionResult &);
    
    /**
     * Adds to a pointer based on type size
     * \param pointerExp the pointer expression to add to. Must be a pointer type
     * \param indexExp the index expression, can be any integral value
    */
    void doPointerIndex(const ExpressionResult &, ExpressionResult&);


// BINARY EXPRESSIONS

    /**
     * Generates byte code for a mathematical binary expression
     * Preserves any non-temporary values
    */
    [[nodiscard]] ExpressionResult mathematicalBinOp(const BinOp&, OpCode, OpCode);

    /**
     * Generates byte code for assignment expressions
     * Values on the left side are not preserved
    */
    [[nodiscard]] ExpressionResult assignmentBinOp(const BinOp&, OpCode, OpCode);

    /**
     * Generates the code for a boolean bin op.
     * When controlFlow is set to true, this will set the jumpOp field in the return value to the correct jump op.
     * When controlFlow is set to false, the result will be put into a register
     * \param binOp the binOp object to generate code for
     * \param op the op code used to do the comparison
     * \param jumpOp the jump op code to use on control flow statement. jump on !condition, so the jump op jumps when the condition is false. used when controlFlow is true
     * \param getOp the get op code to use when placing the result in a register. used when controlFlow is false
     * \param controlFlow dictates if the jump op will be returned, or if the get op will be used and a register will be returned
    */
    [[nodiscard]] ExpressionResult booleanBinOp(const BinOp& binOp, OpCode op, OpCode jumpOp, OpCode getOp);
    [[nodiscard]] ExpressionResult generateExpressionBinOp(const BinOp&);

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


// STATEMENTS
    void generateStatement(const Statement&);
    void generateControlFlowStatement(const ControlFlowStatement&);
    void generateForLoopStatement(const ForLoop& forLoop);
    void generateWhileLoopStatement(const WhileLoop& whileLoop);

    /**
     * Updates jump markers and adds un unconditional jump to loop back
     * Should be called after generating a loop statement
     * \param startOfLoopIndex the start of the loop within the bytecode
     * \param loopJumpMarkersFrom go until this point while updating jump markers
     * \param res the result of the conditional statement
    */
    void generateEndLoop(uint32_t startOfLoopIndex, uint32_t loopJumpMarkersFrom, BranchStatementResult res);

    void generateConditionalStatement(const ConditionalStatement& condStatement);


    /**
     * Generate a branching statement. If return result is BranchStatementResult::ADDED_JUMP,
     * a jump marker was added of type JumpMarkerType::TO_BRANCH_END that must be updated to land at the end of the branch
     * \param branchStatement the branch statement
    */
    BranchStatementResult generateBranchStatement(const BranchStatement& branchStatement);

    /**
    */
    void generateReturnStatement(const ReturnStatement&);

// STACK MANAGEMENT
    uint32_t getPositionOnStack(uint32_t);
    void addSpaceToStack(uint32_t);

    /**
     * Adds padding to the stack
     * \param padding how much padding to add
     * \param virtualOnly if padding should only be added to the virtual stack
    */
    void addPaddingToStack(uint32_t padding, bool virtualOnly = false);

    /**
     * Adds padding + space room to the stack, and adds a stack item for the padding.
     * You should be adding another stack item for the space added
     * \param padding how much padding to add
     * \param space how much space after the padding to add
     * \returns the offset for start of 'space'
    */
    uint32_t addPaddingAndSpaceToStack(uint32_t padding, uint32_t space);

    uint32_t addZeroedSpaceToStack(uint32_t padding, uint32_t space);

    /**
     * Adds a variable dec to the virtual stack. Also adds padding if necessary.
     * \param varDec the variable declaration to be added
     * \param alignTo optionally set the alignment. if left at the default of 0, will use the type's alignment
     * \param virtualOnly if set to true, only adds padding and var dec to virtual stack (does not update bytecode)
     *  otherwise will update the bytecode by padding, and padding only
     * \returns a pointer to the struct info of the type if the type is a struct, otherwise nullptr
    */
    const StructInformation* addVarDecToStack(VariableDec& varDec, uint32_t alignTo = 0, bool virtualOnly = false);

    StackVariable& addItemToStack(uint32_t, uint32_t, bool = false);

    uint32_t getCurrStackPointerPosition();
    StackItem& addExpressionResToStack(ExpressionResult&, StackItemType, uint32_t = 0);
    void addFunctionSignatureToVirtualStack(const FunctionDec&);

    /**
     * Generates the byte code needed to clear the stack at index 'from' until 'to'.
     * A negative 'to' value results in the entire stack being cleared.
     * Does not remove any items from the virtual stack.
     * 
     * \param from the index to start from
     * \param to the index to go to
    */
    void fakeClearStackFromTo(uint32_t from, int32_t to);

    uint32_t getVarOffsetFromSP(const StackVariable &);
    uint32_t getOffsetFromSP(uint32_t);

    /**
     * Pops the item at the top of the stack off
     * \param type the type of the item on the stack
     * \param reg the register to pop the value into
    */
    void popValue(const TokenList& type, bytecode_t reg);

    /**
     * Pops saved values back into the registers where they are expected to be.
     * Should always be called after an expression with isReturnedValue is acquired,
     * but the returned value should be popped first
    */
    void postFunctionCall();

// OTHER
    bytecode_t allocateRegister();
    void freeRegister(bytecode_t);
    uint32_t getSizeOfType(const TokenList*);
    void efficientImmAddOrSub(bytecode_t, uint64_t, OpCode);
};

Token getBaseTypeOfArray(const TokenList *tokenList);

OpCode getLoadOpForSize(uint32_t);
OpCode getStoreOpForSize(uint32_t);
OpCode getPopOpForSize(uint32_t);
OpCode getPushOpForSize(uint32_t);


// ========================================
// PRINTING
// ========================================

std::ostream& operator<<(std::ostream& os, const StackVariable& obj);
std::ostream& operator<<(std::ostream& os, const StackMarkerType& obj);

std::ostream& operator<<(std::ostream& os, const StackItem& obj);
std::ostream& operator<<(std::ostream& os, const std::vector<StackItem>& obj);
