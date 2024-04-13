#pragma once

#include "nodes.hpp"
#include "nodeMemPool.hpp"
#include <map>
#include <unordered_map>
#include "compTime/compTime.hpp"

enum class CheckerErrorType: uint8_t {
    NONE,

    // general
    NAME_ALREADY_IN_USE,
    VOID_TYPE,
    TYPE_DOES_NOT_MATCH,
    UNEXPECTED_TYPE,
    EXPECTED_IDENTIFIER,
    EXPECTING_TYPE,
    INCORRECT_RETURN_TYPE,
    INVALID_EXIT_TYPE,
    NOT_ALL_CODE_PATHS_RETURN,
    EMPTY_STRUCT,
    STRUCT_CYCLE,
    MISSING_MAIN_FUNCTION,
    INVALID_MAIN_FUNCTION_SIGNATURE,
    TYPE_TOO_LARGE_TO_RETURN,
    TYPE_TOO_LARGE_TO_BE_AN_ARGUMENT,
    CANNOT_USE_AS_PARAMETER_TYPE,
    CANNOT_USE_AS_RETURN_TYPE,
    EXPECTING_NAMED_INDEX,
    VARIABLE_INDEX_IN_CONTAINER_LITERAL,
    CONTAINER_LITERAL_TOO_LARGE,
    ELEMENT_TYPE_DOES_NOT_MATCH_ARRAY_TYPE,
    INVALID_STRING_LITERAL,
    USELESS_CONTAINER_LITERAL,

    // array size
    NOT_A_SIZE,
    NON_CONSTANT_SIZE_ARRAY,
    INVALID_ARRAY_SIZE,
    EXPECTED_SIZE,

    // no such
    NO_SUCH_FUNCTION,
    NO_SUCH_TYPE,
    NO_SUCH_VARIABLE,
    NO_SUCH_TEMPLATE,
    NO_SUCH_MEMBER_VARIABLE,
    NO_SUCH_MEMBER_FUNCTION,

    // semantic errors
    CANNOT_REF_A_REF,
    CANNOT_PTR_A_REF,
    CANNOT_HAVE_MULTI_TYPE,
    
    // things in the wrong spot
    CANNOT_HAVE_BREAK_HERE,
    CANNOT_HAVE_CONTINUE_HERE,

    NOT_A_VARIABLE,
    NOT_A_FUNCTION,
    NOT_A_STRUCT,
    NOT_A_TEMPLATE,
    WRONG_NUMBER_OF_ARGS,

    // operator type compatibility
    CANNOT_DEREFERENCE_NON_POINTER_TYPE,
    CANNOT_OPERATE_ON_TEMPORARY,
    CANNOT_ASSIGN_TO_TEMPORARY,
    CANNOT_BE_CONVERTED_TO_BOOL,
    CANNOT_COMPARE_TYPE,
    CANNOT_ASSIGN,
    OPERATION_NOT_DEFINED,
    OPERATION_ON_VOID,
};

struct StructMemberInformation {
    const StructDecList *memberDec{};
    uint32_t position{};
};

struct StructInformation {
    std::unordered_map<std::string, StructMemberInformation> memberLookup{};
    const GeneralDec *const decPtr;
    TokenList *const type;
    uint32_t size = 0;
    uint32_t alignTo = 0;
    StructInformation();
    StructInformation(NodeMemPool&, const GeneralDec*);
};

struct CheckerError {
    Token token;
    const GeneralDec *dec{nullptr};
    uint32_t tkIndex{0};
    CheckerErrorType type{CheckerErrorType::NONE};
    CheckerError() = delete;
    CheckerError(CheckerErrorType);
    CheckerError(CheckerErrorType, uint32_t, Token);
    CheckerError(CheckerErrorType, uint32_t, const GeneralDec*);
    CheckerError(CheckerErrorType, uint32_t, const Expression*);
    CheckerError(CheckerErrorType, uint32_t, Token, const GeneralDec*);
    CheckerError(CheckerErrorType, uint32_t, const Expression*, const GeneralDec*);
    std::string getErrorMessage(std::vector<Tokenizer>&) const;
};

struct ResultingType {
    LiteralValue value;
    bool isLValue;
    bool isLiteral;
    ResultingType(TokenList*, bool);
    ResultingType(TokenList*, bool, bool);
    ResultingType(const LiteralValue&);
};

#define MAX_ERRORS 20

struct Checker {
    std::unordered_map<std::string, const GeneralDec *> lookUp;
    std::unordered_map<const StructDec *, StructInformation> structLookUp;
    std::vector<CheckerError> errors;
    std::vector<std::string> locals;
    Program& program;
    std::vector<Tokenizer>& tokenizers;
    Tokenizer *tk;
    NodeMemPool &memPool;

    Checker(Program&, std::vector<Tokenizer>&, NodeMemPool&);

    bool check(bool = false);

    /**
     * Scans all global declarations and registers them in the table, checking that the name is available
     * Also registers struct members in the struct table
    */
    void firstTopLevelScan();

    /**
     * Validates function types, global variable types, struct member variable types, struct member function types.
     * Everything that was registered in the first pass
    */
    void secondTopLevelScan(bool = false);
    void fullScan();

    /**
     * Validates the internals of a function
     * \param funcDec the function declaration to check
     */
    void checkFunction(FunctionDec& funcDec);

    bool validateFunctionHeader(FunctionDec&);
    void validateStructTopLevel(StructDec&);
    StructInformation& getStructInfo(const StructDec&);
    void checkForStructCycles(const GeneralDec&, std::vector<StructDec *>&);

    /**
     * 
     * \returns if the statement is of return or exit type
    */
    bool checkStatement(Statement&, TokenList&, bool, bool);

    /**
     * \param scope The scope to check
     * \param returnType the return type of the scope
     * \param isReturnRequired set to true if a return is required within this scope
     * \param isLoop is the scope within a loop
     * \param isSwitch is the scope within a switch
     * \returns true if all code paths return a value
    */
    bool checkScope(Scope&, TokenList&, bool, bool);

    bool checkLocalVarDec(VariableDec&);

    /**
     * Returns the resulting type from an expression. The ResultingType always contains a valid pointer.
     * You must check if the result is a literal.
     * If it is a literal, release the sub expression, then make a new LiteralValue node using the memPool,
     * assign the LiteralValue to the original root expression
     * and copy the returned ResultingType.value to the newly allocated LiteralValue node.
     * \param expression the expression to check
     * \param structMap pointer to a struct's lookup map. only used for the right side of binary member access operators
    */
    ResultingType checkExpression(Expression& expression, const StructInformation* structMap = nullptr);
    void postCheckExpression(ResultingType&, Expression& expression);

    ResultingType checkBinOpExpression(BinOp&);
    ResultingType checkUnOpExpression(UnOp&);
    ResultingType checkTokenExpression(Token, const StructInformation* = nullptr);
    ResultingType checkFunctionCallExpression(FunctionCall&, const StructInformation* = nullptr);
    bool checkContainerLiteralStruct(ContainerLiteral&, const StructInformation&);
    ResultingType checkContainerLiteralArray(ContainerLiteral&);
    ResultingType checkMemberAccess(ResultingType&, BinOp&);

    /**
     * Validates a type
     * \param type the type to check
     * \param isReturnType if the type is for a return type
     * \returns true if the type is a valid type, false otherwise (adds the error to errors)
     * \note in the case of the type being just 'void', will return false even though it is valid for function return types.
     *  check if the emplaced error is 'void' and remove it if called for a function return type
    */
    bool checkType(TokenList& type, bool isReturnType = false);

    bool checkAssignment(const TokenList*, const TokenList*, bool, bool = false);

    void addError(const CheckerError&);
    void removeLastError();
    static TokenList& largestType(TokenList&, TokenList&);
};

bool canBeConvertedToBool(const TokenList*);

/**
 * Returns the actual type from a token list
*/
Token getTypeFromTokenList(const TokenList&);

TokenType getTypeQualifier(const TokenList&);

/**
 * Returns the amount of padding needed to align an item in memory
 * \param currOffset is the current offset within the container, 0 is assumed to be 8 byte aligned
 * \param size the size of the item in bytes
 * \param alignTo the alignment needed in bytes
 * \returns the number of bytes of padding needed to align 
*/
uint32_t getPaddingNeeded(uint32_t currOffset, uint32_t size, uint32_t alignTo);

/**
 * Returns the size of a type
 * \param tokenType the type
 * \returns the size of the type
*/
uint32_t getSizeOfBuiltinType(TokenType tokenType);

const GeneralDec *getDecPtr(const TokenList *);
