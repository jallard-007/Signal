#pragma once

#include "nodes.hpp"
#include "nodeMemPool.hpp"
#include <map>
#include <unordered_map>

enum class CheckerErrorType: uint8_t {
    NONE,

    // general
    NAME_ALREADY_IN_USE,
    VOID_TYPE,
    TYPE_DOES_NOT_MATCH,
    UNEXPECTED_TYPE,
    EXPECTED_IDENTIFIER,
    EXPECTING_TYPE,
    EXPECTING_NUMBER,
    INCORRECT_RETURN_TYPE,
    INVALID_EXIT_TYPE,
    NOT_ALL_CODE_PATHS_RETURN,
    EMPTY_STRUCT,
    STRUCT_CYCLE,
    MISSING_MAIN_FUNCTION,
    INVALID_MAIN_FUNCTION_SIGNATURE,
    TYPE_TOO_LARGE_TO_RETURN,
    TYPE_TOO_LARGE_TO_BE_AN_ARGUMENT,

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
    std::unordered_map<std::string, StructMemberInformation> memberLookup;
    uint32_t size = 0;
    uint32_t alignTo = 0;
    StructInformation() = default;
};

struct CheckerError {
    Token token;
    GeneralDec *dec{nullptr};
    uint32_t tkIndex{0};
    CheckerErrorType type{CheckerErrorType::NONE};
    CheckerError() = delete;
    CheckerError(CheckerErrorType);
    CheckerError(CheckerErrorType, uint32_t, Token);
    CheckerError(CheckerErrorType, uint32_t, Token, GeneralDec*);
    CheckerError(CheckerErrorType, uint32_t, Expression*);
    CheckerError(CheckerErrorType, uint32_t, Expression*, GeneralDec*);
    std::string getErrorMessage(std::vector<Tokenizer>&) const;
};

struct ResultingType {
    TokenList *type{nullptr};
    bool isLValue{false};
    ResultingType(TokenList*, bool);
};

#define MAX_ERRORS 20

struct Checker {
    std::unordered_map<std::string, GeneralDec *> lookUp;
    std::unordered_map<const StructDec *, StructInformation> structLookUp;
    std::vector<CheckerError> errors;
    std::vector<std::string> locals;
    Program& program;
    std::vector<Tokenizer>& tokenizers;
    Tokenizer *tk;
    NodeMemPool &memPool;

    static TokenList noneValue;
    static TokenList badValue;
    static TokenList boolValue;
    static TokenList int32Value;
    static TokenList uint32Value;
    static TokenList int64Value;
    static TokenList uint64Value;
    static TokenList charValue;
    static TokenList stringValue;
    // static TokenList floatValue;
    static TokenList doubleValue;
    static TokenList fileValue;
    static TokenList ptrValue;
    static TokenList nullptrValue;
    static TokenList voidValue;

    Checker(Program&, std::vector<Tokenizer>&, NodeMemPool&);
    bool check(bool = false);
    void firstTopLevelScan();
    void secondTopLevelScan(bool = false);
    void fullScan();
    void checkFunction(FunctionDec&);
    bool validateFunctionHeader(FunctionDec&);
    void validateStructTopLevel(StructDec&);
    StructInformation& getStructInfo(const StructDec&);
    void checkForStructCycles(GeneralDec&, std::vector<StructDec *>&);
    bool checkStatement(Statement&, const TokenList&, bool, bool);
    bool checkScope(Scope&, const TokenList&, bool, bool);
    bool checkLocalVarDec(VariableDec&);
    ResultingType checkExpression(Expression&, std::unordered_map<std::string, StructMemberInformation> *structMap = nullptr);
    ResultingType checkMemberAccess(ResultingType&, Expression&);
    bool checkType(TokenList&);
    bool checkAssignment(const TokenList*, const TokenList*, bool);

    void addError(const CheckerError&);
    void removeLastError();
    static TokenList& largestType(TokenList&, TokenList&);
};

bool canBeConvertedToBool(const TokenList*);
Token getTypeFromTokenList(const TokenList&);
const TokenList* getNextFromTokenList(const TokenList&);
const TokenList* getTypeQualifier(const TokenList&);
uint32_t getPaddingNeeded(uint32_t, uint32_t, uint32_t);
uint32_t getSizeOfBuiltinType(TokenType);
