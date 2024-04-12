/**
 * This implements the 'formal' grammar found in sampleCode/grammar.pr
 * note that the grammar found in that file may be out of date, but generally it should be fairly close to what is done here
*/

#pragma once

#include <cassert>
#include "tokenizer/tokenizer.hpp"

/**
 * Putting this define here so that it can be found everywhere else.
 * Could make another file for it, but idk this is easier
*/
#define SIZE_OF_REGISTER 8

typedef class NodeMemPool NodeMemPool;

bool notFirstOfExpression(TokenType);

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct ContainerLiteral ContainerLiteral;

enum class ExpressionType: uint8_t {
    NONE,
    BINARY_OP,
    UNARY_OP,
    VALUE,
    FUNCTION_CALL,
    ARRAY_ACCESS,
    CONTAINER_LITERAL,
    // VALUE_LITERAL,
};

struct Expression {
    /**
     * The pointer to the sub expression and expression type is packed within 8 bytes
     * The reason this works is because since sizeof(void *) is 8, and aligning requirements for a pointer puts it at
     * an address with $address % 8 == 0 ; the least significant 3 bits of the pointer will always be set to 0.
     * By using getters and setters, we can pull out the type before getting a pointer
     * It's important that each subexpression node must also have an alignment requirement of 8.
     * 
     * Since Token is also a subexpression, the Token struct should leave the least significant 3 bits untouched
    */
    private:
    union {
        uint64_t type;
        BinOp *binOp;
        UnOp *unOp;
        Token value;
        FunctionCall *funcCall;
        ArrayAccess *arrAccess;
        ContainerLiteral *containerLiteral;
    };
    public:
    Expression();
    explicit Expression(Token);
    Expression(const Expression&);
    Expression& operator=(const Expression&);
    void prettyPrint(Tokenizer&, std::string&) const;
    Expression deepCopy(NodeMemPool&);
    #define EXPRESSION_MASK 0x7
    inline BinOp* getBinOp() const { /* assert(getType() == ExpressionType::BINARY_OP); */ return (BinOp *)((uint64_t)binOp & ~EXPRESSION_MASK); } 
    inline UnOp* getUnOp() const { assert(getType() == ExpressionType::UNARY_OP); return (UnOp *)((uint64_t)unOp & ~EXPRESSION_MASK); }
    inline Token getToken() const { assert(getType() == ExpressionType::VALUE); return value; }
    inline Token& getTokenRef() { assert(getType() == ExpressionType::VALUE); return value; }
    inline FunctionCall* getFunctionCall() const { assert(getType() == ExpressionType::FUNCTION_CALL); return (FunctionCall *)((uint64_t)funcCall & ~EXPRESSION_MASK); }
    inline ArrayAccess* getArrayAccess() const { assert(getType() == ExpressionType::ARRAY_ACCESS); return (ArrayAccess *)((uint64_t)arrAccess & ~EXPRESSION_MASK); }
    inline ContainerLiteral* getContainerLiteral() const { assert(getType() == ExpressionType::CONTAINER_LITERAL); return (ContainerLiteral *)((uint64_t)containerLiteral & ~EXPRESSION_MASK); }
    inline ExpressionType getType() const { return (ExpressionType)(type & EXPRESSION_MASK); }
    inline const void *getRawPointer() const { return (const void *)((uint64_t)binOp & ~EXPRESSION_MASK); }

    #define SET_EXP(expType) binOp = (BinOp *)(((uint8_t)expType & EXPRESSION_MASK) | ((uint64_t)ref & ~EXPRESSION_MASK))
    inline void setBinOp(BinOp *ref) { SET_EXP(ExpressionType::BINARY_OP); }
    inline void setUnOp(UnOp *ref) { SET_EXP(ExpressionType::UNARY_OP); }
    inline void setToken(Token ref) { value = ref; setType(ExpressionType::VALUE); }
    inline void setToken(TokenType ref) { value.setType(ref); setType(ExpressionType::VALUE); }
    inline void setFunctionCall(FunctionCall *ref) { SET_EXP(ExpressionType::FUNCTION_CALL); }
    inline void setArrayAccess(ArrayAccess *ref) { SET_EXP(ExpressionType::ARRAY_ACCESS); }
    inline void setContainerLiteral(ContainerLiteral *ref) { SET_EXP(ExpressionType::CONTAINER_LITERAL); }
    inline void setType(ExpressionType ref) { type = (type & ~EXPRESSION_MASK) | ((char)ref & EXPRESSION_MASK); }
    #undef SET_EXP
    #undef EXPRESSION_MASK
};

struct ExpressionList {
    Expression curr;
    ExpressionList *next{nullptr};
    ExpressionList() = default;
    ExpressionList deepCopy(NodeMemPool&);
};

typedef struct ControlFlowStatement ControlFlowStatement;
typedef struct Scope Scope;
typedef struct VariableDec VariableDec;

enum class StatementType: uint8_t {
    NONE,
    EXPRESSION,
    CONTROL_FLOW,
    SCOPE,
    VARIABLE_DEC,
    KEYWORD,
};

// statement:=  expression; | controlFlowStatement | scope | varDec | nothing
struct Statement {
    union {
        Expression *expression;
        ControlFlowStatement *controlFlow;
        Scope *scope;
        VariableDec *varDec;
        Token keyword;
    };
    StatementType type;
    Statement();
    Statement(const Statement&);
    Statement& operator=(const Statement&);
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    Statement deepCopy(NodeMemPool&);

};

// typeList: typeQualifier identifier indirectionTypeList
// indirectionTypeList:= nothing
//                     | typeQualifier ptr indirectionTypeList
//                     // | typeQualifier [number | nothing] indirectionTypeList ignore arrays for now
//                     | typeQualifier ref
struct TokenList {
    static_assert(sizeof(Expression) == sizeof(Token));
    Expression exp;
    TokenList *next{nullptr};
    TokenList() = default;
    TokenList(const Token&);
    TokenList(const Token&, TokenList*);
    TokenList(const TokenList&) = default;
    TokenList& operator=(const TokenList&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    bool operator==(const TokenList&) const;
    TokenList deepCopy(NodeMemPool&);
};

// varDec:= simpleVarDec initialization
//                     | nothing
struct VariableDec {
    TokenList type;
    Token name;
    Expression *initialAssignment{nullptr};
    VariableDec() = delete;
    explicit VariableDec(const Token&);
    VariableDec(const VariableDec&) = default;
    VariableDec& operator=(const VariableDec&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    void prettyPrintDefinition(Tokenizer&, std::string&) const;
    VariableDec deepCopy(NodeMemPool&);
};
std::ostream& operator<<(std::ostream& os, const VariableDec& obj);


// statementList:= statement statementList | nothing
struct StatementList {
    Statement curr;
    StatementList *next{nullptr};
    StatementList() = default;
    StatementList(const StatementList &) = default;
    StatementList& operator=(const StatementList &) = default;
    StatementList deepCopy(NodeMemPool&);
};

// scope:= { statementList }
struct Scope {
    StatementList scopeStatements;
    Scope() = default;
    Scope(const Scope &) = default;
    Scope& operator=(const Scope &) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    Scope deepCopy(NodeMemPool&);
};

// EXPRESSIONS

// arrayAccess:= identifier [ expression ]
struct ArrayAccess {
    Expression offset;
    Expression array;
    ArrayAccess() = delete;
    explicit ArrayAccess(const Token&);
    ArrayAccess(const ArrayAccess&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    ArrayAccess *deepCopy(NodeMemPool&);
};

// binOp:= expression binOpOperator expression
struct BinOp {
    Expression rightSide;
    Expression leftSide;
    Token op;
    BinOp() = delete;
    explicit BinOp(const Token&);
    BinOp(const BinOp&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    BinOp *deepCopy(NodeMemPool&);
};

// unaryOp:= unaryOpOperator expression | expression postFixUnaryOpOperator
struct UnOp {
    Expression operand;
    private:
    // to make the memory layout the same as binOp
    Expression padding;
    public:
    Token op;
    UnOp() = delete;
    explicit UnOp(const Token&);
    UnOp(const UnOp&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    UnOp *deepCopy(NodeMemPool&);
};

// functionCall:= identifier(expressionList)
struct FunctionCall {
    ExpressionList args;
    Token name;
    FunctionCall() = delete;
    explicit FunctionCall(const Token &);
    FunctionCall(const FunctionCall&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    FunctionCall *deepCopy(NodeMemPool&);
};

// arrayLiteral:= [ expressionList ]
struct ContainerLiteral {
    ExpressionList values;
    uint32_t pos;
    ContainerLiteral() = default;
    ContainerLiteral(const ContainerLiteral&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
    ContainerLiteral *deepCopy(NodeMemPool&);
};

// CONDITIONAL STATEMENTS

// ifStatement:= if expression scope
struct BranchStatement {
    Scope body;
    Expression condition;
    BranchStatement() = default;
    BranchStatement(const BranchStatement&) = default;
    BranchStatement& operator=(const BranchStatement&) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    BranchStatement deepCopy(NodeMemPool&);
};

// elifStatementList:= elifStatement elifStatementList | nothing
struct ElifStatementList {
    BranchStatement elif;
    ElifStatementList *next{nullptr};
    ElifStatementList() = default;
    ElifStatementList(const ElifStatementList&) = default;
    ElifStatementList *deepCopy(NodeMemPool&);
};

// conditionalStatement:= ifStatement
//                       | ifStatement elifStatementList
//                       | ifStatement elifStatementList elseStatement
struct ConditionalStatement {
    BranchStatement ifStatement;
    ElifStatementList *elifStatement{nullptr};
    Scope *elseStatement{nullptr};
    ConditionalStatement() = default;
    ConditionalStatement(const ConditionalStatement&) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    ConditionalStatement *deepCopy(NodeMemPool&);
};

// returnStatement:= return expression;
struct ReturnStatement {
    Expression returnValue;
    Token token;
    ReturnStatement() = default;
    explicit ReturnStatement(const Token&);
    void prettyPrint(Tokenizer&, std::string&) const;
    ReturnStatement *deepCopy(NodeMemPool&);
};

struct SwitchScopeStatementList {
    Expression *caseExpression{nullptr};
    Scope *caseBody{nullptr};
    SwitchScopeStatementList *next{nullptr};
    SwitchScopeStatementList() = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    SwitchScopeStatementList deepCopy(NodeMemPool&);
};

// switchStatement:=  switch (identifier) switchScope
struct SwitchStatement {
    SwitchScopeStatementList body;
    Expression switched;
    SwitchStatement() = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    SwitchStatement *deepCopy(NodeMemPool&);
};

// LOOPS

// whileLoop:= while (expression) scope
struct WhileLoop {
    BranchStatement statement;
    WhileLoop() = default;
    WhileLoop(const WhileLoop&) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    WhileLoop *deepCopy(NodeMemPool&);
};

// forLoop:= for (expression | varDec | nothing ; expression | nothing; expression | nothing) scope
struct ForLoop {
    Statement initialize;
    BranchStatement statement;
    Expression iteration;
    ForLoop() = default;
    ForLoop(const ForLoop &) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    ForLoop *deepCopy(NodeMemPool&);
};

enum class ControlFlowStatementType: uint8_t {
    NONE,
    FOR_LOOP,
    WHILE_LOOP,
    CONDITIONAL_STATEMENT,
    RETURN_STATEMENT,
    EXIT_STATEMENT,
    SWITCH_STATEMENT,
};

// forLoop | whileLoop | conditionalStatement | returnStatement | switchStatement
struct ControlFlowStatement {
    union {
        ForLoop *forLoop;
        WhileLoop *whileLoop;
        ConditionalStatement *conditional;
        ReturnStatement *returnStatement;
        SwitchStatement *switchStatement;
    };
    ControlFlowStatementType type;
    ControlFlowStatement();

    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    ControlFlowStatement *deepCopy(NodeMemPool&);
};

// STATEMENTS

// DECLARATIONS

// functionDec:= func identifier (varDecList): typeList scope
struct FunctionDec {
    StatementList params;
    Scope body;
    TokenList returnType;
    Token name;
    FunctionDec() = default;
    explicit FunctionDec(const Token&);
    FunctionDec(const FunctionDec&) = default;
    FunctionDec& operator=(const FunctionDec&) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    void prettyPrintDefinition(Tokenizer&, std::string&) const;
    FunctionDec *deepCopy(NodeMemPool&);
};

enum class StructDecType: uint8_t {
    NONE,
    FUNC,
    VAR,
};

// structDecList:= varDec ; decList | functionDec decList | nothing
struct StructDecList {
    union {
        VariableDec *varDec;
        FunctionDec *funcDec;
    };
    StructDecList *next{nullptr};
    StructDecType type{StructDecType::NONE};
    StructDecList();
    StructDecList(const StructDecList&);
    StructDecList& operator=(const StructDecList&);
    StructDecList deepCopy(NodeMemPool&);
};

// structDec:= struct identifier { structDecList }
struct StructDec {
    StructDecList decs;
    Token name;
    bool checked{false};
    bool hasCycle{false};
    StructDec() = default;
    explicit StructDec(const Token&);
    StructDec(const StructDec&) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    void prettyPrintDefinition(Tokenizer&, std::string&) const;
    StructDec *deepCopy(NodeMemPool&);
};

// enumDec:= enum identifier { identifierList }
struct EnumDec {
    TokenList members;
    Token name;
    EnumDec() = delete;
    explicit EnumDec(const Token&);
    EnumDec(const EnumDec&) = default;
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
};


typedef struct GeneralDec GeneralDec;

// templateDec:= template [ identifierList ] structDec
//                                          | functionDec
struct TemplateDec {
    union {
        StructDec structDec;
        FunctionDec funcDec;
    };
    TokenList templateTypes;
    Token token;
    bool isStruct{false};
    TemplateDec();
    void prettyPrint(Tokenizer&, std::string&, uint32_t) const;
    void prettyPrintDefinition(Tokenizer&, std::string&) const;
    GeneralDec *deepCopy(NodeMemPool&, Token);
};

//templateCreation:= create identifier [ identifierList ] as identifier ;
struct TemplateCreation {
    Token templateName;
    TemplateDec *templateDec{nullptr};
    TokenList templateTypes;
    Token typeName;
    TemplateCreation() = default;
    TemplateCreation(const TemplateCreation&) = default;
    void prettyPrint(Tokenizer&, std::string&) const;
};

struct IncludeDec {
    Token file;
    IncludeDec() = default;
    void prettyPrint(Tokenizer&, std::string&) const;
};

struct BuiltinType {
    const Token name;
    StructDec structDec;
    BuiltinType() = delete;
    BuiltinType(const Token& name): name{name} {}
};

struct BuiltinFunc {
    FunctionDec funcDec;
    const Token name;
    BuiltinFunc() = delete;
    BuiltinFunc(const Token& name): name{name} {}
};

enum class GeneralDecType: uint8_t {
    NONE,
    STRUCT,
    VARIABLE,
    FUNCTION,
    ENUM,
    TEMPLATE,
    TEMPLATE_CREATE,
    INCLUDE_DEC,
    BUILTIN_FUNCTION,
};

// globalDec:= structDec | varDec ; | functionDec | enumDec | templateDec | templateCreation
struct GeneralDec {
    union {
        StructDec *structDec;
        VariableDec *varDec;
        FunctionDec *funcDec;
        EnumDec *enumDec;
        TemplateDec *tempDec;
        TemplateCreation *tempCreate;
        IncludeDec *includeDec;
        BuiltinType *builtinType;
        BuiltinFunc *builtinFunc;
    };
    uint32_t tokenizerIndex{0};
    GeneralDecType type{GeneralDecType::NONE};
    GeneralDec();
    void prettyPrint(std::vector<Tokenizer>&, std::string&) const;
    void prettyPrintDefinition(std::vector<Tokenizer>&, std::string&) const;
    GeneralDec *deepCopy(NodeMemPool&);
};

// globalDecList:= globalDec globalDecList | nothing
struct GeneralDecList {
    GeneralDec curr;
    GeneralDecList *next{nullptr};
    GeneralDecList() = default;
    void prettyPrint(std::vector<Tokenizer>&, std::string&) const;
    GeneralDec *deepCopy(NodeMemPool&);
};

// program:= globalDecList
struct Program {
    GeneralDecList decs;
    Program() = default;
    void prettyPrint(std::vector<Tokenizer>&, std::string&) const;
};

/**
 * builtin type TokenLists
 * Used in checker, compTime, and codeGen
*/
namespace TokenListTypes { 
    extern TokenList noneValue;
    extern TokenList badValue;
    extern TokenList boolValue;
    extern TokenList int32Value;
    extern TokenList uint32Value;
    extern TokenList int64Value;
    extern TokenList uint64Value;
    extern TokenList charValue;
    extern TokenList doubleValue;
    extern TokenList fileValue;
    extern TokenList nullptrValue;
    extern TokenList stringValue;
    extern TokenList voidValue;
};
