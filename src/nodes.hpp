#pragma once

#include "tokenizer/tokenizer.hpp"

typedef class NodeMemPool NodeMemPool;

bool notFirstOfExpression(TokenType);

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct ArrayOrStructLiteral ArrayOrStructLiteral;

enum class ExpressionType: uint8_t {
  NONE,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  FUNCTION_CALL,
  ARRAY_ACCESS,
  // ARRAY_LITERAL,
  // STRUCT_LITERAL,
};

struct Expression {
  private:
  union {
    uint64_t type;
    BinOp *binOp;
    UnOp *unOp;
    Token value;
    FunctionCall *funcCall;
    ArrayAccess *arrAccess;
    ArrayOrStructLiteral *arrayOrStruct;
  };
  public:
  Expression();
  explicit Expression(Token);
  Expression(const Expression&);
  Expression& operator=(const Expression&);
  void prettyPrint(Tokenizer&, std::string&);
  Expression deepCopy(NodeMemPool&);
  #define EXPRESSION_MASK 0x7
  inline BinOp* getBinOp() const { return (BinOp *)((uint64_t)binOp & ~EXPRESSION_MASK); } 
  inline UnOp* getUnOp() const { return (UnOp *)((uint64_t)unOp & ~EXPRESSION_MASK); }
  inline Token getToken() const { return value; }
  inline FunctionCall* getFunctionCall() const { return (FunctionCall *)((uint64_t)funcCall & ~EXPRESSION_MASK); }
  inline ArrayAccess* getArrayAccess() const { return (ArrayAccess *)((uint64_t)arrAccess & ~EXPRESSION_MASK); }
  inline ArrayOrStructLiteral* getArrayOrStructLiteral() const { return (ArrayOrStructLiteral *)((uint64_t)arrayOrStruct & ~EXPRESSION_MASK); }
  inline ExpressionType getType() const { return (ExpressionType)(type & EXPRESSION_MASK); }

  #define SET_EXP binOp = (BinOp *)((type & EXPRESSION_MASK) | ((uint64_t)ref & ~EXPRESSION_MASK))
  inline void setBinOp(BinOp *ref) { SET_EXP; }
  inline void setUnOp(UnOp *ref) { SET_EXP; }
  inline void setToken(Token ref) { value = ref; }
  inline void setFunctionCall(FunctionCall *ref) { SET_EXP; }
  inline void setArrayAccess(ArrayAccess *ref) { SET_EXP; }
  inline void setArrayOrStructLiteral(ArrayOrStructLiteral *ref) { SET_EXP; }
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  Statement deepCopy(NodeMemPool&);

};

// typeList: typeQualifier identifier indirectionTypeList
// indirectionTypeList:= nothing
//                     | typeQualifier ptr indirectionTypeList
//                     // | typeQualifier [number | nothing] indirectionTypeList ignore arrays for now
//                     | typeQualifier ref
struct TokenList {
  Token token{0,0,TokenType::NONE};
  TokenList *next{nullptr};
  TokenList() = default;
  TokenList(const Token&);
  TokenList(const Token&, TokenList*);
  TokenList(const TokenList&) = default;
  TokenList& operator=(const TokenList&) = default;
  void prettyPrint(Tokenizer&, std::string&);
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
  void prettyPrint(Tokenizer&, std::string&);
  void prettyPrintDefinition(Tokenizer&, std::string&);
  VariableDec deepCopy(NodeMemPool&);
};

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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
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
  void prettyPrint(Tokenizer&, std::string&);
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
  void prettyPrint(Tokenizer&, std::string&);
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
  void prettyPrint(Tokenizer&, std::string&);
  UnOp *deepCopy(NodeMemPool&);
};

// functionCall:= identifier(expressionList)
struct FunctionCall {
  ExpressionList args;
  Token name;
  FunctionCall() = delete;
  explicit FunctionCall(const Token &);
  FunctionCall(const FunctionCall&) = default;
  void prettyPrint(Tokenizer&, std::string&);
  FunctionCall *deepCopy(NodeMemPool&);
};

// arrayLiteral:= [ expressionList ]
// structLiteral:= { expressionList }
struct ArrayOrStructLiteral {
  ExpressionList values;
  ArrayOrStructLiteral() = default;
  ArrayOrStructLiteral(const ArrayOrStructLiteral&) = default;
  void prettyPrint(Tokenizer&, std::string&);
  ArrayOrStructLiteral *deepCopy(NodeMemPool&);
};

// CONDITIONAL STATEMENTS

// ifStatement:= if expression scope
struct BranchStatement {
  Scope body;
  Expression condition;
  BranchStatement() = default;
  BranchStatement(const BranchStatement&) = default;
  BranchStatement& operator=(const BranchStatement&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  ConditionalStatement *deepCopy(NodeMemPool&);
};

// returnStatement:= return expression;
struct ReturnStatement {
  Expression returnValue;
  Token token;
  ReturnStatement() = default;
  explicit ReturnStatement(const Token&);
  void prettyPrint(Tokenizer&, std::string&);
  ReturnStatement *deepCopy(NodeMemPool&);
};

struct SwitchScopeStatementList {
  Expression *caseExpression{nullptr};
  Scope *caseBody{nullptr};
  SwitchScopeStatementList *next{nullptr};
  SwitchScopeStatementList() = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  SwitchScopeStatementList deepCopy(NodeMemPool&);
};

// switchStatement:=  switch (identifier) switchScope
struct SwitchStatement {
  SwitchScopeStatementList body;
  Expression switched;
  SwitchStatement() = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  SwitchStatement *deepCopy(NodeMemPool&);
};

// LOOPS

// whileLoop:= while (expression) scope
struct WhileLoop {
  BranchStatement statement;
  WhileLoop() = default;
  WhileLoop(const WhileLoop&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  WhileLoop *deepCopy(NodeMemPool&);
};

// forLoop:= for (expression | varDec | nothing ; expression | nothing; expression | nothing) scope
struct ForLoop {
  Statement initialize;
  BranchStatement statement;
  Expression iteration;
  ForLoop() = default;
  ForLoop(const ForLoop &) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
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

  void prettyPrint(Tokenizer&, std::string&, uint32_t);
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  void prettyPrintDefinition(Tokenizer&, std::string&);
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  void prettyPrintDefinition(Tokenizer&, std::string&);
  StructDec *deepCopy(NodeMemPool&);
};

// enumDec:= enum identifier { identifierList }
struct EnumDec {
  TokenList members;
  Token name;
  EnumDec() = delete;
  explicit EnumDec(const Token&);
  EnumDec(const EnumDec&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
  void prettyPrintDefinition(Tokenizer&, std::string&);
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
  void prettyPrint(Tokenizer&, std::string&);
};

struct IncludeDec {
  Token file;
  IncludeDec() = default;
  void prettyPrint(Tokenizer&, std::string&);
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
  };
  uint32_t tokenizerIndex{0};
  GeneralDecType type{GeneralDecType::NONE};
  GeneralDec();
  void prettyPrint(std::vector<Tokenizer>&, std::string&);
  void prettyPrintDefinition(std::vector<Tokenizer>&, std::string&);
  GeneralDec *deepCopy(NodeMemPool&);
};

// globalDecList:= globalDec globalDecList | nothing
struct GeneralDecList {
  GeneralDec curr;
  GeneralDecList *next{nullptr};
  GeneralDecList() = default;
  void prettyPrint(std::vector<Tokenizer>&, std::string&);
  GeneralDec *deepCopy(NodeMemPool&);
};

// program:= globalDecList
struct Program {
  GeneralDecList decs;
  Program() = default;
  void prettyPrint(std::vector<Tokenizer>&, std::string&);
};

