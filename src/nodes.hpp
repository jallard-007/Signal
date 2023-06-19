#pragma once

#include "tokenizer/tokenizer.hpp"

bool notFirstOfExpression(TokenType);

struct Unexpected {
  Token token;
  Unexpected() = delete;
  explicit Unexpected(const Token&);
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  TOKEN,
  FUNCTION_OR_STRUCT_DEC,
  OPERATOR_OR_CLOSE_BRACKET,
  OPERATOR_OR_CLOSE_PAREN,
  OPERATOR_OR_CLOSE_PAREN_OR_COMMA,
  OPERATOR_OR_SEMICOLON,
};

struct Expected {
  Token tokenWhereExpected;
  TokenType expectedTokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, const Token&);
  Expected(ExpectedType, const Token&, TokenType);
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct ArrayOrStructLiteral ArrayOrStructLiteral;

enum class ExpressionType {
  NONE,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  FUNCTION_CALL,
  ARRAY_ACCESS,
  WRAPPED,
  ARRAY_OR_STRUCT_LITERAL
};

struct Expression {
  union {
    BinOp *binOp;
    UnOp *unOp;
    Token *value;
    FunctionCall *funcCall;
    ArrayAccess *arrAccess;
    Expression *wrapped;
    ArrayOrStructLiteral *arrayOrStruct;
  };
  ExpressionType type;
  Expression();
  explicit Expression(Token *);
  Expression(const Expression&);
  void operator=(const Expression&);
  void prettyPrint(Tokenizer&, std::string&);
};

struct ExpressionList {
  Expression curr;
  ExpressionList *next;
  ExpressionList();
  ExpressionList(const ExpressionList&) = default;
  void prettyPrint(Tokenizer&, std::string&);
};

typedef struct ControlFlowStatement ControlFlowStatement;
typedef struct Scope Scope;
typedef struct VariableDec VariableDec;

enum class StatementType {
  NOTHING,
  EXPRESSION,
  CONTROL_FLOW,
  SCOPE,
  VARIABLE_DEC
};

// statement:=  expression; | controlFlowStatement | scope | varDec | nothing
struct Statement {
  union {
    Expression *expression;
    ControlFlowStatement *controlFlow;
    Scope *scope;
    VariableDec *varDec;
  };
  StatementType type;
  Statement();
  explicit Statement(Expression *);
  explicit Statement(ControlFlowStatement *);
  explicit Statement(Scope *);
  explicit Statement(VariableDec *);
  Statement(const Statement&);
  void operator=(const Statement&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// typeList: typeQualifier identifier indirectionTypeList
// indirectionTypeList:= nothing
//                     | typeQualifier ptr indirectionTypeList
//                     // | typeQualifier [number | nothing] indirectionTypeList ignore arrays for now
//                     | typeQualifier ref
struct TokenList {
  Token token{0,0,TokenType::NOTHING};
  TokenList *next{nullptr};
  TokenList() = default;
  TokenList(const Token&);
  TokenList(const TokenList&) = default;
  void operator=(const TokenList&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// varDec:= simpleVarDec initialization
//                     | nothing
struct VariableDec {
  Token name;
  TokenList type;
  Expression *initialAssignment;
  VariableDec() = delete;
  explicit VariableDec(const Token&);
  VariableDec(const VariableDec&) = default;
  VariableDec& operator=(const VariableDec&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// statementList:= statement statementList | nothing
struct StatementList {
  Statement curr{};
  StatementList *next{nullptr};
  StatementList() = default;
  StatementList(const StatementList &) = default;
  void operator=(const StatementList &);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// scope:= { statementList }
struct Scope {
  StatementList scopeStatements{};
  Scope() = default;
  Scope(const Scope &) = default;
  void operator=(const Scope &);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// EXPRESSIONS

// arrayAccess:= identifier [ expression ]
struct ArrayAccess {
  Token array;
  Expression offset;
  ArrayAccess() = delete;
  explicit ArrayAccess(const Token&);
  ArrayAccess(const ArrayAccess&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// binOp:= expression binOpOperator expression
struct BinOp {
  Token op;
  Expression leftSide{};
  Expression rightSide{};
  BinOp() = delete;
  explicit BinOp(const Token&);
  BinOp(const BinOp&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// unaryOp:= unaryOpOperator expression | expression postFixUnaryOpOperator
struct UnOp {
  Token op;
  Expression operand{};
  UnOp() = delete;
  explicit UnOp(const Token&);
  UnOp(const UnOp&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// functionCall:= identifier(expressionList)
struct FunctionCall {
  Token name;
  ExpressionList args;
  FunctionCall() = delete;
  explicit FunctionCall(const Token &);
  FunctionCall(const FunctionCall&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// arrayOrStructLiteral:= [ expressionList ]
struct ArrayOrStructLiteral {
  ExpressionList values;
  ArrayOrStructLiteral() = default;
  ArrayOrStructLiteral(const ArrayOrStructLiteral&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// CONDITIONAL STATEMENTS

// ifStatement:= if (expression) scope
struct IfStatement {
  Expression condition;
  Scope body;
  IfStatement() = default;
  IfStatement(const IfStatement&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// elifStatementList:= elifStatement elifStatementList | nothing
struct ElifStatementList {
  IfStatement elif;
  ElifStatementList *next{nullptr};
  ElifStatementList() = default;
  ElifStatementList(const ElifStatementList&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// conditionalStatement:= ifStatement
//                       | ifStatement elifStatementList
//                       | ifStatement elifStatementList elseStatement
struct ConditionalStatement {
  IfStatement ifStatement;
  ElifStatementList *elifStatement{nullptr};
  Scope *elseStatement{nullptr};
  ConditionalStatement(const ConditionalStatement&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// returnStatement:= return expression;
struct ReturnStatement {
  Token token;
  Expression returnValue;
  ReturnStatement() = delete;
  explicit ReturnStatement(const Token&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct SwitchScope {
  int i;
};

// switchStatement:=  switch (identifier) switchScope
struct SwitchStatement {
  Token token;
  Token switched;
  Scope body;
  SwitchStatement() = delete;
  explicit SwitchStatement(const Token&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// LOOPS

// whileLoop:= while (expression) scope
struct WhileLoop {
  IfStatement statement{};
  WhileLoop() = default;
  WhileLoop(const WhileLoop&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// forLoop:= for (expression | varDec | nothing ; expression | nothing; expression | nothing) scope
struct ForLoop {
  Statement initialize{};
  Expression condition{};
  Expression iteration{};
  Scope body;
  ForLoop() = default;
  ForLoop(const ForLoop &);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

enum class ControlFlowStatementType {
  NONE,
  FOR_LOOP,
  WHILE_LOOP,
  CONDITIONAL_STATEMENT,
  RETURN_STATEMENT,
  SWITCH_STATEMENT,
};

// forLoop | whileLoop | conditionalStatement | returnStatement | switchStatement
struct ControlFlowStatement {
  union {
    ForLoop forLoop;
    WhileLoop whileLoop;
    ConditionalStatement conditional;
    ReturnStatement returnStatement;
    SwitchStatement switchStatement;
  };
  ControlFlowStatementType type;
  ControlFlowStatement();
  ControlFlowStatement(const ForLoop& val);
  ControlFlowStatement(const WhileLoop& val);
  ControlFlowStatement(const ConditionalStatement& val);
  ControlFlowStatement(const ReturnStatement& val);
  ControlFlowStatement(const SwitchStatement& val);

  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// STATEMENTS

// DECLARATIONS

// functionDec:= func identifier (varDecList): typeList scope
struct FunctionDec {
  Token name{0,0,TokenType::NOTHING};
  StatementList params{};
  TokenList returnType{};
  Scope body{};
  FunctionDec() = default;
  explicit FunctionDec(const Token&);
  FunctionDec(const FunctionDec&) = default;
  void operator=(const FunctionDec&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

enum class StructDecType: uint8_t {
  NONE,
  FUNC,
  VAR,
};

// structDecList:= varDec ; decList | functionDec decList | nothing
struct StructDecList {
  union {
    VariableDec varDec;
    FunctionDec funcDec;
  };
  StructDecList *next;
  StructDecType type;
  StructDecList();
  StructDecList(const StructDecList&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// structDec:= struct identifier { structDecList }
struct StructDec {
  Token token;
  StructDecList decs;
  StructDec() = delete;
  explicit StructDec(const Token&);
  StructDec(const StructDec&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// enumDec:= enum identifier { identifierList }
struct EnumDec {
  Token token;
  TokenList members;
  EnumDec() = delete;
  explicit EnumDec(const Token&);
  EnumDec(const EnumDec&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// templateDec:= template [ identifierList ] structDec
//                                          | functionDec
struct TemplateDec {
  union {
    StructDec structDec;
    FunctionDec funcDec;
  };
  TokenList templateTypes{};
  Token token;
  bool isStruct;
  TemplateDec();
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

//templateCreation:= create identifier [ identifierList ] as identifier ;
struct TemplateCreation {
  Token token;
  TemplateDec *templateDec{nullptr};
  TokenList templateTypes{};
  Token identifier;
  TemplateCreation() = delete;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

enum class GlobalDecType {
  NOTHING,
  STRUCT,
  VARIABLE,
  FUNCTION,
  ENUM,
  TEMPLATE,
  TEMPLATE_CREATE,
};

// globalDec:= structDec | varDec ; | functionDec | enumDec | templateDec | templateCreation
struct GlobalDec {
  union {
    StructDec structDec;
    VariableDec varDec;
    FunctionDec funcDec;
    EnumDec enumDec;
    TemplateDec tempDec;
    TemplateCreation tempCreate;
  };
  GlobalDecType type{GlobalDecType::NOTHING};
  GlobalDec();
};

// globalDecList:= globalDec globalDecList | nothing
struct GlobalDecList {
  GlobalDec curr{};
  GlobalDecList *next{nullptr};
  GlobalDecList() = default;
};

// program:= globalDecList
struct Program {
  GlobalDecList decs{};
  Program() = default;
};
