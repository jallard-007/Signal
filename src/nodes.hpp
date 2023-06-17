#pragma once

#include "tokenizer/tokenizer.hpp"

bool notFirstOfExpression(TokenType);

struct Unexpected {
  Token token;
  Unexpected() = delete;
  Unexpected(const Token&);
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  TOKEN,
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

enum class ExpressionType {
  NONE,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  FUNCTION_CALL,
  ARRAY_ACCESS,
  WRAPPED,
};

struct Expression {
  union {
    BinOp *binOp;
    UnOp *unOp;
    Token *value;
    FunctionCall *funcCall;
    ArrayAccess *arrAccess;
    Expression *wrapped;
  };
  ExpressionType type;
  Expression();
  Expression(BinOp *);
  Expression(UnOp *);
  Expression(Token *);
  Expression(FunctionCall *);
  Expression(ArrayAccess *);
  Expression(Expression *);
  Expression(const Expression&);
  Expression(Expression&&);
  void operator=(Expression&&);
  void swap(Expression&);
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// typeList: typeQualifier identifier indirectionTypeList
// indirectionTypeList:= nothing
//                     | typeQualifier ptr indirectionTypeList
//                     // | typeQualifier [number | nothing] indirectionTypeList ignore arrays for now
//                     | typeQualifier ref
struct TokenList {
  Token token;
  TokenList *next;
  TokenList();
  TokenList(const Token&);
  TokenList(const TokenList&) = default;
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
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// statementList:= statement statementList | nothing
struct StatementList {
  Statement curr;
  StatementList *next;
  StatementList();
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// scope:= { statementList }
struct Scope {
  StatementList scopeStatements;
  Scope() = default;
  Scope(const Scope &) = default;
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
  Expression leftSide;
  Expression rightSide;
  explicit BinOp(const Token&);
  BinOp(const BinOp&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// unaryOp:= unaryOpOperator expression | expression postFixUnaryOpOperator
struct UnOp {
  Token op;
  Expression operand;
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
  ElifStatementList *next;
  ElifStatementList() = default;
  ElifStatementList(const ElifStatementList&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// conditionalStatement:= ifStatement
//                       | ifStatement elifStatementList
//                       | ifStatement elifStatementList elseStatement
struct ConditionalStatement {
  IfStatement ifStatement;
  ElifStatementList *elifStatement;
  Scope *elseStatement;
  ConditionalStatement(const Token&);
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
  IfStatement statement;
  WhileLoop() = default;
  WhileLoop(const WhileLoop&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// forLoop:= for (expression | varDec | nothing ; expression | nothing; expression | nothing) scope
struct ForLoop {
  Statement initialize;
  Expression condition;
  Expression iteration;
  Scope body;
  ForLoop() = delete;
  explicit ForLoop(const Token&);
  ForLoop(ForLoop &&);
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
  ControlFlowStatement(ForLoop&& val);
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
  Token name;
  StatementList params;
  TokenList returnType;
  Scope body;
  FunctionDec() = delete;
  explicit FunctionDec(const Token&);
  FunctionDec(const FunctionDec&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// initialization:=  = expression
//                   | arrayOrStructLiteral
struct Initialization {
  union {
    Expression expression;
    ArrayOrStructLiteral arrOrStruct;
  };
  bool isExpression;
  Initialization();
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// varDecList:= varDec varDecTail
// varDecTail:= , varDec varDecTail | nothing
struct VarDecList {
  VariableDec curr;
  VarDecList * next;
  VarDecList() = delete;
  VarDecList(const Token&);
  VarDecList(const VarDecList&) = default;
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

// structDecList:= varDec ; decList | functionDec decList | nothing
struct StructDecList {
  union {
    VariableDec varDec;
    FunctionDec funcDec;
  };
  
  StructDecList *next;
  bool isVarDec;
  StructDecList();
  StructDecList(const StructDecList&);
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

// identifierList:= identifier identifierTail
//                 | nothing
// identifierTail:= , identifier identifierTail
struct IdentifierList {
  Token token;
  IdentifierList *next;
  IdentifierList();
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

// templateDec:= template [ identifierList ] structDec
//                                          | functionDec
struct TemplateDec {
  Token token;
  IdentifierList templateTypes;
  union {
    StructDec structDec;
    FunctionDec funcDec;
  };
  bool isStruct;
  TemplateDec();
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

//templateCreation:= create identifier [ identifierList ] as identifier ;
struct TemplateCreation {
  Token token;
  TemplateDec * templateDec;
  IdentifierList templateTypes;
  Token identifier;
  TemplateCreation() = delete;
  TemplateCreation(const Token &);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

enum class GlobalDecType {
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
  GlobalDecType type;
  GlobalDec();
  GlobalDec();
  GlobalDec();
  GlobalDec();
  GlobalDec();
};

// globalDecList:= globalDec globalDecList | nothing
struct GlobalDecList {
  GlobalDec curr;
  GlobalDecList *next;
  GlobalDecList();
};

// program:= globalDecList
struct Program {
  GlobalDecList decs;
  Program() = default;
};
