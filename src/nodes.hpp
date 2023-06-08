#pragma once

#include "token.hpp"
#include <vector>
#include <memory>

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  TOKEN,
};

struct Unexpected {
  Token token;
  uint32_t line;
  uint32_t column;
  Unexpected() = delete;
  Unexpected(Token, uint32_t, uint32_t);
};

struct Expected {
  uint32_t line;
  uint32_t column;
  TokenType tokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, uint32_t, uint32_t);
  Expected(ExpectedType, uint32_t, uint32_t, TokenType);
};

enum class StatementType {
  NONE,
  BAD,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  VARIABLE_DEC,
  FUNCTION_CALL,
  ARRAY_ACCESS,
  WRAPPED_VALUE,
  SCOPE,
};

bool hasData(StatementType);

struct Type {
  std::vector<Token> tokens;
  
  Type() = default;
  bool operator==(const Type&) const;
};

struct VariableDec {
  Type type;
  Token name;

  VariableDec() = delete;
  explicit VariableDec(Token);
  bool operator==(const VariableDec&) const;
};

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct Scope Scope;

struct Statement {
  union {
    std::unique_ptr<UnOp> unOp;
    std::unique_ptr<BinOp> binOp;
    std::unique_ptr<VariableDec> varDec;
    std::unique_ptr<FunctionCall> funcCall;
    std::unique_ptr<ArrayAccess> arrAccess;
    std::unique_ptr<Statement> wrapped;
    std::unique_ptr<Scope> scope;
    Token var;
  };
  StatementType type = StatementType::NONE;

  Statement();
  Statement(StatementType);
  Statement(const Statement&) = delete;
  Statement(Statement&&) noexcept ;
  explicit Statement(std::unique_ptr<UnOp>);
  explicit Statement(std::unique_ptr<BinOp>);
  explicit Statement(std::unique_ptr<VariableDec>);
  explicit Statement(std::unique_ptr<FunctionCall>);
  explicit Statement(std::unique_ptr<ArrayAccess>);
  explicit Statement(std::unique_ptr<Statement>);
  explicit Statement(std::unique_ptr<Scope>);
  explicit Statement(Token);
  ~Statement();
  void operator=(Statement&&) noexcept;
  bool operator==(const Statement&) const;
  Statement* addStatementToNode(Statement&&);
  std::unique_ptr<Statement>* getChild();
  ExpectedType isValid() const;
};

struct Scope {
  std::vector<Statement> scopeStatements;
  Scope() = default;
  bool operator==(const Scope&) const;
};

struct ArrayAccess {
  Statement offset;
  Token array;

  ArrayAccess() = delete;
  explicit ArrayAccess(Token);
  bool operator==(const ArrayAccess&) const;
};

struct BinOp {
  std::unique_ptr<Statement> leftSide;
  std::unique_ptr<Statement> rightSide;
  TokenType op;
  explicit BinOp(TokenType);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&) noexcept;
  bool operator==(const BinOp&) const;
};

struct UnOp {
  std::unique_ptr<Statement> operand;
  TokenType op;
  explicit UnOp(TokenType);
  UnOp(const UnOp&) = delete;
  bool operator==(const UnOp&) const;
};

struct FunctionDec {
  std::vector<Statement> params;
  std::vector<Statement> bodyStatements;
  Type returnType;
  Token name;

  FunctionDec() = delete;
  explicit FunctionDec(Token);
};

struct FunctionCall {
  std::vector<Statement> args;
  Token name;
  FunctionCall() = delete;
  explicit FunctionCall(Token);
  bool operator==(const FunctionCall&) const;
};

typedef struct Declaration Declaration;

struct Struct {
  std::vector<Declaration> decs;
  Token name;
  Struct(Token);
};

struct Template {
  union {
    std::unique_ptr<FunctionDec> func;
    std::unique_ptr<Struct> struc;
  };
  Token name;
  Template(Token);
};

enum class DecType {
  NONE,
  FUNCTION,
  VARIABLE,
  TEMPLATE,
  STRUCT
};

struct Declaration {
  DecType decType;
  union{
    std::unique_ptr<FunctionDec> func;
    std::unique_ptr<VariableDec> var;
    std::unique_ptr<Template> temp;
    std::unique_ptr<Struct> struc;
  };

  Declaration();
  Declaration(Declaration&&) noexcept;
  explicit Declaration(std::unique_ptr<FunctionDec>);
  ~Declaration();
};

struct Program {
  std::string name;
  std::vector<Declaration> decs;
  Program() = default;
  Program(Program&&) noexcept;
};
