#pragma once

#include "token.hpp"
#include <vector>
#include <memory>

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  END_OF_STATEMENT,
  SYMBOL
};

struct Expected {
  uint32_t position;
  TokenType tokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, uint32_t);
  Expected(ExpectedType, uint32_t, TokenType);
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

struct Statement {
  union {
    std::unique_ptr<UnOp> unOp;
    std::unique_ptr<BinOp> binOp;
    std::unique_ptr<VariableDec> varDec;
    std::unique_ptr<FunctionCall> funcCall;
    std::unique_ptr<ArrayAccess> arrAccess;
    std::unique_ptr<Statement> wrapped;
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
  explicit Statement(Token);
  ~Statement();
  void operator=(Statement&&) noexcept;
  bool operator==(const Statement&) const;
  Statement* addStatementToNode(Statement&&);
  std::unique_ptr<Statement>* getChild();
  ExpectedType isValid() const;
};

struct ArrayAccess {
  Statement offset;
  Token array;

  ArrayAccess() = delete;
  explicit ArrayAccess(Token);
  bool operator==(const ArrayAccess&) const;
};

struct BinOp {
  TokenType op;
  std::unique_ptr<Statement> leftSide;
  std::unique_ptr<Statement> rightSide;

  explicit BinOp(TokenType);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&) noexcept;
  bool operator==(const BinOp&) const;
};

struct UnOp {
  TokenType op;
  std::unique_ptr<Statement> operand;

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

enum class DecType {
  FUNCTION,
  VARIABLE,
  NONE
};

struct Declaration {
  DecType decType;
  union{
    std::unique_ptr<FunctionDec> func;
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
