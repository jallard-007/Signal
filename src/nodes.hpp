#pragma once

#include "token.hpp"
#include <vector>

enum class StatementType {
  NONE,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  VARIABLE_DEC,
  FUNCTION_CALL,
  ARRAY_ACCESS,
};

struct Type {
  std::vector<Token> tokens;
  
  Type() = default;
};

struct VariableDec {
  Type type;
  Token name;

  VariableDec() = delete;
  explicit VariableDec(Token);
};

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;

struct Statement {
  StatementType type = StatementType::NONE;
  union {
    std::unique_ptr<UnOp> unOp;
    std::unique_ptr<BinOp> binOp;
    std::unique_ptr<VariableDec> varDec;
    std::unique_ptr<FunctionCall> funcCall;
    std::unique_ptr<ArrayAccess> arrAccess;
    Token var;
  };

  Statement();
  Statement(const Statement&) = delete;
  Statement(Statement&&) noexcept ;
  explicit Statement(std::unique_ptr<UnOp>);
  explicit Statement(std::unique_ptr<BinOp>);
  explicit Statement(std::unique_ptr<VariableDec>);
  explicit Statement(std::unique_ptr<FunctionCall>);
  explicit Statement(std::unique_ptr<ArrayAccess>);
  explicit Statement(Token);
  ~Statement();
  void operator=(Statement&&) noexcept;
};

struct Arguments {
  std::vector<Statement> list;
  Arguments() = default;
};

struct ArrayAccess {
  Arguments offset;
  Token array;

  ArrayAccess() = delete;
  explicit ArrayAccess(Token);
};

struct BinOp {
  TokenType op;
  std::unique_ptr<Statement> leftSide;
  std::unique_ptr<Statement> rightSide;

  explicit BinOp(TokenType);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&) noexcept;
};

struct UnOp {
  TokenType op;
  std::unique_ptr<Statement> operand;

  explicit UnOp(TokenType);
  UnOp(const UnOp&) = delete;
};

struct FunctionDec {
  std::vector<VariableDec> params;
  std::vector<Statement> body;
  Type returnType;
  Token name;

  FunctionDec() = delete;
  explicit FunctionDec(Token);
};

struct FunctionCall {
  Arguments args;
  Token name;
  FunctionCall() = delete;
  explicit FunctionCall(Token);
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
