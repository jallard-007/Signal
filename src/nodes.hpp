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

struct Constant {
  Type type;
  Token value;
  
  Constant() = delete;
  Constant(Token);
};

struct VariableDec {
  Type type;
  Token name;

  VariableDec() = delete;
  VariableDec(Token);
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
  Statement(Statement&&);
  Statement(std::unique_ptr<UnOp>);
  Statement(std::unique_ptr<BinOp>);
  Statement(std::unique_ptr<VariableDec>);
  Statement(std::unique_ptr<FunctionCall>);
  Statement(std::unique_ptr<ArrayAccess>);
  Statement(Token);
  ~Statement();
  void operator=(Statement&&);
};

struct Arguments {
  std::vector<Statement> list;
  Arguments() = default;
};

struct ArrayAccess {
  Arguments offset;
  Token array;

  ArrayAccess() = delete;
  ArrayAccess(Token);
};

struct BinOp {
  TokenType op;
  std::unique_ptr<Statement> leftSide;
  std::unique_ptr<Statement> rightSide;

  BinOp(TokenType);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&);
};

struct UnOp {
  TokenType op;
  std::unique_ptr<Statement> operand;

  UnOp(TokenType);
  UnOp(const UnOp&) = delete;
};

struct FunctionDec {
  std::vector<VariableDec> params;
  std::vector<Statement> body;
  Type returnType;
  Token name;

  FunctionDec() = delete;
  FunctionDec(Token);
};

struct FunctionCall {
  Arguments args;
  Token name;
  FunctionCall() = delete;
  FunctionCall(Token);
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
  Declaration(Declaration&&);
  Declaration(std::unique_ptr<FunctionDec>);
  ~Declaration();
};

struct Program {
  std::string name;
  std::vector<Declaration> decs;
  Program() = default;
  Program(Program&&);
};
