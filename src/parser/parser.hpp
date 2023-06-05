#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../token.hpp"
#include <vector>

enum class StatementType {
  BINARY_OP,
  UNARY_OP,
  CONSTANT,
  VARIABLE,
};

struct BinOp {
  TokenType op;
  std::unique_ptr<Statement> leftSide;
  std::unique_ptr<Statement> rightSide;
};

struct UnOp {
  TokenType op;
  std::unique_ptr<Statement> operand;
};

struct Constant {
  std::string type;
  std::string value;
};

struct Variable {
  std::string type;
  std::string name;
};

struct Statement {
  StatementType type;
  union {
    std::unique_ptr<Constant> c;
    std::unique_ptr<UnOp> uOp;
    std::unique_ptr<BinOp> bOp;
    std::unique_ptr<Variable> var;
  } statement;
};

struct FunctionDef {
  const char *name;
  std::vector<Variable> params;
  std::vector<Statement> body;
};

enum class DecType {
  FUNCTION,
  VARIABLE,
};

struct Declaration {
  DecType type;
  union {
    std::unique_ptr<FunctionDef> func;
  };
  Declaration(DecType type): type{type} {}
};

struct Program {
  std::string name;
  std::vector<Declaration> defs;
};

struct Parser {
  Program program;
  Tokenizer& tokenizer;
  Parser() = delete;
  Parser(Tokenizer& tokenizer);
  void parse();
};
