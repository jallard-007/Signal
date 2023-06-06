#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../token.hpp"
#include "../nodes.hpp"

bool isStatementDelimiter(TokenType);

struct Parser {
  Program program;
  Tokenizer& tokenizer;
  Parser() = delete;
  explicit Parser(Tokenizer& tokenizer);
  void parse();
  bool functionDec();
  Statement parseStatement(TokenType);
  bool getParams(std::vector<VariableDec>&);
  bool getStatements(std::vector<Statement>&, TokenType);
  Token getType(Type&);
};
