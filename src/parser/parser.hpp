#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../token.hpp"
#include "../nodes.hpp"

struct Parser {
  Program program;
  Tokenizer& tokenizer;
  Parser() = delete;
  Parser(Tokenizer& tokenizer);
  void parse();
  bool getParams(std::vector<Variable>&);
  TokenType getType(Type&);
};
