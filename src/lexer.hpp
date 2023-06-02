#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include "token.hpp"

struct Lexer {
  std::vector<Token> tokens;
  const char* content;
  const uint32_t size;
  uint32_t position;

  Lexer() = delete;

  Lexer(const std::string&);

  std::vector<Token>& tokenize();

  void moveToNextNonWhiteSpaceChar();
  void movePastKeywordOrIdentifier();
  void movePastNumber();
};