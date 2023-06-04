#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include "../token.hpp"

struct Tokenizer {
  std::vector<Token> tokens;
  const char* content;
  const uint32_t size;
  uint32_t position;

  Tokenizer() = delete;

  Tokenizer(const std::string&);

  std::vector<Token>& tokenizeAll();
  Token tokenizeNext();

  void moveToNextNonWhiteSpaceChar();
  void movePastKeywordOrIdentifier();
  void movePastNumber();
  void movePastLiteral(char delimeter);
};
