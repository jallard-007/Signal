#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include "../token.hpp"

struct Tokenizer {
  const std::string& content;
  Token peeked;
  const uint32_t size;
  uint32_t position;
  TokenType prevType;

  Tokenizer() = delete;

  Tokenizer(const std::string&);

  std::vector<Token> tokenizeAll();
  Token tokenizeNext();
  Token peekNext();
  
  void moveToNextNonWhiteSpaceChar();
  void movePastKeywordOrIdentifier();
  void movePastNumber();
  void movePastHexNumber();
  void movePastLiteral(char);
  void moveToNewLine();
  std::string extractToken(Token);
};
