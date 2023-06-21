#pragma once

#include <vector>
#include <unordered_map>
#include "../token.hpp"

struct TokenPositionInfo{
  uint32_t lineNum;
  uint32_t linePos;
  TokenPositionInfo() = delete;
  TokenPositionInfo(uint32_t, uint32_t);
};

struct Tokenizer {
  std::vector<Token> badTokens;
  std::vector<uint32_t> newlinePositions;
  const std::string& filePath;
  const std::string& content;
  Token peeked;
  Token error;
  const uint32_t size;
  uint32_t position;
  TokenType prevType;

  Tokenizer() = delete;

  explicit Tokenizer(const std::string&, const std::string&);

  std::vector<Token> tokenizeAll();
  Token tokenizeNext();
  Token peekNext();
  void consumePeek();

  void moveToNextNonWhiteSpaceChar();
  void movePastIdentifier();
  void movePastNumber();
  void movePastHexNumber();
  bool movePastLiteral(char);
  void moveToNewLine();
  std::string extractToken(Token&);
  TokenPositionInfo getTokenPositionInfo(const Token&);
};
