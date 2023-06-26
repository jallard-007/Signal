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
  std::vector<uint32_t> newlinePositions;
  const std::string filePath;
  const std::string content;
  Token peeked;
  const uint32_t size;
  uint32_t position{0};
  uint32_t tokenizerIndex{0};
  TokenType prevType{TokenType::NOTHING};

  Tokenizer() = delete;

  explicit Tokenizer(std::string&&, std::string&&);
  explicit Tokenizer(std::string&&, const std::string&);

  void tokenizeAll(std::vector<Token>&);
  Token tokenizeNext();
  Token peekNext();
  void consumePeek();

  void moveToNextNonWhiteSpaceChar();
  void movePastIdentifier();
  void movePastNumber();
  void movePastHexNumber();
  bool movePastLiteral(char);
  void movePastNewLine();
  std::string extractToken(const Token&);
  TokenPositionInfo getTokenPositionInfo(const Token&);
};
