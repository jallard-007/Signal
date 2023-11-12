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
  uint32_t position{0};
  uint32_t tokenizerIndex{0};
  TokenType prevType{TokenType::NONE};

  Tokenizer() = delete;

  explicit Tokenizer(std::string&&, std::string&&);
  explicit Tokenizer(std::string&&, const std::string&);

  void tokenizeAll(std::vector<Token>&);
  Token tokenizeNext();
  Token peekNext();
  void consumePeek();
  std::string extractToken(const Token&);
  TokenPositionInfo getTokenPositionInfo(const Token&);

private:
  void moveToNextNonWhiteSpaceChar();
  void movePastIdentifier();
  void movePastNumber();
  void movePastHexNumber();
  bool movePastLiteral(char);
  void movePastNewLine();
};
