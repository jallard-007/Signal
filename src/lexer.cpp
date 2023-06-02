#include <vector>
#include <string>
#include <cctype>
#include "token.hpp"
#include "lexer.hpp"

bool my_isalpha(char ch) {
    return std::isalpha(static_cast<unsigned char>(ch));
}

bool my_isalnum(char ch) {
    return std::isalnum(static_cast<unsigned char>(ch));
}

Lexer::Lexer(const std::string& fs): content{fs.data()}, size{(uint32_t)fs.length()}  {
  if (fs.length() > UINT32_MAX) {
    exit(1);
  }
}

std::vector<Token>& Lexer::tokenize() {
  while (position < size) {
    char c = content[position];
    if (charToType.find(c) != charToType.end()) {
      tokens.emplace_back(position, charToType.at(c));
      ++position;
      moveToNextNonWhiteSpaceChar();
      continue;
    }

    if (c >= '0' && c <= '9') {
      if (position + 1 < size) {
        c = content[position + 1];
        if (c >= '0' && c <= '9') {
          tokens.emplace_back(position, TokenType::DECIMAL_NUMBER);
        } else if (c == 'b') {
          tokens.emplace_back(position, TokenType::BINARY_NUMBER);
        } else if (c == 'x') {
          tokens.emplace_back(position, TokenType::HEX_NUMBER);
        }
        ++position;
      } else {
        tokens.emplace_back(position, TokenType::DECIMAL_NUMBER);
      }
      movePastNumber();
    }
  
    if (my_isalpha(c) || c == '_') {
      const uint32_t oldPos = position;
      moveToEndOfKeywordOrIdentifier();
      if (position - oldPos > MIN_CHARS_TO_DISAMBIG) {
        tokens.emplace_back(oldPos, TokenType::IDENTIFIER);
      } else {
        char chars[MIN_CHARS_TO_DISAMBIG]{};
        chars[0] = c;
        for (uint32_t i = oldPos + 1, j = 1; i < position; ++i, ++j) {
          chars[j] = content[i];
        }
        if (stringToType.find(chars) != stringToType.end()) {
          tokens.emplace_back(oldPos, stringToType.at(chars));
          continue;
        }
      }
      moveToNextNonWhiteSpaceChar();
    }
  }
  return tokens;
}


void Lexer::moveToNextNonWhiteSpaceChar() {
  for (; position < size; ++position) {
    if (content[position] != ' ' && content[position] != '\t') {
      return;
    }
  }
}

void Lexer::moveToEndOfKeywordOrIdentifier() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (!my_isalnum(c) && c != '_') {
      break;
    }
  }
  moveToNextNonWhiteSpaceChar();
}

void Lexer::movePastNumber() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (c < '0' || c > '9') {
      break;
    }
  }
  moveToNextNonWhiteSpaceChar();
}

