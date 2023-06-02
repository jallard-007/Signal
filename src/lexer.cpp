#include <vector>
#include <string>
#include <cctype>
#include "token.hpp"
#include "lexer.hpp"

bool my_isalpha(char c) {
  return std::isalpha(static_cast<unsigned char>(c));
}

bool my_isalnum(char c) {
  return std::isalnum(static_cast<unsigned char>(c));
}

Lexer::Lexer(const std::string& fileContent):
  content{fileContent.data()}, size{(uint32_t)fileContent.length()}
{
  if (fileContent.length() > UINT32_MAX) {
    exit(1);
  }
  position = 0;
}

std::vector<Token>& Lexer::tokenize() {
  while (position < size) {
    char c = content[position];
    if (charToType.find(c) != charToType.end()) {
      tokens.emplace_back(position, charToType.at(c));
      ++position;
    }

    else if (c >= '0' && c <= '9') {
      if (position + 1 < size) {
        c = content[position + 1];
        if (c == 'b') {
          tokens.emplace_back(position, TokenType::BINARY_NUMBER);
          ++position;
        } else if (c == 'x') {
          tokens.emplace_back(position, TokenType::HEX_NUMBER);
          ++position;
        } else {
          tokens.emplace_back(position, TokenType::DECIMAL_NUMBER);
        }
      } else {
        tokens.emplace_back(position, TokenType::DECIMAL_NUMBER);
      }
      movePastNumber();
    }
  
    else if (my_isalpha(c) || c == '_') {
      const uint32_t oldPos = position;
      movePastKeywordOrIdentifier();
      if (position - oldPos >= MIN_CHARS_TO_DISAMBIG) {
        tokens.emplace_back(oldPos, TokenType::IDENTIFIER);
      } else {
        char chars[MIN_CHARS_TO_DISAMBIG]{};
        chars[0] = c;
        for (uint32_t i = oldPos + 1, j = 1; i < position; ++i, ++j) {
          chars[j] = content[i];
        }
        if (stringToType.find(chars) != stringToType.end()) {
          tokens.emplace_back(oldPos, stringToType.at(chars));
        } else {
          tokens.emplace_back(oldPos, TokenType::IDENTIFIER);
        }
      }
    } else {
      tokens.emplace_back(position, TokenType::BAD_VALUE);
      return tokens;
    }
    moveToNextNonWhiteSpaceChar();
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

void Lexer::movePastKeywordOrIdentifier() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (!my_isalnum(c) && c != '_') {
      break;
    }
  }
}

void Lexer::movePastNumber() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (c < '0' || c > '9') {
      break;
    }
  }
}
