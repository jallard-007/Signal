#include <vector>
#include <string>
#include <cctype>
#include "../token.hpp"
#include "tokenizer.hpp"

bool my_isalpha(char c) {
  return std::isalpha(static_cast<unsigned char>(c));
}

bool my_isalnum(char c) {
  return std::isalnum(static_cast<unsigned char>(c));
}

Tokenizer::Tokenizer(const std::string& fileContent):
  content{fileContent}, size{(uint32_t)fileContent.length()}
{
  if (fileContent.length() > UINT32_MAX) {
    exit(1);
  }
  position = 0;
  prevType = TokenType::NOTHING;
}

std::vector<Token> Tokenizer::tokenizeAll() {
  std::vector<Token> tokens;
  while (tokens.emplace_back(tokenizeNext()).type != TokenType::END_OF_FILE);
  return tokens;
}

Token Tokenizer::tokenizeNext() {
  moveToNextNonWhiteSpaceChar();
  if (position >= size) {
    return {position, TokenType::END_OF_FILE};
  }

  const uint32_t tokenStartPos = position;
  TokenType type = TokenType::BAD_VALUE;
  char c = content[position];
  if (charToType.find(c) != charToType.end()) {
    type = charToType.at(c);
    if (type == TokenType::STRING_LITERAL) {
      movePastLiteral('"');
    }
    else if (type == TokenType::CHAR_LITERAL) {
      movePastLiteral('\'');
    }
    else if (++position < size) {
      const char cNext = content[position++];
      if (c == '|' && cNext == '|') {
        type = TokenType::LOGICAL_OR;
      } else if (c == '&' && cNext == '&') {
        type = TokenType::LOGICAL_AND;
      } else if (cNext == '=') {
        TokenType newType = charWithEqualToType.at(c);
        if (newType != TokenType::NOTHING) {
          type = newType;
        } else {
          --position;
        }
      } else if (c == '<' && cNext == '<') {
        if (position < size && content[position] == '=') {
          type = TokenType::SHIFT_LEFT_ASSIGNMENT;
          ++position;
        } else {
          type = TokenType::SHIFT_LEFT;
        }
      } else if (c == '>' && cNext == '>') {
        if (position < size && content[position] == '=') {
          type = TokenType::SHIFT_RIGHT_ASSIGNMENT;
          ++position;
        } else {
          type = TokenType::SHIFT_RIGHT;
        }
      } else if (c == '-') {
        if (cNext == '-') {
          if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACE) {
            type = TokenType::DECREMENT_POSTFIX;
          } else {
            type = TokenType::DECREMENT_PREFIX;
          }
        } else {
          if (prevType == TokenType::IDENTIFIER ||
            isLiteral(prevType) ||
            prevType == TokenType::CLOSE_PAREN ||
            prevType == TokenType::CLOSE_BRACE) {
            type = TokenType::SUBTRACTION;
          } else {
            type = TokenType::NEGATIVE;
          }
        }
      } else if (c == '+' && cNext == '+') {
        if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACE) {
          type = TokenType::INCREMENT_POSTFIX;
        } else {
          type = TokenType::INCREMENT_PREFIX;
        }
      }
      
      else {
        --position;
      }
    }
  }

  else if (c >= '0' && c <= '9') {
    if (c == '0' && position + 1 < size) {
      c = content[++position];
      if (c == 'b') {
        type = TokenType::BINARY_NUMBER;
      } else if (c == 'x') {
        type = TokenType::HEX_NUMBER;
      } else {
        type = TokenType::DECIMAL_NUMBER;
        --position;
      }
    } else {
      type = TokenType::DECIMAL_NUMBER;
    }
    movePastNumber();
  }

  else if (my_isalpha(c) || c == '_') {
    movePastKeywordOrIdentifier();
    if (position - tokenStartPos >= MIN_CHARS_TO_DISAMBIG) {
      type = TokenType::IDENTIFIER;
    } else {
      // + 1 for null termination
      char chars[MIN_CHARS_TO_DISAMBIG + 1]{};
      chars[0] = c;
      for (uint32_t i = tokenStartPos + 1, j = 1; i < position && j < MIN_CHARS_TO_DISAMBIG; ++i, ++j) {
        chars[j] = content[i];
      }
      if (stringToType.find(chars) != stringToType.end()) {
        type = stringToType.at(chars);
      } else {
        type = TokenType::IDENTIFIER;
      }
    }
  }
  
  prevType = type;
  return {tokenStartPos, type};
}

void Tokenizer::moveToNextNonWhiteSpaceChar() {
  for (; position < size; ++position) {
    const char c = content[position];
    if (c != ' ' && c != '\t') {
      return;
    }
  }
}

void Tokenizer::movePastKeywordOrIdentifier() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (!my_isalnum(c) && c != '_') {
      return;
    }
  }
}

void Tokenizer::movePastNumber() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (c < '0' || c > '9') {
      return;
    }
  }
}

void Tokenizer::movePastHexNumber() {
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (!(c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c >= 'f')) {
      return;
    }
  }
}

void Tokenizer::movePastLiteral(char delimeter) {
  char prev = content[position];
  char prevPrev = content[position];
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (c == delimeter && !(prev == '\\' && prevPrev != '\\')) {
      ++position;
      return;
    }
    prevPrev = prev;
    prev = c;
  }
}

void Tokenizer::moveToNewLine() {
  for (; position < size; ++position) {
    if (content[position] == '\n') {
      return;
    }
  }
}

/**
 * Should only be called with types that need to be extracted,
 * otherwise an empty string will be returned.
 * 
 * Valid types include:
 * TokenType::IDENTIFIER, TokenType::DECIMAL_NUMBER, TokenType::HEX_NUMBER,
 * TokenType::BINARY_NUMBER, TokenType::STRING_LITERAL, TokenType::CHAR_LITERAL,
 * and TokenType::COMMENT
*/
std::string Tokenizer::extractToken(Token token) {
  const uint32_t oldPos = position;
  position = token.position;
  switch (token.type) {
    case TokenType::IDENTIFIER:
      movePastKeywordOrIdentifier();
      break;

    case TokenType::DECIMAL_NUMBER:
      movePastNumber();
      break;

    case TokenType::HEX_NUMBER:
      ++position;
      movePastHexNumber();
      break;

    case TokenType::BINARY_NUMBER:
      ++position;
      movePastNumber();
      break;

    case TokenType::STRING_LITERAL:
      movePastLiteral('"');
      break;

    case TokenType::CHAR_LITERAL:
      movePastLiteral('\'');
      break;

    case TokenType::COMMENT:
      moveToNewLine();
      break;

    default:
      // should never reach here
      position = oldPos;
      return "";
  }
  std::string tokenVal = content.substr(token.position, position - token.position);
  position = oldPos;
  return tokenVal;
}
