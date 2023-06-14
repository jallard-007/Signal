#include <iostream>
#include "tokenizer.hpp"

Tokenizer::Tokenizer(const std::string& filePath, const std::string& fileContent):
  filePath{filePath}, content{fileContent}, peeked{0, 0, TokenType::NOTHING}, error{0, 0, TokenType::NOTHING}, size{(uint32_t)fileContent.length()}
{
  if (fileContent.length() > UINT32_MAX) {
    exit(1);
  }
  lineNum = 1;
  lineStart = 0;
  position = 0;
  prevType = TokenType::NOTHING;
}

std::vector<Token> Tokenizer::tokenizeAll() {
  std::vector<Token> tokens;
  while (tokens.emplace_back(tokenizeNext()).type != TokenType::END_OF_FILE);
  return tokens;
}

/**
 * Allows peeking to the next token
 * Successive calls to this function will return the same Token.
 * The Token must be consumed by calling tokenizeNext before peeking to the next
*/
Token Tokenizer::peekNext() {
  if (peeked.type != TokenType::NOTHING) {
    if (prevType == TokenType::BAD_VALUE) {
      // fatal error not handled
      exit(55);
    }
    return peeked;
  }
  peeked = tokenizeNext();
  // put position back
  position = peeked.position;
  return peeked;
}

void Tokenizer::consumePeek() {
  if (peeked.type != TokenType::NOTHING) {
    if (prevType == TokenType::BAD_VALUE) {
      // fatal error not handled
      exit(55);
    }
    peeked.type = TokenType::NOTHING;
    position = peeked.position + peeked.length;
  }
}

Token Tokenizer::tokenizeNext() {
  if (peeked.type != TokenType::NOTHING) {
    if (prevType == TokenType::BAD_VALUE) {
      // fatal error not handled
      exit(55);
    }
    const Token temp = peeked;
    peeked.type = TokenType::NOTHING;
    position = peeked.position + peeked.length;
    return temp;
  }
  moveToNextNonWhiteSpaceChar();
  if (position >= size) {
    return {size, 0, TokenType::END_OF_FILE};
  }

  const uint32_t tokenStartPos = position;
  uint16_t length = 0;
  TokenType type = TokenType::BAD_VALUE;
  char c = content[position];
  type = numToType[(int)c];
  switch (type) {
    case TokenType::IDENTIFIER: {
      movePastKeywordOrIdentifier();
      if (length >= MIN_CHARS_TO_DISAMBIG) {
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
      break;
    }

    case TokenType::STRING_LITERAL: {
      if (!movePastLiteral('"')) {
        error.type = TokenType::STRING_LITERAL;
        error.lineNum = lineNum;
        error.linePos = position - lineStart;
        type = TokenType::BAD_VALUE;
      }
      break;
    }

    case TokenType::CHAR_LITERAL: {
      if (!movePastLiteral('\'')) {
        error.type = TokenType::CHAR_LITERAL;
        error.lineNum = lineNum;
        error.linePos = position - lineStart;
        type = TokenType::BAD_VALUE;
      }
      // TODO: validate content of character. escaped characters :(
      // maybe do this during type checking?
      // ' '\n \r \t \' \" \\ \v \f \e \b \a \127 - \0
      break;
    }

    case TokenType::COMMENT: {
      moveToNewLine();
      break;
    }

    case TokenType::NEWLINE: {
      ++lineNum;
      lineStart = ++position;
      return tokenizeNext();
    }

    case TokenType::DECIMAL_NUMBER: {
      if (c == '0' && position + 1 < size) {
        c = content[++position];
        if (c == 'x') {
          type = TokenType::HEX_NUMBER;
          movePastHexNumber();
        } else {
          if (c == 'b') {
            type = TokenType::BINARY_NUMBER;
          } else {
            type = TokenType::DECIMAL_NUMBER;
            --position;
          }
          movePastNumber();
        }
      } else {
        type = TokenType::DECIMAL_NUMBER;
        movePastNumber();
      }
      break;
    }
  
    case TokenType::BAD_VALUE: {
      std::cerr << filePath << ':' << lineNum << ':' << position + 1 - lineStart << '\n'; 
      std::cerr << "Invalid character with ASCII code: [" << (int)c << "]\n";
      exit(1);
    }

    default: {
      if (++position < size) {
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
          if (cNext == '>') {
            type = TokenType::PTR_MEMBER_ACCESS;
          }
          else if (cNext == '-') {
            if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACKET) {
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
              --position;
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
          if (c == '*' && (prevType == TokenType::OPEN_BRACE || prevType == TokenType::OPEN_PAREN || prevType == TokenType::OPEN_BRACKET || isBinaryOp(prevType))) {
            type = TokenType::DEREFERENCE;
          }
        }
      }
      break;
    }
  }

  if (position - tokenStartPos > UINT16_MAX) {
    // error
    exit(1);
  }
  prevType = type;
  return {tokenStartPos, lineNum, tokenStartPos + 1 - lineStart, static_cast<uint16_t>(position - tokenStartPos), type};
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
    if (numToType[(int)content[position]] != TokenType::IDENTIFIER && numToType[(int)content[position]] != TokenType::DECIMAL_NUMBER) {
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
    if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))) {
      return;
    }
  }
}

bool Tokenizer::movePastLiteral(char delimiter) {
  char prev = content[position];
  char prevPrev = content[position];
  for (++position; position < size; ++position) {
    const char c = content[position];
    if (c == '\n') {
      ++position;
      return false;
    }
    if (c == delimiter && !(prev == '\\' && prevPrev != '\\')) {
      ++position;
      return true;
    }
    prevPrev = prev;
    prev = c;
  }
  return false;
}

void Tokenizer::moveToNewLine() {
  for (; position < size; ++position) {
    if (content[position] == '\n') {
      ++lineNum;
      lineStart = position + 1;
      return;
    }
  }
}

std::string Tokenizer::extractToken(Token &token) {
  return content.substr(token.position, token.length);
}
