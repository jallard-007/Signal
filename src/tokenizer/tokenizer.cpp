#include <iostream>
#include "tokenizer.hpp"

TokenPositionInfo::TokenPositionInfo(uint32_t lineNum, uint32_t linePos): lineNum{lineNum}, linePos{linePos} {}

Tokenizer::Tokenizer(const std::string& filePath, const std::string& fileContent):
  badTokens{}, newlinePositions{}, filePath{filePath}, content{fileContent}, peeked{0, 0, TokenType::NOTHING}, error{0, 0, TokenType::NOTHING}, size{(uint32_t)fileContent.length()}
{
  if (fileContent.length() > UINT32_MAX) {
    exit(1);
  }
  newlinePositions.reserve(fileContent.size() / 40);
  newlinePositions.emplace_back(0);
  position = 0;
  prevType = TokenType::NOTHING;
}

// does binary search on the newline list to find the line number
TokenPositionInfo Tokenizer::getTokenPositionInfo(const Token& tk) {
  if (newlinePositions.empty()) {
    return {1, tk.position + 1};
  }
  uint32_t high = newlinePositions.size() - 1;
  uint32_t low = 0;
  uint32_t middle = high / 2;
  while (low < high) {
    if (tk.position < newlinePositions[middle]) {
      high = middle - 1;
    }
    else if (tk.position >= newlinePositions[middle + 1]) {
      low = middle + 1;
    }
    else {
      return {middle + 1, tk.position + 1 - newlinePositions[middle]};
    }
    middle = (high + low) / 2;
  }
  return {high + 1, tk.position + 1 - newlinePositions[high]};
}

void Tokenizer::tokenizeAll(std::vector<Token>& tokens) {
  while (tokens.emplace_back(tokenizeNext()).type != TokenType::END_OF_FILE);
}

#define END_OF_IDENTIFIER(c) TokenType tNext = numToType[(uint8_t)content[++position]]; \
if (tNext != TokenType::IDENTIFIER && tNext != TokenType::DECIMAL_NUMBER) { \
  type = c; \
  break; \
}

/**
 * Allows peeking to the next token
 * Successive calls to this function will return the same Token.
 * The Token must be consumed by calling tokenizeNext before peeking to the next
*/
Token Tokenizer::peekNext() {
  if (peeked.type != TokenType::NOTHING) {
    return peeked;
  }
  peeked = tokenizeNext();
  // put position back
  position = peeked.position;
  return peeked;
}

void Tokenizer::consumePeek() {
  if (peeked.type != TokenType::NOTHING) {
    peeked.type = TokenType::NOTHING;
    position = peeked.position + peeked.length;
  }
}

Token Tokenizer::tokenizeNext() {
  if (peeked.type != TokenType::NOTHING) {
    const Token temp = peeked;
    peeked.type = TokenType::NOTHING;
    position = peeked.position + peeked.length;
    return temp;
  }
  moveToNextNonWhiteSpaceChar();
  const uint32_t tokenStartPos = position;
  char c = content[position];
  TokenType type = numToType[(uint8_t)c];
  switch (type) {
    case TokenType::IDENTIFIER: {
      switch (c) {
        case 'a': {
          // as
          if (content[++position] == 's') {
            END_OF_IDENTIFIER(TokenType::AS)
          }
          movePastIdentifier();
          break;
        }
        case 'b': {
          // break bool
          if (content[++position] == 'r') {
            if (content[++position] == 'e') {
              if (content[++position] == 'a') {
                if (content[++position] == 'k') {
                  END_OF_IDENTIFIER(TokenType::BREAK)
                }
              }
            }
          }
          else if (content[position] == 'o') {
            if (content[++position] == 'o') {
              if (content[++position] == 'l') {
                END_OF_IDENTIFIER(TokenType::BOOL)
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'c': {
          // case create continue char
          if (content[++position] == 'a') {
            if (content[++position] == 's') {
              if (content[++position] == 'e') {
                END_OF_IDENTIFIER(TokenType::CASE)
              }
            }
          }
          else if (content[position] == 'r') {
            if (content[++position] == 'e') {
              if (content[++position] == 'a') {
                if (content[++position] == 't') {
                  if (content[++position] == 'e') {
                    END_OF_IDENTIFIER(TokenType::CREATE)
                  }
                }
              }
            }
          }
          else if (content[position] == 'o') {
            if (content[++position] == 'n') {
              if (content[++position] == 't') {
                if (content[++position] == 'i') {
                  if (content[++position] == 'n') {
                    if (content[++position] == 'u') {
                      if (content[++position] == 'e') {
                        END_OF_IDENTIFIER(TokenType::CONTINUE)
                      }
                    }
                  }
                }
              }
            }
          }
          else if (content[position] == 'h') {
            if (content[++position] == 'a') {
              if (content[++position] == 'r') {
                END_OF_IDENTIFIER(TokenType::CHAR_TYPE)
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'd': {
          // default double 
          if (content[++position] == 'e') {
            if (content[++position] == 'f') {
              if (content[++position] == 'a') {
                if (content[++position] == 'u') {
                  if (content[++position] == 'l') {
                    if (content[++position] == 't') {
                      END_OF_IDENTIFIER(TokenType::DEFAULT)
                    }
                  }
                }
              }
            }
          }
          else if (content[position] == 'o') {
            if (content[++position] == 'u') {
              if (content[++position] == 'b') {
                if (content[++position] == 'l') {
                  if (content[++position] == 'e') {
                    END_OF_IDENTIFIER(TokenType::DOUBLE_TYPE)
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'e': {
          // elif else enum extern
          if (content[++position] == 'l') {
            if (content[++position] == 'i') {
              if (content[++position] == 'f') {
                END_OF_IDENTIFIER(TokenType::ELIF)
              }
            } else if (content[position] == 's') {
              if (content[++position] == 'e') {
                END_OF_IDENTIFIER(TokenType::ELSE)
              }
            }
          }
          else if (content[position] == 'n') {
            if (content[++position] == 'u') {
              if (content[++position] == 'm') {
                END_OF_IDENTIFIER(TokenType::ENUM)
              }
            }
          }
          else if (content[position] == 'x') {
            if (content[++position] == 't') {
              if (content[++position] == 'e') {
                if (content[++position] == 'r') {
                  if (content[++position] == 'n') {
                    END_OF_IDENTIFIER(TokenType::EXTERN)
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'f': {
          // false for func float
          if (content[++position] == 'a') {
            if (content[++position] == 'l') {
              if (content[++position] == 's') {
                if (content[++position] == 'e') {
                  END_OF_IDENTIFIER(TokenType::FALSE)
                }
              }
            }
          }
          else if (content[position] == 'o') {
            if (content[++position] == 'r') {
              END_OF_IDENTIFIER(TokenType::FOR)
            }
          }
          else if (content[position] == 'u') {
            if (content[++position] == 'n') {
              if (content[++position] == 'c') {
                END_OF_IDENTIFIER(TokenType::FUNC)
              }
            }
          }
          else if (content[position] == 'l') {
            if (content[++position] == 'o') {
              if (content[++position] == 'a') {
                if (content[++position] == 't') {
                  END_OF_IDENTIFIER(TokenType::FLOAT_TYPE)
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'i': {
          // if include int8 int16 int32 int64
          if (content[++position] == 'f') {
            END_OF_IDENTIFIER(TokenType::IF)
          }
          else if (content[position] == 'n') {
            if (content[++position] == 't') {
              if (content[++position] == '8') {
                END_OF_IDENTIFIER(TokenType::INT8_TYPE)
              } else if (content[position] == '1') {
                if (content[++position] == '6') {
                  END_OF_IDENTIFIER(TokenType::INT16_TYPE)
                }
              } else if (content[position] == '3') {
                if (content[++position] == '2') {
                  END_OF_IDENTIFIER(TokenType::INT32_TYPE)
                }
              } else if (content[position] == '6') {
                if (content[++position] == '4') {
                  END_OF_IDENTIFIER(TokenType::INT64_TYPE)
                }
              }
            } else if (content[position] == 'c') {
              if (content[++position] == 'l') {
                if (content[++position] == 'u') {
                  if (content[++position] == 'd') {
                    if (content[++position] == 'e') {
                      END_OF_IDENTIFIER(TokenType::INCLUDE)
                    }
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'n': {
          // nullptr 
          if (content[++position] == 'u') {
            if (content[++position] == 'l') {
              if (content[++position] == 'l') {
                if (content[++position] == 'p') {
                  if (content[++position] == 't') {
                    if (content[++position] == 'r') {
                      END_OF_IDENTIFIER(TokenType::NULL_PTR)
                    }
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'p': {
          // ptr 
          if (content[++position] == 't') {
            if (content[++position] == 'r') {
              END_OF_IDENTIFIER(TokenType::POINTER)
            }
          }
          movePastIdentifier();
          break;
        }
        case 'r': {
          // ref return
          if (content[++position] == 'e') {
            if (content[++position] == 't') {
              if (content[++position] == 'u') {
                if (content[++position] == 'r') {
                  if (content[++position] == 'n') {
                    END_OF_IDENTIFIER(TokenType::RETURN)
                  }
                }
              }
            } else if (content[position] == 'f') {
              END_OF_IDENTIFIER(TokenType::REFERENCE)
            }
          }
          movePastIdentifier();
          break;
        }
        case 's': {
          // switch struct
          if (content[++position] == 'w') {
            if (content[++position] == 'i') {
              if (content[++position] == 't') {
                if (content[++position] == 'c') {
                  if (content[++position] == 'h') {
                    END_OF_IDENTIFIER(TokenType::SWITCH)
                  }
                }
              }
            }
          }
          else if (content[position] == 't') {
            if (content[++position] == 'r') {
              if (content[++position] == 'u') {
                if (content[++position] == 'c') {
                  if (content[++position] == 't') {
                    END_OF_IDENTIFIER(TokenType::STRUCT)
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 't': {
          // true template
          if (content[++position] == 'r') {
            if (content[++position] == 'u') {
              if (content[++position] == 'e') {
                END_OF_IDENTIFIER(TokenType::TRUE)
              }
            }
          } else if (content[position] == 'e') {
            if (content[++position] == 'm') {
              if (content[++position] == 'p') {
                if (content[++position] == 'l') {
                  if (content[++position] == 'a') {
                    if (content[++position] == 't') {
                      if (content[++position] == 'e') {
                        END_OF_IDENTIFIER(TokenType::TEMPLATE)
                      }
                    }
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'u': {
          // uint8 uint16 uint32 uint64
          if (content[++position] == 'i') {
            if (content[++position] == 'n') {
              if (content[++position] == 't') {
                if (content[++position] == '8') {
                  END_OF_IDENTIFIER(TokenType::UINT8_TYPE)
                } else if (content[position] == '1') {
                  if (content[++position] == '6') {
                    END_OF_IDENTIFIER(TokenType::UINT16_TYPE)
                  }
                } else if (content[position] == '3') {
                  if (content[++position] == '2') {
                    END_OF_IDENTIFIER(TokenType::UINT32_TYPE)
                  }
                } else if (content[position] == '6') {
                  if (content[++position] == '4') {
                    END_OF_IDENTIFIER(TokenType::UINT64_TYPE)
                  }
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'v': {
          // void
          if (content[++position] == 'o') {
            if (content[++position] == 'i') {
              if (content[++position] == 'd') {
                END_OF_IDENTIFIER(TokenType::VOID)
              }
            }
          }
          movePastIdentifier();
          break;
        }
        case 'w': {
          // while
          if (content[++position] == 'h') {
            if (content[++position] == 'i') {
              if (content[++position] == 'l') {
                if (content[++position] == 'e') {
                  END_OF_IDENTIFIER(TokenType::WHILE)
                }
              }
            }
          }
          movePastIdentifier();
          break;
        }
        default: {
          movePastIdentifier();
          break;
        }
      }
      break;
    }

    case TokenType::END_OF_FILE: {
      break;
    }

    case TokenType::STRING_LITERAL: {
      if (!movePastLiteral('"')) {
        badTokens.emplace_back(position, 0, TokenType::STRING_LITERAL);
      }
      break;
    }

    case TokenType::CHAR_LITERAL: {
      if (!movePastLiteral('\'')) {
        badTokens.emplace_back(position, 0, TokenType::CHAR_LITERAL);
      }
      // TODO: validate content of character. escaped characters :(
      // maybe do this during type checking?
      // ' '\n \r \t \' \" \\ \v \f \e \b \a \127 - \0
      break;
    }

    case TokenType::COMMENT: {
      movePastNewLine();
      return tokenizeNext();
    }

    case TokenType::NEWLINE: {
      newlinePositions.emplace_back(++position);
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
      std::cerr << filePath << ':' << std::to_string(newlinePositions.size() + 1) << ':' << std::to_string(position + 1 - newlinePositions.back()) << '\n'; 
      std::cerr << "Invalid character with ASCII code: [" << (int)c << "]\n";
      exit(1);
    }

    default: {
      const TokenType tNext = numToType[(uint8_t)content[++position]];
      switch (type) {
        case TokenType::NOT: {
          if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::NOT_EQUAL;
          }
          break;
        }
        case TokenType::BITWISE_XOR: {
          if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::BITWISE_XOR_ASSIGNMENT;
          }
          break;
        }
        case TokenType::MODULO: {
          if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::MODULO_ASSIGNMENT;
          }
          break;
        }
        case TokenType::ASSIGNMENT: {
          if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::EQUAL;
          }
          break;
        }
        case TokenType::DIVISION: {
          if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::DIVISION_ASSIGNMENT;
          }
          break;
        }
        case TokenType::BITWISE_OR: {
          if (tNext == TokenType::BITWISE_OR) {
            type = TokenType::LOGICAL_OR;
            ++position;
          } else if (tNext == TokenType::ASSIGNMENT) {
            type = TokenType::BITWISE_OR_ASSIGNMENT;
            ++position;
          }
          break;
        }
        case TokenType::BITWISE_AND: {
          if (tNext == TokenType::BITWISE_AND) {
            type = TokenType::LOGICAL_AND;
            ++position;
          } else if (tNext == TokenType::ASSIGNMENT) {
            type = TokenType::BITWISE_AND_ASSIGNMENT;
            ++position;
          }
          break;
        }
        case TokenType::LESS_THAN: {
          if (tNext == TokenType::LESS_THAN) {
            ++position;
            if (numToType[(uint8_t)content[position]] == TokenType::ASSIGNMENT) {
              ++position;
              type = TokenType::SHIFT_LEFT_ASSIGNMENT;
            } else {
              type = TokenType::SHIFT_LEFT;
            }
          } else if (tNext == TokenType::ASSIGNMENT) {
            type = TokenType::LESS_THAN_EQUAL;
            ++position;
          }
          break;
        }
        case TokenType::GREATER_THAN: {
          if (tNext == TokenType::GREATER_THAN) {
            ++position;
            if (numToType[(uint8_t)content[position]] == TokenType::ASSIGNMENT) {
              ++position;
              type = TokenType::SHIFT_RIGHT_ASSIGNMENT;
            } else {
              type = TokenType::SHIFT_RIGHT;
            }
          } else if (tNext == TokenType::ASSIGNMENT) {
            type = TokenType::GREATER_THAN_EQUAL;
            ++position;
          }
          break;
        }
        case TokenType::SUBTRACTION: {
          if (tNext == TokenType::GREATER_THAN) {
            ++position;
            type = TokenType::PTR_MEMBER_ACCESS;
          } else if (tNext == TokenType::SUBTRACTION) {
            ++position;
            if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACKET) {
              type = TokenType::DECREMENT_POSTFIX;
            } else {
              type = TokenType::DECREMENT_PREFIX;
            }
          } else if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::SUBTRACTION_ASSIGNMENT;
          } else if (
            prevType != TokenType::IDENTIFIER &&
            prevType != TokenType::CLOSE_PAREN &&
            prevType != TokenType::CLOSE_BRACKET && !isLiteral(prevType)
          ) {
            type = TokenType::NEGATIVE;
          }
          break;
        }
        case TokenType::ADDITION: {
          if (tNext == TokenType::ADDITION) {
            ++position;
            if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACE) {
              type = TokenType::INCREMENT_POSTFIX;
            } else {
              type = TokenType::INCREMENT_PREFIX;
            }
          } else if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::ADDITION_ASSIGNMENT;
          }
          break;
        }
        case TokenType::MULTIPLICATION: {
          if (tNext == TokenType::ASSIGNMENT) {
            ++position;
            type = TokenType::MULTIPLICATION_ASSIGNMENT;
          } else if (prevType == TokenType::OPEN_BRACE || prevType == TokenType::OPEN_PAREN || prevType == TokenType::OPEN_BRACKET || isBinaryOp(prevType)) {
            type = TokenType::DEREFERENCE;
          }
          break;
        }
        default: {
          break;
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
  return {tokenStartPos, (uint16_t)(position - tokenStartPos), type};
}

void Tokenizer::moveToNextNonWhiteSpaceChar() {
  for (; position < size; ++position) {
    const char c = content[position];
    if (c != ' ' && c != '\t') {
      return;
    }
  }
}

void Tokenizer::movePastIdentifier() {
  for (; position < size; ++position) {
    TokenType type = numToType[(uint8_t)content[position]];
    if (type != TokenType::IDENTIFIER && type != TokenType::DECIMAL_NUMBER) {
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

void Tokenizer::movePastNewLine() {
  for (; position < size; ++position) {
    if (content[position] == '\n') {
      newlinePositions.emplace_back(position);
      ++position;
      return;
    }
  }
}

std::string Tokenizer::extractToken(Token &token) {
  return content.substr(token.position, token.length);
}
