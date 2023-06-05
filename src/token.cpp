#include "token.hpp"

bool isLiteral(TokenType type) {
  return type >= TokenType::CHAR_LITERAL && type <= TokenType::HEX_NUMBER;
}

bool isKeyword(TokenType type) {
  return type >= TokenType::AS && type <= TokenType::WHILE;
}

bool isBuiltInType(TokenType type) {
  return type >= TokenType::CHAR_TYPE && type <= TokenType::POINTER;
}

bool isTypeDelimeter(TokenType type) {
  return type == TokenType::COMMA || type == TokenType::CLOSE_PAREN
    || type == TokenType::OPEN_BRACE;
}
