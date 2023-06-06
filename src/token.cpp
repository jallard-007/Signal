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

bool isTypeDelimiter(TokenType type) {
  return type == TokenType::COMMA || type == TokenType::CLOSE_PAREN
    || type == TokenType::OPEN_BRACE || type == TokenType::SEMICOLON;
}

bool isBinaryOp(TokenType type) {
  return type >= TokenType::ADDITION && type <= TokenType::TERNARY;
}

bool isUnaryOp(TokenType type) {
  return type >= TokenType::DOT && type <= TokenType::NEGATIVE;
}
