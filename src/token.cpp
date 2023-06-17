#include "token.hpp"

bool isBuiltInType(TokenType type) {
  return type >= TokenType::BOOL && type <= TokenType::VOID;
}

bool isBinaryOp(TokenType type) {
  return type >= TokenType::DOT && type <= TokenType::GREATER_THAN_EQUAL;
}

bool isUnaryOp(TokenType type) {
  return type >= TokenType::NOT && type <= TokenType::NEGATIVE;
}

bool isConcreteType(TokenType type) {
  return type == TokenType::IDENTIFIER || (type >= TokenType::BOOL && type <= TokenType::DOUBLE_TYPE);
}

bool isLogicalOp(TokenType type) {
  return type >= TokenType::EQUAL && type <= TokenType::GREATER_THAN_EQUAL;
}

bool isControlFlow(TokenType type) {
  return type >= TokenType::IF && type <= TokenType::WHILE;
}

bool Token::operator==(const Token& tk) const {
  return position == tk.position && length == tk.length && type == tk.type;
}
