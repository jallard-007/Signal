#include "token.hpp"

bool isLiteral(TokenType type) {
  return type >= TokenType::CHAR_LITERAL && type <= TokenType::NULL_PTR;
}

bool isKeyword(TokenType type) {
  return type >= TokenType::AS && type <= TokenType::TEMPLATE;
}

bool isBuiltInType(TokenType type) {
  return type >= TokenType::BOOL && type <= TokenType::VOID;
}

bool isBinaryOp(TokenType type) {
  return type >= TokenType::DOT && type <= TokenType::GREATER_THAN_EQUAL;
}

bool isUnaryOp(TokenType type) {
  return type >= TokenType::NOT && type <= TokenType::NEGATIVE;
}

bool isKeywordWithBody(TokenType type) {
  return type >= TokenType::IF && type <= TokenType::WHILE;
}

bool isConcreteType(TokenType type) {
  return type == TokenType::IDENTIFIER || (type >= TokenType::BOOL && type <= TokenType::DOUBLE_TYPE);
}

bool isLogicalOp(TokenType type) {
  return type >= TokenType::EQUAL && type <= TokenType::GREATER_THAN_EQUAL;
}

bool Token::operator==(const Token& tk) const {
  return position == tk.position && length == tk.length && type == tk.type;
}
