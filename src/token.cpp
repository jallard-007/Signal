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

bool isBinaryOp(TokenType type) {
  return type >= TokenType::DOT && type <= TokenType::TERNARY;
}

bool isUnaryOp(TokenType type) {
  return type >= TokenType::NOT && type <= TokenType::NEGATIVE;
}

bool Token::operator==(const Token& tk) const {
  return position == tk.position && length == tk.length && type == tk.type;
}
