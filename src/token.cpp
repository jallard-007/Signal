#include "token.hpp"

bool isLiteral(TokenType type) {
  return type >= TokenType::CHAR_LITERAL && type <= TokenType::NULL_;
}

bool isKeyword(TokenType type) {
  return type >= TokenType::AS && type <= TokenType::TEMPLATE;
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

bool isKeywordWithBody(TokenType type) {
  return type >= TokenType::IF && type <= TokenType::WHILE;
}

bool isTypeDelimiter(TokenType type) {
  return type != TokenType::IDENTIFIER && !isBuiltInType(type);
}

bool Token::operator==(const Token& tk) const {
  return position == tk.position && length == tk.length && type == tk.type;
}
