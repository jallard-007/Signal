#include "token.hpp"

bool isLiteral(TokenType type) {
  return type >= TokenType::CHAR_LITERAL && type <= TokenType::HEX_NUMBER;
}

bool isKeyword(TokenType type) {
  return type >= TokenType::AS && type <= TokenType::WHILE;
}
