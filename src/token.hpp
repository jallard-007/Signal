#pragma once

#include <string>
#include <cstdint>
#include <iostream>
#include <unordered_map>

enum class TokenType : uint8_t {
  // special
  NONE,
  BAD_VALUE,
  END_OF_FILE,
  
  // literals
  CHAR_LITERAL,
  STRING_LITERAL,
  DECIMAL_NUMBER,
  BINARY_NUMBER,
  FLOAT_NUMBER,
  HEX_NUMBER,
  FALSE, //
  TRUE, //
  NULL_PTR, //

  // keywords
  AS, //
  BREAK,
  CASE, //
  CONTINUE,
  CREATE, //
  DEFAULT, //
  ELIF,
  ELSE,
  IF,
  FOR,
  SWITCH, //
  RETURN,
  EXIT,
  WHILE,
  ENUM,
  FUNC,
  INCLUDE, //
  EXTERN,
  STRUCT,
  TEMPLATE,

  // general
  IDENTIFIER,
  COMMENT,
  NEWLINE,
  OPEN_PAREN,
  OPEN_BRACE,
  OPEN_BRACKET,
  CLOSE_PAREN,
  CLOSE_BRACE,
  CLOSE_BRACKET,
  SEMICOLON,
  BACK_SLASH,
  COLON,
  COMMA,
  TERNARY,

  // BINARY
  // general
  DOT,
  PTR_MEMBER_ACCESS,

  // arithmetic
  ADDITION,
  SUBTRACTION,
  MULTIPLICATION,
  DIVISION,
  MODULO,
  BITWISE_OR,
  BITWISE_AND,
  BITWISE_XOR,
  SHIFT_LEFT,
  SHIFT_RIGHT,

  // assignments
  ASSIGNMENT,
  ADDITION_ASSIGNMENT,
  SUBTRACTION_ASSIGNMENT,
  MULTIPLICATION_ASSIGNMENT,
  DIVISION_ASSIGNMENT,
  MODULO_ASSIGNMENT,
  BITWISE_OR_ASSIGNMENT,
  BITWISE_XOR_ASSIGNMENT,
  BITWISE_AND_ASSIGNMENT,
  SHIFT_LEFT_ASSIGNMENT,
  SHIFT_RIGHT_ASSIGNMENT,

  // logical
  EQUAL,
  NOT_EQUAL,
  LOGICAL_AND,
  LOGICAL_OR,
  LESS_THAN,
  LESS_THAN_EQUAL,
  GREATER_THAN,
  GREATER_THAN_EQUAL,

  // UNARY
  NOT,
  ADDRESS_OF,
  DEREFERENCE,
  INCREMENT_POSTFIX,
  INCREMENT_PREFIX,
  DECREMENT_POSTFIX,
  DECREMENT_PREFIX,
  NEGATIVE,

  // types
  BOOL,
  CHAR_TYPE,
  STRING_TYPE,
  INT8_TYPE,
  UINT8_TYPE,
  INT16_TYPE,
  UINT16_TYPE,
  INT32_TYPE,
  UINT32_TYPE,
  INT64_TYPE,
  UINT64_TYPE,
  POINTER,
  DOUBLE_TYPE,
  VOID,
  REFERENCE,

  // type modifiers
  CONST,

  // extra types used by parser to report errors
  TYPE,
  OPERATOR,

  // extra types used by checker
  DEC_PTR,
};

/**
 * Need to change to uint64_t so that this works with little and big endian in Expression node (expression type)
*/
struct Token {
  private:
  [[maybe_unused]] char padding;
  public:
  TokenType type{TokenType::NONE};
  uint16_t length{0};
  uint32_t position{0};
  Token() = default;
  Token(const Token&);
  Token(uint32_t pos, uint16_t len, TokenType t): type{t}, length{len}, position{pos} {}
  bool operator==(const Token&) const;
  Token& operator=(const Token&);
  inline TokenType getType() const { return type; };
};


bool isBuiltInType(TokenType);
bool isTypeModifier(TokenType);
bool isConcreteType(TokenType);
bool isBinaryOp(TokenType);
bool isUnaryOp(TokenType);
bool isControlFlow(TokenType);
bool isLiteral(TokenType);
bool isLogicalOp(TokenType);
bool isAssignment(TokenType);
bool isUnsigned(TokenType);
bool isSigned(TokenType);

extern const TokenType numToType [128];
extern const std::unordered_map<TokenType, std::string> typeToString;

std::ostream& operator<<(std::ostream& os, const TokenType& obj);

extern const char * tokenTypeToString [];
