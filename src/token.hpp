#pragma once

#include <string>
#include <cstdint>
#include <unordered_map>

enum class TokenType : uint8_t {
  // special
  NOTHING,
  BAD_VALUE,
  END_OF_FILE,
  
  // literals
  CHAR_LITERAL,
  STRING_LITERAL,
  DECIMAL_NUMBER,
  BINARY_NUMBER,
  HEX_NUMBER,
  FALSE, //
  TRUE, //
  NULL_, //

  // keywords
  AS, //
  BREAK,
  CASE, //
  CONTINUE,
  CREATE, //
  DEFAULT, //
  IF,
  ELIF,
  ELSE,
  FOR,
  SWITCH, //
  RETURN,
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


  // BINARY
  // general
  DOT,
  COMMA,

  // arithmetic
  ADDITION,
  SUBTRACTION,
  MULTIPLICATION,
  DIVISION,
  MODULO,
  BITWISE_AND,
  BITWISE_XOR,
  BITWISE_OR,
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
  TERNARY,

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
  CHAR_TYPE,
  INT_TYPE,
  DOUBLE_TYPE,
  POINTER,

};

bool isLiteral(TokenType);
bool isKeyword(TokenType);
bool isBuiltInType(TokenType);
bool isBinaryOp(TokenType);
bool isUnaryOp(TokenType);
bool isKeywordWithBody(TokenType);
bool isStatementDelimiter(TokenType);
bool isTypeDelimiter(TokenType);

// maps characters to tokens that can be matched with a single character
const std::unordered_map<char, TokenType> charToType {
  // top row of keyboard
  {'~', TokenType::DEREFERENCE},
  // {'`', TokenType::BACK_TICK},
  {'!', TokenType::NOT},
  {'@', TokenType::ADDRESS_OF},
  {'#', TokenType::COMMENT},
  {'$', TokenType::BITWISE_XOR},
  {'%', TokenType::MODULO},
  {'^', TokenType::POINTER},
  {'&', TokenType::BITWISE_AND},
  {'*', TokenType::MULTIPLICATION},
  {'(', TokenType::OPEN_PAREN},
  {')', TokenType::CLOSE_PAREN},
  {'-', TokenType::NEGATIVE},
  {'+', TokenType::ADDITION},
  {'=', TokenType::ASSIGNMENT},
  // second row
  {'{', TokenType::OPEN_BRACE},
  {'}', TokenType::CLOSE_BRACE},
  {'|', TokenType::BITWISE_OR},
  {']', TokenType::CLOSE_BRACKET},
  {'[', TokenType::OPEN_BRACKET},
  {'\\', TokenType::BACK_SLASH},
  // third row
  {':', TokenType::COLON},
  {';', TokenType::SEMICOLON},
  {'"', TokenType::STRING_LITERAL},
  {'\'', TokenType::CHAR_LITERAL},
  {'\n', TokenType::NEWLINE},
  // fourth row
  {'<', TokenType::LESS_THAN},
  {'>', TokenType::GREATER_THAN},
  {'?', TokenType::TERNARY},
  {',', TokenType::COMMA},
  {'.', TokenType::DOT},
  {'/', TokenType::DIVISION}
};

const std::unordered_map<std::string, TokenType> stringToType {
  {"as", TokenType::AS},
  {"break", TokenType::BREAK},
  {"case", TokenType::CASE},
  {"create", TokenType::CREATE},
  {"continue", TokenType::CONTINUE},
  {"char", TokenType::CHAR_TYPE},
  {"default", TokenType::DEFAULT},
  {"double", TokenType::DOUBLE_TYPE},
  {"elif", TokenType::ELIF},
  {"else", TokenType::ELSE},
  {"enum", TokenType::ENUM},
  {"false", TokenType::FALSE},
  {"for", TokenType::FOR},
  {"func", TokenType::FUNC},
  {"if", TokenType::IF},
  {"int", TokenType::INT_TYPE},
  {"include", TokenType::INCLUDE},
  {"extern", TokenType::EXTERN},
  {"null", TokenType::NULL_},
  {"return", TokenType::RETURN},
  {"struct", TokenType::STRUCT},
  {"switch", TokenType::SWITCH},
  {"template", TokenType::TEMPLATE},
  {"true", TokenType::TRUE},
  {"while", TokenType::WHILE},
};

const std::unordered_map<char, TokenType> charWithEqualToType {
  {'!', TokenType::NOT_EQUAL},
  {'$', TokenType::BITWISE_XOR_ASSIGNMENT},
  {'%', TokenType::MODULO_ASSIGNMENT},
  {'&', TokenType::BITWISE_AND_ASSIGNMENT},
  {'*', TokenType::MULTIPLICATION_ASSIGNMENT},
  {'-', TokenType::SUBTRACTION_ASSIGNMENT},
  {'+', TokenType::ADDITION_ASSIGNMENT},
  {'=', TokenType::EQUAL},
  {'|', TokenType::BITWISE_OR_ASSIGNMENT},
  {'<', TokenType::LESS_THAN_EQUAL},
  {'>', TokenType::GREATER_THAN_EQUAL},
  {'/', TokenType::DIVISION_ASSIGNMENT},

  {'~', TokenType::NOTHING},
  {'@', TokenType::NOTHING},
  {'#', TokenType::NOTHING},
  {'^', TokenType::NOTHING},
  {'(', TokenType::NOTHING},
  {')', TokenType::NOTHING},
  {'{', TokenType::NOTHING},
  {'}', TokenType::NOTHING},
  {']', TokenType::NOTHING},
  {'[', TokenType::NOTHING},
  {'\\', TokenType::NOTHING},
  {':', TokenType::NOTHING},
  {';', TokenType::NOTHING},
  {'"', TokenType::NOTHING},
  {'\'', TokenType::NOTHING},
  {'\n', TokenType::NOTHING},
  {'?', TokenType::NOTHING},
  {',', TokenType::NOTHING},
  {'.', TokenType::NOTHING},
};

// maps characters to tokens that can be matched with a single character
const std::unordered_map<TokenType, std::string> typeToString {
  {TokenType::DEREFERENCE, "~"},
  {TokenType::NOT, "!"},
  {TokenType::NOT_EQUAL, "!="},
  {TokenType::ADDRESS_OF, "@"},
  {TokenType::COMMENT, "#"},
  {TokenType::BITWISE_XOR, "$"},
  {TokenType::BITWISE_XOR_ASSIGNMENT, "$="},
  {TokenType::MODULO, "%"},
  {TokenType::MODULO_ASSIGNMENT, "%="},
  {TokenType::POINTER, "^"},
  {TokenType::BITWISE_AND, "&"},
  {TokenType::BITWISE_AND_ASSIGNMENT, "&="},
  {TokenType::LOGICAL_AND, "&&"},
  {TokenType::MULTIPLICATION, "*"},
  {TokenType::MULTIPLICATION_ASSIGNMENT, "*="},
  {TokenType::OPEN_PAREN, "("},
  {TokenType::CLOSE_PAREN, ")"},
  {TokenType::NEGATIVE, "-"},
  {TokenType::DECREMENT_PREFIX, "--"},
  {TokenType::DECREMENT_POSTFIX, "--"},
  {TokenType::ADDITION, "+"},
  {TokenType::INCREMENT_PREFIX, "++"},
  {TokenType::INCREMENT_POSTFIX, "++"},
  {TokenType::ASSIGNMENT, "="},
  {TokenType::EQUAL, "=="},
  {TokenType::OPEN_BRACE, "{"},
  {TokenType::CLOSE_BRACE, "}"},
  {TokenType::BITWISE_OR, "|"},
  {TokenType::BITWISE_OR_ASSIGNMENT, "|="},
  {TokenType::LOGICAL_OR, "||"},
  {TokenType::OPEN_BRACKET, "["},
  {TokenType::CLOSE_BRACKET, "]"},
  {TokenType::BACK_SLASH, "\\"},
  {TokenType::COLON, ":"},
  {TokenType::SEMICOLON, ";"},
  {TokenType::NEWLINE, "\n"},
  {TokenType::LESS_THAN, "<"},
  {TokenType::LESS_THAN_EQUAL, "<="},
  {TokenType::SHIFT_LEFT, "<<"},
  {TokenType::SHIFT_LEFT_ASSIGNMENT, "<<="},
  {TokenType::GREATER_THAN, ">"},
  {TokenType::GREATER_THAN_EQUAL, ">="},
  {TokenType::SHIFT_RIGHT, ">>"},
  {TokenType::SHIFT_RIGHT_ASSIGNMENT, ">>="},
  {TokenType::TERNARY, "?"},
  {TokenType::COMMA, ","},
  {TokenType::DOT, "."},
  {TokenType::DIVISION, "/"},
  {TokenType::DIVISION_ASSIGNMENT, "/="},
  {TokenType::AS, "as"},
  {TokenType::BREAK, "break"},
  {TokenType::CASE, "case"},
  {TokenType::CREATE, "create"},
  {TokenType::CONTINUE, "continue"},
  {TokenType::DEFAULT, "default"},
  {TokenType::DOUBLE_TYPE, "double"},
  {TokenType::ELIF, "elif"},
  {TokenType::ELSE, "else"},
  {TokenType::FOR, "for"},
  {TokenType::FUNC, "func"},
  {TokenType::IF, "if"},
  {TokenType::INT_TYPE, "int"},
  {TokenType::INCLUDE, "include"},
  {TokenType::RETURN, "return"},
  {TokenType::STRUCT, "struct"},
  {TokenType::SWITCH, "switch"},
  {TokenType::TEMPLATE, "template"},
  {TokenType::WHILE, "while"},
  {TokenType::IDENTIFIER, "identifier"},
  {TokenType::DECIMAL_NUMBER, "10"},
  {TokenType::BINARY_NUMBER, "0b1010"},
  {TokenType::HEX_NUMBER, "0xFF"},
  {TokenType::STRING_LITERAL, "\"string\""},
  {TokenType::CHAR_LITERAL, "'j'"},
};

#define MIN_CHARS_TO_DISAMBIG 9 // length of longest keyword + 1 (currently 'template' at 8)

struct Token {
  uint32_t position;
  uint16_t length;
  TokenType type;
  Token() = delete;
  Token(uint32_t pos, uint16_t len, TokenType t): position{pos}, length{len}, type{t} {}
  bool operator==(const Token&) const;
};
