#pragma once

#include <cstdint>
#include <unordered_map>

enum class TokenType : uint8_t {
  NOTHING,
  
  IDENTIFIER,
  CHAR_LITERAL,
  STRING_LITERAL,
  BINARY_NUMBER,
  DECIMAL_NUMBER,
  HEX_NUMBER,
  NEWLINE,

  // KEYWORDS
  AS,
  INCLUDE,
  FUNC,
  STRUCT,
  TEMPLATE,
  CREATE,
  IF,
  ELIF,
  ELSE,
  RETURN,
  WHILE,
  FOR,
  SWITCH,
  CASE,
  DEFAULT,
  BREAK,
  CONTINUE,

  DEREFERENCE,
  NOT,
  ADDRESS_OF,
  COMMENT,
  MODULO,
  POINTER,
  AMPERSAND,
  MULTIPLICATION,
  OPEN_PAREN,
  CLOSE_PAREN,
  MINUS,
  PLUS,
  EQUAL,
  OPEN_BRACE,
  CLOSE_BRACE,
  BAR,
  OPEN_BRACKET,
  CLOSE_BRACKET,
  BACK_SLASH,
  COLON,
  SEMICOLON,
  LEFT_ARROW,
  RIGHT_ARROW,
  TERNARY,
  COMMA,
  DOT,
  DIVISION,

  // TYPES
  CHAR_TYPE,
  INT_TYPE,
  DOUBLE_TYPE,

  BAD_VALUE
};

// maps characters to tokens that can be matched with a single character
const std::unordered_map<char, TokenType> charToType {
  // top row of keyboard
  {'~', TokenType::DEREFERENCE},
  // {'`', TokenType::BACK_TICK},
  {'!', TokenType::NOT},
  {'@', TokenType::ADDRESS_OF},
  {'#', TokenType::COMMENT},
  // {'$', TokenType::DOLLAR},
  {'%', TokenType::MODULO},
  {'^', TokenType::POINTER},
  {'&', TokenType::AMPERSAND},
  {'*', TokenType::MULTIPLICATION},
  {'(', TokenType::OPEN_PAREN},
  {')', TokenType::CLOSE_PAREN},
  {'-', TokenType::MINUS},
  {'+', TokenType::PLUS},
  {'=', TokenType::EQUAL},
  // second row
  {'{', TokenType::OPEN_BRACE},
  {'}', TokenType::CLOSE_BRACE},
  {'|', TokenType::BAR},
  {']', TokenType::OPEN_BRACKET},
  {'[', TokenType::CLOSE_BRACKET},
  {'\\', TokenType::BACK_SLASH},
  // third row
  {':', TokenType::COLON},
  {';', TokenType::SEMICOLON},
  {'"', TokenType::STRING_LITERAL},
  {'\'', TokenType::CHAR_LITERAL},
  {'\n', TokenType::NEWLINE},
  // fourth row
  {'<', TokenType::LEFT_ARROW},
  {'>', TokenType::RIGHT_ARROW},
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
  {"default", TokenType::DEFAULT},
  {"double", TokenType::DOUBLE_TYPE},
  {"elif", TokenType::ELIF},
  {"else", TokenType::ELSE},
  {"for", TokenType::FOR},
  {"func", TokenType::FUNC},
  {"if", TokenType::IF},
  {"int", TokenType::INT_TYPE},
  {"include", TokenType::INCLUDE},
  {"return", TokenType::RETURN},
  {"struct", TokenType::STRUCT},
  {"switch", TokenType::SWITCH},
  {"template", TokenType::TEMPLATE},
  {"while", TokenType::WHILE},
};

// maps characters to tokens that can be matched with a single character
const std::unordered_map<TokenType, std::string> typeToString {
  {TokenType::DEREFERENCE, "~"},
  {TokenType::NOT, "!"},
  {TokenType::ADDRESS_OF, "@"},
  {TokenType::COMMENT, "#"},
  {TokenType::MODULO, "%"},
  {TokenType::POINTER, "^"},
  {TokenType::AMPERSAND, "&"},
  {TokenType::MULTIPLICATION, "*"},
  {TokenType::OPEN_PAREN, "("},
  {TokenType::CLOSE_PAREN, ")"},
  {TokenType::MINUS, "-"},
  {TokenType::PLUS, "+"},
  {TokenType::EQUAL, "="},
  {TokenType::OPEN_BRACE, "{"},
  {TokenType::CLOSE_BRACE, "}"},
  {TokenType::BAR, "|"},
  {TokenType::OPEN_BRACKET, "]"},
  {TokenType::CLOSE_BRACKET, "["},
  {TokenType::BACK_SLASH, "\\"},
  {TokenType::COLON, ":"},
  {TokenType::SEMICOLON, ";"},
  {TokenType::NEWLINE, "\n"},
  {TokenType::LEFT_ARROW, "<"},
  {TokenType::RIGHT_ARROW, ">"},
  {TokenType::TERNARY, "?"},
  {TokenType::COMMA, ","},
  {TokenType::DOT, "."},
  {TokenType::DIVISION, "/"},
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
  TokenType type;
  Token(u_int32_t pos, TokenType t): position{pos}, type{t} {}
};
