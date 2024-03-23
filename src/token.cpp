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
  return type == TokenType::IDENTIFIER || (type >= TokenType::BOOL && type <= TokenType::VOID);
}

bool isLogicalOp(TokenType type) {
  return type >= TokenType::EQUAL && type <= TokenType::GREATER_THAN_EQUAL;
}

bool isControlFlow(TokenType type) {
  return type >= TokenType::IF && type <= TokenType::WHILE;
}

bool isLiteral(TokenType type) {
  return type >= TokenType::CHAR_LITERAL && type <= TokenType::NULL_PTR;
}

bool isAssignment(TokenType type) {
  return type >= TokenType::ASSIGNMENT && type <= TokenType::SHIFT_RIGHT_ASSIGNMENT;
}

bool isUnsigned(TokenType type) {
  return !isSigned(type);
}

bool isSigned(TokenType type) {
  return type == TokenType::INT8_TYPE || type == TokenType::INT16_TYPE || type == TokenType::INT32_TYPE || type == TokenType::INT64_TYPE;
}

Token::Token(const Token& tk): type{tk.type}, length{tk.length}, position{tk.position} {}
bool Token::operator==(const Token& tk) const {
  return position == tk.position && length == tk.length && type == tk.type;
}
Token& Token::operator=(const Token& tk) {
  type = tk.type;
  length = tk.length;
  position = tk.position;
  return *this;
}

const TokenType numToType [128] {
  TokenType::END_OF_FILE , // 0 null character
  TokenType::BAD_VALUE,  // 1 start of heading
  TokenType::BAD_VALUE , // 2 start of text
  TokenType::BAD_VALUE , // 3 end of text
  TokenType::BAD_VALUE , // 4 end of transmission
  TokenType::BAD_VALUE , // 5 enquiry
  TokenType::BAD_VALUE , // 6 acknowledge
  TokenType::BAD_VALUE , // 7 bell
  TokenType::BAD_VALUE , // 8 backspace
  TokenType::BAD_VALUE , // 9 horizontal tab
  TokenType::NEWLINE , // 10 new line
  TokenType::BAD_VALUE , // 11 vertical tab
  TokenType::BAD_VALUE , // 12 new page
  TokenType::BAD_VALUE , // 13 carriage return
  TokenType::BAD_VALUE , // 14 shift out
  TokenType::BAD_VALUE , // 15 shift in 
  TokenType::BAD_VALUE , // 16 data lin escape
  TokenType::BAD_VALUE , // 17 device control 1
  TokenType::BAD_VALUE , // 18 device control 2
  TokenType::BAD_VALUE , // 19 device control 3
  TokenType::BAD_VALUE , // 20 device control 4
  TokenType::BAD_VALUE , // 21 negative acknowledge
  TokenType::BAD_VALUE , // 22 synchronous idle
  TokenType::BAD_VALUE , // 23 end of trans. block
  TokenType::BAD_VALUE , // 24 cancel
  TokenType::BAD_VALUE , // 25 end of medium
  TokenType::BAD_VALUE , // 26 substitute
  TokenType::BAD_VALUE , // 27 escape
  TokenType::BAD_VALUE , // 28 file separator
  TokenType::BAD_VALUE , // 29 group separator
  TokenType::BAD_VALUE , // 30 record separator
  TokenType::BAD_VALUE , // 31 unit separator
  TokenType::BAD_VALUE , // 32 space
  TokenType::NOT , // 33 !
  TokenType::STRING_LITERAL, // 34 "
  TokenType::COMMENT , // 35 #
  TokenType::BAD_VALUE, // 36 $
  TokenType::MODULO , // 37 %
  TokenType::BITWISE_AND , // 38 &
  TokenType::CHAR_LITERAL , // 39 '
  TokenType::OPEN_PAREN, // 40 (
  TokenType::CLOSE_PAREN, // 41 )
  TokenType::MULTIPLICATION, // 42 *
  TokenType::ADDITION, // 43 +
  TokenType::COMMA, // 44 ,
  TokenType::SUBTRACTION, // 45 -
  TokenType::DOT, // 46 .
  TokenType::DIVISION, // 47 /
  TokenType::DECIMAL_NUMBER, // 48 0
  TokenType::DECIMAL_NUMBER, // 49 1
  TokenType::DECIMAL_NUMBER, // 50 2
  TokenType::DECIMAL_NUMBER, // 51 3
  TokenType::DECIMAL_NUMBER, // 52 4
  TokenType::DECIMAL_NUMBER, // 53 5
  TokenType::DECIMAL_NUMBER, // 54 6
  TokenType::DECIMAL_NUMBER, // 55 7
  TokenType::DECIMAL_NUMBER, // 56 8 
  TokenType::DECIMAL_NUMBER, // 57 9
  TokenType::COLON, // 58 :
  TokenType::SEMICOLON, // 59 ;
  TokenType::LESS_THAN, // 60 <
  TokenType::ASSIGNMENT, // 61 =
  TokenType::GREATER_THAN, // 62 >
  TokenType::BAD_VALUE, // 63 ? TokenType::TERNARY, but dont currently support it
  TokenType::ADDRESS_OF, // 64 @   
  TokenType::IDENTIFIER, // 65 A
  TokenType::IDENTIFIER, // 66 B
  TokenType::IDENTIFIER, // 67 C
  TokenType::IDENTIFIER, // 68 D
  TokenType::IDENTIFIER, // 69 E
  TokenType::IDENTIFIER, // 70 F
  TokenType::IDENTIFIER, // 71 G
  TokenType::IDENTIFIER, // 72 H
  TokenType::IDENTIFIER, // 73 I 
  TokenType::IDENTIFIER, // 74 J
  TokenType::IDENTIFIER, // 75 K
  TokenType::IDENTIFIER, // 76 L
  TokenType::IDENTIFIER, // 77 M
  TokenType::IDENTIFIER, // 78 N
  TokenType::IDENTIFIER, // 79 O
  TokenType::IDENTIFIER, // 80 P
  TokenType::IDENTIFIER, // 81 Q
  TokenType::IDENTIFIER, // 82 R
  TokenType::IDENTIFIER, // 83 S
  TokenType::IDENTIFIER, // 84 T
  TokenType::IDENTIFIER, // 85 U
  TokenType::IDENTIFIER, // 86 V
  TokenType::IDENTIFIER, // 87 W
  TokenType::IDENTIFIER, // 88 X
  TokenType::IDENTIFIER, // 89 Y 
  TokenType::IDENTIFIER, // 90 Z
  TokenType::OPEN_BRACKET, // 91 [
  TokenType::BACK_SLASH, // 92 '\'
  TokenType::CLOSE_BRACKET, // 93 ]
  TokenType::BITWISE_XOR, // 94 ^
  TokenType::IDENTIFIER, // 95 _
  TokenType::BAD_VALUE, // 96 `
  TokenType::IDENTIFIER, // 97 a
  TokenType::IDENTIFIER, // 98 b
  TokenType::IDENTIFIER, // 99 c
  TokenType::IDENTIFIER, // 100 d
  TokenType::IDENTIFIER, // 101 e
  TokenType::IDENTIFIER, // 102 f
  TokenType::IDENTIFIER, // 103 g
  TokenType::IDENTIFIER, // 104 h
  TokenType::IDENTIFIER, // 105 i 
  TokenType::IDENTIFIER, // 106 j
  TokenType::IDENTIFIER, // 107 k
  TokenType::IDENTIFIER, // 108 l
  TokenType::IDENTIFIER, // 109 m
  TokenType::IDENTIFIER, // 110 n
  TokenType::IDENTIFIER, // 111 o
  TokenType::IDENTIFIER, // 112 p
  TokenType::IDENTIFIER, // 113 q
  TokenType::IDENTIFIER, // 114 r
  TokenType::IDENTIFIER, // 115 s
  TokenType::IDENTIFIER, // 116 t
  TokenType::IDENTIFIER, // 117 u
  TokenType::IDENTIFIER, // 118 v
  TokenType::IDENTIFIER, // 119 w
  TokenType::IDENTIFIER, // 120 x
  TokenType::IDENTIFIER, // 121 y
  TokenType::IDENTIFIER, // 122 z
  TokenType::OPEN_BRACE, // 123 {
  TokenType::BITWISE_OR, // 124 |
  TokenType::CLOSE_BRACE, // 125 }
  TokenType::BAD_VALUE, // 126 ~
  TokenType::BAD_VALUE  // 127 DEL
};

const std::unordered_map<TokenType, std::string> typeToString {
  {TokenType::DEREFERENCE, "*"},
  {TokenType::NOT, "!"},
  {TokenType::NOT_EQUAL, " != "},
  {TokenType::ADDRESS_OF, "@"},
  {TokenType::COMMENT, "#"},
  {TokenType::BITWISE_XOR, " ^ "},
  {TokenType::BITWISE_XOR_ASSIGNMENT, " ^= "},
  {TokenType::MODULO, " % "},
  {TokenType::MODULO_ASSIGNMENT, " %= "},
  {TokenType::POINTER, "ptr"},
  {TokenType::BITWISE_AND, " & "},
  {TokenType::BITWISE_AND_ASSIGNMENT, " &= "},
  {TokenType::LOGICAL_AND, " && "},
  {TokenType::MULTIPLICATION, " * "},
  {TokenType::MULTIPLICATION_ASSIGNMENT, " *= "},
  {TokenType::OPEN_PAREN, "("},
  {TokenType::CLOSE_PAREN, ")"},
  {TokenType::NEGATIVE, "-"},
  {TokenType::SUBTRACTION, " - "},
  {TokenType::SUBTRACTION_ASSIGNMENT, "-="},
  {TokenType::DECREMENT_PREFIX, "--"},
  {TokenType::DECREMENT_POSTFIX, "--"},
  {TokenType::ADDITION, " + "},
  {TokenType::ADDITION_ASSIGNMENT, " += "},
  {TokenType::INCREMENT_PREFIX, "++"},
  {TokenType::INCREMENT_POSTFIX, "++"},
  {TokenType::ASSIGNMENT, " = "},
  {TokenType::EQUAL, " == "},
  {TokenType::OPEN_BRACE, "{\n"},
  {TokenType::CLOSE_BRACE, "}\n"},
  {TokenType::BITWISE_OR, "|"},
  {TokenType::BITWISE_OR_ASSIGNMENT, "|="},
  {TokenType::LOGICAL_OR, " || "},
  {TokenType::OPEN_BRACKET, "["},
  {TokenType::CLOSE_BRACKET, "]"},
  {TokenType::BACK_SLASH, "\\"},
  {TokenType::COLON, ":"},
  {TokenType::SEMICOLON, ";"},
  {TokenType::NEWLINE, "\n"},
  {TokenType::LESS_THAN, " < "},
  {TokenType::LESS_THAN_EQUAL, " <= "},
  {TokenType::SHIFT_LEFT, " << "},
  {TokenType::SHIFT_LEFT_ASSIGNMENT, " <<= "},
  {TokenType::GREATER_THAN, " > "},
  {TokenType::GREATER_THAN_EQUAL, " >= "},
  {TokenType::SHIFT_RIGHT, " >> "},
  {TokenType::SHIFT_RIGHT_ASSIGNMENT, " >>= "},
  {TokenType::TERNARY, " ? "},
  {TokenType::COMMA, ", "},
  {TokenType::DOT, "."},
  {TokenType::PTR_MEMBER_ACCESS, "->"},
  {TokenType::DIVISION, " / "},
  {TokenType::DIVISION_ASSIGNMENT, " /= "},
  {TokenType::AS, "as "},
  {TokenType::BREAK, "break"},
  {TokenType::CASE, "case "},
  {TokenType::CREATE, "create "},
  {TokenType::CONTINUE, "continue"},
  {TokenType::DEFAULT, "default"},
  {TokenType::DOUBLE_TYPE, "double"},
  {TokenType::ELIF, "elif "},
  {TokenType::ELSE, "else "},
  {TokenType::ENUM, "enum "},
  {TokenType::FOR, "for "},
  {TokenType::FUNC, "func "},
  {TokenType::IF, "if "},
  {TokenType::INT8_TYPE, "int8"},
  {TokenType::INT16_TYPE, "int16"},
  {TokenType::INT32_TYPE, "int32"},
  {TokenType::INT64_TYPE, "int64"},
  {TokenType::UINT8_TYPE, "uint8"},
  {TokenType::UINT16_TYPE, "uint16"},
  {TokenType::UINT32_TYPE, "uint32"},
  {TokenType::UINT64_TYPE, "uint64"},
  {TokenType::INCLUDE, "include "},
  {TokenType::RETURN, "return"},
  {TokenType::STRUCT, "struct "},
  {TokenType::SWITCH, "switch "},
  {TokenType::TEMPLATE, "template "},
  {TokenType::WHILE, "while "},
  {TokenType::POINTER, "ptr"},
  {TokenType::VOID, "void"},
  {TokenType::BOOL, "bool"},
  {TokenType::NULL_PTR, "nullptr"},
  {TokenType::REFERENCE, "ref"},
  {TokenType::STRING_LITERAL, "\""},
  {TokenType::CHAR_LITERAL, "'"},
  {TokenType::IDENTIFIER, "identifier"},
  {TokenType::TYPE, "type"},
};
