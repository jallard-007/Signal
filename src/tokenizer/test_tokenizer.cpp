#include <catch2/catch_test_macros.hpp>
#include "tokenizer.hpp"

TokenType firstToken(const char* c) {
   Tokenizer tokenizer{c};
   return tokenizer.tokenizeNext().type;
}

TEST_CASE("Unit Test - Special", "[tokenizer]") {
   CHECK(firstToken("$") == TokenType::BAD_VALUE);
   CHECK(firstToken("`") == TokenType::BAD_VALUE);
   CHECK(firstToken("") == TokenType::END_OF_FILE);
}

TEST_CASE("Unit Test - Literals", "[tokenizer]") {
   CHECK(firstToken("'") == TokenType::CHAR_LITERAL);
   CHECK(firstToken("\"") == TokenType::STRING_LITERAL);
   CHECK(firstToken("0") == TokenType::DECIMAL_NUMBER);
   CHECK(firstToken("0b") == TokenType::BINARY_NUMBER);
   CHECK(firstToken("1b") == TokenType::DECIMAL_NUMBER);
   CHECK(firstToken("0x") == TokenType::HEX_NUMBER);
   CHECK(firstToken("1x") == TokenType::DECIMAL_NUMBER);
}

TEST_CASE("Keywords", "[tokenizer]") {
   CHECK(firstToken("as") == TokenType::AS);
   CHECK(firstToken("break") == TokenType::BREAK);
   CHECK(firstToken("case") == TokenType::CASE);
   CHECK(firstToken("continue") == TokenType::CONTINUE);
   CHECK(firstToken("create") == TokenType::CREATE);
   CHECK(firstToken("default") == TokenType::DEFAULT);
   CHECK(firstToken("elif") == TokenType::ELIF);
   CHECK(firstToken("else") == TokenType::ELSE);
   CHECK(firstToken("enum") == TokenType::ENUM);
   CHECK(firstToken("false") == TokenType::FALSE);
   CHECK(firstToken("for") == TokenType::FOR);
   CHECK(firstToken("func") == TokenType::FUNC);
   CHECK(firstToken("function") == TokenType::IDENTIFIER);
   CHECK(firstToken("if") == TokenType::IF);
   CHECK(firstToken("ifelif") == TokenType::IDENTIFIER);
   CHECK(firstToken("null") == TokenType::NULL_);
   CHECK(firstToken("include") == TokenType::INCLUDE);
   CHECK(firstToken("return") == TokenType::RETURN);
   CHECK(firstToken("returns") == TokenType::IDENTIFIER);
   CHECK(firstToken("struct") == TokenType::STRUCT);
   CHECK(firstToken("switch") == TokenType::SWITCH);
   CHECK(firstToken("template") == TokenType::TEMPLATE);
   CHECK(firstToken("templat ") == TokenType::IDENTIFIER);
   CHECK(firstToken("true") == TokenType::TRUE);
   CHECK(firstToken("while") == TokenType::WHILE);
}

TEST_CASE("Unit Test - General", "[tokenizer]") {
   CHECK(firstToken("_") == TokenType::IDENTIFIER);
   CHECK(firstToken("#") == TokenType::COMMENT);
   CHECK(firstToken("\n") == TokenType::NEWLINE);
   CHECK(firstToken("(") == TokenType::OPEN_PAREN);
   CHECK(firstToken(")") == TokenType::CLOSE_PAREN);
   CHECK(firstToken("{") == TokenType::OPEN_BRACE);
   CHECK(firstToken("}") == TokenType::CLOSE_BRACE);
   CHECK(firstToken("[") == TokenType::OPEN_BRACKET);
   CHECK(firstToken("]") == TokenType::CLOSE_BRACKET);
   CHECK(firstToken("\\") == TokenType::BACK_SLASH);
   CHECK(firstToken("@") == TokenType::ADDRESS_OF);
   CHECK(firstToken("~") == TokenType::DEREFERENCE);
   CHECK(firstToken(":") == TokenType::COLON);
   CHECK(firstToken(";") == TokenType::SEMICOLON);
   CHECK(firstToken("?") == TokenType::TERNARY);
   CHECK(firstToken(",") == TokenType::COMMA);
   CHECK(firstToken(".") == TokenType::DOT);
}

TEST_CASE("Unit Test - Arithmetic", "[tokenizer]") {
   CHECK(firstToken("+") == TokenType::PLUS);
   CHECK(firstToken("-") == TokenType::MINUS);
   CHECK(firstToken("*") == TokenType::MULTIPLICATION);
   CHECK(firstToken("/") == TokenType::DIVISION);
   CHECK(firstToken("%") == TokenType::MODULO);
   CHECK(firstToken("&") == TokenType::BITWISE_AND);
   CHECK(firstToken("|") == TokenType::BITWISE_OR);
   CHECK(firstToken("<<") == TokenType::SHIFT_LEFT);
   CHECK(firstToken(">>") == TokenType::SHIFT_RIGHT);
}
TEST_CASE("Unit Test - Assignments", "[tokenizer]") {
   CHECK(firstToken("=") == TokenType::ASSIGNMENT);
   CHECK(firstToken("+=") == TokenType::ADDITION_ASSIGNMENT);
   CHECK(firstToken("-=") == TokenType::SUBTRACTION_ASSIGNMENT);
   CHECK(firstToken("*=") == TokenType::MULTIPLICATION_ASSIGNMENT);
   CHECK(firstToken("/=") == TokenType::DIVISION_ASSIGNMENT);
   CHECK(firstToken("%=") == TokenType::MODULO_ASSIGNMENT);
   CHECK(firstToken("|=") == TokenType::BITWISE_OR_ASSIGNMENT);
   CHECK(firstToken("&=") == TokenType::BITWISE_AND_ASSIGNMENT);
   CHECK(firstToken("<<=") == TokenType::SHIFT_LEFT_ASSIGNMENT);
   CHECK(firstToken(">>=") == TokenType::SHIFT_RIGHT_ASSIGNMENT);
}

TEST_CASE("Unit Test - Logical", "[tokenizer]") {
   CHECK(firstToken("!") == TokenType::NOT);
   CHECK(firstToken("==") == TokenType::EQUAL);
   CHECK(firstToken("!=") == TokenType::NOT_EQUAL);
   CHECK(firstToken("&&") == TokenType::LOGICAL_AND);
   CHECK(firstToken("||") == TokenType::LOGICAL_OR);
   CHECK(firstToken("<") == TokenType::LESS_THAN);
   CHECK(firstToken("<=") == TokenType::LESS_THAN_EQUAL);
   CHECK(firstToken(">") == TokenType::GREATER_THAN);
   CHECK(firstToken(">=") == TokenType::GREATER_THAN_EQUAL);
}

TEST_CASE("Unit Test - Types", "[tokenizer]") {
   CHECK(firstToken("char") == TokenType::CHAR_TYPE);
   CHECK(firstToken("int") == TokenType::INT_TYPE);
   CHECK(firstToken("double") == TokenType::DOUBLE_TYPE);
   CHECK(firstToken("^") == TokenType::POINTER);
}
