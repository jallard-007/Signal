#include <catch2/catch_test_macros.hpp>
#include "tokenizer.hpp"

TokenType firstToken(const char* c) {
   Tokenizer tokenizer{c};
   return tokenizer.tokenizeNext().type;
}

TEST_CASE("Symbols", "[tokenizer]") {
   CHECK(firstToken("!") == TokenType::NOT);
   CHECK(firstToken("@") == TokenType::ADDRESS_OF);
   CHECK(firstToken("%") == TokenType::MODULO);
   CHECK(firstToken("^") == TokenType::POINTER);
   CHECK(firstToken("&") == TokenType::AMPERSAND);
   CHECK(firstToken("*") == TokenType::MULTIPLICATION);
   CHECK(firstToken("(") == TokenType::OPEN_PAREN);
   CHECK(firstToken(")") == TokenType::CLOSE_PAREN);
   CHECK(firstToken("_") == TokenType::IDENTIFIER);
   CHECK(firstToken("+") == TokenType::PLUS);
   CHECK(firstToken("~") == TokenType::DEREFERENCE);
   CHECK(firstToken("-") == TokenType::MINUS);
   CHECK(firstToken("=") == TokenType::EQUAL);
   CHECK(firstToken("{") == TokenType::OPEN_BRACE);
   CHECK(firstToken("}") == TokenType::CLOSE_BRACE);
   CHECK(firstToken("[") == TokenType::OPEN_BRACKET);
   CHECK(firstToken("]") == TokenType::CLOSE_BRACKET);
   CHECK(firstToken("\\") == TokenType::BACK_SLASH);
   CHECK(firstToken("|") == TokenType::BAR);
   CHECK(firstToken(";") == TokenType::SEMICOLON);
   CHECK(firstToken(":") == TokenType::COLON);
   CHECK(firstToken(",") == TokenType::COMMA);
   CHECK(firstToken(".") == TokenType::DOT);
   CHECK(firstToken("'") == TokenType::CHAR_LITERAL);
   CHECK(firstToken("\"") == TokenType::STRING_LITERAL);
   CHECK(firstToken("<") == TokenType::LEFT_ARROW);
   CHECK(firstToken(">") == TokenType::RIGHT_ARROW);
   CHECK(firstToken("?") == TokenType::TERNARY);
   CHECK(firstToken("/") == TokenType::DIVISION);
   CHECK(firstToken("#") == TokenType::COMMENT);
   CHECK(firstToken("$") == TokenType::BAD_VALUE);
   CHECK(firstToken("`") == TokenType::BAD_VALUE);
}

TEST_CASE("Keywords", "[tokenizer]") {
   CHECK(firstToken("as") == TokenType::AS);
   CHECK(firstToken("include") == TokenType::INCLUDE);
   CHECK(firstToken("func") == TokenType::FUNC);
   CHECK(firstToken("function") == TokenType::IDENTIFIER);
   CHECK(firstToken("struct") == TokenType::STRUCT);
   CHECK(firstToken("template") == TokenType::TEMPLATE);
   CHECK(firstToken("templat ") == TokenType::IDENTIFIER);
   CHECK(firstToken("create") == TokenType::CREATE);
   CHECK(firstToken("if") == TokenType::IF);
   CHECK(firstToken("elif") == TokenType::ELIF);
   CHECK(firstToken("else") == TokenType::ELSE);
   CHECK(firstToken("ifelif") == TokenType::IDENTIFIER);
   CHECK(firstToken("return") == TokenType::RETURN);
   CHECK(firstToken("returns") == TokenType::IDENTIFIER);
   CHECK(firstToken("while") == TokenType::WHILE);
   CHECK(firstToken("for") == TokenType::FOR);
   CHECK(firstToken("switch") == TokenType::SWITCH);
   CHECK(firstToken("case") == TokenType::CASE);
   CHECK(firstToken("default") == TokenType::DEFAULT);
   CHECK(firstToken("break") == TokenType::BREAK);
   CHECK(firstToken("continue") == TokenType::CONTINUE);
}