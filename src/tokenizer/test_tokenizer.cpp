#include <catch2/catch_test_macros.hpp>
#include "tokenizer.hpp"

TokenType firstToken(const char* c) {
   Tokenizer tokenizer{c};
   return tokenizer.tokenizeNext().type;
}

TEST_CASE("Symbols", "[tokenizer]") {
   REQUIRE(firstToken("!") == TokenType::NOT);
   REQUIRE(firstToken("@") == TokenType::ADDRESS_OF);
   REQUIRE(firstToken("%") == TokenType::MODULO);
   REQUIRE(firstToken("^") == TokenType::POINTER);
   REQUIRE(firstToken("&") == TokenType::AMPERSAND);
   REQUIRE(firstToken("*") == TokenType::MULTIPLICATION);
   REQUIRE(firstToken("()") == TokenType::OPEN_PAREN);
   REQUIRE(firstToken(")") == TokenType::CLOSE_PAREN);
   REQUIRE(firstToken("_") == TokenType::IDENTIFIER);
   REQUIRE(firstToken("+") == TokenType::PLUS);
   REQUIRE(firstToken("~") == TokenType::DEREFERENCE);
   REQUIRE(firstToken("-") == TokenType::MINUS);
   REQUIRE(firstToken("=") == TokenType::EQUAL);
   REQUIRE(firstToken("{") == TokenType::OPEN_BRACE);
   REQUIRE(firstToken("}") == TokenType::CLOSE_BRACE);
   REQUIRE(firstToken("[") == TokenType::OPEN_BRACKET);
   REQUIRE(firstToken("]") == TokenType::CLOSE_BRACKET);
   REQUIRE(firstToken("\\") == TokenType::BACK_SLASH);
   REQUIRE(firstToken("|") == TokenType::BAR);
   REQUIRE(firstToken(";") == TokenType::SEMICOLON);
   REQUIRE(firstToken(":") == TokenType::COLON);
   REQUIRE(firstToken(",") == TokenType::COMMA);
   REQUIRE(firstToken(".") == TokenType::DOT);
   REQUIRE(firstToken("<") == TokenType::LEFT_ARROW);
   REQUIRE(firstToken(">") == TokenType::RIGHT_ARROW);
   REQUIRE(firstToken("?") == TokenType::TERNARY);
   REQUIRE(firstToken("/") == TokenType::DIVISION);
   REQUIRE(firstToken("#") == TokenType::COMMENT);
   REQUIRE(firstToken("$") == TokenType::BAD_VALUE);
   REQUIRE(firstToken("`") == TokenType::BAD_VALUE);
}

TEST_CASE("Keywords", "[tokenizer]") {
   REQUIRE(firstToken("as") == TokenType::AS);
   REQUIRE(firstToken("include") == TokenType::INCLUDE);
   REQUIRE(firstToken("func") == TokenType::FUNC);
   REQUIRE(firstToken("struct") == TokenType::STRUCT);
   REQUIRE(firstToken("template") == TokenType::TEMPLATE);
   REQUIRE(firstToken("create") == TokenType::CREATE);
   REQUIRE(firstToken("if") == TokenType::IF);
   REQUIRE(firstToken("elif") == TokenType::ELIF);
   REQUIRE(firstToken("else") == TokenType::ELSE);
   REQUIRE(firstToken("return") == TokenType::RETURN);
   REQUIRE(firstToken("while") == TokenType::WHILE);
   REQUIRE(firstToken("for") == TokenType::FOR);
   REQUIRE(firstToken("switch") == TokenType::SWITCH);
   REQUIRE(firstToken("case") == TokenType::CASE);
   REQUIRE(firstToken("default") == TokenType::DEFAULT);
   REQUIRE(firstToken("break") == TokenType::BREAK);
   REQUIRE(firstToken("continue") == TokenType::CONTINUE);
}