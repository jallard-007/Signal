#include <catch2/catch_test_macros.hpp>
#include "parser.hpp"
#include <iostream>

// "Unit testing" the parser is not really possible since the parser uses the tokenizer as it goes,
// regardless, we will call these tests "Unit tests" since they focus on a specific aspect of the parser
// usually just one or two functions at a time
TEST_CASE("Unit Test - getType", "[parser]") {
  const std::string str = " char customType ^^ , int ^^ )";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  {
    Type type;
    REQUIRE(parser.getType(type) == TokenType::COMMA);
    auto& tokens = type.tokens;
    REQUIRE(tokens.size() == 4);
    CHECK(tokens[0].type == TokenType::CHAR_TYPE);
    CHECK(tokens[1].type == TokenType::IDENTIFIER);
    CHECK(tokenizer.extractToken(tokens[1]) == "customType");
    CHECK(tokens[2].type == TokenType::POINTER);
    CHECK(tokens[3].type == TokenType::POINTER);
  }
  {
    Type type;
    REQUIRE(parser.getType(type) == TokenType::CLOSE_PAREN);
    auto& tokens = type.tokens;
    REQUIRE(tokens.size() == 3);
    CHECK(tokens[0].type == TokenType::INT_TYPE);
    CHECK(tokens[2].type == TokenType::POINTER);
    CHECK(tokens[2].type == TokenType::POINTER);
  }

}

TEST_CASE("Unit Test - getParams", "[parser]") {
  const std::string str = "(first: int, second: double, third: customType ^^)";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  std::vector<Variable> vars;
  REQUIRE(parser.getParams(vars) == true);
  REQUIRE(vars.size() == 3);

  CHECK(tokenizer.extractToken(vars[0].name) == "first");
  REQUIRE(vars[0].type.tokens.size() == 1);
  CHECK(vars[0].type.tokens[0].type == TokenType::INT_TYPE);

  CHECK(tokenizer.extractToken(vars[1].name) == "second");
  REQUIRE(vars[1].type.tokens.size() == 1);
  CHECK(vars[1].type.tokens[0].type == TokenType::DOUBLE_TYPE);

  CHECK(tokenizer.extractToken(vars[2].name) == "third");
  REQUIRE(vars[2].type.tokens.size() == 3);
  CHECK(vars[2].type.tokens[0].type == TokenType::IDENTIFIER);
  CHECK(tokenizer.extractToken(vars[2].type.tokens[0]) == "customType");
  CHECK(vars[2].type.tokens[1].type == TokenType::POINTER);
  CHECK(vars[2].type.tokens[2].type == TokenType::POINTER);

}

TEST_CASE("Unit Test - Parse Function Declaration", "[parser]") {
  const std::string str = "func funcName(first: int^): int";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  parser.parse();
  auto& decs = parser.program.decs;
  REQUIRE(decs.size() == 1);
  CHECK(decs[0].decType == DecType::FUNCTION);
  CHECK(tokenizer.extractToken(decs[0].func->name) == "funcName");

  REQUIRE(decs[0].func->params.size() == 1);
  CHECK(tokenizer.extractToken(decs[0].func->params[0].name) == "first");

  REQUIRE(decs[0].func->params[0].type.tokens.size() == 2);
  CHECK(decs[0].func->params[0].type.tokens[0].type == TokenType::INT_TYPE);
  CHECK(decs[0].func->params[0].type.tokens[1].type == TokenType::POINTER);
  CHECK(decs[0].func->returnType.tokens.size() == 1);
  CHECK(decs[0].func->returnType.tokens[0].type == TokenType::INT_TYPE);

}
