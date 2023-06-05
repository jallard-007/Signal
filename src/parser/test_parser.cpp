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
    std::vector<Type> types;
    REQUIRE(parser.getType(types) == TokenType::COMMA);
    REQUIRE(types.size() == 4);
    CHECK(types[0].tp == TokenType::CHAR_TYPE);
    CHECK(types[1].str == "customType");
    CHECK(types[2].tp == TokenType::POINTER);
    CHECK(types[3].tp == TokenType::POINTER);
  }
  {
    std::vector<Type> types;
    REQUIRE(parser.getType(types) == TokenType::CLOSE_PAREN);
    REQUIRE(types.size() == 3);
    CHECK(types[0].tp == TokenType::INT_TYPE);
    CHECK(types[2].tp == TokenType::POINTER);
    CHECK(types[2].tp == TokenType::POINTER);
  }

}

TEST_CASE("Unit Test - getParams", "[parser]") {
  const std::string str = "(first: int, second: double, third: customType ^^)";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  std::vector<Variable> vars;
  REQUIRE(parser.getParams(vars) == true);
  REQUIRE(vars.size() == 3);

  CHECK(vars[0].name == "first");
  REQUIRE(vars[0].type.size() == 1);
  CHECK(vars[0].type[0].tp == TokenType::INT_TYPE);

  CHECK(vars[1].name == "second");
  REQUIRE(vars[1].type.size() == 1);
  CHECK(vars[1].type[0].tp == TokenType::DOUBLE_TYPE);

  CHECK(vars[2].name == "third");
  REQUIRE(vars[2].type.size() == 3);
  CHECK(vars[2].type[0].str == "customType");
  CHECK(vars[2].type[1].tp == TokenType::POINTER);
  CHECK(vars[2].type[2].tp == TokenType::POINTER);

}

TEST_CASE("Unit Test - Parse Function Declaration", "[parser]") {
  const std::string str = "func funcName(first: int^): int";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  parser.parse();
  auto& decs = parser.program.decs;
  REQUIRE(decs.size() == 1);
  CHECK(decs[0].type == DecType::FUNCTION);
  CHECK(decs[0].func->name == "funcName");
  REQUIRE(decs[0].func->params.size() == 1);
  CHECK(decs[0].func->params[0].name == "first");
  REQUIRE(decs[0].func->params[0].type.size() == 2);
  CHECK(decs[0].func->params[0].type[0].tp == TokenType::INT_TYPE);
  CHECK(decs[0].func->params[0].type[1].tp == TokenType::POINTER);
  CHECK(decs[0].func->returnType.tp == TokenType::INT_TYPE);
}
