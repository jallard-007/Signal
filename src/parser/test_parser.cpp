#include <catch2/catch_test_macros.hpp>
#include "parser.hpp"
#include <iostream>

TEST_CASE("getType", "[parser]") {
  const std::string str = " char customType ^^ , int ^^ )";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  {
    Type type;
    REQUIRE(parser.getType(type).type == TokenType::COMMA);
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
    REQUIRE(parser.getType(type).type == TokenType::CLOSE_PAREN);
    auto& tokens = type.tokens;
    REQUIRE(tokens.size() == 3);
    CHECK(tokens[0].type == TokenType::INT_TYPE);
    CHECK(tokens[2].type == TokenType::POINTER);
    CHECK(tokens[2].type == TokenType::POINTER);
  }

}

TEST_CASE("getParams", "[parser]") {
  const std::string str = "(first: int, second: double, third: customType ^^)";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  std::vector<VariableDec> vars;
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

TEST_CASE("Function Declaration", "[parser]") {
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
  REQUIRE(decs[0].func->returnType.tokens.size() == 1);
  CHECK(decs[0].func->returnType.tokens[0].type == TokenType::INT_TYPE);
}

TEST_CASE("Function Call - Base", "[parser]") {
  const std::string str = "functionName();";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON);
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args.list;
  REQUIRE(argsList.size() == 0);
}

TEST_CASE("Function Call - Single Arg", "[parser]") {
  const std::string str = "functionName(arg1);";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON);
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args.list;
  REQUIRE(argsList.size() == 1);
  auto& arg1 = argsList[0];
  REQUIRE(arg1.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(arg1.var) == "arg1");
}

TEST_CASE("Function Call - Multi Arg", "[parser]") {
  const std::string str = "functionName(arg1, arg2);";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON);
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args.list;
  REQUIRE(argsList.size() == 2);
  auto& arg1 = argsList[0];
  REQUIRE(arg1.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(arg1.var) == "arg1");
  auto& arg2 = argsList[1];
  REQUIRE(arg2.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(arg2.var) == "arg2");
}

TEST_CASE("Function Call - Nested", "[parser]") {
  const std::string str = "functionName(arg1[nested()]);";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON);

  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args.list;

  REQUIRE(argsList.size() == 1);
  auto& arg1 = argsList[0];
  REQUIRE(arg1.type == StatementType::ARRAY_ACCESS);
  CHECK(tokenizer.extractToken(arg1.arrAccess->array) == "arg1");

  REQUIRE(arg1.arrAccess->offset.list.size() == 1);
  auto& arg1_arg1 = arg1.arrAccess->offset.list[0];
  REQUIRE(arg1_arg1.type == StatementType::FUNCTION_CALL);
  CHECK(tokenizer.extractToken(arg1_arg1.funcCall->name) == "nested");

  CHECK(arg1_arg1.funcCall->args.list.size() == 0);
}

TEST_CASE("Binary Operators", "[parser]") {
  const std::string str = "4 + 2";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement ; //= parser.parseStatement(TokenType::SEMICOLON);
  REQUIRE(statement.type == StatementType::BINARY_OP);
  auto& binOp = statement.binOp;
  CHECK(binOp->op == TokenType::ADDITION);
  
  REQUIRE(binOp->leftSide->type == StatementType::VALUE);
  CHECK(binOp->leftSide->var.type == TokenType::DECIMAL_NUMBER);
  CHECK(tokenizer.extractToken(binOp->leftSide->var) == "4");

  REQUIRE(binOp->rightSide->type == StatementType::VALUE);
  CHECK(binOp->rightSide->var.type == TokenType::DECIMAL_NUMBER);
  CHECK(tokenizer.extractToken(binOp->rightSide->var) == "2");
}
