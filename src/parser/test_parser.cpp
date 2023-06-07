#include <catch2/catch_test_macros.hpp>
#include "parser.hpp"

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
  REQUIRE(tokenizer.peekNext().type == TokenType::COMMA);
  tokenizer.consumePeek();
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
  const std::string str = "first: int, second: double, third: customType ^^)";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  std::vector<Statement> vars;
  REQUIRE(parser.getStatements(vars, TokenType::COMMA, TokenType::CLOSE_PAREN) == true);
  REQUIRE(vars.size() == 3);

  REQUIRE(vars[0].type == StatementType::VARIABLE_DEC);
  REQUIRE(vars[0].varDec);
  CHECK(tokenizer.extractToken(vars[0].varDec->name) == "first");
  REQUIRE(vars[0].varDec->type.tokens.size() == 1);
  REQUIRE(vars[0].varDec->type.tokens[0].type == TokenType::INT_TYPE);

  REQUIRE(vars[1].type == StatementType::VARIABLE_DEC);
  REQUIRE(vars[1].varDec);
  CHECK(tokenizer.extractToken(vars[1].varDec->name) == "second");
  REQUIRE(vars[1].varDec->type.tokens.size() == 1);
  REQUIRE(vars[1].varDec->type.tokens[0].type == TokenType::DOUBLE_TYPE);

  REQUIRE(vars[2].type == StatementType::VARIABLE_DEC);
  REQUIRE(vars[2].varDec);
  CHECK(tokenizer.extractToken(vars[2].varDec->name) == "third");
  REQUIRE(vars[2].varDec->type.tokens.size() == 3);
  REQUIRE(vars[2].varDec->type.tokens[0].type == TokenType::IDENTIFIER);
  CHECK(tokenizer.extractToken(vars[2].varDec->type.tokens[0]) == "customType");
  REQUIRE(vars[2].varDec->type.tokens[1].type == TokenType::POINTER);
  REQUIRE(vars[2].varDec->type.tokens[2].type == TokenType::POINTER);
}

TEST_CASE("Function Declaration", "[parser]") {
  const std::string str = "funcName(first: int^): int {";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  REQUIRE(parser.functionDec());
  auto& decs = parser.program.decs;
  REQUIRE(decs.size() == 1);
  CHECK(decs[0].decType == DecType::FUNCTION);
  auto& func = decs[0].func;
  REQUIRE(func);
  CHECK(tokenizer.extractToken(func->name) == "funcName");

  // check return type
  REQUIRE(func->returnType.tokens.size() == 1);
  CHECK(func->returnType.tokens[0].type == TokenType::INT_TYPE);

  // check parameters
  REQUIRE(func->params.size() == 1);
  REQUIRE(func->params[0].type == StatementType::VARIABLE_DEC);
  REQUIRE(func->params[0].varDec);
  CHECK(tokenizer.extractToken(func->params[0].varDec->name) == "first");
  REQUIRE(func->params[0].varDec->type.tokens.size() == 2);
  CHECK(func->params[0].varDec->type.tokens[0].type == TokenType::INT_TYPE);
  CHECK(func->params[0].varDec->type.tokens[1].type == TokenType::POINTER);
}

TEST_CASE("Function Call - Base", "[parser]") {
  const std::string str = "functionName();";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON);
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);

  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args.list;
  REQUIRE(argsList.empty());
}

TEST_CASE("Function Call - Single Arg", "[parser]") {
  const std::string str = "functionName(arg1);";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON);
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);

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
  REQUIRE(statement.funcCall);
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
  REQUIRE(statement.funcCall);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args.list;

  REQUIRE(argsList.size() == 1);
  auto& arg1 = argsList[0];
  REQUIRE(arg1.type == StatementType::ARRAY_ACCESS);
  REQUIRE(arg1.arrAccess);
  CHECK(tokenizer.extractToken(arg1.arrAccess->array) == "arg1");
  REQUIRE(arg1.arrAccess->offset.type == StatementType::FUNCTION_CALL);
  auto& arg1_arg1 = arg1.arrAccess->offset.funcCall;
  REQUIRE(arg1_arg1);
  CHECK(tokenizer.extractToken(arg1_arg1->name) == "nested");
  CHECK(arg1_arg1->args.list.empty());
}

TEST_CASE("Binary Operators", "[parser]") {
  // basic bin op
  {
    const std::string str = " 4 + 4 ;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON)};
    REQUIRE(statement.type == StatementType::BINARY_OP);
    auto &binOp = statement.binOp;
    REQUIRE(binOp);
    CHECK(binOp->op == TokenType::ADDITION);

    REQUIRE(binOp->leftSide);
    REQUIRE(binOp->leftSide->type == StatementType::VALUE);
    CHECK(binOp->leftSide->var.type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(binOp->leftSide->var) == "4");

    REQUIRE(binOp->rightSide);
    REQUIRE(binOp->rightSide->type == StatementType::VALUE);
    CHECK(binOp->rightSide->var.type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(binOp->rightSide->var) == "4");
  }

  // operator with higher precedence on right node
  {
    const std::string str = " x - function(var) * 9;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON)};
    CHECK(parser.expectedStatement.empty());

    REQUIRE(statement.type == StatementType::BINARY_OP);
    auto &binOp = statement.binOp;
    REQUIRE(binOp);
    CHECK(binOp->op == TokenType::SUBTRACTION);

    REQUIRE(binOp->leftSide);
    REQUIRE(binOp->leftSide->type == StatementType::VALUE);
    CHECK(binOp->leftSide->var.type == TokenType::IDENTIFIER);
    CHECK(tokenizer.extractToken(binOp->leftSide->var) == "x");

    REQUIRE(binOp->rightSide);
    REQUIRE(binOp->rightSide->type == StatementType::BINARY_OP);
    CHECK(binOp->rightSide->binOp->op == TokenType::MULTIPLICATION);

    auto& rl = binOp->rightSide->binOp->leftSide;
    REQUIRE(rl);
    REQUIRE(rl->type == StatementType::FUNCTION_CALL);
    REQUIRE(rl->funcCall);
    REQUIRE(tokenizer.extractToken(rl->funcCall->name) == "function");
    REQUIRE(rl->funcCall->args.list.size() == 1);
    REQUIRE(rl->funcCall->args.list[0].type == StatementType::VALUE);
    REQUIRE(rl->funcCall->args.list[0].var.type == TokenType::IDENTIFIER);

    auto& rr = binOp->rightSide->binOp->rightSide;
    REQUIRE(rr);
    CHECK(rr->type == StatementType::VALUE);
    CHECK(rr->var.type == TokenType::DECIMAL_NUMBER);

  }

  // operator with higher precedence on left node
  {
    const std::string str = " x * function(var) - 9;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON)};
    CHECK(parser.expectedStatement.empty());

    REQUIRE(statement.type == StatementType::BINARY_OP);
    auto &binOp = statement.binOp;
    REQUIRE(binOp);
    CHECK(binOp->op == TokenType::SUBTRACTION);

    REQUIRE(binOp->rightSide);
    REQUIRE(binOp->rightSide->type == StatementType::VALUE);
    CHECK(binOp->rightSide->var.type == TokenType::DECIMAL_NUMBER);

    REQUIRE(binOp->leftSide);
    REQUIRE(binOp->leftSide->type == StatementType::BINARY_OP);
    CHECK(binOp->leftSide->binOp->op == TokenType::MULTIPLICATION);

    auto& ll = binOp->leftSide->binOp->leftSide;
    REQUIRE(ll);
    REQUIRE(ll->type == StatementType::VALUE);
    CHECK(ll->var.type == TokenType::IDENTIFIER);

    auto& lr = binOp->leftSide->binOp->rightSide;
    REQUIRE(lr->type == StatementType::FUNCTION_CALL);
    REQUIRE(lr->funcCall);
    CHECK(tokenizer.extractToken(lr->funcCall->name) == "function");
    REQUIRE(lr->funcCall->args.list.size() == 1);
    CHECK(lr->funcCall->args.list[0].type == StatementType::VALUE);
    CHECK(lr->funcCall->args.list[0].var.type == TokenType::IDENTIFIER);
  }
}

TEST_CASE("Expected tokens", "[parser]") {
  {
    const std::string str = " x var - 9;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON)};
    REQUIRE(parser.expectedStatement.size() == 1);
    CHECK(parser.expectedStatement[0].position == 3);
    CHECK(parser.expectedStatement[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expectedStatement[0].expectedType == ExpectedType::END_OF_STATEMENT);
  }

  {
    const std::string str = " var - 9 thing() ; ";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON)};
    REQUIRE(parser.expectedStatement.size() == 1);
    CHECK(parser.expectedStatement[0].position == 9);
    CHECK(parser.expectedStatement[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expectedStatement[0].expectedType == ExpectedType::END_OF_STATEMENT);
  }

  {
    const std::string str = " var - ; ";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON)};
    REQUIRE(parser.expectedStatement.size() == 1);
    CHECK(parser.expectedStatement[0].position == 7);
    CHECK(parser.expectedStatement[0].expectedType == ExpectedType::EXPRESSION);
  }

  {
    // this generates the correct tree
    const std::string str = " var: int = thing; var += 3; var_p:char^ = callFunction(var);}";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    std::vector<Statement> s;
    parser.getStatements(s, TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  }
}
