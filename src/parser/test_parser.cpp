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
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
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
  const std::string str = "func funcName(first: int^): int {}";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  parser.parse();
  auto& decs = parser.program.decs;
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(decs.size() == 1);
  REQUIRE(decs[0].decType == DecType::FUNCTION);
  auto& func = decs[0].func;
  REQUIRE(func.get());
  CHECK(tokenizer.extractToken(func->name) == "funcName");

  // check parameters
  REQUIRE(func->params.size() == 1);
  REQUIRE(func->params[0].type == StatementType::VARIABLE_DEC);
  REQUIRE(func->params[0].varDec);
  CHECK(tokenizer.extractToken(func->params[0].varDec->name) == "first");
  REQUIRE(func->params[0].varDec->type.tokens.size() == 2);
  CHECK(func->params[0].varDec->type.tokens[0].type == TokenType::INT_TYPE);
  CHECK(func->params[0].varDec->type.tokens[1].type == TokenType::POINTER);

  // check return type
  REQUIRE(func->returnType.tokens.size() == 1);
  CHECK(func->returnType.tokens[0].type == TokenType::INT_TYPE);

  CHECK(func->bodyStatements.empty());
}

TEST_CASE("Function Call - Base", "[parser]") {
  const std::string str = "functionName();";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);

  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;
  CHECK(argsList.empty());
}

TEST_CASE("Function Call - Single Arg", "[parser]") {
  const std::string str = "functionName(arg1);";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);

  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;
  REQUIRE(argsList.size() == 1);
  auto& arg1 = argsList[0];
  REQUIRE(arg1.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(arg1.var) == "arg1");
}

TEST_CASE("Function Call - Multi Arg", "[parser]") {
  const std::string str = "functionName(arg1, arg2);";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;
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
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;

  REQUIRE(argsList.size() == 1);
  auto& arg1 = argsList[0];
  REQUIRE(arg1.type == StatementType::ARRAY_ACCESS);
  REQUIRE(arg1.arrAccess);
  CHECK(tokenizer.extractToken(arg1.arrAccess->array) == "arg1");
  REQUIRE(arg1.arrAccess->offset.type == StatementType::FUNCTION_CALL);
  auto& arg1_arg1 = arg1.arrAccess->offset.funcCall;
  REQUIRE(arg1_arg1);
  CHECK(tokenizer.extractToken(arg1_arg1->name) == "nested");
  CHECK(arg1_arg1->args.empty());
}

TEST_CASE("Binary Operators", "[parser]") {
  // basic bin op
  {
    const std::string str = " 4 + 4 ;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
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
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
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
    REQUIRE(binOp->rightSide->binOp);
    CHECK(binOp->rightSide->binOp->op == TokenType::MULTIPLICATION);

    auto& rl = binOp->rightSide->binOp->leftSide;
    REQUIRE(rl);
    REQUIRE(rl->type == StatementType::FUNCTION_CALL);
    REQUIRE(rl->funcCall);
    REQUIRE(tokenizer.extractToken(rl->funcCall->name) == "function");
    REQUIRE(rl->funcCall->args.size() == 1);
    REQUIRE(rl->funcCall->args[0].type == StatementType::VALUE);
    REQUIRE(rl->funcCall->args[0].var.type == TokenType::IDENTIFIER);

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
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
    CHECK(parser.expected.empty());

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
    REQUIRE(lr->funcCall->args.size() == 1);
    CHECK(lr->funcCall->args[0].type == StatementType::VALUE);
    CHECK(lr->funcCall->args[0].var.type == TokenType::IDENTIFIER);
  }
}

TEST_CASE("Expected tokens", "[parser]") {
  {
    const std::string str = " x var - 9;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].line == 1);
    CHECK(parser.expected[0].column == 4);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  {
    const std::string str = " var - 9 thing() ; ";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 10);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  {
    const std::string str = " var - ; ";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  {
    const std::string str = "\n\n x + () \n4 ;";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    
    Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    auto& ex = parser.expected;
    REQUIRE(ex.size() == 2);
    CHECK(ex[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(ex[0].line == 3);
    CHECK(ex[0].column == 7);
    CHECK(ex[1].expectedType == ExpectedType::TOKEN);
    CHECK(ex[1].line == 4);
    CHECK(ex[1].column == 1);
    CHECK(ex[1].tokenType == TokenType::SEMICOLON);
  }
}

TEST_CASE("Unexpected tokens", "[parser]") {
}

TEST_CASE("Struct Declaration", "[parser]") {
  {
    const std::string str = "struct sName {\n" \
    " func funcName(): int {} \n" \
    " var: int ;\n" \
    " struct structName {}\n" \
    "}";
    Tokenizer tokenizer{str};
    Parser parser{tokenizer};
    parser.parse();
    REQUIRE(parser.expected.empty());
    REQUIRE(parser.unexpected.empty());
    auto& s = parser.program.decs;
    REQUIRE(s.size() == 1);
    REQUIRE(s[0].decType == DecType::STRUCT);
    REQUIRE(s[0].struc.get());
    CHECK(tokenizer.extractToken(s[0].struc->name) == "sName");
    auto& sd = s[0].struc->decs;
    CHECK(sd.size() == 3);
    CHECK(sd[0].decType == DecType::FUNCTION);
    CHECK(sd[1].decType == DecType::STATEMENT);
    CHECK(sd[2].decType == DecType::STRUCT);
  }
}

TEST_CASE("Template Declaration", "[parser]") {
  const std::string str = "template [T] func functionName(): T {" \
  " doStuff:int = 34;" \
  " other = stuff * another;" \
  "}";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  parser.parse();
  REQUIRE(parser.unexpected.empty());
  REQUIRE(parser.expected.empty());
  REQUIRE(parser.program.decs.size() == 1);
  auto& t = parser.program.decs[0];
  REQUIRE(t.decType == DecType::TEMPLATE);
  REQUIRE(t.temp.get());
  REQUIRE(t.temp->templateIdentifiers.size() == 1);
  REQUIRE(t.temp->templateIdentifiers[0].type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(t.temp->templateIdentifiers[0].var) == "T");
  REQUIRE(t.temp->dec.decType == DecType::FUNCTION);
  REQUIRE(t.temp->dec.func.get());
  CHECK(t.temp->dec.func->bodyStatements.size() == 2);
}

TEST_CASE("Variable Declaration", "[parser]") {
  const std::string str = "thing: stuff = 23 + other()";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  parser.parse();
  REQUIRE(parser.expected.size() == 1);
  CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
  CHECK(parser.expected[0].column == 28);
  REQUIRE(parser.unexpected.empty());
  REQUIRE(parser.program.decs.size() == 1);
  auto& d = parser.program.decs[0];
  CHECK(d.decType == DecType::STATEMENT);
  REQUIRE(d.statement.get());
  CHECK(d.statement->type == StatementType::BINARY_OP);
  REQUIRE(d.statement->binOp.get());
  REQUIRE(d.statement->binOp->leftSide.get());
  CHECK(d.statement->binOp->leftSide->type == StatementType::VARIABLE_DEC);
}

TEST_CASE("Keywords", "[parser]") {
  const std::string str = "if (1) {} }";
  Tokenizer tokenizer{str};
  Parser parser{tokenizer};
  Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  CHECK(s.type == StatementType::KEY_W_BODY);
  REQUIRE(s.keywBody.get());
  CHECK(s.keywBody->keyword == TokenType::IF);
  REQUIRE(s.keywBody->header.get());
  CHECK(s.keywBody->header->type == StatementType::WRAPPED_VALUE);
  REQUIRE(s.keywBody->header->wrapped.get());
  CHECK(s.keywBody->header->wrapped->type == StatementType::VALUE);
  REQUIRE(s.keywBody->body.get());
  CHECK(s.keywBody->body->type == StatementType::SCOPE);
}
