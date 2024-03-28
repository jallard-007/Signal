#include <catch2/catch_test_macros.hpp>
#include "parser.hpp"
#include "testingMemPool.hpp"

TEST_CASE("getType", "[parser]") {
  const std::string str = " char ptr ptr , uint32 ptr ptr )";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  {
    TokenList tokens;
    REQUIRE(parser.getType(tokens) == ParseTypeErrorType::NONE);
    REQUIRE(tokens.next);
    REQUIRE(tokens.next->next);
    CHECK_FALSE(tokens.next->next->next);
    CHECK(tokens.token.type == TokenType::POINTER);
    CHECK(tokens.next->token.type == TokenType::POINTER);
    CHECK(tokens.next->next->token.type == TokenType::CHAR_TYPE);
  }
  REQUIRE(tokenizer.peekNext().type == TokenType::COMMA);
  tokenizer.consumePeek();
  {
    TokenList tokens;
    REQUIRE(parser.getType(tokens) == ParseTypeErrorType::NONE);
    REQUIRE(tokens.next);
    REQUIRE(tokens.next->next);
    CHECK_FALSE(tokens.next->next->next);
    CHECK(tokens.token.type == TokenType::POINTER);
    CHECK(tokens.next->token.type == TokenType::POINTER);
    CHECK(tokens.next->next->token.type == TokenType::UINT32_TYPE);
  }
}

TEST_CASE("Function Declaration", "[parser]") {
  const std::string str = "func funcName(first: uint16 ptr): uint32 {}";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  auto& decs = parser.program.decs;
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(decs.next == nullptr);
  REQUIRE(decs.curr.type == GeneralDecType::FUNCTION);
  auto& func = decs.curr.funcDec;
  REQUIRE(func);
  CHECK(tokenizer.extractToken(func->name) == "funcName");

  // check parameters
  REQUIRE(func->params.curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(func->params.curr.varDec);
  CHECK(tokenizer.extractToken(func->params.curr.varDec->name) == "first");
  CHECK(func->params.curr.varDec->type.token.type == TokenType::POINTER);
  REQUIRE(func->params.curr.varDec->type.next);
  CHECK(func->params.curr.varDec->type.next->token.type == TokenType::UINT16_TYPE);

  // check return type
  CHECK(func->returnType.next == nullptr);
  CHECK(func->returnType.token.type == TokenType::UINT32_TYPE);
  CHECK(func->body.scopeStatements.curr.type == StatementType::NONE);
}

TEST_CASE("Function Call - Base", "[parser]") {
  const std::string str = "functionName();";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  Statement statement;
  ParseStatementErrorType errorType = parser.parseStatement(statement);
  REQUIRE(errorType == ParseStatementErrorType::NONE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::EXPRESSION);
  REQUIRE(statement.expression);
  REQUIRE(statement.expression->getType() == ExpressionType::FUNCTION_CALL);
  REQUIRE(statement.expression->getFunctionCall());
  FunctionCall& funcCall = *statement.expression->getFunctionCall();
  CHECK(tokenizer.extractToken(funcCall.name) == "functionName");
  CHECK(funcCall.args.curr.getType() == ExpressionType::NONE);
}

TEST_CASE("Function Call - Single Arg", "[parser]") {
  const std::string str = "functionName(arg1);";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  Statement statement;
  ParseStatementErrorType errorType = parser.parseStatement(statement);
  REQUIRE(errorType == ParseStatementErrorType::NONE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::EXPRESSION);
  REQUIRE(statement.expression);
  REQUIRE(statement.expression->getType() == ExpressionType::FUNCTION_CALL);
  REQUIRE(statement.expression->getFunctionCall());

  auto& argsList = statement.expression->getFunctionCall()->args;
  REQUIRE(argsList.next == nullptr);
  auto& arg1 = argsList.curr;
  REQUIRE(arg1.getType() == ExpressionType::VALUE);
  CHECK(tokenizer.extractToken(arg1.getToken()) == "arg1");
}

TEST_CASE("Function Call - Multi Arg", "[parser]") {
  const std::string str = "functionName(arg1, arg2);";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  Statement statement;
  ParseStatementErrorType errorType = parser.parseStatement(statement);
  REQUIRE(errorType == ParseStatementErrorType::NONE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::EXPRESSION);
  REQUIRE(statement.expression);
  REQUIRE(statement.expression->getType() == ExpressionType::FUNCTION_CALL);
  REQUIRE(statement.expression->getFunctionCall());

  auto& argsList = statement.expression->getFunctionCall()->args;
  auto& arg1 = argsList.curr;
  REQUIRE(arg1.getType() == ExpressionType::VALUE);
  CHECK(tokenizer.extractToken(arg1.getToken()) == "arg1");

  REQUIRE(argsList.next);
  auto& arg2 = *argsList.next;
  REQUIRE(arg2.curr.getType() == ExpressionType::VALUE);
  CHECK(tokenizer.extractToken(arg2.curr.getToken()) == "arg2");
}

TEST_CASE("Function Call - Nested", "[parser]") {
  const std::string str = "functionName(arg1[nested()]);";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  Statement statement;
  ParseStatementErrorType errorType = parser.parseStatement(statement);
  REQUIRE(errorType == ParseStatementErrorType::NONE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::EXPRESSION);
  REQUIRE(statement.expression);
  REQUIRE(statement.expression->getType() == ExpressionType::FUNCTION_CALL);
  REQUIRE(statement.expression->getFunctionCall());

  auto& argsList = statement.expression->getFunctionCall()->args;
  auto& arg1 = argsList.curr;
  REQUIRE(arg1.getType() == ExpressionType::ARRAY_ACCESS);
  REQUIRE(arg1.getArrayAccess());
  CHECK(tokenizer.extractToken(arg1.getArrayAccess()->array.getToken()) == "arg1");

  REQUIRE(arg1.getArrayAccess()->offset.getType() == ExpressionType::FUNCTION_CALL);
  auto arg1_arg1 = arg1.getArrayAccess()->offset.getFunctionCall();
  REQUIRE(arg1_arg1);
  CHECK(tokenizer.extractToken(arg1_arg1->name) == "nested");
  CHECK(arg1_arg1->args.curr.getType() == ExpressionType::NONE);
}

TEST_CASE("Expressions", "[parser]") {
  // basic bin op
  {
    const std::string str = " 4 + 4 ;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    CHECK(expression.getBinOp()->op.type == TokenType::ADDITION);
    auto binOp = expression.getBinOp();
    REQUIRE(binOp);
    CHECK(binOp->op.type == TokenType::ADDITION);
    REQUIRE(binOp->leftSide.getType() == ExpressionType::VALUE);
    CHECK(binOp->leftSide.getToken().type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(binOp->leftSide.getToken()) == "4");

    REQUIRE(binOp->rightSide.getType() == ExpressionType::VALUE);
    CHECK(binOp->rightSide.getToken().type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(binOp->rightSide.getToken()) == "4");
  }

   // operator with higher precedence on right node
  {
    const std::string str = " x - function(var) * 9;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());

    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    auto binOp = expression.getBinOp();
    REQUIRE(binOp);
    CHECK(binOp->op.type == TokenType::SUBTRACTION);

    REQUIRE(binOp->leftSide.getType() == ExpressionType::VALUE);
    CHECK(tokenizer.extractToken(binOp->leftSide.getToken()) == "x");

    REQUIRE(binOp->rightSide.getType() == ExpressionType::BINARY_OP);
    REQUIRE(binOp->rightSide.getBinOp());
    CHECK(binOp->rightSide.getBinOp()->op.type == TokenType::MULTIPLICATION);

    auto& rl = binOp->rightSide.getBinOp()->leftSide;
    REQUIRE(rl.getType() == ExpressionType::FUNCTION_CALL);
    REQUIRE(rl.getFunctionCall());
    REQUIRE(tokenizer.extractToken(rl.getFunctionCall()->name) == "function");
    CHECK(rl.getFunctionCall()->args.next == nullptr);
    REQUIRE(rl.getFunctionCall()->args.curr.getType() == ExpressionType::VALUE);
    CHECK(tokenizer.extractToken(rl.getFunctionCall()->args.curr.getToken()) == "var");

    auto& rr = binOp->rightSide.getBinOp()->rightSide;
    CHECK(rr.getType() == ExpressionType::VALUE);
    CHECK(tokenizer.extractToken(rr.getToken()) == "9");
  }

  { // array access with postfix index expression
    const std::string str = "content[position++];";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
  }

  { // not
    const std::string str = "!content";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    CHECK(expression.getType() == ExpressionType::UNARY_OP);
    REQUIRE(expression.getUnOp());
    CHECK(expression.getUnOp()->operand.getType() == ExpressionType::VALUE);
    CHECK(expression.getUnOp()->operand.getToken().type == TokenType::IDENTIFIER);
  }

   // operator with higher precedence on left node
  {
    const std::string str = " x * function(var) - 9;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());

    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    auto binOp = expression.getBinOp();
    REQUIRE(binOp);
    CHECK(binOp->op.type == TokenType::SUBTRACTION);

    REQUIRE(binOp->rightSide.getType() == ExpressionType::VALUE);
    CHECK(tokenizer.extractToken(binOp->rightSide.getToken()) == "9");

    REQUIRE(binOp->leftSide.getType() == ExpressionType::BINARY_OP);
    REQUIRE(binOp->leftSide.getBinOp());
    CHECK(binOp->leftSide.getBinOp()->op.type == TokenType::MULTIPLICATION);

    auto& rl = binOp->leftSide.getBinOp()->rightSide;
    REQUIRE(rl.getType() == ExpressionType::FUNCTION_CALL);
    REQUIRE(rl.getFunctionCall());
    REQUIRE(tokenizer.extractToken(rl.getFunctionCall()->name) == "function");
    CHECK(rl.getFunctionCall()->args.next == nullptr);
    REQUIRE(rl.getFunctionCall()->args.curr.getType() == ExpressionType::VALUE);
    CHECK(tokenizer.extractToken(rl.getFunctionCall()->args.curr.getToken()) == "var");

    auto& rr = binOp->leftSide.getBinOp()->leftSide;
    CHECK(rr.getType() == ExpressionType::VALUE);
    CHECK(tokenizer.extractToken(rr.getToken()) == "x");
  }
}

TEST_CASE("Expected tokens/expressions", "[parser]") {
  { // missing semicolon
    const std::string str = " var - 9 thing() ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].tokenWhereExpected.position == 9);
  }

  { // missing semicolon
    const std::string str = " var: int  thing: other;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 16);
    CHECK(parser.expected[0].expectedTokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  { // missing part of expression
    const std::string str = " var - ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    CHECK(errorType == ParseExpressionErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 7);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = "  + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    CHECK(errorType == ParseExpressionErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 2);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var * + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    CHECK(errorType == ParseExpressionErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 7);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var || thing * + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    CHECK(errorType == ParseExpressionErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 16);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var + / var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    CHECK(errorType == ParseExpressionErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 7);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var || / + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    CHECK(errorType == ParseExpressionErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing expression for keyword
    const std::string str = " if () { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 5);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing expression for keyword
    const std::string str = " if { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 4);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing expression for keyword
    const std::string str = " for { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 5);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  { // missing expression for keyword
    const std::string str = " while  } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing scope for keyword
    const std::string str = " while 1 }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 9);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }
  
  { // missing close brace for function
    const std::string str = " func ti(): int { while (1) {} ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 31);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::CLOSE_BRACE);
  }

  { // missing close brace for struct
    const std::string str = "struct t { var:int; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 20);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::CLOSE_BRACE);
  }
  
  { // semicolon missing after return value
    const std::string str = " return 1 1;  } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 10);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::SEMICOLON);
  }

  { // empty paren in expression
    const std::string str = "x + ();";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    auto& ex = parser.expected;
    REQUIRE(ex.size() == 1);
    CHECK(ex[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(ex[0].tokenWhereExpected.position == 5);
  }

  { // missing colon
    const std::string str = " thing ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::COLON);
  }

  { // missing type
    const std::string str = " thing: ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::TYPE);
  }

  { // missing type in template
    const std::string str = "template [] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenWhereExpected.position == 10);
    CHECK(parser.expected[0].expectedTokenType == TokenType::IDENTIFIER);
  }

  { // trailing comma in template type
    const std::string str = "template [ T , ] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenWhereExpected.position == 15);
    CHECK(parser.expected[0].expectedTokenType == TokenType::IDENTIFIER);
  }

  { // leading comma in template type
    const std::string str = "template [ , T ] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenWhereExpected.position == 11);
    CHECK(parser.expected[0].expectedTokenType == TokenType::IDENTIFIER);
  }

  { // missing comma in template type
    const std::string str = "template [ T T ] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenWhereExpected.position == 13);
    CHECK(parser.expected[0].expectedTokenType == TokenType::COMMA);
  }

  { // extra commas in function call
    const std::string str = " functionName( , thing , ); ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);

    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(parser.expected[0].tokenWhereExpected.position == 15);
  }

  { // missing function name
    const std::string str = " func (): int { var : int; } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::IDENTIFIER);
    CHECK(parser.expected[0].tokenWhereExpected.position == 6);
  }

  { // missing open paren
    const std::string str = " func name ): int { var : int; } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::OPEN_PAREN);
    CHECK(parser.expected[0].tokenWhereExpected.position == 11);
  }

  { // missing expression
    const std::string str = " var:int = ; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    CHECK(errorType == ParseStatementErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(parser.expected[0].tokenWhereExpected.position == 11);
  }

  { // doesn't make sense
    const std::string str = " var:int = x:int ;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedTokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenWhereExpected.position == 12);
  }

  { //  global type not delimited 
    const std::string str = " var: int ptr ptr ref";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].expectedTokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].tokenWhereExpected.position == 21);
  }

  { // semicolon in function call
    const std::string str = " functionName( thing ; , ); ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    CHECK(parser.parseStatement(statement) == ParseStatementErrorType::REPORTED);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].tokenWhereExpected.position == 21);
    CHECK(parser.expected[0].expectedTokenType == TokenType::CLOSE_PAREN);
  }
}

TEST_CASE("Unexpected tokens", "[parser]") {
  { // unexpected top level token
    const std::string str = " while (1) { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.unexpected.size() == 1);
    CHECK(parser.unexpected[0].token.position == 1);
    CHECK(parser.unexpected[0].token.type == TokenType::WHILE);
  }

  { // invalid top level operation
    const std::string str = "var:int; ++var;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.unexpected.size() == 1);
    CHECK(parser.unexpected[0].token.position == 9);
    CHECK(parser.unexpected[0].token.type == TokenType::INCREMENT_PREFIX);
  }
}

TEST_CASE("Struct Declaration", "[parser]") {
  {
    const std::string str = "struct sName {\n" \
    " func funcName(): int {} \n" \
    " var: int ;\n" \
    "}";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.empty());
    REQUIRE(parser.unexpected.empty());
    auto& s = parser.program.decs;
    REQUIRE(s.curr.type == GeneralDecType::STRUCT);
    REQUIRE(s.curr.structDec);
    CHECK(tokenizer.extractToken(s.curr.structDec->name) == "sName");
    auto& sd = s.curr.structDec->decs;
    CHECK(sd.type == StructDecType::FUNC);
    REQUIRE(sd.next);
    CHECK(sd.next->type  == StructDecType::VAR);
    CHECK(sd.next->next == nullptr);
  }
}

TEST_CASE("Template Declaration", "[parser]") {
  const std::string str = "template [T] func functionName(): T {" \
  " doStuff:int = 34;" \
  " other = stuff * another;" \
  "}";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  REQUIRE(parser.unexpected.empty());
  REQUIRE(parser.expected.empty());
  auto& t = parser.program.decs.curr;
  REQUIRE(t.type == GeneralDecType::TEMPLATE);
  REQUIRE(t.tempDec);
  CHECK(tokenizer.extractToken(t.tempDec->templateTypes.token) == "T");

  CHECK(t.tempDec->templateTypes.next == nullptr);
  CHECK(parser.program.decs.next == nullptr);

  REQUIRE_FALSE(t.tempDec->isStruct);
  REQUIRE(t.tempDec->funcDec.body.scopeStatements.next);
  CHECK(t.tempDec->funcDec.body.scopeStatements.next->next == nullptr);
}

TEST_CASE("Variable Declaration", "[parser]") {
  const std::string str = "thing: stuff;";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  REQUIRE(parser.expected.empty());
  REQUIRE(parser.unexpected.empty());
  CHECK(parser.program.decs.next == nullptr);
  CHECK(parser.program.decs.curr.type == GeneralDecType::VARIABLE);
}

TEST_CASE("Keywords", "[parser]") {
  {
    const std::string str = "if 1 {} }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    parser.parseStatement(statement);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    CHECK(statement.type == StatementType::CONTROL_FLOW);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    CHECK(statement.controlFlow->conditional->ifStatement.condition.getType() == ExpressionType::VALUE);
    CHECK(statement.controlFlow->conditional->elifStatement == nullptr);
    CHECK(statement.controlFlow->conditional->elseStatement == nullptr);
  }

  // structs not currently supported
  // {
  //   const std::string str = "return {thing, 0}; ";
  //   Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  //   Parser parser{tokenizer, memPool};
  //   Statement statement;
  //   parser.parseStatement(statement);
  //   CHECK(parser.unexpected.empty());
  //   CHECK(parser.expected.empty());
  //   CHECK(statement.type == StatementType::CONTROL_FLOW);
  //   REQUIRE(statement.controlFlow);
  //   REQUIRE(statement.controlFlow->type == ControlFlowStatementType::RETURN_STATEMENT);
  //   CHECK(statement.controlFlow->returnStatement->returnValue.getType() == ExpressionType::STRUCT_LITERAL);
  //   REQUIRE(statement.controlFlow->returnStatement->returnValue.getArrayOrStructLiteral());
  // }

  {
    const std::string str = "for (i : int = 0; i < 34; ++i) {doSomething.something(); } }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    Statement statement;
    parser.parseStatement(statement);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    CHECK(statement.type == StatementType::CONTROL_FLOW);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::FOR_LOOP);
    auto& forLoop = statement.controlFlow->forLoop;
    CHECK(forLoop->initialize.type == StatementType::VARIABLE_DEC);
    CHECK(forLoop->statement.condition.getType() == ExpressionType::BINARY_OP);
    CHECK(forLoop->iteration.getType() == ExpressionType::UNARY_OP);
    CHECK(forLoop->statement.body.scopeStatements.curr.type == StatementType::EXPRESSION);
    CHECK(forLoop->statement.body.scopeStatements.next == nullptr);
  }
}

TEST_CASE("For Loop", "[parser]") {
  const std::string str = "for (i: uint32_t = tokenStartPos + 1; i < position && j < MIN_CHARS_TO_DISAMBIG; ++i) {chars[j] = content[i];} } ";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  Statement statement;
  parser.parseStatement(statement);
  CHECK(parser.expected.empty());
  CHECK(parser.unexpected.empty());
}

TEST_CASE("Switch Statement", "[parser]") {
  const std::string str = "switch x { case 3 case 4 {} default {} } ";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
  Parser parser{tokenizer, memPool};
  Statement statement;
  parser.parseStatement(statement);
  CHECK(parser.expected.empty());
  CHECK(parser.unexpected.empty());
}
