#include <catch2/catch_test_macros.hpp>
#include "parser.hpp"
#include <iostream>

NodeMemPool memPool;

TEST_CASE("getType", "[parser]") {
  const std::string str = " char ptr ptr , int ptr ptr )";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  {
    Type type;
    REQUIRE(parser.getType(type).type == TokenType::COMMA);
    auto& tokens = type.tokens;
    REQUIRE(tokens.next);
    REQUIRE(tokens.next->next);
    CHECK_FALSE(tokens.next->next->next);
    CHECK(tokens.curr.type == TokenType::POINTER);
    CHECK(tokens.next->curr.type == TokenType::POINTER);
    CHECK(tokens.next->next->curr.type == TokenType::CHAR_TYPE);
  }
  REQUIRE(tokenizer.peekNext().type == TokenType::COMMA);
  tokenizer.consumePeek();
  {
    Type type;
    REQUIRE(parser.getType(type).type == TokenType::CLOSE_PAREN);
    auto& tokens = type.tokens;
    REQUIRE(tokens.next);
    REQUIRE(tokens.next->next);
    CHECK_FALSE(tokens.next->next->next);
    CHECK(tokens.curr.type == TokenType::POINTER);
    CHECK(tokens.next->curr.type == TokenType::POINTER);
    CHECK(tokens.next->next->curr.type == TokenType::INT_TYPE);
  }

}

TEST_CASE("getParams", "[parser]") {
  const std::string str = "first: int, second: double, third: customType ptr ptr)";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  StatementList vars;
  REQUIRE(parser.getStatements(vars, TokenType::COMMA, TokenType::CLOSE_PAREN) == true);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());

  REQUIRE(vars.curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(vars.curr.dec);
  CHECK(tokenizer.extractToken(vars.curr.dec->varDec->name) == "first");
  CHECK_FALSE(vars.curr.dec->varDec->type.tokens.next);
  REQUIRE(vars.curr.dec->varDec->type.tokens.curr.type == TokenType::INT_TYPE);
  StatementList * next = vars.next;
  REQUIRE(next);
  REQUIRE(next->curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(next->curr.dec);
  REQUIRE(next->curr.dec->varDec);
  CHECK(tokenizer.extractToken(next->curr.dec->varDec->name) == "second");
  CHECK_FALSE(next->curr.dec->varDec->type.tokens.next);
  REQUIRE(next->curr.dec->varDec->type.tokens.curr.type == TokenType::DOUBLE_TYPE);
  next = next->next;
  REQUIRE(next);
  REQUIRE(next->curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(next->curr.dec);
  REQUIRE(next->curr.dec->varDec);
  CHECK(tokenizer.extractToken(next->curr.dec->varDec->name) == "third");
  REQUIRE(next->curr.dec->varDec->type.tokens.next);
  REQUIRE(next->curr.dec->varDec->type.tokens.next->next);
  CHECK_FALSE(next->curr.dec->varDec->type.tokens.next->next->next);
  REQUIRE(next->curr.dec->varDec->type.tokens.curr.type == TokenType::POINTER);
  REQUIRE(next->curr.dec->varDec->type.tokens.next->curr.type == TokenType::POINTER);
  REQUIRE(next->curr.dec->varDec->type.tokens.next->next->curr.type == TokenType::IDENTIFIER);
  CHECK(tokenizer.extractToken(next->curr.dec->varDec->type.tokens.next->next->curr) == "customType");
}

TEST_CASE("Function Declaration", "[parser]") {
  const std::string str = "func funcName(first: int ptr): int {}";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  auto& decs = parser.program.decs;
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(decs.size() == 1);
  REQUIRE(decs[0].decType == DecType::FUNCTION);
  auto& func = decs[0].func;
  REQUIRE(func);
  CHECK(tokenizer.extractToken(func->name) == "funcName");

  // check parameters
  REQUIRE(func->params.curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(func->params.curr.dec);
  REQUIRE(func->params.curr.dec->varDec);
  CHECK(tokenizer.extractToken(func->params.curr.dec->varDec->name) == "first");
  CHECK(func->params.curr.dec->varDec->type.tokens.curr.type == TokenType::POINTER);
  REQUIRE(func->params.curr.dec->varDec->type.tokens.next);
  CHECK(func->params.curr.dec->varDec->type.tokens.next->curr.type == TokenType::INT_TYPE);

  // check return type
  CHECK_FALSE(func->returnType.tokens.next);
  CHECK(func->returnType.tokens.curr.type == TokenType::INT_TYPE);

  CHECK(func->body.scopeStatements.curr.type == StatementType::NONE);
}

TEST_CASE("Function Call - Base", "[parser]") {
  const std::string str = "functionName();";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);

  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;
  CHECK(argsList.curr.type == StatementType::NONE);
}

TEST_CASE("Function Call - Single Arg", "[parser]") {
  const std::string str = "functionName(arg1);";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);

  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;
  REQUIRE(argsList.next == nullptr);
  auto& arg1 = argsList.curr;
  REQUIRE(arg1.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(*arg1.var) == "arg1");
}

TEST_CASE("Function Call - Multi Arg", "[parser]") {
  const std::string str = "functionName(arg1, arg2);";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;
  auto& arg1 = argsList.curr;
  REQUIRE(arg1.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(*arg1.var) == "arg1");
  REQUIRE(argsList.next);
  auto& arg2 = *argsList.next;
  REQUIRE(arg2.curr.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(*arg2.curr.var) == "arg2");
}

TEST_CASE("Function Call - Nested", "[parser]") {
  const std::string str = "functionName(arg1[nested()]);";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  Statement statement = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.unexpected.empty());
  CHECK(parser.expected.empty());
  REQUIRE(statement.type == StatementType::FUNCTION_CALL);
  REQUIRE(statement.funcCall);
  CHECK(tokenizer.extractToken(statement.funcCall->name) == "functionName");
  auto& argsList = statement.funcCall->args;

  REQUIRE(argsList.next == nullptr);
  auto& arg1 = argsList.curr;
  REQUIRE(arg1.type == StatementType::ARRAY_ACCESS);
  REQUIRE(arg1.arrAccess);
  CHECK(tokenizer.extractToken(arg1.arrAccess->array) == "arg1");
  REQUIRE(arg1.arrAccess->offset.type == StatementType::FUNCTION_CALL);
  auto& arg1_arg1 = arg1.arrAccess->offset.funcCall;
  REQUIRE(arg1_arg1);
  CHECK(tokenizer.extractToken(arg1_arg1->name) == "nested");
  CHECK(arg1_arg1->args.curr.type == StatementType::NONE);
}

TEST_CASE("Binary Operators", "[parser]") {
  // basic bin op
  {
    const std::string str = " 4 + 4 ;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
    REQUIRE(statement.type == StatementType::BINARY_OP);
    auto &binOp = statement.binOp;
    REQUIRE(binOp);
    CHECK(binOp->op.type == TokenType::ADDITION);

    REQUIRE(binOp->leftSide);
    REQUIRE(binOp->leftSide.type == StatementType::VALUE);
    CHECK(binOp->leftSide.var->type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(*binOp->leftSide.var) == "4");

    REQUIRE(binOp->rightSide);
    REQUIRE(binOp->rightSide.type == StatementType::VALUE);
    CHECK(binOp->rightSide.var->type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(*binOp->rightSide.var) == "4");
  }

   // operator with higher precedence on right node
   {
     const std::string str = " x - function(var) * 9;";
     Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
     Parser parser{tokenizer, memPool};
     Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
     CHECK(parser.unexpected.empty());
     CHECK(parser.expected.empty());
     REQUIRE(statement.type == StatementType::BINARY_OP);
     auto &binOp = statement.binOp;
     REQUIRE(binOp);
     CHECK(binOp->op.type == TokenType::SUBTRACTION);

     REQUIRE(binOp->leftSide);
     REQUIRE(binOp->leftSide.type == StatementType::VALUE);
     CHECK(binOp->leftSide.var->type == TokenType::IDENTIFIER);
     CHECK(tokenizer.extractToken(*binOp->leftSide.var) == "x");

     REQUIRE(binOp->rightSide);
     REQUIRE(binOp->rightSide.type == StatementType::BINARY_OP);
     REQUIRE(binOp->rightSide.binOp);
     CHECK(binOp->rightSide.binOp->op.type == TokenType::MULTIPLICATION);

     auto& rl = binOp->rightSide.binOp->leftSide;
     REQUIRE(rl.type == StatementType::FUNCTION_CALL);
     REQUIRE(rl.funcCall);
     REQUIRE(tokenizer.extractToken(rl.funcCall->name) == "function");
     CHECK(rl.funcCall->args.next == nullptr);
     REQUIRE(rl.funcCall->args.curr.type == StatementType::VALUE);
     REQUIRE(rl.funcCall->args.curr.var->type == TokenType::IDENTIFIER);

     auto& rr = binOp->rightSide.binOp->rightSide;
     CHECK(rr.type == StatementType::VALUE);
     CHECK(rr.var->type == TokenType::DECIMAL_NUMBER);

   }

   // operator with higher precedence on left node
   {
     const std::string str = " x * function(var) - 9;";
     Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
     Parser parser{tokenizer, memPool};
     Statement statement{parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)};
     CHECK(parser.expected.empty());

     REQUIRE(statement.type == StatementType::BINARY_OP);
     auto &binOp = statement.binOp;
     REQUIRE(binOp);
     CHECK(binOp->op.type == TokenType::SUBTRACTION);

     REQUIRE(binOp->rightSide.type == StatementType::VALUE);
     CHECK(binOp->rightSide.var->type == TokenType::DECIMAL_NUMBER);

     CHECK(binOp->leftSide.type == StatementType::BINARY_OP);
     REQUIRE(binOp->leftSide.binOp);
     CHECK(binOp->leftSide.binOp->op.type == TokenType::MULTIPLICATION);

     auto& ll = binOp->leftSide.binOp->leftSide;
     REQUIRE(ll.type == StatementType::VALUE);
     CHECK(ll.var->type == TokenType::IDENTIFIER);

     auto& lr = binOp->leftSide.binOp->rightSide;
     REQUIRE(lr.type == StatementType::FUNCTION_CALL);
     REQUIRE(lr.funcCall);
     CHECK(tokenizer.extractToken(lr.funcCall->name) == "function");
     CHECK(lr.funcCall->args.next == nullptr);
     CHECK(lr.funcCall->args.curr.type == StatementType::VALUE);
     CHECK(lr.funcCall->args.curr.var->type == TokenType::IDENTIFIER);
   }
}

TEST_CASE("Expected tokens/expressions", "[parser]") {
  { // missing semicolon
    const std::string str = " var - 9 thing() ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 10);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  { // missing semicolon
    const std::string str = " var: int  thing: other;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 12);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  { // missing part of expression
    const std::string str = " var - ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = "  + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 3);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var * + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var || thing * + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 17);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var + / var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing part of expression
    const std::string str = " var || / + var ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 2);
    CHECK(parser.expected[0].column == 9);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(parser.expected[1].column == 11);
    CHECK(parser.expected[1].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing expression for keyword
    const std::string str = " if () { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 6);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing expression for keyword
    const std::string str = " if { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 5);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing expression for keyword
    const std::string str = " for { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 6);
    CHECK(parser.expected[0].expectedType == ExpectedType::FOR_LOOP_HEADER);
  }

  { // missing expression for keyword
    const std::string str = " while  } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 9);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  { // missing scope for keyword
    const std::string str = " while 1 }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 10);
    CHECK(parser.expected[0].expectedType == ExpectedType::SCOPE);
  }
  
  { // missing close brace for function
    const std::string str = " func ti(): int { while (1) {} ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 32);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenType == TokenType::CLOSE_BRACE);
  }

  { // missing close brace for struct
    const std::string str = "struct t { var:int; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 21);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenType == TokenType::CLOSE_BRACE);
  }
  
  { // semicolon missing after return value
    const std::string str = " return 1 1;  } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 11);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
  }

  { // empty paren in expression
    const std::string str = "x + ();";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    auto& ex = parser.expected;
    REQUIRE(ex.size() == 1);
    CHECK(ex[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(ex[0].column == 6);
  }

  { // missing colon
    const std::string str = " thing ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenType == TokenType::COLON);
  }

  { // missing type
    const std::string str = " thing: ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].tokenType == TokenType::IDENTIFIER);
  }

  { // missing type in template
    const std::string str = "template [] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].column == 11);
    CHECK(parser.expected[0].tokenType == TokenType::IDENTIFIER);
  }

  { // trailing comma in template type
    const std::string str = "template [ T , ] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].column == 16);
    CHECK(parser.expected[0].tokenType == TokenType::IDENTIFIER);
  }

  { // leading comma in template type
    const std::string str = "template [ , T ] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].column == 12);
    CHECK(parser.expected[0].tokenType == TokenType::IDENTIFIER);
  }

  { // missing comma in template type
    const std::string str = "template [ T T ] struct thingTemplate { var:int; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
    CHECK(parser.expected[0].column == 14);
    CHECK(parser.expected[0].tokenType == TokenType::COMMA);
  }

  { // extra commas in function call
    const std::string str = " functionName( , thing , ); ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp", str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 2);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
    CHECK(parser.expected[0].column == 16);
    CHECK(parser.expected[1].expectedType == ExpectedType::EXPRESSION);
    CHECK(parser.expected[1].column == 26);
  }
}

TEST_CASE("Unexpected tokens", "[parser]") {
  { // unexpected top level token
    const std::string str = " while (1) { do.something(); } ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.unexpected.size() == 1);
    CHECK(parser.unexpected[0].token.linePos == 2);
    CHECK(parser.unexpected[0].token.type == TokenType::WHILE);
  }

  { // invalid top level operation
    const std::string str = "var:int; ++var;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.unexpected.size() == 1);
    CHECK(parser.unexpected[0].token.linePos == 10);
    CHECK(parser.unexpected[0].token.type == TokenType::INCREMENT_PREFIX);
  }
}

TEST_CASE("Struct Declaration", "[parser]") {
  {
    const std::string str = "struct sName {\n" \
    " func funcName(): int {} \n" \
    " var: int ;\n" \
    "}";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parse();
    REQUIRE(parser.expected.empty());
    REQUIRE(parser.unexpected.empty());
    auto& s = parser.program.decs;
    REQUIRE(s.size() == 1);
    REQUIRE(s[0].decType == DecType::STRUCT);
    REQUIRE(s[0].struc);
    CHECK(tokenizer.extractToken(s[0].struc->name) == "sName");
    auto& sd = s[0].struc->decs;
    CHECK(sd.size() == 2);
    CHECK(sd[0].decType == DecType::FUNCTION);
    CHECK(sd[1].decType == DecType::VARIABLE_DEC);
  }
}

TEST_CASE("Template Declaration", "[parser]") {
  const std::string str = "template [T] func functionName(): T {" \
  " doStuff:int = 34;" \
  " other = stuff * another;" \
  "}";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  REQUIRE(parser.unexpected.empty());
  REQUIRE(parser.expected.empty());
  REQUIRE(parser.program.decs.size() == 1);
  auto& t = parser.program.decs[0];
  REQUIRE(t.decType == DecType::TEMPLATE);
  REQUIRE(t.temp);
  REQUIRE(t.temp->templateIdentifiers.next == nullptr);
  CHECK(tokenizer.extractToken(t.temp->templateIdentifiers.curr) == "T");
  REQUIRE(t.temp->dec.decType == DecType::FUNCTION);
  REQUIRE(t.temp->dec.func);
  CHECK(t.temp->dec.func->body.scopeStatements.next);
}

TEST_CASE("Variable Declaration", "[parser]") {
  const std::string str = "thing: stuff;";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  REQUIRE(parser.expected.empty());
  REQUIRE(parser.unexpected.empty());
  REQUIRE(parser.program.decs.size() == 1);
  auto& d = parser.program.decs[0];
  CHECK(d.decType == DecType::VARIABLE_DEC);
}

TEST_CASE("Keywords", "[parser]") {
  {
    const std::string str = "if (1) {} }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    CHECK(s.type == StatementType::KEY_W_BODY);
    REQUIRE(s.keyWBody);
    CHECK(s.keyWBody->keyword.type == TokenType::IF);
    CHECK(s.keyWBody->header.type == StatementType::WRAPPED_VALUE);
    REQUIRE(s.keyWBody->header.wrapped);
    CHECK(s.keyWBody->header.wrapped->type == StatementType::VALUE);
  }

  {
    const std::string str = "return [thing, 0]; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    CHECK(s.type == StatementType::KEY_W_BODY);
    REQUIRE(s.keyWBody);
    CHECK(s.keyWBody->keyword.type == TokenType::RETURN);
    REQUIRE(s.keyWBody->header);
    CHECK(s.keyWBody->header.type == StatementType::ARRAY_OR_STRUCT_LITERAL);
    REQUIRE(s.keyWBody->header.arrOrStructLiteral);
    CHECK(s.keyWBody->header.arrOrStructLiteral->list.next);
    CHECK(s.keyWBody->body);
  }

  {
    const std::string str = "for (i : int = 0; i < 34; ++i) {doSomething.something(); } }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    CHECK(parser.expected.empty());
    CHECK(parser.unexpected.empty());
    REQUIRE(s.type == StatementType::KEY_W_BODY);
    REQUIRE(s.keyWBody);
    CHECK(s.keyWBody->keyword.type == TokenType::FOR);
    REQUIRE(s.keyWBody->header.type == StatementType::FOR_LOOP_HEADER);
    REQUIRE(s.keyWBody->header.list);
    REQUIRE(s.keyWBody->header.list->list.next);
    CHECK(s.keyWBody->header.list->list.next->next);
  }
}

TEST_CASE("Array", "[parser]") {
  const std::string str = " [-3 , 23 ]; ";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.expected.empty());
  CHECK(parser.unexpected.empty());
  REQUIRE(s.type == StatementType::ARRAY_OR_STRUCT_LITERAL);
  REQUIRE(s.arrOrStructLiteral->list.next);
  CHECK_FALSE(s.arrOrStructLiteral->list.next->next);
}

TEST_CASE("Scope", "[parser]") {
  const std::string str = " { var:int; } } ";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
  CHECK(parser.expected.empty());
  CHECK(parser.unexpected.empty());
  REQUIRE(s.type == StatementType::SCOPE);
}
