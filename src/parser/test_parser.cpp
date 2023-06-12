#include <catch2/catch_test_macros.hpp>
#include "parser.hpp"
#include <iostream>

NodeMemPool memPool;

TEST_CASE("getType", "[parser]") {
  const std::string str = " char customType ptr ptr , int ptr ptr )";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  {
    Type type;
    REQUIRE(parser.getType(type).type == TokenType::COMMA);
    auto& tokens = type.tokens;
    REQUIRE(tokens.next);
    REQUIRE(tokens.next->next);
    REQUIRE(tokens.next->next->next);
    CHECK_FALSE(tokens.next->next->next->next);
    CHECK(tokens.curr.type == TokenType::CHAR_TYPE);
    CHECK(tokens.next->curr.type == TokenType::IDENTIFIER);
    CHECK(tokenizer.extractToken(tokens.next->curr) == "customType");
    CHECK(tokens.next->next->curr.type == TokenType::POINTER);
    CHECK(tokens.next->next->next->curr.type == TokenType::POINTER);
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
    CHECK(tokens.curr.type == TokenType::INT_TYPE);
    CHECK(tokens.next->curr.type == TokenType::POINTER);
    CHECK(tokens.next->next->curr.type == TokenType::POINTER);
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
  REQUIRE(vars.curr.varDec);
  CHECK(tokenizer.extractToken(vars.curr.varDec->name) == "first");
  CHECK_FALSE(vars.curr.varDec->type.tokens.next);
  REQUIRE(vars.curr.varDec->type.tokens.curr.type == TokenType::INT_TYPE);
  StatementList * next = vars.next;
  REQUIRE(next);
  REQUIRE(next->curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(next->curr.varDec);
  CHECK(tokenizer.extractToken(next->curr.varDec->name) == "second");
  CHECK_FALSE(next->curr.varDec->type.tokens.next);
  REQUIRE(next->curr.varDec->type.tokens.curr.type == TokenType::DOUBLE_TYPE);
  next = next->next;
  REQUIRE(next);
  REQUIRE(next->curr.type == StatementType::VARIABLE_DEC);
  REQUIRE(next->curr.varDec);
  CHECK(tokenizer.extractToken(next->curr.varDec->name) == "third");
  REQUIRE(next->curr.varDec->type.tokens.next);
  REQUIRE(next->curr.varDec->type.tokens.next->next);
  CHECK_FALSE(next->curr.varDec->type.tokens.next->next->next);
  REQUIRE(next->curr.varDec->type.tokens.curr.type == TokenType::IDENTIFIER);
  CHECK(tokenizer.extractToken(next->curr.varDec->type.tokens.curr) == "customType");
  REQUIRE(next->curr.varDec->type.tokens.next->curr.type == TokenType::POINTER);
  REQUIRE(next->curr.varDec->type.tokens.next->next->curr.type == TokenType::POINTER);
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
  REQUIRE(func->params.curr.varDec);
  CHECK(tokenizer.extractToken(func->params.curr.varDec->name) == "first");
  REQUIRE(func->params.curr.varDec->type.tokens.next);
  CHECK(func->params.curr.varDec->type.tokens.curr.type == TokenType::INT_TYPE);
  CHECK(func->params.curr.varDec->type.tokens.next->curr.type == TokenType::POINTER);

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
  CHECK(tokenizer.extractToken(arg1.var) == "arg1");
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
  CHECK(tokenizer.extractToken(arg1.var) == "arg1");
  REQUIRE(argsList.next);
  auto& arg2 = *argsList.next;
  REQUIRE(arg2.curr.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(arg2.curr.var) == "arg2");
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
    CHECK(binOp->op == TokenType::ADDITION);

    REQUIRE(binOp->leftSide);
    REQUIRE(binOp->leftSide.type == StatementType::VALUE);
    CHECK(binOp->leftSide.var.type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(binOp->leftSide.var) == "4");

    REQUIRE(binOp->rightSide);
    REQUIRE(binOp->rightSide.type == StatementType::VALUE);
    CHECK(binOp->rightSide.var.type == TokenType::DECIMAL_NUMBER);
    CHECK(tokenizer.extractToken(binOp->rightSide.var) == "4");
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
     CHECK(binOp->op == TokenType::SUBTRACTION);

     REQUIRE(binOp->leftSide);
     REQUIRE(binOp->leftSide.type == StatementType::VALUE);
     CHECK(binOp->leftSide.var.type == TokenType::IDENTIFIER);
     CHECK(tokenizer.extractToken(binOp->leftSide.var) == "x");

     REQUIRE(binOp->rightSide);
     REQUIRE(binOp->rightSide.type == StatementType::BINARY_OP);
     REQUIRE(binOp->rightSide.binOp);
     CHECK(binOp->rightSide.binOp->op == TokenType::MULTIPLICATION);

     auto& rl = binOp->rightSide.binOp->leftSide;
     REQUIRE(rl.type == StatementType::FUNCTION_CALL);
     REQUIRE(rl.funcCall);
     REQUIRE(tokenizer.extractToken(rl.funcCall->name) == "function");
     CHECK(rl.funcCall->args.next == nullptr);
     REQUIRE(rl.funcCall->args.curr.type == StatementType::VALUE);
     REQUIRE(rl.funcCall->args.curr.var.type == TokenType::IDENTIFIER);

     auto& rr = binOp->rightSide.binOp->rightSide;
     CHECK(rr.type == StatementType::VALUE);
     CHECK(rr.var.type == TokenType::DECIMAL_NUMBER);

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
     CHECK(binOp->op == TokenType::SUBTRACTION);

     REQUIRE(binOp->rightSide.type == StatementType::VALUE);
     CHECK(binOp->rightSide.var.type == TokenType::DECIMAL_NUMBER);

     CHECK(binOp->leftSide.type == StatementType::BINARY_OP);
     REQUIRE(binOp->leftSide.binOp);
     CHECK(binOp->leftSide.binOp->op == TokenType::MULTIPLICATION);

     auto& ll = binOp->leftSide.binOp->leftSide;
     REQUIRE(ll.type == StatementType::VALUE);
     CHECK(ll.var.type == TokenType::IDENTIFIER);

     auto& lr = binOp->leftSide.binOp->rightSide;
     REQUIRE(lr.type == StatementType::FUNCTION_CALL);
     REQUIRE(lr.funcCall);
     CHECK(tokenizer.extractToken(lr.funcCall->name) == "function");
     CHECK(lr.funcCall->args.next == nullptr);
     CHECK(lr.funcCall->args.curr.type == StatementType::VALUE);
     CHECK(lr.funcCall->args.curr.var.type == TokenType::IDENTIFIER);
   }
}

TEST_CASE("Expected tokens", "[parser]") {
  {
    const std::string str = " x var - 9;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].line == 1);
    CHECK(parser.expected[0].column == 4);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  {
    const std::string str = " var - 9 thing() ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 10);
    CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
    CHECK(parser.expected[0].expectedType == ExpectedType::TOKEN);
  }

  {
    const std::string str = " var - ; ";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    REQUIRE(parser.expected.size() == 1);
    CHECK(parser.expected[0].column == 8);
    CHECK(parser.expected[0].expectedType == ExpectedType::EXPRESSION);
  }

  {
    const std::string str = "\n\n x + () \n4 ;";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    
    parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
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
  REQUIRE(t.temp->templateIdentifiers.curr.type == StatementType::VALUE);
  CHECK(tokenizer.extractToken(t.temp->templateIdentifiers.curr.var) == "T");
  REQUIRE(t.temp->dec.decType == DecType::FUNCTION);
  REQUIRE(t.temp->dec.func);
  CHECK(t.temp->dec.func->body.scopeStatements.next);
}

TEST_CASE("Variable Declaration", "[parser]") {
  const std::string str = "thing: stuff = 23 + other()";
  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  parser.parse();
  REQUIRE(parser.expected.size() == 1);
  CHECK(parser.expected[0].tokenType == TokenType::SEMICOLON);
  CHECK(parser.expected[0].column == 28);
  REQUIRE(parser.unexpected.empty());
  REQUIRE(parser.program.decs.size() == 1);
  auto& d = parser.program.decs[0];
  CHECK(d.decType == DecType::STATEMENT);
  REQUIRE(d.statement);
  CHECK(d.statement->type == StatementType::BINARY_OP);
  REQUIRE(d.statement->binOp);
  CHECK(d.statement->binOp->leftSide.type == StatementType::VARIABLE_DEC);
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
    REQUIRE(s.keywBody);
    CHECK(s.keywBody->keyword == TokenType::IF);
    CHECK(s.keywBody->header.type == StatementType::WRAPPED_VALUE);
    REQUIRE(s.keywBody->header.wrapped);
    CHECK(s.keywBody->header.wrapped->type == StatementType::VALUE);
    REQUIRE(s.keywBody->body);
    CHECK(s.keywBody->body.type == StatementType::SCOPE);
  }

  {
    const std::string str = "return [thing, 0]; }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    CHECK(parser.unexpected.empty());
    CHECK(parser.expected.empty());
    CHECK(s.type == StatementType::KEY_W_BODY);
    REQUIRE(s.keywBody);
    CHECK(s.keywBody->keyword == TokenType::RETURN);
    REQUIRE(s.keywBody->header);
    CHECK(s.keywBody->header.type == StatementType::ARRAY_OR_STRUCT_LITERAL);
    REQUIRE(s.keywBody->header.arrOrStructLiteral);
    CHECK(s.keywBody->header.arrOrStructLiteral->list.next);
    REQUIRE(!s.keywBody->body);
  }

  {
    const std::string str = "for (i : int = 0; i < 34; ++i) {doSomething.something(); } }";
    Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
    Parser parser{tokenizer, memPool};
    Statement s = parser.parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
    CHECK(parser.expected.empty());
    CHECK(parser.unexpected.empty());
    REQUIRE(s.type == StatementType::KEY_W_BODY);
    REQUIRE(s.keywBody);
    CHECK(s.keywBody->keyword == TokenType::FOR);
    REQUIRE(s.keywBody->header.type == StatementType::FOR_LOOP_HEADER);
    REQUIRE(s.keywBody->header.list);
    REQUIRE(s.keywBody->header.list->list.next);
    CHECK(s.keywBody->header.list->list.next->next);
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

TEST_CASE("Big Boi", "[parser]") {
  const std::string str =
"func checkForRegistrationOfEvent(eventCode: int): int ptr {"
"  currRegistration: RegistrationNode ptr = registrationList;"
"  while (currRegistration) {"
"    if (~(~currRegistration).registration == eventCode) {"
"      array[length - 1] = (~currRegistration).registration.registrationCode;"
"    }"
"  }"
"  return array;"
"}";

  Tokenizer tokenizer{"./src/parser/test_parser.cpp",  str};
  Parser parser{tokenizer, memPool};
  parser.parse();

  Program program;
  Tokenizer f{"", str};

  // function object
  f.tokenizeNext();
  FunctionDec functionDec{f.tokenizeNext()};
  f.tokenizeNext();


  // param
  VariableDec param{f.tokenizeNext()};
  f.tokenizeNext();

  param.type.tokens.curr = f.tokenizeNext();
  functionDec.params.curr.type = StatementType::VARIABLE_DEC;
  functionDec.params.curr.varDec = &param;
  f.tokenizeNext();
  f.tokenizeNext();

  // return type
  functionDec.returnType.tokens.curr = f.tokenizeNext();
  TokenList g;
  g.curr = f.tokenizeNext();
  functionDec.returnType.tokens.next = &g;
  f.tokenizeNext();


  auto& bS = functionDec.body;

  // "  currRegistration: RegistrationNode^ = registrationList;"
  auto& currRegDec = bS.scopeStatements.curr;
  VariableDec varDec1{f.tokenizeNext()};
  f.tokenizeNext();
  varDec1.type.tokens.curr = f.tokenizeNext();
  TokenList t;
  t.curr = f.tokenizeNext();
  varDec1.type.tokens.next = &t;
  BinOp bOp1{f.tokenizeNext().type};
  bOp1.leftSide.type = StatementType::VARIABLE_DEC;
  bOp1.leftSide.varDec = &varDec1;
  bOp1.rightSide.type = StatementType::VALUE;
  bOp1.rightSide.var = f.tokenizeNext();
  f.tokenizeNext();
  currRegDec.type = StatementType::BINARY_OP;
  currRegDec.binOp = &bOp1;

// "  while (currRegistration) {"
  KeywordWithBody whileLoop{f.tokenizeNext().type};
  StatementList sdg;
  sdg.curr.type = StatementType::KEY_W_BODY;
  sdg.curr.keywBody = &whileLoop;
  bS.scopeStatements.next = &sdg;
  f.tokenizeNext();
  whileLoop.header.type = StatementType::WRAPPED_VALUE;
  Statement st454{f.tokenizeNext()};
  whileLoop.header.wrapped = &st454;
  whileLoop.body.type = StatementType::SCOPE;
  Scope scp123;
  whileLoop.body.scope = &scp123;
  f.tokenizeNext();
  f.tokenizeNext();

// "    if (~(~currRegistration).registration == eventCode) {"
  KeywordWithBody ifCond{f.tokenizeNext().type};
  auto& sp87 = scp123.scopeStatements.curr;
  f.tokenizeNext();
  ifCond.header.type = StatementType::WRAPPED_VALUE;
  ifCond.body.type = StatementType::SCOPE;
  Scope scp95;
  ifCond.body.scope = &scp95;

  UnOp unp123{f.tokenizeNext().type};
  f.tokenizeNext();

  UnOp unp1233{f.tokenizeNext().type};
  unp1233.operand.type = StatementType::VALUE;
  unp1233.operand.var = f.tokenizeNext();
  f.tokenizeNext();
  Statement st312{&unp1233};

  BinOp bin2232{f.tokenizeNext().type};
  bin2232.leftSide.type = StatementType::WRAPPED_VALUE;
  bin2232.leftSide.wrapped = &st312;
  bin2232.rightSide.type = StatementType::VALUE;
  bin2232.rightSide.var = f.tokenizeNext();
  unp123.operand.type = StatementType::BINARY_OP;
  unp123.operand.binOp = &bin2232;

  BinOp bin75{f.tokenizeNext().type};
  bin75.rightSide.type = StatementType::VALUE;
  bin75.rightSide.var = f.tokenizeNext();
  bin75.leftSide.type = StatementType::UNARY_OP;
  bin75.leftSide.unOp = &unp123;

  Statement sg235{&bin75};
  ifCond.header.wrapped = &sg235;

  sp87.type = StatementType::KEY_W_BODY;
  sp87.keywBody = &ifCond;

  f.tokenizeNext(); // (
  f.tokenizeNext(); // {

// "      array[length - 1] = (~currRegistration).registration.registrationCode;"

  //  array[length - 1]
  ArrayAccess arrAcc0{f.tokenizeNext()};
  f.tokenizeNext();
  BinOp bOp2{TokenType::SUBTRACTION};
  bOp2.leftSide.type = StatementType::VALUE;
  bOp2.leftSide.var = f.tokenizeNext();
  f.tokenizeNext();
  bOp2.rightSide.type = StatementType::VALUE;
  bOp2.rightSide.var = f.tokenizeNext();
  f.tokenizeNext(); // [

  BinOp b12{f.tokenizeNext().type}; // =
  b12.leftSide.type = StatementType::ARRAY_ACCESS;
  b12.leftSide.arrAccess = &arrAcc0;

  f.tokenizeNext();
  UnOp up84{f.tokenizeNext().type};
  up84.operand.type = StatementType::VALUE;
  up84.operand.var = f.tokenizeNext();
  Statement si876{&up84};
  f.tokenizeNext();
  BinOp bOp4{f.tokenizeNext().type};
  bOp4.leftSide.type = StatementType::WRAPPED_VALUE;
  bOp4.leftSide.wrapped = &si876;
  bOp4.rightSide.type = StatementType::VALUE;
  bOp4.rightSide.var = f.tokenizeNext();
  BinOp bOp5{f.tokenizeNext().type};
  bOp5.leftSide.type = StatementType::BINARY_OP;
  bOp5.leftSide.binOp = &bOp4;
  bOp5.rightSide.type = StatementType::VALUE;
  bOp5.rightSide.var = f.tokenizeNext();

  b12.rightSide.type = StatementType::BINARY_OP;
  b12.rightSide.binOp = &bOp5;

    
  auto& r = scp95.scopeStatements.curr;
  r.type = StatementType::BINARY_OP;
  r.binOp = &b12;
  f.tokenizeNext(); // ;
  f.tokenizeNext(); // }
  f.tokenizeNext(); // }

  StatementList hhg;
  Statement& j = hhg.curr;
  scp95.scopeStatements.next = &hhg;

  j.type = StatementType::KEY_W_BODY;
  j.keywBody = &whileLoop;

  KeywordWithBody kd{f.tokenizeNext().type};
  StatementList ad23;
  ad23.curr.type = StatementType::KEY_W_BODY;
  ad23.curr.keywBody = &kd;
  sdg.next = &ad23;
  kd.header.type = StatementType::VALUE;
  kd.header.var = f.tokenizeNext();
  f.tokenizeNext(); // ;
  f.tokenizeNext(); // }

  StatementList erw;
  Statement& b = erw.curr;
  hhg.next = &erw;
  b.type = StatementType::KEY_W_BODY;
  b.keywBody = &kd;

  program.decs.emplace_back(&functionDec);
  program.operator==(parser.program);
  if (program == parser.program) {
    // do something
  }
  CHECK(program == parser.program);
}
