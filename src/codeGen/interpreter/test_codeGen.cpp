#include <iostream>
#include "codeGen.hpp"
#include "parser/parser.hpp"
#include "testingMemPool.hpp"

#include <catch2/catch_test_macros.hpp>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define bc bytecode_t

#define testBoilerPlate(str) \
  std::vector<Tokenizer> tokenizers; \
  Tokenizer& tokenizer = tokenizers.emplace_back("./src/parser/test_parser.cpp", str); \
  Parser parser{tokenizer, memPool}; \
  Checker checker{parser.program, tokenizers, memPool}; \
  CodeGen codeGen{parser.program, tokenizers, checker.lookUp}; \
  codeGen.tk = &tokenizer

TEST_CASE("constant expressions", "[codeGen]") {
  // should return the actual value of the expression, and the correct type (smallest type that supports that value, min int32_t)
  SECTION("4 + 4") {
    #define EXPRESSION 4 + 4
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("max int32") {
    #define EXPRESSION INT32_MAX
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("min int32") {
    #define EXPRESSION INT32_MIN
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("max uint32") {
    #define EXPRESSION 4294967295
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::UINT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("negative max uint32") {
    #define EXPRESSION -4294967295U
    auto result = (uint32_t)EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::UNARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::UINT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("complex") {
    #define EXPRESSION 2.0 * 100 / 3
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::DOUBLE_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("overflow") {
    #define EXPRESSION 9223372036854775807 + 1
    auto result = (int64_t)((uint64_t)EXPRESSION); // doing cast to bypass compiler warning
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT64_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 1") {
    #define EXPRESSION 55 % 6
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 2") {
    #define EXPRESSION 8 * 5.3
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::DOUBLE_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 3") {
    #define EXPRESSION 23984723 - 9234.2 * 3 || 0x823ff2
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::BOOL);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 4") {
    #define EXPRESSION 0b11111111111 ^ ((0xfffffff + (15 - 0b0101) / 17  + 7) - 2)
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT32_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 5") {
    #define EXPRESSION (false || 0x23423ac ^ 128) + 1843709551615/2
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT64_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 6") {
    #define EXPRESSION (18446551615-123876) + 2
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::INT64_TYPE);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 7") {
    #define EXPRESSION 5 | (0x3a3423 | 0b1) * 102 || 66
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::BOOL);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 8") {
    #define EXPRESSION 5 > 0.000078
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::BOOL);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 9") {
    #define EXPRESSION ((234532 & 234) < 0xB234) && !(34 - 3234.2 * 1.5 - 75)
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::BOOL);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
  SECTION("ben expression 10") {
    #define EXPRESSION 808 - 0b111010010100001110100111111 && true
    auto result = EXPRESSION;
    const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
    #undef EXPRESSION
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.type->token.getType() == TokenType::BOOL);
    CHECK(*(decltype(result) *)expRes.getData() == result);
  }
}

TEST_CASE("variable creation", "[codeGen]") {
  {
    const std::string str = "x:uint32;";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.varDec);
    codeGen.generateVariableDeclaration(*statement.varDec);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 4, 0}});
    CHECK(codeGen.byteCode == expected.byteCode);
  }
  {
    const std::string str = "x:uint32 = 10;";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.varDec);
    codeGen.generateVariableDeclaration(*statement.varDec);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.addBytes({{(bc)OpCode::MOVE_SI, 1, 10}});
    expected.addBytes({{(bc)OpCode::PUSH_D, 1}});
    CHECK(codeGen.byteCode == expected.byteCode);
  }
}

TEST_CASE("jump statements in always true control flow", "[codeGen]") {
  {
    // at the end of the `if (1)` body, it should jump past the else body
    const std::string str = "if (1) {num:uint64;} else {num:uint64;} ";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateStatement(statement);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 8, 0}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, stackPointerIndex, 8, 0}});
    expected.addBytes({{(bc)OpCode::RS_JUMP, 0}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 8, 0}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, stackPointerIndex, 8, 0}});
    CHECK(codeGen.byteCode == expected.byteCode);
  }
  {
    // should jump to the start of the for loop
    const std::string str = "num:uint64; for (;;) {} ";
    testBoilerPlate(str);
    { // add some instructions above the loop to check if jump address is right
      Statement statement;
      ParseStatementErrorType errorType = parser.parseStatement(statement);
      REQUIRE(errorType == ParseStatementErrorType::NONE);
      codeGen.generateStatement(statement);
    }
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);

    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::FOR_LOOP);
    codeGen.generateStatement(statement);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 8, 0}});
    expected.addBytes({{(bc)OpCode::RS_JUMP, 0}});
    CHECK(codeGen.byteCode == expected.byteCode);
  }
  {
    // should jump to the start of the while loop (just after the variable dec)
    const std::string str = "while true { num:uint64; } ";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);

    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::WHILE_LOOP);
    codeGen.generateStatement(statement);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 8, 0}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, stackPointerIndex, 8, 0}});
    expected.addBytes({{(bc)OpCode::RS_JUMP, 0}});
    CHECK(codeGen.byteCode == expected.byteCode);
  }
}

class TestFixture_GetStructInfo {
  public:
  StructInformation structInfo;
  void setUp(const std::string &str) {
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    structInfo = codeGen.getStructInfo("StructName");
  }
};

TEST_CASE_METHOD(TestFixture_GetStructInfo, "getStructInfo", "[codeGen]") {
  SECTION("one") {
    const std::string str = "struct StructName { x: uint32; }";
    setUp(str);
    CHECK(structInfo.size == 4);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["x"] == 0);
  }
  SECTION("two") {
    const std::string str = "struct StructName { y:bool; x: uint32; }";
    setUp(str);
    CHECK(structInfo.size == 8);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["y"] == 0);
    CHECK(structInfo.offsetMap["x"] == 4);
  }
  SECTION("three") {
    const std::string str = "struct StructName { x: uint32; y:bool; }";
    setUp(str);
    CHECK(structInfo.size == 8);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["y"] == 4);
    CHECK(structInfo.offsetMap["x"] == 0);
  }
  SECTION("four") {
    const std::string str = "struct StructName { y:bool; x: uint32; j:bool; }";
    setUp(str);
    CHECK(structInfo.size == 12);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["y"] == 0);
    CHECK(structInfo.offsetMap["x"] == 4);
    CHECK(structInfo.offsetMap["j"] == 8);
  }
  SECTION("five") {
    const std::string str = "struct Thing { y:bool; x: uint32; j:bool; } struct StructName { y:Thing; x: bool; j:uint32 ptr; }";
    setUp(str);
    CHECK(structInfo.size == 24);
    CHECK(structInfo.alignTo == 8);
    CHECK(structInfo.offsetMap["y"] == 0);
    CHECK(structInfo.offsetMap["x"] == 12);
    CHECK(structInfo.offsetMap["j"] == 16);
  }
}

TEST_CASE("short-circuit logical bin ops", "[codeGen]") {
  {
    const std::string str = "x:uint32; y:uint32; if (x && y) { num:uint32; } ";
    testBoilerPlate(str);
    Statement statement_x, statement_y, cond_statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement_x);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    assert(statement_x.type == StatementType::VARIABLE_DEC);
    errorType = parser.parseStatement(statement_y);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    assert(statement_y.type == StatementType::VARIABLE_DEC);
    errorType = parser.parseStatement(cond_statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(cond_statement.controlFlow);
    REQUIRE(cond_statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    std::vector<std::string> locals;
    checker.checkLocalVarDec(tokenizer, *statement_x.varDec, locals);
    checker.checkLocalVarDec(tokenizer, *statement_y.varDec, locals);
    codeGen.generateStatement(statement_x);
    codeGen.generateStatement(statement_y);
    codeGen.byteCode.clear();
    codeGen.generateStatement(cond_statement);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.addBytes({{(bc)OpCode::MOVE, miscRegisterIndex, stackPointerIndex}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, miscRegisterIndex, 4, 0}});
    expected.addBytes({{(bc)OpCode::LOAD_D, 1, 0}});

    // short circuit
    expected.addBytes({{(bc)OpCode::SET_FLAGS, 1}});
    expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

    // load y
    expected.addBytes({{(bc)OpCode::LOAD_D, 2, stackPointerIndex}});

    expected.addBytes({{(bc)OpCode::LOGICAL_AND, 1, 2}});
    expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 4, 0}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, stackPointerIndex, 4, 0}});

    CHECK(codeGen.byteCode == expected.byteCode);
  }
  {
    const std::string str = "x:uint32; y:uint32; if (x && y || x) { num:uint32; } ";
    testBoilerPlate(str);
    Statement statement_x, statement_y, cond_statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement_x);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    errorType = parser.parseStatement(statement_y);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    errorType = parser.parseStatement(cond_statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(cond_statement.controlFlow);
    REQUIRE(cond_statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateStatement(statement_x);
    codeGen.generateStatement(statement_y);
    codeGen.byteCode.clear();
    codeGen.generateStatement(cond_statement);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    expected.addBytes({{(bc)OpCode::MOVE, miscRegisterIndex, stackPointerIndex}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, miscRegisterIndex, 4, 0}});
    expected.addBytes({{(bc)OpCode::LOAD_D, 1, 0}});

    // short circuit
    expected.addBytes({{(bc)OpCode::SET_FLAGS, 1}});
    expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

    expected.addBytes({{(bc)OpCode::LOAD_D, 2, stackPointerIndex}});

    expected.addBytes({{(bc)OpCode::LOGICAL_AND, 1, 2}});
    expected.addBytes({{(bc)OpCode::GET_NE, 3}});

    // short circuit
    expected.addBytes({{(bc)OpCode::SET_FLAGS, 3}});
    expected.addBytes({{(bc)OpCode::RS_JUMP_NE, 0}});

    expected.addBytes({{(bc)OpCode::LOGICAL_OR, 3, 1}});
    expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 4, 0}});
    expected.alignForImm(2, 2);
    expected.addBytes({{(bc)OpCode::ADD_I, stackPointerIndex, 4, 0}});

    CHECK(codeGen.byteCode == expected.byteCode);
  }
}

TEST_CASE("addFunctionSignatureToVirtualStack", "[codeGen]") {
  SECTION("one") {
    const std::string str = "func testFunction(): void { } ";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    auto genDec = checker.lookUp["testFunction"];
    REQUIRE(genDec);
    REQUIRE(genDec->type == GeneralDecType::FUNCTION);
    REQUIRE(genDec->funcDec);
    FunctionDec& funcDec = *genDec->funcDec;
    codeGen.addFunctionSignatureToVirtualStack(funcDec);
    REQUIRE(codeGen.stackItems.size() == 1);
    CHECK(codeGen.stackItems[0].type == StackItemType::RETURN_ADDRESS);
    CHECK(codeGen.stackItems[0].positionOnStack == 8);
  }
  SECTION("two") {
    const std::string str = "func testFunction(): int32 { return 10; } ";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    auto genDec = checker.lookUp["testFunction"];
    REQUIRE(genDec);
    REQUIRE(genDec->type == GeneralDecType::FUNCTION);
    REQUIRE(genDec->funcDec);
    FunctionDec& funcDec = *genDec->funcDec;
    codeGen.addFunctionSignatureToVirtualStack(funcDec);
    REQUIRE(codeGen.stackItems.size() == 2);
    CHECK(codeGen.stackItems[0].type == StackItemType::RETURN_VALUE);
    CHECK(codeGen.stackItems[0].positionOnStack == 8);
    CHECK(codeGen.stackItems[1].type == StackItemType::RETURN_ADDRESS);
    CHECK(codeGen.stackItems[1].positionOnStack == 16);
  }
  SECTION("three") {
    const std::string str = "func testFunction(arg1: int64): int32 { return 10; } ";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    auto genDec = checker.lookUp["testFunction"];
    REQUIRE(genDec);
    REQUIRE(genDec->type == GeneralDecType::FUNCTION);
    REQUIRE(genDec->funcDec);
    FunctionDec& funcDec = *genDec->funcDec;
    codeGen.addFunctionSignatureToVirtualStack(funcDec);
    // CodeGen expected{parser.program, tokenizers, checker.lookUp};
    // StackItem rv {
    //   .positionOnStack = 8,
    //   .type = StackItemType::RETURN_VALUE
    // };
    // expected.stack.emplace_back(rv);
    // // VariableDec vd{Token()};
    // // StackItem var {
    // //   .variable = ,
    // //   .type = StackItemType::VARIABLE
    // // };
    // // expected.stack.emplace_back(rv);

    // StackItem ra {
    //   .positionOnStack = 24,
    //   .type = StackItemType::RETURN_ADDRESS
    // };
    // expected.stack.emplace_back(ra);

    // CHECK(codeGen.stack == expected.stack);
  
    REQUIRE(codeGen.stackItems.size() == 3);
    CHECK(codeGen.stackItems[0].type == StackItemType::RETURN_VALUE);
    CHECK(codeGen.stackItems[0].positionOnStack == 8);
    CHECK(codeGen.stackItems[1].type == StackItemType::VARIABLE);
    CHECK(codeGen.stackItems[1].variable.positionOnStack == 16);
    CHECK(codeGen.tk->extractToken(codeGen.stackItems[1].variable.varDec.name) == "arg1");
    CHECK(codeGen.stackItems[1].variable.varDec.type.token.type == TokenType::INT64_TYPE);
    CHECK(codeGen.stackItems[2].type == StackItemType::RETURN_ADDRESS);
    CHECK(codeGen.stackItems[2].positionOnStack == 24);
  }
}

TEST_CASE("string literals", "[codeGen]") {
  SECTION("1") {
    const std::string str = R"(func testFunction(arg1: int64 ptr): void { str: const char ptr = "Hello World!\n"; return; } )";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    auto genDec = checker.lookUp["testFunction"];
    REQUIRE(genDec);
    REQUIRE(genDec->type == GeneralDecType::FUNCTION);
    REQUIRE(genDec->funcDec);
    FunctionDec& funcDec = *genDec->funcDec;
    CodeGen otherCodeGen{parser.program, tokenizers, checker.lookUp};
    otherCodeGen.tk = &tokenizer;
    otherCodeGen.generateFunctionDeclaration(funcDec);
    codeGen.startFunctionScope(funcDec);
    codeGen.generateStatementList(&funcDec.body.scopeStatements);
    CodeGen expected{parser.program, tokenizers, checker.lookUp};
    CHECK(codeGen.byteCode == expected.byteCode);
    CHECK(otherCodeGen.byteCode == expected.byteCode);
    std::cout << codeGen.stackItems << '\n';
    std::cout << codeGen.dataSection.data() << '\n';
  }
}
