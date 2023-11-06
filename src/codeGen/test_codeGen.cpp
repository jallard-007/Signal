#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include "codeGen.hpp"
#include "../parser/parser.hpp"
#include "../testingMemPool.hpp"

#define uc unsigned char

#define UT_boilerPlate(str) std::vector<Tokenizer> dummyTokenizers; \
  Program dummyProgram; \
  Checker dummyChecker{dummyProgram, dummyTokenizers, memPool}; \
  CodeGen codeGen{dummyProgram, dummyTokenizers, dummyChecker}; \
  Tokenizer tokenizer {"./src/parser/test_parser.cpp", str}; \
  codeGen.tk = &tokenizer; \
  Parser parser{tokenizer, memPool};

TEST_CASE("expressions", "[codeGen]") {
  // boiler plate
  {
    const std::string str = " 4 + 4 ;";
    UT_boilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK_FALSE(expRes.isStruct);
    CHECK(expRes.val == 8);
  }
}

TEST_CASE("variable creation", "[codeGen]") {
  // boiler plate
  {
    const std::string str = "x:uint32;";
    UT_boilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.varDec);
    uint32_t size = codeGen.generateDeclarationVariable(*statement.varDec);
    CHECK(size == 4);
    const std::vector<uc> expected = {
      (uc)OpCodes::NOP, (uc)OpCodes::NOP,
      (uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0
    };
    CHECK(codeGen.byteCode == expected);

  }
}

TEST_CASE("if statement", "[codeGen]") {
  // boiler plate
  {
    const std::string str = "if (1) {} else {} ";
    UT_boilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateControlFlowStatement(*statement.controlFlow);
    const std::vector<uc> expected =  {
      (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP,
      (uc)OpCodes::JUMP, 16, 0, 0, 0, 0, 0, 0, 0,
    };
    CHECK(codeGen.byteCode == expected);
  }
}
