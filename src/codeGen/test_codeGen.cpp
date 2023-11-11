#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include "codeGen.hpp"
#include "../parser/parser.hpp"
#include "../testingMemPool.hpp"

#define uc unsigned char

#define testBoilerPlate(str) std::vector<Tokenizer> dummyTokenizers; \
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
    testBoilerPlate(str);
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
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.varDec);
    uint32_t size = codeGen.generateVariableDeclaration(*statement.varDec);
    CHECK(size == 4);
    const std::vector<uc> expected = {
      (uc)OpCodes::NOP, (uc)OpCodes::NOP,
      (uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0
    };
    CHECK(codeGen.byteCode == expected);

  }
}

TEST_CASE("jump statements", "[codeGen]") {
  // boiler plate
  {
    const std::string str = "if (1) {} else {} ";
    testBoilerPlate(str);
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
  {
    const std::string str = "";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);

    // fails, need to update the str to have nested control flow and see if the jump addresses are correct
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateControlFlowStatement(*statement.controlFlow);
    const std::vector<uc> expected =  {};
    CHECK(codeGen.byteCode == expected);
  }
}
