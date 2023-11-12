#include <iostream>
#include <catch2/catch_test_macros.hpp>
#include "codeGen.hpp"
#include "../parser/parser.hpp"
#include "../testingMemPool.hpp"

#define uc unsigned char

#define testBoilerPlate(str) \
  std::vector<Tokenizer> tokenizers; \
  Tokenizer& tokenizer = tokenizers.emplace_back("./src/parser/test_parser.cpp", str); \
  Parser parser{tokenizer, memPool}; \
  Checker checker{parser.program, tokenizers, memPool}; \
  CodeGen codeGen{parser.program, tokenizers, checker.lookUp}; \
  codeGen.tk = &tokenizer


// copy of addByte from CodeGen class
void addByte(std::vector<uc>& byteCode, uc byte) {
  byteCode.emplace_back(byte);
}
// copy of addByteOp from CodeGen class
void addByteOp(std::vector<uc>& byteCode, OpCodes opCode) {
  addByte(byteCode, (uc)opCode);
}
// copy of addBytes from CodeGen class
void addBytes(std::vector<uc>& byteCode, const std::vector<uc>& bytes) {
  for (const uc byte: bytes) {
    addByte(byteCode, byte);
  }
}
// copy of alignForImm from CodeGen class
void alignForImm(std::vector<uc>& byteCode, const uint32_t offset, const uint32_t size) {
  uint8_t mod = (byteCode.size() + offset) % size;
  if (mod == 0) {
    return;
  }
  while(mod++ != size) {
    addByteOp(byteCode, OpCodes::NOP);
  }
}

TEST_CASE("expressions", "[codeGen]") {
  {
    // constant expression, should return the actual value of the expression
    const std::string str = " 4 + 4 ;";
    testBoilerPlate(str);
    Expression expression;
    ParseExpressionErrorType errorType = parser.parseExpression(expression);
    REQUIRE(errorType == ParseExpressionErrorType::NONE);
    ExpressionResult expRes = codeGen.generateExpression(expression);
    CHECK_FALSE(expRes.isReg);
    CHECK_FALSE(expRes.isTemp);
    CHECK(expRes.val == 8);
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
    uint32_t size = codeGen.generateVariableDeclaration(*statement.varDec);
    CHECK(size == 4);
    std::vector<uc> expected;
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0});
    CHECK(codeGen.byteCode == expected);
  }
  {
    const std::string str = "x:uint32 = 10;";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.varDec);
    uint32_t size = codeGen.generateVariableDeclaration(*statement.varDec);
    CHECK(size == 4);
    std::vector<uc> expected;
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::MOVE_I, 0, 10, 0, 0, 0});
    addBytes(expected, {(uc)OpCodes::PUSH_D, 0});
    CHECK(codeGen.byteCode == expected);
  }
}

TEST_CASE("jump statements in always true control flow", "[codeGen]") {
  {
    // at the end of the `if (1)` body, it should jump past the else body
    const std::string str = "if (1) {} else {num:uint64;} ";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateStatement(statement);
    std::vector<uc> expected;
    alignForImm(expected, 1, 8);
    addBytes(expected, {(uc)OpCodes::JUMP, 24, 0, 0, 0, 0, 0, 0, 0});
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::SUB_I, stackPointerIndex, 8, 0, 0, 0});
    CHECK(codeGen.byteCode == expected);
  }
  {
    // should jump to the start of the for loop (just after the variable dec)
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
    std::vector<uc> expected;
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::SUB_I, stackPointerIndex, 8, 0, 0, 0});
    alignForImm(expected, 1, 8);
    addBytes(expected, {(uc)OpCodes::JUMP, 8, 0, 0, 0, 0, 0, 0, 0});
    CHECK(codeGen.byteCode == expected);
  }
  {
    // should jump to the start of the while loop (just after the variable dec)
    const std::string str = "num:uint64; while true {} ";
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
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::WHILE_LOOP);
    codeGen.generateStatement(statement);
    std::vector<uc> expected;
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::SUB_I, stackPointerIndex, 8, 0, 0, 0});
    alignForImm(expected, 1, 8);
    addBytes(expected, {(uc)OpCodes::JUMP, 8, 0, 0, 0, 0, 0, 0, 0});
    CHECK(codeGen.byteCode == expected);
  }
}

TEST_CASE("struct info generation", "[codeGen]") {
  {
    const std::string str = "struct Thing { x: uint32; }";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    StructInformation& structInfo = codeGen.getStructInfo("Thing");
    CHECK(structInfo.size == 4);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["x"] == 0);
  }
  {
    const std::string str = "struct Thing { y:bool; x: uint32; }";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    StructInformation& structInfo = codeGen.getStructInfo("Thing");
    CHECK(structInfo.size == 8);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["y"] == 0);
    CHECK(structInfo.offsetMap["x"] == 4);
  }
  {
    const std::string str = "struct Thing { x: uint32; y:bool; }";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    StructInformation& structInfo = codeGen.getStructInfo("Thing");
    CHECK(structInfo.size == 8);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["y"] == 4);
    CHECK(structInfo.offsetMap["x"] == 0);
  }
  {
    const std::string str = "struct Thing { y:bool; x: uint32; j:bool; }";
    testBoilerPlate(str);
    REQUIRE(parser.parse());
    REQUIRE(checker.check());
    StructInformation& structInfo = codeGen.getStructInfo("Thing");
    CHECK(structInfo.size == 12);
    CHECK(structInfo.alignTo == 4);
    CHECK(structInfo.offsetMap["y"] == 0);
    CHECK(structInfo.offsetMap["x"] == 4);
    CHECK(structInfo.offsetMap["j"] == 8);
  }
}

TEST_CASE("short-circuit logical bin ops", "[codeGen]") {
  {
    const std::string str = "if (true && false) { num:uint32; } ";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateStatement(statement);
    std::vector<uc> expected;
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0});
    addBytes(expected, {(uc)OpCodes::SET_Z, 0});

    // short circuit
    alignForImm(expected, 1, 8);
    const uint32_t indexOfShortCircuitJump = expected.size() + 1;
    addBytes(expected, {(uc)OpCodes::JUMP_E, 0, 0, 0, 0, 0, 0, 0, 0});

    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::MOVE_I, 1, 0, 0, 0, 0});
    addBytes(expected, {(uc)OpCodes::LOGICAL_AND, 0, 1});
    expected[indexOfShortCircuitJump] = expected.size();
    alignForImm(expected, 1, 8);
    const uint32_t indexOfSecondJump = expected.size() + 1;
    addBytes(expected, {(uc)OpCodes::JUMP_E, 0, 0, 0, 0, 0, 0, 0, 0});
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0});
    expected[indexOfSecondJump] = expected.size();

    CHECK(codeGen.byteCode == expected);
  }
  {
    const std::string str = "if (true || false) { num:uint32; } ";
    testBoilerPlate(str);
    Statement statement;
    ParseStatementErrorType errorType = parser.parseStatement(statement);
    REQUIRE(errorType == ParseStatementErrorType::NONE);
    REQUIRE(statement.controlFlow);
    REQUIRE(statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
    codeGen.generateStatement(statement);
    std::vector<uc> expected;
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0});
    addBytes(expected, {(uc)OpCodes::SET_Z, 0});

    // short circuit
    alignForImm(expected, 1, 8);
    const uint32_t indexOfShortCircuitJump = expected.size() + 1;
    addBytes(expected, {(uc)OpCodes::JUMP_NE, 0, 0, 0, 0, 0, 0, 0, 0});

    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::MOVE_I, 1, 0, 0, 0, 0});
    addBytes(expected, {(uc)OpCodes::LOGICAL_OR, 0, 1});
    expected[indexOfShortCircuitJump] = expected.size();
    alignForImm(expected, 1, 8);
    const uint32_t indexOfSecondJump = expected.size() + 1;
    addBytes(expected, {(uc)OpCodes::JUMP_E, 0, 0, 0, 0, 0, 0, 0, 0});
    alignForImm(expected, 2, 4);
    addBytes(expected, {(uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0});
    expected[indexOfSecondJump] = expected.size();

    CHECK(codeGen.byteCode == expected);
  }
}
