#include <iostream>
#include "codeGen.hpp"
#include "parser/parser.hpp"
#include "testingMemPool.hpp"

#include <catch2/catch_test_macros.hpp>

#define bc bytecode_t

#define testBoilerPlate(str) \
    std::vector<Tokenizer> tokenizers; \
    Tokenizer& tokenizer = tokenizers.emplace_back("./src/parser/test_parser.cpp", str); \
    Parser parser{tokenizer, memPool}; \
    Checker checker{parser.program, tokenizers, memPool}; \
    checker.tk = &tokenizer; \
    CodeGen codeGen{parser.program, tokenizers, checker.lookUp, checker.structLookUp}; \
    codeGen.tk = &tokenizer

TEST_CASE("variable creation", "[codeGen]") {
    SECTION("1") {
        const std::string str = "x:uint32;";
        testBoilerPlate(str);
        Statement statement;
        ParseStatementErrorType errorType = parser.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(statement.varDec);

        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());

        codeGen.generateVariableDeclaration(*statement.varDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 4, 0}});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("2") {
        const std::string str = "x:uint32 = 10;";
        testBoilerPlate(str);
        Statement statement;
        ParseStatementErrorType errorType = parser.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(statement.varDec);

        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());

        codeGen.generateVariableDeclaration(*statement.varDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
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

        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());

        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
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
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
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
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());

        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::SUB_I, stackPointerIndex, 8, 0}});
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::ADD_I, stackPointerIndex, 8, 0}});
        expected.addBytes({{(bc)OpCode::RS_JUMP, 0}});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("short-circuit logical bin ops", "[codeGen]") {
    SECTION("1") {
        const std::string str = "x:uint32; y:uint32; if (x && y) { num:uint32; } ";
        testBoilerPlate(str);
        Statement statement_x, statement_y, cond_statement;
        ParseStatementErrorType errorType = parser.parseStatement(statement_x);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(statement_x.type == StatementType::VARIABLE_DEC);
        errorType = parser.parseStatement(statement_y);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(statement_y.type == StatementType::VARIABLE_DEC);
        errorType = parser.parseStatement(cond_statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(cond_statement.controlFlow);
        REQUIRE(cond_statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
        checker.checkLocalVarDec(*statement_x.varDec);
        checker.checkLocalVarDec(*statement_y.varDec);
        codeGen.generateStatement(statement_x);
        codeGen.generateStatement(statement_y);
        codeGen.byteCode.clear();
        codeGen.generateStatement(cond_statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{(bc)OpCode::MOVE, 2, stackPointerIndex}});
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::ADD_I, 2, 4, 0}});
        expected.addBytes({{(bc)OpCode::LOAD_D, 1, 2}});

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
    SECTION("2") {
        const std::string str = "x:uint32; y:uint32; if (x && y || x) {} ";
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
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{(bc)OpCode::MOVE, 2, stackPointerIndex}});
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::ADD_I, 2, 4, 0}});
        expected.addBytes({{(bc)OpCode::LOAD_D, 1, 2}});

        // short circuit
        expected.addBytes({{(bc)OpCode::SET_FLAGS, 1}});
        // short circuit; if e (false), don't evaluate rest of expression, it's guaranteed false
        expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

        expected.addBytes({{(bc)OpCode::LOAD_D, 2, stackPointerIndex}});

        expected.addBytes({{(bc)OpCode::LOGICAL_AND, 1, 2}});
        expected.addBytes({{(bc)OpCode::GET_NE, 1}}); // save the result
        // short circuit; if ne (true), don't evaluate rest of expression, it's guaranteed true
        expected.addBytes({{(bc)OpCode::RS_JUMP_NE, 0}});

        expected.addBytes({{(bc)OpCode::MOVE, 3, stackPointerIndex}});
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::ADD_I, 3, 4, 0}});
        expected.addBytes({{(bc)OpCode::LOAD_D, 2, 3}});

        expected.addBytes({{(bc)OpCode::LOGICAL_OR, 1, 2}});
        expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}}); // jump if false

        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("3") {
        const std::string str = "x:char; y:char; j:char; k:char; if (x && y || k && j) {} ";
        testBoilerPlate(str);
        Statement statement_x, statement_y, statement_j, statement_k, cond_statement;
        ParseStatementErrorType errorType = parser.parseStatement(statement_x);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        errorType = parser.parseStatement(statement_y);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
            errorType = parser.parseStatement(statement_j);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
            errorType = parser.parseStatement(statement_k);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        errorType = parser.parseStatement(cond_statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(cond_statement.controlFlow);
        REQUIRE(cond_statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT);
        codeGen.generateStatement(statement_x);
        codeGen.generateStatement(statement_y);
        codeGen.generateStatement(statement_j);
        codeGen.generateStatement(statement_k);
        codeGen.byteCode.clear();
        codeGen.generateStatement(cond_statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{(bc)OpCode::MOVE, 2, stackPointerIndex}});
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::ADD_I, 2, 3, 0}});
        expected.addBytes({{(bc)OpCode::LOAD_B, 1, 2}});

        expected.addBytes({{(bc)OpCode::SET_FLAGS, 1}});
        // short circuit; if e (false), don't evaluate rest of expression, it's guaranteed false
        expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

        expected.addBytes({{(bc)OpCode::MOVE, 3, stackPointerIndex}});
        expected.alignForImm(2, 2);
        expected.addBytes({{(bc)OpCode::ADD_I, 3, 2, 0}});
        expected.addBytes({{(bc)OpCode::LOAD_B, 2, 3}});

        expected.addBytes({{(bc)OpCode::LOGICAL_AND, 1, 2}});
        expected.addBytes({{(bc)OpCode::GET_NE, 1}}); // save the result
        // short circuit; if ne (true), don't evaluate rest of expression, it's guaranteed true
        expected.addBytes({{(bc)OpCode::RS_JUMP_NE, 0}});

        expected.addBytes({{(bc)OpCode::LOAD_B, 2, stackPointerIndex}});
        expected.addBytes({{(bc)OpCode::SET_FLAGS, 2}});
        // short circuit; if e (false), don't evaluate rest of expression, it's guaranteed false
        expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}});

        expected.addBytes({{(bc)OpCode::MOVE, 4, stackPointerIndex}});
        expected.addBytes({{(bc)OpCode::INC, 4}});
        expected.addBytes({{(bc)OpCode::LOAD_B, 3, 4}});

        expected.addBytes({{(bc)OpCode::LOGICAL_AND, 2, 3}});
        expected.addBytes({{(bc)OpCode::GET_NE, 0}}); // save the result

        expected.addBytes({{(bc)OpCode::LOGICAL_OR, 1, 0}});
        expected.addBytes({{(bc)OpCode::RS_JUMP_E, 0}}); // jump if false

        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("addFunctionSignatureToVirtualStack", "[codeGen]") {
    SECTION("one") {
        const std::string str = "func testFunction(): void { } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
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
        REQUIRE(checker.check(true));
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
    SECTION("three") {
        const std::string str = "func testFunction(arg1: int64): int32 { return 10; } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.addFunctionSignatureToVirtualStack(funcDec);
        REQUIRE(codeGen.stackItems.size() == 2);
        CHECK(codeGen.stackItems[0].type == StackItemType::VARIABLE);
        CHECK(codeGen.stackItems[0].variable.positionOnStack == 8);
        CHECK(codeGen.tk->extractToken(codeGen.stackItems[0].variable.varDec.name) == "arg1");
        CHECK(codeGen.stackItems[0].variable.varDec.type.token.getType() == TokenType::INT64_TYPE);
        CHECK(codeGen.stackItems[1].type == StackItemType::RETURN_ADDRESS);
        CHECK(codeGen.stackItems[1].positionOnStack == 16);
    }
}

TEST_CASE("placing literal in data section", "[codeGen]") {
    SECTION("1") {
        const std::string stringLiteral = R"(Hello World!\n)";
        const std::string stringLiteralExpected = "Hello World!\n";
        const std::string str = "str: const char ptr = \"" + stringLiteral + "\";";
        testBoilerPlate(str);
        Statement statement;
        REQUIRE(parser.parseStatement(statement) == ParseStatementErrorType::NONE);
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());
        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::MOVE, 1, dataPointerIndex,
            (bc)OpCode::NOP,
            (bc)OpCode::ADD_I, 1, 24, 0,
            (bc)OpCode::PUSH_Q, 1
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
        REQUIRE(codeGen.dataSectionEntries.size() == 4);
        DataSectionEntry& entry = codeGen.dataSectionEntries[3];
        CHECK(stringLiteralExpected == (char *)(codeGen.dataSection.data() + entry.indexInDataSection) );
    }
    SECTION("2") {
        const std::string str = "file: file_t = stdin;";
        testBoilerPlate(str);
        Statement statement;
        REQUIRE(parser.parseStatement(statement) == ParseStatementErrorType::NONE);
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());
        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::MOVE, 1, dataPointerIndex,
            (bc)OpCode::NOP,
            (bc)OpCode::ADD_I, 1, 8, 0,
            (bc)OpCode::LOAD_Q, 2, 1,
            (bc)OpCode::PUSH_Q, 2
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
        REQUIRE(codeGen.dataSectionEntries.size() == 3);
        DataSectionEntry& entry = codeGen.dataSectionEntries[1];
        CHECK(entry.type == DataSectionEntryType::STDIN);
    }
}

TEST_CASE("generating return statement", "[codeGen]") {
    SECTION("1") {
        const std::string str = "func testFunction(): void { } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("2") {
        const std::string str = "func testFunction(arg1: int64 ptr): void { } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::ADD_I, 30, 8, 0,
            (bc)OpCode::JUMP, 1
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("3") {
        const std::string str = "func testFunction(arg1: int64 ptr, c1: char, c2: char): void { } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::ADD_I, 30, 16, 0,
            (bc)OpCode::JUMP, 1
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("4") {
        const std::string str = "func testFunction(): int32 { return 1; } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::MOVE_SI, 10, 1,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("generating function call", "[codeGen]") {
    SECTION("1") {
        const std::string str = 
R"(
    func testFunction(): void {
        otherTestFunction();
    }
    func otherTestFunction(): void {}
)";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::NOP,
            (bc)OpCode::NOP,
            (bc)OpCode::NOP,
            (bc)OpCode::CALL, 0, 0, 0, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("2") {
        const std::string str = 
R"(
    func testFunction(): void {
        otherTestFunction();
    }
    func otherTestFunction(): int32 { return 1; }
)";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        {
            auto genDec = checker.lookUp["testFunction"];
            REQUIRE(genDec);
            REQUIRE(genDec->type == GeneralDecType::FUNCTION);
            REQUIRE(genDec->funcDec);
            FunctionDec& funcDec = *genDec->funcDec;
            codeGen.generateFunctionDeclaration(funcDec);
        }
        const uint32_t codeSizeAfterFunc = codeGen.byteCode.size();
        {
            auto genDec = checker.lookUp["otherTestFunction"];
            REQUIRE(genDec);
            REQUIRE(genDec->type == GeneralDecType::FUNCTION);
            REQUIRE(genDec->funcDec);
            FunctionDec& funcDec = *genDec->funcDec;
            codeGen.generateFunctionDeclaration(funcDec);
        }
        codeGen.byteCode.resize(codeSizeAfterFunc);
        codeGen.writeFunctionJumpOffsets();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.alignForImm(1, 4);
        const uint32_t indexOfCall = expected.byteCode.size();
        expected.addBytes({{
            (bc)OpCode::CALL, 0, 0, 0, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        const uint32_t functionIndex = expected.byteCode.size();
        *(int32_t *)&expected.byteCode[indexOfCall + 1] = functionIndex - indexOfCall;
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("3") {
        const std::string str = 
R"(
    func testFunction(): void {
        otherTestFunction(1);
    }
    func otherTestFunction(x: int32): int32 { return 1; }
)";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        {
            auto genDec = checker.lookUp["testFunction"];
            REQUIRE(genDec);
            REQUIRE(genDec->type == GeneralDecType::FUNCTION);
            REQUIRE(genDec->funcDec);
            FunctionDec& funcDec = *genDec->funcDec;
            codeGen.generateFunctionDeclaration(funcDec);
        }
        const uint32_t codeSizeAfterFunc = codeGen.byteCode.size();
        {
            auto genDec = checker.lookUp["otherTestFunction"];
            REQUIRE(genDec);
            REQUIRE(genDec->type == GeneralDecType::FUNCTION);
            REQUIRE(genDec->funcDec);
            FunctionDec& funcDec = *genDec->funcDec;
            codeGen.generateFunctionDeclaration(funcDec);
        }
        codeGen.byteCode.resize(codeSizeAfterFunc);
        codeGen.writeFunctionJumpOffsets();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::MOVE_SI, 1, 1,
            (bc)OpCode::NOP,
            (bc)OpCode::SUB_I, stackPointerIndex, 4, 0,
            (bc)OpCode::PUSH_D, 1,
        }});
        expected.alignForImm(1, 4);
        const uint32_t indexOfCall = expected.byteCode.size();
        expected.addBytes({{
            (bc)OpCode::CALL, 0, 0, 0, 0,
            (bc)OpCode::ADD_I, 30, 4, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        const uint32_t functionIndex = expected.byteCode.size();
        *(int32_t *)&expected.byteCode[indexOfCall + 1] = functionIndex - indexOfCall;
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("function call in subexpression", "[codeGen]") {
    SECTION("1") {
        const std::string str = 
R"(
    func testFunction(): void {
        x:int32 = 2 + otherTestFunction();
    }
    func otherTestFunction(): int32 { return 1; }
)";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        {
            auto genDec = checker.lookUp["testFunction"];
            REQUIRE(genDec);
            REQUIRE(genDec->type == GeneralDecType::FUNCTION);
            REQUIRE(genDec->funcDec);
            FunctionDec& funcDec = *genDec->funcDec;
            codeGen.generateFunctionDeclaration(funcDec);
        }
        const uint32_t codeSizeAfterFunc = codeGen.byteCode.size();
        {
            auto genDec = checker.lookUp["otherTestFunction"];
            REQUIRE(genDec);
            REQUIRE(genDec->type == GeneralDecType::FUNCTION);
            REQUIRE(genDec->funcDec);
            FunctionDec& funcDec = *genDec->funcDec;
            codeGen.generateFunctionDeclaration(funcDec);
        }
        codeGen.byteCode.resize(codeSizeAfterFunc);
        codeGen.writeFunctionJumpOffsets();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.alignForImm(1, 4);
        const uint32_t indexOfCall = expected.byteCode.size();
        expected.addBytes({{
            (bc)OpCode::CALL, 0, 0, 0, 0,
            (bc)OpCode::ADD_I, 10, 2, 0,
            (bc)OpCode::PUSH_D, 10,
            (bc)OpCode::ADD_I, 30, 4, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        const uint32_t functionIndex = expected.byteCode.size();
        *(int32_t *)&expected.byteCode[indexOfCall + 1] = functionIndex - indexOfCall;
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("for loop", "[codeGen]") {
    SECTION("1") {
    const std::string str = 
R"(
    func testFunction(): void {
        for (i: uint32 = 0; i < 10; ++i) {}
        c: char;
    }
)";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        codeGen.writeLocalJumpOffsets();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::MOVE_SI, 1, 0,
            (bc)OpCode::PUSH_D, 1,
        }});
        const uint32_t jumpIndex = expected.byteCode.size();
        expected.addBytes({{
            (bc)OpCode::LOAD_D, 1, 30,
            (bc)OpCode::MOVE_SI, 2, 10,
            (bc)OpCode::CMP, 1, 2,
            (bc)OpCode::RS_JUMP_GE
        }});
        const uint32_t conditionJump = expected.byteCode.size();
        expected.addByte(0);
        expected.addBytes({{
            (bc)OpCode::LOAD_D, 1, 30,
            (bc)OpCode::INC, 1,
            (bc)OpCode::STORE_D, 30, 1,
            (bc)OpCode::RS_JUMP
        }});
        expected.addByte(jumpIndex - (expected.byteCode.size() - 1));
        expected.byteCode[conditionJump] = expected.byteCode.size() - (conditionJump - 1);
        expected.alignForImm(2, 2);
        expected.addBytes({{
            (bc)OpCode::DEC, 30,
            (bc)OpCode::ADD_I, 30, 5, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("while loop", "[codeGen]") {
    SECTION("1") {
    const std::string str = 
R"(
    func testFunction(): void {
        i: uint32 = 0;
        while i < 10 {
            ++i;
        }
        c: char;
    }
)";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        auto genDec = checker.lookUp["testFunction"];
        REQUIRE(genDec);
        REQUIRE(genDec->type == GeneralDecType::FUNCTION);
        REQUIRE(genDec->funcDec);
        FunctionDec& funcDec = *genDec->funcDec;
        codeGen.generateFunctionDeclaration(funcDec);
        codeGen.writeLocalJumpOffsets();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        // output should be exactly the same as the for loop code
        expected.addBytes({{
            (bc)OpCode::MOVE_SI, 1, 0,
            (bc)OpCode::PUSH_D, 1,
        }});
        const uint32_t jumpIndex = expected.byteCode.size();
        expected.addBytes({{
            (bc)OpCode::LOAD_D, 1, 30,
            (bc)OpCode::MOVE_SI, 2, 10,
            (bc)OpCode::CMP, 1, 2,
            (bc)OpCode::RS_JUMP_GE
        }});
        const uint32_t conditionJump = expected.byteCode.size();
        expected.addByte(0);
        expected.addBytes({{
            (bc)OpCode::LOAD_D, 1, 30,
            (bc)OpCode::INC, 1,
            (bc)OpCode::STORE_D, 30, 1,
            (bc)OpCode::RS_JUMP
        }});
        expected.addByte(jumpIndex - (expected.byteCode.size() - 1));
        expected.byteCode[conditionJump] = expected.byteCode.size() - (conditionJump - 1);
        expected.alignForImm(2, 2);
        expected.addBytes({{
            (bc)OpCode::DEC, 30,
            (bc)OpCode::ADD_I, 30, 5, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("get address of expression", "[codeGen]") {
    SECTION("1") {
        const std::string str = "x:uint32; y:int32; x; y; ";
        testBoilerPlate(str);
        Statement statement_x, statement_y, load_statement_x, load_statement_y;
        ParseStatementErrorType errorType = parser.parseStatement(statement_x);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(statement_x.type == StatementType::VARIABLE_DEC);
        errorType = parser.parseStatement(statement_y);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(statement_y.type == StatementType::VARIABLE_DEC);
        errorType = parser.parseStatement(load_statement_x);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(load_statement_x.type == StatementType::EXPRESSION);
        REQUIRE(load_statement_x.expression->getType() == ExpressionType::VALUE);
        errorType = parser.parseStatement(load_statement_y);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        REQUIRE(load_statement_y.type == StatementType::EXPRESSION);
        REQUIRE(load_statement_y.expression->getType() == ExpressionType::VALUE);
        codeGen.generateStatement(statement_x);
        codeGen.generateStatement(statement_y);
        codeGen.byteCode.clear();
        ExpressionResult res_x = codeGen.getAddressOfExpression(*load_statement_x.expression);
        ExpressionResult res_y = codeGen.getAddressOfExpression(*load_statement_y.expression);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::MOVE, 1, 30,
            (bc)OpCode::NOP,
            (bc)OpCode::ADD_I, 1, 4, 0,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
        CHECK(res_x.isPointerToValue);
        CHECK(res_x.isReg);
        CHECK(res_x.getReg() == 1);
        CHECK(res_x.value.type->token.getType() == TokenType::UINT32_TYPE);
        CHECK(res_y.isPointerToValue);
        CHECK(res_y.isReg);
        CHECK(res_y.getReg() == 30);
        CHECK_FALSE(res_y.isTemp);
        CHECK(res_y.value.type->token.getType() == TokenType::INT32_TYPE);
    }
}

TEST_CASE("generate array", "[codeGen]") {
    SECTION("1") {
        const std::string str = " var: int32 [1]; ";
        testBoilerPlate(str);
        Statement statement;
        REQUIRE(parser.parseStatement(statement) == ParseStatementErrorType::NONE);
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());
        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, stackPointerIndex, 4, 0
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("2") {
        const std::string str = " var: int32 [10]; ";
        testBoilerPlate(str);
        Statement statement;
        REQUIRE(parser.parseStatement(statement) == ParseStatementErrorType::NONE);
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());
        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, stackPointerIndex, 40, 0
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("3") {
        const std::string str = " var: int32 [10] = [1]; ";
        testBoilerPlate(str);
        Statement statement;
        REQUIRE(parser.parseStatement(statement) == ParseStatementErrorType::NONE);
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());
        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, 30, 40, 0,
            (bc)OpCode::PUSH_Q, 30,
            (bc)OpCode::XOR, 0, 0,
            (bc)OpCode::PUSH_Q, 0,
            (bc)OpCode::MOVE_SI, 0, 40,
            (bc)OpCode::PUSH_Q, 0,
            (bc)OpCode::CALL_B, (bc)BuiltInFunction::MEM_SET,
            (bc)OpCode::MOVE, 1, 30,
            (bc)OpCode::MOVE_SI, 2, 1,
            (bc)OpCode::STORE_D, 1, 2,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("4") {
        const std::string str = " var: int32 [10] = [ .0 = 1, .4 = 5 ]; ";
        testBoilerPlate(str);
        Statement statement;
        REQUIRE(parser.parseStatement(statement) == ParseStatementErrorType::NONE);
        checker.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(checker.errors.empty());
        codeGen.generateStatement(statement);
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, 30, 40, 0,
            (bc)OpCode::PUSH_Q, 30,
            (bc)OpCode::XOR, 0, 0,
            (bc)OpCode::PUSH_Q, 0,
            (bc)OpCode::MOVE_SI, 0, 40,
            (bc)OpCode::PUSH_Q, 0,
            (bc)OpCode::CALL_B, (bc)BuiltInFunction::MEM_SET,
            (bc)OpCode::MOVE, 1, 30,
            (bc)OpCode::MOVE_SI, 2, 1,
            (bc)OpCode::STORE_D, 1, 2,
            (bc)OpCode::NOP,
            (bc)OpCode::ADD_I, 1, 16, 0,
            (bc)OpCode::MOVE_SI, 2, 5,
            (bc)OpCode::STORE_D, 1, 2,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}

TEST_CASE("generate struct", "[codeGen]") {
    SECTION("1") {
        const std::string str = "struct MyStruct { var: int32; } func MyFunc(): void { var: MyStruct; } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        codeGen.generate();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, 30, 4, 0,
            (bc)OpCode::ADD_I, 30, 4, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("2") {
        const std::string str = "struct MyStruct { c: char; v: char; u:int32; } func MyFunc(): MyStruct { g: MyStruct; return g; } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        codeGen.generate();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, stackPointerIndex, 8, 0,
            (bc)OpCode::LOAD_Q, 1, stackPointerIndex,
            (bc)OpCode::MOVE, returnRegisterIndex, 1,
            (bc)OpCode::ADD_I, stackPointerIndex, 8, 0,
            (bc)OpCode::POP_Q, 2,
            (bc)OpCode::JUMP, 2,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("3") {
        const std::string str = "struct MyStruct { x:char ; c: char; y:char; } func MyFunc(): MyStruct { g: MyStruct; return g; } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        codeGen.generate();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::SUB_I, stackPointerIndex, 3, 0,
            (bc)OpCode::MOVE, 2, stackPointerIndex,
            (bc)OpCode::LOAD_W, 1, 2,
            (bc)OpCode::ADD_I, 2, 2, 0,
            (bc)OpCode::LOAD_B, 0, 2,
            (bc)OpCode::SHIFT_L_I, 0, 16, 0,
            (bc)OpCode::OR, 1, 0,
            (bc)OpCode::MOVE, returnRegisterIndex, 1,
            (bc)OpCode::NOP,
            (bc)OpCode::ADD_I, stackPointerIndex, 3, 0,
            (bc)OpCode::POP_Q, 2,
            (bc)OpCode::JUMP, 2,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
    SECTION("4") {
        const std::string str = "struct MyStruct { c: char; v: char; u:int32; } func MyFunc(): void { g: MyStruct = [ .c = 'c', .u = 20 ] ; } ";
        testBoilerPlate(str);
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        codeGen.generate();
        CodeGen expected{parser.program, tokenizers, checker.lookUp, checker.structLookUp};
        expected.addBytes({{
            (bc)OpCode::XOR, 0, 0,
            (bc)OpCode::PUSH_Q, 0,
            (bc)OpCode::MOVE, 1, stackPointerIndex,
            (bc)OpCode::MOVE_SI, 2, 'c',
            (bc)OpCode::STORE_B, 1, 2, 
            (bc)OpCode::ADD_I, 1, 4, 0,
            (bc)OpCode::MOVE_SI, 2, 20,
            (bc)OpCode::STORE_D, 1, 2,

            (bc)OpCode::ADD_I, stackPointerIndex, 8, 0,
            (bc)OpCode::POP_Q, 1,
            (bc)OpCode::JUMP, 1,
        }});
        CHECK(codeGen.byteCode == expected.byteCode);
    }
}
