#include <unistd.h>
#include <iostream>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include "interpreter.hpp"

#define bc bytecode_t

TEST_CASE("move", "[interpreter]") {
    {
        bc const program [] = {(bc)OpCode::NOP,(bc)OpCode::NOP,
        (bc)OpCode::MOVE_I, 0, 1, 0, 0, 0, (bc)OpCode::EXIT, 0};
        REQUIRE(program[2] == (bc)OpCode::MOVE_I);
        bytecode_t data[24];
        Interpreter interpreter(program, data, 128);
        interpreter.runProgram();
        CHECK(interpreter.registers[0] == 1);
    }

    {
        bc const program [] = {(bc)OpCode::NOP,(bc)OpCode::NOP,
        (bc)OpCode::MOVE_I, 0, 0, 0x80, 0, 0, (bc)OpCode::EXIT, 0};
        REQUIRE(program[2] == (bc)OpCode::MOVE_I);
        bytecode_t data[24];
        Interpreter interpreter(program, data, 128);
        interpreter.runProgram();
        CHECK(interpreter.registers[0] == 0x8000);
    }
}

TEST_CASE("push/pop", "[interpreter]") {
    {
        bc const program [] =
        {
        (bc)OpCode::NOP, (bc)OpCode::NOP,
        (bc)OpCode::MOVE_I, 5, 1, 0, 0, 0,
        (bc)OpCode::PUSH_B, 5,
        (bc)OpCode::PUSH_B, 5,
        (bc)OpCode::PUSH_B, 5,
        (bc)OpCode::PUSH_B, 5,
        (bc)OpCode::POP_D, 0,
        (bc)OpCode::EXIT, 0,
        };
        REQUIRE(program[2] == (bc)OpCode::MOVE_I);
        bytecode_t data[24];
        Interpreter interpreter(program, data, 128);
        interpreter.runProgram();
        CHECK(interpreter.registers[0] == 0x01010101);
    }
}

TEST_CASE("print \"Hello World!\"", "[interpreter]") {
    int fd[2]{0};
    REQUIRE(pipe(fd) == 0);
    FILE *fp = fdopen(fd[1], "w");
    bc const program [] =
    {
    // set reg 11 to file *
    // push file pointer
    (bc)OpCode::MOVE, 12, dataPointerIndex,
    (bc)OpCode::LOAD_Q, 11, 12,
    (bc)OpCode::PUSH_Q, 11,
    (bc)OpCode::ADD_I, 12, 24, 0,
    // push pointer to string
    (bc)OpCode::PUSH_Q, 12,
    (bc)OpCode::CALL_B, (bc)BuiltInFunction::PRINT_STRING,
    (bc)OpCode::PUSH_Q, 11,
    (bc)OpCode::CALL_B, (bc)BuiltInFunction::FFLUSH,
    (bc)OpCode::EXIT, 0,
    };
    const char str [] = "Hello World!\n";
    bytecode_t data[24 + sizeof(str)];
    strcpy((char *)data + 24, str);
    Interpreter interpreter(program, data, 128);
    *(FILE **)&data[0] = fp;
    interpreter.runProgram();
    std::string buffer;
    buffer.resize(sizeof(str) - 1);
    REQUIRE(read(fd[0], buffer.data(), buffer.size())); // read from pipe
    CHECK(buffer == str);
    CHECK(interpreter.registers[0] == 0);
    close(fd[0]);
    close(fd[1]);
}

TEST_CASE("read line and store on stack", "[interpreter]") {
    int fd[2]{0};
    REQUIRE_FALSE(pipe(fd) == -1);
    FILE *fp = fdopen(fd[0],"r");

    const std::string input = "Hello World!\n";
    REQUIRE(write(fd[1], input.data(), input.size()));
    bc const program [] =
    {
    (bc)OpCode::MOVE, 2, dataPointerIndex,
    (bc)OpCode::NOP,
    (bc)OpCode::ADD_I, 2, 8, 0,
    (bc)OpCode::LOAD_Q, 11, 2, // load stdin FILE * from data
    (bc)OpCode::MOVE_SI, 12, '\n', // set j to newline char
    (bc)OpCode::SUB_I, stackPointerIndex, 32, 0, // make room on stack for array of 32 chars
    (bc)OpCode::MOVE, 13, stackPointerIndex, // set i to start of buffer

    // loop here
    (bc)OpCode::NOP,
    (bc)OpCode::PUSH_Q, 11, // add file ptr to stack
    (bc)OpCode::CALL_B, (bc)BuiltInFunction::READ_CHAR,
    (bc)OpCode::STORE_B, 13, 10,
    (bc)OpCode::INC, 13, // increment i,
    (bc)OpCode::CMP, 10, 12, // compare character to newline
    (bc)OpCode::RS_JUMP_NE, (bc)(-13), // loop if its not newline

    // add null termination to end of string
    (bc)OpCode::XOR, 2, 2,
    (bc)OpCode::STORE_B, 13, 2, 

    (bc)OpCode::EXIT, 0,
    };
    REQUIRE(program[4] == (bc)OpCode::ADD_I);
    bc data[24];
    Interpreter interpreter(program, data, 128);
    ((FILE**)&data)[1] = fp;
    interpreter.runProgram();
    std::string output{(char *)interpreter.registers[stackPointerIndex]};
    CHECK(output == input);
    close(fd[0]);
    close(fd[1]);
}


TEST_CASE("jump", "[interpreter]") {
    bc data[24];
    bc const program[] = {
        (bc)OpCode::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
        (bc)OpCode::MOVE_SI, 0, 1,
        (bc)OpCode::MOVE_SI, 1, 1,
        (bc)OpCode::CMP, 0, 1,
        (bc)OpCode::XOR, 3, 3, // set exit reg to 0
        (bc)OpCode::RS_JUMP_E, 5, // jumps to exit
        (bc)OpCode::MOVE_SI, 3, 1, // set exit reg to 1, this is how the test case is checked
        (bc)OpCode::EXIT, 3
    };
    REQUIRE(program[3] == (bc)OpCode::MOVE_SI);
    Interpreter interpreter(program, data, 128);
    CHECK(interpreter.runProgram() == 0);
}

TEST_CASE("loop", "[interpreter]") {
    bc data[24];
    bc const program [] =
    {
    (bc)OpCode::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
    (bc)OpCode::XOR, 12, 12, // initialize i
    (bc)OpCode::MOVE_SI, 2, 10,

    // start of loop:
    (bc)OpCode::CMP, 12, 2, // compare i to 9
    (bc)OpCode::RS_JUMP_GE, 6, // jump to end if result is greater than or equal to 10
    (bc)OpCode::INC, 12,
    (bc)OpCode::RS_JUMP, (bc)(-7),
    
    (bc)OpCode::EXIT, 12,
    };
    Interpreter interpreter(program, data, 128);
    uint64_t res = interpreter.runProgram();
    CHECK(res == 10);
}
