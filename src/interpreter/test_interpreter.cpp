#include <unistd.h>
#include <iostream>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include "interpreter.hpp"

#define uc unsigned char

TEST_CASE("move", "[interpreter]") {
  {
    bytecode_t const program [] = {(uc)OpCode::NOP,(uc)OpCode::NOP,
    (uc)OpCode::MOVE_I, 0, 1, 0, 0, 0, (uc)OpCode::EXIT, 0};
    REQUIRE(program[2] == (uc)OpCode::MOVE_I);
    Interpreter interpreter(program, nullptr, 128);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 1);
  }

  {
    bytecode_t const program [] = {(uc)OpCode::NOP,(uc)OpCode::NOP,
    (uc)OpCode::MOVE_I, 0, 0, 0x80, 0, 0, (uc)OpCode::EXIT, 0};
    REQUIRE(program[2] == (uc)OpCode::MOVE_I);
    Interpreter interpreter(program, nullptr, 128);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 0x8000);
  }
}

TEST_CASE("push/pop", "[interpreter]") {
  {
    bytecode_t const program [] =
    {
    (uc)OpCode::NOP, (uc)OpCode::NOP,
    (uc)OpCode::MOVE_I, 5, 1, 0, 0, 0,
    (uc)OpCode::PUSH_B, 5,
    (uc)OpCode::PUSH_B, 5,
    (uc)OpCode::PUSH_B, 5,
    (uc)OpCode::PUSH_B, 5,
    (uc)OpCode::POP_D, 0,
    (uc)OpCode::EXIT, 0,
    };
    REQUIRE(program[2] == (uc)OpCode::MOVE_I);
    Interpreter interpreter(program, nullptr, 128);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 0x01010101);
  }
}

TEST_CASE("print \"Hello World!\"", "[interpreter]") {
  int fd[2]{0};
  REQUIRE(pipe(fd) == 0);
  FILE *fp = fdopen(fd[1], "w");
  uc *split = (uc *)&fp;
  bytecode_t const program [] =
  {
  (uc)OpCode::NOP, (uc)OpCode::NOP,
  (uc)OpCode::NOP, (uc)OpCode::NOP,
  (uc)OpCode::NOP, (uc)OpCode::NOP,
  // set reg 11 to file *
  (uc)OpCode::MOVE_LI, 11, split[0], split[1], split[2], split[3], split[4], split[5], split[6], split[7],
  // make space for return value (4 byte int and 4 byte padding to align next argument)
  (uc)OpCode::SUB_I, stackPointerIndex, 8, 0, 0, 0,
  // push file pointer
  (uc)OpCode::PUSH_Q, 11,
  // push pointer to string
  (uc)OpCode::PUSH_Q, dataPointerIndex,
  (uc)OpCode::CALL_B, (uc)BuiltInFunction::PRINT_STRING,
  (uc)OpCode::SUB_I, stackPointerIndex, 4, 0, 0, 0,
  (uc)OpCode::PUSH_Q, 11,
  (uc)OpCode::CALL_B, (uc)BuiltInFunction::FFLUSH,
  (uc)OpCode::EXIT, 0,
  };
  REQUIRE(program[6] == (uc)OpCode::MOVE_LI);
  std::string data = "Hello World!\n";
  Interpreter interpreter(program, (bytecode_t *)data.data(), 128);
  interpreter.runProgram();
  std::string buffer;
  buffer.resize(data.size());
  REQUIRE(read(fd[0], buffer.data(), buffer.size())); // read from pipe
  CHECK(buffer == data);
  CHECK(interpreter.registers[0] == 0);
  close(fd[0]);
  close(fd[1]);
}

TEST_CASE("read line and store on stack", "[interpreter]") {
  int fd[2]{0};
  REQUIRE_FALSE(pipe(fd) == -1);
  FILE *fp = fdopen(fd[0],"r");
  FILE *fp_stdout = stdout;
  bytecode_t data[16];
  ((FILE**)&data)[0] = fp_stdout;
  ((FILE**)&data)[1] = fp;
  const std::string input = "Hello World!\n";
  REQUIRE(write(fd[1], input.data(), input.size()));
  bytecode_t const program [] =
  {
  (uc)OpCode::MOVE, 2, dataPointerIndex,
  (uc)OpCode::NOP, (uc)OpCode::NOP, (uc)OpCode::NOP,
  (uc)OpCode::ADD_I, 2, 8, 0, 0, 0,
  (uc)OpCode::LOAD_Q, 11, 2, // load stdin FILE * from data
  (uc)OpCode::NOP,
  (uc)OpCode::MOVE_I, 12, '\n', 0, 0, 0, // set j to newline char
  (uc)OpCode::NOP, (uc)OpCode::NOP,
  (uc)OpCode::SUB_I, stackPointerIndex, 32, 0, 0, 0, // make room on stack for array of 32 chars
  (uc)OpCode::MOVE, 13, stackPointerIndex, // set i to start of buffer

  // loop here
  (uc)OpCode::SUB_I, stackPointerIndex, 8, 0, 0, 0, // make room for return value (int) + padding (4 bytes)
  (uc)OpCode::PUSH_Q, 11, // add file ptr to stack
  (uc)OpCode::CALL_B, (uc)BuiltInFunction::READ_CHAR,
  (uc)OpCode::POP_D, 2, // load return value
  (uc)OpCode::STORE_B, 13, 2,
  (uc)OpCode::INC, 13, // increment i,
  (uc)OpCode::CMP, 2, 12, // compare character to newline
  (uc)OpCode::RS_JUMP_NE, (uc)(-20), // loop if its not newline

  // add null termination to end of string
  (uc)OpCode::XOR, 2, 2,
  (uc)OpCode::STORE_B, 13, 2, 

  (uc)OpCode::EXIT, 0,
  };
  REQUIRE(program[6] == (uc)OpCode::ADD_I);
  Interpreter interpreter(program, data, 128);
  interpreter.runProgram();
  std::string output{(char *)interpreter.registers[stackPointerIndex]};
  CHECK(output == input);
  close(fd[0]);
  close(fd[1]);
}


TEST_CASE("jump", "[interpreter]") {
  FILE *fp_stdout = stdout;
  FILE *fp_stdin = stdin;
  bytecode_t data[16];
  ((FILE**)&data)[0] = fp_stdout;
  ((FILE**)&data)[1] = fp_stdin;
  uc const program[] = {
    (uc)OpCode::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
    (uc)OpCode::NOP, (uc)OpCode::NOP, (uc)OpCode::NOP,
    (uc)OpCode::MOVE_I, 0, 1, 0, 0, 0,
    (uc)OpCode::NOP,(uc)OpCode::NOP,
    (uc)OpCode::MOVE_I, 1, 1, 0, 0, 0,
    (uc)OpCode::CMP, 0, 1,
    (uc)OpCode::XOR, 3, 3, // set exit reg to 0
    // index 26:
    (uc)OpCode::RS_JUMP_E, 8, // jumps to exit
    (uc)OpCode::MOVE_I, 3, 1, 0, 0, 0, // set exit reg to 1, this is how the test case is checked
    // index 50:
    (uc)OpCode::EXIT, 3
  };
  REQUIRE(program[6] == (uc)OpCode::MOVE_I);
  Interpreter interpreter(program, data, 128);
  CHECK(interpreter.runProgram() == 0);
}

TEST_CASE("loop", "[interpreter]") {
  FILE *fp_stdout = stdout;
  FILE *fp_stdin = stdin;
  bytecode_t data[16];
  ((FILE**)&data)[0] = fp_stdout;
  ((FILE**)&data)[1] = fp_stdin;
  bytecode_t const program [] =
  {
  (uc)OpCode::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
  (uc)OpCode::XOR, 12, 12, // initialize i
  (uc)OpCode::NOP, (uc)OpCode::NOP,
  (uc)OpCode::MOVE_I, 2, 10, 0, 0, 0,

  // start of loop:
  (uc)OpCode::CMP, 12, 2, // compare i to 9
  (uc)OpCode::RS_JUMP_GE, 6, // jump to end if result is greater than or equal to 10
  (uc)OpCode::INC, 12,
  (uc)OpCode::RS_JUMP, (uc)(-7),
  
  (uc)OpCode::EXIT, 12,
  };
  Interpreter interpreter(program, data, 128);
  uint64_t res = interpreter.runProgram();
  CHECK(res == 10);
}
