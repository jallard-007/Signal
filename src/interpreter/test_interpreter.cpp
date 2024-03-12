#include <unistd.h>
#include <iostream>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include "interpreter.hpp"

#define uc unsigned char

TEST_CASE("move", "[interpreter]") {
  {
    unsigned char const program [] = {(uc)OpCodes::NOP,(uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0, (uc)OpCodes::EXIT, 0};
    REQUIRE(program[2] == (uc)OpCodes::MOVE_I);
    Interpreter interpreter(program, nullptr, 128);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 1);
  }

  {
    unsigned char const program [] = {(uc)OpCodes::NOP,(uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 0, 0, 0x80, 0, 0, (uc)OpCodes::EXIT, 0};
    REQUIRE(program[2] == (uc)OpCodes::MOVE_I);
    Interpreter interpreter(program, nullptr, 128);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 0x8000);
  }
}

TEST_CASE("push/pop", "[interpreter]") {
  {
    unsigned char const program [] =
    {
    (uc)OpCodes::NOP, (uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 5, 1, 0, 0, 0,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::POP_D, 0,
    (uc)OpCodes::EXIT, 0,
    };
    REQUIRE(program[2] == (uc)OpCodes::MOVE_I);
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
  unsigned char const program [] =
  {
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  // set reg 11 to file *
  (uc)OpCodes::MOVE_LI, 11, split[0], split[1], split[2], split[3], split[4], split[5], split[6], split[7],
  // make space for return value (4 byte int and 4 byte padding to align next argument)
  (uc)OpCodes::SUB_I, stackPointerIndex, 8, 0, 0, 0,
  // push file pointer
  (uc)OpCodes::PUSH_Q, 11,
  // push pointer to string
  (uc)OpCodes::PUSH_Q, dataPointerIndex,
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_STRING,
  (uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0,
  (uc)OpCodes::PUSH_Q, 11,
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::FFLUSH,
  (uc)OpCodes::EXIT, 0,
  };
  REQUIRE(program[6] == (uc)OpCodes::MOVE_LI);
  std::string data = "Hello World!\n";
  Interpreter interpreter(program, (unsigned char *)data.data(), 128);
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
  uc data[16];
  ((FILE**)&data)[0] = fp_stdout;
  ((FILE**)&data)[1] = fp;
  const std::string input = "Hello World!\n";
  REQUIRE(write(fd[1], input.data(), input.size()));
  unsigned char const program [] =
  {
  (uc)OpCodes::MOVE, 2, dataPointerIndex,
  (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::ADD_I, 2, 8, 0, 0, 0,
  (uc)OpCodes::LOAD_Q, 11, 2, // load stdin FILE * from data
  (uc)OpCodes::NOP,
  (uc)OpCodes::MOVE_I, 12, '\n', 0, 0, 0, // set j to newline char
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::SUB_I, stackPointerIndex, 32, 0, 0, 0, // make room on stack for array of 32 chars
  (uc)OpCodes::MOVE, 13, stackPointerIndex, // set i to start of buffer

  // loop here
  (uc)OpCodes::SUB_I, stackPointerIndex, 8, 0, 0, 0, // make room for return value (int) + padding (4 bytes)
  (uc)OpCodes::PUSH_Q, 11, // add file ptr to stack
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::READ_CHAR,
  (uc)OpCodes::POP_D, 2, // load return value
  (uc)OpCodes::STORE_B, 13, 2,
  (uc)OpCodes::INC, 13, // increment i,
  (uc)OpCodes::CMP, 2, 12, // compare character to newline
  (uc)OpCodes::RS_JUMP_NE, (uc)(-20), // loop if its not newline

  // add null termination to end of string
  (uc)OpCodes::XOR, 2, 2,
  (uc)OpCodes::STORE_B, 13, 2, 

  (uc)OpCodes::EXIT, 0,
  };
  REQUIRE(program[6] == (uc)OpCodes::ADD_I);
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
  uc data[16];
  ((FILE**)&data)[0] = fp_stdout;
  ((FILE**)&data)[1] = fp_stdin;
  uc const program[] = {
    (uc)OpCodes::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
    (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0,
    (uc)OpCodes::NOP,(uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 1, 1, 0, 0, 0,
    (uc)OpCodes::CMP, 0, 1,
    (uc)OpCodes::XOR, 3, 3, // set exit reg to 0
    // index 26:
    (uc)OpCodes::RS_JUMP_E, 8, // jumps to exit
    (uc)OpCodes::MOVE_I, 3, 1, 0, 0, 0, // set exit reg to 1, this is how the test case is checked
    // index 50:
    (uc)OpCodes::EXIT, 3
  };
  REQUIRE(program[6] == (uc)OpCodes::MOVE_I);
  Interpreter interpreter(program, data, 128);
  CHECK(interpreter.runProgram() == 0);
}

TEST_CASE("loop", "[interpreter]") {
  FILE *fp_stdout = stdout;
  FILE *fp_stdin = stdin;
  uc data[16];
  ((FILE**)&data)[0] = fp_stdout;
  ((FILE**)&data)[1] = fp_stdin;
  unsigned char const program [] =
  {
  (uc)OpCodes::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
  (uc)OpCodes::XOR, 12, 12, // initialize i
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::MOVE_I, 2, 10, 0, 0, 0,

  // start of loop:
  (uc)OpCodes::CMP, 12, 2, // compare i to 9
  (uc)OpCodes::RS_JUMP_GE, 6, // jump to end if result is greater than or equal to 10
  (uc)OpCodes::INC, 12,
  (uc)OpCodes::RS_JUMP, (uc)(-7),
  
  (uc)OpCodes::EXIT, 12,
  };
  Interpreter interpreter(program, data, 128);
  uint64_t res = interpreter.runProgram();
  CHECK(res == 10);
}
