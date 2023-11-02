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
  REQUIRE_FALSE(pipe(fd) == -1);
  FILE *fp = fdopen(fd[1],"w");
  uc *split = (uc *)&fp;
  unsigned char const program [] =
  {
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::MOVE_I, 11, split[4], split[5], split[6], split[7],
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::SHIFT_L_I, 11, 32, 0, 0, 0,
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::ADD_I, 11, split[0], split[1], split[2], split[3],
  (uc)OpCodes::MOVE, 12, dataPointerIndex,
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_STRING,
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::FFLUSH,
  (uc)OpCodes::EXIT, 0,
  };
  REQUIRE(program[2] == (uc)OpCodes::MOVE_I);
  std::string data = "Hello World!\n";
  Interpreter interpreter(program, (unsigned char *)data.data(), 128);
  interpreter.runProgram();
  std::string buffer;
  buffer.resize(data.size());
  read(fd[0], buffer.data(), buffer.size()); // read from pipe
  CHECK(buffer == data);
  CHECK(interpreter.registers[0] == 0);
  close(fd[0]);
  close(fd[1]);
}

TEST_CASE("read line and store on stack", "[interpreter]") {
  int fd[2]{0};
  REQUIRE_FALSE(pipe(fd) == -1);
  FILE *fp = fdopen(fd[0],"r");
  uc *s_stdin = (uc *)&fp;
  FILE *fp_stdout = stdout;
  uc *s_stdout = (uc *)&fp_stdout;
  uc data[] = {
    s_stdout[0], s_stdout[1], s_stdout[2], s_stdout[3], s_stdout[4], s_stdout[5], s_stdout[6], s_stdout[7], // stdout FILE *
    s_stdin[0], s_stdin[1], s_stdin[2], s_stdin[3], s_stdin[4], s_stdin[5], s_stdin[6], s_stdin[7], // stdin FILE *
  };
  const std::string input = "Hello World!\n";
  write(fd[1], input.data(), input.size()); // write to pipe
  unsigned char const program [] =
  {
  (uc)OpCodes::MOVE, 2, dataPointerIndex,
  (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::ADD_I, 2, 8, 0, 0, 0,
  (uc)OpCodes::LOAD_Q, 11, 2, // load stdin FILE * from data
  (uc)OpCodes::NOP,
  (uc)OpCodes::MOVE_I, 2, '\n', 0, 0, 0, // set j to newline char
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::SUB_I, stackPointerIndex, 20, 0, 0, 0, // make room on stack for 20 bytes
  (uc)OpCodes::MOVE, 1, stackPointerIndex, // set i to start of buffer

  // loop here
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::GET_CHAR,
  (uc)OpCodes::STORE_B, 1, 10, // add to stack
  (uc)OpCodes::INC, 1, // increment i,
  (uc)OpCodes::CMP, 10, 2, // compare character to newline
  (uc)OpCodes::RB_JUMP_NZ, (uc)(-10), // loop if its not newline

  // add null termination to end of string
  (uc)OpCodes::XOR, 2, 2,
  (uc)OpCodes::STORE_B, 1, 2, 

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
  uc *s_stdout = (uc *)&fp_stdout;
  FILE *fp_stdin = stdin;
  uc *s_stdin = (uc *)&fp_stdin;
  uc data[] = {
    s_stdout[0], s_stdout[1], s_stdout[2], s_stdout[3], s_stdout[4], s_stdout[5], s_stdout[6], s_stdout[7], // stdout FILE *
    s_stdin[0], s_stdin[1], s_stdin[2], s_stdin[3], s_stdin[4], s_stdin[5], s_stdin[6], s_stdin[7], // stdin FILE *
  };
  uc const program[] = {
    (uc)OpCodes::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
    (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0,
    (uc)OpCodes::NOP,(uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 1, 1, 0, 0, 0,
    (uc)OpCodes::CMP, 0, 1,
    (uc)OpCodes::XOR, 3, 3, // set exit reg to 0
    // index 26:
    (uc)OpCodes::RB_JUMP_Z, 24, // jumps to exit
    (uc)OpCodes::MOVE_I, 3, 1, 0, 0, 0, // set exit reg to 1, this is how the test case is checked
    (uc)OpCodes::MOVE_I, 12, 'P', '\n', 0, 0,
    (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_CHAR,
    (uc)OpCodes::SHIFT_R_I, 12, 8, 0, 0, 0,// shift reg 12 over so that '\n' is the lsb
    (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_CHAR,
    // index 50:
    (uc)OpCodes::EXIT, 3
  };
  REQUIRE(program[6] == (uc)OpCodes::MOVE_I);
  Interpreter interpreter(program, data, 128);
  CHECK(interpreter.runProgram() == 0);
}

TEST_CASE("loop", "[interpreter]") {
  FILE *fp_stdout = stdout;
  uc *s_stdout = (uc *)&fp_stdout;
  FILE *fp_stdin = stdin;
  uc *s_stdin = (uc *)&fp_stdin;
  uc data[] = {
    s_stdout[0], s_stdout[1], s_stdout[2], s_stdout[3], s_stdout[4], s_stdout[5], s_stdout[6], s_stdout[7], // stdout FILE *
    s_stdin[0], s_stdin[1], s_stdin[2], s_stdin[3], s_stdin[4], s_stdin[5], s_stdin[6], s_stdin[7], // stdin FILE *
  };
  unsigned char const program [] =
  {
  (uc)OpCodes::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
  (uc)OpCodes::XOR, 12, 12, // initialize i
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::MOVE_I, 2, 10, 0, 0, 0,

  // start of loop:
  (uc)OpCodes::CMP, 12, 2, // compare i to 9
  (uc)OpCodes::RB_JUMP_NN, 6, // jump to end if result is not negative
  (uc)OpCodes::INC, 12,
  (uc)OpCodes::RB_JUMP, (uc)(-7),
  
  (uc)OpCodes::EXIT, 12,
  };
  Interpreter interpreter(program, data, 128);
  uint64_t res = interpreter.runProgram();
  CHECK(res == 10);
}
