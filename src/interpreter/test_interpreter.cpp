#include <unistd.h>
#include <iostream>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include "interpreter.hpp"

#define uc unsigned char

TEST_CASE("move", "[interpreter]") {
  {
    unsigned char const program [] = {(uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0, (uc)OpCodes::EXIT, 0};
    REQUIRE(program[0] == (uc)OpCodes::MOVE_I);
    Interpreter interpreter(program, nullptr, 2000);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 1);
  }

  {
    unsigned char const program [] = {(uc)OpCodes::MOVE_I, 0, 0, 0x80, 0, 0, (uc)OpCodes::EXIT, 0};
    REQUIRE(program[0] == (uc)OpCodes::MOVE_I);
    Interpreter interpreter(program, nullptr, 128);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 0x8000);
  }
}

TEST_CASE("push/pop", "[interpreter]") {
  {
    unsigned char const program [] =
    {
    (uc)OpCodes::MOVE_I, 5, 1, 0, 0, 0,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::PUSH_B, 5,
    (uc)OpCodes::POP_D, 0,
    (uc)OpCodes::EXIT, 0,
    };
    REQUIRE(program[0] == (uc)OpCodes::MOVE_I);
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
  (uc)OpCodes::MOVE_I, 11, split[4], split[5], split[6], split[7],
  (uc)OpCodes::SHIFT_L_I, 11, 11, 32, 0, 0, 0,
  (uc)OpCodes::ADD_I, 11, 11, split[0], split[1], split[2], split[3],
  (uc)OpCodes::MOVE, 12, dataPointerIndex,
  (uc)OpCodes::CALL, (uc)BuiltInFunctions::PRINT_STRING,
  (uc)OpCodes::CALL, (uc)BuiltInFunctions::FFLUSH,
  (uc)OpCodes::EXIT, 0,
  };
  REQUIRE(program[0] == (uc)OpCodes::MOVE_I);
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
  uc *split = (uc *)&fp;
  const std::string input = "Hello World!\n";
  write(fd[1], input.data(), input.size()); // write to pipe
  unsigned char const program [] =
  {
  (uc)OpCodes::MOVE_I, 11, split[4], split[5], split[6], split[7],
  (uc)OpCodes::SHIFT_L_I, 11, 11, 32, 0, 0, 0,
  (uc)OpCodes::ADD_I, 11, 11, split[0], split[1], split[2], split[3],

  (uc)OpCodes::MOVE_I, 2, '\n', 0, 0, 0, // set j to newline char
  (uc)OpCodes::SUB_I, stackPointerIndex, stackPointerIndex, 20, 0, 0, 0, // make room on stack for 20 bytes
  (uc)OpCodes::MOVE, 1, stackPointerIndex, // set i to start of buffer
  (uc)OpCodes::MOVE, 3, instructionPointerIndex, // loop marker
  (uc)OpCodes::ADD_I, 3, 3, 9, 0, 0, 0,

  // loop here
  (uc)OpCodes::CALL, (uc)BuiltInFunctions::GET_CHAR,
  (uc)OpCodes::STORE_B, 1, 10, // add to stack
  (uc)OpCodes::ADD_I, 1, 1, 1, 0, 0, 0, // increment i,
  (uc)OpCodes::CMP, 10, 2, // compare character to newline
  (uc)OpCodes::JUMP_NZ, 3, // loop if its not newline

  // add null termination to end of string
  (uc)OpCodes::XOR, 2, 2,
  (uc)OpCodes::STORE_B, 1, 2, 

  (uc)OpCodes::EXIT, 0,
  };
  REQUIRE(program[0] == (uc)OpCodes::MOVE_I);
  Interpreter interpreter(program, nullptr, 128);
  interpreter.runProgram();
  std::string output{(char *)interpreter.registers[stackPointerIndex]};
  CHECK(output == input);
  close(fd[0]);
  close(fd[1]);
}
