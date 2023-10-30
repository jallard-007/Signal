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
  if (pipe(fd) == -1) {
    CHECK(1 == 0);
    return;
  }
  FILE *fp = fdopen(fd[1],"w");
  uc *split = (uc *)&fp;
  {
    unsigned char const program [] =
    {
    (uc)OpCodes::MOVE_I, 11, split[4], split[5], split[6], split[7],
    (uc)OpCodes::SHIFT_L_I, 11, 11, 32, 0, 0, 0,
    (uc)OpCodes::ADD_I, 11, 11, split[0], split[1], split[2], split[3],
    (uc)OpCodes::MOVE, 12, dataPointerIndex,
    (uc)OpCodes::CALL, (uc)BuiltInFunctions::PRINT_STRING,
    (uc)OpCodes::CALL, (uc)34,
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
  }
  close(fd[0]);
  close(fd[1]);
}
