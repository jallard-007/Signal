#include <catch2/catch_test_macros.hpp>
#include "interpreter.hpp"

#define uc unsigned char

TEST_CASE("move", "[interpreter]") {
  {
    unsigned char const program [] = {(uc)OpCodes::MOVE_I, 0, 1, 0, (uc)OpCodes::EXIT, 0};
    REQUIRE(program[0] == (uc)OpCodes::MOVE_I);
    Interpreter interpreter(program, nullptr, 2000);
    interpreter.runProgram();
    CHECK(interpreter.registers[0] == 1);
  }

  {
    unsigned char const program [] = {(uc)OpCodes::MOVE_I, 0, 0, 0x80, (uc)OpCodes::EXIT, 0};
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
    (uc)OpCodes::MOVE_I, 5, 1, 0,
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
