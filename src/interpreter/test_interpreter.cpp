#include <catch2/catch_test_macros.hpp>
#include "interpreter.hpp"

#define uc unsigned char

TEST_CASE("move", "[interpreter]") {
  unsigned char const program [] = {(uc)OpCode::MOVE_W, 0, 1, 0, (uc)OpCode::EXIT, 0};
  REQUIRE(program[0] == (uc)OpCode::MOVE_W);
  Interpreter interpreter(program, 0, 2000);
  interpreter.runProgram();
  CHECK(interpreter.registers[0] == 1);
}
