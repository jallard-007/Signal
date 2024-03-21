#include <iostream>
#include "bytecodeDesign.hpp"
#include <catch2/catch_test_macros.hpp>


TEST_CASE("test enums", "[bytecode design]") {
  REQUIRE((uint32_t)OpCode::FIRST == 0);
  REQUIRE((uint32_t)OpCode::LAST == (uint32_t)OpCode::F_DIV_I);
  REQUIRE((uint32_t)BuiltInFunction::FIRST == 0);
  REQUIRE((uint32_t)BuiltInFunction::LAST == (uint32_t)BuiltInFunction::SEEK);
}
