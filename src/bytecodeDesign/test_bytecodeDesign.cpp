#include <iostream>
#include "bytecodeDesign.hpp"
#include <catch2/catch_test_macros.hpp>


TEST_CASE("test bytecode string mappings", "[bytecode design]") {
  CHECK(std::string("NOP") == bytecode_t_to_op[(unsigned char)OpCode::NOP]);
  CHECK(std::string("PUSH_D") == bytecode_t_to_op[(unsigned char)OpCode::PUSH_D]);
  CHECK(std::string("XOR_I") == bytecode_t_to_op[(unsigned char)OpCode::XOR_I]);
  CHECK(std::string("JUMP_GE") == bytecode_t_to_op[(unsigned char)OpCode::JUMP_GE]);
  CHECK(std::string("GET_NE") == bytecode_t_to_op[(unsigned char)OpCode::GET_NE]);
  CHECK(std::string("F_DIV_I") == bytecode_t_to_op[(unsigned char)OpCode::F_DIV_I]);
}

