#pragma once

#include <cstdint>
#include <cstdlib>
#include <vector>
#include "bytecodeDesign/bytecodeDesign.hpp"

struct Interpreter {
  private:
  std::vector<bytecode_t> __stack;
  public:
  uint64_t registers [NUM_REGISTERS] {0};
  bytecode_t *stack {0};
  bytecode_t const * const program;
  int exitCode {0};
  bool z {0}; // zero flag
  bool p {0}; // positive flag
  bool running {true};
  Interpreter() = delete;
  Interpreter(
    const bytecode_t *programInstructions,
    bytecode_t *programData,
    uint64_t stackSize
  );
  int runProgram();
};
