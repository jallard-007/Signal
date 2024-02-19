#pragma once

#include <cstdint>
#include <cstdlib>
#include <vector>
#include "../bytecodeDesign.hpp"

struct Interpreter {
  private:
  std::vector<unsigned char> __stack;
  public:
  uint64_t registers [NUM_REGISTERS] {0};
  unsigned char *stack {0};
  unsigned char const * const program;
  int64_t exitCode {0};
  bool z {0}; // zero flag
  bool p {0}; // positive flag
  bool running {true};
  Interpreter() = delete;
  Interpreter(
    unsigned char const *programInstructions,
    unsigned char *programData,
    uint64_t stackSize
  );
  int64_t runProgram();
};
