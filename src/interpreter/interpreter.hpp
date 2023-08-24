#include <cstdint>
#include <cstdlib>
#include "../bytecodeDesign.hpp"

#define NUM_REGISTERS 16

struct Interpreter {
  uint64_t registers [NUM_REGISTERS] {0};
  uint8_t *stack {0};
  unsigned char const * const program;
  uint64_t ip {0}; // instruction pointer
  uint64_t sp {0}; // stack pointer
  int64_t exitCode {0};
  bool z {0}; // zero flag
  bool p {0}; // positive flag
  bool running {true};
  Interpreter() = delete;
  Interpreter(unsigned char const *programInstructions, uint64_t startInstructionIndex, uint64_t stackSize);
  ~Interpreter();
  int64_t runProgram();
  void executeNextInstruction();
};
