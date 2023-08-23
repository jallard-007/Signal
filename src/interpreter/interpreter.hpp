#include <vector>
#include <cstdint>

#define NUM_REGISTERS 16

struct Interpreter {
  uint64_t registers [NUM_REGISTERS] {0};
  uint8_t *stack {0};
  unsigned char const * const program;
  uint64_t ip {0};
  uint64_t sp {0};
  int64_t exitCode {0};
  bool z {0};
  bool p {0};
  bool running {true};
  Interpreter() = delete;
  Interpreter(unsigned char *programInstructions, uint64_t startInstructionIndex, uint64_t stackSize);
  ~Interpreter();
  int64_t runProgram();
  void executeNextInstruction();
};
