#include <iostream>
#include "interpreter.hpp"
#include "../bytecodeDesign.hpp"

Interpreter::Interpreter(
  unsigned char *programInstructions,
  uint64_t startInstructionIndex,
  uint64_t stackSize
): program{programInstructions}, ip{startInstructionIndex} {
  stack = new uint8_t [stackSize];
}

Interpreter::~Interpreter() {
  if (stack) {
    delete[] stack;
  }
}

int64_t Interpreter::runProgram() {
  while (running) {
    executeNextInstruction();
  }
  return exitCode;
}

#define arithmeticOp_B(operator) \
  registers[program[ip++]] = (uint8_t)(registers[program[ip++]] operator registers[program[ip++]])

#define arithmeticOp_W(operator) \
  registers[program[ip++]] = (uint16_t)(registers[program[ip++]] operator registers[program[ip++]])

#define arithmeticOp_D(operator) \
  registers[program[ip++]] = (uint32_t)(registers[program[ip++]] operator registers[program[ip++]])

#define arithmeticOp_Q(operator) \
  registers[program[ip++]] = registers[program[ip++]] operator registers[program[ip++]]

#define arithmeticOp_F(operator)  \
  uint64_t dest = program[ip++]; \
  const double temp = *(double *)&registers[program[ip++]] operator *(double *)&registers[program[ip++]];  \
  registers[dest] = *(uint64_t *)&temp

void Interpreter::executeNextInstruction() {
  OpCode c;
  switch (c) {
    case OpCode::EXIT: {
      exitCode = 1;
      running = false;
      break;
    }
    case OpCode::CALL: {
      break;
    }
    case OpCode::LOAD_B: {
      registers[program[ip++]] = *(uint8_t*)registers[program[ip++]];
      break;
    }
    case OpCode::LOAD_W: {
      registers[program[ip++]] = *(uint16_t*)registers[program[ip++]];
      break;
    }
    case OpCode::LOAD_D: {
      registers[program[ip++]] = *(uint32_t*)registers[program[ip++]];
      break;
    }
    case OpCode::LOAD_Q: {
      registers[program[ip++]] = *(uint64_t*)registers[program[ip++]];
      break;
    }
    case OpCode::STORE_B: {
      *(uint8_t*)registers[program[ip++]] = (uint8_t)registers[program[ip++]];
      break;
    }
    case OpCode::STORE_W: {
      *(uint16_t*)registers[program[ip++]] = (uint16_t)registers[program[ip++]];
      break;
    }
    case OpCode::STORE_D: {
      *(uint32_t*)registers[program[ip++]] = (uint32_t)registers[program[ip++]];
      break;
    }
    case OpCode::STORE_Q: {
      *(uint64_t*)registers[program[ip++]] = registers[program[ip++]];
      break;
    }
    case OpCode::JUMP: {
      ip = *(uint64_t *)(program + ip);
      break;
    }
    case OpCode::JUMP_Z: {
      if (z) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCode::JUMP_NZ: {
      if (!z) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCode::JUMP_P: {
      if (p) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCode::JUMP_N: {
      if (!p) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCode::MOVE: {
      registers[program[ip++]] = registers[program[ip++]];
      break;
    }
    case OpCode::MOVE_B: {
      registers[program[ip++]] = program[ip++];
      break;
    }
    case OpCode::MOVE_W: {
      registers[program[ip++]] = *(uint16_t *)(program + ip);
      ip += 2;
      break;
    }
    case OpCode::MOVE_D: {
      registers[program[ip++]] = *(uint32_t *)(program + ip);
      ip += 4;
      break;
    }
    case OpCode::MOVE_Q: {
      registers[program[ip++]] = *(uint64_t *)(program + ip);
      ip += 8;
      break;
    }
    case OpCode::PUSH_B: {
      stack[sp++] = registers[program[ip++]];
      break;
    }
    case OpCode::PUSH_W: {
      *(uint16_t *)(stack + sp) = registers[program[ip++]];
      sp += 2;
      break;
    }
    case OpCode::PUSH_D: {
      *(uint32_t *)(stack + sp) = registers[program[ip++]];
      sp += 4;
      break;
    }
    case OpCode::PUSH_Q: {
      *(uint64_t *)(stack + sp) = registers[program[ip++]];
      sp += 8;
      break;
    }
    case OpCode::POP_B: {
      registers[program[ip++]] = stack[sp--];
      break;
    }
    case OpCode::POP_W: {
      registers[program[ip++]] = *(uint16_t *)(stack + sp);
      sp -= 2;
      break;
    }
    case OpCode::POP_D: {
      registers[program[ip++]] = *(uint32_t *)(stack + sp);
      sp -= 4;
      break;
    }
    case OpCode::POP_Q: {
      registers[program[ip++]] = *(uint64_t *)(stack + sp);
      sp -= 8;
      break;
    }
    case OpCode::ADD_B: {
      arithmeticOp_B(+);
      break;
    }
    case OpCode::ADD_W: {
      arithmeticOp_W(+);
      break;
    }
    case OpCode::ADD_D: {
      arithmeticOp_D(+);
      break;
    }
    case OpCode::ADD_Q: {
      arithmeticOp_Q(+);
      break;
    }
    case OpCode::SUB_B: {
      arithmeticOp_B(-);
      break;
    }
    case OpCode::SUB_W: {
      arithmeticOp_W(-);
      break;
    }
    case OpCode::SUB_D: {
      arithmeticOp_D(-);
      break;
    }
    case OpCode::SUB_Q: {
      arithmeticOp_Q(-);
      break;
    }
    case OpCode::MUL_B: {
      arithmeticOp_B(*);
      break;
    }
    case OpCode::MUL_W: {
      arithmeticOp_W(*);
      break;
    }
    case OpCode::MUL_D: {
      arithmeticOp_D(*);
      break;
    }
    case OpCode::MUL_Q: {
      arithmeticOp_Q(*);
      break;
    }
    case OpCode::DIV_B: {
      arithmeticOp_B(/);
      break;
    }
    case OpCode::DIV_W: {
      arithmeticOp_W(/);
      break;
    }
    case OpCode::DIV_D: {
      arithmeticOp_D(/);
      break;
    }
    case OpCode::DIV_Q: {
      arithmeticOp_Q(/);
      break;
    }
    case OpCode::MOD_B: {
      arithmeticOp_B(%);
      break;
    }
    case OpCode::MOD_W: {
      arithmeticOp_W(%);
      break;
    }
    case OpCode::MOD_D: {
      arithmeticOp_D(%);
      break;
    }
    case OpCode::MOD_Q: {
      arithmeticOp_Q(%);
      break;
    }
    case OpCode::OR_B: {
      arithmeticOp_B(|);
      break;
    }
    case OpCode::OR_W: {
      arithmeticOp_W(|);
      break;
    }
    case OpCode::OR_D: {
      arithmeticOp_D(|);
      break;
    }
    case OpCode::OR_Q: {
      arithmeticOp_Q(|);
      break;
    }
    case OpCode::AND_B: {
      arithmeticOp_B(&);
      break;
    }
    case OpCode::AND_W: {
      arithmeticOp_W(&);
      break;
    }
    case OpCode::AND_D: {
      arithmeticOp_D(&);
      break;
    }
    case OpCode::AND_Q: {
      arithmeticOp_Q(&);
      break;
    }
    case OpCode::XOR_B: {
      arithmeticOp_B(^);
      break;
    }
    case OpCode::XOR_W: {
      arithmeticOp_W(^);
      break;
    }
    case OpCode::XOR_D: {
      arithmeticOp_D(^);
      break;
    }
    case OpCode::XOR_Q: {
      arithmeticOp_Q(^);
      break;
    }
    case OpCode::SHIFT_L_B: {
      arithmeticOp_B(<<);
      break;
    }
    case OpCode::SHIFT_L_W: {
      arithmeticOp_W(<<);
      break;
    }
    case OpCode::SHIFT_L_D: {
      arithmeticOp_D(<<);
      break;
    }
    case OpCode::SHIFT_L_Q: {
      arithmeticOp_Q(<<);
      break;
    }
    case OpCode::SHIFT_R_B: {
      arithmeticOp_B(>>);
      break;
    }
    case OpCode::SHIFT_R_W: {
      arithmeticOp_W(>>);
      break;
    }
    case OpCode::SHIFT_R_D: {
      arithmeticOp_D(>>);
      break;
    }
    case OpCode::SHIFT_R_Q: {
      arithmeticOp_Q(>>);
      break;
    }
    case OpCode::F_ADD: {
      arithmeticOp_F(+);
      break;
    }
    case OpCode::F_SUB: {
      arithmeticOp_F(-);
      break;
    }
    case OpCode::F_MUL: {
      arithmeticOp_F(*);
      break;
    }
    case OpCode::F_DIV: {
      arithmeticOp_F(/);
      break;
    }
    default: {
      std::cerr << "Runtime Error: Invalid OpCode [" << (uint8_t)c << "]\n";
      exit(1);
    }
  }
}
