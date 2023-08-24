#include <iostream>
#include "interpreter.hpp"

Interpreter::Interpreter(
  unsigned char const *programInstructions,
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
  registers[program[ip]] = (uint8_t)(registers[program[ip+1]] operator registers[program[ip+2]]); \
  ip += 3

#define arithmeticOp_W(operator) \
  registers[program[ip]] = (uint16_t)(registers[program[ip+1]] operator registers[program[ip+2]]); \
  ip += 3

#define arithmeticOp_D(operator) \
  registers[program[ip]] = (uint32_t)(registers[program[ip+1]] operator registers[program[ip+2]]); \
  ip += 3

#define arithmeticOp_Q(operator) \
  registers[program[ip]] = registers[program[ip+1]] operator registers[program[ip+2]]; \
  ip += 3

#define arithmeticOp_F(operator) \
  uint64_t dest = program[ip++]; \
  const double temp = *(double *)&registers[program[ip]] operator *(double *)&registers[program[ip+1]]; \
  ip += 2; \
  registers[dest] = *(uint64_t *)&temp

void Interpreter::executeNextInstruction() {
  OpCodes op = (OpCodes)program[ip++];
  switch (op) {
    case OpCodes::NOP: {
      break;
    }
    case OpCodes::EXIT: {
      exitCode = 1;
      running = false;
      break;
    }
    case OpCodes::CALL: {
      BuiltInFunctions func = (BuiltInFunctions)program[ip++];
      switch (func) {
        case BuiltInFunctions::ALLOCATE: {
          registers[10] = (uint64_t)malloc(registers[11]);
          ip += 2;
          break;
        }
        case BuiltInFunctions::REALLOCATE: {
          registers[10] = (uint64_t)realloc((void *)registers[11], registers[12]);
          ip += 2;
          break;
        }
        case BuiltInFunctions::DEALLOCATE: {
          free((void *)registers[11]);
          break;
        }
        case BuiltInFunctions::PRINT_STRING: {
          std::cout << (char *)registers[11];
          break;
        }
        case BuiltInFunctions::PRINT_CHAR: {
          std::cout << (char)registers[11];
          break;
        }
        case BuiltInFunctions::PRINT_SIGNED: {
          std::cout << (int64_t)registers[11];
          break;
        }
        case BuiltInFunctions::PRINT_UNSIGNED: {
          std::cout << registers[11];
          break;
        }
        case BuiltInFunctions::PRINT_HEX: {
          std::cout << (void *)registers[11];
          break;
        }
        default: {
          std::cerr << "Runtime Error: Invalid Function Code [" << (uint32_t)func << "]\n";
          exit(1);
        }
      }
      break;
    }
    case OpCodes::CMP: {
      int64_t res = (int64_t)registers[program[ip]] - (int64_t)registers[program[ip+1]];
      ip += 2;
      if (res == 0) {
        z = true;
        p = false;
      } else if (res > 0) {
        z = false;
        p = true;
      } else {
        z = false;
        p = false;
      }
      break;
    }
    case OpCodes::SET_Z: {
      registers[program[ip++]] = z;
      break;
    }
    case OpCodes::SET_P: {
      registers[program[ip++]] = p;
      break;
    }
    case OpCodes::LOAD_B: {
      registers[program[ip]] = *(uint8_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::LOAD_W: {
      registers[program[ip]] = *(uint16_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::LOAD_D: {
      registers[program[ip]] = *(uint32_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::LOAD_Q: {
      registers[program[ip]] = *(uint64_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::STORE_B: {
      *(uint8_t*)registers[program[ip]] = (uint8_t)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::STORE_W: {
      *(uint16_t*)registers[program[ip]] = (uint16_t)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::STORE_D: {
      *(uint32_t*)registers[program[ip]] = (uint32_t)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::STORE_Q: {
      *(uint64_t*)registers[program[ip]] = registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::JUMP: {
      ip = *(uint64_t *)(program + ip);
      break;
    }
    case OpCodes::JUMP_Z: {
      if (z) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCodes::JUMP_NZ: {
      if (!z) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCodes::JUMP_P: {
      if (p) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCodes::JUMP_N: {
      if (!p && !z) {
        ip = *(uint64_t *)(program + ip);
      }
      break;
    }
    case OpCodes::MOVE: {
      registers[program[ip]] = registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::MOVE_B: {
      registers[program[ip]] = program[ip+1];
      ip += 2;
      break;
    }
    case OpCodes::MOVE_W: {
      registers[program[ip]] = *(uint16_t *)(program + ip + 1);
      ip += 3;
      break;
    }
    case OpCodes::MOVE_D: {
      registers[program[ip]] = *(uint32_t *)(program + ip + 1);
      ip += 5;
      break;
    }
    case OpCodes::MOVE_Q: {
      registers[program[ip]] = *(uint64_t *)(program + ip + 1);
      ip += 9;
      break;
    }
    case OpCodes::PUSH_B: {
      stack[sp++] = registers[program[ip++]];
      break;
    }
    case OpCodes::PUSH_W: {
      *(uint16_t *)(stack + sp) = registers[program[ip++]];
      sp += 2;
      break;
    }
    case OpCodes::PUSH_D: {
      *(uint32_t *)(stack + sp) = registers[program[ip++]];
      sp += 4;
      break;
    }
    case OpCodes::PUSH_Q: {
      *(uint64_t *)(stack + sp) = registers[program[ip++]];
      sp += 8;
      break;
    }
    case OpCodes::POP_B: {
      registers[program[ip++]] = stack[sp--];
      break;
    }
    case OpCodes::POP_W: {
      registers[program[ip++]] = *(uint16_t *)(stack + sp);
      sp -= 2;
      break;
    }
    case OpCodes::POP_D: {
      registers[program[ip++]] = *(uint32_t *)(stack + sp);
      sp -= 4;
      break;
    }
    case OpCodes::POP_Q: {
      registers[program[ip++]] = *(uint64_t *)(stack + sp);
      sp -= 8;
      break;
    }
    case OpCodes::ADD_B: {
      arithmeticOp_B(+);
      break;
    }
    case OpCodes::ADD_W: {
      arithmeticOp_W(+);
      break;
    }
    case OpCodes::ADD_D: {
      arithmeticOp_D(+);
      break;
    }
    case OpCodes::ADD_Q: {
      arithmeticOp_Q(+);
      break;
    }
    case OpCodes::SUB_B: {
      arithmeticOp_B(-);
      break;
    }
    case OpCodes::SUB_W: {
      arithmeticOp_W(-);
      break;
    }
    case OpCodes::SUB_D: {
      arithmeticOp_D(-);
      break;
    }
    case OpCodes::SUB_Q: {
      arithmeticOp_Q(-);
      break;
    }
    case OpCodes::MUL_B: {
      arithmeticOp_B(*);
      break;
    }
    case OpCodes::MUL_W: {
      arithmeticOp_W(*);
      break;
    }
    case OpCodes::MUL_D: {
      arithmeticOp_D(*);
      break;
    }
    case OpCodes::MUL_Q: {
      arithmeticOp_Q(*);
      break;
    }
    case OpCodes::DIV_B: {
      arithmeticOp_B(/);
      break;
    }
    case OpCodes::DIV_W: {
      arithmeticOp_W(/);
      break;
    }
    case OpCodes::DIV_D: {
      arithmeticOp_D(/);
      break;
    }
    case OpCodes::DIV_Q: {
      arithmeticOp_Q(/);
      break;
    }
    case OpCodes::MOD_B: {
      arithmeticOp_B(%);
      break;
    }
    case OpCodes::MOD_W: {
      arithmeticOp_W(%);
      break;
    }
    case OpCodes::MOD_D: {
      arithmeticOp_D(%);
      break;
    }
    case OpCodes::MOD_Q: {
      arithmeticOp_Q(%);
      break;
    }
    case OpCodes::OR_B: {
      arithmeticOp_B(|);
      break;
    }
    case OpCodes::OR_W: {
      arithmeticOp_W(|);
      break;
    }
    case OpCodes::OR_D: {
      arithmeticOp_D(|);
      break;
    }
    case OpCodes::OR_Q: {
      arithmeticOp_Q(|);
      break;
    }
    case OpCodes::AND_B: {
      arithmeticOp_B(&);
      break;
    }
    case OpCodes::AND_W: {
      arithmeticOp_W(&);
      break;
    }
    case OpCodes::AND_D: {
      arithmeticOp_D(&);
      break;
    }
    case OpCodes::AND_Q: {
      arithmeticOp_Q(&);
      break;
    }
    case OpCodes::XOR_B: {
      arithmeticOp_B(^);
      break;
    }
    case OpCodes::XOR_W: {
      arithmeticOp_W(^);
      break;
    }
    case OpCodes::XOR_D: {
      arithmeticOp_D(^);
      break;
    }
    case OpCodes::XOR_Q: {
      arithmeticOp_Q(^);
      break;
    }
    case OpCodes::SHIFT_L_B: {
      arithmeticOp_B(<<);
      break;
    }
    case OpCodes::SHIFT_L_W: {
      arithmeticOp_W(<<);
      break;
    }
    case OpCodes::SHIFT_L_D: {
      arithmeticOp_D(<<);
      break;
    }
    case OpCodes::SHIFT_L_Q: {
      arithmeticOp_Q(<<);
      break;
    }
    case OpCodes::SHIFT_R_B: {
      arithmeticOp_B(>>);
      break;
    }
    case OpCodes::SHIFT_R_W: {
      arithmeticOp_W(>>);
      break;
    }
    case OpCodes::SHIFT_R_D: {
      arithmeticOp_D(>>);
      break;
    }
    case OpCodes::SHIFT_R_Q: {
      arithmeticOp_Q(>>);
      break;
    }
    case OpCodes::F_ADD: {
      arithmeticOp_F(+);
      break;
    }
    case OpCodes::F_SUB: {
      arithmeticOp_F(-);
      break;
    }
    case OpCodes::F_MUL: {
      arithmeticOp_F(*);
      break;
    }
    case OpCodes::F_DIV: {
      arithmeticOp_F(/);
      break;
    }
    default: {
      std::cerr << "Runtime Error: Invalid OpCode [" << (uint32_t)op << "]\n";
      exit(1);
    }
  }
}
