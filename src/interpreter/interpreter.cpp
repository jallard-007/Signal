#include <iostream>
#include "interpreter.hpp"

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define bp registers[basePointerIndex]
#define dp registers[dataPointerIndex]
#define misc registers[miscIndex]

Interpreter::Interpreter(
  unsigned char const *programInstructions,
  unsigned char *programData,
  uint64_t stackSize
): program{programInstructions} {
  stack = new unsigned char [stackSize];
  sp = (uint64_t)stack + stackSize;
  bp = sp;
  dp = (uint64_t)programData;
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

#define arithmeticOp(op) \
  registers[program[ip]] = registers[program[ip+1]] op registers[program[ip+2]]; \
  ip += 3

#define arithmeticOp_I(op) \
  registers[program[ip]] = registers[program[ip+1]] op *(uint16_t *)(program+ip+2); \
  ip += 4

#define arithmeticOp_F(op) \
  registers[program[ip]] = *(double *)registers+program[ip+1] op *(double *)registers+program[ip+2]; \
  ip += 3

#define arithmeticOp_F_I(op) \
  registers[program[ip]] = *(double *)registers+program[ip+1] op *(double *)(program+ip+2); \
  ip += 10

void Interpreter::executeNextInstruction() {
  OpCodes op = (OpCodes)program[ip++];
  switch (op) {
    case OpCodes::NOP: {
      break;
    }
    case OpCodes::EXIT: {
      exitCode = registers[0];
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
    case OpCodes::MOVE_I: {
      registers[program[ip]] = *(uint16_t *)(program + ip + 1);
      ip += 3;
      break;
    }
    case OpCodes::PUSH_B: {
      --sp;
      *(uint8_t *)sp = registers[program[ip++]];
      break;
    }
    case OpCodes::PUSH_W: {
      sp -= 2;
      *(uint16_t *)sp = registers[program[ip++]];
      break;
    }
    case OpCodes::PUSH_D: {
      sp -= 4;
      *(uint32_t *)sp = registers[program[ip++]];
      break;
    }
    case OpCodes::PUSH_Q: {
      sp -= 8;
      *(uint64_t *)sp = registers[program[ip++]];
      break;
    }
    case OpCodes::POP_B: {
      registers[program[ip++]] = *(uint8_t *)sp;
      ++sp;
      break;
    }
    case OpCodes::POP_W: {
      registers[program[ip++]] = *(uint16_t *)sp;
      sp += 2;
      break;
    }
    case OpCodes::POP_D: {
      registers[program[ip++]] = *(uint32_t *)sp;
      sp += 4;
      break;
    }
    case OpCodes::POP_Q: {
      registers[program[ip++]] = *(uint64_t *)sp;
      sp += 8;
      break;
    }
    case OpCodes::ADD: {
      arithmeticOp(+);
      break;
    }
    case OpCodes::ADD_I: {
      arithmeticOp_I(+);
      break;
    }
    case OpCodes::SUB: {
      arithmeticOp(-);
      break;
    }
    case OpCodes::SUB_I: {
      arithmeticOp_I(-);
      break;
    }
    case OpCodes::MUL: {
      arithmeticOp(*);
      break;
    }
    case OpCodes::MUL_I: {
      arithmeticOp_I(*);
      break;
    }
    case OpCodes::DIV: {
      arithmeticOp(/);
      break;
    }
    case OpCodes::DIV_I: {
      arithmeticOp_I(/);
      break;
    }
    case OpCodes::MOD: {
      arithmeticOp(%);
      break;
    }
    case OpCodes::MOD_I: {
      arithmeticOp_I(%);
      break;
    }
    case OpCodes::OR: {
      arithmeticOp(|);
      break;
    }
    case OpCodes::OR_I: {
      arithmeticOp_I(|);
      break;
    }
    case OpCodes::AND: {
      arithmeticOp(&);
      break;
    }
    case OpCodes::AND_I: {
      arithmeticOp_I(&);
      break;
    }
    case OpCodes::XOR: {
      arithmeticOp(^);
      break;
    }
    case OpCodes::XOR_I: {
      arithmeticOp_I(^);
      break;
    }
    case OpCodes::SHIFT_L: {
      arithmeticOp(<<);
      break;
    }
    case OpCodes::SHIFT_L_I: {
      arithmeticOp_I(<<);
      break;
    }
    case OpCodes::SHIFT_R: {
      arithmeticOp(>>);
      break;
    }
    case OpCodes::SHIFT_R_I: {
      arithmeticOp_I(>>);
      break;
    }
    case OpCodes::F_ADD: {
      arithmeticOp_F(+);
      break;
    }
    case OpCodes::F_ADD_I: {
      arithmeticOp_F_I(+);
      break;
    }
    case OpCodes::F_SUB: {
      arithmeticOp_F(-);
      break;
    }
    case OpCodes::F_SUB_I: {
      arithmeticOp_F_I(-);
      break;
    }
    case OpCodes::F_MUL: {
      arithmeticOp_F(*);
      break;
    }
    case OpCodes::F_MUL_I: {
      arithmeticOp_F_I(*);
      break;
    }
    case OpCodes::F_DIV: {
      arithmeticOp_F(/);
      break;
    }
    case OpCodes::F_DIV_I: {
      arithmeticOp_F_I(/);
      break;
    }
    default: {
      std::cerr << "Runtime Error: Invalid OpCode [" << (uint32_t)op << "]\n";
      exit(1);
    }
  }
}
