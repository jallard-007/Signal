#include <iostream>
#include <cinttypes>
#include <cstring>
#include "interpreter.hpp"

#define uc unsigned char
#define u64 uint64_t

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define bp registers[basePointerIndex]
#define dp registers[dataPointerIndex]
#define misc registers[miscIndex]

#define valAtSP(offset) *(u64 *)(sp + offset)

Interpreter::Interpreter(
  uc const *programInstructions,
  uc *programData,
  uint64_t stackSize
): program{programInstructions} {
  stack = new uc [stackSize];
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
  registers[program[ip]] op registers[program[ip+1]]; \
  ip += 2

#define arithmeticOp_I(op) \
  registers[program[ip]] op *(uint32_t *)(program+ip+1); \
  ip += 5

#define arithmeticOp_F(op) \
  *(double *)(registers+program[ip]) = *(double *)(registers+program[ip+1]) op *(double *)(registers+program[ip+2]); \
  ip += 3

#define arithmeticOp_F_I(op) \
  *(double *)(registers+program[ip]) = *(double *)(registers+program[ip+1]) op *(double *)(program+ip+2); \
  ip += 10

void Interpreter::executeNextInstruction() {
  OpCodes op = (OpCodes)program[ip++];
  switch (op) {
    case OpCodes::NOP: {
      break;
    }
    case OpCodes::EXIT: {
      exitCode = registers[program[ip]];
      running = false;
      break;
    }
    case OpCodes::CALL_B: {
      BuiltInFunctions func = (BuiltInFunctions)program[ip++];
      switch (func) {
        case BuiltInFunctions::ALLOCATE: {
          valAtSP(8) = (uint64_t)malloc(valAtSP(0));
          sp += 8;
          break;
        }
        case BuiltInFunctions::REALLOCATE: {
          valAtSP(16) = (u64)realloc((void *)valAtSP(0), *(u64 *)valAtSP(8));
          sp += 16;
          break;
        }
        case BuiltInFunctions::DEALLOCATE: {
          free((void *)sp);
          sp += 8;
          break;
        }
        case BuiltInFunctions::PRINT_STRING: {
          *(int32_t *)(sp + 20) = fprintf((FILE *)valAtSP(0), "%s", (char *)valAtSP(8));
          sp += 20;
          break;
        }
        case BuiltInFunctions::PRINT_CHAR: {
          *(int32_t *)(sp + 13) = fprintf((FILE *)valAtSP(0), "%c", *(char *)(sp + 9));
          sp += 13;
          break;
        }
        case BuiltInFunctions::PRINT_SIGNED: {
          *(int32_t *)(sp + 20) = fprintf((FILE *)valAtSP(0), "%" PRId64, (int64_t)valAtSP(8));
          sp += 20;
          break;
        }
        case BuiltInFunctions::PRINT_UNSIGNED: {
          *(int32_t *)(sp + 20) = fprintf((FILE *)valAtSP(0), "%" PRIu64, valAtSP(8));
          sp += 20;
          break;
        }
        case BuiltInFunctions::PRINT_HEX: {
          *(int32_t *)(sp + 20) = fprintf((FILE *)valAtSP(0), "0x%08" PRIx64, valAtSP(8));
          sp += 20;
          break;
        }
        case BuiltInFunctions::GET_CHAR: {
          *(int32_t *)(sp + 12) = (uint64_t)getc((FILE *)valAtSP(0));
          sp += 12;
          break;
        }
        case BuiltInFunctions::FFLUSH: {
          *(int32_t *)(sp + 12) = fflush((FILE *)valAtSP(0));
          sp += 12;
          break;
        }
        case BuiltInFunctions::MEM_COPY: {
          std::memcpy((void *)valAtSP(8), (void *)valAtSP(16), valAtSP(0));
          sp += 24;
          break;
        }
        default: {
          std::cerr << "Runtime Error: Invalid BuiltInFunction, Code [" << (uint32_t)func << "]\n";
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
      z = registers[program[ip++]];
      break;
    }
    case OpCodes::SET_P: {
      p = registers[program[ip++]];
      break;
    }
    case OpCodes::GET_E: {
      registers[program[ip++]] = z;
      break;
    }
    case OpCodes::GET_NE: {
      registers[program[ip++]] = !z;
      break;
    }
    case OpCodes::GET_G: {
      registers[program[ip++]] = p;
      break;
    }
    case OpCodes::GET_GE: {
      registers[program[ip++]] = z || p;
      break;
    }
    case OpCodes::GET_L: {
      registers[program[ip++]] = !z && !p;
      break;
    }
    case OpCodes::GET_LE: {
      registers[program[ip++]] = z || !p;
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
      ip = *(uint64_t*)(program + ip);
      break;
    }
    case OpCodes::JUMP_E: {
      if (z) {
        ip = *(uint64_t*)(program + ip);
      } else {
        ip+=8;
      }
      break;
    }
    case OpCodes::JUMP_NE: {
      if (!z) {
        ip = *(uint64_t*)(program + ip);
      } else {
        ip+=8;
      }
      break;
    }
    case OpCodes::JUMP_G: {
      if (p) {
        ip = *(uint64_t*)(program + ip);
      } else {
        ip+=8;
      }
      break;
    }
    case OpCodes::JUMP_GE: {
      if (z || p) {
        ip = *(uint64_t*)(program + ip);
      } else {
        ip+=8;
      }
      break;
    }
    case OpCodes::JUMP_L: {
      if (!z && !p) {
        ip = *(uint64_t*)(program + ip);
      } else {
        ip+=8;
      }
      break;
    }
    case OpCodes::JUMP_LE: {
      if (z || !p) {
        ip = *(uint64_t*)(program + ip);
      } else {
        ip+=8;
      }
      break;
    }
    case OpCodes::RB_JUMP: {
      ip += (int8_t)program[ip] - 1;
      break;
    }
    case OpCodes::RB_JUMP_E: {
      if (z) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RB_JUMP_NE: {
      if (!z) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RB_JUMP_G: {
      if (p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RB_JUMP_GE: {
      if (z || p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RB_JUMP_L: {
      if (!z && !p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RB_JUMP_LE: {
      if (z || !p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::MOVE: {
      registers[program[ip]] = registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCodes::MOVE_I: {
      registers[program[ip]] = *(uint32_t *)(program + ip + 1);
      ip += 5;
      break;
    }
    case OpCodes::PUSH_B: {
      --sp;
      *(uint8_t *)sp = (uint8_t)registers[program[ip++]];
      break;
    }
    case OpCodes::PUSH_W: {
      sp -= 2;
      *(uint16_t *)sp = (uint16_t)registers[program[ip++]];
      break;
    }
    case OpCodes::PUSH_D: {
      sp -= 4;
      *(uint32_t *)sp = (uint32_t)registers[program[ip++]];
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
    case OpCodes::INC: {
      ++registers[program[ip]];
      ++ip;
      break;
    }
    case OpCodes::DEC: {
      --registers[program[ip]];
      ++ip;
      break;
    }
    case OpCodes::NOT: {
      registers[program[ip]] = !registers[program[ip]];
      ++ip;
      break;
    }
    case OpCodes::ADD: {
      arithmeticOp(+=);
      break;
    }
    case OpCodes::ADD_I: {
      arithmeticOp_I(+=);
      break;
    }
    case OpCodes::SUB: {
      arithmeticOp(-=);
      break;
    }
    case OpCodes::SUB_I: {
      arithmeticOp_I(-=);
      break;
    }
    case OpCodes::MUL: {
      arithmeticOp(*=);
      break;
    }
    case OpCodes::MUL_I: {
      arithmeticOp_I(*=);
      break;
    }
    case OpCodes::DIV: {
      arithmeticOp(/=);
      break;
    }
    case OpCodes::DIV_I: {
      arithmeticOp_I(/=);
      break;
    }
    case OpCodes::MOD: {
      arithmeticOp(%=);
      break;
    }
    case OpCodes::MOD_I: {
      arithmeticOp_I(%=);
      break;
    }
    case OpCodes::OR: {
      arithmeticOp(|=);
      break;
    }
    case OpCodes::OR_I: {
      arithmeticOp_I(|=);
      break;
    }
    case OpCodes::AND: {
      arithmeticOp(&=);
      break;
    }
    case OpCodes::AND_I: {
      arithmeticOp_I(&=);
      break;
    }
    case OpCodes::XOR: {
      arithmeticOp(^=);
      break;
    }
    case OpCodes::XOR_I: {
      arithmeticOp_I(^=);
      break;
    }
    case OpCodes::SHIFT_L: {
      arithmeticOp(<<=);
      break;
    }
    case OpCodes::SHIFT_L_I: {
      arithmeticOp_I(<<=);
      break;
    }
    case OpCodes::SHIFT_R: {
      arithmeticOp(>>=);
      break;
    }
    case OpCodes::SHIFT_R_I: {
      arithmeticOp_I(>>=);
      break;
    }
    case OpCodes::LOGICAL_OR: {
      z = !(registers[program[ip]] || registers[program[ip+1]]);
      ip += 2;
      break;
    }
    case OpCodes::LOGICAL_AND: {
      z = !(registers[program[ip]] && registers[program[ip+1]]);
      ip += 2;
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
      std::cerr << "\nRuntime Error: Invalid OpCode [" << (uint32_t)op << "]\n";
      running = false;
      break;
    }
  }
}
