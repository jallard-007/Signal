#include <iostream>
#include <cinttypes>
#include <cstring>
#include "interpreter.hpp"

#define uc unsigned char

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define bp registers[basePointerIndex]
#define dp registers[dataPointerIndex]
#define misc registers[miscIndex]

// macros for getting values from the stack
#define UINT64_SP(offset) *(uint64_t *)(sp + offset)
#define INT64_SP(offset) *(int64_t *)(sp + offset)
#define UINT32_SP(offset) *(uint32_t *)(sp + offset)
#define INT32_SP(offset) *(int32_t *)(sp + offset)
#define CHAR_SP(offset) *(char *)(sp + offset)
#define CHAR_P_SP(offset) *(char **)(sp + offset)
#define VOID_P_SP(offset) *(void **)(sp + offset)
#define FILE_P_SP(offset) *(FILE **)(sp + offset)


Interpreter::Interpreter(
  uc const *programInstructions,
  uc *programData,
  uint64_t stackSize
): program{programInstructions} {
  __stack.resize(stackSize);
  stack = __stack.data();
  sp = (uint64_t)stack + stackSize;
  bp = sp;
  dp = (uint64_t)programData;
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

int Interpreter::runProgram() {
  while (running) {
  OpCodes op = (OpCodes)program[ip++];
  #define MY_DEBUG
  #ifdef MY_DEBUG
  uint32_t arg1 = program[ip];
  (void)arg1;
  uint32_t arg2 = program[ip+1];
  (void)arg2;
  uint64_t *curr_stack = (uint64_t *)sp;
  (void)curr_stack;
  #endif
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
      switch ((BuiltInFunctions)program[ip++]) {
        // memory
        case BuiltInFunctions::ALLOCATE: {
          // 8 bytes return value | 8 bytes size
          UINT64_SP(8) = (uint64_t)malloc(UINT64_SP(0));
          sp += 8;
          break;
        }
        case BuiltInFunctions::REALLOCATE: {
          // 8 bytes return value | 8 bytes pointer | 8 bytes size
          UINT64_SP(16) = (uint64_t)realloc((void *)UINT64_SP(8), *(uint64_t *)UINT64_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunctions::DEALLOCATE: {
          // 8 bytes pointer
          free((void *)UINT64_SP(0));
          sp += 8;
          break;
        }

        // general
        case BuiltInFunctions::MEM_COPY: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          UINT64_SP(24) = (uint64_t)std::memcpy((void *)UINT64_SP(16), (void *)UINT64_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunctions::MEM_MOVE: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          UINT64_SP(24) = (uint64_t)std::memmove((void *)UINT64_SP(16), (void *)UINT64_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunctions::MEM_COMPARE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          INT32_SP(28) = std::memcmp(VOID_P_SP(16), VOID_P_SP(8), UINT64_SP(0));
          sp += 28;
          break;
        }
  
        // strings
        case BuiltInFunctions::STR_LENGTH: {
          // 8 bytes return value | 8 bytes string pointer
          UINT64_SP(8) = strlen(CHAR_P_SP(0));
          sp += 8;
          break;
        }
        case BuiltInFunctions::STR_COMPARE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes str1 | 8 bytes str2
          INT32_SP(20) = strcmp(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 20;
          // res is 0 if equal, >0 if str1 is greater, <0 if str2 is greater
          break;
        }
        case BuiltInFunctions::STR_N_COMPARE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes str1 | 8 bytes str2 | 8 bytes n
          INT32_SP(28) = strncmp(CHAR_P_SP(16), CHAR_P_SP(8), UINT64_SP(0));
          sp += 28;
          break;
        }
        case BuiltInFunctions::STR_COPY: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2
          CHAR_P_SP(16) = strcpy(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunctions::STR_N_COPY: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          CHAR_P_SP(24) = strncpy(CHAR_P_SP(16), CHAR_P_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunctions::STR_CAT: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2
          CHAR_P_SP(16) = strcat(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunctions::STR_N_CAT: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          CHAR_P_SP(24) = strncat(CHAR_P_SP(16), CHAR_P_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }

        // printing
        case BuiltInFunctions::PRINT_STRING: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 bytes str
          INT32_SP(20) = fputs(CHAR_P_SP(0), FILE_P_SP(8));
          sp += 20;
          break;
        }
        case BuiltInFunctions::PRINT_CHAR: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 1 byte char
          INT32_SP(13) = fputc(CHAR_SP(0), FILE_P_SP(1));
          sp += 13;
          break;
        }
        case BuiltInFunctions::PRINT_SIGNED: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 byte num
          INT32_SP(20) = fprintf(FILE_P_SP(8), "%" PRId64, (int64_t)UINT64_SP(0));
          sp += 20;
          break;
        }
        case BuiltInFunctions::PRINT_UNSIGNED: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 byte num
          INT32_SP(20) = fprintf(FILE_P_SP(8), "%" PRIu64, UINT64_SP(0));
          sp += 20;
          break;
        }
        case BuiltInFunctions::PRINT_HEX: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 byte num
          INT32_SP(20) = fprintf(FILE_P_SP(0), "0x%08" PRIx64, UINT64_SP(8));
          sp += 20;
          break;
        }
        case BuiltInFunctions::FFLUSH: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer
          INT32_SP(12) = fflush(FILE_P_SP(0));
          sp += 12;
          break;
        }
        
        // files
        case BuiltInFunctions::OPEN: {
          // 8 bytes return value | 8 bytes str | 8 bytes str
          // modes =
          //   "r" read
          //   "w" write
          //   "a" append
          //   "r+" read and write
          //   "w+" create, read and write
          //   "a+" create, read and append
          FILE_P_SP(16) = fopen(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunctions::CLOSE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer
          INT32_SP(12) = fclose(FILE_P_SP(0));
          sp += 12;
          break;
        }
        case BuiltInFunctions::READ: {
          // 8 bytes return value | 8 bytes buffer | 8 bytes n | 8 bytes file pointer
          UINT64_SP(24) = fread(VOID_P_SP(16), 1, UINT64_SP(8), FILE_P_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunctions::READ_LINE: {
          // 8 bytes return value | 8 bytes buffer | 4 bytes n | 4 bytes padding | 8 bytes file pointer
          CHAR_P_SP(24) = fgets(CHAR_P_SP(16), INT32_SP(12), FILE_P_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunctions::READ_CHAR: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer
          INT32_SP(12) = getc(FILE_P_SP(0));
          sp += 12;
          break;
        }
        case BuiltInFunctions::WRITE: {
          // 8 bytes return value | 8 bytes data | 8 bytes n | 8 bytes file pointer
          UINT64_SP(24) = fwrite(VOID_P_SP(16), 1, UINT64_SP(12), FILE_P_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunctions::SEEK: {
          // whenceOptions =
          //   SEEK_SET start of file
          //   SEEK_CUR curr position
          //   SEEK_END end of file
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 bytes offset | 4 bytes whence
          INT32_SP(24) = fseek(FILE_P_SP(12), INT64_SP(4), INT32_SP(0));
          sp += 24;
          break;
        }
        default: {
          std::cerr << "Runtime Error: Invalid BuiltInFunction, Code [" << (uint32_t)program[ip - 1] << "]\n";
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
    case OpCodes::SET_FLAGS: {
      int64_t res = (int64_t)registers[program[ip]];
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
      ip = registers[program[ip + 1]];
      break;
    }
    case OpCodes::JUMP_E: {
      if (z) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::JUMP_NE: {
      if (!z) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::JUMP_G: {
      if (p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::JUMP_GE: {
      if (z || p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::JUMP_L: {
      if (!z && !p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::JUMP_LE: {
      if (z || !p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::R_JUMP: {
      ip += *(int16_t *)(program + ip) - 1;
      break;
    }
    case OpCodes::R_JUMP_E: {
      if (z) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::R_JUMP_NE: {
      if (!z) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::R_JUMP_G: {
      if (p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::R_JUMP_GE: {
      if (z || p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::R_JUMP_L: {
      if (!z && !p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::R_JUMP_LE: {
      if (z || !p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCodes::RS_JUMP: {
      ip += (int8_t)program[ip] - 1;
      break;
    }
    case OpCodes::RS_JUMP_E: {
      if (z) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RS_JUMP_NE: {
      if (!z) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RS_JUMP_G: {
      if (p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RS_JUMP_GE: {
      if (z || p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RS_JUMP_L: {
      if (!z && !p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCodes::RS_JUMP_LE: {
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
    case OpCodes::MOVE_LI: {
      registers[program[ip]] = *(uint64_t *)(program + ip + 1);
      ip += 9;
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
  return exitCode;
}
