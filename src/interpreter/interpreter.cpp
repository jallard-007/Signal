#include <iostream>
#include <cinttypes>
#include <cstring>
#include "interpreter.hpp"

#define bc bytecode_t

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
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
  bc const *programInstructions,
  unsigned char *programData,
  uint64_t stackSize
): program{programInstructions} {
  __stack.resize(stackSize);
  stack = __stack.data();
  sp = (uint64_t)stack + stackSize;
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
  OpCode op = (OpCode)program[ip++];
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
    case OpCode::NOP: {
      break;
    }
    case OpCode::EXIT: {
      exitCode = registers[program[ip]];
      running = false;
      break;
    }
    case OpCode::CALL_B: {
      switch ((BuiltInFunction)program[ip++]) {
        // memory
        case BuiltInFunction::ALLOCATE: {
          // 8 bytes return value | 8 bytes size
          UINT64_SP(8) = (uint64_t)malloc(UINT64_SP(0));
          sp += 8;
          break;
        }
        case BuiltInFunction::REALLOCATE: {
          // 8 bytes return value | 8 bytes pointer | 8 bytes size
          UINT64_SP(16) = (uint64_t)realloc((void *)UINT64_SP(8), *(uint64_t *)UINT64_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunction::DEALLOCATE: {
          // 8 bytes pointer
          free((void *)UINT64_SP(0));
          sp += 8;
          break;
        }

        // general
        case BuiltInFunction::MEM_COPY: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          UINT64_SP(24) = (uint64_t)std::memcpy((void *)UINT64_SP(16), (void *)UINT64_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunction::MEM_MOVE: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          UINT64_SP(24) = (uint64_t)std::memmove((void *)UINT64_SP(16), (void *)UINT64_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunction::MEM_COMPARE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          INT32_SP(28) = std::memcmp(VOID_P_SP(16), VOID_P_SP(8), UINT64_SP(0));
          sp += 28;
          break;
        }
  
        // strings
        case BuiltInFunction::STR_LENGTH: {
          // 8 bytes return value | 8 bytes string pointer
          UINT64_SP(8) = strlen(CHAR_P_SP(0));
          sp += 8;
          break;
        }
        case BuiltInFunction::STR_COMPARE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes str1 | 8 bytes str2
          INT32_SP(20) = strcmp(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 20;
          // res is 0 if equal, >0 if str1 is greater, <0 if str2 is greater
          break;
        }
        case BuiltInFunction::STR_N_COMPARE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes str1 | 8 bytes str2 | 8 bytes n
          INT32_SP(28) = strncmp(CHAR_P_SP(16), CHAR_P_SP(8), UINT64_SP(0));
          sp += 28;
          break;
        }
        case BuiltInFunction::STR_COPY: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2
          CHAR_P_SP(16) = strcpy(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunction::STR_N_COPY: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          CHAR_P_SP(24) = strncpy(CHAR_P_SP(16), CHAR_P_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunction::STR_CAT: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2
          CHAR_P_SP(16) = strcat(CHAR_P_SP(8), CHAR_P_SP(0));
          sp += 16;
          break;
        }
        case BuiltInFunction::STR_N_CAT: {
          // 8 bytes return value | 8 bytes str1 | 8 bytes str2 | 8 bytes size
          CHAR_P_SP(24) = strncat(CHAR_P_SP(16), CHAR_P_SP(8), UINT64_SP(0));
          sp += 24;
          break;
        }

        // printing
        case BuiltInFunction::PRINT_STRING: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 bytes str
          INT32_SP(20) = fputs(CHAR_P_SP(0), FILE_P_SP(8));
          sp += 20;
          break;
        }
        case BuiltInFunction::PRINT_CHAR: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 1 byte char
          INT32_SP(13) = fputc(CHAR_SP(0), FILE_P_SP(1));
          sp += 13;
          break;
        }
        case BuiltInFunction::PRINT_SIGNED: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 byte num
          INT32_SP(20) = fprintf(FILE_P_SP(8), "%" PRId64, (int64_t)UINT64_SP(0));
          sp += 20;
          break;
        }
        case BuiltInFunction::PRINT_UNSIGNED: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 byte num
          INT32_SP(20) = fprintf(FILE_P_SP(8), "%" PRIu64, UINT64_SP(0));
          sp += 20;
          break;
        }
        case BuiltInFunction::PRINT_HEX: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer | 8 byte num
          INT32_SP(20) = fprintf(FILE_P_SP(0), "0x%08" PRIx64, UINT64_SP(8));
          sp += 20;
          break;
        }
        case BuiltInFunction::FFLUSH: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer
          INT32_SP(12) = fflush(FILE_P_SP(0));
          sp += 12;
          break;
        }
        
        // files
        case BuiltInFunction::OPEN: {
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
        case BuiltInFunction::CLOSE: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer
          INT32_SP(12) = fclose(FILE_P_SP(0));
          sp += 12;
          break;
        }
        case BuiltInFunction::READ: {
          // 8 bytes return value | 8 bytes buffer | 8 bytes n | 8 bytes file pointer
          UINT64_SP(24) = fread(VOID_P_SP(16), 1, UINT64_SP(8), FILE_P_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunction::READ_LINE: {
          // 8 bytes return value | 8 bytes buffer | 4 bytes n | 4 bytes padding | 8 bytes file pointer
          CHAR_P_SP(24) = fgets(CHAR_P_SP(16), INT32_SP(12), FILE_P_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunction::READ_CHAR: {
          // 4 bytes return value | 4 bytes padding | 8 bytes file pointer
          INT32_SP(12) = getc(FILE_P_SP(0));
          sp += 12;
          break;
        }
        case BuiltInFunction::WRITE: {
          // 8 bytes return value | 8 bytes data | 8 bytes n | 8 bytes file pointer
          UINT64_SP(24) = fwrite(VOID_P_SP(16), 1, UINT64_SP(12), FILE_P_SP(0));
          sp += 24;
          break;
        }
        case BuiltInFunction::SEEK: {
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
    case OpCode::CMP: {
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
    case OpCode::SET_FLAGS: {
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
    case OpCode::GET_E: {
      registers[program[ip++]] = z;
      break;
    }
    case OpCode::GET_NE: {
      registers[program[ip++]] = !z;
      break;
    }
    case OpCode::GET_G: {
      registers[program[ip++]] = p;
      break;
    }
    case OpCode::GET_GE: {
      registers[program[ip++]] = z || p;
      break;
    }
    case OpCode::GET_L: {
      registers[program[ip++]] = !z && !p;
      break;
    }
    case OpCode::GET_LE: {
      registers[program[ip++]] = z || !p;
      break;
    }
    case OpCode::LOAD_B: {
      registers[program[ip]] = *(uint8_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::LOAD_W: {
      registers[program[ip]] = *(uint16_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::LOAD_D: {
      registers[program[ip]] = *(uint32_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::LOAD_Q: {
      registers[program[ip]] = *(uint64_t*)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::STORE_B: {
      *(uint8_t*)registers[program[ip]] = (uint8_t)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::STORE_W: {
      *(uint16_t*)registers[program[ip]] = (uint16_t)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::STORE_D: {
      *(uint32_t*)registers[program[ip]] = (uint32_t)registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::STORE_Q: {
      *(uint64_t*)registers[program[ip]] = registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::JUMP: {
      ip = registers[program[ip + 1]];
      break;
    }
    case OpCode::JUMP_E: {
      if (z) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::JUMP_NE: {
      if (!z) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::JUMP_G: {
      if (p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::JUMP_GE: {
      if (z || p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::JUMP_L: {
      if (!z && !p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::JUMP_LE: {
      if (z || !p) {
        ip = registers[program[ip + 1]];
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::R_JUMP: {
      ip += *(int16_t *)(program + ip) - 1;
      break;
    }
    case OpCode::R_JUMP_E: {
      if (z) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::R_JUMP_NE: {
      if (!z) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::R_JUMP_G: {
      if (p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::R_JUMP_GE: {
      if (z || p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::R_JUMP_L: {
      if (!z && !p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::R_JUMP_LE: {
      if (z || !p) {
        ip += *(int16_t *)(program + ip) - 1;
      } else {
        ip += 2;
      }
      break;
    }
    case OpCode::RS_JUMP: {
      ip += (int8_t)program[ip] - 1;
      break;
    }
    case OpCode::RS_JUMP_E: {
      if (z) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCode::RS_JUMP_NE: {
      if (!z) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCode::RS_JUMP_G: {
      if (p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCode::RS_JUMP_GE: {
      if (z || p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCode::RS_JUMP_L: {
      if (!z && !p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCode::RS_JUMP_LE: {
      if (z || !p) {
        ip += (int8_t)program[ip] - 1;
      } else {
        ++ip;
      }
      break;
    }
    case OpCode::MOVE: {
      registers[program[ip]] = registers[program[ip+1]];
      ip += 2;
      break;
    }
    case OpCode::MOVE_SI: {
      registers[program[ip]] = *(uint8_t *)(program + ip + 1);
      ip += 2;
      break;
    }
    case OpCode::MOVE_I: {
      registers[program[ip]] = *(uint32_t *)(program + ip + 1);
      ip += 5;
      break;
    }
    case OpCode::MOVE_LI: {
      registers[program[ip]] = *(uint64_t *)(program + ip + 1);
      ip += 9;
      break;
    }
    case OpCode::PUSH_B: {
      --sp;
      *(uint8_t *)sp = (uint8_t)registers[program[ip++]];
      break;
    }
    case OpCode::PUSH_W: {
      sp -= 2;
      *(uint16_t *)sp = (uint16_t)registers[program[ip++]];
      break;
    }
    case OpCode::PUSH_D: {
      sp -= 4;
      *(uint32_t *)sp = (uint32_t)registers[program[ip++]];
      break;
    }
    case OpCode::PUSH_Q: {
      sp -= 8;
      *(uint64_t *)sp = registers[program[ip++]];
      break;
    }
    case OpCode::POP_B: {
      registers[program[ip++]] = *(uint8_t *)sp;
      ++sp;
      break;
    }
    case OpCode::POP_W: {
      registers[program[ip++]] = *(uint16_t *)sp;
      sp += 2;
      break;
    }
    case OpCode::POP_D: {
      registers[program[ip++]] = *(uint32_t *)sp;
      sp += 4;
      break;
    }
    case OpCode::POP_Q: {
      registers[program[ip++]] = *(uint64_t *)sp;
      sp += 8;
      break;
    }
    case OpCode::INC: {
      ++registers[program[ip]];
      ++ip;
      break;
    }
    case OpCode::DEC: {
      --registers[program[ip]];
      ++ip;
      break;
    }
    case OpCode::NOT: {
      registers[program[ip]] = !registers[program[ip]];
      ++ip;
      break;
    }
    case OpCode::ADD: {
      arithmeticOp(+=);
      break;
    }
    case OpCode::ADD_I: {
      arithmeticOp_I(+=);
      break;
    }
    case OpCode::SUB: {
      arithmeticOp(-=);
      break;
    }
    case OpCode::SUB_I: {
      arithmeticOp_I(-=);
      break;
    }
    case OpCode::MUL: {
      arithmeticOp(*=);
      break;
    }
    case OpCode::MUL_I: {
      arithmeticOp_I(*=);
      break;
    }
    case OpCode::DIV: {
      arithmeticOp(/=);
      break;
    }
    case OpCode::DIV_I: {
      arithmeticOp_I(/=);
      break;
    }
    case OpCode::MOD: {
      arithmeticOp(%=);
      break;
    }
    case OpCode::MOD_I: {
      arithmeticOp_I(%=);
      break;
    }
    case OpCode::OR: {
      arithmeticOp(|=);
      break;
    }
    case OpCode::OR_I: {
      arithmeticOp_I(|=);
      break;
    }
    case OpCode::AND: {
      arithmeticOp(&=);
      break;
    }
    case OpCode::AND_I: {
      arithmeticOp_I(&=);
      break;
    }
    case OpCode::XOR: {
      arithmeticOp(^=);
      break;
    }
    case OpCode::XOR_I: {
      arithmeticOp_I(^=);
      break;
    }
    case OpCode::SHIFT_L: {
      arithmeticOp(<<=);
      break;
    }
    case OpCode::SHIFT_L_I: {
      arithmeticOp_I(<<=);
      break;
    }
    case OpCode::SHIFT_R: {
      arithmeticOp(>>=);
      break;
    }
    case OpCode::SHIFT_R_I: {
      arithmeticOp_I(>>=);
      break;
    }
    case OpCode::LOGICAL_OR: {
      z = !(registers[program[ip]] || registers[program[ip+1]]);
      ip += 2;
      break;
    }
    case OpCode::LOGICAL_AND: {
      z = !(registers[program[ip]] && registers[program[ip+1]]);
      ip += 2;
      break;
    }
    case OpCode::F_ADD: {
      arithmeticOp_F(+);
      break;
    }
    case OpCode::F_ADD_I: {
      arithmeticOp_F_I(+);
      break;
    }
    case OpCode::F_SUB: {
      arithmeticOp_F(-);
      break;
    }
    case OpCode::F_SUB_I: {
      arithmeticOp_F_I(-);
      break;
    }
    case OpCode::F_MUL: {
      arithmeticOp_F(*);
      break;
    }
    case OpCode::F_MUL_I: {
      arithmeticOp_F_I(*);
      break;
    }
    case OpCode::F_DIV: {
      arithmeticOp_F(/);
      break;
    }
    case OpCode::F_DIV_I: {
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
