#pragma once

#define NUM_REGISTERS 32
#define instructionPointerIndex NUM_REGISTERS - 1
#define stackPointerIndex NUM_REGISTERS - 2
#define basePointerIndex NUM_REGISTERS - 3
#define dataPointerIndex NUM_REGISTERS - 4
#define miscIndex NUM_REGISTERS - 5

// arguments are passed through registers r11 - r1x, return value passed through register r10
enum class BuiltInFunctions: unsigned char {
  ALLOCATE, // ALLOCATE r11 | allocate r11 bytes and place address in r10
  REALLOCATE, // REALLOCATE r11, r12 | reallocate memory at r11 with new size r12, place new address in r10
  DEALLOCATE, // DEALLOCATE r11 | deallocate memory
  PRINT_STRING, // prints the string (null terminated) pointed at by r11
  PRINT_CHAR, // prints the lowest byte in r11 as a character
  PRINT_SIGNED, // prints r11 as a signed integer
  PRINT_UNSIGNED, // prints r11 as an unsigned integer
  PRINT_HEX, // prints r11 in hex
};

enum class OpCodes: unsigned char {
  NOP, // do nothing
  EXIT, // EXIT src | exits the program with the code in src
  CALL, // CALL BuiltInFunctions | call a built in function such as print, etc.

  // CMP src1, src2 | sets flags based on the result of (signed) src1 - (signed) src2
  CMP,

  // SET_ src | set register to the value of the flag
  SET_Z,
  SET_P,

  // LOAD dest, src | loads data from memory at the address src and places it in dest
  LOAD_B,
  LOAD_W,
  LOAD_D,
  LOAD_Q,

  // STORE dest, src| stores data from src into memory at the address dest
  STORE_B,
  STORE_W,
  STORE_D,
  STORE_Q,

  // jumps to an instruction, "src" being the address to jump to
  JUMP, // JUMP src
  JUMP_Z, // jump if zero | JUMP_Z src
  JUMP_NZ, // jump if not zero | JUMP_NZ src
  JUMP_P, // jump if positive | JUMP_P src
  JUMP_N, // jump if negative | JUMP_N src

  // move data between registers
  MOVE, // MOVE dest, src
  MOVE_I, // MOVE dest, 4 byte immediate

  // move data to/from the stack
  PUSH_B,
  PUSH_W,
  PUSH_D,
  PUSH_Q,
  POP_B,
  POP_W,
  POP_D,
  POP_Q,

  // arithmetic
  ADD,
  ADD_I,
  SUB,
  SUB_I,
  MUL,
  MUL_I,
  DIV,
  DIV_I,
  MOD,
  MOD_I,
  OR,
  OR_I,
  AND,
  AND_I,
  XOR,
  XOR_I,
  SHIFT_L,
  SHIFT_L_I,
  SHIFT_R,
  SHIFT_R_I,

  // floating point arithmetic
  F_ADD,
  F_ADD_I,
  F_SUB,
  F_SUB_I,
  F_MUL,
  F_MUL_I,
  F_DIV,
  F_DIV_I,
};

/*

  EXIT,
  CALL,
  CMP,
  SET_Z,
  SET_P,
  LOAD_B,
  LOAD_W,
  LOAD_D,
  LOAD_Q,
  STORE_B,
  STORE_W,
  STORE_D,
  STORE_Q,
  JUMP,
  JUMP_Z,
  JUMP_NZ,
  JUMP_P,
  JUMP_N,
  MOVE,
  MOVE_I,
  PUSH_B,
  PUSH_W,
  PUSH_D,
  PUSH_Q,
  POP_B,
  POP_W,
  POP_D,
  POP_Q,
  ADD,
  ADD_I,
  SUB,
  SUB_I,
  MUL,
  MUL_I,
  DIV,
  DIV_I,
  MOD,
  MOD_I,
  OR,
  OR_I,
  AND,
  AND_I,
  XOR,
  XOR_I,
  SHIFT_L,
  SHIFT_L_I,
  SHIFT_R,
  SHIFT_R_I,
  F_ADD,
  F_ADD_I,
  F_SUB,
  F_SUB_I,
  F_MUL,
  F_MUL_I,
  F_DIV,
  F_DIV_I,

*/