#pragma once

#define NUM_REGISTERS 32
#define instructionPointerIndex NUM_REGISTERS - 1
#define stackPointerIndex NUM_REGISTERS - 2
#define basePointerIndex NUM_REGISTERS - 3
#define dataPointerIndex NUM_REGISTERS - 4
#define miscIndex 0

/*
arguments are passed on the stack. before calling a function, space for the return value needs to be made
initial position = sp
sp += size of return value
space for return value will be between initial position and new sp (sp will point to start of data since stack grows down)
each argument will be placed one after the other (padding if necessary) starting with the first argument, ex:

function with args (char, int32) and returns int32
ADD sp 4  // for 32 bit return value
PUSH_B arg1  // first argument of size 8 bit (adds 1 to the sp)
ADD sp 3  // padding for 32 bit value
PUSH_D arg2  // second argument of size 32 bit (adds 4 to the sp)

arguments can then be accessed within the function via:
argument 1: sp + 7 (4 + 3)
argument 2: sp

and return value via: sp + 8

the return value can be accessed by the caller from sp
*/
enum class BuiltInFunctions: unsigned char {
  ALLOCATE, // ALLOCATE arg1 | allocate arg1 bytes and return address
  REALLOCATE, // REALLOCATE arg1, arg2 | reallocate memory pointed at by arg1 with new size arg2, return new address
  DEALLOCATE, // DEALLOCATE arg1 | deallocate memory at arg1
  PRINT_STRING, // PRINT_STRING arg1, arg2 | prints the string (null terminated) pointed at by arg2 to FILE* arg1
  PRINT_CHAR, // PRINT_STRING arg1, arg2 | prints the lowest byte in arg2 as a character to FILE* arg1
  PRINT_SIGNED, // PRINT_STRING arg1, arg2 | prints arg2 as a signed integer to FILE* arg1
  PRINT_UNSIGNED, // PRINT_STRING arg1, arg2 | prints arg2 as an unsigned integer to FILE* arg1
  PRINT_HEX, // PRINT_STRING arg1, arg2 | prints arg2 in hex to FILE* arg1
  GET_CHAR, // PRINT_STRING arg1 | returns one character from file descriptor arg1
  FFLUSH, // PRINT_STRING arg1 | flush buffer FILE* arg1
  MEM_COPY, // MEM_COPY arg1, arg2, arg3 | copy arg3 bytes from arg1 to arg2
};

enum class OpCodes: unsigned char {
  NOP, // do nothing
  EXIT, // EXIT src | exits the program with the code in src
  CALL_B, // CALL_B BuiltInFunctions | call a built in function such as print, etc.

  // CMP src1, src2 | sets flags based on the result of (signed) src1 - (signed) src2
  CMP, // CMP src1, src2

  SET_Z, // SET src
  SET_P, // SET src
  GET_E, // GET_E dest | set dest to 1 if z is true
  GET_NE, // GET_NE dest | set dest to 1 if z is false
  GET_G, // GET_G dest | set dest to 1 if p is true
  GET_GE, // GET_GE dest | set dest to 1 if z or p are true
  GET_L, // GET_L dest | set dest to 1 if z and p are false
  GET_LE, // GET_LE dest | set dest to 1 if z is true or p is false


  // LOAD dest, src | loads data from memory at the address src and places it in dest
  LOAD_B, // LOAD dest, src
  LOAD_W, // LOAD dest, src
  LOAD_D, // LOAD dest, src
  LOAD_Q, // LOAD dest, src

  // STORE dest, src| stores data from src into memory at the address dest
  STORE_B, // STORE dest, src
  STORE_W, // STORE dest, src
  STORE_D, // STORE dest, src
  STORE_Q, // STORE dest, src

  // jumps to an instruction, "imm" being the 8 byte immediate address to jump to
  JUMP, // JUMP 8 byte imm
  JUMP_E, // jump if equal (zero) | JUMP_E 8 byte imm
  JUMP_NE, // jump if not equal (not zero) | JUMP_NE 8 byte imm
  JUMP_G, // jump if greater (positive) | JUMP_G 8 byte imm
  JUMP_GE, // jump if greater or equal (zero or positive) | JUMP_GE 8 byte imm
  JUMP_L, // jump if less (not zero and not positive) | JUMP_L 8 byte imm
  JUMP_LE, // jump if not less (zero or not positive) | JUMP_LE 8 byte imm

  // relative jump by 1 byte signed 
  RB_JUMP, // RB_JUMP 1 byte imm
  RB_JUMP_E, // jump if equal (zero) | RB_JUMP_E 1 byte imm
  RB_JUMP_NE, // jump if not equal (not zero) | RB_JUMP_NE 1 byte imm
  RB_JUMP_G, // jump if greater (positive) | RB_JUMP_G 1 byte imm
  RB_JUMP_GE, // jump if greater or equal (zero or positive) | RB_JUMP_GE 1 byte imm
  RB_JUMP_L, // jump if less (not zero and not positive) | RB_JUMP_L 1 byte imm
  RB_JUMP_LE, // jump if not less (zero or not positive) | RB_JUMP_LE 1 byte imm

  // move data between registers
  MOVE, // MOVE dest, src
  MOVE_I, // MOVE dest, 4 byte imm

  // move data to/from the stack
  PUSH_B, // PUSH_B src
  PUSH_W, // PUSH_W src
  PUSH_D, // PUSH_D src
  PUSH_Q, // PUSH_Q src
  POP_B, // POP_B dest
  POP_W, // POP_W dest
  POP_D, // POP_D dest
  POP_Q, // POP_Q dest

  // arithmetic
  INC,
  DEC,
  NOT,
  ADD, // ADD dest, src
  ADD_I, // ADD_I dest, 4 byte imm
  SUB, // SUB dest, src
  SUB_I, // SUB_I dest, 4 byte imm
  MUL, // MUL dest, src
  MUL_I, // MUL_I dest, 4 byte imm
  DIV, // DIV dest, src
  DIV_I, // DIV_I dest, 4 byte imm
  MOD, // MOD dest, src
  MOD_I, // MOD_I dest, 4 byte imm
  OR, // OR dest, src
  OR_I, // OR_I dest, 4 byte imm
  AND, // AND dest, src
  AND_I, // AND_I dest, 4 byte imm
  XOR, // XOR dest, src
  XOR_I, // XOR_I dest, 4 byte imm
  SHIFT_L, // SHIFT_L dest, src
  SHIFT_L_I, // SHIFT_L_I dest, 4 byte imm
  SHIFT_R, // SHIFT_R dest, src
  SHIFT_R_I, // SHIFT_R_I dest, 4 byte imm

  LOGICAL_OR,
  LOGICAL_AND,

  // floating point arithmetic
  F_ADD,
  F_ADD_I,
  F_SUB,
  F_SUB_I,
  F_MUL,
  F_MUL_I,
  F_DIV,
  F_DIV_I,

  // this marks the end of ops that should be in the executable
  // any ops below are temporary ops that should be changed before code gen process is done
  END_OF_VALID_OPS,
  FUNC_CALL,
};

/*

  EXIT,
  CALL_B,
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
  JUMP_E,
  JUMP_NE,
  JUMP_GT,
  JUMP_LT,
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