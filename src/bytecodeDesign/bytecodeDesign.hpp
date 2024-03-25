#pragma once

#include <vector>
#include <iostream>

#define SIZE_OF_REGISTER 8
#define NUM_REGISTERS 32
#define instructionPointerIndex NUM_REGISTERS - 1
#define stackPointerIndex NUM_REGISTERS - 2
#define dataPointerIndex NUM_REGISTERS - 3
#define miscRegisterIndex 0 // setting the zero register to a reserved register so that we can easily test if a register has been allocated

typedef unsigned char bytecode_t;

/* BUILT IN FUNCTIONS
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
enum class BuiltInFunction: bytecode_t {
  // memory management
  ALLOCATE, // ALLOCATE arg1 | allocate arg1 bytes and return address
  REALLOCATE, // REALLOCATE arg1, arg2 | reallocate memory pointed at by arg1 with new size arg2, return new address
  DEALLOCATE, // DEALLOCATE arg1 | deallocate memory at arg1

  // general
  MEM_COPY, // MEM_COPY arg1, arg2, arg3 | copy arg3 bytes from arg2 to arg1
  MEM_MOVE, // MEM_MOVE arg1, arg2, arg3 | copy arg3 bytes from arg2 to arg1, handles arg1 and arg2 overlapping
  MEM_COMPARE, // MEM_COMPARE arg1, arg2, arg3 | compare arg3 bytes from arg2 to arg1

  // strings
  STR_LENGTH, // STR_LENGTH arg1 | returns the length of char array array at arg1
  STR_COMPARE, // STR_COMPARE arg1, arg2 | compares arg1 to arg1. returns 0 if same, >0 if str1 is greater <0 if str2 is greater
  STR_N_COMPARE, // STR_N_COMPARE arg1, arg2, arg3 | same as STR_COMPARE, but compares at most arg3 chars
  STR_COPY, // STR_COPY arg1, arg2 | copy arg2 into arg1, returns arg1
  STR_N_COPY, // STR_N_COPY arg1, arg2, arg3 | copy at most arg3 chars from arg2 into arg1, returns arg1
  STR_CAT, // STR_CAT arg1, arg2 | concatenate str2 to str1 
  STR_N_CAT, // STR_CAT arg1, arg2, arg3 | concatenate at most arg3 chars from str2 to str1

  // printing
  PRINT_STRING, // PRINT_STRING arg1, arg2 | prints the string pointed at by arg2 to FILE* arg1
  PRINT_CHAR, // PRINT_CHAR arg1, arg2 | prints arg2 as a character to FILE* arg1
  PRINT_SIGNED, // PRINT_SIGNED arg1, arg2 | prints arg2 as a signed integer to FILE* arg1
  PRINT_UNSIGNED, // PRINT_UNSIGNED arg1, arg2 | prints arg2 as an unsigned integer to FILE* arg1
  PRINT_HEX, // PRINT_HEX arg1, arg2 | prints arg2 in hex to FILE* arg1
  FFLUSH, // FFLUSH arg1 | flush buffer FILE* arg1

  // files
  OPEN, // OPEN arg1, arg2 | open file at path arg1 with mode arg2, returns a file pointer
  CLOSE, // CLOSE arg1 | close file pointer arg1, returns 0 on success
  READ, // READ arg1, arg2, arg3 | read arg2 bytes from file pointer arg3 to buffer arg1
  READ_LINE, // READ_LINE, arg1, arg2, arg3 | read contents from arg3 to buffer arg1 until either a newline or eof is encountered, or arg2 bytes are read
  READ_CHAR, // READ_CHAR arg1 | returns one character from file pointer arg1
  WRITE, // WRITE arg1, arg2, arg3 | write arg2 bytes from arg1 to file pointer arg3, returns number of bytes written
  SEEK, // SEEK arg1, arg2, arg3 | seek to a position with offset arg2 from "type" arg3 in file pointer arg1. 'type' being start of file, curr position, or end of file

  FIRST = ALLOCATE,
  LAST = SEEK
};

enum class OpCode: bytecode_t {
  NOP, // do nothing
  EXIT, // EXIT src | exits the program with the code in src
  CALL_B, // CALL_B BuiltInFunction | call a built in function such as print, etc.

  // CMP src1, src2 | sets flags based on the result of (signed) src1 - (signed) src2
  CMP, // CMP src1, src2

  SET_FLAGS, // SET src
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

  // jumps to an instruction, reg containing the address to jump to
  JUMP, // JUMP reg
  JUMP_E, // jump if equal (zero) | JUMP_E reg
  JUMP_NE, // jump if not equal (not zero) | JUMP_NE reg
  JUMP_G, // jump if greater (positive) | JUMP_G reg
  JUMP_GE, // jump if greater or equal (zero or positive) | JUMP_GE reg
  JUMP_L, // jump if less (not zero and not positive) | JUMP_L reg
  JUMP_LE, // jump if not less (zero or not positive) | JUMP_LE reg

  // relative jumps are relative to the jump instruction
  // relative jump by int16
  R_JUMP, // RB_JUMP 2 byte imm
  R_JUMP_E, // jump if equal (zero) | R_JUMP_E 2 byte imm
  R_JUMP_NE, // jump if not equal (not zero) | R_JUMP_NE 2 byte imm
  R_JUMP_G, // jump if greater (positive) | R_JUMP_G 2 byte imm
  R_JUMP_GE, // jump if greater or equal (zero or positive) | R_JUMP_GE 2 byte imm
  R_JUMP_L, // jump if less (not zero and not positive) | R_JUMP_L 2 byte imm
  R_JUMP_LE, // jump if not less (zero or not positive) | R_JUMP_LE 2 byte imm

  // relative short jump by int8
  RS_JUMP, // RS_JUMP 1 byte imm
  RS_JUMP_E, // jump if equal (zero) | RS_JUMP_E 1 byte imm
  RS_JUMP_NE, // jump if not equal (not zero) | RS_JUMP_NE 1 byte imm
  RS_JUMP_G, // jump if greater (positive) | RS_JUMP_G 1 byte imm
  RS_JUMP_GE, // jump if greater or equal (zero or positive) | RS_JUMP_GE 1 byte imm
  RS_JUMP_L, // jump if less (not zero and not positive) | RS_JUMP_L 1 byte imm
  RS_JUMP_LE, // jump if not less (zero or not positive) | RS_JUMP_LE 1 byte imm

  // move data between registers
  MOVE, // MOVE dest, src
  MOVE_SI, // MOVE dest, 1 byte imm
  MOVE_I, // MOVE dest, 4 byte imm
  MOVE_LI, // MOVE dest, 8 byte imm

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
  NEGATE,
  ADD, // ADD dest, src
  ADD_I, // ADD_I dest, 2 byte imm
  SUB, // SUB dest, src
  SUB_I, // SUB_I dest, 2 byte imm
  MUL, // MUL dest, src
  MUL_I, // MUL_I dest, 2 byte imm
  DIV, // DIV dest, src
  DIV_I, // DIV_I dest, 2 byte imm
  MOD, // MOD dest, src
  MOD_I, // MOD_I dest, 2 byte imm
  OR, // OR dest, src
  OR_I, // OR_I dest, 2 byte imm
  AND, // AND dest, src
  AND_I, // AND_I dest, 2 byte imm
  XOR, // XOR dest, src
  XOR_I, // XOR_I dest, 2 byte imm
  SHIFT_L, // SHIFT_L dest, src
  SHIFT_L_I, // SHIFT_L_I dest, 2 byte imm
  SHIFT_R, // SHIFT_R dest, src
  SHIFT_R_I, // SHIFT_R_I dest, 2 byte imm

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
  FIRST = NOP,
  LAST = F_DIV_I,
};

std::ostream& operator<<(std::ostream& os, const std::vector<bytecode_t>& obj);

extern const char * bytecode_t_to_op [];
extern const char * bytecode_t_to_builtin_function [];
