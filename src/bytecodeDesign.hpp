
enum class OpCode: unsigned char {
  EXIT, // EXIT src | exits the program with the code in src
  CALL, // call a built in function such as malloc, free, print, etc.

  // CMP src1, src2 | sets flags based on the result of (signed) src1 - (signed) src2
  CMP,

  // set register to the value of the flag
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
  MOVE_B, // MOVE dest, 1 byte immediate
  MOVE_W, // MOVE dest, 2 byte immediate
  MOVE_D, // MOVE dest, 4 byte immediate
  MOVE_Q, // MOVE dest, 8 byte immediate

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
  ADD_B,
  ADD_W,
  ADD_D,
  ADD_Q,

  SUB_B,
  SUB_W,
  SUB_D,
  SUB_Q,

  MUL_B,
  MUL_W,
  MUL_D,
  MUL_Q,

  DIV_B,
  DIV_W,
  DIV_D,
  DIV_Q,

  MOD_B,
  MOD_W,
  MOD_D,
  MOD_Q,

  OR_B,
  OR_W,
  OR_D,
  OR_Q,

  AND_B,
  AND_W,
  AND_D,
  AND_Q,

  XOR_B,
  XOR_W,
  XOR_D,
  XOR_Q,

  SHIFT_L_B,
  SHIFT_L_W,
  SHIFT_L_D,
  SHIFT_L_Q,

  SHIFT_R_B,
  SHIFT_R_W,
  SHIFT_R_D,
  SHIFT_R_Q,

// floating point arithmetic
  F_ADD, // F_ADD dest, src1, src2
  F_SUB, // F_SUB dest, src1, src2
  F_MUL, // F_MUL dest, src1, src2
  F_DIV, // F_DIV dest, src1, src2
};

/*

  EXIT,
  CALL,
  FLAGS_U,
  FLAGS_S,
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
  MOVE_B,
  MOVE_W,
  MOVE_D,
  MOVE_Q,
  PUSH_B,
  PUSH_W,
  PUSH_D,
  PUSH_Q,
  POP_B,
  POP_W,
  POP_D,
  POP_Q,
  ADD_B,
  ADD_W,
  ADD_D,
  ADD_Q,
  SUB_B,
  SUB_W,
  SUB_D,
  SUB_Q,
  MUL_B,
  MUL_W,
  MUL_D,
  MUL_Q,
  DIV_B,
  DIV_W,
  DIV_D,
  DIV_Q,
  MOD_B,
  MOD_W,
  MOD_D,
  MOD_Q,
  OR_B,
  OR_W,
  OR_D,
  OR_Q,
  AND_B,
  AND_W,
  AND_D,
  AND_Q,
  XOR_B,
  XOR_W,
  XOR_D,
  XOR_Q,
  SHIFT_L_B,
  SHIFT_L_W,
  SHIFT_L_D,
  SHIFT_L_Q,
  SHIFT_R_B,
  SHIFT_R_W,
  SHIFT_R_D,
  SHIFT_R_Q,
  F_ADD,
  F_SUB,
  F_MUL,
  F_DIV,


*/