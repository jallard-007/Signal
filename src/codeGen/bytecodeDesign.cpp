
enum class OpCodes: char {

  EXIT, // EXIT max 64 bit number

  LOAD, // LOAD dest, src
  STORE, // STORE dest, src
  MOVE, // MOVE dest, src
  MOVE_IMM, // MOVE_IMM dest, max 64 bit number
  PUSH, // PUSH src
  POP, // POP dest
  RETURN, // RETURN
  JUMP, // JUMP src
  JUMP_Z, // jump if zero | JUMP_Z src
  JUMP_P, // jump if positive | JUMP_P src
  JUMP_N, // jump if negative | JUMP_N src

  // arithmetic
  ADD, // ADD dest, src1, src2
  SUB, // SUB dest, src1, src2
  MUL, // MUL dest, src1, src2
  DIV, // DIV dest, src1, src2
  MOD, // MOD dest, src1, src2
  OR, // OR dest, src1, src2
  AND, // AND dest, src1, src2
  XOR, // XOR dest, src1, src2
  SHIFT_L, // XOR dest, src1, src2
  SHIFT_R, // XOR dest, src1, src2

  // floating point
  F_ADD, // F_ADD dest, src1, src2
  F_SUB, // F_SUB dest, src1, src2
  F_MUL, // F_MUL dest, src1, src2
  F_DIV, // F_DIV dest, src1, src2

  // printing to console
  PRINT_CHAR, // PRINT_CHAR src
  PRINT_STRING, // PRINT_STRING src
  PRINT_DOUBLE, // PRINT_DOUBLE src
  PRINT_INT_SIGNED, // PRINT_INT_SIGNED src
  PRINT_INT_UNSIGNED, // PRINT_INT_UNSIGNED src
  PRINT_HEX, // PRINT_HEX src
};
