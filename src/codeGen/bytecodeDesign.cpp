
enum class OpCodes: char {

  EXIT,

  LOAD,
  STORE,
  MOVE,
  PUSH,
  POP,
  RETURN,
  JUMP,
  JUMP_Z, // jump if zero
  JUMP_P, // jump if positive
  JUMP_N, // jump if negative

  // arithmetic
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  OR,
  AND,
  XOR,
  INC,
  DEC,
  SHIFT_L,
  SHIFT_R,

  // printing to console
  PRINT_CHAR,
  PRINT_STRING,
  PRINT_NUMBER_SIGNED,
  PRINT_NUMBER_UNSIGNED,
  PRINT_POINTER,

};
