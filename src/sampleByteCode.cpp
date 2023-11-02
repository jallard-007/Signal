#include <iostream>
#include "bytecodeDesign.hpp"
#include "interpreter/interpreter.hpp"

#define uc unsigned char

/*
if 1 != 1 {
  print "P\n";
}
*/
void printIfBlah() {
  FILE *fp_stdout = stdout;
  uc *s_stdout = (uc *)&fp_stdout;
  FILE *fp_stdin = stdin;
  uc *s_stdin = (uc *)&fp_stdin;
  uc data[] = {
    s_stdout[0], s_stdout[1], s_stdout[2], s_stdout[3], s_stdout[4], s_stdout[5], s_stdout[6], s_stdout[7], // stdout FILE *
    s_stdin[0], s_stdin[1], s_stdin[2], s_stdin[3], s_stdin[4], s_stdin[5], s_stdin[6], s_stdin[7], // stdin FILE *
  };
  uc const program[] = {
    (uc)OpCodes::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
    (uc)OpCodes::NOP, (uc)OpCodes::NOP, (uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 0, 1, 0, 0, 0,
    (uc)OpCodes::NOP,(uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 1, 1, 0, 0, 0,
    (uc)OpCodes::CMP, 0, 1,
    // index 23:
    (uc)OpCodes::RB_JUMP_Z, 19, // jump to exit if reg0 is equal to reg1 (1 == 1)
    (uc)OpCodes::NOP,
    (uc)OpCodes::MOVE_I, 12, 'P', '\n', 0, 0,
    (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_CHAR,
    (uc)OpCodes::SHIFT_R_I, 12, 8, 0, 0, 0,
    (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_CHAR,
    // index 42:
    (uc)OpCodes::EXIT, 0
  };
  Interpreter interpreter(program, data, 128);
  interpreter.runProgram();
}

/*
for i: int8 = 0; i < 10; ++i {
  print i;
}
*/
void loopToTen() {
  FILE *fp_stdout = stdout;
  uc *s_stdout = (uc *)&fp_stdout;
  FILE *fp_stdin = stdin;
  uc *s_stdin = (uc *)&fp_stdin;
  uc data[] = {
    s_stdout[0], s_stdout[1], s_stdout[2], s_stdout[3], s_stdout[4], s_stdout[5], s_stdout[6], s_stdout[7], // stdout FILE *
    s_stdin[0], s_stdin[1], s_stdin[2], s_stdin[3], s_stdin[4], s_stdin[5], s_stdin[6], s_stdin[7], // stdin FILE *
  };
  unsigned char const program [] =
  {
  (uc)OpCodes::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
  (uc)OpCodes::XOR, 12, 12, // initialize i
  (uc)OpCodes::NOP, (uc)OpCodes::NOP,
  (uc)OpCodes::MOVE_I, 2, 10, 0, 0, 0,

  // start of loop:
  (uc)OpCodes::CMP, 12, 2, // compare i to 10
  (uc)OpCodes::RB_JUMP_NN, 8, // loop if its not newline
  (uc)OpCodes::CALL_B, (uc)BuiltInFunctions::PRINT_UNSIGNED,
  (uc)OpCodes::INC, 12,
  (uc)OpCodes::RB_JUMP, (uc)(-9),
  
  (uc)OpCodes::XOR, 0, 0,
  (uc)OpCodes::EXIT, 0,
  };
  Interpreter interpreter(program, data, 128);
  interpreter.runProgram();
}
