#include <iostream>
#include "bytecodeDesign.hpp"
#include "interpreter/interpreter.hpp"

#define bc bytecode_t

/*
if 1 != 1 {
    print "P\n";
}
*/
void printIfBlah() {
    bc data[16];
    ((FILE **)data)[0] = stdout;
    ((FILE **)data)[1] = stdin;
    bc const program[] = {
        (bc)OpCode::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
        (bc)OpCode::NOP, (bc)OpCode::NOP, (bc)OpCode::NOP,
        (bc)OpCode::MOVE_I, 0, 1, 0, 0, 0,
        (bc)OpCode::NOP,(bc)OpCode::NOP,
        (bc)OpCode::MOVE_I, 1, 1, 0, 0, 0,
        (bc)OpCode::CMP, 0, 1,
        // index 23:
        (bc)OpCode::RB_JUMP_Z, 19, // jump to exit if reg0 is equal to reg1 (1 == 1)
        (bc)OpCode::NOP,
        (bc)OpCode::MOVE_I, 12, 'P', '\n', 0, 0,
        (bc)OpCode::CALL_B, (bc)BuiltInFunction::PRINT_CHAR,
        (bc)OpCode::SHIFT_R_I, 12, 8, 0, 0, 0,
        (bc)OpCode::CALL_B, (bc)BuiltInFunction::PRINT_CHAR,
        // index 42:
        (bc)OpCode::EXIT, 0
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
    bc data[16];
    ((FILE **)data)[0] = stdout;
    ((FILE **)data)[1] = stdin;
    bytecode_t const program [] =
    {
    (bc)OpCode::LOAD_Q, 11, dataPointerIndex, // load stdout FILE * from data
    (bc)OpCode::XOR, 12, 12, // initialize i
    (bc)OpCode::NOP, (bc)OpCode::NOP,
    (bc)OpCode::MOVE_I, 2, 10, 0, 0, 0,

    // start of loop:
    (bc)OpCode::CMP, 12, 2, // compare i to 10
    (bc)OpCode::RB_JUMP_NN, 8, // loop if its not newline
    (bc)OpCode::CALL_B, (bc)BuiltInFunction::PRINT_UNSIGNED,
    (bc)OpCode::INC, 12,
    (bc)OpCode::RB_JUMP, (bc)(-9),
    
    (bc)OpCode::XOR, 0, 0,
    (bc)OpCode::EXIT, 0,
    };
    Interpreter interpreter(program, data, 128);
    interpreter.runProgram();
}
