#pragma once

#include <cstdint>
#include <cstdlib>
#include <vector>
#include "bytecodeDesign/bytecodeDesign.hpp"

struct Interpreter {
    private:
    std::vector<unsigned char> __stack;
    public:
    uint64_t registers [NUM_REGISTERS] {0};
    unsigned char *stack {0};
    bytecode_t const * const program;
    int exitCode {0};
    bool z {0}; // zero flag
    bool p {0}; // positive flag
    bool running {true};
    Interpreter() = delete;
    Interpreter(
        const bytecode_t *programInstructions,
        unsigned char *programData,
        uint64_t stackSize
    );
    int runProgram();
};
