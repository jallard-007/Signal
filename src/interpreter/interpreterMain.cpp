#include "interpreter.hpp"
#include "utils.hpp"

int main(int argc, char **argv) {
    if (argc != 2) {
        return 1;
    }
    std::vector<unsigned char> file;
    if (!openAndReadFile(argv[1], file)) {
        exit(1);
    }
    bytecode_t data[24];
    Interpreter interpreter{file.data(), data, 1000000};
    return interpreter.runProgram();
}
