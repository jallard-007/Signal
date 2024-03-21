#include "interpreter.hpp"
#include "utils.hpp"

int main(int argc, char **argv) {
  if (argc != 2) {
    return 1;
  }
  std::vector<unsigned char> file;
  openAndReadFile(argv[1], file);
  Interpreter interpreter{(bytecode_t *)file.data(), (bytecode_t *)nullptr, 1000000};
  return interpreter.runProgram();
}
