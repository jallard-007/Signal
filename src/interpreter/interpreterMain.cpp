#include "interpreter.hpp"
#include "../utils.hpp"

int main(int argc, char **argv) {
  if (argc != 2) {
    return 1;
  }
  std::vector<unsigned char> file;
  openAndReadFile(argv[1], file);
  Interpreter interpreter{(unsigned char *)file.data(), (unsigned char *)nullptr, 1000000};
  return interpreter.runProgram();
}
