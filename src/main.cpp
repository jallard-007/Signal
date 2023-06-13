#include <iostream>
#include <fstream>
#include <string>
#include "./parser/parser.hpp"
#include <time.h>

int main(int argc, char **argv) {
  clock_t begin = clock();
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <Filepath>\n";
    return 1;
  }
  std::cout << "Filepath: " << argv[1] << '\n';
  std::ifstream t(argv[1]);
  if (!t.is_open()) {
    std::cerr << "Could not open the file\n";
    return 1;
  }
  t.seekg(0, std::ios::end);
  size_t size = t.tellg();
  std::string buffer;
  buffer.resize(size);
  t.seekg(0);
  t.read(&buffer[0], size);
  t.close();
  Tokenizer tk(argv[1], buffer);
  NodeMemPool mem;
  Parser p{tk, mem};
  if (!p.parse()) {
    std::cout << "Error:\n";
  }
  if (!p.expected.empty()) {
    for (auto enx : p.expected) {
      std::cout << enx.getErrorMessage(argv[1]);
    }
    exit(1);
  }
  if (!p.unexpected.empty()) {
    for (auto enx : p.unexpected) {
      std::cout << enx.getErrorMessage(tk, argv[1]);
    }
    exit(1);
  }
  std::string pretty;
  pretty.reserve(size);
  p.program.prettyPrint(tk, pretty);
  std::cout << pretty << '\n';
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  std::cout << time_spent << '\n';
  return 0;
}

// void wtf() {
//   const char * str = "(first: int, second: double, third: customType ^^)";
//   Tokenizer tokenizer{str};
//   std::cout << tokenizer.content << '\n';
//   std::vector<std::string> params;
//   std::string st = tokenizer.extractToken({1, TokenType::IDENTIFIER});
//   params.emplace_back(st);
//   st = tokenizer.extractToken({13, TokenType::IDENTIFIER});
//   params.emplace_back(st);
//   std::cout << tokenizer.content << '\n';
// }
