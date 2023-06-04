#include <iostream>
#include <fstream>
#include "./tokenizer/tokenizer.hpp"

void printUsage() {
  std::cout << "Usage: ./main <filename>\n";
}

int main(int argc, char **argv) {
  if (argc != 2) {
    printUsage();
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
  Tokenizer lex(buffer);
  auto vec = lex.tokenizeAll();
  if (vec.back().type == TokenType::BAD_VALUE) {
    std::cerr << "Unknown symbol\n";
    return 1;
  }
  for (auto v : vec) {
    if (typeToString.find(v.type) != typeToString.end()) {
      std::cout << typeToString.at(v.type);
    }
  }
  return 0;
}