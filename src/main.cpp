#include <iostream>
#include <fstream>
#include "./tokenizer/tokenizer.hpp"

int main(int argc, char **argv) {
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
