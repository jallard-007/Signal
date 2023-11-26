#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <iterator>
#include <chrono>
#include "./parser/parser.hpp"
#include "./checker/checker.hpp"

bool openAndReadFile(const std::string& file, std::string& buffer) {
  std::ifstream t(file);
  if (!t.is_open()) {
    std::cerr << "Could not open file: " << file << '\n';
    return false;
  }
  t.seekg(0, std::ios::end);
  size_t size = t.tellg();
  buffer.resize(size);
  t.seekg(0);
  t.read(&buffer[0], size);
  t.close();
  return true;
}

/**
 * Splits a file path by /, separating the directories
*/
void splitFilePath(const std::string& filePath, std::vector<std::string>& split) {
  size_t last = 0; 
  size_t next = 0;
  while ((next = filePath.find('/', last)) != std::string::npos) {
    split.push_back(filePath.substr(last, next-last));
    last = next + 1;
  }
  split.push_back(filePath.substr(last));
}

/**
 * General design and details:
 * - Every parsed file has it's own Tokenizer. When an 'include' declaration is encountered, a new Tokenizer is created
 * for the included file and swapped into the Parser object. This allows for good error message details
 * since position and file information can easily be extracted even during the Checker phase.
 * 
 * - The file path is stored in the Tokenizer object. Paths are kept minimized by spliting paths
 * and removing or adding directories only when required from 'include's
*/
int main(int argc, char **argv) {
  auto begin = std::chrono::high_resolution_clock::now();
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <Filepath>\n";
    return 1;
  }
  // try to open the cl argument
  std::string mainFile = argv[1];
  std::cout << "Filepath: " << mainFile << '\n';
  std::string buffer;
  if (!openAndReadFile(mainFile, buffer)) {
    return 1;
  }

  std::map<std::string, bool> includedFiles;
  includedFiles.emplace(mainFile, true);

  // split the relative file path by directory
  // we split by directory so that when we encounter an "include", we can easily alter the path to keep it minimal
  std::vector<std::string> currentFileDirectory;
  splitFilePath(mainFile, currentFileDirectory);
  currentFileDirectory.pop_back(); // pop the filename off

  std::vector<Tokenizer> tokenizers;
  tokenizers.emplace_back(std::move(mainFile), std::move(buffer)); // create a tokenizer for the main file
  NodeMemPool mem;
  Parser parser{tokenizers[0], mem};
  uint32_t tokenizerIndex = 0;
  while (true) {
    GeneralDec* dec = parser.parseNext();
    if (!dec) {
      // there was some error
      break;
    }
    dec->tokenizerIndex = tokenizerIndex;
    if (dec->type == GeneralDecType::NONE) {
      // end of file for current tokenizer. find next valid tokenizer, swap, and continue parsing
      if (tokenizerIndex == 0) {
        // no more tokenizers
        break;
      }
      while (tokenizerIndex > 0) {
        if (tokenizers[--tokenizerIndex].peekNext().type != TokenType::END_OF_FILE) {
          parser.swapTokenizer(tokenizers[tokenizerIndex]);
          currentFileDirectory.clear();
          splitFilePath(tokenizers[tokenizerIndex].filePath, currentFileDirectory);
          currentFileDirectory.pop_back();
          break;
        }
      }
    }
    else if (dec->type == GeneralDecType::INCLUDE_DEC) {
      Tokenizer& tk = tokenizers[tokenizerIndex];
      std::string includedFileName = tk.extractToken(dec->includeDec->file);

      // remove quotes at front and end
      includedFileName.erase(includedFileName.end()-1);
      includedFileName.erase(includedFileName.begin());
      if (includedFileName.empty()) {
        std::cerr << "Invalid include path\n";
        TokenPositionInfo posInfo = tk.getTokenPositionInfo(dec->includeDec->file);
        std::cerr << tk.filePath << ':' << posInfo.lineNum << ':' << posInfo.linePos << '\n';
        return 1;
      }

      std::vector<std::string> splittedPath;
      splitFilePath(includedFileName, splittedPath);
      for (auto& directory: splittedPath) {
        if (directory == "..") {
          // move up one directory
          if (currentFileDirectory.empty()) {
            currentFileDirectory.emplace_back("..");
          } else if (currentFileDirectory.back() == "..") {
            currentFileDirectory.emplace_back("..");
          } else if (currentFileDirectory.back() == ".") {
            currentFileDirectory.back() += '.';
          } else {
            currentFileDirectory.pop_back();
          }
        } else if (directory != ".") { 
          currentFileDirectory.emplace_back(directory);
        }
      }
      // join the vector of strings into one string
      const char* const delim = "/";
      std::ostringstream imploded;
      std::copy(currentFileDirectory.begin(), currentFileDirectory.end(), std::ostream_iterator<std::string>(imploded, delim));
      // remove the filename from the file path
      currentFileDirectory.pop_back();
      std::string relativePath = imploded.str();
      if (!relativePath.empty()) {
        // remove the trailing /
        relativePath.pop_back();
      }

      // has the file already been included?
      if (!includedFiles[includedFileName]) {
        includedFiles[includedFileName] = true;
        if (!openAndReadFile(relativePath, buffer)) {
          TokenPositionInfo posInfo = tk.getTokenPositionInfo(dec->includeDec->file);
          std::cerr << tk.filePath << ':' << posInfo.lineNum << ':' << posInfo.linePos << '\n';
          return 1;
        }
        tokenizers.emplace_back(std::move(relativePath), std::move(buffer));
        tokenizerIndex = (uint32_t)tokenizers.size() - 1;
        tokenizers.back().tokenizerIndex = tokenizerIndex;
        parser.swapTokenizer(tokenizers.back());
      }
    }
  }
  if (parser.globalPrev) {
    mem.release(parser.globalPrev->next);
    parser.globalPrev->next = nullptr;
  }

  if (!parser.expected.empty() || !parser.unexpected.empty()) {
    for (auto& error : parser.expected) {
      std::cerr << error.getErrorMessage(tokenizers);
    }
    for (auto& error : parser.unexpected) {
      std::cerr << error.getErrorMessage(tokenizers);
    }
    return 1;
  }
  Checker checker{parser.program, tokenizers, mem};
  checker.check();
  auto end = std::chrono::high_resolution_clock::now();
  double total = std::chrono::duration_cast<std::chrono::duration<double>>(end - begin).count();
  std::cout << "Execution time: " << total << '\n';
  for (const CheckerError &err : checker.errors) {
    std::cout << err.getErrorMessage(tokenizers) << '\n';
  }
  if (checker.errors.size() == MAX_ERRORS) {
    std::cout << "Max errors reached\n";
  }
  return !checker.errors.empty();
}
