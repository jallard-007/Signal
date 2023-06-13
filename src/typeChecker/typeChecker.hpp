#pragma once

#include "../nodes.hpp"
#include <map>

enum class TypeCheckerErrorType {
  NONE,
  NAME_ALREADY_IN_USE,
  NO_MATCH,
  DEFAULT,
};

struct TypeCheckerError {
  TypeCheckerErrorType errorType;
  TypeCheckerError() = delete;
  TypeCheckerError(TypeCheckerErrorType);
};

struct TypeChecker {
  std::map<std::string, std::map<std::string, Declaration *>> structsLookUp;
  std::map<std::string, Declaration *> lookUp;
  std::vector<TypeCheckerError> errors;
  Program& program;
  Tokenizer& tokenizer;
  TypeChecker(Program&, Tokenizer&);
  
  void scanTopLevel();
  void validateProgram();

  Type *unaryTypeOperandCheck(TokenType, Token);
  Type *binaryTypeOperandCheck(Token, TokenType, Token);
};

// first do top level scan and place every dec in the map
// this includes structs and their members (variables, function headers)
// then start at the beginning and do a full search;