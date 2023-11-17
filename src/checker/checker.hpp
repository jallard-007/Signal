#pragma once

#include "../nodes.hpp"
#include "../nodeMemPool.hpp"
#include <map>

enum class CheckerErrorType: uint8_t {
  NONE,

  // general
  NAME_ALREADY_IN_USE,
  VOID_TYPE,
  TYPE_DOES_NOT_MATCH,
  UNEXPECTED_TYPE,
  EXPECTED_IDENTIFIER,
  EXPECTING_TYPE,
  EXPECTING_NUMBER,
  INCORRECT_RETURN_TYPE,
  INVALID_EXIT_TYPE,
  NOT_ALL_CODE_PATHS_RETURN,
  EMPTY_STRUCT,
  STRUCT_CYCLE,

  // no such
  NO_SUCH_FUNCTION,
  NO_SUCH_TYPE,
  NO_SUCH_VARIABLE,
  NO_SUCH_TEMPLATE,
  NO_SUCH_MEMBER_VARIABLE,
  NO_SUCH_MEMBER_FUNCTION,

  // semantic errors
  CANNOT_REF_A_REF,
  CANNOT_PTR_A_REF,
  CANNOT_HAVE_MULTI_TYPE,
  
  // things in the wrong spot
  CANNOT_HAVE_BREAK_HERE,
  CANNOT_HAVE_CONTINUE_HERE,

  NOT_A_VARIABLE,
  NOT_A_FUNCTION,
  NOT_A_STRUCT,
  NOT_A_TEMPLATE,
  WRONG_NUMBER_OF_ARGS,

  // operator type compatibility
  CANNOT_DEREFERENCE_NON_POINTER_TYPE,
  CANNOT_OPERATE_ON_TEMPORARY,
  CANNOT_ASSIGN_TO_TEMPORARY,
  CANNOT_BE_CONVERTED_TO_BOOL,
  CANNOT_COMPARE_TYPE,
  CANNOT_ASSIGN,
  OPERATION_NOT_DEFINED,
  OPERATION_ON_VOID,
};

struct CheckerError {
  Token token;
  GeneralDec *dec;
  uint32_t tkIndex;
  CheckerErrorType type;
  CheckerError() = delete;
  CheckerError(CheckerErrorType, uint32_t, Token);
  CheckerError(CheckerErrorType, uint32_t, Token, GeneralDec*);
  CheckerError(CheckerErrorType, uint32_t, Expression*);
  CheckerError(CheckerErrorType, uint32_t, Expression*, GeneralDec*);
  std::string getErrorMessage(std::vector<Tokenizer>&);
};

struct ResultingType {
  TokenList *type{nullptr};
  bool isLValue{false};
  ResultingType(TokenList*, bool);
};

struct Checker {
  std::map<std::string, std::map<std::string, StructDecList *>> structsLookUp;
  std::map<std::string, GeneralDec *> lookUp;
  std::vector<CheckerError> errors;
  Program& program;
  std::vector<Tokenizer>& tokenizers;
  NodeMemPool &memPool;
  static TokenList noneValue;
  static TokenList badValue;
  static TokenList boolValue;
  static TokenList int32Value;
  static TokenList uint32Value;
  static TokenList int64Value;
  static TokenList uint64Value;
  static TokenList charValue;
  static TokenList stringValue;
  static TokenList floatValue;
  static TokenList doubleValue;
  static TokenList ptrValue;
  static TokenList nullptrValue;
  static TokenList voidValue;

  Checker(Program&, std::vector<Tokenizer>&, NodeMemPool&);
  bool check();
  void firstTopLevelScan();
  void secondTopLevelScan();
  void fullScan();
  void checkFunction(Tokenizer&, FunctionDec&);
  bool validateFunctionHeader(Tokenizer&, FunctionDec&);
  void validateStructTopLevel(Tokenizer&, StructDec&);
  void checkForStructCycles(GeneralDec&, std::vector<StructDec *>&);
  bool checkScope(Tokenizer&, Scope&, TokenList&, bool, bool);
  bool checkLocalVarDec(Tokenizer&, VariableDec&, std::vector<std::string>&);
  ResultingType checkExpression(Tokenizer&, Expression&, std::map<std::string, StructDecList *> *structMap = nullptr);
  ResultingType checkMemberAccess(Tokenizer&, ResultingType&, Expression&);
  bool checkType(Tokenizer&, TokenList&);
  static TokenList& largestType(TokenList&, TokenList&);
};

bool canBeConvertedToBool(TokenList&);
bool checkAssignment(const TokenList&, const TokenList&);
