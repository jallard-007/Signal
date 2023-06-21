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
  EXPECTING_N_ARGS,
  UNEXPECTED_TYPE,

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
  EXPECTING_TYPE,
  CANNOT_HAVE_BREAK_HERE,
  CANNOT_HAVE_CONTINUE_HERE,

  NOT_A_VARIABLE,
  NOT_A_FUNCTION,
  NOT_A_TYPE,
  NOT_A_TEMPLATE,
  WRONG_NUMBER_OF_ARGS,

  // operator type compatibility
  CANNOT_DEREFERENCE_NON_POINTER_TYPE,
  CANNOT_OPERATE_ON_TEMPORARY,
  CANNOT_BE_CONVERTED_TO_BOOL,
};

struct CheckerError {
  Token token{0,0,TokenType::NOTHING};
  GeneralDec *dec;
  CheckerErrorType type;
  CheckerError() = delete;
  CheckerError(CheckerErrorType, Token );
  CheckerError(CheckerErrorType, Token , GeneralDec*);
  CheckerError(CheckerErrorType, Expression *);
  CheckerError(CheckerErrorType, Expression *, GeneralDec*);
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

struct ResultingType {
  TokenList *type{nullptr};
  bool isLValue{false};
  ResultingType() = default;
  ResultingType(TokenList*, bool);
};

struct Checker {
  std::map<std::string, std::map<std::string, StructDecList *>> structsLookUp;
  std::map<std::string, GeneralDec *> lookUp;
  std::vector<CheckerError> errors;
  Program& program;
  Tokenizer& tokenizer;
  NodeMemPool &memPool;
  TokenList badValue {Token{0,0,TokenType::BAD_VALUE}};
  TokenList boolValue {Token{0,0,TokenType::BOOL}};
  TokenList intValue {Token{0,0,TokenType::INT32_TYPE}};
  TokenList charValue {Token{0,0,TokenType::CHAR_TYPE}};
  TokenList stringValue {Token{0,0,TokenType::STRING_LITERAL}};
  TokenList floatValue {Token{0,0,TokenType::FLOAT_TYPE}};
  TokenList doubleValue {Token{0,0,TokenType::DOUBLE_TYPE}};
  TokenList nullptrValue {Token{0,0,TokenType::NULL_PTR}};
  TokenList voidValue {Token{0,0,TokenType::VOID}};

  Checker(Program&, Tokenizer&, NodeMemPool &);
  bool check();
  void firstTopLevelScan();
  void secondTopLevelScan();
  bool checkFunction(FunctionDec&);
  bool validateFunctionHeader(FunctionDec&);
  bool validateStructInnards(StructDecList& innerDecs);
  
  bool checkScope(Scope&, std::vector<std::string>&, TokenList&, bool, bool, bool);

  bool checkStatement(Statement&);
  ResultingType checkExpression(Expression& expression, std::map<std::string, StructDecList *>* structMap = nullptr);

  bool checkType(TokenList&);
};


bool canBeConvertedToBool(TokenList&);
