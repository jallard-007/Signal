#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../nodeMemPool.hpp"

struct Unexpected {
  Token token;
  uint32_t tkIndex;
  Unexpected() = delete;
  explicit Unexpected(const Token&, uint32_t);
  std::string getErrorMessage(std::vector<Tokenizer>&);
};

enum class ExpectedType : uint8_t {
  NONE,
  EXPRESSION,
  TOKEN,
};

struct Expected {
  Token tokenWhereExpected;
  uint32_t tkIndex;
  TokenType expectedTokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, const Token&, uint32_t);
  Expected(ExpectedType, const Token&, TokenType, uint32_t);
  std::string getErrorMessage(std::vector<Tokenizer>&);
};

enum class ParseExpressionErrorType: uint8_t {
  NONE,
  REPORTED,
};

enum class ParseStatementErrorType: uint8_t {
  NONE,
  REPORTED,
};

enum class ParseTypeErrorType: uint8_t {
  NONE,
  REPORTED,
};

struct Item {
  Expression exp;
  uint8_t prec;
  Item(const Expression&, uint8_t);
};

struct Parser {
  Program program;
  std::vector<Unexpected> unexpected;
  std::vector<Expected> expected;
  std::vector<Item> items;

  Tokenizer *tokenizer;
  NodeMemPool &memPool;
  GeneralDecList *globalPrev = nullptr;
  GeneralDecList *globalList = &program.decs;
  Token errorToken;
  Parser() = delete;
  ~Parser();
  explicit Parser(Tokenizer&, NodeMemPool&);
  bool parse();
  GeneralDec *parseNext();
  bool parseFunction(FunctionDec&);
  bool parseStruct(StructDec&);
  bool parseTemplate(TemplateDec&);
  void swapTokenizer(Tokenizer&);

  ParseStatementErrorType parseStatement(Statement&);
  ParseStatementErrorType parseScope(StatementList&);
  ParseStatementErrorType parseExpressionBeforeScope(Expression&);
  ParseStatementErrorType parseIfStatement(IfStatement& condStatement);
  ParseStatementErrorType parseIdentifierStatement(Statement&, Token);
  ParseStatementErrorType parseVariableDec(VariableDec&);
#define OLD false
#if OLD
  ParseExpressionErrorType parseExpression(Expression&, uint8_t = 0);
  void parseIncreasingPrecBinaryOp(Expression*& node, Expression& left, uint8_t minPrec);
  uint8_t parseIncreasingPrecUnaryOp(Expression*& node, Expression& next, uint8_t minPrec);
#else
  ParseExpressionErrorType parseExpression(Expression&);
  Expression* parseIncreasingPrecUnaryOp(Expression& next, uint8_t minPrec);
#endif
  ParseExpressionErrorType parseLeaf(Expression& expression);
  ParseExpressionErrorType parseArrayOrStructLiteral(ArrayOrStructLiteral&);
  ParseExpressionErrorType getExpressions(ExpressionList&, TokenType);
  ParseTypeErrorType getType(TokenList&);
};
