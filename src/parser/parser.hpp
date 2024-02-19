#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../nodeMemPool.hpp"

enum class UnexpectedType : uint8_t {
  NONE,
  STRUCT_MEMBER_INITIALIZE_UNSUPPORTED,
};

struct Unexpected {
  Token token;
  uint32_t tkIndex;
  UnexpectedType unexpectedType = UnexpectedType::NONE;
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

struct Parser {
  Program program;
  std::vector<Unexpected> unexpected;
  std::vector<Expected> expected;
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
  ParseStatementErrorType parseIfStatement(IfStatement&);
  ParseStatementErrorType parseIdentifierStatement(Statement&, Token);
  ParseStatementErrorType parseVariableDec(VariableDec&);
  ParseExpressionErrorType parseExpression(Expression&);
  ParseExpressionErrorType parseLeaf(Expression&);
  ParseExpressionErrorType parseArrayOrStructLiteral(ArrayOrStructLiteral&, TokenType);
  ParseExpressionErrorType getExpressions(ExpressionList&, TokenType);
  ParseTypeErrorType getType(TokenList&);
};
