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
  NOTHING,
  EXPRESSION,
  TOKEN,
  FUNCTION_OR_STRUCT_DEC,
  OPERATOR_OR_CLOSE_BRACKET,
  OPERATOR_OR_CLOSE_PAREN,
  OPERATOR_OR_CLOSE_PAREN_OR_COMMA,
  OPERATOR_OR_SEMICOLON,
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
  EXPRESSION_AFTER_EXPRESSION,
  NOT_EXPRESSION,
};

enum class ParseStatementErrorType: uint8_t {
  NONE,
  REPORTED,
  NOT_STATEMENT,
  EXPRESSION_AFTER_EXPRESSION,
  NOT_EXPRESSION,
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
  ParseStatementErrorType parseIfStatement(IfStatement& condStatement);
  ParseStatementErrorType parseIdentifierStatement(Statement&, Token);
  ParseStatementErrorType parseVariableDec(VariableDec&);
  ParseExpressionErrorType parseExpression(Expression&);
  ParseExpressionErrorType parseArrayOrStructLiteral(ArrayOrStructLiteral&);
  ParseExpressionErrorType getExpressions(ExpressionList&, TokenType);
  ParseTypeErrorType getType(TokenList&);
};
