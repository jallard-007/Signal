#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../nodeMemPool.hpp"

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
  Tokenizer& tokenizer;
  NodeMemPool& memPool;
  Token errorToken;
  Parser() = delete;
  ~Parser();
  explicit Parser(Tokenizer&, NodeMemPool&);
  bool parse();
  bool parseFunction(FunctionDec&);
  bool parseStruct(StructDec&);
  bool parseTemplate(TemplateDec&);
  ParseStatementErrorType parseStatement(Statement&);
  ParseStatementErrorType parseScope(StatementList&);
  ParseStatementErrorType parseIfStatement(IfStatement& condStatement);
  ParseStatementErrorType parseIdentifierStatement(Statement&, Token);
  ParseStatementErrorType parseVariableDec(VariableDec&);
  ParseExpressionErrorType parseExpression(Expression&);
  ParseExpressionErrorType parseArrayOrStructLiteral(ArrayOrStructLiteral&);
  ParseExpressionErrorType getExpressions(ExpressionList&, TokenType);
  ParseTypeErrorType getType(TokenList&);
};
