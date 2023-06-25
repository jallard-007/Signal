#pragma once

#include "../tokenizer/tokenizer.hpp"
#include "../nodeMemPool.hpp"

struct Unexpected {
  Token token;
  Unexpected() = delete;
  explicit Unexpected(const Token&);
  std::string getErrorMessage(Tokenizer&, const std::string&);
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
  TokenType expectedTokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, const Token&);
  Expected(ExpectedType, const Token&, TokenType);
  std::string getErrorMessage(Tokenizer&, const std::string&);
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
