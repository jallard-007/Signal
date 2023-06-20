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
  ParseStatementErrorType parseIdentifierStatement(Statement&, Token&);
  ParseStatementErrorType parseVariableDec(VariableDec&);
  ParseExpressionErrorType parseExpression(Expression&, Token* start = nullptr);
  ParseExpressionErrorType parseArrayOrStructLiteral(ArrayOrStructLiteral&);
  ParseExpressionErrorType getExpressions(ExpressionList&, TokenType);
  ParseTypeErrorType getType(TokenList&);
};

const std::unordered_map<TokenType, uint8_t> operatorPrecedence {
  {TokenType::COMMA, 0},

  {TokenType::ASSIGNMENT, 1},
  {TokenType::MODULO_ASSIGNMENT, 1},
  {TokenType::ADDITION_ASSIGNMENT, 1},
  {TokenType::DIVISION_ASSIGNMENT, 1},
  {TokenType::BITWISE_OR_ASSIGNMENT, 1},
  {TokenType::SHIFT_LEFT_ASSIGNMENT, 1},
  {TokenType::BITWISE_AND_ASSIGNMENT, 1},
  {TokenType::SHIFT_RIGHT_ASSIGNMENT, 1},
  {TokenType::SUBTRACTION_ASSIGNMENT, 1},
  {TokenType::MULTIPLICATION_ASSIGNMENT, 1},
  {TokenType::BITWISE_XOR_ASSIGNMENT, 1},
  {TokenType::TERNARY, 1},

  {TokenType::LOGICAL_OR, 2},
  {TokenType::LOGICAL_AND, 3},
  {TokenType::BITWISE_OR, 4},
  {TokenType::BITWISE_XOR, 5},
  {TokenType::BITWISE_AND, 6},

  {TokenType::EQUAL, 7},
  {TokenType::NOT_EQUAL, 7},

  {TokenType::GREATER_THAN, 8},
  {TokenType::GREATER_THAN_EQUAL, 8},
  {TokenType::LESS_THAN, 8},
  {TokenType::LESS_THAN_EQUAL, 8},

  {TokenType::SHIFT_LEFT, 9},
  {TokenType::SHIFT_RIGHT, 9},

  {TokenType::ADDITION, 10},
  {TokenType::SUBTRACTION, 10},

  {TokenType::MULTIPLICATION, 11},
  {TokenType::DIVISION, 11},
  {TokenType::MODULO, 11},

  {TokenType::ADDRESS_OF, 13},
  {TokenType::DEREFERENCE, 13},
  {TokenType::NOT, 13},
  {TokenType::NEGATIVE, 13},
  {TokenType::DECREMENT_PREFIX, 13},
  {TokenType::INCREMENT_PREFIX, 13},

  {TokenType::DOT, 14},
  {TokenType::PTR_MEMBER_ACCESS, 14},
  {TokenType::DECREMENT_POSTFIX, 14},
  {TokenType::INCREMENT_POSTFIX, 14},
};
