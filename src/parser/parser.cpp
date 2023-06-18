#include "parser.hpp"
#include <memory>
#include <iostream>

Parser::Parser(Tokenizer& tokenizer, NodeMemPool& memPool):
  tokenizer{tokenizer}, memPool{memPool}, errorToken{0,0,TokenType::NOTHING} {}

Parser::~Parser() {
  memPool.reset();
}

bool Parser::parse() {
  GlobalDecList * list = &program.decs;
  Token token = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    if (token.type == TokenType::FUNC) {
      tokenizer.consumePeek();
      list->curr.type = GlobalDecType::FUNCTION;
      if (!parseFunction(list->curr.funcDec)) {
        return false;
      }
    }
    else if (token.type == TokenType::STRUCT) {
      tokenizer.consumePeek();
      list->curr.type = GlobalDecType::STRUCT;
      if (!parseStruct(list->curr.structDec)) {
        return false;
      }
    }
    else if (token.type == TokenType::TEMPLATE) {
      tokenizer.consumePeek();
      list->curr.type = GlobalDecType::TEMPLATE;
      if (!parseTemplate(list->curr.tempDec)) {
        return false;
      }
    }
    else if (token.type == TokenType::IDENTIFIER) {
      tokenizer.consumePeek();
      if (tokenizer.peekNext().type == TokenType::COLON) {
        tokenizer.consumePeek();
        list->curr.type = GlobalDecType::VARIABLE;
        list->curr.varDec.name = token;
        ParseStatementErrorType errorType = parseVariableDec(list->curr.varDec);
        if (errorType != ParseStatementErrorType::NONE) {
          if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
            return false;
          } else if (errorType == ParseStatementErrorType::NOT_EXPRESSION) {
            expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
          }
          return false;
        }
      }
      else {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
        return false;
      }
    }
    else {
      unexpected.emplace_back(token);
      return false;
    }
    list->next = memPool.makeGlobalDec();
    list = list->next;
  }
  return true;
}

bool Parser::parseFunction(FunctionDec& dec) {
  Token name = tokenizer.peekNext();
  if (name.type != TokenType::IDENTIFIER) {
    expected.emplace_back(ExpectedType::TOKEN, name, TokenType::IDENTIFIER);
    return false;
  }
  // consume identifier
  tokenizer.consumePeek();
  dec.name = name;
  if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_PAREN);
    return false;
  }
  // consume open paren
  tokenizer.consumePeek();
  StatementList *list = &dec.params;
  if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
    while (1) {
      Token nextToken = tokenizer.peeked;
      if (nextToken.type != TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, nextToken, TokenType::IDENTIFIER);
        return false;
      }
      // consume identifier
      tokenizer.consumePeek();
      if (tokenizer.peekNext().type != TokenType::COLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
        return false;
      }
      // consume colon
      tokenizer.consumePeek();
      list->curr.varDec = memPool.makeVariableDec(VariableDec{nextToken});
      ParseStatementErrorType errorType = parseVariableDec(*list->curr.varDec);
      if (errorType != ParseStatementErrorType::NONE) {
        if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
          expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::COMMA);
        }
        else if (errorType == ParseStatementErrorType::NOT_EXPRESSION) {
          expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
        }
        return false;
      }
      if (tokenizer.peekNext().type == TokenType::COMMA) {
        list->next = memPool.makeStatementList();
        list = list->next;
      } else if (tokenizer.peeked.type == TokenType::CLOSE_PAREN) {
        break;
      } else {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
        return false;
      }
    }
  } else {
    // consume close paren
    tokenizer.consumePeek();
  }
  ParseStatementErrorType errorType = parseScope(dec.body.scopeStatements);
  if (errorType != ParseStatementErrorType::NONE) {
    return false;
  }
  return true;
}

bool Parser::parseStruct(StructDec& dec) {

}

bool Parser::parseTemplate(TemplateDec& dec) {
  
}

/**
 * parses a scope
 * consumes the final close brace, unless there was an error
*/
ParseStatementErrorType Parser::parseScope(StatementList& statementList) {
  StatementList* list = &statementList;
  Token token = tokenizer.tokenizeNext();
  while (token.type != TokenType::CLOSE_BRACE) {
    if (token.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE);
      return ParseStatementErrorType::REPORTED;
    }
    ParseStatementErrorType errorType = parseStatement(list->curr);
    if (errorType != ParseStatementErrorType::NONE) {
      return errorType;
    }
    list->next = memPool.makeStatementList();
    list = list->next;
  }
  // consume close brace
  tokenizer.consumePeek();
  return ParseStatementErrorType::NONE;
}

/**
 * parses a single statement within a scope
 * consumes the whole statement, unless there was an error
*/
ParseStatementErrorType Parser::parseStatement(Statement &statement) {
  Token token = tokenizer.peekNext();

  // varDec or expression
  if (token.type == TokenType::IDENTIFIER) {
    tokenizer.consumePeek();
    if (parseIdentifierStatement(statement, token) != ParseStatementErrorType::NONE) {
      return ParseStatementErrorType::REPORTED;
    }
    if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
      return ParseStatementErrorType::REPORTED;
    }
    return ParseStatementErrorType::NONE;
  }

  // control flow
  else if (isControlFlow(token.type)) {
    statement.type = StatementType::CONTROL_FLOW;
    statement.controlFlow = memPool.makeControlFlowStatement();
    if (token.type == TokenType::IF) {
      statement.controlFlow->type = ControlFlowStatementType::CONDITIONAL_STATEMENT;
      tokenizer.consumePeek();
      ConditionalStatement& cond = statement.controlFlow->conditional;
      if (parseIfStatement(cond.ifStatement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }

      // elifs
      ElifStatementList**curr = &cond.elifStatement;
      while (tokenizer.peekNext().type == TokenType::ELIF) {
        tokenizer.consumePeek();
        *curr = memPool.makeElifStatementList();
        if (parseIfStatement((*curr)->elif) == ParseStatementErrorType::REPORTED) {
          return ParseStatementErrorType::REPORTED;
        }
        *curr = (*curr)->next;
      }

      if (tokenizer.peeked.type == TokenType::ELSE) {
        tokenizer.consumePeek();
        if (tokenizer.peekNext().type != TokenType::OPEN_BRACE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_BRACE);
          return ParseStatementErrorType::REPORTED;
        }
        tokenizer.consumePeek();

        cond.elseStatement = memPool.makeScope();
        ParseStatementErrorType errorType = parseScope(cond.elseStatement->scopeStatements);
        // DUP CODE. parsing scope
        if (errorType != ParseStatementErrorType::NONE) {
          if (errorType == ParseStatementErrorType::NOT_STATEMENT) {
            unexpected.emplace_back(errorToken);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer.peekNext().type != TokenType::CLOSE_BRACE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACE);
          return ParseStatementErrorType::REPORTED;
        }
        // consume close brace
        tokenizer.consumePeek();
        // END DUP
      }
    }
  
    else if (token.type == TokenType::WHILE) {
      statement.controlFlow->type = ControlFlowStatementType::WHILE_LOOP;
      tokenizer.consumePeek();
      if (parseIfStatement(statement.controlFlow->whileLoop.statement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }
    }
    
    else if (token.type == TokenType::RETURN) {
      tokenizer.consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::RETURN_STATEMENT;
      ParseExpressionErrorType errorType = parseExpression(statement.controlFlow->returnStatement.returnValue);
      if (errorType != ParseExpressionErrorType::NONE) {
        if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
          expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
        } else if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
          expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
        }
        return ParseStatementErrorType::REPORTED;
      }
      if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peekNext(), TokenType::SEMICOLON);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer.consumePeek();
    }
    
    // forLoop:= for (expression | varDec | nothing ; expression | nothing; expression | nothing) scope
    else if (token.type == TokenType::FOR) {
      auto& forLoop = statement.controlFlow->forLoop;
      tokenizer.consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::FOR_LOOP;
      if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_PAREN);
        return ParseStatementErrorType::REPORTED;
      }
      // consume open paren
      tokenizer.consumePeek();
      Token next = tokenizer.peekNext();
      if (next.type == TokenType::IDENTIFIER) {
        // consume identifier
        tokenizer.consumePeek();
        if (tokenizer.peekNext().type == TokenType::COLON) {
          parseIdentifierStatement(forLoop.initialize, next);
        }
      }

      if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer.consumePeek();
    }
  }

  // scope
  else if (token.type == TokenType::OPEN_BRACE) {
    tokenizer.consumePeek();
    statement.type = StatementType::SCOPE;
    statement.scope = memPool.makeScope();
    ParseStatementErrorType errorType = parseScope(statement.scope->scopeStatements);
    if (errorType != ParseStatementErrorType::NONE) {
      if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
      } else if (errorType == ParseStatementErrorType::NOT_STATEMENT) {
        unexpected.emplace_back(errorToken);
      } else if (errorType == ParseStatementErrorType::NOT_EXPRESSION) {
        expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
      }
      return ParseStatementErrorType::REPORTED;
    }
  }

  // unexpected token
  else if (notFirstOfExpression(token.type)) {
    unexpected.emplace_back(token);
    return ParseStatementErrorType::REPORTED;
  }

  // expression
  else {
    statement.type = StatementType::EXPRESSION;
    statement.expression = memPool.makeDefaultedExpression();
    ParseExpressionErrorType errorType = parseExpression(*statement.expression);
    if (errorType != ParseExpressionErrorType::NONE) {
      if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
        expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
      } else if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
      }
      return ParseStatementErrorType::REPORTED;
    }

    if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
      return ParseStatementErrorType::REPORTED;
    }
    tokenizer.consumePeek();
  }

  return ParseStatementErrorType::NONE;
}


ParseStatementErrorType Parser::parseVariableDec(VariableDec& varDec) {
  ParseTypeErrorType typeErrorType = getType(varDec.type);
  if (typeErrorType != ParseTypeErrorType::NONE) {
    return ParseStatementErrorType::REPORTED;
  }
  Token next = tokenizer.peekNext();
  if (next.type == TokenType::ASSIGNMENT) {
    tokenizer.consumePeek();
    // initialize
    varDec.initialAssignment = memPool.makeDefaultedExpression();
    ParseExpressionErrorType errorType = parseExpression(*varDec.initialAssignment);
    if (errorType != ParseExpressionErrorType::NONE) {
      if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
        return ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION;
      } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
        return ParseStatementErrorType::NOT_EXPRESSION;
      }
      return ParseStatementErrorType::REPORTED;
    }
  }
  return ParseStatementErrorType::NONE;
}

/**
 * Parses a statement that starts with an identifier
 * \param token the identifier token. It should be consumed by the tokenizer
 * Does NOT consume the token after the statement (semicolon, comma, etc.)
*/
ParseStatementErrorType Parser::parseIdentifierStatement(Statement& statement, Token& token) {
  Token next = tokenizer.peekNext();
  if (next.type == TokenType::COLON) {
    tokenizer.consumePeek();
    statement.type = StatementType::VARIABLE_DEC;
    statement.varDec = memPool.makeVariableDec(VariableDec{token});
    ParseStatementErrorType errorType = parseVariableDec(*statement.varDec);
    if (errorType != ParseStatementErrorType::NONE) {
      return errorType;
    }
  }
  // expression
  statement.type = StatementType::EXPRESSION;
  statement.expression = memPool.makeExpression(Expression{memPool.makeToken(token)});
  ParseExpressionErrorType errorType = parseExpression(*statement.expression, statement.expression);
  if (errorType != ParseExpressionErrorType::NONE) {
    if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
      return ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION;
    } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
      return ParseStatementErrorType::NOT_EXPRESSION;
    }
    return ParseStatementErrorType::REPORTED;
  }
  return ParseStatementErrorType::NONE;
}

/**
 * Parses if statements
 * \note this include if, elif, and while statements
*/
ParseStatementErrorType Parser::parseIfStatement(IfStatement& condStatement) {
  {
    ParseExpressionErrorType errorType = parseExpression(condStatement.condition);
    if (errorType != ParseExpressionErrorType::NONE) {
      if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::OPERATOR);
      } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
        expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
      }
      return ParseStatementErrorType::REPORTED;
    }
    if (tokenizer.peekNext().type != TokenType::OPEN_BRACE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_BRACE);
      return ParseStatementErrorType::REPORTED;
    }
    // consume open brace
    tokenizer.consumePeek();
  }
  
  ParseStatementErrorType errorType = parseScope(condStatement.body.scopeStatements);
  // DUP CODE. parsing scope
  if (errorType != ParseStatementErrorType::NONE) {
    if (errorType == ParseStatementErrorType::NOT_STATEMENT) {
      unexpected.emplace_back(errorToken);
    }
    return ParseStatementErrorType::REPORTED;
  }
  if (tokenizer.peekNext().type != TokenType::CLOSE_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACE);
    return ParseStatementErrorType::REPORTED;
  }
  // consume close brace
  tokenizer.consumePeek();
  // END DUP
  return ParseStatementErrorType::NONE;
}

/**
 * Extracts comma delimited expressions until it reaches something else, or an expression parse fails
 * Does NOT consume the final token
*/
ParseExpressionErrorType Parser::getExpressions(ExpressionList& expressions) {
  if (notFirstOfExpression(tokenizer.peekNext().type)) {
    return ParseExpressionErrorType::NONE;
  }
  ExpressionList *list = &expressions;
  while (1) {
    ParseExpressionErrorType errorType = parseExpression(list->curr);
    if (errorType != ParseExpressionErrorType::NONE) {
      return errorType;
    }
    if (tokenizer.peekNext().type != TokenType::COMMA) {
      return ParseExpressionErrorType::NONE;
    }
    tokenizer.consumePeek();
    list->next = memPool.makeExpressionList();
    list = list->next;
  }
}

/**
 * Parses a complete expression until it reaches something else, placing the root expression in rootExpression
 * Consumes the entire expression unless there was an error
*/
ParseExpressionErrorType Parser::parseExpression(Expression& rootExpression, Expression *bottom) {
  Token token = tokenizer.peekNext();
  while (1) {
    bool binary = isBinaryOp(token.type);
    if (binary || isUnaryOp(token.type)) {
      tokenizer.consumePeek();
      Expression expression;
      if (binary) {
        expression.type = ExpressionType::BINARY_OP;
        expression.binOp = memPool.makeBinOp(BinOp{token});
        if (!bottom) {
          // expected expression
          expected.emplace_back(ExpectedType::EXPRESSION, token);
          rootExpression = std::move(expression);
          bottom = &rootExpression;
          continue;
        }
      } else {
        expression.type = ExpressionType::UNARY_OP;
        expression.unOp = memPool.makeUnOp(UnOp{token});
        if (!bottom) {
          if (token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) {
            // expected expression
            expected.emplace_back(ExpectedType::EXPRESSION, token);
          }
          rootExpression = std::move(expression);
          bottom = &rootExpression;
          continue;
        }
      }
      if (rootExpression.type != ExpressionType::BINARY_OP && rootExpression.type != ExpressionType::UNARY_OP) {
        if (binary) {
          expression.binOp->leftSide = std::move(rootExpression);
        } else {
          expression.unOp->operand = std::move(rootExpression);
        }
        rootExpression = std::move(expression);
      }
      else {
        Expression* prev = nullptr;
        Expression* listIter;
        Expression* next = &rootExpression;
        while (next) {
          listIter = next;
          TokenType op;
          if (listIter->type == ExpressionType::BINARY_OP) {
            op = listIter->binOp->op.type;
            next = &listIter->binOp->rightSide;
          } else if (listIter->type == ExpressionType::UNARY_OP) {
            op = listIter->unOp->op.type;
            next = &listIter->unOp->operand;
          } else {
            break;
          }
          if (operatorPrecedence.at(token.type) <= operatorPrecedence.at(op)) {
            break;
          }
          prev = listIter;
        }

        if (prev) {
          if (binary) {
            expression.binOp->leftSide = std::move(*listIter);
          } else {
            expression.unOp->operand = std::move(*listIter);
          }
          if (prev->type == ExpressionType::BINARY_OP) {
            prev->binOp->rightSide = std::move(expression);
          } else {
            prev->unOp->operand = std::move(expression);
          }
        }
        else {
          if (binary) {
            expression.binOp->leftSide = std::move(rootExpression);
          } else {
            expression.unOp->operand = std::move(rootExpression);
          }
          // move the statement to the root
          rootExpression = std::move(expression);
          bottom = &rootExpression;
        }
      }
    } else {
      Expression expression;
      switch(token.type) {
        case TokenType::CHAR_LITERAL:
        case TokenType::STRING_LITERAL:
        case TokenType::BINARY_NUMBER:
        case TokenType::HEX_NUMBER:
        case TokenType::DECIMAL_NUMBER:
          tokenizer.consumePeek();
          expression.type = ExpressionType::VALUE;
          expression.value = memPool.makeToken(token);
          break;

        case TokenType::OPEN_PAREN: {
          tokenizer.consumePeek();
          expression.type = ExpressionType::WRAPPED;
          expression.wrapped = memPool.makeDefaultedExpression();
          ParseExpressionErrorType errorType = parseExpression(*expression.wrapped);
          if (errorType != ParseExpressionErrorType::NONE) {
            if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
              expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_PAREN, errorToken);
            } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
              expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
            }
            return ParseExpressionErrorType::REPORTED;
          }
          if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
            return ParseExpressionErrorType::REPORTED;
          }
          break;
        }

        case TokenType::IDENTIFIER: {
          tokenizer.consumePeek();
          Token next = tokenizer.peekNext();
          // DUP CODE. parsing identifier in expression
          if (next.type == TokenType::OPEN_PAREN) {
            expression.type = ExpressionType::FUNCTION_CALL;
            expression.funcCall = memPool.makeFunctionCall(FunctionCall{token});
            ParseExpressionErrorType errorType = getExpressions(expression.funcCall->args);
            if (errorType != ParseExpressionErrorType::NONE) {
              if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
                expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_PAREN_OR_COMMA, errorToken);
              } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
                expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
              }
              return ParseExpressionErrorType::REPORTED;
            }
            if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
              expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
            }
            tokenizer.consumePeek();
          }
          else if (next.type == TokenType::OPEN_BRACKET) {
            expression.type = ExpressionType::ARRAY_ACCESS;
            expression.arrAccess = memPool.makeArrayAccess(ArrayAccess{token});
            ParseExpressionErrorType errorType = parseExpression(expression.arrAccess->offset);
            if (errorType != ParseExpressionErrorType::NONE) {
              if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
                expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_BRACKET, errorToken);
              } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
                expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
              }
              return ParseExpressionErrorType::REPORTED;
            }
            if (tokenizer.peekNext().type != TokenType::CLOSE_BRACKET) {
              expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACKET);
              return ParseExpressionErrorType::REPORTED;
            }
            tokenizer.consumePeek();
          }
          // END DUP
          else {
            expression.type = ExpressionType::VALUE;
            expression.value = memPool.makeToken(token);
          }
          break;
        }

        default:
          errorToken = token;
          return ParseExpressionErrorType::NOT_EXPRESSION;
      }
      if (bottom) {
        if (bottom->type == ExpressionType::BINARY_OP) {
          if (bottom->binOp->rightSide.type != ExpressionType::NONE) {
            errorToken = token;
            return ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION;
          }
          bottom->binOp->rightSide = std::move(expression);
          bottom = &bottom->binOp->rightSide;
        } else if (bottom->type == ExpressionType::UNARY_OP) {
          if (bottom->unOp->operand.type != ExpressionType::NONE) {
            errorToken = token;
            return ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION;
          }
          bottom->unOp->operand = std::move(expression);
          bottom = &bottom->unOp->operand;
        } else {
          errorToken = token;
          return ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION;
        }
      } else {
        rootExpression = std::move(expression);
        bottom = &rootExpression;
      }
    }
    token = tokenizer.peekNext();
  }
}

/**
 * Returns the next token after the type list, adding tokens to type as it goes. tokens are in reverse order
 * Does NOT consume the final token
*/
ParseTypeErrorType Parser::getType(TokenList& type) {
  Token tp = tokenizer.peekNext();
  TokenList *curr = memPool.makeTokenList();
  if (isConcreteType(tp.type)) {
    tokenizer.consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.makeTokenList();
    curr->next = prev;
    tp = tokenizer.peekNext();
  } else {
    expected.emplace_back(ExpectedType::TOKEN, tp, TokenType::TYPE);
    return ParseTypeErrorType::REPORTED;
  }
  while (tp.type != TokenType::END_OF_FILE) {
    if (tp.type < TokenType::POINTER) {
      break;
    }
    tokenizer.consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.makeTokenList();
    curr->next = prev;
    tp = tokenizer.peekNext();
  }
  type.token = curr->next->token;
  type.next = curr->next->next;
  memPool.release(curr);
  return ParseTypeErrorType::NONE;
}
