#include "parser.hpp"
#include <memory>
#include <iostream>

Parser::Parser(Tokenizer& tokenizer, NodeMemPool& memPool): tokenizer{tokenizer}, memPool{memPool} {}

Parser::~Parser() {
  memPool.reset();
}

bool Parser::parse() {
  GlobalDecList * list = &program.decs;
  Token token = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    switch (token.type) {
      case TokenType::INCLUDE: {
        tokenizer.consumePeek();
        break;
      }

      case TokenType::FUNC: {
        tokenizer.consumePeek();
        list->curr.type = GlobalDecType::FUNCTION;
        if (!functionDec(list->curr.funcDec)) {
          return false;
        }
        list->next = memPool.getGlobalDec();
        list = list->next;
        break;
      }

      case TokenType::STRUCT: {
        tokenizer.consumePeek();
        list->curr.type == GlobalDecType::STRUCT;
        if (!structDec(list->curr.structDec)) {
          return false;
        }
        list->next = memPool.getGlobalDec();
        list = list->next;
        break;
      }

      case TokenType::TEMPLATE: {
        tokenizer.consumePeek();
        list->curr.type == GlobalDecType::TEMPLATE;
        if (!templateDec(list->curr.tempDec)) {
          return false;
        }
        list->next = memPool.getGlobalDec();
        list = list->next;
        break;
      }

      case TokenType::CREATE: {
        tokenizer.consumePeek();
        // template creation
        break;
      }

      case TokenType::ENUM: {
        tokenizer.consumePeek();
        // enum
        break;
      }

      case TokenType::IDENTIFIER: {
        // global variable declaration
        tokenizer.consumePeek();
        if (tokenizer.peekNext().type != TokenType::COLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
          return false;
        }
        tokenizer.consumePeek();
        VariableDec ac{token};
        Token tokenAfter = getType(ac.type);
        if (ac.type.tokens.curr.type == TokenType::NOTHING) {
          expected.emplace_back(ExpectedType::TOKEN, tokenAfter, TokenType::IDENTIFIER);
          return false;
        }
        if (tokenAfter.type == TokenType::END_OF_FILE) {
          // never reached a type delimiter
          expected.emplace_back(ExpectedType::TOKEN, tokenAfter, TokenType::SEMICOLON);
          return false;
        }
        if (tokenAfter.type == TokenType::ASSIGNMENT) {
          tokenizer.consumePeek();
          Token tkBefore = tokenAfter;
          tkBefore.linePos += 1;
          ac.initialAssignment = memPool.get(parseStatement(TokenType::SEMICOLON, TokenType::SEMICOLON));
          if (ac.initialAssignment->type == StatementType::NONE) {
            expected.emplace_back(ExpectedType::EXPRESSION, tkBefore);
          } else if (!hasData(ac.initialAssignment->type) && ac.initialAssignment->type != StatementType::ARRAY_OR_STRUCT_LITERAL) {
            expected.emplace_back(ExpectedType::EXPRESSION, tkBefore);
            expected.emplace_back(ExpectedType::TOKEN, tkBefore, TokenType::SEMICOLON);
            unexpected.emplace_back(tokenAfter);
          }
          tokenAfter = tokenizer.peekNext();
        }
        if (tokenAfter.type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenAfter, TokenType::SEMICOLON);
          break;
        }

        tokenizer.consumePeek();

        program.decs.emplace_back(memPool.get(std::move(ac)));
        break;
      }
        
      default:
        // error, unexpected token
        unexpected.emplace_back(token);
        return false;
    }
    token = tokenizer.peekNext();
  }
  return true;
}

bool Parser::functionDec(FunctionDec& dec) {
  const Token token = tokenizer.tokenizeNext();
  if (token.type != TokenType::IDENTIFIER) {
    // expected identifier
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER);
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
    // expected open paren
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_PAREN);
    return false;
  }
  FunctionDec funDec{token};
  // get parameters
  tokenizer.consumePeek();
  if (!parseScope(funDec.params, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
    if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
      return false;
    }
  } else {
    // consume close paren
    tokenizer.consumePeek();
  }
  if (tokenizer.peekNext().type != TokenType::COLON) {
    // expected a colon
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
    return false;
  }
  tokenizer.consumePeek();
  // get return type
  if (getType(funDec.returnType).type != TokenType::OPEN_BRACE) {
    // expected open brace after type
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_BRACE);
    return false;
  }
  // consume open brace
  tokenizer.consumePeek();
  if (!parseScope(funDec.body.scopeStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE)) {
    if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACE);
      return false;
    }
  } else {
    // consume close brace
    tokenizer.consumePeek();
  }

  dec.func = memPool.get(std::move(funDec));
  dec.decType = DecType::FUNCTION;
  return true;
}

bool Parser::structDec(Declaration& dec) {
  Token token = tokenizer.tokenizeNext();
  if (token.type != TokenType::IDENTIFIER) {
    // expected identifier
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER);
    return false;
  }
  if (tokenizer.tokenizeNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::OPEN_BRACE);
    return false;
  }
  // now parse struct body
  // can consist of variables and functions at the top level, nothing else
  Struct sDec{token};
  token = tokenizer.peekNext();
  while (1) {
    // some statement
    if (token.type == TokenType::IDENTIFIER) {
      tokenizer.consumePeek();
      if (tokenizer.peekNext().type != TokenType::COLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
        return false;
      }
      tokenizer.consumePeek();
      VariableDec ac{token};
      if (getType(ac.type).type != TokenType::SEMICOLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
        return false;
      }
      tokenizer.consumePeek();
      if (ac.type.tokens.curr.type == TokenType::NOTHING) {
        expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.peeked);
      }
      sDec.decs.emplace_back(memPool.get(std::move(ac)));
    }
    // function dec
    else if (token.type == TokenType::FUNC) {
      auto& funcDec = sDec.decs.emplace_back();
      tokenizer.consumePeek();
      if (!functionDec(funcDec)) {
        sDec.decs.pop_back();
        return false;
      }
    }
    // end of struct
    else if (token.type == TokenType::CLOSE_BRACE) {
      tokenizer.consumePeek();
      dec.decType = DecType::STRUCT;
      dec.struc = memPool.get(std::move(sDec));
      return true;
    }
    // invalid token
    else { 
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE);
      return false;
    }
    token = tokenizer.peekNext();
  }
}

bool Parser::templateDec(Declaration& dec) {
  Token token = tokenizer.peekNext();
  if (token.type != TokenType::OPEN_BRACKET) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::OPEN_BRACKET);
    return false;
  }
  tokenizer.consumePeek();
  Template temp;
  token = tokenizer.peekNext();
  if (token.type == TokenType::CLOSE_BRACKET) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER);
    tokenizer.consumePeek();
  } else {
    TokenList* prev = nullptr;
    TokenList* templateTypes = &temp.templateIdentifiers;
    while (1) {
      if (token.type != TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER);
        return false;
      }
      templateTypes->curr = token;
      templateTypes->next = memPool.getTokenList();
      prev = templateTypes;
      templateTypes = templateTypes->next;
      tokenizer.consumePeek();
      token = tokenizer.peekNext();
      if (token.type == TokenType::COMMA) {
        tokenizer.consumePeek();
        token = tokenizer.peekNext();
      } else if (token.type == TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::COMMA);
      } else if (token.type == TokenType::CLOSE_BRACKET) {
        prev->next = nullptr;
        memPool.release(templateTypes);
        tokenizer.consumePeek();
        break;
      }
    }
  }

  token = tokenizer.peekNext();
  if (token.type == TokenType::STRUCT) {
    tokenizer.consumePeek();
    if (!structDec(temp.dec)) {
      return false;
    }
  } else if (token.type == TokenType::FUNC) {
    tokenizer.consumePeek();
    if (!functionDec(temp.dec)) {
      return false;
    }
  } else {
    unexpected.emplace_back(token);
    return false;
  }
  dec.temp = memPool.get(std::move(temp));
  dec.decType = DecType::TEMPLATE;
  return true;
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
    list->next = memPool.getStatementList();
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
  if (isControlFlow(token.type)) {
    statement.type == StatementType::CONTROL_FLOW;
    statement.controlFlow = memPool.getControlFlowStatement();
    if (token.type == TokenType::IF) {
      statement.controlFlow->type == ControlFlowStatementType::CONDITIONAL_STATEMENT;
      tokenizer.consumePeek();
      ConditionalStatement& cond = statement.controlFlow->conditional;
      if (parseIfStatement(cond.ifStatement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }

      // elifs
      ElifStatementList**curr = &cond.elifStatement;
      while (tokenizer.peekNext().type == TokenType::ELIF) {
        tokenizer.consumePeek();
        *curr = memPool.getElifStatementList();
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

        cond.elseStatement = memPool.getScope();
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
      statement.controlFlow->type == ControlFlowStatementType::WHILE_LOOP;
      tokenizer.consumePeek();
      if (parseIfStatement(statement.controlFlow->whileLoop.statement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }
    }
    
    else if (token.type == TokenType::RETURN) {
      tokenizer.consumePeek();
      statement.controlFlow->type == ControlFlowStatementType::RETURN_STATEMENT;
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
      statement.controlFlow->type == ControlFlowStatementType::FOR_LOOP;
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

  }

  // unexpected token
  else if (notFirstOfExpression(token.type)) {
    
  }

  // expression
  else {
    statement.type = StatementType::EXPRESSION;
    statement.expression = memPool.getDefaultedExpression();
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

/**
 * Parses a statement that starts with an identifier
 * \param token the identifier token. It should be consumed by the tokenizer
 * Does NOT consume the token after the statement (semicolon, comma, etc.)
*/
ParseStatementErrorType Parser::parseIdentifierStatement(Statement& statement, Token& token) {
  Token next = tokenizer.peekNext();
  if (next.type == TokenType::COLON) {
    statement.type = StatementType::VARIABLE_DEC;
    statement.varDec = memPool.getVariableDec(VariableDec{token});
    ParseTypeErrorType typeErrorType = getType(statement.varDec->type);
    if (typeErrorType != ParseTypeErrorType::NONE) {
      return ParseStatementErrorType::REPORTED;
    }
    if (next.type == TokenType::ASSIGNMENT) {
      // initialize
      statement.varDec->initialAssignment = memPool.getDefaultedExpression();
      ParseExpressionErrorType errorType = parseExpression(*statement.varDec->initialAssignment);
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
  // expression
  statement.type = StatementType::EXPRESSION;
  statement.expression = memPool.getExpression(Expression{memPool.getToken(token)});
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
    list->next = memPool.getExpressionList();
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
        expression.binOp = memPool.getBinOp(BinOp{token});
        if (!bottom) {
          // expected expression
          expected.emplace_back(ExpectedType::EXPRESSION, token);
          rootExpression = std::move(expression);
          bottom = &rootExpression;
          continue;
        }
      } else {
        expression.type = ExpressionType::UNARY_OP;
        expression.unOp = memPool.getUnOp(UnOp{token});
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
          expression.value = memPool.getToken(token);
          break;

        case TokenType::OPEN_PAREN: {
          tokenizer.consumePeek();
          expression.type = ExpressionType::WRAPPED;
          expression.wrapped = memPool.getDefaultedExpression();
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
            expression.funcCall = memPool.getFunctionCall(FunctionCall{token});
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
            expression.arrAccess = memPool.getArrayAccess(ArrayAccess{token});
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
            expression.value = memPool.getToken(token);
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

// var ptr ptr
ParseTypeErrorType Parser::getType(TokenList& type) {
  Token tp = tokenizer.peekNext();
  TokenList *curr = memPool.getTokenList();
  if (isConcreteType(tp.type)) {
    tokenizer.consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.getTokenList();
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
    curr = memPool.getTokenList();
    curr->next = prev;
    tp = tokenizer.peekNext();
  }
  type.token = curr->next->token;
  type.next = curr->next->next;
  memPool.release(curr);
  return ParseTypeErrorType::NONE;
}
