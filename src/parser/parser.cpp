#include "parser.hpp"

Parser::Parser(Tokenizer& tokenizer, NodeMemPool& memPool):
  tokenizer{tokenizer}, memPool{memPool}, errorToken{0,0,TokenType::NOTHING} {}

Parser::~Parser() {
  memPool.reset();
}

bool Parser::parse() {
  GlobalDecList *prev = nullptr;
  GlobalDecList *list = &program.decs;
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
          return false;
        }
        if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
          return false;
        }
        tokenizer.consumePeek();
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
    prev = list;
    list->next = memPool.makeGlobalDec();
    list = list->next;
    token = tokenizer.peekNext();
  }
  prev->next = nullptr;
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
  if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
    StatementList *list = &dec.params;
    while (true) {
      Token nextToken = tokenizer.peekNext();
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
      list->curr.type = StatementType::VARIABLE_DEC;
      list->curr.varDec = memPool.makeVariableDec(VariableDec{nextToken});
      ParseStatementErrorType errorType = parseVariableDec(*list->curr.varDec);
      if (errorType != ParseStatementErrorType::NONE) {
        if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
          expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::COMMA);
        }
        return false;
      }
      if (tokenizer.peekNext().type == TokenType::COMMA) {
        tokenizer.consumePeek();
        list->next = memPool.makeStatementList();
        list = list->next;
      } else if (tokenizer.peeked.type == TokenType::CLOSE_PAREN) {
        break;
      } else {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
        return false;
      }
    }
  }
  // consume close paren
  tokenizer.consumePeek();
  // get return type
  if (tokenizer.peekNext().type != TokenType::COLON) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
    return false;
  }
  tokenizer.consumePeek();
  if (getType(dec.returnType) != ParseTypeErrorType::NONE) {
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_BRACE);
    return false;
  }
  tokenizer.consumePeek();
  ParseStatementErrorType errorType = parseScope(dec.body.scopeStatements);
  if (errorType != ParseStatementErrorType::NONE) {
    return false;
  }
  return true;
}

bool Parser::parseStruct(StructDec& dec) {
  Token token = tokenizer.peekNext();
  if (token.type != TokenType::IDENTIFIER) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER);
    return false;
  }
  tokenizer.consumePeek();
  dec.token = token;
  if (tokenizer.peekNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_BRACE);
    return false;
  }
  tokenizer.consumePeek();
  token = tokenizer.peekNext();
  StructDecList *prev = nullptr;
  StructDecList *list = &dec.decs;
  while (true) {
    if (token.type == TokenType::IDENTIFIER) {
      tokenizer.consumePeek();
      if (tokenizer.peekNext().type == TokenType::COLON) {
        tokenizer.consumePeek();
        list->type = StructDecType::VAR;
        list->varDec.name = token;
        ParseStatementErrorType errorType = parseVariableDec(list->varDec);
        if (errorType != ParseStatementErrorType::NONE) {
          if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
          }
          return false;
        }
        if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
          return false;
        }
        tokenizer.consumePeek();
      }
      else {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COLON);
        return false;
      }
    }
    else if (token.type == TokenType::FUNC) {
      tokenizer.consumePeek();
      list->type = StructDecType::FUNC;
      if (!parseFunction(list->funcDec)) {
        return false;
      }
    }
    else if (token.type == TokenType::CLOSE_BRACE) {
      tokenizer.consumePeek();
      if (prev) {
        prev->next = nullptr;
        memPool.release(list);
      }
      return true;
    }
    else {
      if (token.type == TokenType::END_OF_FILE) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE);
      } else {
        unexpected.emplace_back(token);
      }
      return false;
    }
    token = tokenizer.peekNext();
    prev = list;
    list->next = memPool.makeStructDecList();
    list = list->next;
  }
}

bool Parser::parseTemplate(TemplateDec& dec) {
  Token token = tokenizer.peekNext();
  if (token.type != TokenType::OPEN_BRACKET) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::OPEN_BRACKET);
    return false;
  }
  tokenizer.consumePeek();
  TokenList *list = &dec.templateTypes;
  token = tokenizer.peekNext();
  while (true) {
    if (token.type != TokenType::IDENTIFIER) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER);
      return false;
    }
    tokenizer.consumePeek();
    dec.templateTypes.token = token;
    if (tokenizer.peekNext().type == TokenType::CLOSE_BRACKET) {
      tokenizer.consumePeek();
      break;
    }
    if (tokenizer.peeked.type != TokenType::COMMA) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::COMMA);
      return false;
    }
    tokenizer.consumePeek();
    token = tokenizer.peekNext();
    list->next = memPool.makeTokenList();
    list = list->next;
  }
  token = tokenizer.peekNext();
  if (token.type == TokenType::STRUCT) {
    tokenizer.consumePeek();
    dec.isStruct = true;
    return parseStruct(dec.structDec);
  } else if (token.type == TokenType::FUNC) {
    tokenizer.consumePeek();
    dec.isStruct = false;
    return parseFunction(dec.funcDec);
  } else {
    expected.emplace_back(ExpectedType::FUNCTION_OR_STRUCT_DEC, token);
    return false;
  }
}

/**
 * parses a scope. the first open brace should be consumed before calling this function
 * consumes the final close brace, unless there was an error
*/
ParseStatementErrorType Parser::parseScope(StatementList& statementList) {
  StatementList* prev = nullptr;
  StatementList* list = &statementList;
  Token token = tokenizer.peekNext();
  while (token.type != TokenType::CLOSE_BRACE) {
    if (token.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE);
      return ParseStatementErrorType::REPORTED;
    }
    ParseStatementErrorType errorType = parseStatement(list->curr);
    if (errorType != ParseStatementErrorType::NONE) {
      return errorType;
    }
    prev = list;
    list->next = memPool.makeStatementList();
    list = list->next;
    token = tokenizer.peekNext();
  }
  // consume close brace
  tokenizer.consumePeek();
  if (prev) {
    prev->next = nullptr;
    memPool.release(list);
  }
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
    tokenizer.consumePeek();
    return ParseStatementErrorType::NONE;
  }

  // control flow
  else if (isControlFlow(token.type)) {
    statement.type = StatementType::CONTROL_FLOW;
    statement.controlFlow = memPool.makeControlFlowStatement();
    if (token.type == TokenType::IF) {
      tokenizer.consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::CONDITIONAL_STATEMENT;
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
          return ParseStatementErrorType::REPORTED;
        }
        // END DUP
      }
    }
  
    else if (token.type == TokenType::WHILE) {
      tokenizer.consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::WHILE_LOOP;
      if (parseIfStatement(statement.controlFlow->whileLoop.statement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }
    }
    
    else if (token.type == TokenType::RETURN) {
      tokenizer.consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::RETURN_STATEMENT;
      if (tokenizer.peekNext().type == TokenType::OPEN_BRACKET) {
        tokenizer.consumePeek();
        statement.controlFlow->returnStatement.returnValue.type = ExpressionType::ARRAY_OR_STRUCT_LITERAL;
        statement.controlFlow->returnStatement.returnValue.arrayOrStruct = memPool.makeArrayOrStruct();
        ParseExpressionErrorType errorType = parseArrayOrStructLiteral(*statement.controlFlow->returnStatement.returnValue.arrayOrStruct);
        if (tokenizer.peekNext().type != TokenType::CLOSE_BRACKET) {
          expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::CLOSE_BRACKET);
          return ParseStatementErrorType::REPORTED;
        }
        tokenizer.consumePeek();
        if (errorType != ParseExpressionErrorType::NONE) {
          return ParseStatementErrorType::REPORTED;
        }
      }
      else if (tokenizer.peeked.type != TokenType::SEMICOLON) {
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
      }
      tokenizer.consumePeek();
    }
    
    // forLoop:= for (expression | varDec | nothing ; expression | nothing; expression | nothing) scope
    else if (token.type == TokenType::FOR) {
      tokenizer.consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::FOR_LOOP;
      auto& forLoop = statement.controlFlow->forLoop;
      if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_PAREN);
        return ParseStatementErrorType::REPORTED;
      }
      // consume open paren
      tokenizer.consumePeek();
      Token next = tokenizer.peekNext();

      // parse initialize statement. can be expression or varDec
      if (next.type == TokenType::IDENTIFIER) {
        // consume identifier
        tokenizer.consumePeek();
        ParseStatementErrorType errorType = parseIdentifierStatement(forLoop.initialize, next);
        if (errorType != ParseStatementErrorType::NONE) {
          if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
          } else if (errorType == ParseStatementErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
          return ParseStatementErrorType::REPORTED;
        }
      } else if (next.type != TokenType::SEMICOLON) {
        forLoop.initialize.type = StatementType::EXPRESSION;
        forLoop.initialize.expression = memPool.makeDefaultedExpression();
        ParseExpressionErrorType errorType = parseExpression(*forLoop.initialize.expression);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer.consumePeek();

      // parse condition statement
      if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
        ParseExpressionErrorType errorType = parseExpression(forLoop.condition);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer.consumePeek();

      // parse iteration statement
      if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
        ParseExpressionErrorType errorType = parseExpression(forLoop.iteration);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::CLOSE_PAREN);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer.consumePeek();

      // parse scope
      if (tokenizer.peekNext().type != TokenType::OPEN_BRACE) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::OPEN_BRACE);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer.consumePeek();
      ParseStatementErrorType errorType = parseScope(forLoop.body.scopeStatements);
      if (errorType != ParseStatementErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
      }
    }
  }

  // scope
  else if (token.type == TokenType::OPEN_BRACE) {
    tokenizer.consumePeek();
    statement.type = StatementType::SCOPE;
    statement.scope = memPool.makeScope();
    ParseStatementErrorType errorType = parseScope(statement.scope->scopeStatements);
    if (errorType != ParseStatementErrorType::NONE) {
      return ParseStatementErrorType::REPORTED;
    }
  }

  else if (token.type == TokenType::BREAK || token.type == TokenType::CONTINUE) {
    tokenizer.consumePeek();
    statement.type = StatementType::KEYWORD;
    statement.keyword = memPool.makeToken(token);
    if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::SEMICOLON);
      return ParseStatementErrorType::REPORTED;
    }
    tokenizer.consumePeek();
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

/**
 * consume the colon after the variable name before calling this function
*/
ParseStatementErrorType Parser::parseVariableDec(VariableDec& varDec) {
  ParseTypeErrorType typeErrorType = getType(varDec.type);
  if (typeErrorType != ParseTypeErrorType::NONE) {
    return ParseStatementErrorType::REPORTED;
  }
  Token next = tokenizer.peekNext();
  if (next.type == TokenType::ASSIGNMENT) {
    varDec.initialAssignment = memPool.makeDefaultedExpression();
    tokenizer.consumePeek();
    // initialize
    if (tokenizer.peekNext().type == TokenType::OPEN_BRACKET) {
      tokenizer.consumePeek();
      varDec.initialAssignment->type = ExpressionType::ARRAY_OR_STRUCT_LITERAL;
      varDec.initialAssignment->arrayOrStruct = memPool.makeArrayOrStruct();
      ParseExpressionErrorType errorType = parseArrayOrStructLiteral(*varDec.initialAssignment->arrayOrStruct);
      if (tokenizer.peekNext().type != TokenType::CLOSE_BRACKET) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::CLOSE_BRACKET);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer.consumePeek();
      if (errorType != ParseExpressionErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
      }
    } else {
      varDec.initialAssignment = memPool.makeDefaultedExpression();
      ParseExpressionErrorType errorType = parseExpression(*varDec.initialAssignment);
      if (errorType != ParseExpressionErrorType::NONE) {
        if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
          return ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION;
        } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
          expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
        }
        return ParseStatementErrorType::REPORTED;
      }
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
    return errorType;
  }
  // expression
  statement.type = StatementType::EXPRESSION;
  statement.expression = memPool.makeDefaultedExpression();
  ParseExpressionErrorType errorType = parseExpression(*statement.expression, &token);
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
    return ParseStatementErrorType::REPORTED;
  }
  // END DUP
  return ParseStatementErrorType::NONE;
}

/**
 * Extracts comma delimited expressions until it reaches something else, or an expression parse fails
 * Does NOT consume the final token
*/
ParseExpressionErrorType Parser::getExpressions(ExpressionList& expressions, TokenType close) {
  if (tokenizer.peekNext().type == close) {
    return ParseExpressionErrorType::NONE;
  }
  ExpressionList *list = &expressions;
  while (true) {
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
 * the open bracket should be consumed before calling this. does not consume the final close bracket, similar to getExpressions
 * actual expressions cannot have an array/struct literal since it only really makes sense to initialize with / return them.
*/
ParseExpressionErrorType Parser::parseArrayOrStructLiteral(ArrayOrStructLiteral& arrayOrStruct) {
  if (tokenizer.peekNext().type == TokenType::CLOSE_BRACKET) {
    return ParseExpressionErrorType::NONE;
  }
  ExpressionList *list = &arrayOrStruct.values;
  while (true) {
    ParseExpressionErrorType errorType;
    if (tokenizer.peekNext().type == TokenType::OPEN_BRACKET) {
      tokenizer.consumePeek();
      list->curr.type = ExpressionType::ARRAY_OR_STRUCT_LITERAL;
      list->curr.arrayOrStruct = memPool.makeArrayOrStruct();
      errorType = parseArrayOrStructLiteral(*list->curr.arrayOrStruct);
      if (tokenizer.peekNext().type != TokenType::CLOSE_BRACKET) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACKET);
        return ParseExpressionErrorType::REPORTED;
      }
      tokenizer.consumePeek();
    } else {
      errorType = parseExpression(list->curr);
    }
    if (errorType != ParseExpressionErrorType::NONE) {
      if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::COMMA);
      } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
        expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
      }
      return ParseExpressionErrorType::REPORTED;
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
ParseExpressionErrorType Parser::parseExpression(Expression& rootExpression, Token* start) {
  Expression *bottom = nullptr;
  Token token = start ? *start : tokenizer.peekNext();
  while (true) {
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
          rootExpression = expression;
          bottom = &rootExpression;
          tokenizer.consumePeek();
          token = tokenizer.peekNext();
          continue;
        } else if (bottom->type == ExpressionType::BINARY_OP || bottom->type == ExpressionType::UNARY_OP) {
          expected.emplace_back(ExpectedType::EXPRESSION, token);
          return ParseExpressionErrorType::REPORTED;
        }
      } else {
        expression.type = ExpressionType::UNARY_OP;
        expression.unOp = memPool.makeUnOp(UnOp{token});
        if (!bottom) {
          if (token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) {
            // expected expression
            expected.emplace_back(ExpectedType::EXPRESSION, token);
          }
          rootExpression = expression;
          bottom = &rootExpression;
          tokenizer.consumePeek();
          token = tokenizer.peekNext();
          continue;
        } else if (token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) {
          if (bottom->type == ExpressionType::BINARY_OP || bottom->type == ExpressionType::UNARY_OP) {
            // expected expression
            expected.emplace_back(ExpectedType::EXPRESSION, token);
            return ParseExpressionErrorType::REPORTED;
          }
        }
      }
      if (rootExpression.type != ExpressionType::BINARY_OP && rootExpression.type != ExpressionType::UNARY_OP) {
        if (binary) {
          expression.binOp->leftSide = rootExpression;
        } else {
          expression.unOp->operand = rootExpression;
        }
        rootExpression = expression;
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
            expression.binOp->leftSide = *listIter;
          } else {
            expression.unOp->operand = *listIter;
          }
          if (prev->type == ExpressionType::BINARY_OP) {
            prev->binOp->rightSide = expression;
            bottom = &prev->binOp->rightSide;
          } else {
            prev->unOp->operand = expression;
            bottom = &prev->unOp->operand;
          }
        }
        else {
          if (binary) {
            expression.binOp->leftSide = rootExpression;
          } else {
            expression.unOp->operand = rootExpression;
          }
          // move the statement to the root
          rootExpression = expression;
          bottom = &rootExpression;
        }
      }
    }
    else {
      Expression expression;
      if (isLiteral(token.type)) {
        tokenizer.consumePeek();
        expression.type = ExpressionType::VALUE;
        expression.value = memPool.makeToken(token);
      }
      else if (token.type == TokenType::OPEN_PAREN) {
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
        tokenizer.consumePeek();
      }
      else if (token.type == TokenType::IDENTIFIER) {
        if (!start) {
          tokenizer.consumePeek();
        } else {
          start = nullptr;
        }
        Token next = tokenizer.peekNext();
        if (next.type == TokenType::OPEN_PAREN) {
          tokenizer.consumePeek();
          expression.type = ExpressionType::FUNCTION_CALL;
          expression.funcCall = memPool.makeFunctionCall(FunctionCall{token});
          ParseExpressionErrorType errorType = getExpressions(expression.funcCall->args, TokenType::CLOSE_PAREN);
          if (errorType != ParseExpressionErrorType::NONE) {
            if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
              expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_PAREN_OR_COMMA, errorToken);
            } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
              expected.emplace_back(ExpectedType::EXPRESSION, errorToken);
            }
            return ParseExpressionErrorType::REPORTED;
          }
          if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
            if (tokenizer.peeked.type == TokenType::COMMA) {
              expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.peeked);
            } else {
              expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
            }
            return ParseExpressionErrorType::REPORTED;
          }
          tokenizer.consumePeek();
        }
        else if (next.type == TokenType::OPEN_BRACKET) {
          tokenizer.consumePeek();
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
        else {
          expression.type = ExpressionType::VALUE;
          expression.value = memPool.makeToken(token);
        }
      }
      else {
        break;
      }
      if (bottom) {
        if (bottom->type == ExpressionType::BINARY_OP) {
          if (bottom->binOp->rightSide.type != ExpressionType::NONE) {
            errorToken = token;
            return ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION;
          }
          bottom->binOp->rightSide = expression;
          bottom = &bottom->binOp->rightSide;
        } else if (bottom->type == ExpressionType::UNARY_OP) {
          if (bottom->unOp->operand.type != ExpressionType::NONE) {
            errorToken = token;
            return ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION;
          }
          bottom->unOp->operand = expression;
          bottom = &bottom->unOp->operand;
        } else {
          errorToken = token;
          return ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION;
        }
      } else {
        rootExpression = expression;
        bottom = &rootExpression;
      }
    }
    token = tokenizer.peekNext();
  }
  if (bottom) {
    if (bottom->type == ExpressionType::BINARY_OP) {
      if (bottom->binOp->rightSide.type == ExpressionType::NONE) {
        expected.emplace_back(ExpectedType::EXPRESSION, token);
        return ParseExpressionErrorType::REPORTED;
      }
      return ParseExpressionErrorType::NONE;
    } else if (bottom->type == ExpressionType::UNARY_OP) {
      if (bottom->unOp->operand.type == ExpressionType::NONE) {
        expected.emplace_back(ExpectedType::EXPRESSION, token);
        return ParseExpressionErrorType::REPORTED;
      }
      return ParseExpressionErrorType::NONE;
    } else {
      return ParseExpressionErrorType::NONE;
    }
  } else {
    errorToken = token;
    return ParseExpressionErrorType::NOT_EXPRESSION;
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
