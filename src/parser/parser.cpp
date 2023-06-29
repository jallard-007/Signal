#include "parser.hpp"

Unexpected::Unexpected(const Token& token, uint32_t tkIndex): token{token}, tkIndex{tkIndex} {}
std::string Unexpected::getErrorMessage(std::vector<Tokenizer>& tks) {
  auto& tk = tks[tkIndex];
  TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
  std::string message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
  return message + "Unexpected Token: " + tk.extractToken(token) + "\n\n";
}

Expected::Expected(ExpectedType exType, const Token& token, uint32_t tkIndex): tokenWhereExpected{token}, tkIndex{tkIndex}, expectedTokenType{TokenType::NOTHING}, expectedType{exType} {}
Expected::Expected(ExpectedType exType, const Token& token, TokenType tkType, uint32_t tkIndex): tokenWhereExpected{token}, tkIndex{tkIndex}, expectedTokenType{tkType}, expectedType{exType} {}
std::string Expected::getErrorMessage(std::vector<Tokenizer>& tks) {
  auto& tk = tks[tkIndex];
  TokenPositionInfo posInfo = tk.getTokenPositionInfo(tokenWhereExpected);
  std::string message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
  if (expectedType == ExpectedType::EXPRESSION) {
    return message + "Expected Expression\n\n";
  }
  if (expectedType == ExpectedType::TOKEN) {
    return message + "Expected Token: " + typeToString.at(expectedTokenType) + "\n\n";
  }
  return message + "\n\n";
}



// TokenType::NEGATIVE is the "largest" operator token type with an enum value of 82, hence size 83
uint8_t operatorPrecedence [83]{};
__attribute__((constructor))
void initializeOperatorPrecedence() {
  operatorPrecedence[(uint8_t)TokenType::ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::MODULO_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::ADDITION_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::DIVISION_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::BITWISE_OR_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::SHIFT_LEFT_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::BITWISE_AND_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::SHIFT_RIGHT_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::SUBTRACTION_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::MULTIPLICATION_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::BITWISE_XOR_ASSIGNMENT] = 1;
  operatorPrecedence[(uint8_t)TokenType::TERNARY] = 1;
  operatorPrecedence[(uint8_t)TokenType::LOGICAL_OR] = 2;
  operatorPrecedence[(uint8_t)TokenType::LOGICAL_AND] = 3;
  operatorPrecedence[(uint8_t)TokenType::BITWISE_OR] = 4;
  operatorPrecedence[(uint8_t)TokenType::BITWISE_XOR] = 5;
  operatorPrecedence[(uint8_t)TokenType::BITWISE_AND] = 6;
  operatorPrecedence[(uint8_t)TokenType::EQUAL] = 7;
  operatorPrecedence[(uint8_t)TokenType::NOT_EQUAL] = 7;
  operatorPrecedence[(uint8_t)TokenType::GREATER_THAN] = 8;
  operatorPrecedence[(uint8_t)TokenType::GREATER_THAN_EQUAL] = 8;
  operatorPrecedence[(uint8_t)TokenType::LESS_THAN] = 8;
  operatorPrecedence[(uint8_t)TokenType::LESS_THAN_EQUAL] = 8;
  operatorPrecedence[(uint8_t)TokenType::SHIFT_LEFT] = 9;
  operatorPrecedence[(uint8_t)TokenType::SHIFT_RIGHT] = 9;
  operatorPrecedence[(uint8_t)TokenType::ADDITION] = 10;
  operatorPrecedence[(uint8_t)TokenType::SUBTRACTION] = 10;
  operatorPrecedence[(uint8_t)TokenType::MULTIPLICATION] = 11;
  operatorPrecedence[(uint8_t)TokenType::DIVISION] = 11;
  operatorPrecedence[(uint8_t)TokenType::MODULO] = 11;
  operatorPrecedence[(uint8_t)TokenType::ADDRESS_OF] = 13;
  operatorPrecedence[(uint8_t)TokenType::DEREFERENCE] = 13;
  operatorPrecedence[(uint8_t)TokenType::NOT] = 13;
  operatorPrecedence[(uint8_t)TokenType::NEGATIVE] = 13;
  operatorPrecedence[(uint8_t)TokenType::DECREMENT_PREFIX] = 13;
  operatorPrecedence[(uint8_t)TokenType::INCREMENT_PREFIX] = 13;
  operatorPrecedence[(uint8_t)TokenType::DOT] = 14;
  operatorPrecedence[(uint8_t)TokenType::PTR_MEMBER_ACCESS] = 14;
  operatorPrecedence[(uint8_t)TokenType::DECREMENT_POSTFIX] = 14;
  operatorPrecedence[(uint8_t)TokenType::INCREMENT_POSTFIX] = 14;
}

Parser::Parser(Tokenizer& tokenizer, NodeMemPool& memPool):
  tokenizer{&tokenizer}, memPool{memPool}, errorToken{0,0,TokenType::NOTHING} {}

Parser::~Parser() {
  memPool.reset();
}

void Parser::swapTokenizer(Tokenizer& nextTokenizer) {
  tokenizer = &nextTokenizer;
}

/**
 * Parses the entire tokenizer ouput
*/
bool Parser::parse() {
  Token token = tokenizer->peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    if (!parseNext()) {
      return false;
    }
    token = tokenizer->peekNext();
  }
  globalPrev->next = nullptr;
  return true;
}

/**
 * Parses the next general declaration
 * \returns a pointer to the declaration
*/
GeneralDec* Parser::parseNext() {
  Token token = tokenizer->tokenizeNext();
  if (token.type == TokenType::FUNC) {
    globalList->curr.type = GeneralDecType::FUNCTION;
    globalList->curr.funcDec = memPool.makeFunctionDec();
    if (!parseFunction(*globalList->curr.funcDec)) {
      return nullptr;
    }
  }
  else if (token.type == TokenType::STRUCT) {
    globalList->curr.type = GeneralDecType::STRUCT;
    globalList->curr.structDec = memPool.makeStructDec();
    if (!parseStruct(*globalList->curr.structDec)) {
      return nullptr;
    }
  }
  else if (token.type == TokenType::TEMPLATE) {
    globalList->curr.type = GeneralDecType::TEMPLATE;
    globalList->curr.tempDec = memPool.makeTemplateDec();
    if (!parseTemplate(*globalList->curr.tempDec)) {
      return nullptr;
    }
  }
  else if (token.type == TokenType::IDENTIFIER) {
    if (tokenizer->tokenizeNext().type == TokenType::COLON) {
      globalList->curr.type = GeneralDecType::VARIABLE;
      globalList->curr.varDec = memPool.makeVariableDec(VariableDec{token});
      ParseStatementErrorType errorType = parseVariableDec(*globalList->curr.varDec);
      if (errorType != ParseStatementErrorType::NONE) {
        return nullptr;
      }
      if (tokenizer->tokenizeNext().type != TokenType::SEMICOLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
        return nullptr;
      }
    }
    else {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
      return nullptr;
    }
  }
  else if (token.type == TokenType::CREATE) {
    globalList->curr.type = GeneralDecType::TEMPLATE_CREATE;
    globalList->curr.tempCreate = memPool.makeTemplateCreation();
    token = tokenizer->tokenizeNext();
    if (token.type != TokenType::IDENTIFIER) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
      return nullptr;
    }
    globalList->curr.tempCreate->templateName = token;
    if (tokenizer->tokenizeNext().type != TokenType::OPEN_BRACKET) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACKET, tokenizer->tokenizerIndex);
      return nullptr;
    }
    token = tokenizer->tokenizeNext();
    if (token.type != TokenType::IDENTIFIER && !isBuiltInType(token.type)) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::TYPE, tokenizer->tokenizerIndex);
      return nullptr;
    }
    globalList->curr.tempCreate->templateTypes.token = token;
    TokenList *tokenPrev = &globalList->curr.tempCreate->templateTypes;
    tokenPrev->next = memPool.makeTokenList();
    TokenList *tkList = tokenPrev->next;
    while (tokenizer->peekNext().type == TokenType::COMMA) {
      tokenizer->consumePeek();
      token = tokenizer->tokenizeNext();
      if (token.type != TokenType::IDENTIFIER && !isBuiltInType(token.type)) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::TYPE, tokenizer->tokenizerIndex);
        return nullptr;
      }
      tokenPrev = tkList;
      tkList->token = token;
      tkList->next = memPool.makeTokenList();
      tkList = tkList->next;
    }
    tokenPrev->next = nullptr;
    memPool.release(tkList);
    if (tokenizer->tokenizeNext().type != TokenType::CLOSE_BRACKET) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
      return nullptr;
    }
    if (tokenizer->tokenizeNext().type != TokenType::AS) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::AS, tokenizer->tokenizerIndex);
      return nullptr;
    }
    token = tokenizer->tokenizeNext();
    if (token.type != TokenType::IDENTIFIER) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
      return nullptr;
    }
    globalList->curr.tempCreate->typeName = token;
    if (tokenizer->tokenizeNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
      return nullptr;
    }
  }
  else if (token.type == TokenType::INCLUDE) {
    globalList->curr.type = GeneralDecType::INCLUDE_DEC;
    globalList->curr.includeDec = memPool.makeIncludeDec();
    token = tokenizer->tokenizeNext();
    if (token.type != TokenType::STRING_LITERAL) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::STRING_LITERAL, tokenizer->tokenizerIndex);
      return nullptr;
    }
    globalList->curr.includeDec->file = token;
  }
  else if (token.type == TokenType::END_OF_FILE) {
    return &globalList->curr;
  }
  else {
    unexpected.emplace_back(token, tokenizer->tokenizerIndex);
    return nullptr;
  }
  globalPrev = globalList;
  globalList->next = memPool.makeGeneralDecList();
  globalList = globalList->next;
  return &globalPrev->curr;
}

bool Parser::parseFunction(FunctionDec& dec) {
  Token name = tokenizer->peekNext();
  if (name.type != TokenType::IDENTIFIER) {
    expected.emplace_back(ExpectedType::TOKEN, name, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
    return false;
  }
  // consume identifier
  tokenizer->consumePeek();
  dec.name = name;
  if (tokenizer->peekNext().type != TokenType::OPEN_PAREN) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_PAREN, tokenizer->tokenizerIndex);
    return false;
  }
  // consume open paren
  tokenizer->consumePeek();
  if (tokenizer->peekNext().type != TokenType::CLOSE_PAREN) {
    StatementList *paramList = &dec.params;
    while (true) {
      Token nextToken = tokenizer->peekNext();
      if (nextToken.type != TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, nextToken, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
        return false;
      }
      // consume identifier
      tokenizer->consumePeek();
      if (tokenizer->peekNext().type != TokenType::COLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
        return false;
      }
      // consume colon
      tokenizer->consumePeek();
      paramList->curr.type = StatementType::VARIABLE_DEC;
      paramList->curr.varDec = memPool.makeVariableDec(VariableDec{nextToken});
      ParseStatementErrorType errorType = parseVariableDec(*paramList->curr.varDec);
      if (errorType != ParseStatementErrorType::NONE) {
        if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
          expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::COMMA, tokenizer->tokenizerIndex);
        }
        return false;
      }
      if (tokenizer->peekNext().type == TokenType::COMMA) {
        tokenizer->consumePeek();
        paramList->next = memPool.makeStatementList();
        paramList = paramList->next;
      } else if (tokenizer->peeked.type == TokenType::CLOSE_PAREN) {
        break;
      } else {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
        return false;
      }
    }
  }
  // consume close paren
  tokenizer->consumePeek();
  // get return type
  if (tokenizer->peekNext().type != TokenType::COLON) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
    return false;
  }
  tokenizer->consumePeek();
  if (getType(dec.returnType) != ParseTypeErrorType::NONE) {
    return false;
  }
  if (tokenizer->peekNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
    return false;
  }
  tokenizer->consumePeek();
  ParseStatementErrorType errorType = parseScope(dec.body.scopeStatements);
  if (errorType != ParseStatementErrorType::NONE) {
    return false;
  }
  return true;
}

bool Parser::parseStruct(StructDec& dec) {
  Token token = tokenizer->peekNext();
  if (token.type != TokenType::IDENTIFIER) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
    return false;
  }
  tokenizer->consumePeek();
  dec.name = token;
  if (tokenizer->peekNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
    return false;
  }
  tokenizer->consumePeek();
  token = tokenizer->peekNext();
  StructDecList *prev = nullptr;
  StructDecList *list = &dec.decs;
  while (true) {
    if (token.type == TokenType::IDENTIFIER) {
      tokenizer->consumePeek();
      if (tokenizer->peekNext().type == TokenType::COLON) {
        tokenizer->consumePeek();
        list->type = StructDecType::VAR;
        list->varDec = memPool.makeVariableDec(VariableDec{token});
        ParseStatementErrorType errorType = parseVariableDec(*list->varDec);
        if (errorType != ParseStatementErrorType::NONE) {
          if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          }
          return false;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          return false;
        }
        tokenizer->consumePeek();
      }
      else {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
        return false;
      }
    }
    else if (token.type == TokenType::FUNC) {
      tokenizer->consumePeek();
      list->type = StructDecType::FUNC;
      list->funcDec = memPool.makeFunctionDec();
      if (!parseFunction(*list->funcDec)) {
        return false;
      }
    }
    else if (token.type == TokenType::CLOSE_BRACE) {
      tokenizer->consumePeek();
      if (prev) {
        prev->next = nullptr;
        memPool.release(list);
      }
      return true;
    }
    else {
      if (token.type == TokenType::END_OF_FILE) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
      } else {
        unexpected.emplace_back(token, tokenizer->tokenizerIndex);
      }
      return false;
    }
    token = tokenizer->peekNext();
    prev = list;
    list->next = memPool.makeStructDecList();
    list = list->next;
  }
}

bool Parser::parseTemplate(TemplateDec& dec) {
  Token token = tokenizer->peekNext();
  if (token.type != TokenType::OPEN_BRACKET) {
    expected.emplace_back(ExpectedType::TOKEN, token, TokenType::OPEN_BRACKET, tokenizer->tokenizerIndex);
    return false;
  }
  tokenizer->consumePeek();
  TokenList *list = &dec.templateTypes;
  token = tokenizer->peekNext();
  while (true) {
    if (token.type != TokenType::IDENTIFIER) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
      return false;
    }
    tokenizer->consumePeek();
    dec.templateTypes.token = token;
    if (tokenizer->peekNext().type == TokenType::CLOSE_BRACKET) {
      tokenizer->consumePeek();
      break;
    }
    if (tokenizer->peeked.type != TokenType::COMMA) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COMMA, tokenizer->tokenizerIndex);
      return false;
    }
    tokenizer->consumePeek();
    token = tokenizer->peekNext();
    list->next = memPool.makeTokenList();
    list = list->next;
  }
  token = tokenizer->peekNext();
  if (token.type == TokenType::STRUCT) {
    tokenizer->consumePeek();
    dec.isStruct = true;
    return parseStruct(dec.structDec);
  } else if (token.type == TokenType::FUNC) {
    tokenizer->consumePeek();
    dec.isStruct = false;
    return parseFunction(dec.funcDec);
  } else {
    expected.emplace_back(ExpectedType::FUNCTION_OR_STRUCT_DEC, token, tokenizer->tokenizerIndex);
    return false;
  }
}

/**
 * parses a scope. the first open brace should be consumed before calling this function
 * consumes the final close brace, unless there was an error
 * \returns one of ParseStatementErrorType::REPORTED and ParseStatementErrorType::NONE
*/
ParseStatementErrorType Parser::parseScope(StatementList& statementList) {
  StatementList* prev = nullptr;
  StatementList* list = &statementList;
  Token token = tokenizer->peekNext();
  while (token.type != TokenType::CLOSE_BRACE) {
    if (token.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
      return ParseStatementErrorType::REPORTED;
    }
    ParseStatementErrorType errorType = parseStatement(list->curr);
    if (errorType != ParseStatementErrorType::NONE) {
      return errorType;
    }
    prev = list;
    list->next = memPool.makeStatementList();
    list = list->next;
    token = tokenizer->peekNext();
  }
  // consume close brace
  tokenizer->consumePeek();
  if (prev) {
    prev->next = nullptr;
    memPool.release(list);
  }
  return ParseStatementErrorType::NONE;
}

/**
 * parses a single statement within a scope
 * consumes the whole statement, unless there was an error
 * \returns one of ParseStatementErrorType::REPORTED and ParseStatementErrorType::NONE
*/
ParseStatementErrorType Parser::parseStatement(Statement &statement) {
  Token token = tokenizer->peekNext();
  if (token.type == TokenType::IDENTIFIER) { // varDec or expression
    tokenizer->consumePeek();
    if (parseIdentifierStatement(statement, token) != ParseStatementErrorType::NONE) {
      return ParseStatementErrorType::REPORTED;
    }
    if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
      return ParseStatementErrorType::REPORTED;
    }
    tokenizer->consumePeek();
    return ParseStatementErrorType::NONE;
  }
  else if (isControlFlow(token.type)) {
    statement.type = StatementType::CONTROL_FLOW;
    statement.controlFlow = memPool.makeControlFlowStatement();
    if (token.type == TokenType::IF) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::CONDITIONAL_STATEMENT;
      statement.controlFlow->conditional = memPool.makeConditionalStatement();
      ConditionalStatement& cond = *statement.controlFlow->conditional;
      if (parseIfStatement(cond.ifStatement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }

      ElifStatementList **curr = &cond.elifStatement;
      while (tokenizer->peekNext().type == TokenType::ELIF) {
        tokenizer->consumePeek();
        *curr = memPool.makeElifStatementList();
        if (parseIfStatement((*curr)->elif) == ParseStatementErrorType::REPORTED) {
          return ParseStatementErrorType::REPORTED;
        }
        curr = &(*curr)->next;
      }

      if (tokenizer->peeked.type == TokenType::ELSE) {
        tokenizer->consumePeek();
        if (tokenizer->peekNext().type != TokenType::OPEN_BRACE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
        tokenizer->consumePeek();

        cond.elseStatement = memPool.makeScope();
        ParseStatementErrorType errorType = parseScope(cond.elseStatement->scopeStatements);
        if (errorType != ParseStatementErrorType::NONE) {
          return ParseStatementErrorType::REPORTED;
        }
      }
    }
    else if (token.type == TokenType::WHILE) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::WHILE_LOOP;
      statement.controlFlow->whileLoop = memPool.makeWhileLoop();
      if (parseIfStatement(statement.controlFlow->whileLoop->statement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }
    }
    else if (token.type == TokenType::RETURN) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::RETURN_STATEMENT;
      statement.controlFlow->returnStatement = memPool.makeReturnStatement();
      auto& returnValue = statement.controlFlow->returnStatement->returnValue;
      if (tokenizer->peekNext().type == TokenType::OPEN_BRACKET) {
        tokenizer->consumePeek();
        returnValue.type = ExpressionType::ARRAY_OR_STRUCT_LITERAL;
        returnValue.arrayOrStruct = memPool.makeArrayOrStruct();
        ParseExpressionErrorType errorType = parseArrayOrStructLiteral(*returnValue.arrayOrStruct);
        if (tokenizer->peekNext().type != TokenType::CLOSE_BRACKET) {
          expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
        tokenizer->consumePeek();
        if (errorType != ParseExpressionErrorType::NONE) {
          return ParseStatementErrorType::REPORTED;
        }
      }
      else if (tokenizer->peeked.type != TokenType::SEMICOLON) {
        ParseExpressionErrorType errorType = parseExpression(returnValue);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
          } else if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peekNext(), TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer->consumePeek();
    }
    else if (token.type == TokenType::FOR) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::FOR_LOOP;
      statement.controlFlow->forLoop = memPool.makeForLoop();

      auto& forLoop = statement.controlFlow->forLoop;
      if (tokenizer->peekNext().type != TokenType::OPEN_PAREN) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_PAREN, tokenizer->tokenizerIndex);
        return ParseStatementErrorType::REPORTED;
      }
      // consume open paren
      tokenizer->consumePeek();
      Token next = tokenizer->peekNext();

      // parse initialize statement. can be expression or varDec
      if (next.type == TokenType::IDENTIFIER) {
        // consume identifier
        tokenizer->consumePeek();
        ParseStatementErrorType errorType = parseIdentifierStatement(forLoop->initialize, next);
        if (errorType != ParseStatementErrorType::NONE) {
          if (errorType == ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          } else if (errorType == ParseStatementErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken, tokenizer->tokenizerIndex);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
      } else if (next.type != TokenType::SEMICOLON) {
        forLoop->initialize.type = StatementType::EXPRESSION;
        forLoop->initialize.expression = memPool.makeExpression();
        ParseExpressionErrorType errorType = parseExpression(*forLoop->initialize.expression);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken, tokenizer->tokenizerIndex);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer->consumePeek();

      // parse condition statement
      if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
        ParseExpressionErrorType errorType = parseExpression(forLoop->condition);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken, tokenizer->tokenizerIndex);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer->consumePeek();

      // parse iteration statement
      if (tokenizer->peekNext().type != TokenType::CLOSE_PAREN) {
        ParseExpressionErrorType errorType = parseExpression(forLoop->iteration);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            unexpected.emplace_back(errorToken, tokenizer->tokenizerIndex);
          }
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::CLOSE_PAREN) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
      }
      tokenizer->consumePeek();

      // parse scope
      if (tokenizer->peekNext().type != TokenType::OPEN_BRACE) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer->consumePeek();
      ParseStatementErrorType errorType = parseScope(forLoop->body.scopeStatements);
      if (errorType != ParseStatementErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
      }
    }
    else if (token.type == TokenType::SWITCH) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::SWITCH_STATEMENT;
      auto& switchStatement = statement.controlFlow->switchStatement;
      switchStatement = memPool.makeSwitchStatement();
      if (parseExpressionBeforeScope(switchStatement->switched) != ParseStatementErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
      }
      for (SwitchScopeStatementList *list = &switchStatement->body, *prev = nullptr;; list = list->next) {
        Token next = tokenizer->peekNext();
        if (next.type == TokenType::CASE) {
          tokenizer->consumePeek();
          list->caseExpression = memPool.makeExpression();
          if (parseExpressionBeforeScope(*list->caseExpression) != ParseStatementErrorType::NONE) {
            if (expected.back().expectedType != ExpectedType::TOKEN && expected.back().expectedTokenType != TokenType::OPEN_BRACE) {
              return ParseStatementErrorType::REPORTED;
            }
            expected.pop_back();
          } else {
            list->caseBody = memPool.makeScope();
            parseScope(list->caseBody->scopeStatements);
          }
        }
        else if (next.type == TokenType::DEFAULT) {
          tokenizer->consumePeek();
          if (tokenizer->peekNext().type != TokenType::OPEN_BRACE) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
            return ParseStatementErrorType::REPORTED;
          }
          // consume open brace
          tokenizer->consumePeek();
          list->caseBody = memPool.makeScope();
          if (parseScope(list->caseBody->scopeStatements) != ParseStatementErrorType::NONE) {
            return ParseStatementErrorType::REPORTED;
          }
        }
        else if (next.type == TokenType::CLOSE_BRACE) {
          if (prev) {
            prev->next = nullptr;
            memPool.release(list);
          }
          tokenizer->consumePeek();
          break;
        }
        else {
          unexpected.emplace_back(next, tokenizer->tokenizerIndex);
          return ParseStatementErrorType::REPORTED;
        }
        prev = list;
        list->next = memPool.makeSwitchScopeStatementList();
      }
    }
  }
  else if (token.type == TokenType::OPEN_BRACE) { // scope
    tokenizer->consumePeek();
    statement.type = StatementType::SCOPE;
    statement.scope = memPool.makeScope();
    ParseStatementErrorType errorType = parseScope(statement.scope->scopeStatements);
    if (errorType != ParseStatementErrorType::NONE) {
      return ParseStatementErrorType::REPORTED;
    }
  }
  else if (token.type == TokenType::BREAK || token.type == TokenType::CONTINUE) {
    tokenizer->consumePeek();
    statement.type = StatementType::KEYWORD;
    statement.keyword = token;
    if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
      return ParseStatementErrorType::REPORTED;
    }
    tokenizer->consumePeek();
  }
  else if (notFirstOfExpression(token.type)) { // unexpected token
    unexpected.emplace_back(token, tokenizer->tokenizerIndex);
    return ParseStatementErrorType::REPORTED;
  }
  else { // expression
    statement.type = StatementType::EXPRESSION;
    statement.expression = memPool.makeExpression();
    ParseExpressionErrorType errorType = parseExpression(*statement.expression);
    if (errorType != ParseExpressionErrorType::NONE) {
      if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
        expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
      } else if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
      }
      return ParseStatementErrorType::REPORTED;
    }

    if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
      return ParseStatementErrorType::REPORTED;
    }
    tokenizer->consumePeek();
  }

  return ParseStatementErrorType::NONE;
}

/**
 * consume the colon after the variable name before calling this function
 * \returns one of ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION, ParseStatementErrorType::REPORTED, and ParseStatementErrorType::NONE
*/
ParseStatementErrorType Parser::parseVariableDec(VariableDec& varDec) {
  ParseTypeErrorType typeErrorType = getType(varDec.type);
  if (typeErrorType != ParseTypeErrorType::NONE) {
    return ParseStatementErrorType::REPORTED;
  }
  Token next = tokenizer->peekNext();
  if (next.type == TokenType::ASSIGNMENT) {
    varDec.initialAssignment = memPool.makeExpression();
    tokenizer->consumePeek();
    // initialize
    if (tokenizer->peekNext().type == TokenType::OPEN_BRACKET) {
      tokenizer->consumePeek();
      varDec.initialAssignment->type = ExpressionType::ARRAY_OR_STRUCT_LITERAL;
      varDec.initialAssignment->arrayOrStruct = memPool.makeArrayOrStruct();
      ParseExpressionErrorType errorType = parseArrayOrStructLiteral(*varDec.initialAssignment->arrayOrStruct);
      if (tokenizer->peekNext().type != TokenType::CLOSE_BRACKET) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer->consumePeek();
      if (errorType != ParseExpressionErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
      }
    } else {
      varDec.initialAssignment = memPool.makeExpression();
      ParseExpressionErrorType errorType = parseExpression(*varDec.initialAssignment);
      if (errorType != ParseExpressionErrorType::NONE) {
        if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
          return ParseStatementErrorType::EXPRESSION_AFTER_EXPRESSION;
        } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
          expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
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
ParseStatementErrorType Parser::parseIdentifierStatement(Statement& statement, Token token) {
  Token next = tokenizer->peekNext();
  if (next.type == TokenType::COLON) {
    tokenizer->consumePeek();
    statement.type = StatementType::VARIABLE_DEC;
    statement.varDec = memPool.makeVariableDec(VariableDec{token});
    ParseStatementErrorType errorType = parseVariableDec(*statement.varDec);
    return errorType;
  }
  // expression
  statement.type = StatementType::EXPRESSION;
  statement.expression = memPool.makeExpression();
  tokenizer->position = token.position;
  tokenizer->peeked.type = TokenType::NOTHING;
  ParseExpressionErrorType errorType = parseExpression(*statement.expression);
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
 * \note this includes if, elif, and while statements
*/
ParseStatementErrorType Parser::parseIfStatement(IfStatement& condStatement) {
  if (parseExpressionBeforeScope(condStatement.condition) != ParseStatementErrorType::NONE) {
    return ParseStatementErrorType::REPORTED;
  }
  ParseStatementErrorType errorType = parseScope(condStatement.body.scopeStatements);
  if (errorType != ParseStatementErrorType::NONE) {
    return ParseStatementErrorType::REPORTED;
  }
  return ParseStatementErrorType::NONE;
}

ParseStatementErrorType Parser::parseExpressionBeforeScope(Expression& expression) {
  ParseExpressionErrorType errorType = parseExpression(expression);
  if (errorType != ParseExpressionErrorType::NONE) {
    if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
      expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::OPERATOR, tokenizer->tokenizerIndex);
    } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
      expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
    }
    return ParseStatementErrorType::REPORTED;
  }
  if (tokenizer->peekNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
    return ParseStatementErrorType::REPORTED;
  }
  // consume open brace
  tokenizer->consumePeek();
  return ParseStatementErrorType::NONE;
}

/**
 * Extracts comma delimited expressions until it reaches something else, or an expression parse fails
 * Does NOT consume the final token
*/
ParseExpressionErrorType Parser::getExpressions(ExpressionList& expressions, TokenType close) {
  if (tokenizer->peekNext().type == close) {
    return ParseExpressionErrorType::NONE;
  }
  ExpressionList *list = &expressions;
  while (true) {
    ParseExpressionErrorType errorType = parseExpression(list->curr);
    if (errorType != ParseExpressionErrorType::NONE) {
      return errorType;
    }
    if (tokenizer->peekNext().type != TokenType::COMMA) {
      return ParseExpressionErrorType::NONE;
    }
    tokenizer->consumePeek();
    list->next = memPool.makeExpressionList();
    list = list->next;
  }
}

/**
 * the open bracket should be consumed before calling this. does not consume the final close bracket, similar to getExpressions
 * actual expressions cannot have an array/struct literal since it only really makes sense to initialize with / return them.
*/
ParseExpressionErrorType Parser::parseArrayOrStructLiteral(ArrayOrStructLiteral& arrayOrStruct) {
  if (tokenizer->peekNext().type == TokenType::CLOSE_BRACKET) {
    return ParseExpressionErrorType::NONE;
  }
  ExpressionList *list = &arrayOrStruct.values;
  while (true) {
    ParseExpressionErrorType errorType;
    if (tokenizer->peekNext().type == TokenType::OPEN_BRACKET) {
      tokenizer->consumePeek();
      list->curr.type = ExpressionType::ARRAY_OR_STRUCT_LITERAL;
      list->curr.arrayOrStruct = memPool.makeArrayOrStruct();
      errorType = parseArrayOrStructLiteral(*list->curr.arrayOrStruct);
      if (tokenizer->peekNext().type != TokenType::CLOSE_BRACKET) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
        return ParseExpressionErrorType::REPORTED;
      }
      tokenizer->consumePeek();
    } else {
      errorType = parseExpression(list->curr);
    }
    if (errorType != ParseExpressionErrorType::NONE) {
      if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
        expected.emplace_back(ExpectedType::TOKEN, errorToken, TokenType::COMMA, tokenizer->tokenizerIndex);
      } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
        expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
      }
      return ParseExpressionErrorType::REPORTED;
    }
    if (tokenizer->peekNext().type != TokenType::COMMA) {
      return ParseExpressionErrorType::NONE;
    }
    tokenizer->consumePeek();
    list->next = memPool.makeExpressionList();
    list = list->next;
  }
}

/**
 * Parses a complete expression until it reaches something else, placing the root expression in rootExpression
 * Consumes the entire expression unless there was an error
*/
ParseExpressionErrorType Parser::parseExpression(Expression& rootExpression) {
  Expression *bottom = nullptr;
  Token token = tokenizer->peekNext();
  while (true) {
    bool binary = isBinaryOp(token.type);
    if (binary || isUnaryOp(token.type)) {
      tokenizer->consumePeek();
      Expression expression;
      if (binary) {
        expression.type = ExpressionType::BINARY_OP;
        expression.binOp = memPool.makeBinOp(BinOp{token});
        if (!bottom) {
          // expected expression
          expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
          rootExpression = expression;
          bottom = &rootExpression;
          tokenizer->consumePeek();
          token = tokenizer->peekNext();
          continue;
        } else if (bottom->type == ExpressionType::BINARY_OP || bottom->type == ExpressionType::UNARY_OP) {
          expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
          return ParseExpressionErrorType::REPORTED;
        }
      }
      else {
        expression.type = ExpressionType::UNARY_OP;
        expression.unOp = memPool.makeUnOp(UnOp{token});
        if (!bottom) {
          if (token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) {
            // expected expression
            expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
          }
          rootExpression = expression;
          bottom = &rootExpression;
          tokenizer->consumePeek();
          token = tokenizer->peekNext();
          continue;
        } else if (token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) {
          if (bottom->type == ExpressionType::BINARY_OP || bottom->type == ExpressionType::UNARY_OP) {
            // expected expression
            expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
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
          if (operatorPrecedence[(uint_fast8_t)token.type] <= operatorPrecedence[(uint_fast8_t)op]) {
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
            bottom = &rootExpression;
          } else {
            expression.unOp->operand = rootExpression;
            bottom = &expression.unOp->operand;
          }
          // move the statement to the root
          rootExpression = expression;
        }
      }
    }
    else {
      Expression expression;
      if (isLiteral(token.type)) {
        tokenizer->consumePeek();
        expression.type = ExpressionType::VALUE;
        expression.value = token;
      }
      else if (token.type == TokenType::OPEN_PAREN) {
        tokenizer->consumePeek();
        expression.type = ExpressionType::WRAPPED;
        expression.wrapped = memPool.makeExpression();
        ParseExpressionErrorType errorType = parseExpression(*expression.wrapped);
        if (errorType != ParseExpressionErrorType::NONE) {
          if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
            expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_PAREN, errorToken, tokenizer->tokenizerIndex);
          } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
            expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
          }
          return ParseExpressionErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::CLOSE_PAREN) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
          return ParseExpressionErrorType::REPORTED;
        }
        tokenizer->consumePeek();
      }
      else if (token.type == TokenType::IDENTIFIER) {
        tokenizer->consumePeek();
        Token next = tokenizer->peekNext();
        if (next.type == TokenType::OPEN_PAREN) {
          tokenizer->consumePeek();
          expression.type = ExpressionType::FUNCTION_CALL;
          expression.funcCall = memPool.makeFunctionCall(FunctionCall{token});
          ParseExpressionErrorType errorType = getExpressions(expression.funcCall->args, TokenType::CLOSE_PAREN);
          if (errorType != ParseExpressionErrorType::NONE) {
            if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
              expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_PAREN_OR_COMMA, errorToken, tokenizer->tokenizerIndex);
            } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
              expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
            }
            return ParseExpressionErrorType::REPORTED;
          }
          if (tokenizer->peekNext().type != TokenType::CLOSE_PAREN) {
            if (tokenizer->peeked.type == TokenType::COMMA) {
              expected.emplace_back(ExpectedType::EXPRESSION, tokenizer->peeked, tokenizer->tokenizerIndex);
            } else {
              expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
            }
            return ParseExpressionErrorType::REPORTED;
          }
          tokenizer->consumePeek();
        }
        else if (next.type == TokenType::OPEN_BRACKET) {
          tokenizer->consumePeek();
          expression.type = ExpressionType::ARRAY_ACCESS;
          expression.arrAccess = memPool.makeArrayAccess(ArrayAccess{token});
          ParseExpressionErrorType errorType = parseExpression(expression.arrAccess->offset);
          if (errorType != ParseExpressionErrorType::NONE) {
            if (errorType == ParseExpressionErrorType::EXPRESSION_AFTER_EXPRESSION) {
              expected.emplace_back(ExpectedType::OPERATOR_OR_CLOSE_BRACKET, errorToken, tokenizer->tokenizerIndex);
            } else if (errorType == ParseExpressionErrorType::NOT_EXPRESSION) {
              expected.emplace_back(ExpectedType::EXPRESSION, errorToken, tokenizer->tokenizerIndex);
            }
            return ParseExpressionErrorType::REPORTED;
          }
          if (tokenizer->peekNext().type != TokenType::CLOSE_BRACKET) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
            return ParseExpressionErrorType::REPORTED;
          }
          tokenizer->consumePeek();
        }
        else {
          expression.type = ExpressionType::VALUE;
          expression.value = token;
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
    token = tokenizer->peekNext();
  }
  if (bottom) {
    if (bottom->type == ExpressionType::BINARY_OP) {
      if (bottom->binOp->rightSide.type == ExpressionType::NONE) {
        expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
        return ParseExpressionErrorType::REPORTED;
      }
      return ParseExpressionErrorType::NONE;
    } else if (bottom->type == ExpressionType::UNARY_OP) {
      if (bottom->unOp->operand.type == ExpressionType::NONE) {
        expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
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
  Token tp = tokenizer->peekNext();
  TokenList *curr = memPool.makeTokenList();
  if (isConcreteType(tp.type)) {
    tokenizer->consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.makeTokenList();
    curr->next = prev;
    tp = tokenizer->peekNext();
  } else {
    expected.emplace_back(ExpectedType::TOKEN, tp, TokenType::TYPE, tokenizer->tokenizerIndex);
    return ParseTypeErrorType::REPORTED;
  }
  while (tp.type != TokenType::END_OF_FILE) {
    if (tp.type < TokenType::POINTER) {
      break;
    }
    tokenizer->consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.makeTokenList();
    curr->next = prev;
    tp = tokenizer->peekNext();
  }
  type.token = curr->next->token;
  type.next = curr->next->next;
  memPool.release(curr);
  return ParseTypeErrorType::NONE;
}
