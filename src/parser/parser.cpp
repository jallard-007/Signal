#include <cassert>
#include "parser.hpp"

TokenType closingOfOpening(TokenType);

Unexpected::Unexpected(const Token& token, uint32_t tkIndex): token{token}, tkIndex{tkIndex} {}
std::string Unexpected::getErrorMessage(std::vector<Tokenizer>& tks) {
  auto& tk = tks[tkIndex];
  TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
  std::string message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
  if (unexpectedType == UnexpectedType::STRUCT_MEMBER_INITIALIZE_UNSUPPORTED) {
    return message + "Struct member initialization is not supported\n\n";
  }
  return message + "Unexpected Token: " + tk.extractToken(token) + "\n\n";
}

Expected::Expected(ExpectedType exType, const Token& token, uint32_t tkIndex): tokenWhereExpected{token}, tkIndex{tkIndex}, expectedTokenType{TokenType::NONE}, expectedType{exType} {}
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

#if defined(_MSC_VER)
struct Construct { Construct(void (*f)(void)) { f(); } };
#define constructor(fn) \
void fn(void); static Construct constructor_##fn(fn);
#elif defined(__GNUC__)
#define constructor(fn) \
__attribute__((constructor))
#endif

// TokenType::NEGATIVE is the "largest" operator token type with an enum value of 83, hence size 84
uint8_t operatorPrecedence [84]{};
constructor(initializeOperatorPrecedence)
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
  tokenizer{&tokenizer}, memPool{memPool}, errorToken{0,0,TokenType::NONE} {}

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
  else if (token.type == TokenType::BUILTIN) {
    if (!parseBuiltin(globalList->curr)) {
      return nullptr;
    }
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

bool Parser::parseBuiltin(GeneralDec&) {
  Token token = tokenizer->tokenizeNext();
  if (token.type == TokenType::FUNC) {
    token = tokenizer->tokenizeNext();
    globalList->curr.type = GeneralDecType::BUILTIN_FUNCTION;
    globalList->curr.builtinFunc = memPool.makeBuiltinFunc(token);
    if (!parseFunctionHeader(globalList->curr.builtinFunc->funcDec)) {
      return false;
    }
    token = tokenizer->tokenizeNext();
    if (token.getType() != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
      return false;
    }
  }
  else {
    unexpected.emplace_back(token, tokenizer->tokenizerIndex);
    return false;
  }
  return true;
}

bool Parser::parseFunctionHeader(FunctionDec& dec) {
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
  return true;
}


bool Parser::parseFunction(FunctionDec& dec) {
  if (!parseFunctionHeader(dec)) {
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
          return false;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
          return false;
        }
        if (list->varDec->initialAssignment) {
          // not supported yet
          unexpected.emplace_back(tokenizer->peeked, tokenizer->tokenizerIndex);
          unexpected.back().unexpectedType = UnexpectedType::STRUCT_MEMBER_INITIALIZE_UNSUPPORTED;
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
    unexpected.emplace_back(token, tokenizer->tokenizerIndex);
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
      if (parseBranchStatement(cond.ifStatement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }

      ElifStatementList **curr = &cond.elifStatement;
      while (tokenizer->peekNext().type == TokenType::ELIF) {
        tokenizer->consumePeek();
        *curr = memPool.makeElifStatementList();
        if (parseBranchStatement((*curr)->elif) == ParseStatementErrorType::REPORTED) {
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
      if (parseBranchStatement(statement.controlFlow->whileLoop->statement) == ParseStatementErrorType::REPORTED) {
        return ParseStatementErrorType::REPORTED;
      }
    }
    else if (token.type == TokenType::RETURN) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::RETURN_STATEMENT;
      statement.controlFlow->returnStatement = memPool.makeReturnStatement();
      auto& returnValue = statement.controlFlow->returnStatement->returnValue;
      // parses struct literals, not currently supported
      // if (tokenizer->peekNext().type == TokenType::OPEN_BRACE) {
      //   tokenizer->consumePeek();
      //   returnValue.setType(ExpressionType::STRUCT_LITERAL);
      //   returnValue.setArrayOrStructLiteral(memPool.makeArrayOrStruct());
      //   ParseExpressionErrorType errorType = parseArrayOrStructLiteral(*returnValue.getArrayOrStructLiteral(), TokenType::CLOSE_BRACE);
      //   if (errorType != ParseExpressionErrorType::NONE) {
      //     return ParseStatementErrorType::REPORTED;
      //   }
      //   if (tokenizer->peekNext().type != TokenType::CLOSE_BRACE) {
      //     expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
      //     return ParseStatementErrorType::REPORTED;
      //   }
      // } else
      if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
        ParseExpressionErrorType errorType = parseExpression(returnValue);
        if (errorType != ParseExpressionErrorType::NONE) {
          return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
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
        ParseExpressionErrorType errorType = parseExpression(forLoop->statement.condition);
        if (errorType != ParseExpressionErrorType::NONE) {
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
      ParseStatementErrorType errorType = parseScope(forLoop->statement.body.scopeStatements);
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
    else if (token.type == TokenType::EXIT) {
      tokenizer->consumePeek();
      statement.controlFlow->type = ControlFlowStatementType::EXIT_STATEMENT;
      statement.controlFlow->returnStatement = memPool.makeReturnStatement();
      auto& exitValue = statement.controlFlow->returnStatement->returnValue;
      ParseExpressionErrorType errorType = parseExpression(exitValue);
      if (errorType != ParseExpressionErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
      }
      if (tokenizer->peekNext().type != TokenType::SEMICOLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peekNext(), TokenType::SEMICOLON, tokenizer->tokenizerIndex);
        return ParseStatementErrorType::REPORTED;
      }
      tokenizer->consumePeek();
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
    // this parses structs and array literals, but they are not currently supported
    // initialize
    // if (tokenizer->peekNext().type == TokenType::OPEN_BRACKET || tokenizer->peeked.type == TokenType::OPEN_BRACE) {
    //   TokenType closing = closingOfOpening(tokenizer->peeked.type);
    //   tokenizer->consumePeek();
    //   if (closing == TokenType::CLOSE_BRACE) {
    //     varDec.initialAssignment->setType(ExpressionType::STRUCT_LITERAL);
    //   } else {
    //     varDec.initialAssignment->setType(ExpressionType::ARRAY_LITERAL);
    //   }
    //   varDec.initialAssignment->setArrayOrStructLiteral(memPool.makeArrayOrStruct());
    //   ParseExpressionErrorType errorType = parseArrayOrStructLiteral(*varDec.initialAssignment->getArrayOrStructLiteral(), closing);
    //   if (tokenizer->peekNext().type != closing) {
    //     expected.emplace_back(ExpectedType::TOKEN, errorToken, closing, tokenizer->tokenizerIndex);
    //     return ParseStatementErrorType::REPORTED;
    //   }
    //   tokenizer->consumePeek();
    //   if (errorType != ParseExpressionErrorType::NONE) {
    //     return ParseStatementErrorType::REPORTED;
    //   }
    // } else {
    //   varDec.initialAssignment = memPool.makeExpression();
    //   ParseExpressionErrorType errorType = parseExpression(*varDec.initialAssignment);
    //   if (errorType != ParseExpressionErrorType::NONE) {
    //     return ParseStatementErrorType::REPORTED;
    //   }
    // }
    varDec.initialAssignment = memPool.makeExpression();
    ParseExpressionErrorType errorType = parseExpression(*varDec.initialAssignment);
    if (errorType != ParseExpressionErrorType::NONE) {
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
  tokenizer->peeked.type = TokenType::NONE;
  ParseExpressionErrorType errorType = parseExpression(*statement.expression);
  if (errorType != ParseExpressionErrorType::NONE) {
    return ParseStatementErrorType::REPORTED;
  }
  return ParseStatementErrorType::NONE;
}

/**
 * Parses if statements
 * \note this includes if, elif, and while statements
*/
ParseStatementErrorType Parser::parseBranchStatement(BranchStatement& condStatement) {
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

TokenType closingOfOpening(TokenType type) {
  assert(type == TokenType::OPEN_BRACE || type == TokenType::OPEN_BRACKET || type == TokenType::OPEN_PAREN);
  #define DIFF_TO_MATCHING_CLOSE ((uint8_t)TokenType::CLOSE_BRACKET - (uint8_t)TokenType::OPEN_BRACKET)
  assert((uint8_t)TokenType::CLOSE_BRACE - (uint8_t)TokenType::OPEN_BRACE == DIFF_TO_MATCHING_CLOSE);
  assert((uint8_t)TokenType::CLOSE_PAREN - (uint8_t)TokenType::OPEN_PAREN == DIFF_TO_MATCHING_CLOSE);
  return (TokenType)((uint8_t)type + DIFF_TO_MATCHING_CLOSE);
  #undef DIFF_TO_MATCHING_CLOSE
}

/**
 * the open bracket should be consumed before calling this. does not consume the final close bracket, similar to getExpressions
 * actual expressions cannot have an array/struct literal since it only really makes sense to initialize with / return them, so this is not part of parseExpression
*/
ParseExpressionErrorType Parser::parseArrayOrStructLiteral(ArrayOrStructLiteral& arrayOrStruct, TokenType closing) {
  if (tokenizer->peekNext().type == closing) {
    return ParseExpressionErrorType::NONE;
  }
  ExpressionList *list = &arrayOrStruct.values;
  while (true) {
    ParseExpressionErrorType errorType;
    // this parses struct literals, but it's not currently supported
    // if (tokenizer->peekNext().type == TokenType::OPEN_BRACE) {
    //   tokenizer->consumePeek();
    //   list->curr.setType(ExpressionType::STRUCT_LITERAL);
    //   list->curr.setArrayOrStructLiteral(memPool.makeArrayOrStruct());
    //   errorType = parseArrayOrStructLiteral(*list->curr.getArrayOrStructLiteral(), TokenType::CLOSE_BRACE);
    //   if (tokenizer->peekNext().type != TokenType::CLOSE_BRACE) {
    //     expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
    //     return ParseExpressionErrorType::REPORTED;
    //   }
    //   tokenizer->consumePeek();
    // } else {
    //   errorType = parseExpression(list->curr);
    // }
    errorType = parseExpression(list->curr);
    if (errorType != ParseExpressionErrorType::NONE) {
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
  // ensures each part of the BinOp and UnOp lines
  assert(&((BinOp *)nullptr)->rightSide == &((UnOp *)nullptr)->operand);
  assert(&((BinOp *)nullptr)->op == &((UnOp *)nullptr)->op);
  Token token = tokenizer->peekNext();
  // bottom should always point to the leaf node
  Expression *bottom = &rootExpression;
  while (true) {
    if (!isUnaryOp(token.type)) {
      ParseExpressionErrorType errorType = parseLeaf(*bottom);
      if (errorType != ParseExpressionErrorType::NONE) {
        return errorType;
      }
      token = tokenizer->peekNext();
    }
    OPERATOR_PARSE:
    const ExpressionType expressionType = isBinaryOp(token.type) ? ExpressionType::BINARY_OP : isUnaryOp(token.type) ? ExpressionType::UNARY_OP : ExpressionType::NONE;
    if (expressionType == ExpressionType::NONE) {
      return ParseExpressionErrorType::NONE;
    }
    tokenizer->consumePeek();
    // place expression in right spot within tree based on precedence
    Expression* curr = &rootExpression;
    // iterate down and to the right of the tree until we find precedence match
    while (curr != bottom) { // this condition checks that curr has not reached the last leaf node
      // reliant on BinOp.op and UnOp.op being the same spot in their structs (assertion at start)
      if (operatorPrecedence[(uint8_t)token.type] <= operatorPrecedence[(uint8_t)curr->getBinOp()->op.type]) {
        break;
      }
      // reliant on BinOp.rightSide and UnOp.operand being the same spot in their structs (assertion at start)
      curr = &curr->getBinOp()->rightSide;
    }
    if (expressionType == ExpressionType::BINARY_OP) {
      BinOp *binOp = memPool.makeBinOp(BinOp{token});
      // set bottom to next empty expression
      bottom = &binOp->rightSide;
      // copy curr into this binary op
      binOp->leftSide = *curr;
      // change curr to point to the new binary op
      curr->setBinOp(binOp);
      curr->setType(expressionType);
    } else {
      UnOp *unOp = memPool.makeUnOp(UnOp{token});
      // set bottom to next empty expression
      bottom = &unOp->operand;
      // have to consider postfix operators
      if (token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) {
        // copy curr into this unary op
        unOp->operand = *curr;
        // change curr to point to the new unary op
        curr->setUnOp(unOp);
        curr->setType(expressionType);
        token = tokenizer->peekNext();
        /* have to do a jump since postfix operators
        are followed by an operator or end the expression.
        bottom is already used up */
        goto OPERATOR_PARSE;
      } else {
        // we dont need to copy curr since curr is empty
        // change curr to point to the new unary op
        curr->setUnOp(unOp);
        curr->setType(expressionType);
      }
    }
    token = tokenizer->peekNext();
  }
  return ParseExpressionErrorType::NONE;
}

ParseExpressionErrorType Parser::parseLeaf(Expression& expression) {
  const Token token = tokenizer->peekNext();
  if (isLiteral(token.type)) {
    tokenizer->consumePeek();
    expression.setType(ExpressionType::VALUE);
    expression.setToken(token);
  }
  else if (token.type == TokenType::OPEN_PAREN) {
    tokenizer->consumePeek();
    ParseExpressionErrorType errorType = parseExpression(expression);
    if (errorType != ParseExpressionErrorType::NONE) {
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
      expression.setType(ExpressionType::FUNCTION_CALL);
      expression.setFunctionCall(memPool.makeFunctionCall(FunctionCall{token}));
      ParseExpressionErrorType errorType = getExpressions(expression.getFunctionCall()->args, TokenType::CLOSE_PAREN);
      if (errorType != ParseExpressionErrorType::NONE) {
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
      expression.setType(ExpressionType::ARRAY_ACCESS);
      expression.setArrayAccess(memPool.makeArrayAccess(ArrayAccess{token}));
      ParseExpressionErrorType errorType = parseExpression(expression.getArrayAccess()->offset);
      if (errorType != ParseExpressionErrorType::NONE) {
        return ParseExpressionErrorType::REPORTED;
      }
      if (tokenizer->peekNext().type != TokenType::CLOSE_BRACKET) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
        return ParseExpressionErrorType::REPORTED;
      }
      tokenizer->consumePeek();
    }
    else {
      expression.setType(ExpressionType::VALUE);
      expression.setToken(token);
    }
  }
  else {
    expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
    return ParseExpressionErrorType::REPORTED;
  }
  return ParseExpressionErrorType::NONE;
}

/**
 * Returns the next token after the type list, adding tokens to type as it goes. tokens are in reverse order
 * Does NOT consume the final token
*/
ParseTypeErrorType Parser::getType(TokenList& type) {
  Token tp = tokenizer->peekNext();
  TokenList *curr = memPool.makeTokenList();
  if (!isBuiltInType(tp.type) && !isTypeQualifier(tp.getType()) && tp.getType() != TokenType::REFERENCE && tp.getType() != TokenType::IDENTIFIER) {
    expected.emplace_back(ExpectedType::TOKEN, tp, TokenType::TYPE, tokenizer->tokenizerIndex);
    return ParseTypeErrorType::REPORTED;
  }
  {
    tokenizer->consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.makeTokenList();
    curr->next = prev;
    tp = tokenizer->peekNext();
  }
  while (tp.type != TokenType::END_OF_FILE) {
    if (!isBuiltInType(tp.type) && !isTypeQualifier(tp.getType()) && tp.getType() != TokenType::REFERENCE && tp.getType() != TokenType::IDENTIFIER) {
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
