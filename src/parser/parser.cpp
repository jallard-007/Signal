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
  if (!getStatements(funDec.params, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
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
  if (!getStatements(funDec.body.scopeStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE)) {
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
 * Extracts tokens until it reaches the specified delimiter, forming a Statement.
 * Two delimiters can be specified to accommodate enclosed lists, leave one as NOTHING if only one is needed
 * Does NOT consume the delimiter, you must do this after 
*/
Statement Parser::parseStatement(TokenType delimiterA, const TokenType delimiterB) {
  if (delimiterA == TokenType::NOTHING) {
    delimiterA = delimiterB;
  }
  Statement rootStatement;
  Token token = tokenizer.peekNext();
  if (token.type == delimiterA || token.type == delimiterB) {
    token.type = TokenType::NOTHING;
    rootStatement.var = memPool.get(Token{token});
    return rootStatement;
  }

  Statement *bottom = nullptr;
  tokenizer.consumePeek();
  Token lookAhead = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {

    if (isBinaryOp(token.type) || isUnaryOp(token.type)) {
      Statement statement;
      if (isBinaryOp(token.type)) {
        statement.type = StatementType::BINARY_OP;
        statement.binOp = memPool.get(BinOp{token});
        if (!bottom) {
          expected.emplace_back(ExpectedType::EXPRESSION, token);
        }
      } else {
        statement.type = StatementType::UNARY_OP;
        statement.unOp = memPool.get(UnOp{token});
      }
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      }
      
      else {
        const StatementType sType = rootStatement.type;
        if (sType == StatementType::NONE) {
          // never will be a node of this type in the list
          exit(1);
        }
        
        else if (sType == StatementType::UNARY_OP || sType == StatementType::BINARY_OP) {
          Statement* prev = nullptr;
          Statement* list_iter = &rootStatement;
          do {
            Statement* next;
            TokenType op;
            if (list_iter->type == StatementType::BINARY_OP) {
              op = list_iter->binOp->op.type;
              next = &list_iter->binOp->rightSide;
            } else if (list_iter->type == StatementType::UNARY_OP) {
              op = list_iter->unOp->op.type;
              next = &list_iter->unOp->operand;
            } else {
              if (prev != bottom) {
                std::cerr << "Only \'other\' nodes should be at the bottom\n";
                std::cerr << "Statement Type: " << (int)list_iter->type << '\n';
                std::cerr << "Bottom Statement Type: " << (int)bottom->type << '\n';
                exit(1);
              }
              // cant insert the node
              list_iter = nullptr;
              break;
            }
            
            if (operatorPrecedence.at(token.type) <= operatorPrecedence.at(op)) {
              // place it between prev and current
              if (prev) {
                Statement* childOfPrev = prev->getChild();
                // if there is a previous, it means there is a child, right? no need to check for nullptr
                ExpectedType exType = childOfPrev->isValid();
                if (exType != ExpectedType::NOTHING) {
                  expected.emplace_back(exType, token, token.type);
                }
                if (statement.type == StatementType::BINARY_OP) {
                  statement.binOp->leftSide = std::move(*childOfPrev);
                } else {
                  statement.unOp->operand = std::move(*childOfPrev);
                }

                if (prev->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
                  std::cerr << "theres a problem\n";
                  exit(1);
                }
                prev = nullptr;
              }
              
              // root node
              else {
                ExpectedType exType = rootStatement.isValid();
                if (exType != ExpectedType::NOTHING) {
                  expected.emplace_back(exType, token, token.type);
                }
                if (statement.type == StatementType::BINARY_OP) {
                  statement.binOp->leftSide = std::move(rootStatement);
                } else {
                  statement.unOp->operand = std::move(rootStatement);
                }
                // move the statement to the root
                rootStatement = std::move(statement);
                bottom = &rootStatement;
              }
              
              // exit the list loop, we inserted the node
              break;
            }
            
            prev = list_iter;
            list_iter = next;
          } while (list_iter);

          // we went through the whole list, so add it to the bottom
          if (prev) {
            Statement *childOfPrev = prev->getChild();
            
            // DUP CODE: moving child node
            if (!childOfPrev ) {
              // expected token between operators
              if (statement.type == StatementType::BINARY_OP) {
                expected.emplace_back(ExpectedType::EXPRESSION, token);
              }
            } else {
              if (statement.type == StatementType::BINARY_OP) {
                statement.binOp->leftSide = std::move(*childOfPrev);
              } else {
                statement.unOp->operand = std::move(*childOfPrev);
              }
            }
            // END DUP CODE
            ExpectedType exType = prev->addStatementToNode(std::move(statement));
            if (exType != ExpectedType::NOTHING) {
              expected.emplace_back(exType, token, delimiterA);
              break;
            } else {
              if (prev->type == StatementType::BINARY_OP) {
                bottom = &prev->binOp->rightSide;
              } else {
                bottom = &prev->unOp->operand;
              }
            }
          }
        }
        
        else if (hasData(sType)) {
          // first "value" token in statement: take it
          // this is the only node in the list (list.size == 1)
          if (statement.type == StatementType::BINARY_OP) {
            statement.binOp->leftSide = std::move(rootStatement);
          } else {
            statement.unOp->operand = std::move(rootStatement);
          }
          // move the statement to the tree root
          rootStatement = std::move(statement);
        }

        else {
          unexpected.emplace_back(token);
        }
      }
    }

    else if (token.type == TokenType::IDENTIFIER) {
      Statement statement;

      // function call: identifier (
      if (lookAhead.type == TokenType::OPEN_PAREN) {
        statement.funcCall = memPool.get(FunctionCall{token});
        statement.type = StatementType::FUNCTION_CALL;
        // consume the open paren
        tokenizer.consumePeek();
        if (!getStatements(statement.funcCall->args, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
          if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
          }
        } else {
          // consume close_paren
          tokenizer.consumePeek();
        }
        lookAhead = tokenizer.peekNext();
      }

      // pointer access with offset: identifier [
      else if (lookAhead.type == TokenType::OPEN_BRACKET) {
        statement.arrAccess = memPool.get(ArrayAccess{token});
        statement.type = StatementType::ARRAY_ACCESS;
        tokenizer.tokenizeNext();
        statement.arrAccess->offset = parseStatement(TokenType::NOTHING, TokenType::CLOSE_BRACKET);
        lookAhead = tokenizer.peekNext();
        if (lookAhead.type == TokenType::CLOSE_BRACKET) {
          tokenizer.consumePeek();
        }
        lookAhead = tokenizer.peekNext();
      }

      // variable declaration: identifier :
      else if (lookAhead.type == TokenType::COLON && delimiterA != TokenType::COLON) {
        statement.dec = memPool.get(Declaration{memPool.get(VariableDec{token})});
        statement.type = StatementType::VARIABLE_DEC;
        // consume the colon
        tokenizer.consumePeek();
        lookAhead = getType(statement.dec->varDec->type);
        if (lookAhead.type == TokenType::END_OF_FILE) {
          // never reached a type delimiter
          expected.emplace_back(ExpectedType::TOKEN, lookAhead, delimiterB);
          break;
        }
        if (lookAhead.type == TokenType::ASSIGNMENT) {
          tokenizer.consumePeek();
          Token tkBefore = lookAhead;
          tkBefore.linePos += 1;
          statement.dec->varDec->initialAssignment = memPool.get(parseStatement(delimiterA, delimiterB));
          if (statement.dec->varDec->initialAssignment->type == StatementType::NONE) {
            expected.emplace_back(ExpectedType::EXPRESSION, tkBefore);
          } else if (!hasData(statement.dec->varDec->initialAssignment->type) && statement.dec->varDec->initialAssignment->type != StatementType::ARRAY_OR_STRUCT_LITERAL) {
            expected.emplace_back(ExpectedType::EXPRESSION, tkBefore);
            expected.emplace_back(ExpectedType::TOKEN, tkBefore, delimiterA);
          }
          lookAhead = tokenizer.peekNext();
        }
        if (lookAhead.type != TokenType::SEMICOLON && delimiterA == TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, lookAhead, delimiterA);
          break;
        }
        // cant be keyword, unary op,
        // can be binary op, delimiter
        else if (lookAhead.type != delimiterA && lookAhead.type != delimiterB && !isBinaryOp(lookAhead.type)) {
          unexpected.emplace_back(lookAhead);
          break;
        }
      }

      // we are using the identifier's actual value
      else {
        statement.type = StatementType::VALUE;
        statement.var = memPool.get(Token{token});
      }

      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else {
        ExpectedType exType = bottom->addStatementToNode(std::move(statement));
        if (exType != ExpectedType::NOTHING) {
          expected.emplace_back(exType, token, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    else if (isLiteral(token.type)) {
      Statement statement{memPool.get(Token{token})};
      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else {
        ExpectedType exType = bottom->addStatementToNode(std::move(statement));
        if (exType != ExpectedType::NOTHING) {
          expected.emplace_back(exType, token, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    else if (isKeyword(token.type)) {
      Statement statement;
      if (isKeywordWithBody(token.type)) {
        statement.keyWBody = memPool.get(KeywordWithBody{token});
        statement.type = StatementType::KEY_W_BODY;
      }
      else if (token.type == TokenType::BREAK || token.type == TokenType::CONTINUE) {
        statement.type = StatementType::KEYWORD;
        statement.key = token.type;
        if (lookAhead.type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, lookAhead, TokenType::SEMICOLON);
          break;
        }
      }
      else {
        // unexpected
        unexpected.emplace_back(token);
        break;
      }
      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else {
        ExpectedType exType = bottom->addStatementToNode(std::move(statement));
        if (exType != ExpectedType::NOTHING) {
          expected.emplace_back(exType, token, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    else if (token.type == TokenType::OPEN_PAREN) {
      Statement statement;
      if (rootStatement.type == StatementType::KEY_W_BODY && rootStatement.keyWBody->keyword.type == TokenType::FOR) {
        statement.type = StatementType::FOR_LOOP_HEADER;
        statement.list = memPool.get(ForLoopHeader{});
        if (!getStatements(statement.list->list, TokenType::SEMICOLON, TokenType::CLOSE_PAREN)) {
          if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_PAREN);
          }
        } else {
          lookAhead = tokenizer.peekNext();
        }
      }
      else {
        statement.type = StatementType::WRAPPED_VALUE;
        statement.wrapped = memPool.get(parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_PAREN));
        lookAhead = tokenizer.peekNext();
        if (statement.wrapped->type == StatementType::NONE) {
          expected.emplace_back(ExpectedType::EXPRESSION, lookAhead);
        }
      }

      if (lookAhead.type != TokenType::CLOSE_PAREN) {
        // expected close paren
        expected.emplace_back(ExpectedType::TOKEN, lookAhead, TokenType::CLOSE_PAREN);
        break;
      } else {
        // consume the close paren
        tokenizer.consumePeek();
      }

      lookAhead = tokenizer.peekNext();
      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else {
        ExpectedType exType = bottom->addStatementToNode(std::move(statement));
        if (exType != ExpectedType::NOTHING) {
          expected.emplace_back(exType, token, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    // scope
    else if (token.type == TokenType::OPEN_BRACE) {
      if (delimiterB != TokenType::CLOSE_BRACE) {
        // naughty naughty naughty
        unexpected.emplace_back(token);
        break;
      }
      Statement statement{memPool.get(Scope{})};
      if (!getStatements(statement.scope->scopeStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE)) {
        if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACE);
        }
      } else {
        // consume the close_brace
        tokenizer.consumePeek();
      }

      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else {
        ExpectedType exType = bottom->addStatementToNode(std::move(statement));
        if (exType != ExpectedType::NOTHING) {
          expected.emplace_back(exType, token, delimiterA);
          break;
        }
      }
      // END DUP CODE
      if ((rootStatement.type == StatementType::KEY_W_BODY || rootStatement.type == StatementType::SCOPE) && delimiterA == TokenType::SEMICOLON) {
        --tokenizer.position;
        tokenizer.peeked.type = TokenType::SEMICOLON;
        tokenizer.peeked.length = 1;
      }
      lookAhead = tokenizer.peekNext();
    }

    // array literals, structs
    else if (token.type == TokenType::OPEN_BRACKET) {
      Statement statement{memPool.get(ArrOrStructLiteral{})};
      statement.type = StatementType::ARRAY_OR_STRUCT_LITERAL;
      if (!getStatements(statement.arrOrStructLiteral->list, TokenType::COMMA, TokenType::CLOSE_BRACKET)) {
        if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.peeked, TokenType::CLOSE_BRACKET);
        }
      } else {
        // consume the close_bracket
        tokenizer.consumePeek();
      }
      lookAhead = tokenizer.peekNext();
      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else {
        ExpectedType exType = bottom->addStatementToNode(std::move(statement));
        if (exType != ExpectedType::NOTHING) {
          expected.emplace_back(exType, token, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    else {
      // unexpected, i guess...
      unexpected.emplace_back(token);
    }

    if (lookAhead.type == delimiterA || lookAhead.type == delimiterB) {
      break;
    }
    
    token = tokenizer.tokenizeNext();
    lookAhead = tokenizer.peekNext();
  }

  if (bottom) {
    ExpectedType exType = bottom->isValid();
    if (exType != ExpectedType::NOTHING) {
      expected.emplace_back(exType, lookAhead, delimiterA);
    }
  } else {
    if (rootStatement.type != StatementType::NOTHING) {
      exit(1);
    }
    lookAhead.type = TokenType::NOTHING;
    rootStatement.expression = memPool.get(Token{lookAhead});
  }
  return rootStatement;
}

/**
 * Extracts delimiterMinor delimited expressions until it reaches the delimiterMajor
 * Returns true on a successful parse (when it reaches the delimiterMajor),
 *  false otherwise (invalid expression, end of file, etc.)
 * Does NOT consume the final delimiter, just peeks at it (tokenizer.peeked.type == delimiterMajor on true)
*/
bool Parser::getExpressions(ExpressionList& expressions, TokenType delimiterMinor, TokenType delimiterMajor) {
  ExpressionList *prev = nullptr;
  ExpressionList *ptr = &expressions;
  Token token = tokenizer.peekNext();
  if (token.type == delimiterMajor) {
    return true;
  }
  while (token.type != TokenType::END_OF_FILE) {
    if (!parseExpression(ptr->curr)) {
      // invalid expression
      return false;
    }
    token = tokenizer.peekNext();
    if (token.type == delimiterMajor) {
      return true;
    } else if (token.type != delimiterMinor) {
      expected.emplace_back(ExpectedType::TOKEN, token, delimiterMajor);
      break;
    }
    ptr->next = memPool.getExpressionList();
    prev = ptr;
    ptr = ptr->next;
    tokenizer.consumePeek();

  }
  if (ptr) {
    if (delimiterMinor == TokenType::SEMICOLON && delimiterMajor == TokenType::CLOSE_BRACE && ptr->curr.type != StatementType::NONE) {
      expected.emplace_back(ExpectedType::TOKEN, token, TokenType::SEMICOLON);
    }
    if (ptr->next) {
      memPool.releaseExpressionList(ptr->next);
      ptr->next = nullptr;
    }
    if (ptr->curr.type == ExpressionType::NONE) {
      prev->next = nullptr;
      memPool.releaseExpressionList(ptr);
    }
  }
  return false;
}

bool Parser::parseExpression(Expression& rootExpression) {
  Expression *bottom = nullptr;
  Token tk = tokenizer.peekNext();
  while (1){

    bool binary = isBinaryOp(tk.type);
    if (binary || isUnaryOp(tk.type)) {
      Expression expression;
      if (binary) {
        expression.type = ExpressionType::BINARY_OP;
        expression.binOp = memPool.getBinOp(BinOp{tk});
        if (!bottom) {
          // expected expression
          expected.emplace_back(ExpectedType::EXPRESSION, tk);
          rootExpression = std::move(expression);
          bottom = &rootExpression;
          continue;
        }
      } else {
        expression.type = ExpressionType::UNARY_OP;
        expression.unOp = memPool.getUnOp(UnOp{tk});
        if (!bottom) {
          if (tk.type == TokenType::DECREMENT_POSTFIX || tk.type == TokenType::INCREMENT_POSTFIX) {
            // expected expression
            expected.emplace_back(ExpectedType::EXPRESSION, tk);
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
          if (operatorPrecedence.at(tk.type) <= operatorPrecedence.at(op)) {
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
    }


    switch(tk.type) {
      case TokenType::BINARY_NUMBER:
      case TokenType::HEX_NUMBER:
      case TokenType::DECIMAL_NUMBER: 
      case TokenType::IDENTIFIER: 
        break;

      default:
        break;


    }
  }
}

/**
 * Returns the next token after the type list, adding tokens to type as it goes. tokens are in reverse order
 * Does NOT consume the final token
*/
Token Parser::getType(TokenList& type) {
  Token tp = tokenizer.peekNext();
  TokenList *curr = &type;
  bool typeFound = false;
  while (tp.type != TokenType::END_OF_FILE) {
    if (isConcreteType(tp.type)) {
      if (typeFound) {
        break;
      }
      typeFound = true;
    } else if (tp.type < TokenType::POINTER) {
      break;
    }
    tokenizer.consumePeek();
    curr->token = tp;
    TokenList *prev = curr;
    curr = memPool.getTokenList();
    curr->next = prev;
    tp = tokenizer.peekNext();
  }
  if (!curr->next) {
    memPool.release(curr);
  } else {
    type.token = curr->next->token;
    type.next = curr->next->next;
    memPool.release(curr);
  }
  return tp;
}
