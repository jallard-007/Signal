#include "parser.hpp"
#include <memory>
#include <iostream>

Parser::Parser(Tokenizer& tokenizer, NodeMemPool& memPool): tokenizer{tokenizer}, memPool{memPool} {}

Parser::~Parser() {
  memPool.reset();
}

bool Parser::parse() {
  Token token = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    switch (token.type) {
      case TokenType::INCLUDE: {
        tokenizer.consumePeek();
        break;
      }

      case TokenType::FUNC: {
        tokenizer.consumePeek();
        auto& funcDec = program.decs.emplace_back();
        if (!functionDec(funcDec)) {
          program.decs.pop_back();
          return false;
        }
        break;
      }

      case TokenType::STRUCT: {
        tokenizer.consumePeek();
        auto& sDec = program.decs.emplace_back();
        if (!structDec(sDec)) {
          program.decs.pop_back();
          return false;
        }
        break;
      }

      case TokenType::TEMPLATE: {
        tokenizer.consumePeek();
        auto& tempDec = program.decs.emplace_back();
        if (!templateDec(tempDec)) {
          program.decs.pop_back();
          return false;
        }
        break;
      }

      case TokenType::CREATE: {
        tokenizer.consumePeek();
        // template creation
        break;
      }

      case TokenType::ENUM: {
        tokenizer.consumePeek();

        auto& r = program.decs.emplace_back(memPool.get(Enum{}));
        if (tokenizer.peekNext().type == TokenType::IDENTIFIER) {
          r.enm->name = tokenizer.peeked;
          tokenizer.consumePeek();
        }

        if (tokenizer.peekNext().type != TokenType::OPEN_BRACE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::OPEN_BRACE);
        }
        tokenizer.consumePeek();
        token = tokenizer.peekNext();
        while (tokenizer.peeked.type != TokenType::CLOSE_BRACE) {
          if (token.type != TokenType::IDENTIFIER) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::OPEN_BRACE);
          }
          if (token.type == TokenType::IDENTIFIER) {
            r.enm->members.emplace_back(token);
            tokenizer.consumePeek();
          } else {
            unexpected.emplace_back(token, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart);
            program.decs.pop_back();
            return false;
          }
          if (tokenizer.peekNext().type == TokenType::COMMA) {
            tokenizer.consumePeek();
          }
        }
        break;
      }

      case TokenType::IDENTIFIER: {
        // global variable declaration
        tokenizer.consumePeek();
        if (tokenizer.peekNext().type != TokenType::COLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::COLON);
          return false;
        }
        tokenizer.consumePeek();
        VariableDec ac{token};
        if (getType(ac.type).type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::SEMICOLON);
          return false;
        }
        tokenizer.consumePeek();
        if (ac.type.tokens.curr.type == TokenType::NOTHING) {
          expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart);
          return false;
        }
        program.decs.emplace_back(memPool.get(std::move(ac)));
        break;
      }
        
      default:
        // error, unexpected token
        unexpected.emplace_back(token, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart);
        return false;
    }
    token = tokenizer.peekNext();
  }
  return true;
}

bool Parser::functionDec(Declaration& dec) {
  const Token token = tokenizer.tokenizeNext();
  if (token.type != TokenType::IDENTIFIER) {
    // expected identifier
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart, TokenType::IDENTIFIER);
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
    // expected open paren
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position + 1 - tokenizer.lineStart, TokenType::OPEN_PAREN);
    return false;
  }
  FunctionDec funDec{token};
  // get parameters
  tokenizer.consumePeek();
  if (!getStatements(funDec.params, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
    if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_PAREN);
      return false;
    }
    else if (tokenizer.prevType == TokenType::BAD_VALUE) {
      return false;
    }
  } else {
    // consume close paren
    tokenizer.consumePeek();
  }
  if (tokenizer.peekNext().type != TokenType::COLON) {
    // expected a colon
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position + 1 - tokenizer.lineStart, TokenType::COLON);
    return false;
  }
  tokenizer.consumePeek();
  // get return type
  if (getType(funDec.returnType).type != TokenType::OPEN_BRACE) {
    // expected open brace after type
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position + 1 - tokenizer.lineStart, TokenType::OPEN_BRACE);
    return false;
  }
  // consume open brace
  tokenizer.consumePeek();
  if (!getStatements(funDec.body.scopeStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE)) {
    if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_BRACE);
      return false;
    }
    else if (tokenizer.prevType == TokenType::BAD_VALUE) {
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
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart, TokenType::IDENTIFIER);
    return false;
  }
  if (tokenizer.tokenizeNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart, TokenType::OPEN_BRACE);
    return false;
  }
  // now parse struct body
  // can consist of variables and functions at the top level, nothing else
  Struct sDec{token};
  token = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    // some statement
    if (token.type == TokenType::IDENTIFIER) {
      tokenizer.consumePeek();
      if (tokenizer.peekNext().type != TokenType::COLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::COLON);
        return false;
      }
      tokenizer.consumePeek();
      VariableDec ac{token};
      if (getType(ac.type).type != TokenType::SEMICOLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::SEMICOLON);
        return false;
      }
      tokenizer.consumePeek();
      if (ac.type.tokens.curr.type == TokenType::NOTHING) {
        expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart);
      }
      sDec.decs.emplace_back(memPool.get(std::move(ac)));
    }
    // function dec
    else if (token.type == TokenType::FUNC) {
      auto& funcDec = sDec.decs.emplace_back();
      tokenizer.consumePeek();
      if (!functionDec(funcDec)) {
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
      unexpected.emplace_back(token, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart);
      return false;
    }
    token = tokenizer.peekNext();
  }
  return false;
}

bool Parser::templateDec(Declaration& dec) {
  Token token = tokenizer.tokenizeNext();
  if (token.type != TokenType::OPEN_BRACKET) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart, TokenType::OPEN_BRACKET);
    return false;
  }
  Template temp;
  if (!getStatements(temp.templateIdentifiers, TokenType::COMMA, TokenType::CLOSE_BRACKET)) {
    if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_BRACKET);
      return false;
    }
    else if (tokenizer.prevType == TokenType::BAD_VALUE) {
      return false;
    }
  } else {
    // consume close_bracket
    tokenizer.consumePeek();
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
    unexpected.emplace_back(token, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart);
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
  if (token.type == TokenType::BAD_VALUE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.error.position + 1 - tokenizer.lineStart, tokenizer.error.type);
    return rootStatement;
  }
  if (token.type == delimiterA || token.type == delimiterB) {
    return rootStatement;
  }

  Statement *bottom = nullptr;
  uint32_t lineNum = tokenizer.lineNum, lineStart = tokenizer.lineStart;
  tokenizer.consumePeek();
  Token lookAhead = tokenizer.peekNext();
  if (lookAhead.type == TokenType::BAD_VALUE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.error.position + 1 - tokenizer.lineStart, tokenizer.error.type);
    return rootStatement;
  }
  while (token.type != TokenType::END_OF_FILE) {

    if (isBinaryOp(token.type) || isUnaryOp(token.type)) {
      Statement statement;
      if (isBinaryOp(token.type)) {
        statement.type = StatementType::BINARY_OP;
        statement.binOp = memPool.get(BinOp{token.type});
        if (!bottom) {
          expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
        }
      } else {
        statement.type = StatementType::UNARY_OP;
        statement.unOp = memPool.get(UnOp{token.type});
      }
      if (!bottom) {
        // expected expression before binary operator
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      }
      
      else {
        const StatementType sType = rootStatement.type;
        if (sType == StatementType::BAD || sType == StatementType::NONE) {
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
              op = list_iter->binOp->op;
              next = &list_iter->binOp->rightSide;
            } else if (list_iter->type == StatementType::UNARY_OP) {
              op = list_iter->unOp->op;
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

                // DUP CODE: moving child node
                if (!childOfPrev ) {
                  // expected token between operators
                  if (statement.type == StatementType::BINARY_OP) {
                    expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
                  }
                } else {
                  if (statement.type == StatementType::BINARY_OP) {
                    statement.binOp->leftSide = std::move(*childOfPrev);
                  } else {
                    statement.unOp->operand = std::move(*childOfPrev);
                  }
                }
                // END DUP CODE

                if (prev->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
                  std::cerr << "theres a problem\n";
                  exit(1);
                }
                prev = nullptr;
              }
              
              // root node
              else {
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
                expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
              }
            } else {
              if (statement.type == StatementType::BINARY_OP) {
                statement.binOp->leftSide = std::move(*childOfPrev);
              } else {
                statement.unOp->operand = std::move(*childOfPrev);
              }
            }
            // END DUP CODE

            if (prev->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
              expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
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
          unexpected.emplace_back(token, lineNum, token.position + 1 - lineStart);
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
            expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_PAREN);
          }
          else if (tokenizer.prevType == TokenType::BAD_VALUE) {
            break;
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
        statement.varDec = memPool.get(VariableDec{token});
        statement.type = StatementType::VARIABLE_DEC;
        // consume the colon
        tokenizer.consumePeek();
        lookAhead = getType(statement.varDec->type);
        if (lookAhead.type == TokenType::END_OF_FILE) {
          // never reached a type delimiter
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart, delimiterB);
          break;
        } 
        // cant be keyword, unary op, 
        // can be binary op, delimiter
        else if (lookAhead.type != delimiterA && lookAhead.type != delimiterB && !isBinaryOp(lookAhead.type)) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart, delimiterA);
          break;
        }
      }

      // we are using the identifier's actual value
      else {
        statement.type = StatementType::VALUE;
        statement.var = token;
      }

      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else if (bottom->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
        break;
      }
      // END DUP CODE
    }

    else if (isLiteral(token.type)) {
      Statement statement{token};
      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else if (bottom->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
        break;
      }
      // END DUP CODE
    }

    else if (isKeyword(token.type)) {
      Statement statement;
      if (isKeywordWithBody(token.type)) {
        statement.keywBody = memPool.get(KeywordWithBody{token.type});
        statement.type = StatementType::KEY_W_BODY;
      }
      else if (token.type == TokenType::BREAK || token.type == TokenType::CONTINUE) {
        statement.type = StatementType::KEYWORD;
        statement.key = token.type;
        if (lookAhead.type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::SEMICOLON);
        }
      }
      else if (token.type == TokenType::RETURN) {
        statement.keywBody = memPool.get(KeywordWithBody{TokenType::RETURN});
        statement.type = StatementType::KEY_W_BODY;
        statement.keywBody->header = parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
        if (tokenizer.peekNext().type != TokenType::CLOSE_BRACE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::SEMICOLON);
          break;
        }
      }
      else {
        // unexpected
        unexpected.emplace_back(token, lineNum, token.position + 1 - lineStart);
      }
      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else if (bottom->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
        break;
      }
      // END DUP CODE
    }

    else if (token.type == TokenType::OPEN_PAREN) {
      Statement statement;
      if (rootStatement.type == StatementType::KEY_W_BODY && rootStatement.keywBody->keyword == TokenType::FOR) {
        statement.type = StatementType::FOR_LOOP_HEADER;
        statement.list = memPool.get(ForLoopHeader{});
        if (!getStatements(statement.list->list, TokenType::SEMICOLON, TokenType::CLOSE_PAREN)) {
          if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_PAREN);
          }
          else if (tokenizer.prevType == TokenType::BAD_VALUE) {
            break;
          }
        }
        lookAhead = tokenizer.peekNext();
      } else {
        statement.type = StatementType::WRAPPED_VALUE;
        statement.wrapped = memPool.get(parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_PAREN));
        lookAhead = tokenizer.peekNext();
        if (statement.wrapped->type == StatementType::NONE) {
          expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart);
        }
      }

      if (lookAhead.type != TokenType::CLOSE_PAREN) {
        // expected close paren
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart, TokenType::CLOSE_PAREN);
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
      } else if (bottom->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
        break;
      }
      // END DUP CODE
    }

    // scope
    else if (token.type == TokenType::OPEN_BRACE) {
      if (delimiterB != TokenType::CLOSE_BRACE) {
        // naughty naughty naughty
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterB);
        break;
      }
      Statement statement{memPool.get(Scope{})};
      if (!getStatements(statement.scope->scopeStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE)) {
        if (tokenizer.peeked.type == TokenType::END_OF_FILE) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_BRACE);
        }
        else if (tokenizer.prevType == TokenType::BAD_VALUE) {
          break;
        }
      } else {
        // consume the close_brace
        tokenizer.consumePeek();
      }

      // DUP CODE: adding leaf node to the tree by move
      if (!bottom) {
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      } else if (bottom->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
        break;
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
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::CLOSE_BRACKET);
        }
        else if (tokenizer.prevType == TokenType::BAD_VALUE) {
          break;
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
      } else if (bottom->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
        break;
      }
      // END DUP CODE
    }

    else if (token.type != TokenType::COMMENT && token.type != TokenType::SEMICOLON) {
      // unexpected, i guess...
      unexpected.emplace_back(token, lineNum, token.position + 1 - lineStart);
    }

    if (lookAhead.type == TokenType::SEMICOLON && delimiterA != TokenType::SEMICOLON && delimiterB != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, lineNum, lookAhead.position + 1 - lineStart, delimiterB);
      break;
    }
    
    if (lookAhead.type == delimiterA || lookAhead.type == delimiterB) {
      break;
    }
    
    token = tokenizer.tokenizeNext();
    lineNum = tokenizer.lineNum;
    lineStart = tokenizer.lineStart;
    lookAhead = tokenizer.peekNext();
    if (lookAhead.type == TokenType::BAD_VALUE) {
      expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.error.position + 1 - tokenizer.lineStart, tokenizer.error.type);
      break;
    }
  }

  if (bottom) {
    ExpectedType exType = bottom->isValid();
    if (exType != ExpectedType::NOTHING) {
      expected.emplace_back(exType, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart);
    }
  }

  return rootStatement;
}

/**
 * Extracts delimiterMinor delimited statements until it reaches the delimiterMajor delimiter
 * Returns true on a successful parse (when it reaches the delimiterMajor),
 *  false otherwise (EOF, or when the delimiterMajor was expected by the parser)
 * Does NOT consume the final delimiter, just peeks at it (tokenizer.peeked.type == delimiterMajor on true)
*/
bool Parser::getStatements(StatementList& statements, const TokenType delimiterMinor, const TokenType delimiterMajor) {
  Token token = tokenizer.peekNext();
  if (token.type == TokenType::BAD_VALUE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.error.position + 1 - tokenizer.lineStart, tokenizer.error.type);
    return false;
  }
  StatementList *prev = nullptr;
  StatementList *ptr = &statements;
  size_t prevExpectedSize = expected.size();
  while (token.type != TokenType::END_OF_FILE) {
    Statement s = parseStatement(delimiterMinor, delimiterMajor);
    if (s.type == StatementType::NONE) {
      if (delimiterMinor == TokenType::COMMA && token.type != delimiterMajor) {
        expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, token.position + 1 - tokenizer.lineStart);
      }
    } else {
      ptr->curr = std::move(s);
      ptr->next = memPool.getStatementList();
    }
    if (tokenizer.prevType == TokenType::BAD_VALUE) {
      break;
    }
    if (expected.size() > prevExpectedSize) {
      if (expected.back().tokenType == delimiterMajor) {
        break;
      }
      prevExpectedSize = expected.size();
    }
    token = tokenizer.peekNext();
    if (token.type == delimiterMajor) {
      if (ptr->next) {
        memPool.release(ptr->next);
        ptr->next = nullptr;
      }
      if (prev && ptr->curr.type == StatementType::NONE) {
        prev->next = nullptr;
        memPool.release(ptr);
      }
      return true;
    }
    prev = ptr;
    ptr = ptr->next;
    tokenizer.consumePeek();
  }
  if (ptr->next) {
    memPool.release(ptr->next);
    ptr->next = nullptr;
  }
  if (prev && ptr->curr.type == StatementType::NONE) {
    prev->next = nullptr;
    memPool.release(ptr);
  }
  return false;
}

/**
 * Returns the next token after the type list, adding tokens to Type as it goes
 * Does NOT consume the final token
*/
Token Parser::getType(Type& type) {
  Token tp = tokenizer.peekNext();
  TokenList * prev = nullptr;
  TokenList * ptr = &type.tokens;
  while (tp.type != TokenType::END_OF_FILE) {
    if (isTypeDelimiter(tp.type)) {
      if (ptr->next) {
        memPool.release(ptr->next);
        ptr->next = nullptr;
      }
      if (prev && ptr->curr.type == TokenType::NOTHING) {
        prev->next = nullptr;
        memPool.release(ptr);
      }
      break;
    }
    tokenizer.consumePeek();
    ptr->curr = tp;
    ptr->next = memPool.getTokenList();
    prev = ptr;
    ptr = ptr->next;
    tp = tokenizer.peekNext();
  }
  return tp;
}
