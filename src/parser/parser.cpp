#include "parser.hpp"
#include <memory>
#include <iostream>

Parser::Parser(Tokenizer& tokenizer): tokenizer{tokenizer} {
  program.name = "ProgName";
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

        auto& r = program.decs.emplace_back(std::make_unique<Enum>());
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
        program.decs.emplace_back(s.get(parseStatement(TokenType::NOTHING, TokenType::SEMICOLON)));
        if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::SEMICOLON);
        } else {
          tokenizer.consumePeek();
        }
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
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position - tokenizer.lineStart, TokenType::IDENTIFIER);
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
    // expected open paren
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position - tokenizer.lineStart, TokenType::OPEN_PAREN);
    return false;
  }
  FunctionDec funDec{token};
  // get parameters
  tokenizer.consumePeek();
  if (!getStatements(funDec.params, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position - tokenizer.lineStart, TokenType::CLOSE_PAREN);
    return false;
  }
  // consume close paren
  tokenizer.consumePeek();
  if (tokenizer.peekNext().type != TokenType::COLON) {
    // expected a colon
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position - tokenizer.lineStart, TokenType::COLON);
    return false;
  }
  tokenizer.consumePeek();
  // get return type
  if (getType(funDec.returnType).type != TokenType::OPEN_BRACE) {
    // expected open brace after type
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position - tokenizer.lineStart, TokenType::OPEN_BRACE);
    return false;
  }
  // consume open brace
  tokenizer.consumePeek();
  if (!getStatements(funDec.bodyStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE)) {
    return false;
  }

  // consume close brace
  tokenizer.consumePeek();
  dec.func = std::make_unique<FunctionDec>(std::move(funDec));
  dec.decType = DecType::FUNCTION;
  return true;
}

bool Parser::structDec(Declaration& dec) {
  Token token = tokenizer.tokenizeNext();
  if (token.type != TokenType::IDENTIFIER) {
    // expected identifier
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position - tokenizer.lineStart, TokenType::IDENTIFIER);
    return false;
  }
  if (tokenizer.tokenizeNext().type != TokenType::OPEN_BRACE) {
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, token.position - tokenizer.lineStart, TokenType::OPEN_BRACE);
    return false;
  }
  // now parse struct body
  // can consist of variables and functions at the top level, nothing else
  Struct sDec{token};
  token = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    // variable dec
    if (token.type == TokenType::IDENTIFIER) {
      sDec.decs.emplace_back(s.get(parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)));
      if (tokenizer.peekNext().type != TokenType::SEMICOLON) {
        return false;
      }
      tokenizer.consumePeek();
    }
    // function dec
    else if (token.type == TokenType::FUNC) {
      auto& funcDec = sDec.decs.emplace_back();
      tokenizer.consumePeek();
      if (!functionDec(funcDec)) {
        return false;
      }
    }
    // struct
    else if (token.type == TokenType::STRUCT) {
      auto& subSDec = sDec.decs.emplace_back();
      tokenizer.consumePeek();
      if (!structDec(subSDec)) {
        return false;
      }
    }
    // end of struct
    else if (token.type == TokenType::CLOSE_BRACE) {
      tokenizer.consumePeek();
      dec.decType = DecType::STRUCT;
      dec.struc = std::make_unique<Struct>(std::move(sDec));
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
    expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.position - tokenizer.lineStart, TokenType::CLOSE_BRACKET);
  }
  tokenizer.consumePeek();
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
  dec.temp = std::make_unique<Template>(std::move(temp));
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
    return rootStatement;
  }

  Statement *bottom = nullptr;
  uint32_t lineNum = tokenizer.lineNum, lineStart = tokenizer.lineStart;
  tokenizer.consumePeek();
  Token lookAhead = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {

    if (isBinaryOp(token.type) || isUnaryOp(token.type)) {
      Statement statement;
      if (isBinaryOp(token.type)) {
        statement.type = StatementType::BINARY_OP;
        statement.binOp = std::make_unique<BinOp>(token.type);
      } else {
        statement.type = StatementType::UNARY_OP;
        statement.unOp = std::make_unique<UnOp>(token.type);
      }
      if (!bottom) {
        // expected expression before binary operator
        expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
        rootStatement = std::move(statement);
        bottom = &rootStatement;
      }
      
      else {
        const StatementType sType = rootStatement.type;
        if (sType == StatementType::BAD || sType == StatementType::NONE) {
          // never will be a node of this type in the list
          exit(1);
        }
        
        else if (hasData(sType)) {
          // first "value" token in statement: take it
          // this is the only node in the list (list.size == 1)
          if (statement.type == StatementType::BINARY_OP) {
            statement.binOp->leftSide = s.get(std::move(rootStatement));
          } else {
            statement.unOp->operand = s.get(std::move(rootStatement));
          }
          // move the statement to the tree root
          rootStatement = std::move(statement);
        }
        
        else if (sType == StatementType::UNARY_OP || sType == StatementType::BINARY_OP) {
          Statement* prev = nullptr;
          Statement* list_iter = &rootStatement;
          while (list_iter) {
            Statement* next;
            TokenType op;
            if (list_iter->type == StatementType::BINARY_OP) {
              op = list_iter->binOp->op;
              next = list_iter->binOp->rightSide;
            } else if (list_iter->type == StatementType::UNARY_OP) {
              op = list_iter->unOp->op;
              next = list_iter->unOp->operand;
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
                Statement** childOfPrev = prev->getChild();

                // DUP CODE: moving child node
                if (!childOfPrev && statement.type == StatementType::BINARY_OP) {
                  // expected token between operators
                  expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
                } else {
                  if (statement.type == StatementType::BINARY_OP) {
                    statement.binOp->leftSide = std::move(prev->binOp->rightSide);
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
                  statement.binOp->leftSide = s.get(std::move(rootStatement));
                } else {
                  statement.unOp->operand = s.get(std::move(rootStatement));
                }
                // move the statement to the root
                rootStatement = std::move(statement);
              }
              
              // exit the list loop, we inserted the node
              break;
            }
            
            prev = list_iter;
            list_iter = next;
          }

          // we went through the whole list, so add it to the bottom
          if (!list_iter && prev) {
            Statement **childOfPrev = prev->getChild();
            
            // DUP CODE: moving child node
            if (!childOfPrev ) {
              // expected token between operators
              if (statement.type == StatementType::BINARY_OP) {
                expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
              }
            } else {
              if (statement.type == StatementType::BINARY_OP) {
                statement.binOp->leftSide = *childOfPrev;
              } else {
                statement.unOp->operand = *childOfPrev;
              }
            }
            // END DUP CODE

            if (prev->addStatementToNode(std::move(statement)) != ExpectedType::NOTHING) {
              expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
              break;
            } else {
              if (prev->type == StatementType::BINARY_OP) {
                bottom = prev->binOp->rightSide;
              } else {
                bottom = prev->unOp->operand;
              }
            }
          } else {
            unexpected.emplace_back(token, lineNum, token.position + 1 - lineStart);
          }
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
        statement.funcCall = std::make_unique<FunctionCall>(token);
        statement.type = StatementType::FUNCTION_CALL;
        tokenizer.tokenizeNext();
        if (!getStatements(statement.funcCall->args, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
          break;
        }
        // consume close paren
        tokenizer.consumePeek();
        lookAhead = tokenizer.peekNext();
      }

      // pointer access with offset: identifier [
      else if (lookAhead.type == TokenType::OPEN_BRACKET) {
        statement.arrAccess = std::make_unique<ArrayAccess>(token);
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
        statement.varDec = std::make_unique<VariableDec>(token);
        statement.type = StatementType::VARIABLE_DEC;
        // consume the colon
        tokenizer.consumePeek();
        lookAhead = getType(statement.varDec->type);
        if (lookAhead.type == TokenType::END_OF_FILE) {
          // never reached a type delimiter
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, lookAhead.position - tokenizer.lineStart, delimiterB);
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
        statement.keywBody = std::make_unique<KeywordWithBody>(token.type);
        statement.type = StatementType::KEY_W_BODY;
      }
      else if (token.type == TokenType::BREAK || token.type == TokenType::CONTINUE) {
        statement.type = StatementType::KEYWORD;
        statement.key = token.type;
        if (lookAhead.type != TokenType::SEMICOLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::SEMICOLON);
        }
      }
      else if (token.type == TokenType::DEFAULT) {
        if (lookAhead.type != TokenType::COLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::COLON);
        } else {
          tokenizer.consumePeek();
          lookAhead = tokenizer.peekNext();
        }
      }
      else if (token.type == TokenType::CASE) {
        Statement caseValue = parseStatement(TokenType::NOTHING, TokenType::COLON);
        lookAhead = tokenizer.peekNext();
        if (caseValue.type == StatementType::NONE) {
          expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart);
        }
        if (lookAhead.type != TokenType::COLON) {
          expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, tokenizer.peeked.position + 1 - tokenizer.lineStart, TokenType::COLON);
        } else {
          tokenizer.consumePeek();
          lookAhead = tokenizer.peekNext();
        }
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
      Statement inner = parseStatement(TokenType::NOTHING, TokenType::CLOSE_PAREN);
      lookAhead = tokenizer.peekNext();
      Statement statement;
      if (inner.type != StatementType::NONE) {
        statement.wrapped = s.get(std::move(inner));
        statement.type = StatementType::WRAPPED_VALUE;
      } else {
        expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart);
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
      if (lookAhead.type != TokenType::CLOSE_PAREN) {
        // expected close paren
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart, TokenType::CLOSE_PAREN);
        break;
      } else {
        // consume the close paren
        tokenizer.consumePeek();
        lookAhead = tokenizer.peekNext();
      }
    }

    // scope
    else if (token.type == TokenType::OPEN_BRACE) {
      if (delimiterB != TokenType::CLOSE_BRACE) {
        // naughty naughty naughty
        expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterB);
        break;
      }
      Statement statement{std::make_unique<Scope>()};
      getStatements(statement.scope->scopeStatements, TokenType::SEMICOLON, TokenType::CLOSE_BRACE);
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
      if (lookAhead.type != TokenType::CLOSE_BRACE) {
        // expected close brace
        expected.emplace_back(ExpectedType::TOKEN, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart, TokenType::CLOSE_BRACE);
        break;
      } else {
        // consume the close brace
        tokenizer.consumePeek();
        lookAhead = tokenizer.peekNext();
      }
    }

    // array literals, structs
    else if (token.type == TokenType::OPEN_BRACKET) {
    }

    else if (token.type == TokenType::SEMICOLON && delimiterA != TokenType::SEMICOLON && delimiterB != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position - lineStart, delimiterB);
      break;
    }
  
    else if (token.type != TokenType::COMMENT) {
      // unexpected, i guess...
      unexpected.emplace_back(token, lineNum, token.position - lineStart);
    }
    
    if (lookAhead.type == delimiterA || lookAhead.type == delimiterB) {
      break;
    }
    
    token = tokenizer.tokenizeNext();
    lineNum = tokenizer.lineNum;
    lineStart = tokenizer.lineStart;
    lookAhead = tokenizer.peekNext();
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
 * Returns true on a successful parse (when it reaches the delimiterMajor), false otherwise (EOF)
 * Does NOT consume the final delimiter, just peeks at it (tokenizer.peeked.type == delimiterMajor on true)
*/
bool Parser::getStatements(std::vector<Statement>& statements, const TokenType delimiterMinor, const TokenType delimiterMajor) {
  Token token = tokenizer.peekNext();
  size_t prevExpectedSize = expected.size();
  while (token.type != TokenType::END_OF_FILE) {
    Statement s = parseStatement(delimiterMinor, delimiterMajor);
    if (s.type != StatementType::NONE) {
      statements.emplace_back(std::move(s));
    }
    if (expected.size() > prevExpectedSize) {
      if (expected.back().tokenType == delimiterMajor) {
        break;
      }
      prevExpectedSize = expected.size();
    }
    token = tokenizer.peekNext();
    if (token.type == delimiterMajor) {
      return true;
    }
    tokenizer.consumePeek();
  }
  return false;
}

/**
 * Returns the next token after the type list, adding tokens to Type as it goes
 * Does NOT consume the final token
*/
Token Parser::getType(Type& type) {
  Token tp = tokenizer.peekNext();
  while (tp.type != TokenType::END_OF_FILE) {
    if (isTypeDelimiter(tp.type)) {
      break;
    }
    tokenizer.consumePeek();
    type.tokens.emplace_back(tp);
    tp = tokenizer.peekNext();
  }
  return tp;
}
