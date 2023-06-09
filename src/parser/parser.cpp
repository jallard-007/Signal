#include "parser.hpp"
#include <list>
#include <memory>
#include <iostream>

Parser::Parser(Tokenizer& tokenizer): tokenizer{tokenizer} {
  program.name = "ProgName";
}

void Parser::parse() {
  Token token = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    switch (token.type) {
      case TokenType::INCLUDE: {
        tokenizer.consumePeek();
        // include statement
        break;
      }

      case TokenType::FUNC: {
        tokenizer.consumePeek();
        auto& funcDec = program.decs.emplace_back();
        if (!functionDec(funcDec)) {
          program.decs.pop_back();
          // try to recover?
          return;
        }
        break;
      }

      case TokenType::STRUCT: {
        tokenizer.consumePeek();
        auto& sDec = program.decs.emplace_back();
        if (!structDec(sDec)) {
          program.decs.pop_back();
          // try to recover?
          return;
        }
        break;
      }

      case TokenType::TEMPLATE: {
        tokenizer.consumePeek();
        auto& tempDec = program.decs.emplace_back();
        if (!templateDec(tempDec)) {
          program.decs.pop_back();
          // try to recover?
          return;
        }
        break;
      }

      case TokenType::CREATE: {
        tokenizer.consumePeek();
        // template creation
        break;
      }

      case TokenType::IDENTIFIER: {
        // global variable declaration
        program.decs.emplace_back(std::make_unique<Statement>(parseStatement(TokenType::NOTHING, TokenType::SEMICOLON)));
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
        break;

    }
    token = tokenizer.peekNext();
  }
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
      sDec.decs.emplace_back(std::make_unique<Statement>(parseStatement(TokenType::SEMICOLON, TokenType::CLOSE_BRACE)));
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
  Token token = tokenizer.peekNext();
  if (token.type == delimiterA || token.type == delimiterB) {
    return {StatementType::NONE};
  }

  /**
   * The first node in the list is the root node of the whole statement
   *  - root node has ownership over the rest
   * 
   * Nodes should be placed in ascending precedence
   *  - lowest precedence operator in the whole statement should be at the top
   * 
   * The list has no ownership over the root statement, hence the raw pointer type,
   * so be sure to free the memory at the 0 index before returning the statement
   * 
   * When a node is placed in the list/tree, we clear nodes with higher precedence from the list
   *  - they cannot be accessed anymore since they are to the left of the new node
  */
  std::list<Statement*> list;
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
      if (list.empty()) {
        // expected expression before binary operator
        expected.emplace_back(ExpectedType::EXPRESSION, lineNum, token.position - lineStart);
        list.emplace_back(new Statement{std::move(statement)});
      }
      
      else {
        const StatementType sType = list.front()->type;
        if (sType == StatementType::BAD || sType == StatementType::NONE) {
          // never will be a node of this type in the list
          exit(1);
        }
        else if (hasData(sType)) {
          // first "value" token in statement: take it
          // this is the only node in the list (list.size == 1)
          if (statement.type == StatementType::BINARY_OP) {
            statement.binOp->leftSide = std::make_unique<Statement>(std::move(*list.front()));
          } else {
            statement.unOp->operand = std::make_unique<Statement>(std::move(*list.front()));
          }
          // move the statement to the tree root
          new (list.front()) Statement{std::move(statement)};
        } 
        
        else {
          Statement* prev = nullptr;
          auto list_iter = list.begin();
          while (list_iter != list.end()) {
            TokenType op;
            if ((*list_iter)->type == StatementType::BINARY_OP) {
              op = (*list_iter)->binOp->op;
            } else if ((*list_iter)->type == StatementType::UNARY_OP) {
              op = (*list_iter)->unOp->op;
            } else {
              // not possible to reach here:
              // there should never be a different statement type placed in the list
              // if there is, the code is incorrect
              exit(1);
            }
            if (operatorPrecedence.at(token.type) <= operatorPrecedence.at(op)) {
              // place it above
              if (prev) {
                std::unique_ptr<Statement>* childOfPrev = prev->getChild();

                // DUP CODE: moving child node
                if (!childOfPrev) {
                  if (statement.type == StatementType::BINARY_OP) {
                    // expected token between operators
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

                Statement* addedStatement = prev->addStatementToNode(std::move(statement));
                if (!addedStatement) {
                  prev = nullptr;
                  break;
                }
                list.erase(list_iter, list.end());
                list.emplace_back(addedStatement);
              } else {
                // root node
                if (statement.type == StatementType::BINARY_OP) {
                  statement.binOp->leftSide = std::make_unique<Statement>(std::move(*list.front()));
                } else {
                  statement.unOp->operand = std::make_unique<Statement>(std::move(*list.front()));
                }
                // move the statement to the root
                new (list.front()) Statement{std::move(statement)};
                list.erase(++list.begin(), list.end());
              }
              prev = nullptr;
              break;
            }
            prev = *list_iter;
            ++list_iter;
          }
          if (list_iter == list.end() && prev) {
            std::unique_ptr<Statement>* childOfPrev = prev->getChild();
            
            // DUP CODE: moving child node
            if (!childOfPrev) {
              if (statement.type == StatementType::BINARY_OP) {
                // expected expression between operators
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

            Statement* addedStatement = prev->addStatementToNode(std::move(statement));
            list.emplace_back(addedStatement);
          }
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
      else if (lookAhead.type == TokenType::COLON) {
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
      if (list.empty()) {
        list.emplace_back(new Statement{std::move(statement)});
      } else {
        if (hasData(list.front()->type) || !list.back()->addStatementToNode(std::move(statement))) {
          // restart the statement parse, discard everything previously
          expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    else if (isLiteral(token.type)) {
      // DUP CODE: adding leaf node to the tree in place
      if (list.empty()) {
        list.emplace_back(new Statement{token});
      } else {
        if (hasData(list.front()->type) || !list.back()->addStatementToNode(Statement{token})) {
          // restart the statement parse, discard everything previously
          expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
          break;
        }
      }
      // END DUP CODE
    }

    else if (token.type == TokenType::OPEN_PAREN) {
      Statement inner = parseStatement(TokenType::NOTHING, TokenType::CLOSE_PAREN);
      lookAhead = tokenizer.peekNext();
      Statement statement;
      if (inner.type != StatementType::NONE) {
        statement.wrapped = std::make_unique<Statement>(std::move(inner));
        statement.type = StatementType::WRAPPED_VALUE;
      } else {
        expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart);
      }
      // DUP CODE: adding leaf node to the tree by move
      if (list.empty()) {
        list.emplace_back(new Statement{std::move(statement)});
      } else {
        if (hasData(list.front()->type) || !list.back()->addStatementToNode(std::move(statement))) {
          // restart the statement parse, discard everything previously
          expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
          break;
        }
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

    // else if (isKeyword(token.type)) {
    //   // handling all keywords in separate functions would prevent this mess of a function from getting worse
    // }

    // skip comments
    else if (token.type == TokenType::COMMENT) {
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
      if (list.empty()) {
        list.emplace_back(new Statement{std::move(statement)});
      } else {
        if (hasData(list.front()->type) || !list.back()->addStatementToNode(std::move(statement))) {
          // restart the statement parse, discard everything previously
          expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position + 1 - lineStart, delimiterA);
          break;
        }
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

    // array literals
    else if (token.type == TokenType::OPEN_BRACKET) {
    }

    else if (token.type == TokenType::SEMICOLON && delimiterA != TokenType::SEMICOLON && delimiterB != TokenType::SEMICOLON) {
      expected.emplace_back(ExpectedType::TOKEN, lineNum, token.position - lineStart, delimiterB);
      break;
    }
  
    else {
      // unexpected, i guess...
      unexpected.emplace_back(token, lineNum, token.position - lineStart);
      break;
    }
    
    if (lookAhead.type == delimiterA || lookAhead.type == delimiterB) {
      break;
    }
    
    token = tokenizer.tokenizeNext();
    lineNum = tokenizer.lineNum;
    lineStart  =tokenizer.lineStart;
    lookAhead = tokenizer.peekNext();
  }

  if (list.empty()) {
    return {StatementType::NONE};
  }

  // validate statement, list.back() should be filled
  if (list.back()->isValid() == ExpectedType::EXPRESSION) {
    expected.emplace_back(ExpectedType::EXPRESSION, tokenizer.lineNum, lookAhead.position + 1 - tokenizer.lineStart);
  }
  Statement* front = list.front();
  Statement r_statement{std::move(*front)};
  delete front;
  return r_statement;
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

bool isStatementDelimiter(TokenType type) {
  return type >= TokenType::CLOSE_PAREN && type <= TokenType::CLOSE_BRACKET;
}

bool isTypeDelimiter(TokenType type) {
  return type != TokenType::IDENTIFIER && !isBuiltInType(type);
}
