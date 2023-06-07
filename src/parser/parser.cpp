#include "parser.hpp"
#include <list>
#include <memory>

Parser::Parser(Tokenizer& tokenizer): tokenizer{tokenizer} {
  program.name = "ProgName";
}

void Parser::parse() {
  Token token = tokenizer.tokenizeNext();
  while (token.type != TokenType::END_OF_FILE) {
    switch (token.type) {
      case TokenType::INCLUDE: {
        // include statement
        break;
      }
        
      case TokenType::FUNC: {
        if (!functionDec()) {
          return;
        }
        break;
      }

      case TokenType::TEMPLATE: {
        // template declaration
        break;
      }
        

      case TokenType::CREATE: {
        // template creation
        break;
      }
       

      case TokenType::IDENTIFIER: {
        // global variable declaration
        break;
      }
        
      
      default:
        // error, unexpected token
        break;

    }
    token = tokenizer.tokenizeNext();
  }
}

bool Parser::functionDec() {
  const Token token = tokenizer.tokenizeNext();
  if (token.type != TokenType::IDENTIFIER) {
    // expected identifier
    return false;
  }
  // add function declaration node
  auto& func = program.decs.emplace_back(std::make_unique<FunctionDec>(token)).func;

  if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
    // expected open paren
    return false;
  }
  // get parameters
  tokenizer.consumePeek();
  if (!getStatements(func->params, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
    // expected close paren
    expectedStatement.emplace_back(ExpectedType::SYMBOL, tokenizer.peeked.position, TokenType::COLON);
    return false;
  }
  // consume close paren
  tokenizer.consumePeek();
  if (tokenizer.peekNext().type != TokenType::COLON) {
    // expected a colon
    expectedStatement.emplace_back(ExpectedType::SYMBOL, tokenizer.peeked.position, TokenType::COLON);
  } else {
    // consume colon
    tokenizer.consumePeek();
  }
  // get return type
  {
    Token t = getType(func->returnType);
    if (t.type != TokenType::OPEN_BRACE) {
      // expected open brace after type
      expectedStatement.emplace_back(ExpectedType::SYMBOL, tokenizer.peeked.position, TokenType::OPEN_BRACE);
    }
  }
  // consume open brace
  tokenizer.consumePeek();
  return true;
}

/**
 * Extracts tokens until it reaches the specified delimiter, forming a Statement.
 * Does NOT consume the delimiter, you must do this after 
*/
Statement Parser::parseStatement(const TokenType delimiter) {
  Token token = tokenizer.peekNext();
  if (token.type == delimiter || isStatementDelimiter(token.type)) {
    return {StatementType::NONE};
  }

  // nodes at the end of the list have higher precedence
  // the list has ownership of the first node, but the rest are linked to the first node
  // need to free or move ownership of the first node before destruction
  std::list<Statement*> list;

  tokenizer.consumePeek();
  Token lookAhead = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {

    // TODO : take into account parentheses in the statement
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
        expectedStatement.emplace_back(ExpectedType::EXPRESSION, token.position);
        list.emplace_back(new Statement{std::move(statement)});
      }
      
      else {
        const StatementType sType = list.front()->type;
        if (sType == StatementType::BAD || sType == StatementType::NONE) {
          // idk man
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
          *list.front() = std::move(statement);
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
                    expectedStatement.emplace_back(ExpectedType::EXPRESSION, token.position);
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
                // front node
                if (statement.type == StatementType::BINARY_OP) {
                  statement.binOp->leftSide = std::make_unique<Statement>(std::move(*list.front()));
                } else {
                  statement.unOp->operand = std::make_unique<Statement>(std::move(*list.front()));
                }
                *list.front() = std::move(statement);
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
                expectedStatement.emplace_back(ExpectedType::EXPRESSION, token.position);
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
      // function call
      if (lookAhead.type == TokenType::OPEN_PAREN) {
        statement.funcCall = std::make_unique<FunctionCall>(token);
        statement.type = StatementType::FUNCTION_CALL;
        tokenizer.tokenizeNext();
        if (!getStatements(statement.funcCall->args.list, TokenType::COMMA, TokenType::CLOSE_PAREN)) {
          break;
        }
        lookAhead = tokenizer.peekNext();
        if (lookAhead.type != TokenType::CLOSE_PAREN) {
          expectedStatement.emplace_back(ExpectedType::END_OF_STATEMENT, lookAhead.position);
        } else {
          // consume the CLOSE::PAREN
          tokenizer.consumePeek();
          lookAhead = tokenizer.peekNext();
        }
      }

      // pointer access with offset, array[x]
      else if (lookAhead.type == TokenType::OPEN_BRACKET) {
        statement.arrAccess = std::make_unique<ArrayAccess>(token);
        statement.type = StatementType::ARRAY_ACCESS;
        tokenizer.tokenizeNext();
        statement.arrAccess->offset = parseStatement(TokenType::CLOSE_BRACKET);
        lookAhead = tokenizer.peekNext();
        if (lookAhead.type != TokenType::CLOSE_BRACKET) {

        } else {
          tokenizer.consumePeek();
          lookAhead = tokenizer.peekNext();
        }
      }

      // variable declaration
      else if (lookAhead.type == TokenType::COLON) {
        statement.varDec = std::make_unique<VariableDec>(token);
        statement.type = StatementType::VARIABLE_DEC;
        // consume the colon
        tokenizer.consumePeek();
        lookAhead = getType(statement.varDec->type);
        if (lookAhead.type == TokenType::END_OF_FILE) {
          // never reached a type delimiter
          expectedStatement.emplace_back(ExpectedType::END_OF_STATEMENT, lookAhead.position, delimiter);
        }
      }

      else {
        statement.type = StatementType::VALUE;
        statement.var = token;
      }

      if (list.empty()) {
        list.emplace_back(new Statement{std::move(statement)});
      } else {
        if (hasData(list.front()->type) || !list.back()->addStatementToNode(std::move(statement))) {
          // restart the statement parse, discard everything previously
          expectedStatement.emplace_back(ExpectedType::END_OF_STATEMENT, token.position, delimiter);
          list.front()->~Statement();
          new (list.front()) Statement{std::move(statement)};
        }
      }
    }

    else if (isLiteral(token.type)) {
      if (list.empty()) {
        list.emplace_back(new Statement{token});
      } else {
        if (hasData(list.front()->type) || !list.back()->addStatementToNode(Statement{token})) {
          // restart the statement parse, discard everything previously
          expectedStatement.emplace_back(ExpectedType::END_OF_STATEMENT, token.position, delimiter);
          list.front()->~Statement();
          new (list.front()) Statement{token};
        }
      }
    }
    
    else if (isKeyword(token.type)) {

    }

    else if (token.type == TokenType::OPEN_PAREN) {
      // wrapped value
      Statement statement;
    }

    if (lookAhead.type == delimiter || isStatementDelimiter(lookAhead.type)){
      break;
    }
    
    token = tokenizer.tokenizeNext();
    lookAhead = tokenizer.peekNext();
  }

  if (list.empty()) {
    return {StatementType::NONE};
  }

  // validate statement, list.back() should be filled
  if (list.back()->isValid() == ExpectedType::EXPRESSION) {
    expectedStatement.emplace_back(ExpectedType::EXPRESSION, lookAhead.position);
  }
  Statement* front = list.front();
  Statement r_statement{std::move(*front)};
  delete front;
  return r_statement;
}

/**
 * Extracts delimiterMinor delimited statements until it reaches the delimiterMajor delimiter
 * hard delimiters include TokenType::CLOSE_PAREN, TokenType::CLOSE_BRACKET, and TokenType::CLOSE_BRACE
 * Does NOT consume the final delimiter
 * Returns true on a successful parse (when it reaches the delimiterMajor), false otherwise
*/
bool Parser::getStatements(std::vector<Statement>& statements, const TokenType delimiterMinor, const TokenType delimiterMajor) {
  Token token = tokenizer.peekNext();

  while (token.type != TokenType::END_OF_FILE) {
    Statement s = parseStatement(delimiterMinor);
    if (s.type != StatementType::NONE) {
      statements.emplace_back(std::move(s));
    }
    token = tokenizer.peekNext();
    if (token.type == delimiterMajor) {
      return true;
    } else if (isStatementDelimiter(token.type)) {
      // if blockType and close brace, skip this, dont need delimiter on blockType statements
      unexpectedTokens.emplace_back(token);
    }
    tokenizer.consumePeek();
  }
  return false;
}

/**
 * Returns the next token after the type list, adding tokens to Type as it goes
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
