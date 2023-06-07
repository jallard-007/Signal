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
  if (!getStatements(func->params, TokenType::CLOSE_PAREN)) {
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::CLOSE_PAREN) {
    // expected close paren
    return false;
  }
  // consume close paren
  tokenizer.consumePeek();
  if (tokenizer.peekNext().type != TokenType::COLON) {
    // expected a colon
    expectedToken.emplace_back(tokenizer.peeked.position + tokenizer.peeked.length, 0, TokenType::COLON);
    return false;
  }
  // consume colon
  tokenizer.consumePeek();
  // get return type
  {
    Token t = getType(func->returnType);
    if (t.type != TokenType::OPEN_BRACE) {
      // expected open brace after type
      expectedToken.emplace_back(t.position + t.length, 0, TokenType::OPEN_BRACE);
      return false;
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

    // TODO: BINARY OPs
    // binary operator token, try to merge it with other tokens / set it as the root statement
    if (isBinaryOp(token.type)) {
      Statement statement{std::make_unique<BinOp>(token.type)};
      if (list.empty()) {
        // expected a token before binary operator
        list.emplace_back(new Statement{std::move(statement)});
      }
      
      else {
        const StatementType sType = list.front()->type;
        if (sType == StatementType::BAD || sType == StatementType::NONE) {
          // idk man
        }
        else if (hasData(sType)) {
          // first "value" token in statement, take it
          // we also include unary op in here even though it is not valid
          statement.binOp->leftSide = std::make_unique<Statement>(std::move(*list.front()));
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
              // unexpected
              break;
            }
            if (operatorPrecedence.at(token.type) <= operatorPrecedence.at(op)) {
              // place it above
              statement.binOp->leftSide = std::make_unique<Statement>(std::move(*(*list_iter)));
              if (prev) {
                Statement* addedStatement = prev->addStatementToNode(std::move(statement));
                list.erase(list_iter, list.end());
                list.emplace_back(addedStatement);
              } else {
                *list.front() = std::move(statement);
                list.erase(++list.begin(), list.end());
              }
              break;
            }
            prev = *list_iter;
            ++list_iter;
          }
          if (list_iter == list.end() && !prev) {
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
        if (!getStatements(statement.funcCall->args.list, TokenType::CLOSE_PAREN)) {
          break;
        }
        lookAhead = tokenizer.peekNext();
        if (lookAhead.type != TokenType::CLOSE_PAREN) {
          // issue with offset statement, expected a close bracket
          // continue as if there was one there
          // add to expectedTokens
          expectedToken.emplace_back(lookAhead.position, 0, TokenType::CLOSE_PAREN);
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
          // issue with offset statement, expected a close bracket 
          // continue as if there was one there
          // add to expectedTokens
          expectedToken.emplace_back(lookAhead.position, 0, TokenType::CLOSE_BRACKET);
        } else {
          // consume the CLOSE::BRACKET
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
        }
      }

      else {
        statement.type = StatementType::VALUE;
        statement.var = token;
      }

      if (list.empty()) {
        list.emplace_back(new Statement{std::move(statement)});
      } else {
        if (!list.back()->addStatementToNode(std::move(statement))) {
          // could not add the node; node does not accept Literal
          // place token in unexpectedTokens since it cannot fit in the tree
          unexpectedToken.emplace_back(token);
        }
      }
    }

    else if (isLiteral(token.type)) {
      if (list.empty()) {
        list.emplace_back(new Statement{token});
      } else {
        if (!list.back()->addStatementToNode(Statement{token})) {
          // could not add the node; node does not accept Literal
          // place token in unexpectedTokens since it cannot fit in the tree
          unexpectedToken.emplace_back(token);
        }
      }
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
  Statement* front = list.front();
  Statement r_statement{std::move(*front)};
  delete front;
  return r_statement;
}

/**
 * Extracts comma delimited statements until it reaches the specified delimiter, or a statement delimiter
 * Does NOT consume the final delimiter
 * Returns true on a successful parse (when it reaches the delimiter), false otherwise
*/
bool Parser::getStatements(std::vector<Statement>& statements, const TokenType delimiter) {
  Token token = tokenizer.peekNext();

  while (token.type != TokenType::END_OF_FILE) {
    Statement s = parseStatement(TokenType::COMMA);
    if (s.type != StatementType::NONE) {
      statements.emplace_back(std::move(s));
    }
    token = tokenizer.peekNext();
    if (token.type == delimiter || isStatementDelimiter(token.type)) {
      return true;
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
  return type >= TokenType::CLOSE_PAREN && type <= TokenType::SEMICOLON;
}

bool isTypeDelimiter(TokenType type) {
  return type != TokenType::IDENTIFIER && !isBuiltInType(type);
}
