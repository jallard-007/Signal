#include "parser.hpp"

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
  auto& func = program.decs.emplace_back(
    std::make_unique<FunctionDec>(token)
  ).func;

  if (tokenizer.peekNext().type != TokenType::OPEN_PAREN) {
    // expected open paren
    return false;
  }
  // get parameters
  if (!getParams(func->params)) {
    return false;
  }
  if (tokenizer.peekNext().type != TokenType::COLON) {
    // expected a colon
    return false;
  }
  tokenizer.tokenizeNext();
  // get return type
  if (getType(func->returnType).type != TokenType::OPEN_BRACE) {
    // expected open brace after type
    return false;
  }
  return true;
}

/**
 * Extracts tokens until it reaches the specified delimiter, forming a Statement.
*/
Statement Parser::parseStatement(const TokenType delimiter) {
  Statement r_statement;
  Token token = tokenizer.peekNext();
  if (token.type == delimiter || isStatementDelimiter(token.type)) {
    return r_statement;
  }
  std::vector<Statement> stack;
  token = tokenizer.tokenizeNext();
  Token lookAhead = tokenizer.peekNext();
  while (token.type != TokenType::END_OF_FILE) {
    if (isBinaryOp(token.type)) {
      if (!stack.empty()) {
        auto prevStatement = std::move(stack.back());
        stack.pop_back();
        auto& statement = stack.emplace_back(std::make_unique<BinOp>(token.type));
        statement.binOp->leftSide = std::make_unique<Statement>(std::move(prevStatement));
      } else {
        stack.emplace_back(std::make_unique<BinOp>(token.type));
      }
    }

    else if (isUnaryOp(token.type)) {
      if ((token.type == TokenType::DECREMENT_POSTFIX || token.type == TokenType::INCREMENT_POSTFIX) && !stack.empty()) {
        auto prevStatement = std::move(stack.back());
        stack.pop_back();
        auto& statement = stack.emplace_back(std::make_unique<UnOp>(token.type));
        statement.unOp->operand = std::make_unique<Statement>(std::move(prevStatement));
      } else {
        stack.emplace_back(std::make_unique<UnOp>(token.type));
      }
    }

    else if (token.type == TokenType::IDENTIFIER) {
      // function call
      if (lookAhead.type == TokenType::OPEN_PAREN) {
        auto& statement = stack.emplace_back(std::make_unique<FunctionCall>(token));
        if (!getStatements(statement.funcCall->args.list, TokenType::CLOSE_PAREN)) {
          break;
        }
        lookAhead = tokenizer.peekNext();
      }

      // pointer access with offset, array[x]
      else if (lookAhead.type == TokenType::OPEN_BRACKET) {
        auto& statement = stack.emplace_back(std::make_unique<ArrayAccess>(token));
        if (!getStatements(statement.arrAccess->offset.list, TokenType::CLOSE_BRACKET)) {
          break;
        }
        lookAhead = tokenizer.peekNext();
      }

      // variable declaration
      else if (lookAhead.type == TokenType::COLON) {
        auto& statement = stack.emplace_back(std::make_unique<VariableDec>(token));
        lookAhead = getType(statement.varDec->type);
      }

      else {
        stack.emplace_back(token);
      }
    }

    // get second last statement
    // need to figure out precedence before grouping statements

    if (lookAhead.type == delimiter || isStatementDelimiter(lookAhead.type)){
      break;
    }
    
    token = tokenizer.tokenizeNext();
    lookAhead = tokenizer.peekNext();
  }

  if (stack.size() >= 1) {
    r_statement = std::move(stack.back());
  }
  return r_statement;
}

/**
 * Extracts comma delimited parameters for a function declaration
 * 
 * Returns true on a successful parse (when it reaches a close paren), false otherwise
*/
bool Parser::getParams(std::vector<VariableDec>& params) {
  if (tokenizer.tokenizeNext().type != TokenType::OPEN_PAREN) {
    // expected open paren
    return false;
  }
  Token identifier = tokenizer.tokenizeNext();
  if (identifier.type == TokenType::CLOSE_PAREN) {
    return true;
  }
  while (identifier.type != TokenType::END_OF_FILE) {
    if (identifier.type != TokenType::IDENTIFIER) {
      // expected identifier
      return false;
    }
    if (tokenizer.tokenizeNext().type != TokenType::COLON) {
      // expected colon
      return false;
    }
    VariableDec& var = params.emplace_back(identifier);
    const Token typeAfter = getType(var.type);
    if (typeAfter.type == TokenType::CLOSE_PAREN) {
      return true;
    }
    if (typeAfter.type == TokenType::BAD_VALUE) {
      return false;
    }
    identifier = tokenizer.tokenizeNext();
  }
  return false;
}

/**
 * Extracts comma delimited statements until it reaches the specified delimiter
 * 
 * Returns true on a successful parse (when it reaches the delimiter), false otherwise
*/
bool Parser::getStatements(std::vector<Statement>& statements, const TokenType delimiter) {
  Token token = tokenizer.tokenizeNext();
  while (token.type != TokenType::END_OF_FILE) {
    if (token.type == delimiter) {
      return true;
    }
    Statement s = parseStatement(TokenType::COMMA);
    if (s.type != StatementType::NONE) {
      statements.emplace_back(std::move(s));
    }
    token = tokenizer.tokenizeNext();
  }
  return false;
}

/**
 * Returns the next token after the type list
*/
Token Parser::getType(Type& type) {
  Token tp = tokenizer.tokenizeNext();
  while (tp.type != TokenType::END_OF_FILE) {
    if (isTypeDelimiter(tp.type)) {
      break;
    }
    // TODO: type lookup to see if the type exists yet, or maybe we do that later...
    if (isBuiltInType(tp.type) || tp.type == TokenType::IDENTIFIER) {
      type.tokens.emplace_back(tp);
    } else {
      // unexpected token
      break;
    }
    tp = tokenizer.tokenizeNext();
  }
  return tp;
}

bool isStatementDelimiter(TokenType type) {
  return type == TokenType::CLOSE_BRACE || type == TokenType::CLOSE_PAREN
    || type == TokenType::CLOSE_BRACKET || type == TokenType::SEMICOLON;
}
