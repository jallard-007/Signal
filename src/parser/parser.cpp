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
        token = tokenizer.tokenizeNext();
        if (token.type != TokenType::IDENTIFIER) {
          // expected identifier
          return;
        }
        Declaration& dec = program.decs.emplace_back(
          std::make_unique<FunctionDec>(token)
        );
        if (!getParams(dec.func->params)) {
          // syntax of function parameters is incorrect
          return;
        }
        if (tokenizer.tokenizeNext().type != TokenType::COLON) {
          // expected a colon
          return;
        }
        if (getType(dec.func->returnType) != TokenType::CLOSE_BRACE) {
          // expected close brace after type
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

  return;
}

/**
 * Returns true on a successful parse (when it reaches a close paren), false otherwise
*/
bool Parser::getParams(std::vector<Variable>& params) {
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
    Variable& var = params.emplace_back(identifier);
    const TokenType typeAfter = getType(var.type);
    if (typeAfter == TokenType::CLOSE_PAREN) {
      return true;
    }
    if (typeAfter == TokenType::BAD_VALUE) {
      return false;
    }
    identifier = tokenizer.tokenizeNext();
  }
  return false;
}


/**
 * Returns the next token's type after the type list
*/
TokenType Parser::getType(Type& type) {
  Token tp = tokenizer.tokenizeNext();
  while (tp.type != TokenType::END_OF_FILE) {
    if (isTypeDelimeter(tp.type)) {
      return tp.type;
    }
    // TODO: type lookup to see if the type exists yet, or maybe we do that later...
    if (isBuiltInType(tp.type) || tp.type == TokenType::IDENTIFIER) {
      type.tokens.emplace_back(tp);
    } else {
      // unexpected token
      return TokenType::BAD_VALUE;
    }
    tp = tokenizer.tokenizeNext();
  }
  return TokenType::END_OF_FILE;
}

