#include "parser.hpp"

Parser::Parser(Tokenizer& tokenizer): tokenizer{tokenizer} {
  program.name = "ProgName";
}

void Parser::parse() {
  Token token = tokenizer.tokenizeNext();
  while (token.type != TokenType::END_OF_FILE) {
    switch (token.type) {
      case TokenType::INCLUDE:
        // include statement
        break;

      case TokenType::FUNC:
        token = tokenizer.tokenizeNext();
        if (token.type != TokenType::IDENTIFIER) {
          return;
        }
        Declaration& funcDec = program.defs.emplace_back(DecType::FUNCTION);

        // function declaration
        break;

      case TokenType::TEMPLATE:
        // template declaration
        break;

      case TokenType::CREATE:
        // template creation
        break;

      case TokenType::IDENTIFIER:
        // global variable declaration
        break;
      
      default:
        // error, unexpected token
        break;

    }
    token = tokenizer.tokenizeNext();
  }

  return;
}


