#include "checker.hpp"

CheckerError::CheckerError(CheckerErrorType type, const Token &token): token{token}, type{type}, dec{} {}
CheckerError::CheckerError(CheckerErrorType type, const Token &token, Declaration *decPtr): token{token}, type{type}, dec{decPtr} {}

Checker::Checker(Program& prog, Tokenizer& tk):
structsLookUp{}, lookUp{}, errors{}, program{prog}, tokenizer{tk} {}

void Checker::scanTopLevel() {
  for (auto& dec : program.decs) {
    switch (dec.decType) {
      case DecType::FUNCTION: {
        Declaration* &decPtr = lookUp[tokenizer.extractToken(dec.func->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, dec.func->name, decPtr);
        } else {
          decPtr = &dec;
        }
        break;
      }
      case DecType::VARIABLE_DEC: {
        Declaration* &decPtr = lookUp[tokenizer.extractToken(dec.varDec->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, dec.func->name, decPtr);
        } else {
          decPtr = &dec;
        }
        break;
      }
      case DecType::STRUCT: {
        const std::string structName = tokenizer.extractToken(dec.struc->name);
        Declaration* &decPtr = lookUp[structName];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, dec.func->name, decPtr);
        } else {
          decPtr = &dec;
          auto& structDecLookUp = structsLookUp[structName];
          for (auto& inner : dec.struc->decs) {
            if (inner.decType == DecType::VARIABLE_DEC) {
              Declaration* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner.varDec->name)];
              if (innerStructDecPtr) {
                errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, dec.func->name, innerStructDecPtr);
              } else {
                innerStructDecPtr = &inner;
              }
            } else if (inner.decType == DecType::FUNCTION) {
              Declaration* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner.func->name)];
              if (innerStructDecPtr) {
                errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, dec.func->name, innerStructDecPtr);
              } else {
                innerStructDecPtr = &inner;
              }
            } else {
              // only functions and variables at top level of struct. parser catches this
              exit (1);
            }
          }
        }

        break;
      }
      case DecType::TEMPLATE: {
        Token * tk = nullptr;
        if (dec.temp->dec.decType == DecType::STRUCT) {
         tk = &dec.temp->dec.struc->name;
        } else if (dec.temp->dec.decType == DecType::FUNCTION) {
         tk = &dec.temp->dec.func->name;
        } else {
          // not a valid template. parser catches this
          exit(1);
        }
        Declaration*& decPtr = lookUp[tokenizer.extractToken(*tk)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, *tk, decPtr);
        } else {
          decPtr = &dec;
        }
      }
      case DecType::ENUM: {
        break;
      }
      default: {
        break;
      }
    }
  }
}

bool Checker::checkFunction(FunctionDec& funcDec) {
  bool wasThereAnError = false;
  bool requireReturn = true;
  // validate return type
  if (!checkType(funcDec.returnType)) {
    if (errors.back().type == CheckerErrorType::VOID_TYPE) {
      errors.pop_back();
      requireReturn = false;
    } else {
      wasThereAnError = true;
    }
  }
  // parser needs to check that they are just variable declarations, nothing else;
  // validate parameter types and name
  std::vector<std::string> locals;
  if (funcDec.params.curr.type != StatementType::NONE) {
    StatementList *list = &funcDec.params;
    while (list) {
      locals.emplace_back(tokenizer.extractToken(list->curr.dec->varDec->name));
      Declaration* &paramDec = lookUp[locals.back()];
      if (paramDec) {
        errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.dec->varDec->name, paramDec);
      } else {
        // name is available, check type
        if (!checkType(list->curr.dec->varDec->type)) {
          wasThereAnError = true;
        }
        paramDec = list->curr.dec;
      }
      list = list->next;
    }
  }

  if (!checkScope(funcDec.body, locals, funcDec.returnType, requireReturn, false, false)) {
    wasThereAnError = true;
  }
  while (!locals.empty()) {
    // remove locals from table
    lookUp.erase(locals.back());
    locals.pop_back();
  }
  return wasThereAnError;
}


/**
 * \param scope The scope to check
 * \param locals name of all local variables allocated 
 * \param returnType the return type of the scope
 * \param isReturnRequired set to true if a return is required within this scope
 * \returns true if the scope is valid, false otherwise. (error type appended to errors)
*/
bool Checker::checkScope(Scope& scope, std::vector<std::string>& locals, Type& returnType, bool isReturnRequired, bool isLoop, bool isSwitch) {
  const uint32_t marker = locals.size();
  StatementList* list = &scope.scopeStatements;
  do {
    switch (list->curr.type) {
      case StatementType::BINARY_OP: {
        break;
      }
      case StatementType::FUNCTION_CALL: {
        break;
      }
      case StatementType::KEY_W_BODY: {
        break;
      }
      case StatementType::KEYWORD: {
        if (list->curr.keywBody->keyword == TokenType::CONTINUE) {
          if (!isLoop) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_CONTINUE_HERE, Token{0,0,TokenType::NOTHING});
          }
          break;
        }
        else if (list->curr.keywBody->keyword == TokenType::BREAK) {
          if (!isLoop && !isSwitch) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_BREAK_HERE, Token{0,0,TokenType::NOTHING});
          }
          break;
        } else {
          // idk. are there any others?
        }
        break;
      }
      case StatementType::SCOPE: {
        if (!checkScope(*list->curr.scope, locals, returnType, false, false, false)) {
          return false;
        }
        break;
      }
      case StatementType::UNARY_OP: {
        break;
      }
      case StatementType::VARIABLE_DEC: {
        break;
      }
      default: {
        // statement does nothing;
        break;
      }
    }
  } while (list);
  while (marker < locals.size()) {
    // remove locals from table
    lookUp.erase(locals.back());
    locals.pop_back();
  }
  return isReturnRequired;
}

/**
 * Validates a type
 * \param type the type to check
 * \returns true if the type is a valid type, false otherwise (adds the error to errors)
 * \note in the case of the type being just 'void', will return false even though it is valid for function return types.
 *  check if the emplaced error is 'void' and remove it if called for a function return type
*/
bool Checker::checkType(Type& type) {
  /**
   * Used to track the type info. 0 means no type, 1 means type, 2 means ptr, 3 means ref
   * Can go forward, but cant go back. Must hit 1 before 2 or 3
   * Examples:
   * "int ptr ref"
   *    processing "int" moves to 1, "ptr" to 2, and "ref" to 3
   * 
   * "ptr"
   *    processing "ptr", cannot go from 0 to 2, error
   * 
   * "int ref ptr"
   *    processing "int" moves to 1, "ref" to 3, and "ptr" cannot go back to 2, error
   * 
   * "int int ptr"
   *    processing first "int" moves to 1, second "int" is already at 1, error
   * 
  */
  uint8_t typeType = 0;

  CheckerErrorType errorType = CheckerErrorType::NONE;
  TokenList *list = &type.tokens;
  if (list->curr.type == TokenType::NOTHING) {
    errors.emplace_back(CheckerErrorType::MISSING_TYPE, list->curr);
    return false;
  }
  do {
    if (isBuiltInType(list->curr.type)) {
      const TokenType tokenType = list->curr.type;
      if (tokenType == TokenType::POINTER) {
        if (typeType == 0) {
          errorType = CheckerErrorType::PTR_MUST_POINT_TO_A_TYPE;
          break;
        }
        if (typeType == 3) {
          errorType = CheckerErrorType::CANNOT_PTR_A_REF;
          break;
        }
        typeType = 2;
      }
      else if (tokenType == TokenType::REFERENCE) {
        if (typeType == 0) {
          errorType = CheckerErrorType::REF_MUST_REF_A_TYPE;
          break;
        }
        if (typeType == 3) {
          errorType = CheckerErrorType::CANNOT_REF_A_REF;
          break;
        }
        typeType = 3;
      }
      else if (tokenType == TokenType::VOID) {
        if (!list->next) {
          errorType = CheckerErrorType::VOID_TYPE;
          break;
        }
      }
      else {
        if (typeType == 1) {
          errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
          break;
        }
        typeType = 1;
      }
    } else {
      if (typeType > 0) {
        errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
        break;
      }
      Declaration* &typeDec = lookUp[tokenizer.extractToken(list->curr)];

      if (!typeDec) {
        // type does not exist
        errorType = CheckerErrorType::NO_SUCH_TYPE;
        break;
      }
      if (typeDec->decType != DecType::STRUCT) {
        errorType = CheckerErrorType::EXPECTING_TYPE;
        // received type->decType;
        break;
      }
      // type exists
      typeType = 1;
    }
    list = list->next;
  } while (list);
  if (errorType == CheckerErrorType::NONE) {
    return true;
  }
  errors.emplace_back(errorType, list->curr);
  return false;
}

const TokenList *Checker::checkStatement(Statement& statement) {
  switch (statement.type) {
    case StatementType::UNARY_OP: {
    }

    case StatementType::BINARY_OP: {
    }

    case StatementType::VARIABLE_DEC: {
    }

    case StatementType::FUNCTION_CALL: {
    }

    case StatementType::ARRAY_ACCESS: {
    }

    case StatementType::WRAPPED_VALUE: {
    }

    case StatementType::KEYWORD: {
    }

    case StatementType::VALUE: {
      if (statement.var->type == TokenType::IDENTIFIER) {
        Declaration *&decPtr = lookUp[tokenizer.extractToken(*statement.var)];
        if (!decPtr) {
          errors.emplace_back(CheckerErrorType::NO_SUCH_VARIABLE, *statement.var);
          break;
        }
        if (decPtr->decType != DecType::VARIABLE_DEC) {
          errors.emplace_back(CheckerErrorType::NOT_A_VARIABLE, *statement.var, decPtr);
          break;
        }
        return &decPtr->varDec->type.tokens;
      } else if (statement.var->type == TokenType::DECIMAL_NUMBER) {
        return &intValue;
      } else if (statement.var->type == TokenType::NULL_PTR) {
        return &pointerValue;
      } else if (statement.var->type == TokenType::FALSE || statement.var->type == TokenType::TRUE) {
        return &boolValue;
      } else if (statement.var->type == TokenType::STRING_LITERAL) {
        return &stringValue;
      } else if (statement.var->type == TokenType::CHAR_LITERAL) {
        return &charValue;
      }
      break;
    }
    default: break;
  }
  return nullptr;
}
