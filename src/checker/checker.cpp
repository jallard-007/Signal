#include "checker.hpp"

CheckerError::CheckerError(CheckerErrorType type, Token *token): token{token}, type{type}, dec{} {}
CheckerError::CheckerError(CheckerErrorType type, Token *token, Declaration *decPtr): token{token}, type{type}, dec{decPtr} {}
CheckerError::CheckerError(CheckerErrorType type, Statement *st, Declaration *decPtr): token{nullptr}, type{type}, dec{decPtr} {
  switch(st->type) {
    case StatementType::BINARY_OP:
      token = &st->binOp->op; break;
    case StatementType::UNARY_OP:
      token = &st->unOp->op; break;
    case StatementType::VALUE:
      token = st->var; break;
    case StatementType::VARIABLE_DEC:
      token = &st->dec->varDec->name; break;
    case StatementType::FUNCTION_CALL:
      token = &st->funcCall->name; break;
    case StatementType::ARRAY_ACCESS:
      token = &st->arrAccess->array; break;
    case StatementType::KEYWORD:
      token = st->var; break;
    case StatementType::KEY_W_BODY:
      token = &st->keyWBody->keyword; break;

    case StatementType::WRAPPED_VALUE:
    case StatementType::ARRAY_OR_STRUCT_LITERAL:
    case StatementType::FOR_LOOP_HEADER:
    case StatementType::SCOPE:
      token = nullptr; break;

    default:
      token = nullptr; break;
  }
}

Checker::Checker(Program& prog, Tokenizer& tk, NodeMemPool& mem):
structsLookUp{}, lookUp{}, errors{}, program{prog}, tokenizer{tk}, memPool{mem} {}

/**
 * Scans all global declarations and registers them in the table, checking that the name is available
 * Also registers struct members in the struct's table
*/
void Checker::firstTopLevelScan() {
  for (auto& dec : program.decs) {
    switch (dec.decType) {
      case DecType::FUNCTION: {
        Declaration* &decPtr = lookUp[tokenizer.extractToken(dec.func->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &dec.func->name, decPtr);
        } else {
          decPtr = &dec;
        }
        break;
      }
      case DecType::VARIABLE_DEC: {
        Declaration* &decPtr = lookUp[tokenizer.extractToken(dec.varDec->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &dec.varDec->name, decPtr);
        } else {
          decPtr = &dec;
        }
        break;
      }
      case DecType::STRUCT: {
        const std::string structName = tokenizer.extractToken(dec.struc->name);
        Declaration* &decPtr = lookUp[structName];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &dec.struc->name, decPtr);
        } else {
          decPtr = &dec;
          auto& structDecLookUp = structsLookUp[structName];
          for (auto& inner : dec.struc->decs) {
            if (inner.decType == DecType::VARIABLE_DEC) {
              Declaration* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner.varDec->name)];
              if (innerStructDecPtr) {
                errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &dec.varDec->name, innerStructDecPtr);
              } else {
                innerStructDecPtr = &inner;
              }
            }
            // inner.decType == DecType::FUNCTION
            else {
              Declaration* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner.func->name)];
              if (innerStructDecPtr) {
                errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &dec.func->name, innerStructDecPtr);
              } else {
                innerStructDecPtr = &inner;
              }
            }
          }
        }

        break;
      }
      case DecType::TEMPLATE: {
        Token * tk = nullptr;
        if (dec.temp->dec.decType == DecType::STRUCT) {
         tk = &dec.temp->dec.struc->name;
        } 
        // dec.temp->dec.decType == DecType::FUNCTION
        else {
         tk = &dec.temp->dec.func->name;
        }
        Declaration*& decPtr = lookUp[tokenizer.extractToken(*tk)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, tk, decPtr);
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

bool Checker::validateFunctionHeader(FunctionDec &funcDec) {
  bool valid = true;
  if (!checkType(funcDec.returnType)) {
    if (errors.back().type == CheckerErrorType::VOID_TYPE) {
      errors.pop_back();
    } else {
      valid = false;
    }
  }
  // check parameters
  if (funcDec.params.curr.type != StatementType::NONE) {
    StatementList* params = &funcDec.params;
    do {
      if (!checkType(params->curr.dec->varDec->type)) {
        valid = false;
      }
      params = params->next;
    } while (params);
  }
  return valid;
}

/**
 * Validates function types, global variable types, struct member variable types, struct member function types
*/
void Checker::secondTopLevelScan() {
  for (auto& dec : program.decs) {
    switch (dec.decType) {
      case DecType::FUNCTION: {
        dec.isValid = validateFunctionHeader(*dec.func);
        break;
      }
      case DecType::VARIABLE_DEC: {
        dec.isValid = checkType(dec.varDec->type);
        break;
      }
      case DecType::STRUCT: {
        for (auto& inner : dec.struc->decs) {
          if (inner.decType == DecType::VARIABLE_DEC) {
            inner.isValid = checkType(inner.varDec->type);
          }
          // inner.decType == DecType::FUNCTION
          else {
            inner.isValid = validateFunctionHeader(*inner.func);
          }
        }
        break;
      }
      case DecType::TEMPLATE: {
        // parser validates that there is at least one type
        std::vector<std::string> templateTypes;
        TokenList *templateIdentifiers = &dec.temp->templateIdentifiers;
        do {
          templateTypes.push_back(tokenizer.extractToken(templateIdentifiers->curr));
          Declaration *&tempTypeDec = lookUp[templateTypes.back()];
          if (tempTypeDec) {
            errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &templateIdentifiers->curr, tempTypeDec);
          } else {
            tempTypeDec = memPool.get(Declaration{});
            tempTypeDec->decType = DecType::STRUCT;
          }
          templateIdentifiers = templateIdentifiers->next;
        } while (templateIdentifiers);

        if (dec.temp->dec.decType == DecType::STRUCT) {
          for (auto& inner : dec.temp->dec.struc->decs) {
            if (inner.decType == DecType::VARIABLE_DEC) {
              inner.isValid = checkType(inner.varDec->type);
            }
            // inner.decType == DecType::FUNCTION
            else {
              inner.isValid = validateFunctionHeader(*inner.func);
            }
          }
        }
        // dec.temp->dec.decType == DecType::FUNCTION
        else {
          dec.temp->dec.isValid = validateFunctionHeader(*dec.temp->dec.func);
        }
        while (!templateTypes.empty()) {
          auto tempType = lookUp.find(templateTypes.back());
          memPool.release(tempType->second);
          lookUp.erase(tempType);
          templateTypes.pop_back();
        }
        break;
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
        errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &list->curr.dec->varDec->name, paramDec);
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
        auto&binOp = *list->curr.binOp;
        if (hasData(binOp.leftSide.type)) {

        }
        break;
      }
      case StatementType::UNARY_OP: {
        break;
      }
      case StatementType::FUNCTION_CALL: {
        break;
      }
      case StatementType::KEY_W_BODY: {
        break;
      }
      case StatementType::KEYWORD: {
        if (list->curr.keyWBody->keyword.type == TokenType::CONTINUE) {
          if (!isLoop) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_CONTINUE_HERE, &list->curr.keyWBody->keyword);
          }
          break;
        }
        else if (list->curr.keyWBody->keyword.type == TokenType::BREAK) {
          if (!isLoop && !isSwitch) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_BREAK_HERE, &list->curr.keyWBody->keyword);
          }
          break;
        } else {
          // idk. are there any others?
          // nope
          exit(1);
        }
        break;
      }
      case StatementType::SCOPE: {
        if (!checkScope(*list->curr.scope, locals, returnType, false, isLoop, isSwitch)) {
          return false;
        }
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
    list = list->next;
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
   * Used to track the type info. 0 means we can have a ref, 0-2 means pointer, and 3 means an actual type was found
   * Can go forward, but cant go back. 
   * start -> 0
   * ref -> 1
   * ptr -> 2
   * type -> 3
   * Examples:
   * "ref ptr int"
   *    processing "ref" moves to 1, "ptr" moves to 2, and "int" to 3
   * 
   * "ptr"
   *    processing "ptr" moves to 2, no type after, error
   * 
   * "ptr ref int"
   *    processing "ptr" moves to 2, "ref" invalid since type is not 0, error
   * 
   * "ptr int int"
   *    processing "ptr" moves to 2, first "int" to 3, second "int" already at 3, error
   * 
  */
  uint8_t typeType = 0;

  CheckerErrorType errorType = CheckerErrorType::NONE;
  TokenList *list = &type.tokens;
  if (list->curr.type == TokenType::NOTHING) {
    errors.emplace_back(CheckerErrorType::MISSING_TYPE, &list->curr);
    return false;
  }
  do {
    if (isBuiltInType(list->curr.type)) {
      const TokenType tokenType = list->curr.type;
      if (tokenType == TokenType::POINTER) {
        if (typeType == 3) {
          errorType = CheckerErrorType::UNEXPECTED_TYPE;
          break;
        }
        typeType = 2;
      }
      else if (tokenType == TokenType::REFERENCE) {
        if (typeType == 1) {
          errorType = CheckerErrorType::CANNOT_REF_A_REF;
          break;
        } else if (typeType == 2) {
          errorType = CheckerErrorType::CANNOT_PTR_A_REF;
          break;
        } else if (typeType == 3) {
          errorType = CheckerErrorType::UNEXPECTED_TYPE;
          break;
        }
        typeType = 1;
      }
      else {
        if (typeType == 3) {
          errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
          break;
        }
        typeType = 3;
      }
    } else {
      if (typeType == 3) {
        errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
        break;
      }
      Declaration* &typeDec = lookUp[tokenizer.extractToken(list->curr)];
      if (!typeDec) {
        errorType = CheckerErrorType::NO_SUCH_TYPE;
        break;
      }
      if (typeDec->decType != DecType::STRUCT) {
        errors.emplace_back(CheckerErrorType::EXPECTING_TYPE, &list->curr, typeDec);
        return false;
      }
      typeType = 3;
    }
    list = list->next;
  } while (list);
  if (errorType == CheckerErrorType::NONE) {
    return true;
  }
  errors.emplace_back(errorType, &list->curr);
  return false;
}

TokenList *Checker::checkStatement(Statement& statement) {
  switch (statement.type) {
    case StatementType::UNARY_OP: {
    }

    case StatementType::BINARY_OP: {
      //TokenList *leftSide = checkStatement(statement.binOp->leftSide);
      //TokenList *rightSide = checkStatement(statement.binOp->rightSide);
      // do conversions
      break;
    }

    case StatementType::VARIABLE_DEC: {
    }

    case StatementType::FUNCTION_CALL: {
      Declaration*& dec = lookUp[tokenizer.extractToken(statement.funcCall->name)];
      if (!dec) {
        // dec does not exist
        errors.emplace_back(CheckerErrorType::NO_SUCH_FUNCTION, &statement.funcCall->name);
      } else if (dec->decType != DecType::FUNCTION) {
        // not a function
        errors.emplace_back(CheckerErrorType::NOT_A_FUNCTION, &statement.funcCall->name, dec);
      }
      // valid function, now check parameters
      // parameters are already validated before on second top level scan. so assume the statements are all varDecs and valid
      StatementList* paramList = &dec->func->params;
      StatementList* argList = &statement.funcCall->args;
      // have to get type of each argument since they are statements themselves
      do {
        TokenList *resultingType = checkStatement(argList->curr);
        if (!resultingType) {
          // there was an error in the parameter, return
          return nullptr;
        }
        if (!(paramList->curr.dec->varDec->type.tokens == *resultingType)) {
          // types dont match
          errors.emplace_back(CheckerErrorType::TYPE_DOES_NOT_MATCH, &argList->curr, paramList->curr.dec);
          return nullptr;
        }
        // types match. yay
        paramList = paramList->next;
        argList = argList->next;
      } while (argList && paramList);
      if (argList || paramList) {
        errors.emplace_back(CheckerErrorType::WRONG_NUMBER_OF_ARGS, &statement.funcCall->name, dec);
        return nullptr;
      }
      // valid parameters. return returnType: assume this was also checked and valid
      return &dec->func->returnType.tokens;
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
          errors.emplace_back(CheckerErrorType::NO_SUCH_VARIABLE, statement.var);
          break;
        }
        if (decPtr->decType != DecType::VARIABLE_DEC) {
          errors.emplace_back(CheckerErrorType::NOT_A_VARIABLE, statement.var, decPtr);
          break;
        }
        return &decPtr->varDec->type.tokens;
      } else if (statement.var->type == TokenType::DECIMAL_NUMBER) {
        // need to get the actual number and see if it fits in a 32bit int, if not, unsigned, if not, 64bit
        // for now, just dump all numbers as ints
        return &intValue;
      } else if (statement.var->type == TokenType::NULL_PTR) {
        return &nullptrValue;
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
