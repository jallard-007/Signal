#include "checker.hpp"

CheckerError::CheckerError(CheckerErrorType type, Token *token): token{token}, type{type}, dec{} {}
CheckerError::CheckerError(CheckerErrorType type, Token *token, GeneralDec *decPtr): token{token}, type{type}, dec{decPtr} {}

Checker::Checker(Program& prog, Tokenizer& tk, NodeMemPool& mem):
structsLookUp{}, lookUp{}, errors{}, program{prog}, tokenizer{tk}, memPool{mem} {}

/**
 * Scans all global declarations and registers them in the table, checking that the name is available
 * Also registers struct members in the struct's table
*/
void Checker::firstTopLevelScan() {
  for (GeneralDecList *list = &program.decs; list; list = list->next) {
    switch (list->curr.type) {
      case GeneralDecType::FUNCTION: {
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(list->curr.funcDec->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &list->curr.funcDec->name, decPtr);
        } else {
          decPtr = &list->curr;
        }
        break;
      }
      case GeneralDecType::VARIABLE: {
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(list->curr.varDec->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &list->curr.varDec->name, decPtr);
        } else {
          decPtr = &list->curr;
        }
        break;
      }
      case GeneralDecType::STRUCT: {
        const std::string structName = tokenizer.extractToken(list->curr.structDec->name);
        GeneralDec* &decPtr = lookUp[structName];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &list->curr.structDec->name, decPtr);
        } else {
          decPtr = &list->curr;
          auto& structDecLookUp = structsLookUp[structName];
          for (StructDecList* inner = &list->curr.structDec->decs; inner; inner = inner->next) {
            if (inner->type == StructDecType::VAR) {
              StructDecList* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner->varDec.name)];
              if (innerStructDecPtr) {
                errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &inner->varDec.name, innerStructDecPtr);
              } else {
                innerStructDecPtr = inner;
              }
            }
            // inner->type == StructDecType::FUNC
            else {
              StructDecList* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner->funcDec.name)];
              if (innerStructDecPtr) {
                errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &inner->funcDec.name, innerStructDecPtr);
              } else {
                innerStructDecPtr = inner;
              }
            }
          }
        }

        break;
      }
      case GeneralDecType::TEMPLATE: {
        Token *tk = nullptr;
        if (list->curr.tempDec->isStruct) {
         tk = &list->curr.tempDec->structDec.name;
        } 
        // dec.temp->dec.decType == DecType::FUNCTION
        else {
         tk = &list->curr.tempDec->funcDec.name;
        }
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(*tk)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, tk, decPtr);
        } else {
          decPtr = &list->curr;
        }
      }
      case GeneralDecType::ENUM: {
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
  if (funcDec.params.curr.type != StatementType::NOTHING) {
    StatementList* params = &funcDec.params;
    do {
      if (!checkType(params->curr.varDec->type)) {
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
  for (GeneralDecList* list = &program.decs; list; list = list->next) {
    switch (list->curr.type) {
      case GeneralDecType::FUNCTION: {
        list->curr.isValid = validateFunctionHeader(*list->curr.funcDec);
        break;
      }
      case GeneralDecType::VARIABLE: {
        list->curr.isValid = checkType(list->curr.varDec->type);
        break;
      }
      case GeneralDecType::STRUCT: {
        for (StructDecList *inner = &list->curr.structDec->decs; inner; inner = inner->next) {
          if (inner->type == StructDecType::VAR) {
            inner->isValid = checkType(inner->varDec.type);
          }
          // inner.decType == DecType::FUNC
          else {
            inner->isValid = validateFunctionHeader(inner->funcDec);
          }
        }
        break;
      }
      case GeneralDecType::TEMPLATE: {
        // parser validates that there is at least one type
        std::vector<std::string> templateTypes;
        TokenList *templateIdentifiers = &list->curr.tempDec->templateTypes;
        do {
          templateTypes.push_back(tokenizer.extractToken(templateIdentifiers->token));
          GeneralDec *&tempTypeDec = lookUp[templateTypes.back()];
          if (tempTypeDec) {
            errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &templateIdentifiers->token, tempTypeDec);
          } else {
            tempTypeDec = memPool.makeGlobalDec();
            tempTypeDec->type = GeneralDecType::STRUCT;
          }
          templateIdentifiers = templateIdentifiers->next;
        } while (templateIdentifiers);

        if (list->curr.tempDec->isStruct) {
          bool isValid = true;
          for (StructDecList *inner = &list->curr.tempDec->structDec.decs; list; list = list->next) {
            if (inner->type == StructDecType::VAR) {
              inner->isValid = checkType(inner->varDec.type);
              if (!inner->isValid) {
                isValid = false;
              }
            }
            // inner->type == StructDecType::FUNC
            else {
              inner->isValid= validateFunctionHeader(inner->funcDec);
              if (!inner->isValid) {
                isValid = false;
              }
            }
          }
          list->curr.isValid = isValid;
        }
        else {
          list->curr.isValid = validateFunctionHeader(list->curr.tempDec->funcDec);
        }

        while (!templateTypes.empty()) {
          auto tempType = lookUp.find(templateTypes.back());
          memPool.release(tempType->second);
          lookUp.erase(tempType);
          templateTypes.pop_back();
        }
        break;
      }
      case GeneralDecType::ENUM: {
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
  if (funcDec.params.curr.type != StatementType::NOTHING) {
    StatementList *list = &funcDec.params;
    while (list) {
      locals.emplace_back(tokenizer.extractToken(list->curr.varDec->name));
      GeneralDec* &paramDec = lookUp[locals.back()];
      if (paramDec) {
        errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, &list->curr.varDec->name, paramDec);
      } else {
        // name is available, check type
        if (!checkType(list->curr.varDec->type)) {
          wasThereAnError = true;
        }
        paramDec = memPool.makeGlobalDec();
        paramDec->varDec = list->curr.varDec;
        paramDec->type = GeneralDecType::VARIABLE;
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
bool Checker::checkScope(Scope& scope, std::vector<std::string>& locals, TokenList& returnType, bool isReturnRequired, bool isLoop, bool isSwitch) {
  const uint32_t marker = locals.size();
  StatementList* list = &scope.scopeStatements;
  do {
    switch (list->curr.type) {
      case StatementType::CONTROL_FLOW: {
      }
      case StatementType::EXPRESSION: {
        checkExpression(*list->curr.expression);
        break;
      }
      case StatementType::KEYWORD: {
        if (list->curr.keyword->type == TokenType::CONTINUE) {
          if (!isLoop) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_CONTINUE_HERE, list->curr.keyword);
          }
          break;
        }
        else if (list->curr.keyword->type == TokenType::BREAK) {
          if (!isLoop && !isSwitch) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_BREAK_HERE, list->curr.keyword);
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

TokenList *Checker::checkExpression(Expression& expression) {
  switch(expression.type) {
    case ExpressionType::BINARY_OP: {
        // logical
      // EQUAL,
      // NOT_EQUAL,
      // LOGICAL_AND,
      // LOGICAL_OR,
      // LESS_THAN,
      // LESS_THAN_EQUAL,
      // GREATER_THAN,
      // GREATER_THAN_EQUAL,



      TokenList *leftSide = checkExpression(expression.binOp->leftSide);
      TokenList *rightSide = checkExpression(expression.binOp->rightSide);
      break;
    }
    case ExpressionType::UNARY_OP: {
      // NOT,
      // ADDRESS_OF,
      // DEREFERENCE,
      // INCREMENT_POSTFIX,
      // INCREMENT_PREFIX,
      // DECREMENT_POSTFIX,
      // DECREMENT_PREFIX,
      // NEGATIVE,
      TokenList *sd;
      break;
    }
    case ExpressionType::VALUE: {
      if (expression.value->type == TokenType::IDENTIFIER) {
        GeneralDec *&decPtr = lookUp[tokenizer.extractToken(*expression.value)];
        if (!decPtr) {
          errors.emplace_back(CheckerErrorType::NO_SUCH_VARIABLE, expression.value);
          break;
        }
        if (decPtr->type != GeneralDecType::VARIABLE) {
          errors.emplace_back(CheckerErrorType::NOT_A_VARIABLE, expression.value, decPtr);
          break;
        }
        return &decPtr->varDec->type;
      } else if (expression.value->type == TokenType::DECIMAL_NUMBER) {
        // need to get the actual number and see if it fits in a 32bit int, if not, unsigned, if not, 64bit
        // for now, just dump all numbers as ints
        return &intValue;
      } else if (expression.value->type == TokenType::NULL_PTR) {
        return &nullptrValue;
      } else if (expression.value->type == TokenType::FALSE || expression.value->type == TokenType::TRUE) {
        return &boolValue;
      } else if (expression.value->type == TokenType::STRING_LITERAL) {
        return &stringValue;
      } else if (expression.value->type == TokenType::CHAR_LITERAL) {
        return &charValue;
      }
      break;
    }
    case ExpressionType::FUNCTION_CALL: {
      GeneralDec* &dec = lookUp[tokenizer.extractToken(expression.funcCall->name)];
      if (!dec) {
        // dec does not exist
        errors.emplace_back(CheckerErrorType::NO_SUCH_FUNCTION, &expression.funcCall->name);
      } else if (dec->type != GeneralDecType::FUNCTION) {
        // not a function
        errors.emplace_back(CheckerErrorType::NOT_A_FUNCTION, &expression.funcCall->name, dec);
      }
      // valid function, now check parameters
      // parameters are already validated on second top level scan. so assume the statements are all varDecs and valid
      StatementList* paramList = &dec->funcDec->params;
      ExpressionList* argList = &expression.funcCall->args;
      // have to get type of each argument since they are statements themselves
      do {
        TokenList *resultingType = checkExpression(argList->curr);
        if (!resultingType) {
          // there was an error in the parameter, return
          return nullptr;
        }
        if (!(paramList->curr.varDec->type == *resultingType)) {
          // types dont match
          errors.emplace_back(CheckerErrorType::TYPE_DOES_NOT_MATCH, &argList->curr, dec);
          return nullptr;
        }
        // types match. yay
        paramList = paramList->next;
        argList = argList->next;
      } while (argList && paramList);
      if (argList || paramList) {
        errors.emplace_back(CheckerErrorType::WRONG_NUMBER_OF_ARGS, &expression.funcCall->name, dec);
        return nullptr;
      }
      // valid parameters. return returnType: assume this was also checked and valid
      return &dec->funcDec->returnType;
    }
    case ExpressionType::ARRAY_ACCESS: {
      break;
    }
    case ExpressionType::WRAPPED: {
      return checkExpression(*expression.wrapped);
    }
    case ExpressionType::ARRAY_OR_STRUCT_LITERAL: {
      break;
    }
    default: {
      return &voidValue;
    }
  }
  return &voidValue;
}

/**
 * Validates a type
 * \param type the type to check
 * \returns true if the type is a valid type, false otherwise (adds the error to errors)
 * \note in the case of the type being just 'void', will return false even though it is valid for function return types.
 *  check if the emplaced error is 'void' and remove it if called for a function return type
*/
bool Checker::checkType(TokenList& type) {
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
  TokenList *list = &type;
  do {
    if (isBuiltInType(list->token.type)) {
      const TokenType tokenType = list->token.type;
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
      GeneralDec* &typeDec = lookUp[tokenizer.extractToken(list->token)];
      if (!typeDec) {
        errorType = CheckerErrorType::NO_SUCH_TYPE;
        break;
      }
      if (typeDec->type != GeneralDecType::STRUCT) {
        errors.emplace_back(CheckerErrorType::EXPECTING_TYPE, &list->token, typeDec);
        return false;
      }
      typeType = 3;
    }
    list = list->next;
  } while (list);
  if (errorType == CheckerErrorType::NONE) {
    return true;
  }
  errors.emplace_back(errorType, &list->token);
  return false;
}

bool Checker::checkStatement(Statement& statement) {
  switch (statement.type) {
    case StatementType::VARIABLE_DEC: {
    }

    case StatementType::KEYWORD: {
    }


    default: break;
  }
  return true;
}

bool canBeConvertedToBool(TokenList& type) {
  TokenList *actual = &type;
  if (type.next) {
    if (type.next->token.type == TokenType::REFERENCE) {
      // move to next;
      actual = type.next;
    }
  }
  if (isBuiltInType(actual->token.type)) {
    const TokenType tokenType = actual->token.type;
    if (tokenType == TokenType::FLOAT_TYPE || tokenType == TokenType::DOUBLE_TYPE || tokenType == TokenType::VOID) {
      return false;
    }
    return true;
  }
  // custom type
  return true;
}
