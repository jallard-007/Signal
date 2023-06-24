#include "checker.hpp"
#include <iostream>

Token getTokenOfExpression(Expression& exp) {
  switch (exp.type) {
    case ExpressionType::ARRAY_ACCESS: {
      return exp.arrAccess->array;
    }
    case ExpressionType::ARRAY_OR_STRUCT_LITERAL: {
      return getTokenOfExpression(exp.arrayOrStruct->values.curr);
    }
    case ExpressionType::BINARY_OP: {
      return exp.binOp->op;
    }
    case ExpressionType::FUNCTION_CALL: {
      return exp.funcCall->name;
    }
    case ExpressionType::UNARY_OP: {
      return exp.unOp->op;
    }
    case ExpressionType::VALUE: {
      return exp.value;
    }
    case ExpressionType::WRAPPED: {
      return getTokenOfExpression(*exp.wrapped);
    }
    default: {
      return {999,999,TokenType::BAD_VALUE};
    }
  }
}

TokenList Checker::badValue {Token{0,0,TokenType::BAD_VALUE}};
TokenList Checker::boolValue {Token{0,0,TokenType::BOOL}};
TokenList Checker::int32Value {Token{0,0,TokenType::INT32_TYPE}};
TokenList Checker::uint32Value {Token{0,0,TokenType::UINT32_TYPE}};
TokenList Checker::int64Value {Token{0,0,TokenType::INT64_TYPE}};
TokenList Checker::uint64Value {Token{0,0,TokenType::UINT64_TYPE}};
TokenList Checker::charValue {Token{0,0,TokenType::CHAR_TYPE}};
TokenList Checker::stringValue {Token{0,0,TokenType::POINTER}, &Checker::charValue};
TokenList Checker::floatValue {Token{0,0,TokenType::FLOAT_TYPE}};
TokenList Checker::doubleValue {Token{0,0,TokenType::DOUBLE_TYPE}};
TokenList Checker::voidValue {Token{0,0,TokenType::VOID}};
TokenList Checker::ptrValue {Token{0,0,TokenType::POINTER}, &Checker::voidValue};
TokenList Checker::nullptrValue {Token{0,0,TokenType::NULL_PTR}};

CheckerError::CheckerError(CheckerErrorType type, Token token): token{token}, dec{nullptr}, type{type}  {}
CheckerError::CheckerError(CheckerErrorType type, Token token, GeneralDec *decPtr): token{token}, dec{decPtr}, type{type} {}
CheckerError::CheckerError(CheckerErrorType type, Expression *expression): dec{}, type{type} {
  token = getTokenOfExpression(*expression);
}
CheckerError::CheckerError(CheckerErrorType type, Expression *expression, GeneralDec *decPtr): dec{decPtr}, type{type} {
  token = getTokenOfExpression(*expression);
}

std::string CheckerError::getErrorMessage(Tokenizer& tk, const std::string& file) {
  std::cout << tk.extractToken(token) << '\n';

  TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
  std::string message = file + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
  switch (type) {
    case CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL: message += "Value cannot be converted to boolean\n"; break;
    case CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE: message += "Cannot dereference non-pointer type\n"; break;
    case CheckerErrorType::CANNOT_HAVE_BREAK_HERE: message += "\"break\" can only be inside loops and switch cases\n"; break;
    case CheckerErrorType::CANNOT_HAVE_CONTINUE_HERE: message += "\"continue\" can only be inside loops\n"; break;
    case CheckerErrorType::CANNOT_HAVE_MULTI_TYPE: message += "Multi type not allowed\n"; break;
    case CheckerErrorType::CANNOT_OPERATE_ON_TEMPORARY: message += "Cannot perform this operation on a temporary value\n"; break;
    case CheckerErrorType::CANNOT_ASSIGN_TO_TEMPORARY: message += "Cannot assign a value to a temporary value\n"; break;
    case CheckerErrorType::CANNOT_PTR_A_REF: message += "Cannot have a pointer to a reference type\n"; break;
    case CheckerErrorType::CANNOT_REF_A_REF: message += "Cannot have a reference to a reference type\n"; break;
    case CheckerErrorType::NAME_ALREADY_IN_USE: message += "Name already in use\n"; break;
    case CheckerErrorType::NO_SUCH_FUNCTION: message += "Function does not exist\n"; break;
    case CheckerErrorType::NO_SUCH_MEMBER_FUNCTION: message += "Member function does not exist\n"; break;
    case CheckerErrorType::NO_SUCH_MEMBER_VARIABLE: message += "Member variable does not exist\n"; break;
    case CheckerErrorType::NO_SUCH_TYPE: message += "No such type\n"; break;
    case CheckerErrorType::NO_SUCH_TEMPLATE: message += "No such template\n"; break;
    case CheckerErrorType::NO_SUCH_VARIABLE: message += "No such variable\n"; break;
    case CheckerErrorType::NOT_A_FUNCTION: message += "Not a function\n"; break;
    case CheckerErrorType::NOT_A_STRUCT: message += "Not a type\n"; break;
    case CheckerErrorType::NOT_A_TEMPLATE: message += "Not a template\n"; break;
    case CheckerErrorType::NOT_A_VARIABLE: message += "Not a variable\n"; break;
    case CheckerErrorType::TYPE_DOES_NOT_MATCH: message += "Type does not match\n"; break;
    case CheckerErrorType::UNEXPECTED_TYPE: message += "Unexpected type\n"; break;
    case CheckerErrorType::VOID_TYPE: message += "Void type not allowed\n"; break;
    case CheckerErrorType::WRONG_NUMBER_OF_ARGS: message += "Incorrect number of arguments\n"; break;
    default: message += "Error of some kind, sorry bro\n"; break;
  }
  if (dec) {
    message += "Declaration defined as such:\n  ";
    dec->prettyPrintDefinition(tk, message);
    message += "\n\n";
  } else {
    message += '\n';
  }
  return message;
}

ResultingType::ResultingType(TokenList* type, bool isLValue): type{type}, isLValue{isLValue} {}

Checker::Checker(Program& prog, Tokenizer& tk, NodeMemPool& mem):
structsLookUp{}, lookUp{}, errors{}, program{prog}, tokenizer{tk}, memPool{mem} {}

bool Checker::check() {
  firstTopLevelScan();
  if (!errors.empty()) {
    return false;
  }
  secondTopLevelScan();
  if (!errors.empty()) {
    return false;
  }
  fullScan();
  if (!errors.empty()) {
    return false;
  }
  return true;
}

/**
 * Scans all global declarations and registers them in the table, checking that the name is available
 * Also registers struct members in the struct table
*/
void Checker::firstTopLevelScan() {
  for (GeneralDecList *list = &program.decs; list; list = list->next) {
    switch (list->curr.type) {
      case GeneralDecType::FUNCTION: {
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(list->curr.funcDec->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.funcDec->name, decPtr);
        } else {
          decPtr = &list->curr;
        }
        break;
      }
      case GeneralDecType::VARIABLE: {
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(list->curr.varDec->name)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.varDec->name, decPtr);
        } else {
          decPtr = &list->curr;
        }
        break;
      }
      case GeneralDecType::STRUCT: {
        const std::string structName = tokenizer.extractToken(list->curr.structDec->name);
        GeneralDec* &decPtr = lookUp[structName];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.structDec->name, decPtr);
        } else {
          decPtr = &list->curr;
          auto& structDecLookUp = structsLookUp[structName];
          for (StructDecList* inner = &list->curr.structDec->decs; inner; inner = inner->next) {
            StructDecList** innerStructDecPtr;
            Token tk;
            if (inner->type == StructDecType::VAR) {
              tk = inner->varDec.name;
              innerStructDecPtr = &structDecLookUp[tokenizer.extractToken(inner->varDec.name)];
            } else {
              // inner->type == StructDecType::FUNC
              tk = inner->funcDec.name;
              innerStructDecPtr = &structDecLookUp[tokenizer.extractToken(inner->funcDec.name)];
            }
            if (*innerStructDecPtr) {
              GeneralDec *errorDec = memPool.makeGeneralDec();
              if ((*innerStructDecPtr)->type == StructDecType::FUNC) {
                errorDec->type = GeneralDecType::FUNCTION;
                errorDec->funcDec = &(*innerStructDecPtr)->funcDec;
              } else {
                errorDec->type = GeneralDecType::VARIABLE;
                errorDec->varDec = &(*innerStructDecPtr)->varDec;
              }
              errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, tk, errorDec);
            } else {
              *innerStructDecPtr = inner;
            }
          }
        }

        break;
      }
      case GeneralDecType::TEMPLATE: {
        Token tk{0,0,TokenType::NOTHING};
        if (list->curr.tempDec->isStruct) {
          tk = list->curr.tempDec->structDec.name;
        } else {
          // dec.temp->dec.decType == DecType::FUNCTION
          tk = list->curr.tempDec->funcDec.name;
        }
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(tk)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, tk, decPtr);
        } else {
          decPtr = &list->curr;
        }
        break;
      }
      case GeneralDecType::TEMPLATE_CREATE: {
        GeneralDec* &decPtr = lookUp[tokenizer.extractToken(list->curr.tempCreate->typeName)];
        if (decPtr) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.tempCreate->typeName, decPtr);
        } else {
          decPtr = &list->curr;
        }
        break;
      }
      default: {
        break;
      }
    }
  }
}

/**
 * Validates function types, global variable types, struct member variable types, struct member function types.
 * Everything that was registered in the first pass
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
        list->curr.isValid = validateStructTopLevel(list->curr.tempDec->structDec.decs);
        break;
      }
      case GeneralDecType::TEMPLATE: {
        // parser validates that there is at least one type
        std::vector<std::string> templateTypes;
        TokenList *templateIdentifiers = &list->curr.tempDec->templateTypes;
        // add templated types to global lookup
        do {
          templateTypes.push_back(tokenizer.extractToken(templateIdentifiers->token));
          GeneralDec *&tempTypeDec = lookUp[templateTypes.back()];
          if (tempTypeDec) {
            errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, templateIdentifiers->token, tempTypeDec);
            list->curr.isValid = false;
            break;
          }
          tempTypeDec = memPool.makeGeneralDec();
          tempTypeDec->type = GeneralDecType::STRUCT;
          templateIdentifiers = templateIdentifiers->next;
        } while (templateIdentifiers);
        // validate top level types
        if (!list->curr.isValid) {
          break;
        }
        if (list->curr.tempDec->isStruct) {
          list->curr.isValid = validateStructTopLevel(list->curr.tempDec->structDec.decs);
        } else {
          list->curr.isValid = validateFunctionHeader(list->curr.tempDec->funcDec);
        }
        // remove templated types
        while (!templateTypes.empty()) {
          auto tempType = lookUp.find(templateTypes.back());
          memPool.release(tempType->second);
          lookUp.erase(tempType);
          templateTypes.pop_back();
        }
        break;
      }
      case GeneralDecType::TEMPLATE_CREATE: {
        // check that the template exists
        GeneralDec* dec = lookUp[tokenizer.extractToken(list->curr.tempCreate->templateName)];
        if (!dec) {
          errors.emplace_back(CheckerErrorType::NO_SUCH_TEMPLATE, list->curr.tempCreate->templateName);
          list->curr.isValid = false;
          break;
        } else if (dec->type != GeneralDecType::TEMPLATE) {
          errors.emplace_back(CheckerErrorType::NOT_A_TEMPLATE, list->curr.tempCreate->templateName, dec);
          list->curr.isValid = false;
          break;
        }
        // check that the number of types match and that the types exist
        TokenList *tempList = &dec->tempDec->templateTypes, *createList = &list->curr.tempCreate->templateTypes;
        for (;tempList && createList; tempList = tempList->next, createList = createList->next) {
          GeneralDec *templateType = lookUp[tokenizer.extractToken(createList->token)];
          if (!templateType) {
            errors.emplace_back(CheckerErrorType::NO_SUCH_TYPE, createList->token);
            list->curr.isValid = false;
            break;
          }
        }
        if (tempList || createList) {
          if (createList) {
            errors.emplace_back(CheckerErrorType::WRONG_NUMBER_OF_ARGS, createList->token, dec);
          } else {
            errors.emplace_back(CheckerErrorType::WRONG_NUMBER_OF_ARGS, list->curr.tempCreate->templateTypes.token, dec);
          }
          list->curr.isValid = false;
          break;
        }
        break;
        // have to deep copy the template declaration, replace all occurrences of templated types with the actual
      }

      default: break;
    }

  }
}

void Checker::fullScan() {
  for (GeneralDecList* list = &program.decs; list; list = list->next) {
    switch (list->curr.type) {
      case GeneralDecType::FUNCTION: {
        checkFunction(*list->curr.funcDec);
        break;
      }
      
      default: break;
    }
  }
}

bool Checker::validateFunctionHeader(FunctionDec &funcDec) {
  bool valid = true;
  // check return type
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

bool Checker::validateStructTopLevel(StructDecList& innerDecs) {
  bool isValid = true;
  for (StructDecList *inner = &innerDecs; inner; inner = inner->next) {
    if (inner->type == StructDecType::VAR) {
      inner->isValid = checkType(inner->varDec.type);
      if (!inner->isValid) {
        isValid = false;
      }
    }
    // inner.decType == DecType::FUNC
    else {
      inner->isValid = validateFunctionHeader(inner->funcDec);
      if (!inner->isValid) {
        isValid = false;
      }
    }
  }
  return isValid;
}

/**
 * Validates the internals of a function
 * \param funcDec the function declaration to check
 * \returns true if the function is valid
 */
bool Checker::checkFunction(FunctionDec& funcDec) {
  // validate parameter names
  std::vector<std::string> locals;
  if (funcDec.params.curr.type != StatementType::NOTHING) {
    StatementList *list = &funcDec.params;
    while (list) {
      locals.emplace_back(tokenizer.extractToken(list->curr.varDec->name));
      GeneralDec* &paramDec = lookUp[locals.back()];
      if (paramDec) {
        errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.varDec->name, paramDec);
        return false;
      }
      // type already checked on second top level scan, just add it
      paramDec = memPool.makeGeneralDec();
      paramDec->varDec = list->curr.varDec;
      paramDec->type = GeneralDecType::VARIABLE;
      list = list->next;
    }
  }
  bool requireReturn = funcDec.returnType.token.type != TokenType::VOID;
  bool valid = checkScope(funcDec.body, locals, funcDec.returnType, requireReturn, false, false);
  while (!locals.empty()) {
    // remove locals from table
    lookUp.erase(locals.back());
    locals.pop_back();
  }
  return valid;
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
  bool valid = true;
  bool wasReturned = true;
  do {
    switch (list->curr.type) {
      case StatementType::CONTROL_FLOW: {
        switch (list->curr.controlFlow->type) {
          // ForLoop forLoop;
          case ControlFlowStatementType::FOR_LOOP: {
            break;
          }
          case ControlFlowStatementType::CONDITIONAL_STATEMENT: {
            break;

          }
          case ControlFlowStatementType::RETURN_STATEMENT: {
            break;

          }
          case ControlFlowStatementType::SWITCH_STATEMENT: {
            break;

          }
          case ControlFlowStatementType::WHILE_LOOP: {
            break;

          }
          case ControlFlowStatementType::NONE: {
            break;
          }
        }
      }
      
      case StatementType::EXPRESSION: {
        if (checkExpression(*list->curr.expression).type->token.type == TokenType::BAD_VALUE) {
          valid = false;
        }
        break;
      }
      
      case StatementType::KEYWORD: {
        if (list->curr.keyword.type == TokenType::CONTINUE) {
          if (!isLoop) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_CONTINUE_HERE, list->curr.keyword);
            valid = false;
          }
          break;
        }
        else if (list->curr.keyword.type == TokenType::BREAK) {
          if (!isLoop && !isSwitch) {
            errors.emplace_back(CheckerErrorType::CANNOT_HAVE_BREAK_HERE, list->curr.keyword);
            valid = false;
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
          valid = false;
        }
        break;
      }

      case StatementType::VARIABLE_DEC: {
        // add local to table
        GeneralDec*& dec = lookUp[tokenizer.extractToken(list->curr.varDec->name)];
        if (dec) {
          errors.emplace_back(CheckerErrorType::NAME_ALREADY_IN_USE, list->curr.varDec->name, dec);
          valid = false;
          break;
        }
        if (!checkType(list->curr.varDec->type)) {
          valid = false;
          break;
        }
        dec = memPool.makeGeneralDec();
        dec->type = GeneralDecType::VARIABLE;
        dec->varDec = list->curr.varDec;
        if (dec->varDec->initialAssignment) {
          ResultingType expressionType = checkExpression(*list->curr.varDec->initialAssignment);
          if (expressionType.type->token.type == TokenType::BAD_VALUE) {
            valid = false;
          }
          else {
            ResultingType varType {&list->curr.varDec->type, true};
            if (!checkAssignment(varType, expressionType)) {
              errors.emplace_back(CheckerErrorType::CANNOT_ASSIGN, list->curr.varDec->initialAssignment);
              valid = false;
            }
          }
        }
        break;
      }
      
      default: {
        exit(1);
      }
    }
    list = list->next;
  } while (list);
  while (marker < locals.size()) {
    // remove locals from table
    lookUp.erase(locals.back());
    locals.pop_back();
  }
  if (isReturnRequired && !wasReturned) {
    return false;
  }
  return valid;
}

/**
 * Returns the resulting type from an expression
 * the ResultingType always contains a valid pointer
 * \param structMap pointer to a struct's lookup map. only used for the right side of binary member access operators
*/
ResultingType Checker::checkExpression(Expression& expression, std::map<std::string, StructDecList *>* structMap) {
  switch(expression.type) {
    case ExpressionType::BINARY_OP: {
      ResultingType leftSide = checkExpression(expression.binOp->leftSide);

      if (expression.binOp->op.type == TokenType::LOGICAL_AND || expression.binOp->op.type == TokenType::LOGICAL_OR) {
        if (leftSide.type->token.type != TokenType::BAD_VALUE) {
          if (!canBeConvertedToBool(*leftSide.type)) {
            errors.emplace_back(CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, &expression.binOp->leftSide);
          }
        }
        ResultingType rightSide = checkExpression(expression.binOp->rightSide);
        if (!canBeConvertedToBool(*rightSide.type)) {
          errors.emplace_back(CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, &expression.binOp->rightSide);
        }
        return {&boolValue, false};
      }
      
      if (isLogicalOp(expression.binOp->op.type)) {
        if (leftSide.type->token.type == TokenType::IDENTIFIER || leftSide.type->token.type == TokenType::VOID) {
          errors.emplace_back(CheckerErrorType::CANNOT_COMPARE_TYPE, &expression.binOp->leftSide);
        }
        ResultingType rightSide = checkExpression(expression.binOp->leftSide);
        if (rightSide.type->token.type == TokenType::IDENTIFIER || rightSide.type->token.type == TokenType::VOID) {
          errors.emplace_back(CheckerErrorType::CANNOT_COMPARE_TYPE, &expression.binOp->rightSide);
        }
        return {&boolValue, false};
      }

      // member access or number with decimal
      if (expression.binOp->op.type == TokenType::DOT) {
        TokenType tkType = leftSide.type->token.type;
        if (tkType == TokenType::DECIMAL_NUMBER || tkType == TokenType::HEX_NUMBER || tkType == TokenType::BINARY_NUMBER) {
          if (expression.binOp->rightSide.type != ExpressionType::VALUE) {
            errors.emplace_back(CheckerErrorType::EXPECTING_NUMBER, &expression.binOp->rightSide);
          }
          else {
            tkType = expression.binOp->rightSide.value.type;
            if (tkType != TokenType::DECIMAL_NUMBER || tkType == TokenType::HEX_NUMBER || tkType == TokenType::BINARY_NUMBER) {
              errors.emplace_back(CheckerErrorType::EXPECTING_NUMBER, &expression.binOp->rightSide);
            }
          }
          return {&doubleValue, false};
        } else {
          if (leftSide.type->token.type == TokenType::BAD_VALUE) {
            return {&badValue, false};
          }
          return checkMemberAccess(leftSide, expression);
        }
      }
      
      // pointer member access
      if (expression.binOp->op.type == TokenType::PTR_MEMBER_ACCESS) {
        if (leftSide.type->token.type == TokenType::BAD_VALUE) {
          return {&badValue, false};
        }
        if (leftSide.type->token.type != TokenType::POINTER) {
          errors.emplace_back(CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, expression.binOp->op);
          return {&badValue, false};
        }
        return checkMemberAccess(leftSide, expression);
      }

      ResultingType rightSide = checkExpression(expression.binOp->rightSide);
      if (isAssignment(expression.binOp->op.type)) {
        if (!leftSide.isLValue) {
          errors.emplace_back(CheckerErrorType::CANNOT_ASSIGN_TO_TEMPORARY, &expression.binOp->leftSide);
        }
        else if (!checkAssignment(leftSide, rightSide)) {
          errors.emplace_back(CheckerErrorType::CANNOT_ASSIGN, &expression);
        }
        return {leftSide.type, true};
      }

      if (leftSide.type->token.type == TokenType::BAD_VALUE && rightSide.type->token.type == TokenType::BAD_VALUE) {
        return {&badValue, false};
      } else if (leftSide.type->token.type == TokenType::BAD_VALUE) {
        return {rightSide.type, false};
      } else if (rightSide.type->token.type == TokenType::BAD_VALUE) {
        return {leftSide.type, false};
      }

      if (leftSide.type->token.type == TokenType::IDENTIFIER || rightSide.type->token.type == TokenType::IDENTIFIER) {
        errors.emplace_back(CheckerErrorType::OPERATION_NOT_DEFINED, &expression);
        return {&badValue, false};
      }
      if (leftSide.type->token.type == TokenType::VOID || rightSide.type->token.type == TokenType::VOID) {
        errors.emplace_back(CheckerErrorType::OPERATION_ON_VOID, &expression);
        return {&badValue, false};
      }
      TokenList& largest = largestType(*leftSide.type, *rightSide.type);
      if (largest.token.type < TokenType::INT32_TYPE) {
        return {&int32Value, false};
      }
      return {&largest, false};
    }
    
    case ExpressionType::UNARY_OP: {
      if (expression.unOp->op.type == TokenType::DEREFERENCE) {
        ResultingType res = checkExpression(expression.unOp->operand);
        if (res.type->token.type != TokenType::POINTER) {
          errors.emplace_back(CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, expression.unOp->op);
        }
        return {res.type->next, true};
      }
      if (expression.unOp->op.type == TokenType::NOT) {
        ResultingType res = checkExpression(expression.unOp->operand);
        if (!canBeConvertedToBool(*res.type)) {
          errors.emplace_back(CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, expression.unOp->op);
        }
        return {&boolValue, false};
      }
      if (expression.unOp->op.type == TokenType::ADDRESS_OF || expression.unOp->op.type == TokenType::INCREMENT_POSTFIX || expression.unOp->op.type == TokenType::INCREMENT_PREFIX || expression.unOp->op.type == TokenType::DECREMENT_PREFIX || expression.unOp->op.type == TokenType::DECREMENT_POSTFIX) {
        ResultingType res = checkExpression(expression.unOp->operand);
        if (!res.isLValue) {
          errors.emplace_back(CheckerErrorType::CANNOT_OPERATE_ON_TEMPORARY, expression.unOp->op);
        }
        if (expression.unOp->op.type == TokenType::ADDRESS_OF) {
          TokenList *ptrToType = memPool.makeTokenList();
          ptrToType->token.type = TokenType::POINTER;
          ptrToType->next = res.type;
          return {ptrToType, false};
        }
        return {res.type, false};
      }
      if (expression.unOp->op.type == TokenType::NEGATIVE) {
        // nothing for now
        return {checkExpression(expression.unOp->operand).type, false};
      }
      return {&badValue, false};
    }
    
    case ExpressionType::VALUE: {
      if (expression.value.type == TokenType::IDENTIFIER) {
        GeneralDec *decPtr;
        if (structMap) {
          StructDecList *structDec = (*structMap)[tokenizer.extractToken(expression.value)];
          if (!structDec) {
            errors.emplace_back(CheckerErrorType::NO_SUCH_MEMBER_VARIABLE, expression.value);
            return {&badValue, false};
          }
          if (structDec->type != StructDecType::VAR) {
            errors.emplace_back(CheckerErrorType::NOT_A_VARIABLE, expression.value);
            return {&badValue, false};
          }
          decPtr = memPool.makeGeneralDec();
          decPtr->type = GeneralDecType::VARIABLE;
          decPtr->varDec = &structDec->varDec;
        } else {
          decPtr = lookUp[tokenizer.extractToken(expression.value)];
          if (!decPtr) {
            errors.emplace_back(CheckerErrorType::NO_SUCH_VARIABLE, expression.value);
            return {&badValue, false};
          }
          if (decPtr->type != GeneralDecType::VARIABLE) {
            errors.emplace_back(CheckerErrorType::NOT_A_VARIABLE, expression.value, decPtr);
            return {&badValue, false};
          }
        }
        if (decPtr->varDec->type.token.type == TokenType::REFERENCE) {
          return {decPtr->varDec->type.next, true};
        }
        return {&decPtr->varDec->type, true};
      }
      if (expression.value.type == TokenType::DECIMAL_NUMBER) {
        // need to get the actual number and see if it fits in a 32bit int, if not, unsigned, if not, 64bit
        // for now, just dump all numbers as ints
        return {&int32Value, false};
      }
      if (expression.value.type == TokenType::NULL_PTR) {
        return {&nullptrValue, false};
      }
      if (expression.value.type == TokenType::FALSE || expression.value.type == TokenType::TRUE) {
        return {&boolValue, false};
      }
      if (expression.value.type == TokenType::STRING_LITERAL) {
        return {&stringValue, false};
      }
      return {&charValue, false};
    }
    
    case ExpressionType::FUNCTION_CALL: {
      GeneralDec *decPtr;
      // member function
      if (structMap) {
        StructDecList *structDec = (*structMap)[tokenizer.extractToken(expression.funcCall->name)];
        if (!structDec) {
          errors.emplace_back(CheckerErrorType::NO_SUCH_MEMBER_FUNCTION, expression.funcCall->name);
          return {&badValue, false};
        }
        if (structDec->type != StructDecType::FUNC) {
          errors.emplace_back(CheckerErrorType::NOT_A_FUNCTION, expression.funcCall->name);
          return {&badValue, false};
        }
        decPtr = memPool.makeGeneralDec();
        decPtr->type = GeneralDecType::FUNCTION;
        decPtr->funcDec = &structDec->funcDec;
      }
      // normal function call
      else {
        decPtr = lookUp[tokenizer.extractToken(expression.funcCall->name)];
        if (!decPtr) {
          // dec does not exist
          errors.emplace_back(CheckerErrorType::NO_SUCH_FUNCTION, expression.funcCall->name);
          return {&badValue, false};
        }
        if (decPtr->type != GeneralDecType::FUNCTION) {
          // not a function
          errors.emplace_back(CheckerErrorType::NOT_A_FUNCTION, expression.funcCall->name, decPtr);
          return {&badValue, false};
        }
      }
      // valid function, now check parameters
      // parameters are already validated on second top level scan. so assume the statements are all varDecs and valid
      StatementList* paramList = &decPtr->funcDec->params;
      ExpressionList* argList = &expression.funcCall->args;
      do {
        ResultingType resultingType = checkExpression(argList->curr);
        if (resultingType.type->token.type != TokenType::BAD_VALUE) {
          if (!paramList->curr.varDec) {
            if (resultingType.type->token.type != TokenType::VOID) {
              errors.emplace_back(CheckerErrorType::WRONG_NUMBER_OF_ARGS, expression.funcCall->name, decPtr);
            }
          }
          else if (!(paramList->curr.varDec->type == *resultingType.type)) {
            // types dont match
            errors.emplace_back(CheckerErrorType::TYPE_DOES_NOT_MATCH, expression.funcCall->name, decPtr);
          }
        }
        // type matches
        paramList = paramList->next;
        argList = argList->next;
      } while (argList && paramList);
      if (argList || paramList) {
        errors.emplace_back(CheckerErrorType::WRONG_NUMBER_OF_ARGS, expression.funcCall->name, decPtr);
      }
      if (decPtr->funcDec->returnType.token.type == TokenType::REFERENCE) {
        return {decPtr->funcDec->returnType.next, true};
      }
      return {&decPtr->funcDec->returnType, false};
    }
    
    case ExpressionType::ARRAY_ACCESS: {
      return {&badValue, false};
    }
    
    case ExpressionType::WRAPPED: {
      return checkExpression(*expression.wrapped);
    }
    
    case ExpressionType::ARRAY_OR_STRUCT_LITERAL: {
      return {&badValue, false};
    }

    case ExpressionType::NONE: {
      return {&voidValue, false};
    }
    
    default: {
      exit(1);
    }
  }
}

/**
 * Validates a type
 * \param type the type to check
 * \returns true if the type is a valid type, false otherwise (adds the error to errors)
 * \note in the case of the type being just 'void', will return false even though it is valid for function return types.
 *  check if the emplaced error is 'void' and remove it if called for a function return type
*/
bool Checker::checkType(const TokenList& type) {
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
  const TokenList *list = &type;
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
      else {
        if (typeType == 3) {
          errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
          break;
        }
        typeType = 3;
      }
    }
    else if (list->token.type == TokenType::REFERENCE) {
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
      GeneralDec* &typeDec = lookUp[tokenizer.extractToken(list->token)];
      if (!typeDec) {
        errorType = CheckerErrorType::NO_SUCH_TYPE;
        break;
      }
      if (typeDec->type != GeneralDecType::STRUCT) {
        errors.emplace_back(CheckerErrorType::EXPECTING_TYPE, list->token, typeDec);
        return false;
      }
      typeType = 3;
    }
    list = list->next;
  } while (list);
  if (errorType == CheckerErrorType::NONE) {
    return true;
  }
  errors.emplace_back(errorType, list->token);
  return false;
}

ResultingType Checker::checkMemberAccess(ResultingType& leftSide, Expression& expression) {
  if (expression.binOp->rightSide.type == ExpressionType::VALUE) {
    if (expression.binOp->rightSide.value.type != TokenType::IDENTIFIER) {
      errors.emplace_back(CheckerErrorType::EXPECTED_IDENTIFIER, expression.binOp->rightSide.value);
      return {&badValue, false};
    }
  }
  else if (expression.binOp->rightSide.type != ExpressionType::FUNCTION_CALL && expression.binOp->rightSide.type != ExpressionType::ARRAY_ACCESS) {
    errors.emplace_back(CheckerErrorType::EXPECTED_IDENTIFIER, expression.binOp->rightSide.value);
    return {&badValue, false};
  }
  auto structMap = &structsLookUp[tokenizer.extractToken(leftSide.type->token)];
  if (!structMap) {
    errors.emplace_back(CheckerErrorType::NOT_A_STRUCT, &expression.binOp->leftSide);
    return {&badValue, false};
  }
  return checkExpression(expression.binOp->rightSide, structMap);
}

bool checkAssignment(const ResultingType& leftSide, const ResultingType& rightSide) {
  if (leftSide.type->token.type == TokenType::IDENTIFIER || rightSide.type->token.type == TokenType::IDENTIFIER
    || leftSide.type->token.type == TokenType::VOID || rightSide.type->token.type == TokenType::VOID
    || leftSide.type->token.type == TokenType::BAD_VALUE || rightSide.type->token.type == TokenType::BAD_VALUE) {
    return false;
  }
  if (leftSide.type->token.type == TokenType::POINTER) {
    if (rightSide.type->token.type != TokenType::POINTER) {
      return true;
    }
    const TokenList* rType = rightSide.type;
    const TokenList* lType = leftSide.type;
    while (rType && lType) {
      if (rType->token.type != lType->token.type) {
        if (rType->token.type == TokenType::VOID) {
          return true;
        }
        return false;
      }
      rType = rType->next;
      lType = lType->next;
    }
  }
  return true;
}

// only builtin types can be converted to bool, except for void.
bool canBeConvertedToBool(TokenList& type) {
  return isBuiltInType(type.token.type) && type.token.type != TokenType::VOID;
}

TokenList& Checker::largestType(TokenList& typeA, TokenList& typeB) {
  if (typeA.token.type == TokenType::POINTER || typeB.token.type == TokenType::POINTER) {
    return ptrValue;
  }
  if (typeA.token.type > typeB.token.type) {
    return typeA;
  }
  return typeB;
}
