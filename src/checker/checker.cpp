#include <iostream>
#include <cassert>
#include "checker.hpp"

Token getTokenOfExpression(Expression& exp) {
    switch (exp.getType()) {
        case ExpressionType::ARRAY_ACCESS: {
            return getTokenOfExpression(exp.getArrayAccess()->array);
        }
        // case ExpressionType::ARRAY_LITERAL: 
        // case ExpressionType::STRUCT_LITERAL: {
        //   return getTokenOfExpression(exp.getArrayOrStructLiteral()->values.curr);
        // }
        case ExpressionType::BINARY_OP: {
            return exp.getBinOp()->op;
        }
        case ExpressionType::FUNCTION_CALL: {
            return exp.getFunctionCall()->name;
        }
        case ExpressionType::UNARY_OP: {
            return exp.getUnOp()->op;
        }
        case ExpressionType::VALUE: {
            return exp.getToken();
        }
        default: {
            std::cerr << "cannot get token of this expression\n";
            exit(1);
        }
    }
}

TokenList Checker::noneValue {Token{0,0,TokenType::NONE}};
TokenList Checker::badValue {Token{0,0,TokenType::BAD_VALUE}};
TokenList Checker::boolValue {Token{0,0,TokenType::BOOL}};
TokenList Checker::int32Value {Token{0,0,TokenType::INT32_TYPE}};
TokenList Checker::uint32Value {Token{0,0,TokenType::UINT32_TYPE}};
TokenList Checker::int64Value {Token{0,0,TokenType::INT64_TYPE}};
TokenList Checker::uint64Value {Token{0,0,TokenType::UINT64_TYPE}};
TokenList Checker::charValue {Token{0,0,TokenType::CHAR_TYPE}};
TokenList Checker::stringValue {Token{0,0,TokenType::STRING_TYPE}};
TokenList Checker::doubleValue {Token{0,0,TokenType::DOUBLE_TYPE}};
TokenList Checker::fileValue {Token{0,0,TokenType::FILE_TYPE}};
TokenList Checker::voidValue {Token{0,0,TokenType::VOID}};
TokenList Checker::ptrValue {Token{0,0,TokenType::POINTER}, &Checker::voidValue};
TokenList Checker::nullptrValue {Token{0,0,TokenType::NULL_PTR}};

CheckerError::CheckerError(CheckerErrorType type): token{}, tkIndex{}, type{type}  {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, Token token): token{token}, tkIndex{tkIndex}, type{type}  {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, Token token, GeneralDec *decPtr): token{token}, dec{decPtr}, tkIndex{tkIndex}, type{type} {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, Expression *expression): tkIndex{tkIndex}, type{type} {
    token = getTokenOfExpression(*expression);
}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, Expression *expression, GeneralDec *decPtr): dec{decPtr}, tkIndex{tkIndex}, type{type} {
    token = getTokenOfExpression(*expression);
}

std::string CheckerError::getErrorMessage(std::vector<Tokenizer>& tokenizers) const {
    switch(type) {
        case CheckerErrorType::MISSING_MAIN_FUNCTION: return "No 'main' function found\n\n";
        case CheckerErrorType::INVALID_MAIN_FUNCTION_SIGNATURE: return "main function should be defined as so:\nfunc main(argc: uint32, argv: char ptr ptr): int32\n\n";
        default: break;
    }
    auto& tk = tokenizers[tkIndex];
    TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
    std::string message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
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
        case CheckerErrorType::NOT_A_STRUCT: message += "Not a struct\n"; break;
        case CheckerErrorType::NOT_A_TEMPLATE: message += "Not a template\n"; break;
        case CheckerErrorType::NOT_A_VARIABLE: message += "Not a variable\n"; break;
        case CheckerErrorType::TYPE_DOES_NOT_MATCH: message += "Type does not match\n"; break;
        case CheckerErrorType::UNEXPECTED_TYPE: message += "Unexpected type\n"; break;
        case CheckerErrorType::VOID_TYPE: message += "Void type not allowed\n"; break;
        case CheckerErrorType::WRONG_NUMBER_OF_ARGS: message += "Incorrect number of arguments\n"; break;
        case CheckerErrorType::CANNOT_ASSIGN: message += "Cannot assign\n"; break;
        case CheckerErrorType::INCORRECT_RETURN_TYPE: message += "Incorrect return type\n"; break;
        case CheckerErrorType::INVALID_EXIT_TYPE: message += "Invalid exit type\n"; break;
        case CheckerErrorType::NOT_ALL_CODE_PATHS_RETURN: message += "Not all code paths return a value\n"; break;
        case CheckerErrorType::OPERATION_NOT_DEFINED: message += "Operation not defined for these operands\n"; break;
        case CheckerErrorType::EMPTY_STRUCT: message += "Empty struct\n"; break;
        case CheckerErrorType::STRUCT_CYCLE: message += "Struct cycle detected. Size of struct is not finite\n"; break;
        default: message += "Error of some kind, sorry bro\n"; break;
    }
    if (dec) {
        message += "Declaration defined as such:\n  ";
        dec->prettyPrintDefinition(tokenizers, message);
        message += "\n\n";
    } else {
        message += '\n';
    }
    return message;
}

ResultingType::ResultingType(TokenList* type, bool isLValue): type{type}, isLValue{isLValue} {}

Checker::Checker(Program& prog, std::vector<Tokenizer>& tks, NodeMemPool& mem):
    lookUp{}, structLookUp{}, program{prog}, tokenizers{tks}, memPool{mem} {}

bool Checker::check(bool testing) {
    firstTopLevelScan();
    if (!errors.empty()) {
        return false;
    }
    secondTopLevelScan(testing);
    if (!errors.empty()) {
        return false;
    }
    std::vector<StructDec *> chain;
    for (GeneralDecList *list = &program.decs; list; list = list->next) {
        if (list->curr.type != GeneralDecType::STRUCT) {
            continue;
        }
        if (!list->curr.structDec->checked) {
            checkForStructCycles(list->curr, chain);
        }
    }
    if (!errors.empty()) {
        return false;
    }
    fullScan();
    return errors.empty();
}

/**
 * Scans all global declarations and registers them in the table, checking that the name is available
 * Also registers struct members in the struct table
*/
void Checker::firstTopLevelScan() {
    for (GeneralDecList *list = &program.decs; list; list = list->next) {
        tk = &tokenizers[list->curr.tokenizerIndex];
        switch (list->curr.type) {
            case GeneralDecType::FUNCTION: {
                GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.funcDec->name)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.funcDec->name, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::VARIABLE: {
                GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.varDec->name)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.varDec->name, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::STRUCT: {
                const std::string structName = tk->extractToken(list->curr.structDec->name);
                GeneralDec* &decPtr = lookUp[structName];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.structDec->name, decPtr});
                    break;
                }
                decPtr = &list->curr;
                auto& structInfo = structLookUp[decPtr->structDec];
                if (list->curr.structDec->decs.type == StructDecType::NONE) {
                    addError({CheckerErrorType::EMPTY_STRUCT, tk->tokenizerIndex, list->curr.structDec->name});
                    break;
                }
                bool memberVariableFound = false;
                for (StructDecList* inner = &list->curr.structDec->decs; inner; inner = inner->next) {
                    StructMemberInformation* memberInfo;
                    Token token;
                    if (inner->type == StructDecType::VAR) {
                        token = inner->varDec->name;
                        memberVariableFound = true;
                        memberInfo = &structInfo.memberLookup[tk->extractToken(inner->varDec->name)];
                    } else {
                        token = inner->funcDec->name;
                        memberInfo = &structInfo.memberLookup[tk->extractToken(inner->funcDec->name)];
                    }
                    if (memberInfo->memberDec) {
                        GeneralDec *errorDec = memPool.makeGeneralDec();
                        if (memberInfo->memberDec->type == StructDecType::FUNC) {
                            errorDec->type = GeneralDecType::FUNCTION;
                            errorDec->funcDec = memberInfo->memberDec->funcDec;
                        } else {
                            errorDec->type = GeneralDecType::VARIABLE;
                            errorDec->varDec = memberInfo->memberDec->varDec;
                        }
                        addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, token, errorDec});
                    } else {
                        memberInfo->memberDec = inner;
                    }
                }
                if (!memberVariableFound) {
                    addError({CheckerErrorType::EMPTY_STRUCT, tk->tokenizerIndex, list->curr.structDec->name});
                }
                break;
            }
            case GeneralDecType::TEMPLATE: {
                Token token{0,0,TokenType::NONE};
                if (list->curr.tempDec->isStruct) {
                    token = list->curr.tempDec->structDec.name;
                } else {
                    // dec.temp->dec.decType == DecType::FUNCTION
                    token = list->curr.tempDec->funcDec.name;
                }
                GeneralDec* &decPtr = lookUp[tk->extractToken(token)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, token, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::TEMPLATE_CREATE: {
                GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.tempCreate->typeName)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.tempCreate->typeName, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::BUILTIN_FUNCTION: {
                GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.builtinFunc->funcDec.name)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.builtinFunc->funcDec.name, decPtr});
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
void Checker::secondTopLevelScan(bool testing) {
    for (GeneralDecList* list = &program.decs; list; list = list->next) {
        tk = &tokenizers[list->curr.tokenizerIndex];
        switch (list->curr.type) {
            case GeneralDecType::FUNCTION: {
                validateFunctionHeader(*list->curr.funcDec);
                break;
            }
            case GeneralDecType::VARIABLE: {
                checkType(list->curr.varDec->type);
                break;
            }
            case GeneralDecType::STRUCT: {
                getStructInfo(*list->curr.structDec);
                validateStructTopLevel(*list->curr.structDec);
                break;
            }
            case GeneralDecType::TEMPLATE: {
                // parser validates that there is at least one type
                std::vector<std::string> templateTypes;
                const TokenList *templateIdentifiers = &list->curr.tempDec->templateTypes;
                // add templated types to global lookup
                bool errorFound = false;
                do {
                    templateTypes.push_back(tk->extractToken(templateIdentifiers->exp.getToken()));
                    GeneralDec *&tempTypeDec = lookUp[templateTypes.back()];
                    if (tempTypeDec) {
                        templateTypes.pop_back();
                        addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, templateIdentifiers->exp.getToken(), tempTypeDec});
                        errorFound = true;
                        break;
                    }
                    tempTypeDec = memPool.makeGeneralDec();
                    tempTypeDec->type = GeneralDecType::STRUCT;
                    templateIdentifiers = templateIdentifiers->next;
                } while (templateIdentifiers);
                // validate top level types
                if (!errorFound) {
                    if (list->curr.tempDec->isStruct) {
                        getStructInfo(list->curr.tempDec->structDec);
                        validateStructTopLevel(list->curr.tempDec->structDec);
                    } else {
                        validateFunctionHeader(list->curr.tempDec->funcDec);
                    }
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
                GeneralDec* dec = lookUp[tk->extractToken(list->curr.tempCreate->templateName)];
                if (!dec) {
                    addError({CheckerErrorType::NO_SUCH_TEMPLATE, tk->tokenizerIndex, list->curr.tempCreate->templateName});
                    break;
                } else if (dec->type != GeneralDecType::TEMPLATE) {
                    addError({CheckerErrorType::NOT_A_TEMPLATE, tk->tokenizerIndex, list->curr.tempCreate->templateName, dec});
                    break;
                }
                // check that the number of types match and that the types exist
                const TokenList *tempList = &dec->tempDec->templateTypes, *createList = &list->curr.tempCreate->templateTypes;
                for (;tempList && createList; tempList = tempList->next, createList = createList->next) {
                    if (createList->exp.getToken().getType() == TokenType::IDENTIFIER) {
                        GeneralDec *templateType = lookUp[tk->extractToken(createList->exp.getToken())];
                        if (!templateType) {
                            addError({CheckerErrorType::NO_SUCH_TYPE, tk->tokenizerIndex, createList->exp.getToken()});
                            tempList = nullptr;
                            createList = nullptr;
                            break;
                        }
                    }
                }
                if (tempList || createList) {
                    if (createList) {
                        addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, createList->exp.getToken(), dec});
                    } else {
                        addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, list->curr.tempCreate->templateTypes.exp.getToken(), dec});
                    }
                    break;
                }
                break;
                // have to deep copy the template declaration, replace all occurrences of templated types with the actual
            }
            case GeneralDecType::BUILTIN_FUNCTION: {
                validateFunctionHeader(list->curr.builtinFunc->funcDec);
                break;
            }
            default: break;
        }
    }
    if (!testing) {
        GeneralDec* mainDec = lookUp["main"];
        if (!mainDec) {
            addError({CheckerErrorType::MISSING_MAIN_FUNCTION});
        } else if (mainDec->type != GeneralDecType::FUNCTION) {
            addError({CheckerErrorType::MISSING_MAIN_FUNCTION});
        } else {
            FunctionDec& mainFuncDec = *mainDec->funcDec;
            StatementList* params = &mainFuncDec.params;
            TokenList clArgsP1;
            clArgsP1.exp.setToken(TokenType::POINTER);
            TokenList clArgsP2;
            clArgsP2.exp.setToken(TokenType::POINTER);
            clArgsP2.next = &Checker::charValue;
            clArgsP1.next = &clArgsP2;
            if (
                mainFuncDec.returnType != Checker::int32Value ||
                params->curr.type != StatementType::VARIABLE_DEC ||
                params->curr.varDec->type != Checker::uint32Value ||
                !params->next || params->next->curr.type != StatementType::VARIABLE_DEC ||
                params->next->curr.varDec->type != clArgsP1
            ) {
                addError({CheckerErrorType::INVALID_MAIN_FUNCTION_SIGNATURE});
            }
        }
    }
}

void Checker::fullScan() {
    for (GeneralDecList* list = &program.decs; list; list = list->next) {
        tk = &tokenizers[list->curr.tokenizerIndex];
        switch (list->curr.type) {
            case GeneralDecType::STRUCT: {
                StructDecList* structDecList = &list->curr.structDec->decs;
                while (structDecList) {
                    if (structDecList->type == StructDecType::FUNC) {
                        checkFunction(*structDecList->funcDec);
                    }
                    structDecList = structDecList->next;
                }
                break;
            }
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
            removeLastError();
        } else {
            valid = false;
        }
    } else {
        // insure that the type can fit in a register
        const Token typeToken = getTypeFromTokenList(funcDec.returnType);
        uint32_t size;
        if (typeToken.getType() == TokenType::IDENTIFIER) {
            GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
            assert(genDec->type == GeneralDecType::STRUCT);
            const StructInformation& structInfo = getStructInfo(*genDec->structDec);
            size = structInfo.size;
        } else {
            size = getSizeOfBuiltinType(typeToken.getType());
        }
        if (size > 8) { // 8 being size of register
            valid = false;
            GeneralDec* genDec = memPool.makeGeneralDec();
            genDec->type = GeneralDecType::FUNCTION;
            genDec->funcDec = &funcDec;
            addError({CheckerErrorType::TYPE_TOO_LARGE_TO_RETURN, tk->tokenizerIndex, typeToken, genDec});
        }
    }
    // check parameters
    if (funcDec.params.curr.type != StatementType::NONE) {
        StatementList* params = &funcDec.params;
        do {
            if (!checkType(params->curr.varDec->type)) {
                valid = false;
            } else {
                // insure that the type can fit in a register
                const Token typeToken = getTypeFromTokenList(params->curr.varDec->type);
                uint32_t size;
                if (typeToken.getType() == TokenType::IDENTIFIER) {
                    GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
                    assert(genDec->type == GeneralDecType::STRUCT);
                    const StructInformation& structInfo = getStructInfo(*genDec->structDec);
                    size = structInfo.size;
                } else {
                    size = getSizeOfBuiltinType(typeToken.getType());
                }
                if (size > 8) { // 8 being size of register
                    valid = false;
                    GeneralDec* genDec = memPool.makeGeneralDec();
                    genDec->type = GeneralDecType::FUNCTION;
                    genDec->funcDec = &funcDec;
                    addError({CheckerErrorType::TYPE_TOO_LARGE_TO_BE_AN_ARGUMENT, tk->tokenizerIndex, typeToken, genDec});
                }
            }
            params = params->next;
        } while (params);
    }
    return valid;
}

void Checker::checkForStructCycles(GeneralDec &generalDec, std::vector<StructDec *>& structChain) {
    structChain.emplace_back(generalDec.structDec);
    // get the tokenizer for this declaration
    Tokenizer &tokenizer = tokenizers[generalDec.tokenizerIndex];
    for (StructDecList *list = &generalDec.structDec->decs; list; list = list->next) {
        if (list->type != StructDecType::VAR) {
            continue;
        }
        // check for cycle
        const TokenList* tokenList = &list->varDec->type;
        if (tokenList->exp.getToken().getType() == TokenType::REFERENCE) {
            tokenList = tokenList->next;
        }
        if (tokenList->exp.getToken().getType() != TokenType::IDENTIFIER) {
            continue;
        }
        GeneralDec *dec = lookUp[tokenizer.extractToken(tokenList->exp.getToken())];
        if (dec->structDec->checked) {
            continue; // dec already checked
        }
        for (StructDec *chainLink : structChain) {
            if (chainLink == dec->structDec) {
                // cycle found
                addError({CheckerErrorType::STRUCT_CYCLE, generalDec.tokenizerIndex, tokenList->exp.getToken(), dec});
                chainLink->hasCycle = true;
                break;
            }
        }
        if (!dec->structDec->hasCycle) {
            checkForStructCycles(*dec, structChain);
        }
    }
    generalDec.structDec->checked = true;
    structChain.pop_back();
}

void Checker::validateStructTopLevel(StructDec& structDec) {
    for (StructDecList *inner = &structDec.decs; inner; inner = inner->next) {
        if (inner->type == StructDecType::VAR) {
            checkType(inner->varDec->type);
        }
        else if (inner->type == StructDecType::FUNC) {
            FunctionDec& funcDec = *inner->funcDec;
            StatementList *statementList = memPool.makeStatementList();
            *statementList = funcDec.params;
            funcDec.params.curr.type = StatementType::VARIABLE_DEC;
            funcDec.params.curr.varDec = memPool.makeVariableDec(VariableDec{Token{0, 0, TokenType::THIS}});
            funcDec.params.curr.varDec->type.exp.setToken(TokenType::POINTER);
            funcDec.params.curr.varDec->type.next = memPool.makeTokenList();
            funcDec.params.curr.varDec->type.next->exp.setToken(structDec.name);
            funcDec.params.next = statementList;
            validateFunctionHeader(*inner->funcDec);
        }
    }
}

/**
 * Validates the internals of a function
 * \param funcDec the function declaration to check
 * \returns true if the function is valid
  */
void Checker::checkFunction(FunctionDec& funcDec) {
    // validate parameter names
    if (funcDec.params.curr.type != StatementType::NONE) {
        StatementList *list = &funcDec.params;
        while (list) {
            locals.emplace_back(tk->extractToken(list->curr.varDec->name));
            GeneralDec* &paramDec = lookUp[locals.back()];
            if (paramDec) {
                addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.varDec->name, paramDec});
                return;
            }
            // type already checked on second top level scan, just add it
            paramDec = memPool.makeGeneralDec();
            paramDec->varDec = list->curr.varDec;
            paramDec->type = GeneralDecType::VARIABLE;
            list = list->next;
        }
    }
    bool requireReturn = funcDec.returnType.exp.getToken().getType() != TokenType::VOID;
    if (!checkScope(funcDec.body, funcDec.returnType, false, false)) {
        if (requireReturn) {
            addError({CheckerErrorType::NOT_ALL_CODE_PATHS_RETURN, tk->tokenizerIndex, funcDec.name});
        } else {
            // add return statement to end of scope
            StatementList *scopeStatements = &funcDec.body.scopeStatements;
            while (scopeStatements->next) {
                scopeStatements = scopeStatements->next;
            }
            StatementList* statement = memPool.makeStatementList();
            statement->curr.type = StatementType::CONTROL_FLOW;
            statement->curr.controlFlow = memPool.makeControlFlowStatement();
            statement->curr.controlFlow->type = ControlFlowStatementType::RETURN_STATEMENT;
            statement->curr.controlFlow->returnStatement = memPool.makeReturnStatement();

            scopeStatements->next = statement;
        }
    }
    while (!locals.empty()) {
        // remove locals from table
        lookUp.erase(locals.back());
        locals.pop_back();
    }
}

/**
 * 
 * \returns if the statement is of return or exit type
*/
bool Checker::checkStatement(Statement& statement, const TokenList& returnType, bool isLoop, bool isSwitch) {
    switch (statement.type) {
        case StatementType::CONTROL_FLOW: {
            switch (statement.controlFlow->type) {
                case ControlFlowStatementType::FOR_LOOP: {
                    auto& forLoop = *statement.controlFlow->forLoop;
                    if (forLoop.initialize.type == StatementType::VARIABLE_DEC) {
                        checkLocalVarDec(*forLoop.initialize.varDec);
                    } else if (forLoop.initialize.type == StatementType::EXPRESSION) {
                        checkExpression(*forLoop.initialize.expression);
                    } else if (forLoop.initialize.type != StatementType::NONE) {
                        exit(1);
                    }
                    ResultingType res = checkExpression(forLoop.statement.condition);
                    if (res.type->exp.getToken().getType() != TokenType::BAD_VALUE && res.type->exp.getToken().getType() != TokenType::NONE && !canBeConvertedToBool(res.type)) {
                        addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &forLoop.statement.condition});
                    }
                    checkExpression(forLoop.iteration);
                    checkScope(forLoop.statement.body, returnType, isLoop, isSwitch);
                    if (forLoop.initialize.type == StatementType::VARIABLE_DEC) {
                        lookUp.erase(locals.back());
                        locals.pop_back();
                    }
                    break;
                }
                case ControlFlowStatementType::CONDITIONAL_STATEMENT: {
                    auto & cond = *statement.controlFlow->conditional;
                    {
                        ResultingType res = checkExpression(cond.ifStatement.condition);
                        if (res.type->exp.getToken().getType() != TokenType::BAD_VALUE && 
                        !canBeConvertedToBool(res.type)) {
                            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &cond.ifStatement.condition});
                        }
                    }
                    checkScope(cond.ifStatement.body, returnType, isLoop, isSwitch);
                    for(ElifStatementList* elifList = cond.elifStatement; elifList; elifList = elifList->next) {
                        ResultingType res = checkExpression(elifList->elif.condition);
                        if (res.type->exp.getToken().getType() != TokenType::BAD_VALUE && 
                        !canBeConvertedToBool(res.type)) {
                            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &cond.ifStatement.condition});
                        }
                        checkScope(elifList->elif.body, returnType, isLoop, isSwitch);
                    }
                    if (cond.elseStatement) {
                        checkScope(*cond.elseStatement, returnType, isLoop, isSwitch);
                    }
                    break;
                }
                case ControlFlowStatementType::RETURN_STATEMENT: {
                    ResultingType res = checkExpression(statement.controlFlow->returnStatement->returnValue);
                    if (res.type->exp.getToken().getType() == TokenType::NONE && returnType.exp.getToken().getType() == TokenType::VOID) {
                        break; // ok
                    }
                    if (!checkAssignment(&returnType, res.type, false)) {
                        addError({CheckerErrorType::INCORRECT_RETURN_TYPE, tk->tokenizerIndex, &statement.controlFlow->returnStatement->returnValue});
                    }
                    return true;
                }
                case ControlFlowStatementType::EXIT_STATEMENT: {
                    ResultingType res = checkExpression(statement.controlFlow->returnStatement->returnValue);
                    if (res.type->exp.getToken().getType() == TokenType::NONE && returnType.exp.getToken().getType() == TokenType::VOID) {
                        break; // ok
                    }
                    if (!checkAssignment(&int64Value, res.type, false)) {
                        addError({CheckerErrorType::INVALID_EXIT_TYPE, tk->tokenizerIndex, &statement.controlFlow->returnStatement->returnValue});
                    }
                    return true;
                }
                case ControlFlowStatementType::SWITCH_STATEMENT: {
                    break;
                }
                case ControlFlowStatementType::WHILE_LOOP: {
                    checkExpression(statement.controlFlow->whileLoop->statement.condition);
                    checkScope(statement.controlFlow->whileLoop->statement.body, returnType, isLoop, isSwitch);
                    break;
                }
                case ControlFlowStatementType::NONE: {
                    break;
                }
            }
            break;
        }
        
        case StatementType::EXPRESSION: {
            checkExpression(*statement.expression);
            break;
        }
        
        case StatementType::KEYWORD: {
            if (statement.keyword.getType() == TokenType::CONTINUE) {
                if (!isLoop) {
                    addError({CheckerErrorType::CANNOT_HAVE_CONTINUE_HERE, tk->tokenizerIndex, statement.keyword});
                }
                break;
            }
            else if (statement.keyword.getType() == TokenType::BREAK) {
                if (!isLoop && !isSwitch) {
                    addError({CheckerErrorType::CANNOT_HAVE_BREAK_HERE, tk->tokenizerIndex, statement.keyword});
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
            checkScope(*statement.scope, returnType, isLoop, isSwitch);
            break;
        }

        case StatementType::VARIABLE_DEC: {
            checkLocalVarDec(*statement.varDec);
            break;
        }

        case StatementType::NONE: {
            break;
        }
    }
    return false;
}

/**
 * \param scope The scope to check
 * \param locals name of all local variables allocated 
 * \param returnType the return type of the scope
 * \param isReturnRequired set to true if a return is required within this scope
 * \returns true if all code paths return a value
*/
bool Checker::checkScope(Scope& scope, const TokenList& returnType, bool isLoop, bool isSwitch) {
    const uint32_t prevSize = locals.size();
    StatementList* list = &scope.scopeStatements;
    bool wasReturned = false;
    do {
        if (checkStatement(list->curr, returnType, isLoop, isSwitch)) {
            wasReturned = true;
        }
        list = list->next;
    } while (list);
    while (locals.size() != prevSize) {
        // remove locals from table
        lookUp.erase(locals.back());
        locals.pop_back();
    }
    return wasReturned;
}

bool Checker::checkLocalVarDec(VariableDec& varDec) {
    // add local to table
    locals.emplace_back(tk->extractToken(varDec.name));
    GeneralDec*& dec = lookUp[locals.back()];
    if (dec) {
        addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, varDec.name, dec});
        return false;
    }
    if (!checkType(varDec.type)) {
        return false;
    }
    dec = memPool.makeGeneralDec();
    dec->type = GeneralDecType::VARIABLE;
    dec->varDec = &varDec;
    if (dec->varDec->initialAssignment) {
        ResultingType expressionType = checkExpression(*varDec.initialAssignment);
        if (expressionType.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
            return false;
        }
        ResultingType varType {&varDec.type, true};
        if (!checkAssignment(varType.type, expressionType.type, true)) {
            addError({CheckerErrorType::CANNOT_ASSIGN, tk->tokenizerIndex, varDec.initialAssignment});
            return false;
        }
    }
    return true;
}

/**
 * Returns the resulting type from an expression
 * the ResultingType always contains a valid pointer
 * \param structMap pointer to a struct's lookup map. only used for the right side of binary member access operators
*/
ResultingType Checker::checkExpression(Expression& expression, std::unordered_map<std::string, StructMemberInformation>* structMap) {
    switch(expression.getType()) {
        case ExpressionType::BINARY_OP: {
            ResultingType leftSide = checkExpression(expression.getBinOp()->leftSide);

            if (expression.getBinOp()->op.getType() == TokenType::LOGICAL_AND || expression.getBinOp()->op.getType() == TokenType::LOGICAL_OR) {
                if (leftSide.type->exp.getToken().getType() != TokenType::BAD_VALUE) {
                    if (!canBeConvertedToBool(leftSide.type)) {
                        addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &expression.getBinOp()->leftSide});
                    }
                }
                ResultingType rightSide = checkExpression(expression.getBinOp()->rightSide);
                if (!canBeConvertedToBool(rightSide.type)) {
                    addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &expression.getBinOp()->rightSide});
                }
                return {&boolValue, false};
            }
            
            if (isLogicalOp(expression.getBinOp()->op.getType())) {
                if (
                    leftSide.type->exp.getToken().getType() == TokenType::IDENTIFIER
                    || leftSide.type->exp.getToken().getType() == TokenType::VOID
                    || leftSide.type->exp.getToken().getType() == TokenType::STRING_TYPE
                ) {
                    addError({CheckerErrorType::CANNOT_COMPARE_TYPE, tk->tokenizerIndex, &expression.getBinOp()->leftSide});
                }
                ResultingType rightSide = checkExpression(expression.getBinOp()->leftSide);
                if (
                    rightSide.type->exp.getToken().getType() == TokenType::IDENTIFIER
                    || rightSide.type->exp.getToken().getType() == TokenType::VOID
                    || rightSide.type->exp.getToken().getType() == TokenType::STRING_TYPE
                ) {
                    addError({CheckerErrorType::CANNOT_COMPARE_TYPE, tk->tokenizerIndex, &expression.getBinOp()->rightSide});
                }
                return {&boolValue, false};
            }

            // member access or number with decimal
            if (expression.getBinOp()->op.getType() == TokenType::DOT) {
                TokenType tkType = leftSide.type->exp.getToken().getType();
                if (tkType == TokenType::DECIMAL_NUMBER || tkType == TokenType::HEX_NUMBER || tkType == TokenType::BINARY_NUMBER) {
                    if (expression.getBinOp()->rightSide.getType() != ExpressionType::VALUE) {
                        addError({CheckerErrorType::EXPECTING_NUMBER, tk->tokenizerIndex, &expression.getBinOp()->rightSide});
                    }
                    else {
                        tkType = expression.getBinOp()->rightSide.getToken().getType();
                        if (tkType != TokenType::DECIMAL_NUMBER && tkType != TokenType::HEX_NUMBER && tkType != TokenType::BINARY_NUMBER) {
                            addError({CheckerErrorType::EXPECTING_NUMBER, tk->tokenizerIndex, &expression.getBinOp()->rightSide});
                        }
                    }
                    return {&doubleValue, false};
                } else {
                    if (leftSide.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
                        return {&badValue, false};
                    }
                    return checkMemberAccess(leftSide, expression);
                }
            }
            
            // pointer member access
            if (expression.getBinOp()->op.getType() == TokenType::PTR_MEMBER_ACCESS) {
                if (leftSide.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
                    return {&badValue, false};
                }
                if (leftSide.type->exp.getToken().getType() != TokenType::POINTER) {
                    addError({CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, tk->tokenizerIndex, expression.getBinOp()->op});
                    return {&badValue, false};
                }
                leftSide.type = leftSide.type->next;
                return checkMemberAccess(leftSide, expression);
            }

            ResultingType rightSide = checkExpression(expression.getBinOp()->rightSide);
            if (isAssignment(expression.getBinOp()->op.getType())) {
                if (leftSide.type->exp.getToken().getType() == TokenType::BAD_VALUE || rightSide.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
                    return {&badValue, false};
                }
                if (!leftSide.isLValue) {
                    addError({CheckerErrorType::CANNOT_ASSIGN_TO_TEMPORARY, tk->tokenizerIndex, &expression.getBinOp()->leftSide});
                }
                else if (!checkAssignment(leftSide.type, rightSide.type, false)) {
                    addError({CheckerErrorType::CANNOT_ASSIGN, tk->tokenizerIndex, &expression});
                }
                return {leftSide.type, true};
            }

            if (leftSide.type->exp.getToken().getType() == TokenType::BAD_VALUE && rightSide.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
                return {&badValue, false};
            } else if (leftSide.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
                return {rightSide.type, false};
            } else if (rightSide.type->exp.getToken().getType() == TokenType::BAD_VALUE) {
                return {leftSide.type, false};
            }

            if (
                leftSide.type->exp.getToken().getType() == TokenType::IDENTIFIER || rightSide.type->exp.getToken().getType() == TokenType::IDENTIFIER ||
                leftSide.type->exp.getToken().getType() == TokenType::STRING_TYPE || rightSide.type->exp.getToken().getType() == TokenType::STRING_TYPE
            ) {
                addError({CheckerErrorType::OPERATION_NOT_DEFINED, tk->tokenizerIndex, &expression});
                return {&badValue, false};
            }
            if (leftSide.type->exp.getToken().getType() == TokenType::VOID || rightSide.type->exp.getToken().getType() == TokenType::VOID) {
                addError({CheckerErrorType::OPERATION_ON_VOID, tk->tokenizerIndex, &expression});
                return {&badValue, false};
            }
            TokenList& largest = largestType(*leftSide.type, *rightSide.type);
            if (largest.exp.getToken().getType() < TokenType::INT32_TYPE) {
                return {&int32Value, false};
            }
            return {&largest, false};
        }
        
        case ExpressionType::UNARY_OP: {
            if (expression.getUnOp()->op.getType() == TokenType::DEREFERENCE) {
                ResultingType res = checkExpression(expression.getUnOp()->operand);
                if (res.type->exp.getToken().getType() != TokenType::POINTER) {
                    addError({CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, tk->tokenizerIndex, expression.getUnOp()->op});
                    return {&badValue, false};
                }
                return {res.type->next, true};
            }
            if (expression.getUnOp()->op.getType() == TokenType::NOT) {
                ResultingType res = checkExpression(expression.getUnOp()->operand);
                if (!canBeConvertedToBool(res.type)) {
                    addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, expression.getUnOp()->op});
                }
                return {&boolValue, false};
            }
            if (expression.getUnOp()->op.getType() == TokenType::ADDRESS_OF || expression.getUnOp()->op.getType() == TokenType::INCREMENT_POSTFIX || expression.getUnOp()->op.getType() == TokenType::INCREMENT_PREFIX || expression.getUnOp()->op.getType() == TokenType::DECREMENT_PREFIX || expression.getUnOp()->op.getType() == TokenType::DECREMENT_POSTFIX) {
                ResultingType res = checkExpression(expression.getUnOp()->operand);
                if (!res.isLValue) {
                    addError({CheckerErrorType::CANNOT_OPERATE_ON_TEMPORARY, tk->tokenizerIndex, expression.getUnOp()->op});
                }
                if (expression.getUnOp()->op.getType() == TokenType::ADDRESS_OF) {
                    TokenList *ptrToType = memPool.makeTokenList();
                    ptrToType->exp.setToken(TokenType::POINTER);
                    // const_cast may be considered evil, but i need to assign next here
                    // i'm not modifying res.type, also it's getting returned as const anyways
                    const_cast<const TokenList*&>(ptrToType->next) = res.type;
                    return {ptrToType, false};
                }
                return {res.type, false};
            }
            if (expression.getUnOp()->op.getType() == TokenType::NEGATIVE) {
                // nothing for now
                return {checkExpression(expression.getUnOp()->operand).type, false};
            }
            return {&badValue, false};
        }
        
        case ExpressionType::VALUE: {
            if (expression.getToken().getType() == TokenType::IDENTIFIER) {
                GeneralDec *decPtr;
                if (structMap) {
                    const auto& structMapIter = (*structMap).find(tk->extractToken(expression.getToken()));
                    if (structMapIter == structMap->end()) {
                        addError({CheckerErrorType::NO_SUCH_MEMBER_VARIABLE, tk->tokenizerIndex, expression.getToken()});
                        return {&badValue, false};
                    }
                    const StructMemberInformation& structMemberInfo = (*structMapIter).second;
                    if (structMemberInfo.memberDec->type != StructDecType::VAR) {
                        addError({CheckerErrorType::NOT_A_VARIABLE, tk->tokenizerIndex, expression.getToken()});
                        return {&badValue, false};
                    }
                    decPtr = memPool.makeGeneralDec();
                    decPtr->type = GeneralDecType::VARIABLE;
                    decPtr->varDec = structMemberInfo.memberDec->varDec;
                } else {
                    decPtr = lookUp[tk->extractToken(expression.getToken())];
                    if (!decPtr) {
                        addError({CheckerErrorType::NO_SUCH_VARIABLE, tk->tokenizerIndex, expression.getToken()});
                        return {&badValue, false};
                    }
                    if (decPtr->type != GeneralDecType::VARIABLE) {
                        addError({CheckerErrorType::NOT_A_VARIABLE, tk->tokenizerIndex, expression.getToken(), decPtr});
                        return {&badValue, false};
                    }
                }
                if (decPtr->varDec->type.exp.getToken().getType() == TokenType::REFERENCE) {
                    return {decPtr->varDec->type.next, true};
                }
                return {&decPtr->varDec->type, true};
            }
            if (expression.getToken().getType() == TokenType::DECIMAL_NUMBER) {
                // need to get the actual number and see if it fits in a 32bit int, if not, unsigned, if not, 64bit
                // for now, just dump all numbers as ints
                return {&int32Value, false};
            }
            if (expression.getToken().getType() == TokenType::NULL_PTR) {
                return {&nullptrValue, false};
            }
            if (expression.getToken().getType() == TokenType::FALSE || expression.getToken().getType() == TokenType::TRUE) {
                return {&boolValue, false};
            }
            if (expression.getToken().getType() == TokenType::STRING_LITERAL) {
                return {&stringValue, false};
            }
            if (expression.getToken().getType() == TokenType::STDIN || expression.getToken().getType() == TokenType::STDERR || expression.getToken().getType() == TokenType::STDOUT) {
                return {&fileValue, false};
            }
            if (expression.getToken().getType() == TokenType::STRING_LITERAL) {
                return {&stringValue, false};
            }
            return {&charValue, false};
        }
        
        case ExpressionType::FUNCTION_CALL: {
            GeneralDec *decPtr;
            StatementList *paramList;
            TokenList *returnType;
            // member function
            if (structMap) {
                const auto& structMapIter = (*structMap).find(tk->extractToken(expression.getToken()));
                if (structMapIter == structMap->end()) {
                    addError({CheckerErrorType::NO_SUCH_MEMBER_FUNCTION, tk->tokenizerIndex, expression.getFunctionCall()->name});
                    return {&badValue, false};
                }
                const StructMemberInformation& structMemberInfo = (*structMapIter).second;
                if (structMemberInfo.memberDec->type != StructDecType::FUNC) {
                    addError({CheckerErrorType::NOT_A_FUNCTION, tk->tokenizerIndex, expression.getFunctionCall()->name});
                    return {&badValue, false};
                }
                decPtr = memPool.makeGeneralDec();
                decPtr->type = GeneralDecType::FUNCTION;
                decPtr->funcDec = structMemberInfo.memberDec->funcDec;
                paramList = &decPtr->funcDec->params;
                returnType = &decPtr->funcDec->returnType;
            }
            // normal function call
            else {
                decPtr = lookUp[tk->extractToken(expression.getFunctionCall()->name)];
                if (!decPtr) {
                    // dec does not exist
                    addError({CheckerErrorType::NO_SUCH_FUNCTION, tk->tokenizerIndex, expression.getFunctionCall()->name});
                    return {&badValue, false};
                }
                if (decPtr->type == GeneralDecType::BUILTIN_FUNCTION) {
                    paramList = &decPtr->builtinFunc->funcDec.params;
                    returnType = &decPtr->builtinFunc->funcDec.returnType;
                } else if (decPtr->type == GeneralDecType::FUNCTION ) {
                    paramList = &decPtr->funcDec->params;
                    returnType = &decPtr->funcDec->returnType;
                }
                else {
                    // not a function
                    addError({CheckerErrorType::NOT_A_FUNCTION, tk->tokenizerIndex, expression.getFunctionCall()->name, decPtr});
                    return {&badValue, false};
                }
            }
            // valid function, now check parameters
            // parameters are already validated on second top level scan. so assume the statements are all varDecs and valid
            ExpressionList* argList = &expression.getFunctionCall()->args;
            do {
                ResultingType resultingType = checkExpression(argList->curr);
                if (resultingType.type->exp.getToken().getType() != TokenType::BAD_VALUE) {
                    if (!paramList->curr.varDec) {
                        if (resultingType.type->exp.getToken().getType() != TokenType::NONE) {
                            addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, expression.getFunctionCall()->name, decPtr});
                        }
                    }
                    else if (argList->curr.getType() == ExpressionType::NONE) {
                        argList = nullptr;
                        break;
                    }
                    else if (!checkAssignment(&paramList->curr.varDec->type, resultingType.type, true)) {
                        // types dont match
                        addError({CheckerErrorType::TYPE_DOES_NOT_MATCH, tk->tokenizerIndex, &argList->curr, decPtr});
                    }
                }
                paramList = paramList->next;
                argList = argList->next;
            } while (argList && paramList);
            if (argList || paramList) {
                addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, expression.getFunctionCall()->name, decPtr});
            }
            if (returnType->exp.getToken().getType() == TokenType::REFERENCE) {
                return {returnType->next, true};
            }
            return {returnType, false};
        }
        
        case ExpressionType::ARRAY_ACCESS: {
            ResultingType arrayType = checkExpression(expression.getArrayAccess()->array);
            if (arrayType.type->exp.getToken().getType() != TokenType::POINTER) {
                addError({CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, tk->tokenizerIndex, &expression});
                return {&badValue, false};
            }
            if (!arrayType.type->next) {
                return {&badValue, false};
            }
            return {arrayType.type->next, true};
        }
        
        // case ExpressionType::ARRAY_LITERAL:
        // case ExpressionType::STRUCT_LITERAL: {
        //   return {&badValue, false};
        // }

        case ExpressionType::NONE: {
            return {&noneValue, false};
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
    TokenType prevType = TokenType::NONE;
    TokenList *list = &type;
    do {
        if (isBuiltInType(list->exp.getToken().getType())) {
            const TokenType tokenType = list->exp.getToken().getType();
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
        else if (list->exp.getToken().getType() == TokenType::REFERENCE) {
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
        else if (list->exp.getToken().getType() == TokenType::CONST) {
            if (typeType == 0 || typeType == 1 || prevType == TokenType::CONST) {
                errorType = CheckerErrorType::EXPECTING_TYPE;
                break;
            }
        }
        else {
            assert(list->exp.getToken().getType() == TokenType::IDENTIFIER);
            if (typeType == 3) {
                errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
                break;
            }
            GeneralDec* &typeDec = lookUp[tk->extractToken(list->exp.getToken())];
            if (!typeDec) {
                errorType = CheckerErrorType::NO_SUCH_TYPE;
                break;
            }
            if (typeDec->type != GeneralDecType::STRUCT) {
                addError({CheckerErrorType::EXPECTING_TYPE, tk->tokenizerIndex, list->exp.getToken(), typeDec});
                return false;
            }
            if (list->next && isTypeQualifier(list->next->exp.getToken().getType())) {
                list = list->next;
            }
            if (list->next) {
                errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
                break;
            }
            // adding an extra item in the type list which points to the struct declaration's node, allows for fast comparison
            list->next = memPool.makeTokenList();
            list->next->exp.setToken(TokenType::DEC_PTR);
            list->next->next = (TokenList *)&typeDec;
            return true;
        }
        prevType = list->exp.getToken().getType();
        list = list->next;
    } while (list);
    if (errorType == CheckerErrorType::NONE) {
        return true;
    }
    addError({errorType, tk->tokenizerIndex, list->exp.getToken()});
    return false;
}

ResultingType Checker::checkMemberAccess(ResultingType& leftSide, Expression& expression) {
    if (expression.getBinOp()->rightSide.getType() == ExpressionType::VALUE) {
        if (expression.getBinOp()->rightSide.getToken().getType() != TokenType::IDENTIFIER) {
            addError({CheckerErrorType::EXPECTED_IDENTIFIER, tk->tokenizerIndex, expression.getBinOp()->rightSide.getToken()});
            return {&badValue, false};
        }
    }
    else if (expression.getBinOp()->rightSide.getType() != ExpressionType::FUNCTION_CALL && expression.getBinOp()->rightSide.getType() != ExpressionType::ARRAY_ACCESS) {
        addError({CheckerErrorType::EXPECTED_IDENTIFIER, tk->tokenizerIndex, expression.getBinOp()->rightSide.getToken()});
        return {&badValue, false};
    }
    auto dec = lookUp[tk->extractToken(leftSide.type->exp.getToken())];
    if (!dec || dec->type != GeneralDecType::STRUCT)  {
        addError({CheckerErrorType::NOT_A_STRUCT, tk->tokenizerIndex, &expression.getBinOp()->leftSide});
        return {&badValue, false};
    }
    auto& structMap = structLookUp[dec->structDec];
    return checkExpression(expression.getBinOp()->rightSide, &structMap.memberLookup);
}

void Checker::addError(const CheckerError& error) {
    if (errors.size() > MAX_ERRORS) {
        return;
    }
    errors.emplace_back(error);
}

void Checker::removeLastError() {
    errors.pop_back();
}

bool Checker::checkAssignment(const TokenList* leftSide, const TokenList* rightSide, bool initialAssignment) {
    assert(leftSide && rightSide);
    if (leftSide->exp.getToken().getType() == TokenType::REFERENCE) {
        leftSide = getNextFromTokenList(*leftSide);
    }
    if (rightSide->exp.getToken().getType() == TokenType::REFERENCE) {
        rightSide = getNextFromTokenList(*rightSide);
    }
    const TokenType lFirstType = leftSide->exp.getToken().getType();
    const TokenType rFirstType = rightSide->exp.getToken().getType();
    if (lFirstType == TokenType::VOID || rFirstType == TokenType::VOID
        || lFirstType == TokenType::BAD_VALUE || rFirstType == TokenType::BAD_VALUE) {
        return false;
    }
    const TokenList *lQualifier = getTypeQualifier(*leftSide);
    const TokenList *rQualifier = getTypeQualifier(*rightSide);
    // trying to change const value
    if (!initialAssignment && lQualifier && lQualifier->exp.getToken().getType() == TokenType::CONST) {
        // const value
        return false;
    }
    // both pointers
    if (lFirstType == TokenType::POINTER && rFirstType == TokenType::POINTER) {
        // first const does not matter
        leftSide = getNextFromTokenList(*leftSide);
        rightSide = getNextFromTokenList(*rightSide);
        while (leftSide && rightSide) {
            lQualifier = getTypeQualifier(*leftSide);
            rQualifier = getTypeQualifier(*rightSide);
            if (
                ((lQualifier || rQualifier) && !(lQualifier && rQualifier)) ||
                lQualifier->exp.getToken().getType() != rQualifier->exp.getToken().getType()
            ) {
                // qualifiers do not match
                return false;
            }
            if (leftSide->exp.getToken().getType() != TokenType::POINTER || rightSide->exp.getToken().getType() != TokenType::POINTER) {
                break;
            }
            leftSide = getNextFromTokenList(*leftSide);
            rightSide = getNextFromTokenList(*rightSide);
        }
        if (!leftSide || !rightSide) {
            assert(false); // should be caught by the checkType function
            return false;
        }
        lQualifier = getTypeQualifier(*leftSide);
        rQualifier = getTypeQualifier(*rightSide);
        if (rQualifier && !lQualifier) {
            // right is const but left isn't, no no
            return false;
        }
        if (leftSide->exp.getToken().getType() == TokenType::VOID) {
            return true;
        }
        if (rightSide->exp.getToken().getType() == TokenType::VOID) {
            // cannot assign void pointer to non void pointer without casting. might change this in the future
            return false;
        }
        if (leftSide->exp.getToken().getType() != rightSide->exp.getToken().getType()) {
            // types are different
            return false;
        }
        if (leftSide->exp.getToken().getType() == TokenType::IDENTIFIER) {
            leftSide = getNextFromTokenList(*leftSide);
            rightSide = getNextFromTokenList(*rightSide);
            assert(leftSide && rightSide);
            assert(leftSide->exp.getToken().getType() == TokenType::DEC_PTR && rightSide->exp.getToken().getType() == TokenType::DEC_PTR);
            return leftSide->next == rightSide->next;
        }
        return true;
    }
    // TODO:
    // if (lFirstType == TokenType::ARRAY) {

    // }
    if (lFirstType == TokenType::IDENTIFIER || rFirstType == TokenType::IDENTIFIER) {
        if (lFirstType != rFirstType) {
            return false;
        }
        leftSide = getNextFromTokenList(*leftSide);
        rightSide = getNextFromTokenList(*rightSide);
        assert(leftSide->exp.getToken().getType() == TokenType::DEC_PTR || rightSide->exp.getToken().getType() == TokenType::DEC_PTR);
        return leftSide->next->next == rightSide->next->next;
    }
    assert(isBuiltInType(lFirstType) && isBuiltInType(rFirstType));
    const uint32_t lSize = getSizeOfBuiltinType(lFirstType);
    const uint32_t rSize = getSizeOfBuiltinType(rFirstType);
    return rSize <= lSize;
}

// only builtin types can be converted to bool, except for void and string literal.
bool canBeConvertedToBool(const TokenList* type) {
    TokenType tokenType = type->exp.getToken().getType();
    if (type->exp.getToken().getType() == TokenType::REFERENCE) {
        assert(type->next);
        if (type->next->exp.getType() == ExpressionType::CONTAINER_LITERAL) {
            return false;
        } else {
            assert(type->next->exp.getType() == ExpressionType::VALUE);
            tokenType = type->next->exp.getToken().getType();
        }
    }
    return isBuiltInType(tokenType) && tokenType != TokenType::VOID && tokenType != TokenType::STRING_TYPE;
}

TokenList& Checker::largestType(TokenList& typeA, TokenList& typeB) {
    if (typeA.exp.getToken().getType() == TokenType::POINTER || typeB.exp.getToken().getType() == TokenType::POINTER) {
        return ptrValue;
    }
    if (typeA.exp.getToken().getType() > typeB.exp.getToken().getType()) {
        return typeA;
    }
    return typeB;
}

StructInformation& Checker::getStructInfo(const StructDec& structDec) {
    StructInformation &info = structLookUp[&structDec];
    if (info.size > 0) {
        return info;
    }
    const StructDecList *structDecList = &structDec.decs;
    while (structDecList) {
        if (structDecList->type != StructDecType::VAR) {
            continue;
        }
        auto& memberInfo = info.memberLookup[tk->extractToken(structDecList->varDec->name)];
        memberInfo.memberDec = structDecList;
        Token typeToken = getTypeFromTokenList(structDecList->varDec->type);
        
        // get size and alignment
        uint32_t size, alignTo;
        if (typeToken.getType() == TokenType::IDENTIFIER) {
            GeneralDec* subStruct = lookUp[tk->extractToken(typeToken)];
            StructInformation& subStructInfo = getStructInfo(*subStruct->structDec);
            size = subStructInfo.size;
            alignTo = subStructInfo.alignTo;
        }
        else if (isBuiltInType(typeToken.getType()) || typeToken.getType() == TokenType::REFERENCE) {
            size = getSizeOfBuiltinType(typeToken.getType());
            alignTo = size;
        }
        else {
            std::cerr << "Invalid token type in Checker::getStructInfo: " << typeToken.getType() << '\n';
            exit(1);
        }

        uint32_t paddingRequired = size - ((info.size) % size);
        if (paddingRequired == size) {
            paddingRequired = 0;
        }
        info.size += paddingRequired;
        memberInfo.position = info.size;
        info.size += size;
        if (alignTo > info.alignTo) {
            info.alignTo = alignTo;
        }
        structDecList = structDecList->next;
    }
    uint32_t paddingRequired = info.alignTo - ((info.size) % info.alignTo);
    if (paddingRequired != info.alignTo) {
        info.size += paddingRequired;
    }
    assert(info.size > 0);
    return info;
}


/**
 * Returns the size of a type
 * \param token the token of the type
 * \returns the size of the type
*/
uint32_t getSizeOfBuiltinType(const TokenType type) {
    switch (type) {
        case TokenType::BOOL: {
            return sizeof (bool);
        }
        case TokenType::CHAR_TYPE: {
            return sizeof (char);
        }
        case TokenType::STRING_TYPE: {
            return sizeof (char *);
        }
        case TokenType::INT8_TYPE:
        case TokenType::UINT8_TYPE: {
            return sizeof (uint8_t);
        }
        case TokenType::INT16_TYPE:
        case TokenType::UINT16_TYPE: {
            return sizeof (uint16_t);
        }
        case TokenType::INT32_TYPE:
        case TokenType::UINT32_TYPE: {
            return sizeof (uint32_t);
        }
        case TokenType::INT64_TYPE:
        case TokenType::UINT64_TYPE: {
            return sizeof (uint64_t);
        }
        case TokenType::POINTER: {
            return sizeof (void *);
        }
        case TokenType::DOUBLE_TYPE: {
            return sizeof (double);
        }
        case TokenType::FILE_TYPE: {
            return sizeof(stdin);
        }
        case TokenType::VOID: {
            return 0;
        }
        case TokenType::REFERENCE: {
            return sizeof (void *);
        }
        default: {
            std::cerr << "Invalid TokenType "<< type << " in getSizeOfBuiltinType\n";
            exit(1);
        }
    }
}

/**
 * Returns the amount of padding needed to align an item in memory
 * \param currOffset is the current offset within the container, 0 is assumed to be 8 byte aligned
 * \param size the size of the item in bytes
 * \param alignTo the alignment needed in bytes
 * \returns the number of bytes of padding needed to align 
*/
uint32_t getPaddingNeeded(const uint32_t currOffset, const uint32_t size, const uint32_t alignTo) {
    const uint32_t mod = (currOffset + size) % alignTo;
    return mod != 0 ? alignTo - mod : 0;
}

/**
 * Returns the actual type from a token list
*/
Token getTypeFromTokenList(const TokenList& tokenList) {
    return tokenList.exp.getToken();
}

const TokenList* getNextFromTokenList(const TokenList& tokenList) {
    const TokenList* next = tokenList.next;
    while (next && isTypeQualifier(next->exp.getToken().getType())) {
        next = next->next;
    }
    return next;
}

const TokenList* getTypeQualifier(const TokenList& tokenList) {
    const TokenList* next = tokenList.next;
    if (next && isTypeQualifier(next->exp.getToken().getType())) {
        return next;
    }
    return nullptr;
}
