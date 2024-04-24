#include <iostream>
#include <cassert>
#include <unordered_set>
#include <memory>
#include "checker.hpp"


void memPoolReleaseWholeExpression(NodeMemPool& memPool, Expression& exp);

void memPoolReleaseWholeExpressionList(NodeMemPool& memPool, ExpressionList* expList) {
    memPoolReleaseWholeExpression(memPool, expList->curr);
    expList = expList->next;
    while (expList) {
        memPoolReleaseWholeExpression(memPool, expList->curr);
        ExpressionList* prev = expList;
        expList = expList->next;
        memPool.release(prev);
    }
}

void memPoolReleaseWholeExpression(NodeMemPool& memPool, Expression& exp) {
    if (!exp.getRawPointer()) {
        return;
    }
    switch(exp.getType()) {
        case ExpressionType::NONE: break;
        case ExpressionType::BINARY_OP: { memPoolReleaseWholeExpression(memPool, exp.getBinOp()->rightSide); memPoolReleaseWholeExpression(memPool, exp.getBinOp()->leftSide); memPool.release(exp.getBinOp()); break; }
        case ExpressionType::UNARY_OP: { memPoolReleaseWholeExpression(memPool, exp.getUnOp()->operand); memPool.release(exp.getUnOp()); break; }
        case ExpressionType::VALUE: break;
        case ExpressionType::FUNCTION_CALL:  { memPoolReleaseWholeExpressionList(memPool, &exp.getFunctionCall()->args); memPool.release(exp.getFunctionCall()); break; }
        case ExpressionType::ARRAY_ACCESS: { memPoolReleaseWholeExpression(memPool, exp.getArrayAccess()->array); memPoolReleaseWholeExpression(memPool, exp.getArrayAccess()->offset); memPool.release(exp.getArrayAccess()); break; }
        case ExpressionType::CONTAINER_LITERAL: { memPoolReleaseWholeExpressionList(memPool, &exp.getContainerLiteral()->values); memPool.release(exp.getContainerLiteral()); break; }
        case ExpressionType::LITERAL_VALUE: { memPool.release(exp.getLiteralValue()); break; }
    }
}

Token getTokenOfExpression(const Expression& exp) {
    switch (exp.getType()) {
        case ExpressionType::ARRAY_ACCESS: {
            return getTokenOfExpression(exp.getArrayAccess()->array);
        }
        case ExpressionType::CONTAINER_LITERAL: {
            Token token = getTokenOfExpression(exp.getContainerLiteral()->values.curr);
            if (token.getType() == TokenType::NONE) {
                return {exp.getContainerLiteral()->pos, 1, TokenType::OPEN_BRACKET};
            }
            return token;
        }
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

        case ExpressionType::LITERAL_VALUE:
        case ExpressionType::NONE: {
            return Token();
        }
    }
    assert(false);
    exit(1);
}

CheckerError::CheckerError(CheckerErrorType type): token{}, tkIndex{}, type{type}  {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, Token token): token{token}, tkIndex{tkIndex}, type{type}  {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, Token token, const GeneralDec *decPtr): token{token}, dec{decPtr}, tkIndex{tkIndex}, type{type} {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, const Expression *expression): tkIndex{tkIndex}, type{type} {
    token = getTokenOfExpression(*expression);
}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, const GeneralDec *decPtr): dec{decPtr}, tkIndex{tkIndex}, type{type} {}
CheckerError::CheckerError(CheckerErrorType type, uint32_t tkIndex, const Expression *expression, const GeneralDec *decPtr): dec{decPtr}, tkIndex{tkIndex}, type{type} {
    token = getTokenOfExpression(*expression);
}

std::string CheckerError::getErrorMessage(std::vector<Tokenizer>& tokenizers) const {
    std::string message;
    if (token.getType() != TokenType::NONE) {
        auto& tk = tokenizers[tkIndex];
        TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
        message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
    }
    switch (type) {
        case CheckerErrorType::NONE: message += "No error, this should not be displayed\n"; break;
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
        case CheckerErrorType::EXPECTED_IDENTIFIER: message += "Expected an identifier\n"; break;
        case CheckerErrorType::EXPECTING_TYPE: message += "Expecting type\n"; break;
        case CheckerErrorType::TYPE_TOO_LARGE_TO_RETURN: message += "Type is too large to return\n"; break;
        case CheckerErrorType::TYPE_TOO_LARGE_TO_BE_AN_ARGUMENT: message += "Type is too large to be an argument\n"; break;
        case CheckerErrorType::NOT_A_SIZE: message += "Expression does not result in a number\n"; break;
        case CheckerErrorType::NON_CONSTANT_SIZE_ARRAY: message += "Non-constant size array is not allowed\n"; break;
        case CheckerErrorType::INVALID_ARRAY_SIZE: message += "Invalid array size\n"; break;
        case CheckerErrorType::EXPECTED_SIZE: message += "Expecting size for array\n"; break;
        case CheckerErrorType::CANNOT_COMPARE_TYPE: message += "Cannot compare these types\n"; break;
        case CheckerErrorType::OPERATION_ON_VOID: message += "Cannot do operation on void\n"; break;
        case CheckerErrorType::MISSING_MAIN_FUNCTION: message += "No 'main' function found\n"; break;
        case CheckerErrorType::CANNOT_USE_AS_PARAMETER_TYPE: message += "Invalid parameter type\n"; break;
        case CheckerErrorType::CANNOT_USE_AS_RETURN_TYPE: message += "Invalid return type\n"; break;
        case CheckerErrorType::EXPECTING_NAMED_INDEX: message += "Expecting named index\n"; break;
        case CheckerErrorType::INVALID_STRING_LITERAL: message += "Invalid escape sequence in literal\n"; break;
        case CheckerErrorType::VARIABLE_INDEX_IN_CONTAINER_LITERAL: message += "Cannot use variable as index in container initializer\n"; break;
        case CheckerErrorType::CONTAINER_LITERAL_TOO_LARGE: message += "Container literal too large\n"; break;
        case CheckerErrorType::USELESS_CONTAINER_LITERAL: message += "Useless container literal\n"; break;
        case CheckerErrorType::ELEMENT_TYPE_DOES_NOT_MATCH_ARRAY_TYPE: message += "Mixing types in array literal\n"; break;
        case CheckerErrorType::REFERENCE_VARIABLE_MISSING_INITIALIZER: message += "Mixing types in array literal\n"; break;
        case CheckerErrorType::INVALID_MAIN_FUNCTION_SIGNATURE: message += "main function should be defined as so:\nfunc main(argc: uint32, argv: char ptr ptr): int32\n"; break;
        case CheckerErrorType::UNSPECIFIED: message += "TODO\n"; break;
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

ResultingType::ResultingType(TokenList* type, bool isLValue) {
    this->isLValue = isLValue;
    this->isLiteral = false;
    value.type = type;
}

ResultingType::ResultingType(TokenList* type, bool isLValue, bool isLiteral) {
    this->isLValue = isLValue;
    this->isLiteral = isLiteral;
    value.type = type;
}

ResultingType::ResultingType(const LiteralValue& literalValue) {
    this->isLValue = false;
    this->isLiteral = true;
    value = literalValue;
}
StructInformation::StructInformation(): decPtr{nullptr}, type{nullptr} {
    assert(false);
    exit(1);
}
StructInformation::StructInformation(NodeMemPool& memPool, const GeneralDec* decPtr): decPtr{decPtr}, type{memPool.makeTokenList()} {
    type->token = {0, 0, TokenType::IDENTIFIER};
    type->next = memPool.makeTokenList();
    type->next->token = {0, 0, TokenType::DEC_PTR};
    type->next->next = (TokenList *)decPtr;
}

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

void Checker::firstTopLevelScan() {
    for (GeneralDecList *list = &program.decs; list; list = list->next) {
        tk = &tokenizers[list->curr.tokenizerIndex];
        switch (list->curr.type) {
            case GeneralDecType::FUNCTION: {
                const GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.funcDec->name)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.funcDec->name, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::VARIABLE: {
                const GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.varDec->name)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.varDec->name, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::STRUCT: {
                const std::string structName = tk->extractToken(list->curr.structDec->name);
                const GeneralDec* &decPtr = lookUp[structName];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.structDec->name, decPtr});
                    break;
                }
                decPtr = &list->curr;
                const auto& structLookUpIter = structLookUp.emplace(std::make_pair(decPtr->structDec, StructInformation{memPool, decPtr}));
                assert(structLookUpIter.second);
                auto& structInfo = structLookUpIter.first->second;
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
                const GeneralDec* &decPtr = lookUp[tk->extractToken(token)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, token, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::TEMPLATE_CREATE: {
                const GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.tempCreate->typeName)];
                if (decPtr) {
                    addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.tempCreate->typeName, decPtr});
                } else {
                    decPtr = &list->curr;
                }
                break;
            }
            case GeneralDecType::BUILTIN_FUNCTION: {
                const GeneralDec* &decPtr = lookUp[tk->extractToken(list->curr.builtinFunc->funcDec.name)];
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
                validateStructTopLevel(*list->curr.structDec);
                getStructInfo(*list->curr.structDec);
                break;
            }
            case GeneralDecType::TEMPLATE: {
                assert(false);
                // // parser validates that there is at least one type
                // std::vector<std::string> templateTypes;
                // const TokenList *templateIdentifiers = &list->curr.tempDec->templateTypes;
                // // add templated types to global lookup
                // bool errorFound = false;
                // do {
                //     templateTypes.push_back(tk->extractToken(templateIdentifiers->token));
                //     auto& lookUpIter = lookUp.find(templateTypes.back());
                //     if (lookUpIter != lookUp.end()) {
                //         templateTypes.pop_back();
                //         addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, templateIdentifiers->token, tempTypeDec});
                //         errorFound = true;
                //         break;
                //     }
                //     const GeneralDec *&tempTypeDec = lookUp.emplace(std::make_pair(templateTypes.back(), ));
                //     tempTypeDec = memPool.makeGeneralDec();
                //     tempTypeDec->type = GeneralDecType::STRUCT;
                //     templateIdentifiers = templateIdentifiers->next;
                // } while (templateIdentifiers);
                // // validate top level types
                // if (!errorFound) {
                //     if (list->curr.tempDec->isStruct) {
                //         getStructInfo(list->curr.tempDec->structDec);
                //         validateStructTopLevel(list->curr.tempDec->structDec);
                //     } else {
                //         validateFunctionHeader(list->curr.tempDec->funcDec);
                //     }
                // }
                // // remove templated types
                // while (!templateTypes.empty()) {
                //     auto tempType = lookUp.find(templateTypes.back());
                //     memPool.release(tempType->second);
                //     lookUp.erase(tempType);
                //     templateTypes.pop_back();
                // }
                break;
            }
            case GeneralDecType::TEMPLATE_CREATE: {
                assert(false);
                // // check that the template exists
                // const GeneralDec* dec = lookUp[tk->extractToken(list->curr.tempCreate->templateName)];
                // if (!dec) {
                //     addError({CheckerErrorType::NO_SUCH_TEMPLATE, tk->tokenizerIndex, list->curr.tempCreate->templateName});
                //     break;
                // } else if (dec->type != GeneralDecType::TEMPLATE) {
                //     addError({CheckerErrorType::NOT_A_TEMPLATE, tk->tokenizerIndex, list->curr.tempCreate->templateName, dec});
                //     break;
                // }
                // // check that the number of types match and that the types exist
                // const TokenList *tempList = &dec->tempDec->templateTypes, *createList = &list->curr.tempCreate->templateTypes;
                // for (;tempList && createList; tempList = tempList->next, createList = createList->next) {
                //     if (createList->token.getType() == TokenType::IDENTIFIER) {
                //         const GeneralDec *templateType = lookUp[tk->extractToken(createList->token)];
                //         if (!templateType) {
                //             addError({CheckerErrorType::NO_SUCH_TYPE, tk->tokenizerIndex, createList->token});
                //             tempList = nullptr;
                //             createList = nullptr;
                //             break;
                //         }
                //     }
                // }
                // if (tempList || createList) {
                //     if (createList) {
                //         addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, createList->token, dec});
                //     } else {
                //         addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, list->curr.tempCreate->templateTypes.token, dec});
                //     }
                //     break;
                // }
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
        const GeneralDec* mainDec = lookUp["main"];
        if (!mainDec || mainDec->type != GeneralDecType::FUNCTION) {
            addError({CheckerErrorType::MISSING_MAIN_FUNCTION});
        } else {
            FunctionDec& mainFuncDec = *mainDec->funcDec;
            StatementList* params = &mainFuncDec.params;
            TokenList clArgsP1;
            clArgsP1.token = {0, 0, TokenType::POINTER};
            TokenList clArgsP2;
            clArgsP2.token = {0, 0, TokenType::POINTER};
            clArgsP2.next = &BaseTypeListTypes::charValue;
            clArgsP1.next = &clArgsP2;
            if (
                mainFuncDec.returnType != BaseTypeListTypes::int32Value ||
                params->curr.type != StatementType::VARIABLE_DEC ||
                params->curr.varDec->type != BaseTypeListTypes::uint32Value ||
                !params->next || params->next->curr.type != StatementType::VARIABLE_DEC ||
                params->next->curr.varDec->type != clArgsP1
            ) {
                addError({CheckerErrorType::INVALID_MAIN_FUNCTION_SIGNATURE, tk->tokenizerIndex, mainDec});
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
    if (!checkType(funcDec.returnType, true)) {
        valid = false;
    } else {
        const Token typeToken = getTypeFromTokenList(funcDec.returnType);
        if (!isBuiltInType(typeToken.getType()) && typeToken.getType() != TokenType::IDENTIFIER) {
            valid = false;
            GeneralDec* genDec = memPool.makeGeneralDec();
            genDec->type = GeneralDecType::FUNCTION;
            genDec->funcDec = &funcDec;
            addError({CheckerErrorType::CANNOT_USE_AS_RETURN_TYPE, tk->tokenizerIndex, genDec});
        } else {
            // insure that the type can fit in a register
            uint32_t size;
            if (typeToken.getType() == TokenType::IDENTIFIER) {
                const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
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
    }
    // check parameters
    if (funcDec.params.curr.type != StatementType::NONE) {
        StatementList* params = &funcDec.params;
        do {
            if (!checkType(params->curr.varDec->type)) {
                valid = false;
            } else {
                const Token typeToken = getTypeFromTokenList(params->curr.varDec->type);
                if (!isBuiltInType(typeToken.getType()) && typeToken.getType() != TokenType::IDENTIFIER) {
                    valid = false;
                    GeneralDec* genDec = memPool.makeGeneralDec();
                    genDec->type = GeneralDecType::FUNCTION;
                    genDec->funcDec = &funcDec;
                    addError({CheckerErrorType::CANNOT_USE_AS_PARAMETER_TYPE, tk->tokenizerIndex, params->curr.varDec->name, genDec});
                } else {
                    // insure that the type can fit in a register
                    uint32_t size;
                    if (typeToken.getType() == TokenType::IDENTIFIER) {
                        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
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
                        addError({CheckerErrorType::TYPE_TOO_LARGE_TO_BE_AN_ARGUMENT, tk->tokenizerIndex, params->curr.varDec->name, genDec});
                    }
                }
            }
            params = params->next;
        } while (params);
    }
    return valid;
}

void Checker::checkForStructCycles(const GeneralDec &generalDec, std::vector<StructDec *>& structChain) {
    structChain.emplace_back(generalDec.structDec);
    // get the tokenizer for this declaration
    Tokenizer &tokenizer = tokenizers[generalDec.tokenizerIndex];
    for (StructDecList *list = &generalDec.structDec->decs; list; list = list->next) {
        if (list->type != StructDecType::VAR) {
            continue;
        }
        // check for cycle
        const TokenList* tokenList = &list->varDec->type;
        if (tokenList->token.getType() == TokenType::REFERENCE) {
            tokenList = tokenList->next;
        }
        if (tokenList->token.getType() != TokenType::IDENTIFIER) {
            continue;
        }
        const GeneralDec *dec = lookUp[tokenizer.extractToken(tokenList->token)];
        if (dec->structDec->checked) {
            continue; // dec already checked
        }
        for (StructDec *chainLink : structChain) {
            if (chainLink == dec->structDec) {
                // cycle found
                addError({CheckerErrorType::STRUCT_CYCLE, generalDec.tokenizerIndex, tokenList->token, dec});
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
            funcDec.params.curr.varDec->type.token = {0, 0, TokenType::POINTER};
            funcDec.params.curr.varDec->type.next = memPool.makeTokenList();
            funcDec.params.curr.varDec->type.next->token = structDec.name;
            funcDec.params.next = statementList;
            validateFunctionHeader(*inner->funcDec);
        }
    }
}

void Checker::checkFunction(FunctionDec& funcDec) {
    // validate parameter names
    if (funcDec.params.curr.type != StatementType::NONE) {
        StatementList *list = &funcDec.params;
        while (list) {
            locals.emplace_back(tk->extractToken(list->curr.varDec->name));
            GeneralDec* paramDec = memPool.makeGeneralDec();
            paramDec->varDec = list->curr.varDec;
            paramDec->type = GeneralDecType::VARIABLE;
            const auto& lookUpIter = lookUp.emplace(std::make_pair(locals.back(), paramDec));
            if (!lookUpIter.second) {
                addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, list->curr.varDec->name, paramDec});
                return;
            }
            list = list->next;
        }
    }
    bool requireReturn = funcDec.returnType.token.getType() != TokenType::VOID;
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

bool Checker::checkStatement(Statement& statement, TokenList& returnType, bool isLoop, bool isSwitch) {
    switch (statement.type) {
        case StatementType::CONTROL_FLOW: {
            switch (statement.controlFlow->type) {
                case ControlFlowStatementType::FOR_LOOP: {
                    auto& forLoop = *statement.controlFlow->forLoop;
                    if (forLoop.initialize.type == StatementType::VARIABLE_DEC) {
                        checkLocalVarDec(*forLoop.initialize.varDec);
                    } else if (forLoop.initialize.type == StatementType::EXPRESSION) {
                        ResultingType res = checkExpression(*forLoop.initialize.expression);
                        postCheckExpression(res, *forLoop.initialize.expression);
                    } else if (forLoop.initialize.type != StatementType::NONE) {
                        assert(false);
                        exit(1);
                    }
                    { // check condition
                        ResultingType res = checkExpression(forLoop.loop.condition);
                        if (res.value.type->token.getType() != TokenType::BAD_VALUE && res.value.type->token.getType() != TokenType::NONE && !canBeConvertedToBool(res.value.type)) {
                            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &forLoop.loop.condition});
                        }
                        postCheckExpression(res, forLoop.loop.condition);
                    }
                    ResultingType res = checkExpression(forLoop.iteration);
                    postCheckExpression(res, forLoop.iteration);
                    checkScope(forLoop.loop.body, returnType, isLoop, isSwitch);
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
                        if (
                            res.value.type->token.getType() != TokenType::BAD_VALUE && 
                            !canBeConvertedToBool(res.value.type)
                        ) {
                            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &cond.ifStatement.condition});
                        }
                        postCheckExpression(res, cond.ifStatement.condition);
                    }
                    checkScope(cond.ifStatement.body, returnType, isLoop, isSwitch);

                    for (ElifStatementList* elifList = cond.elifStatement; elifList; elifList = elifList->next) {
                        ResultingType res = checkExpression(elifList->elif.condition);
                        if (
                            res.value.type->token.getType() != TokenType::BAD_VALUE && 
                            !canBeConvertedToBool(res.value.type)
                        ) {
                            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &cond.ifStatement.condition});
                        }
                        postCheckExpression(res, elifList->elif.condition);
                        checkScope(elifList->elif.body, returnType, isLoop, isSwitch);
                    }

                    if (cond.elseStatement) {
                        checkScope(*cond.elseStatement, returnType, isLoop, isSwitch);
                    }
                    break;
                }
                case ControlFlowStatementType::RETURN_STATEMENT: {
                    ResultingType res = checkExpression(statement.controlFlow->returnStatement->returnValue);
                    postCheckExpression(res, statement.controlFlow->returnStatement->returnValue);
                    if (res.value.type->token.getType() == TokenType::NONE && returnType.token.getType() == TokenType::VOID) {
                        break; // ok
                    }
                    if (!checkAssignment(&returnType, res, false)) {
                        addError({CheckerErrorType::INCORRECT_RETURN_TYPE, tk->tokenizerIndex, &statement.controlFlow->returnStatement->returnValue});
                    }
                    return true;
                }
                case ControlFlowStatementType::EXIT_STATEMENT: {
                    ResultingType res = checkExpression(statement.controlFlow->returnStatement->returnValue);
                    if (!checkAssignment(&BaseTypeListTypes::int64Value, res, false)) {
                        addError({CheckerErrorType::INVALID_EXIT_TYPE, tk->tokenizerIndex, &statement.controlFlow->returnStatement->returnValue});
                    }
                    postCheckExpression(res, statement.controlFlow->returnStatement->returnValue);
                    return true;
                }
                case ControlFlowStatementType::SWITCH_STATEMENT: {
                    break;
                }
                case ControlFlowStatementType::WHILE_LOOP: {
                    ResultingType res = checkExpression(statement.controlFlow->whileLoop->loop.condition);
                    postCheckExpression(res, statement.controlFlow->whileLoop->loop.condition);
                    checkScope(statement.controlFlow->whileLoop->loop.body, returnType, isLoop, isSwitch);
                    break;
                }
                case ControlFlowStatementType::NONE: {
                    break;
                }
            }
            break;
        }
        
        case StatementType::EXPRESSION: {
            ResultingType res = checkExpression(*statement.expression);
            postCheckExpression(res, *statement.expression);
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
                assert(false);
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

bool Checker::checkScope(Scope& scope, TokenList& returnType, bool isLoop, bool isSwitch) {
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
        memPool.release(lookUp[locals.back()]);
        lookUp.erase(locals.back());
        locals.pop_back();
    }
    return wasReturned;
}

bool Checker::checkLocalVarDec(VariableDec& varDec) {
    const std::string varName = tk->extractToken(varDec.name);
    GeneralDec* dec = memPool.makeGeneralDec();
    dec->type = GeneralDecType::VARIABLE;
    dec->varDec = &varDec;
    // add local to table
    const auto& lookUpIter = lookUp.emplace(std::make_pair(varName, dec));
    if (!lookUpIter.second) {
        memPool.release(dec);
        addError({CheckerErrorType::NAME_ALREADY_IN_USE, tk->tokenizerIndex, varDec.name, dec});
        return false;
    }
    if (!checkType(varDec.type)) {
        return false;
    }
    locals.emplace_back(varName);

    if (varDec.type.token.getType() == TokenType::REFERENCE && !dec->varDec->initialAssignment) {
        // reference var missing initializer
        addError({CheckerErrorType::REFERENCE_VARIABLE_MISSING_INITIALIZER, tk->tokenizerIndex, varDec.name});
        return false;
    }

    if (dec->varDec->initialAssignment) {
        const StructInformation *structInfo = nullptr;
        if (varDec.type.token.getType() == TokenType::IDENTIFIER) {
            assert(varDec.type.next->token.getType() == TokenType::DEC_PTR);
            assert(varDec.type.next->next);
            assert(structLookUp.contains(((GeneralDec*)varDec.type.next->next)->structDec));
            structInfo = &structLookUp[((GeneralDec*)varDec.type.next->next)->structDec];
        }
        ResultingType expressionType = checkExpression(*varDec.initialAssignment, structInfo);
        if (expressionType.value.type->token.getType() == TokenType::BAD_VALUE) {
            return false;
        }
        if (!checkAssignment(&varDec.type, expressionType, true)) {
            addError({CheckerErrorType::CANNOT_ASSIGN, tk->tokenizerIndex, varDec.initialAssignment});
            return false;
        }
        postCheckExpression(expressionType, *varDec.initialAssignment);
    }
    else if (varDec.type.token.getType() == TokenType::ARRAY_TYPE) {
        if (varDec.type.token.getLength() == 0) {
            addError({CheckerErrorType::EXPECTED_SIZE, tk->tokenizerIndex, varDec.type.token});
            return false;
        }
    }
    return true;
}

ResultingType Checker::checkFunctionCallExpression(FunctionCall& funcCall, const StructInformation* structMap) {
    const GeneralDec *decPtr;
    StatementList *paramList;
    TokenList *returnType;
    // member function
    if (structMap) {
        const auto& structMapIter = structMap->memberLookup.find(tk->extractToken(funcCall.name));
        if (structMapIter == structMap->memberLookup.end()) {
            addError({CheckerErrorType::NO_SUCH_MEMBER_FUNCTION, tk->tokenizerIndex, funcCall.name});
            return {&BaseTypeListTypes::badValue, false};
        }
        const StructMemberInformation& structMemberInfo = (*structMapIter).second;
        if (structMemberInfo.memberDec->type != StructDecType::FUNC) {
            addError({CheckerErrorType::NOT_A_FUNCTION, tk->tokenizerIndex, funcCall.name});
            return {&BaseTypeListTypes::badValue, false};
        }
        GeneralDec*temp = memPool.makeGeneralDec();
        temp->type = GeneralDecType::FUNCTION;
        temp->funcDec = structMemberInfo.memberDec->funcDec;
        paramList = &temp->funcDec->params;
        returnType = &temp->funcDec->returnType;
        decPtr = temp;
    }
    // normal function call
    else {
        decPtr = lookUp[tk->extractToken(funcCall.name)];
        if (!decPtr) {
            // dec does not exist
            addError({CheckerErrorType::NO_SUCH_FUNCTION, tk->tokenizerIndex, funcCall.name});
            return {&BaseTypeListTypes::badValue, false};
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
            addError({CheckerErrorType::NOT_A_FUNCTION, tk->tokenizerIndex, funcCall.name, decPtr});
            return {&BaseTypeListTypes::badValue, false};
        }
    }
    // valid function, now check parameters
    // parameters are already validated on second top level scan. so assume the statements are all varDecs and valid
    ExpressionList* argList = &funcCall.args;
    std::unique_ptr<std::unordered_map<std::string, VariableDec*>> params{nullptr};

    // check positional arguments
    do {
        // check for indexed assignment
        if (
            argList->curr.getType() == ExpressionType::BINARY_OP &&
            argList->curr.getBinOp()->op.getType() == TokenType::INDEXED_ASSIGNMENT
        ) {
            // first indexed item. create set of remaining parameters
            // TODO: maybe change this to mem pool
            params = std::make_unique<std::unordered_map<std::string, VariableDec*>>();
            Tokenizer& functionDecTk = tokenizers[decPtr->tokenizerIndex];
            while (paramList) {
                std::string paramName = functionDecTk.extractToken(paramList->curr.varDec->name);
                assert((*params)[paramName] == nullptr);
                (*params)[paramName] = paramList->curr.varDec;
                paramList = paramList->next;
            }
            break;
        }

        ResultingType resultingType = checkExpression(argList->curr);
        if (resultingType.value.type->token.getType() != TokenType::BAD_VALUE) {
            if (!paramList->curr.varDec) {
                if (resultingType.value.type->token.getType() != TokenType::NONE) {
                    addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, funcCall.name, decPtr});
                }
            }
            else if (argList->curr.getType() == ExpressionType::NONE) {
                argList = nullptr;
                break;
            }
            else if (!checkAssignment(&paramList->curr.varDec->type, resultingType, true)) {
                // types dont match
                addError({CheckerErrorType::TYPE_DOES_NOT_MATCH, tk->tokenizerIndex, &argList->curr, decPtr});
            }
            postCheckExpression(resultingType, argList->curr);
        }
        paramList = paramList->next;
        argList = argList->next;
    } while (argList && paramList);

    // now check indexed arguments
    if (params.get()) {
        for (; argList; argList = argList->next) {
            if (
                argList->curr.getType() != ExpressionType::BINARY_OP ||
                argList->curr.getBinOp()->op.getType() != TokenType::INDEXED_ASSIGNMENT
            ) {
                // error, cannot have positional argument after indexed argument
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
                continue;
            }
            BinOp& indexedExpression = *argList->curr.getBinOp();
            ResultingType rightSideResultingType = checkExpression(indexedExpression.rightSide);
            if (rightSideResultingType.value.type == &BaseTypeListTypes::badValue) {
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
                continue;
            }
            if (indexedExpression.leftSide.getType() != ExpressionType::VALUE) {
                // error, can only have identifier indexed arguments in a function call
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
                continue;
            }
            if (indexedExpression.leftSide.getToken().getType() != TokenType::IDENTIFIER) {
                // error, can only have identifier indexed arguments in a function call
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
                continue;
            }
            std::string paramName = tk->extractToken(indexedExpression.leftSide.getToken());
            assert(params.get() != nullptr);
            auto itemIter = params->find(paramName);
            if (itemIter == params->end()) {
                // error, not a parameter
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
                continue;
            }
            if (!itemIter->second) {
                // error, item already assigned to
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
                continue;
            }
            TokenList* leftSideType = &itemIter->second->type;
            // mark as assigned
            itemIter->second = nullptr;
            if (!checkAssignment(leftSideType, rightSideResultingType, true)) {
                // types dont match
                addError({CheckerErrorType::UNSPECIFIED, tk->tokenizerIndex, &argList->curr, decPtr});
            }
        }
        // check that all required args have been set
        for (auto& param: *params) {
            if (param.second && !param.second->initialAssignment) {
                // error, missing argument for this parameter
                addError({CheckerErrorType::UNSPECIFIED, decPtr->tokenizerIndex, param.second->name, decPtr});
            }
        }
    } else {
        if (argList) {
            // too many arguments for function
            addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, funcCall.name, decPtr});
        } else {
            for (; paramList; paramList = paramList->next) {
                if (!paramList->curr.varDec->initialAssignment) {
                    // missing argument
                    addError({CheckerErrorType::WRONG_NUMBER_OF_ARGS, tk->tokenizerIndex, funcCall.name, decPtr});
                    break;
                }
            }
        }
    }
    if (returnType->token.getType() == TokenType::REFERENCE) {
        return {returnType, true};
    }
    return {returnType, false};
}

ResultingType Checker::checkTokenExpression(const Token token, const StructInformation* structMap) {
    if (token.getType() == TokenType::IDENTIFIER) {
        VariableDec *varDec;
        if (structMap) {
            const auto& structMapIter = (*structMap).memberLookup.find(tk->extractToken(token));
            if (structMapIter == structMap->memberLookup.end()) {
                addError({CheckerErrorType::NO_SUCH_MEMBER_VARIABLE, tk->tokenizerIndex, token});
                return {&BaseTypeListTypes::badValue, false};
            }
            const StructMemberInformation& structMemberInfo = (*structMapIter).second;
            if (structMemberInfo.memberDec->type != StructDecType::VAR) {
                addError({CheckerErrorType::NOT_A_VARIABLE, tk->tokenizerIndex, token});
                return {&BaseTypeListTypes::badValue, false};
            }
            varDec = structMapIter->second.memberDec->varDec;
        } else {
            const GeneralDec* genDec = lookUp[tk->extractToken(token)];
            if (!genDec) {
                addError({CheckerErrorType::NO_SUCH_VARIABLE, tk->tokenizerIndex, token});
                return {&BaseTypeListTypes::badValue, false};
            }
            if (genDec->type != GeneralDecType::VARIABLE) {
                addError({CheckerErrorType::NOT_A_VARIABLE, tk->tokenizerIndex, token, genDec});
                return {&BaseTypeListTypes::badValue, false};
            }
            varDec = genDec->varDec;
        }
        return {&varDec->type, true};
    }
    if (token.getType() == TokenType::STDIN || token.getType() == TokenType::STDERR || token.getType() == TokenType::STDOUT) {
        return {&BaseTypeListTypes::fileValueConst, false};
    }
    return { loadLiteralValue(*tk, token) };
}

ResultingType Checker::checkUnOpExpression(UnOp& unOp) {
    const TokenType op = unOp.op.getType();
    if (op == TokenType::DEREFERENCE) {
        ResultingType res = checkExpression(unOp.operand);
        if (res.value.type->token.getType() != TokenType::POINTER) {
            addError({CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, tk->tokenizerIndex, unOp.op});
            return {&BaseTypeListTypes::badValue, false};
        }
        return {res.value.type->next, true};
    }
    if (op == TokenType::NOT) {
        ResultingType res = checkExpression(unOp.operand);
        if (!canBeConvertedToBool(res.value.type)) {
            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, unOp.op});
        }
        if (res.isLiteral) {
            return evaluateUnaryOpImmExpression(op, res.value);
        }
        return {&BaseTypeListTypes::boolValue, false};
    }
    if (op == TokenType::ADDRESS_OF || op == TokenType::INCREMENT_POSTFIX || op == TokenType::INCREMENT_PREFIX || op == TokenType::DECREMENT_PREFIX || op == TokenType::DECREMENT_POSTFIX) {
        ResultingType res = checkExpression(unOp.operand);
        if (!res.isLValue) {
            addError({CheckerErrorType::CANNOT_OPERATE_ON_TEMPORARY, tk->tokenizerIndex, unOp.op});
        }
        if (unOp.op.getType() == TokenType::ADDRESS_OF) {
            TokenList *ptrToType = memPool.makeTokenList();
            ptrToType->token = {0, 0, TokenType::POINTER};
            ptrToType->next = res.value.type;
            return {ptrToType, false};
        }
        return {res.value.type, false};
    }
    if (op == TokenType::NEGATIVE) {
        // nothing for now
        ResultingType res = checkExpression(unOp.operand);
        if (res.isLiteral) {
            return evaluateUnaryOpImmExpression(op, res.value);
        }
        return res;
    }
    return {&BaseTypeListTypes::badValue, false};
}

ResultingType Checker::checkBinOpExpression(BinOp& binOp) {
    ResultingType leftSide = checkExpression(binOp.leftSide);
    const TokenType op = binOp.op.getType();
    const TokenType leftSideType = leftSide.value.type->token.getType();
    if (op == TokenType::LOGICAL_AND || op == TokenType::LOGICAL_OR) {
        if (leftSideType != TokenType::BAD_VALUE) {
            if (!canBeConvertedToBool(leftSide.value.type)) {
                addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &binOp.leftSide});
            }
        }
        ResultingType rightSide = checkExpression(binOp.rightSide);
        if (!canBeConvertedToBool(rightSide.value.type)) {
            addError({CheckerErrorType::CANNOT_BE_CONVERTED_TO_BOOL, tk->tokenizerIndex, &binOp.rightSide});
        }
        if (leftSide.isLiteral && rightSide.isLiteral) {
            return evaluateBinOpImmExpression(op, leftSide.value, rightSide.value);
        }
        postCheckExpression(leftSide, binOp.leftSide);
        postCheckExpression(rightSide, binOp.rightSide);
        return {&BaseTypeListTypes::boolValue, false};
    }
    
    if (isLogicalOp(op)) {
        if (
            leftSideType == TokenType::IDENTIFIER
            || leftSideType == TokenType::VOID
            || leftSideType == TokenType::CONTAINER_LITERAL
        ) {
            addError({CheckerErrorType::CANNOT_COMPARE_TYPE, tk->tokenizerIndex, &binOp.leftSide});
        }
        ResultingType rightSide = checkExpression(binOp.rightSide);
        if (
            rightSide.value.type->token.getType() == TokenType::IDENTIFIER
            || rightSide.value.type->token.getType() == TokenType::VOID
            || rightSide.value.type->token.getType() == TokenType::CONTAINER_LITERAL
        ) {
            addError({CheckerErrorType::CANNOT_COMPARE_TYPE, tk->tokenizerIndex, &binOp.rightSide});
        }
        if (leftSide.isLiteral && rightSide.isLiteral) {
            return evaluateBinOpImmExpression(op, leftSide.value, rightSide.value);
        }
        postCheckExpression(leftSide, binOp.leftSide);
        postCheckExpression(rightSide, binOp.rightSide);
        return {&BaseTypeListTypes::boolValue, false};
    }

    // member access
    if (op == TokenType::DOT) {
        if (leftSideType != TokenType::IDENTIFIER) {
            return {&BaseTypeListTypes::badValue, false};
        }
        return checkMemberAccess(leftSide, binOp);
    }
    
    // pointer member access
    if (op == TokenType::PTR_MEMBER_ACCESS) {
        if (leftSideType == TokenType::BAD_VALUE) {
            return {&BaseTypeListTypes::badValue, false};
        }
        if (leftSideType != TokenType::POINTER) {
            addError({CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, tk->tokenizerIndex, binOp.op});
            return {&BaseTypeListTypes::badValue, false};
        }
        leftSide.value.type = leftSide.value.type->next;
        return checkMemberAccess(leftSide, binOp);
    }

    ResultingType rightSide = checkExpression(binOp.rightSide);
    postCheckExpression(rightSide, binOp.rightSide);
    const TokenType rightSideType = rightSide.value.type->token.getType();
    if (isAssignment(binOp.op.getType())) {
        if (leftSideType == TokenType::BAD_VALUE || rightSideType == TokenType::BAD_VALUE) {
            return {&BaseTypeListTypes::badValue, false};
        }
        if (!leftSide.isLValue) {
            addError({CheckerErrorType::CANNOT_ASSIGN_TO_TEMPORARY, tk->tokenizerIndex, &binOp.leftSide});
        }
        else if (!checkAssignment(leftSide.value.type, rightSide, false)) {
            addError({CheckerErrorType::CANNOT_ASSIGN, tk->tokenizerIndex, binOp.op});
        }
        postCheckExpression(leftSide, binOp.leftSide);
        return {leftSide.value.type, true};
    }
    postCheckExpression(leftSide, binOp.leftSide);

    if (leftSideType == TokenType::BAD_VALUE && rightSideType == TokenType::BAD_VALUE) {
        return {&BaseTypeListTypes::badValue, false};
    } else if (leftSideType == TokenType::BAD_VALUE) {
        return {rightSide.value.type, false};
    } else if (rightSideType == TokenType::BAD_VALUE) {
        return {leftSide.value.type, false};
    }

    if (
        leftSideType == TokenType::IDENTIFIER || rightSideType == TokenType::IDENTIFIER ||
        leftSideType == TokenType::CONTAINER_LITERAL || rightSideType == TokenType::CONTAINER_LITERAL
    ) {
        addError({CheckerErrorType::OPERATION_NOT_DEFINED, tk->tokenizerIndex, binOp.op});
        return {&BaseTypeListTypes::badValue, false};
    }
    if (leftSideType == TokenType::VOID || rightSideType == TokenType::VOID) {
        addError({CheckerErrorType::OPERATION_ON_VOID, tk->tokenizerIndex, binOp.op});
        return {&BaseTypeListTypes::badValue, false};
    }
    if (leftSide.isLiteral && rightSide.isLiteral) {
        return evaluateBinOpImmExpression(op, leftSide.value, rightSide.value);
    }
    TokenList& largest = largestType(*leftSide.value.type, *rightSide.value.type);
    if (largest.token.getType() < TokenType::INT32_TYPE) {
        return {&BaseTypeListTypes::int32Value, false};
    }
    return {&largest, false};
}

ResultingType Checker::checkExpression(Expression& expression, const StructInformation* structMap) {
    switch(expression.getType()) {
        case ExpressionType::BINARY_OP: {
            return checkBinOpExpression(*expression.getBinOp());
        }
        case ExpressionType::UNARY_OP: {
            return checkUnOpExpression(*expression.getUnOp());
        }
        case ExpressionType::VALUE: {
            return checkTokenExpression(expression.getToken(), structMap);
        }
        case ExpressionType::FUNCTION_CALL: {
            return checkFunctionCallExpression(*expression.getFunctionCall(), structMap);
        }
        case ExpressionType::ARRAY_ACCESS: {
            ResultingType arrayType = checkExpression(expression.getArrayAccess()->array);
            if (arrayType.value.type->token.getType() != TokenType::POINTER) {
                addError({CheckerErrorType::CANNOT_DEREFERENCE_NON_POINTER_TYPE, tk->tokenizerIndex, &expression});
                return {&BaseTypeListTypes::badValue, false};
            }
            if (!arrayType.value.type->next) {
                return {&BaseTypeListTypes::badValue, false};
            }
            postCheckExpression(arrayType, expression.getArrayAccess()->array);
            ResultingType offset = checkExpression(expression.getArrayAccess()->offset);
            postCheckExpression(offset, expression.getArrayAccess()->offset);
            return {arrayType.value.type->next, true};
        }
        case ExpressionType::NONE: {
            return {&BaseTypeListTypes::noneValue, false};
        }
        case ExpressionType::CONTAINER_LITERAL: {
            if (structMap) {
                if (!checkContainerLiteralStruct(*expression.getContainerLiteral(), *structMap)) {
                    return {&BaseTypeListTypes::badValue, false};
                }
                return {structMap->type, false};
            }
            return checkContainerLiteralArray(*expression.getContainerLiteral());
        }
        case ExpressionType::LITERAL_VALUE: {
            assert(false);
            exit(1);
        }
    }
    assert(false);
    exit(1);
}

void Checker::postCheckExpression(ResultingType& res, Expression& expression) {
    if (res.isLiteral) {
        memPoolReleaseWholeExpression(memPool, expression);
        expression.setLiteralValue(memPool.makeLiteralValue());
        *expression.getLiteralValue() = res.value;
    }
}

bool Checker::checkType(TokenList& type, bool isReturnType) {
    /**
   * Used to track the type info. 0 means we can have a ref, 0-2 means pointer, and 3 means an actual type was found
   * Can go forward, but cant go back. 
   * start -> 0
   * ref -> 1
   * ptr / array -> 2
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
        const TokenType tokenType = list->token.getType();
        if (tokenType == TokenType::ARRAY_TYPE) {
            if (typeType == 3) {
                errorType = CheckerErrorType::UNEXPECTED_TYPE;
                break;
            }
            typeType = 2;
            uint16_t isLengthGiven = list->token.getLength();
            if (isLengthGiven == 0) {
                // leaving length field as 0 signifies that the size is based on initial assignment
                prevType = TokenType::ARRAY_TYPE;
                list = list->next;
                continue;
            }
            assert(list->next);
            TokenList *const prev = list;
            list = list->next;
            if (list->token.getType() == TokenType::IDENTIFIER) {
                errorType = CheckerErrorType::NON_CONSTANT_SIZE_ARRAY;
                break;
            }
            ResultingType res = loadLiteralValue(*tk, list->token);
            const TokenType resType = getTypeFromTokenList(*res.value.type).getType();
            if (!isUnsigned(resType) && !isSigned(resType)) {
                errorType = CheckerErrorType::EXPECTED_SIZE;
                break;
            }
            {
                LiteralValue maxArraySize;
                maxArraySize.set(UINT32_MAX);
                LiteralValue validLiteral = evaluateBinOpImmExpression(TokenType::LESS_THAN_EQUAL, res.value, maxArraySize);
                assert(validLiteral.type == &BaseTypeListTypes::boolValueConst);
                if (!*(bool *)validLiteral.getData()) {
                    errorType = CheckerErrorType::INVALID_ARRAY_SIZE;
                    break;
                }
            }
            {
                LiteralValue minArraySize;
                minArraySize.set(1);
                LiteralValue validLiteral = evaluateBinOpImmExpression(TokenType::GREATER_THAN_EQUAL, res.value, minArraySize);
                assert(validLiteral.type == &BaseTypeListTypes::boolValueConst);
                if (!*(bool *)validLiteral.getData()) {
                    errorType = CheckerErrorType::INVALID_ARRAY_SIZE;
                    break;
                }
            }
            const uint16_t arraySize = *(uint16_t*)res.value.getData();
            prev->token.setLength(arraySize);
            assert(prev->next == list);
            prev->next = list->next;
            memPool.release(list);
            prevType = TokenType::ARRAY_TYPE;
            list = prev->next;
            continue;
        }
        if (isConcreteType(tokenType)) {
            if (typeType == 3) {
                errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
                break;
            }
            typeType = 3;
            if (tokenType == TokenType::IDENTIFIER) {
                const GeneralDec* &typeDec = lookUp[tk->extractToken(list->token)];
                if (!typeDec) {
                    errorType = CheckerErrorType::NO_SUCH_TYPE;
                    break;
                }
                if (typeDec->type != GeneralDecType::STRUCT) {
                    addError({CheckerErrorType::EXPECTING_TYPE, tk->tokenizerIndex, list->token, typeDec});
                    return false;
                }
                if (list->next && isTypeQualifier(list->next->token.getType())) {
                    list = list->next;
                }
                if (list->next) {
                    errorType = CheckerErrorType::CANNOT_HAVE_MULTI_TYPE;
                    break;
                }
                // adding an extra item in the type list which points to the struct declaration's node, allows for fast comparison
                // could instead just set next, but for now this will make it more stable since we can check for DEC_PTR type
                list->next = memPool.makeTokenList();
                list->next->token = {0, 0, TokenType::DEC_PTR};
                list->next->next = (TokenList *)typeDec;
                return true;
            }
        }
        else if (tokenType == TokenType::POINTER) {
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
        else if (tokenType == TokenType::CONST) {
            if (typeType == 0 || typeType == 1 || prevType == TokenType::CONST) {
                errorType = CheckerErrorType::EXPECTING_TYPE;
                break;
            }
        }
        else if (tokenType == TokenType::VOID) {
            if (prevType == TokenType::NONE) {
                return isReturnType;
            }
            return false;
        }
        else {
            assert(false);
            exit(1);
        }
        prevType = list->token.getType();
        list = list->next;
    } while (list);
    if (errorType == CheckerErrorType::NONE) {
        return true;
    }
    addError({errorType, tk->tokenizerIndex, list->token});
    return false;
}

ResultingType Checker::checkMemberAccess(ResultingType& leftSide, BinOp& binOp) {
    if (binOp.rightSide.getType() == ExpressionType::VALUE) {
        if (binOp.rightSide.getToken().getType() != TokenType::IDENTIFIER) {
            addError({CheckerErrorType::EXPECTED_IDENTIFIER, tk->tokenizerIndex, binOp.rightSide.getToken()});
            return {&BaseTypeListTypes::badValue, false};
        }
    }
    else if (binOp.rightSide.getType() != ExpressionType::FUNCTION_CALL && binOp.rightSide.getType() != ExpressionType::ARRAY_ACCESS) {
        addError({CheckerErrorType::EXPECTED_IDENTIFIER, tk->tokenizerIndex, binOp.rightSide.getToken()});
        return {&BaseTypeListTypes::badValue, false};
    }
    auto dec = getDecPtr(leftSide.value.type);
    if (!dec || dec->type != GeneralDecType::STRUCT)  {
        addError({CheckerErrorType::NOT_A_STRUCT, tk->tokenizerIndex, &binOp.leftSide});
        return {&BaseTypeListTypes::badValue, false};
    }
    auto& structMap = structLookUp[dec->structDec];
    return checkExpression(binOp.rightSide, &structMap);
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

bool Checker::checkContainerLiteralStruct(ContainerLiteral& containerLiteral, const StructInformation& structInfo) {
    ExpressionList* expList = &containerLiteral.values;
    if (expList->curr.getType() == ExpressionType::NONE) {
        return true;
    }
    for (; expList; expList = expList->next) {
        if (
            expList->curr.getType() != ExpressionType::BINARY_OP ||
            expList->curr.getBinOp()->op.getType() != TokenType::INDEXED_ASSIGNMENT
        ) {
            addError({CheckerErrorType::EXPECTING_NAMED_INDEX, tk->tokenizerIndex, &expList->curr});
            return false;
        }
        ResultingType indexType = checkExpression(expList->curr.getBinOp()->leftSide, &structInfo);
        postCheckExpression(indexType, expList->curr.getBinOp()->leftSide);
        const Token typeToken = getTypeFromTokenList(*indexType.value.type);
        if (typeToken.getType() == TokenType::BAD_VALUE) {
            return false;
        }
        StructInformation *subStructInfo = nullptr;
        if (typeToken.getType() == TokenType::IDENTIFIER) {
            const GeneralDec* genDec = getDecPtr(indexType.value.type);
            assert(genDec->type == GeneralDecType::STRUCT);
            subStructInfo = &structLookUp[genDec->structDec];
        }
        ResultingType resType = checkExpression(expList->curr.getBinOp()->rightSide, subStructInfo);
        postCheckExpression(resType, expList->curr.getBinOp()->rightSide);
        if (!checkAssignment(indexType.value.type, resType, true, false)) {
            addError({CheckerErrorType::ELEMENT_TYPE_DOES_NOT_MATCH_ARRAY_TYPE, tk->tokenizerIndex, &expList->curr});
            return false;
        }
    }
    return true;
}

ResultingType Checker::checkContainerLiteralArray(ContainerLiteral& containerLiteral) {
    ExpressionList* expList = &containerLiteral.values;
    TokenList *actualContainerType = memPool.makeTokenList();
    actualContainerType->token = {containerLiteral.pos, 0, TokenType::CONTAINER_LITERAL};
    if (expList->curr.getType() == ExpressionType::NONE) {
        return {actualContainerType, false};
    }
    ResultingType arrayItemType{nullptr, false};
    uint64_t size = 0;
    for (; expList; expList = expList->next) {
        if (size >= UINT16_MAX) {
            addError({CheckerErrorType::CONTAINER_LITERAL_TOO_LARGE, tk->tokenizerIndex, &expList->curr});
            return {&BaseTypeListTypes::badValue, false};
        }
        if (
            expList->curr.getType() == ExpressionType::BINARY_OP &&
            expList->curr.getBinOp()->op.getType() == TokenType::INDEXED_ASSIGNMENT
        ) {
            ResultingType indexType = checkExpression(expList->curr.getBinOp()->leftSide);
            postCheckExpression(indexType, expList->curr.getBinOp()->leftSide);
            const Token typeToken = getTypeFromTokenList(*indexType.value.type);
            if (typeToken.getType() == TokenType::BAD_VALUE) {
                return {&BaseTypeListTypes::badValue, false};
            }
            if (!isUnsigned(typeToken.getType()) && !isSigned(typeToken.getType())) {
                addError({CheckerErrorType::EXPECTING_NAMED_INDEX, tk->tokenizerIndex, &expList->curr});
                return {&BaseTypeListTypes::badValue, false};
            }

            ResultingType resType = checkExpression(expList->curr.getBinOp()->rightSide);
            postCheckExpression(resType, expList->curr.getBinOp()->rightSide);
            if (!arrayItemType.value.type) {
                arrayItemType.value.type = resType.value.type;
            }
            else if (!checkAssignment(arrayItemType.value.type, resType, true, true)) {
                if (checkAssignment(resType.value.type, arrayItemType, true, true)) {
                    arrayItemType.value.type = resType.value.type;
                } else {
                    addError({CheckerErrorType::ELEMENT_TYPE_DOES_NOT_MATCH_ARRAY_TYPE, tk->tokenizerIndex, &expList->curr});
                    return {&BaseTypeListTypes::badValue, false};
                }
            }
            if (!indexType.isLiteral) {
                addError({CheckerErrorType::VARIABLE_INDEX_IN_CONTAINER_LITERAL, tk->tokenizerIndex, &expList->curr});
                return {&BaseTypeListTypes::badValue, false};
            }
            assert(isUnsigned(typeToken.getType()) || isSigned(typeToken.getType()));
            LiteralValue zeroLiteral;
            zeroLiteral.set(0);
            LiteralValue isValidSize = evaluateBinOpImmExpression(TokenType::GREATER_THAN_EQUAL, indexType.value, zeroLiteral);
            if (!*(bool *)isValidSize.getData()) {
                return {&BaseTypeListTypes::badValue, false};
            }
            uint64_t index = *(uint64_t*)indexType.value.getData();
            if (index + 1 > size) {
                size = index + 1;
            }
            continue;
        }
        ResultingType resType = checkExpression(expList->curr);
        if (!arrayItemType.value.type) {
            arrayItemType.value.type = resType.value.type;
            if (arrayItemType.value.type->token.getType() == TokenType::STRING_LITERAL) {
                TokenList* &arrayItemTypeRef = arrayItemType.value.type;
                arrayItemTypeRef = memPool.makeTokenList();
                arrayItemTypeRef->token = {expList->curr.getToken().getPosition(), 0, TokenType::POINTER};
                arrayItemTypeRef->next = memPool.makeTokenList();
                arrayItemTypeRef->next->token = {0, 0, TokenType::CHAR_TYPE};
                arrayItemTypeRef->next->next = memPool.makeTokenList();
                arrayItemTypeRef->next->next->token = {0, 0, TokenType::CONST};
            }
        }
        else if (!checkAssignment(arrayItemType.value.type, resType, true, true)) {
            if (checkAssignment(resType.value.type, arrayItemType, true, true)) {
                arrayItemType.value.type = resType.value.type;
            } else {
                addError({CheckerErrorType::ELEMENT_TYPE_DOES_NOT_MATCH_ARRAY_TYPE, tk->tokenizerIndex, &expList->curr});
                return {&BaseTypeListTypes::badValue, false};
            }
        }
        postCheckExpression(resType, expList->curr);
        ++size;
    }
    {
        Token temp = actualContainerType->token;
        temp.setLength(size);
        actualContainerType->token = temp;
        actualContainerType->next = arrayItemType.value.type;
    }
    return {actualContainerType, false};
}

bool checkIdentifierAssignment(const TokenList* lTypeList, const TokenList* rTypeList) {
    assert(lTypeList->token.getType() == TokenType::IDENTIFIER);
    assert(rTypeList->token.getType() == TokenType::IDENTIFIER);
    
    assert(lTypeList->next);
    assert(rTypeList->next);
    assert(lTypeList->next->token.getType() == TokenType::DEC_PTR);
    assert(rTypeList->next->token.getType() == TokenType::DEC_PTR);
    assert(lTypeList->next->next);
    assert(rTypeList->next->next);
    return lTypeList->next->next == rTypeList->next->next;
}

/**
 * Checks, in a relaxed fashion (implicit casting), if a type can be assigned to another
 * Both of the types must be of concrete type.
 * \note this is not declared in the header file
*/
bool concreteTypesCanBeDirectlyAssigned(const TokenList* lTypeList, const TokenList* rTypeList) {
    const TokenType lType = getTypeFromTokenList(*lTypeList).getType();
    const TokenType rType = getTypeFromTokenList(*rTypeList).getType();
    assert(isConcreteType(lType) || isConcreteType(rType));
    if (lType == TokenType::IDENTIFIER || rType == TokenType::IDENTIFIER) {
        if (lType != rType) {
            return false;
        }
        return checkIdentifierAssignment(lTypeList, rTypeList);
    }
    // types that require strict assignment
    if (isStrictType(lType) || isStrictType(rType)) {
        return lType == rType;
    }
    assert(isConcreteBuiltInType(lType));
    assert(isConcreteBuiltInType(rType));
    const uint32_t lSize = getSizeOfBuiltinType(lType);
    const uint32_t rSize = getSizeOfBuiltinType(rType);
    return rSize <= lSize;
}

bool Checker::checkAssignment(
    const TokenList* leftSide,
    const ResultingType& rightSideRes,
    bool initialAssignment,
    bool checkCompatibility
) {
    bool strictRequired = false;
    const TokenList* rightSide = rightSideRes.value.type;
    for (bool firstItem = true; ; firstItem = false) {
        assert(leftSide && rightSide);
        const Token lTypeToken = getTypeFromTokenList(*leftSide);
        const Token rTypeToken = getTypeFromTokenList(*rightSide);
        const TokenType lType = lTypeToken.getType();
        if (lType == TokenType::BAD_VALUE) {
            return false;
        }
        const TokenType rType = rTypeToken.getType();
        if (rType == TokenType::BAD_VALUE) {
            return false;
        }
        if (lType == TokenType::REFERENCE || rType == TokenType::REFERENCE) {
            if (lType == TokenType::REFERENCE) {
                // have to move past the reference
                leftSide = getNextFromTokenListConst(*leftSide);
                // now strict type checking is required
                strictRequired = true;
                if (rightSideRes.isLValue) {
                    // check for compatibility rather than assignment since we are taking by ref
                    checkCompatibility = true; 
                } // if !rightIsLValue, we still need to check for assignment
            }
            if (rType == TokenType::REFERENCE) {
                // right side being a reference does not matter, just move past
                rightSide = getNextFromTokenListConst(*rightSide);
            }
            continue;
        }
        const TokenType lQualifier = getTypeQualifier(*leftSide);
        if (firstItem && !initialAssignment && lQualifier == TokenType::CONST) {
            return false;
        }
        const TokenType rQualifier = getTypeQualifier(*rightSide);
        if (strictRequired) {
            if (rQualifier != TokenType::NONE && lQualifier != rQualifier) {
                return false;
            }
        }
        const bool lIsConcrete = isConcreteType(lType);
        const bool rIsConcrete = isConcreteType(rType);
        if (lIsConcrete && rIsConcrete) {
            if (strictRequired) {
                if (lType == TokenType::IDENTIFIER || rType == TokenType::IDENTIFIER) {
                    if (lType != rType) {
                        return false;
                    }
                    return checkIdentifierAssignment(leftSide, rightSide);
                }
                return lType == rType;
            }
            return concreteTypesCanBeDirectlyAssigned(leftSide, rightSide);
        }
        if (lIsConcrete || rIsConcrete) {
            return false;
        }
        assert(isIndirectionType(lType) || (checkCompatibility && lType == TokenType::CONTAINER_LITERAL));
        assert(isIndirectionType(rType) || rType == TokenType::CONTAINER_LITERAL || rType == TokenType::STRING_LITERAL);
        if (lType == TokenType::REFERENCE || rType == TokenType::REFERENCE) {
            if (lType == TokenType::REFERENCE) {
                // have to move past the reference, but now strict type checking is required
                leftSide = getNextFromTokenListConst(*leftSide);
                strictRequired = true;
            }
            if (rType == TokenType::REFERENCE) {
                // right side being a reference does not matter, just move past
                rightSide = getNextFromTokenListConst(*rightSide);
            }
            continue;
        }
        if (lType == TokenType::POINTER || rType == TokenType::POINTER) {
            if (lType == TokenType::ARRAY_TYPE) {
                // not ok
                return false;
            }
            if (rType == TokenType::STRING_LITERAL) {
                leftSide = getNextFromTokenListConst(*leftSide);
                const TokenType lNextType = getTypeFromTokenList(*leftSide).getType();
                if (lNextType != TokenType::CHAR_TYPE) {
                    return false;
                }
                const TokenType lNextQualifier = getTypeQualifier(*leftSide);
                if (lNextQualifier != TokenType::CONST) {
                    return false;
                }
                return true;
            }
            assert(lType == TokenType::POINTER);
            assert(rType == TokenType::POINTER || rType == TokenType::ARRAY_TYPE);
            strictRequired = true;
        }
        else {
            assert(
                lType == TokenType::ARRAY_TYPE ||
                (checkCompatibility && lType == TokenType::CONTAINER_LITERAL)
            );
            assert(rType == TokenType::CONTAINER_LITERAL || rType == TokenType::STRING_LITERAL || rType == TokenType::ARRAY_TYPE);
            if (rType == TokenType::STRING_LITERAL) {
                if (lType != TokenType::ARRAY_TYPE) {
                    return false;
                }
                leftSide = getNextFromTokenListConst(*leftSide);
                const TokenType lNextType = getTypeFromTokenList(*leftSide).getType();
                if (lNextType != TokenType::CHAR_TYPE) {
                    return false;
                }
                if (strictRequired) {
                    const TokenType lNextQualifier = getTypeQualifier(*leftSide);
                    if (lNextQualifier != TokenType::CONST) {
                        return false;
                    }
                }
                const uint16_t lArraySize = lTypeToken.getLength();
                const uint16_t rArraySize = rTypeToken.getLength();
                return lArraySize == 0 || rArraySize <= lArraySize;
            }
            if (rType == TokenType::ARRAY_TYPE) {
                if (!checkCompatibility) {
                    return false;
                } else if (lType == TokenType::CONTAINER_LITERAL) {
                    return false;
                }
            }
            else {
                if (checkCompatibility) {
                    if (lType != TokenType::CONTAINER_LITERAL) {
                        return false;
                    }
                    if (!getNextFromTokenListConst(*leftSide)) {
                        return false;
                    }
                }
                if (!getNextFromTokenListConst(*rightSide)) {
                    return true;
                }
            }
            const uint16_t lArraySize = lTypeToken.getLength();
            const uint16_t rArraySize = rTypeToken.getLength();
            if (strictRequired && rArraySize != lArraySize) {
                return false;                
            }
            if ((lArraySize > 0 && rArraySize > lArraySize) || (rArraySize == 0 && lArraySize == 0)) {
                return false;
            }
        }

        leftSide = getNextFromTokenListConst(*leftSide);
        rightSide = getNextFromTokenListConst(*rightSide);
    }
}

uint32_t Checker::getSizeOfType(const TokenList& tokenList) {
    return ::getSizeOfType(lookUp, structLookUp, *tk, &tokenList);
}

bool canBeConvertedToBool(const TokenList* type) {
    // only builtin types can be converted to bool, except for void, string literal, and array type.
    TokenType tokenType = type->token.getType();
    if (type->token.getType() == TokenType::REFERENCE) {
        assert(type->next);
        tokenType = type->next->token.getType();
    }
    return isBuiltInType(tokenType) && tokenType != TokenType::VOID && tokenType != TokenType::ARRAY_TYPE;
}

TokenList& Checker::largestType(TokenList& typeA, TokenList& typeB) {
    if (typeA.token.getType() > typeB.token.getType()) {
        return typeA;
    }
    return typeB;
}

uint32_t getSizeOfType(
    std::unordered_map<std::string, const GeneralDec *> &lookUp,
    std::unordered_map<const StructDec *, StructInformation>& structLookUp,
    Tokenizer &tk,
    const TokenList *tokenList
) {
    Token typeToken = getTypeFromTokenList(*tokenList);
    uint32_t size = 1;
    while (typeToken.getType() == TokenType::ARRAY_TYPE) {
        const uint32_t arraySize = typeToken.getLength();
        size *= arraySize;
        tokenList = getNextFromTokenListConst(*tokenList);
        assert(tokenList);
        typeToken = getTypeFromTokenList(*tokenList);
    }
    if (typeToken.getType() == TokenType::IDENTIFIER) {
        const GeneralDec* genDec = lookUp[tk.extractToken(typeToken)];
        const StructInformation& structInfo = structLookUp[genDec->structDec];
        return size * structInfo.size;
    }
    return size * getSizeOfBuiltinType(typeToken.getType());
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
        const std::string& varName = tk->extractToken(structDecList->varDec->name);
        auto& memberInfo = info.memberLookup[varName];
        assert(memberInfo.memberDec = structDecList);
        Token typeToken = getTypeFromTokenList(structDecList->varDec->type);
        
        // get size and alignment
        uint32_t size, subItemSize = 0, alignTo;
        if (typeToken.getType() == TokenType::IDENTIFIER) {
            const GeneralDec* subStruct = lookUp[tk->extractToken(typeToken)];
            StructInformation& subStructInfo = getStructInfo(*subStruct->structDec);
            size = subStructInfo.size;
            alignTo = subStructInfo.alignTo;
        }
        else if (isBuiltInType(typeToken.getType())) {
            assert(typeToken.getType() != TokenType::VOID);
            size = getSizeOfBuiltinType(typeToken.getType());
            alignTo = size;
        }
        else if (typeToken.getType() == TokenType::ARRAY_TYPE) {
            const uint32_t arraySize = typeToken.getLength();
            const TokenList* arrayType = getNextFromTokenListConst(structDecList->varDec->type);
            assert(arrayType);
            typeToken = getTypeFromTokenList(*arrayType);
            if (typeToken.getType() == TokenType::IDENTIFIER) {
                const GeneralDec* subStruct = lookUp[tk->extractToken(typeToken)];
                StructInformation& subStructInfo = getStructInfo(*subStruct->structDec);
                subItemSize = subStructInfo.size;
                alignTo = subStructInfo.alignTo;
            }
            else if (isBuiltInType(typeToken.getType())) {
                assert(typeToken.getType() != TokenType::VOID);
                subItemSize = getSizeOfBuiltinType(typeToken.getType());
                alignTo = subItemSize;
            } else {
                assert(false);
                exit(1);
            }
            size = subItemSize * arraySize;
        }
        else {
            std::cerr << "Invalid token type in Checker::getStructInfo: " << typeToken.getType() << '\n';
            exit(1);
        }

        if (!subItemSize) {
            subItemSize = size;
        }
        uint32_t paddingRequired = subItemSize - ((info.size) % subItemSize);
        if (paddingRequired == subItemSize) {
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
    assert(info.size > 0);
    uint32_t paddingRequired = info.alignTo - ((info.size) % info.alignTo);
    if (paddingRequired != info.alignTo) {
        info.size += paddingRequired;
    }
    return info;
}

uint32_t getSizeOfBuiltinType(const TokenType type) {
    switch (type) {
        case TokenType::BOOL: {
            return sizeof (bool);
        }
        case TokenType::CHAR_TYPE: {
            return sizeof (char);
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
            assert(false);
            exit(1);
        }
    }
}

uint32_t getPaddingNeeded(const uint32_t currOffset, const uint32_t size, const uint32_t alignTo) {
    const uint32_t mod = (currOffset + size) % alignTo;
    return mod != 0 ? alignTo - mod : 0;
}

Token getTypeFromTokenList(const TokenList& tokenList) {
    return tokenList.token;
}

const TokenList* getNextFromTokenListConst(const TokenList& tokenList) {
    const TokenList* next = tokenList.next;
    while (next && isTypeQualifier(next->token.getType())) {
        next = next->next;
    }
    return next;
}

TokenType getTypeQualifier(const TokenList& tokenList) {
    TokenList* next = tokenList.next;
    if (next && isTypeQualifier(next->token.getType())) {
        return next->token.getType();
    }
    return TokenType::NONE;
}

const GeneralDec *getDecPtr(const TokenList *type) {
    assert(type->token.getType() == TokenType::IDENTIFIER);
    assert(type->next);
    type = getNextFromTokenListConst(*type);
    assert(type->token.getType() == TokenType::DEC_PTR);
    return (const GeneralDec *)type->next;
}
