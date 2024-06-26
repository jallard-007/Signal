#include <cassert>
#include "parser.hpp"


Unexpected::Unexpected(const Token& token, uint32_t tkIndex): token{token}, tkIndex{tkIndex} {}
std::string Unexpected::getErrorMessage(std::vector<Tokenizer>& tks) {
    auto& tk = tks[tkIndex];
    TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
    std::string message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
    if (unexpectedType == UnexpectedType::STRUCT_MEMBER_INITIALIZE_UNSUPPORTED) {
        return message + "Struct member initialization is not supported\n\n";
    }
    return message + "Unexpected Token: " + tk.extractToken(token) + "\n\n";
}

Expected::Expected(ExpectedType exType, const Token& token, uint32_t tkIndex): tokenWhereExpected{token}, tkIndex{tkIndex}, expectedTokenType{TokenType::NONE}, expectedType{exType} {}
Expected::Expected(ExpectedType exType, const Token& token, TokenType tkType, uint32_t tkIndex): tokenWhereExpected{token}, tkIndex{tkIndex}, expectedTokenType{tkType}, expectedType{exType} {}
std::string Expected::getErrorMessage(std::vector<Tokenizer>& tks) {
    auto& tk = tks[tkIndex];
    TokenPositionInfo posInfo = tk.getTokenPositionInfo(tokenWhereExpected);
    std::string message = tk.filePath + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
    if (expectedType == ExpectedType::EXPRESSION) {
        return message + "Expected Expression\n\n";
    }
    if (expectedType == ExpectedType::TOKEN) {
        return message + "Expected Token: " + typeToString.at(expectedTokenType) + "\n\n";
    }
    return message + "\n\n";
}

#if defined(_MSC_VER)
struct Construct { Construct(void (*f)(void)) { f(); } };
#define constructor(fn) \
void fn(void); static Construct constructor_##fn(fn);
#elif defined(__GNUC__)
#define constructor(fn) \
__attribute__((constructor))
#endif

// TokenType::NEGATIVE is the "largest" operator token type with an enum value of 90, hence size 91
uint8_t operatorPrecedence [91]{};
constructor(initializeOperatorPrecedence)
void initializeOperatorPrecedence() {
    static_assert((uint32_t)TokenType::NEGATIVE + 1 == sizeof (operatorPrecedence));
    operatorPrecedence[(uint8_t)TokenType::ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::MODULO_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::ADDITION_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::DIVISION_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::BITWISE_OR_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::SHIFT_LEFT_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::BITWISE_AND_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::SHIFT_RIGHT_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::SUBTRACTION_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::MULTIPLICATION_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::BITWISE_XOR_ASSIGNMENT] = 1;
    operatorPrecedence[(uint8_t)TokenType::TERNARY] = 1;
    operatorPrecedence[(uint8_t)TokenType::LOGICAL_OR] = 2;
    operatorPrecedence[(uint8_t)TokenType::LOGICAL_AND] = 3;
    operatorPrecedence[(uint8_t)TokenType::BITWISE_OR] = 4;
    operatorPrecedence[(uint8_t)TokenType::BITWISE_XOR] = 5;
    operatorPrecedence[(uint8_t)TokenType::BITWISE_AND] = 6;
    operatorPrecedence[(uint8_t)TokenType::EQUAL] = 7;
    operatorPrecedence[(uint8_t)TokenType::NOT_EQUAL] = 7;
    operatorPrecedence[(uint8_t)TokenType::GREATER_THAN] = 8;
    operatorPrecedence[(uint8_t)TokenType::GREATER_THAN_EQUAL] = 8;
    operatorPrecedence[(uint8_t)TokenType::LESS_THAN] = 8;
    operatorPrecedence[(uint8_t)TokenType::LESS_THAN_EQUAL] = 8;
    operatorPrecedence[(uint8_t)TokenType::SHIFT_LEFT] = 9;
    operatorPrecedence[(uint8_t)TokenType::SHIFT_RIGHT] = 9;
    operatorPrecedence[(uint8_t)TokenType::ADDITION] = 10;
    operatorPrecedence[(uint8_t)TokenType::SUBTRACTION] = 10;
    operatorPrecedence[(uint8_t)TokenType::MULTIPLICATION] = 11;
    operatorPrecedence[(uint8_t)TokenType::DIVISION] = 11;
    operatorPrecedence[(uint8_t)TokenType::MODULO] = 11;
    operatorPrecedence[(uint8_t)TokenType::ADDRESS_OF] = 13;
    operatorPrecedence[(uint8_t)TokenType::DEREFERENCE] = 13;
    operatorPrecedence[(uint8_t)TokenType::NOT] = 13;
    operatorPrecedence[(uint8_t)TokenType::SIZEOF] = 13;
    operatorPrecedence[(uint8_t)TokenType::NEGATIVE] = 13;
    operatorPrecedence[(uint8_t)TokenType::DECREMENT_PREFIX] = 13;
    operatorPrecedence[(uint8_t)TokenType::INCREMENT_PREFIX] = 13;
    operatorPrecedence[(uint8_t)TokenType::DOT] = 14;
    operatorPrecedence[(uint8_t)TokenType::PTR_MEMBER_ACCESS] = 14;
    operatorPrecedence[(uint8_t)TokenType::DECREMENT_POSTFIX] = 14;
    operatorPrecedence[(uint8_t)TokenType::INCREMENT_POSTFIX] = 14;
}

Parser::Parser(Tokenizer& tokenizer, NodeMemPool& memPool):
    tokenizer{&tokenizer}, memPool{memPool}, errorToken{0,0,TokenType::NONE} {}

Parser::~Parser() {
    memPool.reset();
}

void Parser::swapTokenizer(Tokenizer& nextTokenizer) {
    tokenizer = &nextTokenizer;
}


bool Parser::parse() {
    Token token = tokenizer->peekNext();
    while (token.getType() != TokenType::END_OF_FILE) {
        if (!parseNext()) {
            return false;
        }
        token = tokenizer->peekNext();
    }
    globalPrev->next = nullptr;
    return true;
}

GeneralDec* Parser::parseNext() {
    Token token = tokenizer->tokenizeNext();
    if (token.getType() == TokenType::FUNC) {
        globalList->curr.type = GeneralDecType::FUNCTION;
        globalList->curr.funcDec = memPool.makeFunctionDec();
        if (!parseFunction(*globalList->curr.funcDec)) {
            return nullptr;
        }
    }
    else if (token.getType() == TokenType::STRUCT) {
        globalList->curr.type = GeneralDecType::STRUCT;
        globalList->curr.structDec = memPool.makeStructDec();
        if (!parseStruct(*globalList->curr.structDec)) {
            return nullptr;
        }
    }
    else if (token.getType() == TokenType::TEMPLATE) {
        globalList->curr.type = GeneralDecType::TEMPLATE;
        globalList->curr.tempDec = memPool.makeTemplateDec();
        if (!parseTemplate(*globalList->curr.tempDec)) {
            return nullptr;
        }
    }
    else if (token.getType() == TokenType::IDENTIFIER) {
        if (tokenizer->tokenizeNext().getType() == TokenType::COLON) {
            globalList->curr.type = GeneralDecType::VARIABLE;
            globalList->curr.varDec = memPool.makeVariableDec(VariableDec{token});
            ParseStatementErrorType errorType = parseVariableDec(*globalList->curr.varDec);
            if (errorType != ParseStatementErrorType::NONE) {
                return nullptr;
            }
            if (tokenizer->tokenizeNext().getType() != TokenType::SEMICOLON) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                return nullptr;
            }
        }
        else {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
            return nullptr;
        }
    }
    else if (token.getType() == TokenType::CREATE) {
        globalList->curr.type = GeneralDecType::TEMPLATE_CREATE;
        globalList->curr.tempCreate = memPool.makeTemplateCreation();
        token = tokenizer->tokenizeNext();
        if (token.getType() != TokenType::IDENTIFIER) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
            return nullptr;
        }
        globalList->curr.tempCreate->templateName = token;
        if (tokenizer->tokenizeNext().getType() != TokenType::OPEN_BRACKET) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACKET, tokenizer->tokenizerIndex);
            return nullptr;
        }
        token = tokenizer->tokenizeNext();
        if (token.getType() != TokenType::IDENTIFIER && !isBuiltInType(token.getType())) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::TYPE, tokenizer->tokenizerIndex);
            return nullptr;
        }
        globalList->curr.tempCreate->templateTypes.token = token;
        TokenList *tokenPrev = &globalList->curr.tempCreate->templateTypes;
        tokenPrev->next = memPool.makeTokenList();
        TokenList *tkList = tokenPrev->next;
        while (tokenizer->peekNext().getType() == TokenType::COMMA) {
            tokenizer->consumePeek();
            token = tokenizer->tokenizeNext();
            if (token.getType() != TokenType::IDENTIFIER && !isBuiltInType(token.getType())) {
                expected.emplace_back(ExpectedType::TOKEN, token, TokenType::TYPE, tokenizer->tokenizerIndex);
                return nullptr;
            }
            tokenPrev = tkList;
            tkList->token = token;
            tkList->next = memPool.makeTokenList();
            tkList = tkList->next;
        }
        tokenPrev->next = nullptr;
        memPool.release(tkList);
        if (tokenizer->tokenizeNext().getType() != TokenType::CLOSE_BRACKET) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
            return nullptr;
        }
        if (tokenizer->tokenizeNext().getType() != TokenType::AS) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::AS, tokenizer->tokenizerIndex);
            return nullptr;
        }
        token = tokenizer->tokenizeNext();
        if (token.getType() != TokenType::IDENTIFIER) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
            return nullptr;
        }
        globalList->curr.tempCreate->typeName = token;
        if (tokenizer->tokenizeNext().getType() != TokenType::SEMICOLON) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
            return nullptr;
        }
    }
    else if (token.getType() == TokenType::INCLUDE) {
        globalList->curr.type = GeneralDecType::INCLUDE_DEC;
        globalList->curr.includeDec = memPool.makeIncludeDec();
        token = tokenizer->tokenizeNext();
        if (token.getType() != TokenType::STRING_LITERAL) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::STRING_LITERAL, tokenizer->tokenizerIndex);
            return nullptr;
        }
        globalList->curr.includeDec->file = token;
    }
    else if (token.getType() == TokenType::END_OF_FILE) {
        return &globalList->curr;
    }
    else if (token.getType() == TokenType::BUILTIN) {
        if (!parseBuiltin(globalList->curr)) {
            return nullptr;
        }
    }
    else {
        unexpected.emplace_back(token, tokenizer->tokenizerIndex);
        return nullptr;
    }
    globalPrev = globalList;
    globalList->next = memPool.makeGeneralDecList();
    globalList = globalList->next;
    return &globalPrev->curr;
}

bool Parser::parseBuiltin(GeneralDec&) {
    Token token = tokenizer->tokenizeNext();
    if (token.getType() == TokenType::FUNC) {
        token = tokenizer->tokenizeNext();
        globalList->curr.type = GeneralDecType::BUILTIN_FUNCTION;
        globalList->curr.builtinFunc = memPool.makeBuiltinFunc(token);
        if (!parseFunctionHeader(globalList->curr.builtinFunc->funcDec)) {
            return false;
        }
        token = tokenizer->tokenizeNext();
        if (token.getType() != TokenType::SEMICOLON) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
            return false;
        }
    }
    else {
        unexpected.emplace_back(token, tokenizer->tokenizerIndex);
        return false;
    }
    return true;
}

bool Parser::parseFunctionHeader(FunctionDec& dec) {
    Token name = tokenizer->peekNext();
    if (name.getType() != TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, name, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
        return false;
    }
    // consume identifier
    tokenizer->consumePeek();
    dec.name = name;
    if (tokenizer->peekNext().getType() != TokenType::OPEN_PAREN) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_PAREN, tokenizer->tokenizerIndex);
        return false;
    }
    // consume open paren
    tokenizer->consumePeek();
    if (tokenizer->peekNext().getType() != TokenType::CLOSE_PAREN) {
        StatementList *paramList = &dec.params;
        while (true) {
            Token nextToken = tokenizer->peekNext();
            if (nextToken.getType() != TokenType::IDENTIFIER) {
                expected.emplace_back(ExpectedType::TOKEN, nextToken, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
                return false;
            }
            // consume identifier
            tokenizer->consumePeek();
            if (tokenizer->peekNext().getType() != TokenType::COLON) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
                return false;
            }
            // consume colon
            tokenizer->consumePeek();
            paramList->curr.type = StatementType::VARIABLE_DEC;
            paramList->curr.varDec = memPool.makeVariableDec(VariableDec{nextToken});
            ParseStatementErrorType errorType = parseVariableDec(*paramList->curr.varDec);
            if (errorType != ParseStatementErrorType::NONE) {
                return false;
            }
            if (tokenizer->peekNext().getType() == TokenType::COMMA) {
                tokenizer->consumePeek();
                paramList->next = memPool.makeStatementList();
                paramList = paramList->next;
            } else if (tokenizer->peeked.getType() == TokenType::CLOSE_PAREN) {
                break;
            } else {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
                return false;
            }
        }
    }
    // consume close paren
    tokenizer->consumePeek();
    // get return type
    if (tokenizer->peekNext().getType() != TokenType::COLON) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
        return false;
    }
    tokenizer->consumePeek();
    if (getType(dec.returnType) != ParseTypeErrorType::NONE) {
        return false;
    }
    return true;
}


bool Parser::parseFunction(FunctionDec& dec) {
    if (!parseFunctionHeader(dec)) {
        return false;
    }
    if (tokenizer->peekNext().getType() != TokenType::OPEN_BRACE) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
        return false;
    }
    tokenizer->consumePeek();
    ParseStatementErrorType errorType = parseScope(dec.body.scopeStatements);
    if (errorType != ParseStatementErrorType::NONE) {
        return false;
    }
    return true;
}

bool Parser::parseStruct(StructDec& dec) {
    Token token = tokenizer->peekNext();
    if (token.getType() != TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
        return false;
    }
    tokenizer->consumePeek();
    dec.name = token;
    if (tokenizer->peekNext().getType() != TokenType::OPEN_BRACE) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
        return false;
    }
    tokenizer->consumePeek();
    token = tokenizer->peekNext();
    StructDecList *prev = nullptr;
    StructDecList *list = &dec.decs;
    while (true) {
        if (token.getType() == TokenType::IDENTIFIER) {
            tokenizer->consumePeek();
            if (tokenizer->peekNext().getType() == TokenType::COLON) {
                tokenizer->consumePeek();
                list->type = StructDecType::VAR;
                list->varDec = memPool.makeVariableDec(VariableDec{token});
                ParseStatementErrorType errorType = parseVariableDec(*list->varDec);
                if (errorType != ParseStatementErrorType::NONE) {
                    return false;
                }
                if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                    return false;
                }
                if (list->varDec->initialAssignment) {
                    // not supported yet
                    unexpected.emplace_back(tokenizer->peeked, tokenizer->tokenizerIndex);
                    unexpected.back().unexpectedType = UnexpectedType::STRUCT_MEMBER_INITIALIZE_UNSUPPORTED;
                    return false;
                }
                tokenizer->consumePeek();
            }
            else {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COLON, tokenizer->tokenizerIndex);
                return false;
            }
        }
        else if (token.getType() == TokenType::FUNC) {
            tokenizer->consumePeek();
            list->type = StructDecType::FUNC;
            list->funcDec = memPool.makeFunctionDec();
            if (!parseFunction(*list->funcDec)) {
                return false;
            }
        }
        else if (token.getType() == TokenType::CLOSE_BRACE) {
            tokenizer->consumePeek();
            if (prev) {
                prev->next = nullptr;
                memPool.release(list);
            }
            return true;
        }
        else {
            if (token.getType() == TokenType::END_OF_FILE) {
                expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
            } else {
                unexpected.emplace_back(token, tokenizer->tokenizerIndex);
            }
            return false;
        }
        token = tokenizer->peekNext();
        prev = list;
        list->next = memPool.makeStructDecList();
        list = list->next;
    }
}

bool Parser::parseTemplate(TemplateDec& dec) {
    Token token = tokenizer->peekNext();
    if (token.getType() != TokenType::OPEN_BRACKET) {
        expected.emplace_back(ExpectedType::TOKEN, token, TokenType::OPEN_BRACKET, tokenizer->tokenizerIndex);
        return false;
    }
    tokenizer->consumePeek();
    TokenList *list = &dec.templateTypes;
    token = tokenizer->peekNext();
    while (true) {
        if (token.getType() != TokenType::IDENTIFIER) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::IDENTIFIER, tokenizer->tokenizerIndex);
            return false;
        }
        tokenizer->consumePeek();
        dec.templateTypes.token = token;
        if (tokenizer->peekNext().getType() == TokenType::CLOSE_BRACKET) {
            tokenizer->consumePeek();
            break;
        }
        if (tokenizer->peeked.getType() != TokenType::COMMA) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::COMMA, tokenizer->tokenizerIndex);
            return false;
        }
        tokenizer->consumePeek();
        token = tokenizer->peekNext();
        list->next = memPool.makeTokenList();
        list = list->next;
    }
    token = tokenizer->peekNext();
    if (token.getType() == TokenType::STRUCT) {
        tokenizer->consumePeek();
        dec.isStruct = true;
        return parseStruct(dec.structDec);
    } else if (token.getType() == TokenType::FUNC) {
        tokenizer->consumePeek();
        dec.isStruct = false;
        return parseFunction(dec.funcDec);
    } else {
        unexpected.emplace_back(token, tokenizer->tokenizerIndex);
        return false;
    }
}

/**
 * parses a scope. the first open brace should be consumed before calling this function
 * consumes the final close brace, unless there was an error
 * \returns one of ParseStatementErrorType::REPORTED and ParseStatementErrorType::NONE
*/
ParseStatementErrorType Parser::parseScope(StatementList& statementList) {
    StatementList* prev = nullptr;
    StatementList* list = &statementList;
    Token token = tokenizer->peekNext();
    while (token.getType() != TokenType::CLOSE_BRACE) {
        if (token.getType() == TokenType::END_OF_FILE) {
            expected.emplace_back(ExpectedType::TOKEN, token, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
            return ParseStatementErrorType::REPORTED;
        }
        ParseStatementErrorType errorType = parseStatement(list->curr);
        if (errorType != ParseStatementErrorType::NONE) {
            return errorType;
        }
        prev = list;
        list->next = memPool.makeStatementList();
        list = list->next;
        token = tokenizer->peekNext();
    }
    // consume close brace
    tokenizer->consumePeek();
    if (prev) {
        prev->next = nullptr;
        memPool.release(list);
    }
    return ParseStatementErrorType::NONE;
}

/**
 * parses a single statement within a scope
 * consumes the whole statement, unless there was an error
 * \returns one of ParseStatementErrorType::REPORTED and ParseStatementErrorType::NONE
*/
ParseStatementErrorType Parser::parseStatement(Statement &statement) {
    Token token = tokenizer->peekNext();
    if (token.getType() == TokenType::IDENTIFIER) { // varDec or expression
        tokenizer->consumePeek();
        if (parseIdentifierStatement(statement, token) != ParseStatementErrorType::NONE) {
            return ParseStatementErrorType::REPORTED;
        }
        if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
            return ParseStatementErrorType::REPORTED;
        }
        tokenizer->consumePeek();
        return ParseStatementErrorType::NONE;
    }
    else if (isControlFlow(token.getType())) {
        statement.type = StatementType::CONTROL_FLOW;
        statement.controlFlow = memPool.makeControlFlowStatement();
        if (token.getType() == TokenType::IF) {
            tokenizer->consumePeek();
            statement.controlFlow->type = ControlFlowStatementType::CONDITIONAL_STATEMENT;
            statement.controlFlow->conditional = memPool.makeConditionalStatement();
            ConditionalStatement& cond = *statement.controlFlow->conditional;
            if (parseBranchStatement(cond.ifStatement) == ParseStatementErrorType::REPORTED) {
                return ParseStatementErrorType::REPORTED;
            }

            ElifStatementList **curr = &cond.elifStatement;
            while (tokenizer->peekNext().getType() == TokenType::ELIF) {
                tokenizer->consumePeek();
                *curr = memPool.makeElifStatementList();
                if (parseBranchStatement((*curr)->elif) == ParseStatementErrorType::REPORTED) {
                    return ParseStatementErrorType::REPORTED;
                }
                curr = &(*curr)->next;
            }

            if (tokenizer->peeked.getType() == TokenType::ELSE) {
                tokenizer->consumePeek();
                if (tokenizer->peekNext().getType() != TokenType::OPEN_BRACE) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
                tokenizer->consumePeek();

                cond.elseStatement = memPool.makeScope();
                ParseStatementErrorType errorType = parseScope(cond.elseStatement->scopeStatements);
                if (errorType != ParseStatementErrorType::NONE) {
                    return ParseStatementErrorType::REPORTED;
                }
            }
        }
        else if (token.getType() == TokenType::WHILE) {
            tokenizer->consumePeek();
            statement.controlFlow->type = ControlFlowStatementType::WHILE_LOOP;
            statement.controlFlow->whileLoop = memPool.makeWhileLoop();
            if (parseBranchStatement(statement.controlFlow->whileLoop->loop) == ParseStatementErrorType::REPORTED) {
                return ParseStatementErrorType::REPORTED;
            }
        }
        else if (token.getType() == TokenType::RETURN) {
            tokenizer->consumePeek();
            statement.controlFlow->type = ControlFlowStatementType::RETURN_STATEMENT;
            statement.controlFlow->returnStatement = memPool.makeReturnStatement();
            auto& returnValue = statement.controlFlow->returnStatement->returnValue;
            if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                ParseExpressionErrorType errorType = parseExpression(returnValue);
                if (errorType != ParseExpressionErrorType::NONE) {
                    return ParseStatementErrorType::REPORTED;
                }
                if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
            }
            tokenizer->consumePeek();
        }
        else if (token.getType() == TokenType::FOR) {
            tokenizer->consumePeek();
            statement.controlFlow->type = ControlFlowStatementType::FOR_LOOP;
            statement.controlFlow->forLoop = memPool.makeForLoop();

            auto& forLoop = statement.controlFlow->forLoop;
            if (tokenizer->peekNext().getType() != TokenType::OPEN_PAREN) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_PAREN, tokenizer->tokenizerIndex);
                return ParseStatementErrorType::REPORTED;
            }
            // consume open paren
            tokenizer->consumePeek();
            Token next = tokenizer->peekNext();

            // parse initialize statement. can be expression or varDec
            if (next.getType() == TokenType::IDENTIFIER) {
                // consume identifier
                tokenizer->consumePeek();
                ParseStatementErrorType errorType = parseIdentifierStatement(forLoop->initialize, next);
                if (errorType != ParseStatementErrorType::NONE) {
                    return ParseStatementErrorType::REPORTED;
                }
                if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
            } else if (next.getType() != TokenType::SEMICOLON) {
                forLoop->initialize.type = StatementType::EXPRESSION;
                forLoop->initialize.expression = memPool.makeExpression();
                ParseExpressionErrorType errorType = parseExpression(*forLoop->initialize.expression);
                if (errorType != ParseExpressionErrorType::NONE) {
                    return ParseStatementErrorType::REPORTED;
                }
                if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
            }
            tokenizer->consumePeek();

            // parse condition statement
            if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                ParseExpressionErrorType errorType = parseExpression(forLoop->loop.condition);
                if (errorType != ParseExpressionErrorType::NONE) {
                    return ParseStatementErrorType::REPORTED;
                }
                if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
            }
            tokenizer->consumePeek();

            // parse iteration statement
            if (tokenizer->peekNext().getType() != TokenType::CLOSE_PAREN) {
                ParseExpressionErrorType errorType = parseExpression(forLoop->iteration);
                if (errorType != ParseExpressionErrorType::NONE) {
                    return ParseStatementErrorType::REPORTED;
                }
                if (tokenizer->peekNext().getType() != TokenType::CLOSE_PAREN) {
                    expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
            }
            tokenizer->consumePeek();

            // parse scope
            if (tokenizer->peekNext().getType() != TokenType::OPEN_BRACE) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
                return ParseStatementErrorType::REPORTED;
            }
            tokenizer->consumePeek();
            ParseStatementErrorType errorType = parseScope(forLoop->loop.body.scopeStatements);
            if (errorType != ParseStatementErrorType::NONE) {
                return ParseStatementErrorType::REPORTED;
            }
        }
        else if (token.getType() == TokenType::SWITCH) {
            tokenizer->consumePeek();
            statement.controlFlow->type = ControlFlowStatementType::SWITCH_STATEMENT;
            auto& switchStatement = statement.controlFlow->switchStatement;
            switchStatement = memPool.makeSwitchStatement();
            if (parseExpressionBeforeScope(switchStatement->switched) != ParseStatementErrorType::NONE) {
                return ParseStatementErrorType::REPORTED;
            }
            for (SwitchScopeStatementList *list = &switchStatement->body, *prev = nullptr;; list = list->next) {
                Token next = tokenizer->peekNext();
                if (next.getType() == TokenType::CASE) {
                    tokenizer->consumePeek();
                    list->caseExpression = memPool.makeExpression();
                    if (parseExpressionBeforeScope(*list->caseExpression) != ParseStatementErrorType::NONE) {
                        if (expected.back().expectedType != ExpectedType::TOKEN && expected.back().expectedTokenType != TokenType::OPEN_BRACE) {
                            return ParseStatementErrorType::REPORTED;
                        }
                        expected.pop_back();
                    } else {
                        list->caseBody = memPool.makeScope();
                        parseScope(list->caseBody->scopeStatements);
                    }
                }
                else if (next.getType() == TokenType::DEFAULT) {
                    tokenizer->consumePeek();
                    if (tokenizer->peekNext().getType() != TokenType::OPEN_BRACE) {
                        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACE, tokenizer->tokenizerIndex);
                        return ParseStatementErrorType::REPORTED;
                    }
                    // consume open brace
                    tokenizer->consumePeek();
                    list->caseBody = memPool.makeScope();
                    if (parseScope(list->caseBody->scopeStatements) != ParseStatementErrorType::NONE) {
                        return ParseStatementErrorType::REPORTED;
                    }
                }
                else if (next.getType() == TokenType::CLOSE_BRACE) {
                    if (prev) {
                        prev->next = nullptr;
                        memPool.release(list);
                    }
                    tokenizer->consumePeek();
                    break;
                }
                else {
                    unexpected.emplace_back(next, tokenizer->tokenizerIndex);
                    return ParseStatementErrorType::REPORTED;
                }
                prev = list;
                list->next = memPool.makeSwitchScopeStatementList();
            }
        }
        else if (token.getType() == TokenType::EXIT) {
            tokenizer->consumePeek();
            statement.controlFlow->type = ControlFlowStatementType::EXIT_STATEMENT;
            statement.controlFlow->returnStatement = memPool.makeReturnStatement();
            auto& exitValue = statement.controlFlow->returnStatement->returnValue;
            ParseExpressionErrorType errorType = parseExpression(exitValue);
            if (errorType != ParseExpressionErrorType::NONE) {
                return ParseStatementErrorType::REPORTED;
            }
            if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peekNext(), TokenType::SEMICOLON, tokenizer->tokenizerIndex);
                return ParseStatementErrorType::REPORTED;
            }
            tokenizer->consumePeek();
        }
    }
    else if (token.getType() == TokenType::OPEN_BRACE) { // scope
        tokenizer->consumePeek();
        statement.type = StatementType::SCOPE;
        statement.scope = memPool.makeScope();
        ParseStatementErrorType errorType = parseScope(statement.scope->scopeStatements);
        if (errorType != ParseStatementErrorType::NONE) {
            return ParseStatementErrorType::REPORTED;
        }
    }
    else if (token.getType() == TokenType::BREAK || token.getType() == TokenType::CONTINUE) {
        tokenizer->consumePeek();
        statement.type = StatementType::KEYWORD;
        statement.keyword = token;
        if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
            return ParseStatementErrorType::REPORTED;
        }
        tokenizer->consumePeek();
    }
    else if (notFirstOfExpression(token.getType())) { // unexpected token
        unexpected.emplace_back(token, tokenizer->tokenizerIndex);
        return ParseStatementErrorType::REPORTED;
    }
    else { // expression
        statement.type = StatementType::EXPRESSION;
        statement.expression = memPool.makeExpression();
        ParseExpressionErrorType errorType = parseExpression(*statement.expression);
        if (errorType != ParseExpressionErrorType::NONE) {
            return ParseStatementErrorType::REPORTED;
        }

        if (tokenizer->peekNext().getType() != TokenType::SEMICOLON) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::SEMICOLON, tokenizer->tokenizerIndex);
            return ParseStatementErrorType::REPORTED;
        }
        tokenizer->consumePeek();
    }

    return ParseStatementErrorType::NONE;
}

/**
 * consume the colon after the variable name before calling this function
 * \returns ParseStatementErrorType::REPORTED, or ParseStatementErrorType::NONE
*/
ParseStatementErrorType Parser::parseVariableDec(VariableDec& varDec) {
    ParseTypeErrorType typeErrorType = getType(varDec.type);
    if (typeErrorType != ParseTypeErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
    }
    Token next = tokenizer->peekNext();
    if (next.getType() == TokenType::ASSIGNMENT) {
        varDec.initialAssignment = memPool.makeExpression();
        tokenizer->consumePeek();
        varDec.initialAssignment = memPool.makeExpression();
        ParseExpressionErrorType errorType = parseExpression(*varDec.initialAssignment);
        if (errorType != ParseExpressionErrorType::NONE) {
            return ParseStatementErrorType::REPORTED;
        }
    }
    return ParseStatementErrorType::NONE;
}

/**
 * Parses a statement that starts with an identifier
 * \param token the identifier token. It should be consumed by the tokenizer
 * Does NOT consume the token after the statement (semicolon, comma, etc.)
*/
ParseStatementErrorType Parser::parseIdentifierStatement(Statement& statement, Token token) {
    Token next = tokenizer->peekNext();
    if (next.getType() == TokenType::COLON) {
        tokenizer->consumePeek();
        statement.type = StatementType::VARIABLE_DEC;
        statement.varDec = memPool.makeVariableDec(VariableDec{token});
        ParseStatementErrorType errorType = parseVariableDec(*statement.varDec);
        return errorType;
    }
    // expression
    statement.type = StatementType::EXPRESSION;
    statement.expression = memPool.makeExpression();
    tokenizer->position = token.getPosition();
    tokenizer->peeked.setType(TokenType::NONE);
    ParseExpressionErrorType errorType = parseExpression(*statement.expression);
    if (errorType != ParseExpressionErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
    }
    return ParseStatementErrorType::NONE;
}

/**
 * Parses if statements
 * \note this includes if, elif, and while statements
*/
ParseStatementErrorType Parser::parseBranchStatement(BranchStatement& condStatement) {
    if (parseExpressionBeforeScope(condStatement.condition) != ParseStatementErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
    }
    ParseStatementErrorType errorType = parseScope(condStatement.body.scopeStatements);
    if (errorType != ParseStatementErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
    }
    return ParseStatementErrorType::NONE;
}

ParseStatementErrorType Parser::parseExpressionBeforeScope(Expression& expression) {
    ParseExpressionErrorType errorType = parseExpression(expression);
    if (errorType != ParseExpressionErrorType::NONE) {
        return ParseStatementErrorType::REPORTED;
    }
    if (tokenizer->peekNext().getType() != TokenType::OPEN_BRACE) {
        expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::OPEN_BRACE, tokenizer->tokenizerIndex);
        return ParseStatementErrorType::REPORTED;
    }
    // consume open brace
    tokenizer->consumePeek();
    return ParseStatementErrorType::NONE;
}

/**
 * Extracts comma delimited expressions until it reaches something else, or an expression parse fails
 * Does NOT consume the final token
*/
ParseExpressionErrorType Parser::getIndexedExpressions(ExpressionList& expressions, TokenType close) {
    if (tokenizer->peekNext().getType() == close) {
        return ParseExpressionErrorType::NONE;
    }
    ExpressionList *list = &expressions;
    while (true) {
        bool indexed = tokenizer->peekNext().getType() == TokenType::DOT;
        if (indexed) {
            tokenizer->consumePeek();
        }
        ParseExpressionErrorType errorType = parseExpression(list->curr);
        if (errorType != ParseExpressionErrorType::NONE) {
            return errorType;
        }
        if (indexed) {
            if (list->curr.getType() != ExpressionType::BINARY_OP && list->curr.getBinOp()->op.getType() != TokenType::ASSIGNMENT) {
                return ParseExpressionErrorType::REPORTED;
            }
            list->curr.getBinOp()->op.setType(TokenType::INDEXED_ASSIGNMENT);
        }
        if (tokenizer->peekNext().getType() != TokenType::COMMA) {
            return ParseExpressionErrorType::NONE;
        }
        tokenizer->consumePeek();
        list->next = memPool.makeExpressionList();
        list = list->next;
    }
}

/**
 * Parses a complete expression until it reaches something else, placing the root expression in rootExpression
 * Consumes the entire expression unless there was an error
*/
ParseExpressionErrorType Parser::parseExpression(Expression& rootExpression) {
    // ensures each part of the BinOp and UnOp lines
    assert(&((BinOp *)nullptr)->rightSide == &((UnOp *)nullptr)->operand);
    assert(&((BinOp *)nullptr)->op == &((UnOp *)nullptr)->op);
    Token token = tokenizer->peekNext();
    // bottom should always point to the leaf node
    Expression *bottom = &rootExpression;
    while (true) {
        if (!isUnaryOp(token.getType())) {
            ParseExpressionErrorType errorType = parseLeaf(*bottom);
            if (errorType != ParseExpressionErrorType::NONE) {
                return errorType;
            }
            token = tokenizer->peekNext();
        }
        OPERATOR_PARSE:
        const ExpressionType expressionType = isBinaryOp(token.getType()) ? ExpressionType::BINARY_OP : isUnaryOp(token.getType()) ? ExpressionType::UNARY_OP : ExpressionType::NONE;
        if (expressionType == ExpressionType::NONE) {
            return ParseExpressionErrorType::NONE;
        }
        tokenizer->consumePeek();
        // place expression in right spot within tree based on precedence
        Expression* curr = &rootExpression;
        // iterate down and to the right of the tree until we find precedence match
        while (curr != bottom) { // this condition checks that curr has not reached the last leaf node
            // reliant on BinOp.op and UnOp.op being the same spot in their structs (assertion at start)
            if (operatorPrecedence[(uint8_t)token.getType()] <= operatorPrecedence[(uint8_t)curr->getBinOp()->op.getType()]) {
                break;
            }
            // reliant on BinOp.rightSide and UnOp.operand being the same spot in their structs (assertion at start)
            curr = &curr->getBinOp()->rightSide;
        }
        if (expressionType == ExpressionType::BINARY_OP) {
            BinOp *binOp = memPool.makeBinOp(BinOp{token});
            // set bottom to next empty expression
            bottom = &binOp->rightSide;
            // copy curr into this binary op
            binOp->leftSide = *curr;
            // change curr to point to the new binary op
            curr->setBinOp(binOp);
        } else {
            UnOp *unOp = memPool.makeUnOp(UnOp{token});
            // set bottom to next empty expression
            bottom = &unOp->operand;
            // have to consider postfix operators
            if (token.getType() == TokenType::DECREMENT_POSTFIX || token.getType() == TokenType::INCREMENT_POSTFIX) {
                // copy curr into this unary op
                unOp->operand = *curr;
                // change curr to point to the new unary op
                curr->setUnOp(unOp);
                token = tokenizer->peekNext();
                /* have to do a jump since postfix operators
                are followed by an operator or end the expression.
                bottom is already used up */
                goto OPERATOR_PARSE;
            } else {
                // we dont need to copy curr since curr is empty
                // change curr to point to the new unary op
                curr->setUnOp(unOp);
            }
        }
        token = tokenizer->peekNext();
    }
    return ParseExpressionErrorType::NONE;
}

ParseExpressionErrorType Parser::parseLeaf(Expression& expression) {
    const Token token = tokenizer->peekNext();
    if (isLiteral(token.getType())) {
        tokenizer->consumePeek();
        expression.setToken(token);
    }
    else if (token.getType() == TokenType::OPEN_PAREN) {
        tokenizer->consumePeek();
        ParseExpressionErrorType errorType = parseExpression(expression);
        if (errorType != ParseExpressionErrorType::NONE) {
            return ParseExpressionErrorType::REPORTED;
        }
        if (tokenizer->peekNext().getType() != TokenType::CLOSE_PAREN) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
            return ParseExpressionErrorType::REPORTED;
        }
        tokenizer->consumePeek();
    }
    else if (token.getType() == TokenType::IDENTIFIER) {
        tokenizer->consumePeek();
        Token next = tokenizer->peekNext();
        if (next.getType() == TokenType::OPEN_PAREN) {
            tokenizer->consumePeek();
            expression.setFunctionCall(memPool.makeFunctionCall(FunctionCall{token}));
            ParseExpressionErrorType errorType = getIndexedExpressions(expression.getFunctionCall()->args, TokenType::CLOSE_PAREN);
            if (errorType != ParseExpressionErrorType::NONE) {
                return ParseExpressionErrorType::REPORTED;
            }
            if (tokenizer->peekNext().getType() != TokenType::CLOSE_PAREN) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_PAREN, tokenizer->tokenizerIndex);
                return ParseExpressionErrorType::REPORTED;
            }
            tokenizer->consumePeek();
        }
        else if (next.getType() == TokenType::OPEN_BRACKET) {
            tokenizer->consumePeek();
            expression.setArrayAccess(memPool.makeArrayAccess(ArrayAccess{token}));
            ParseExpressionErrorType errorType = parseExpression(expression.getArrayAccess()->offset);
            if (errorType != ParseExpressionErrorType::NONE) {
                return ParseExpressionErrorType::REPORTED;
            }
            if (tokenizer->peekNext().getType() != TokenType::CLOSE_BRACKET) {
                expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
                return ParseExpressionErrorType::REPORTED;
            }
            tokenizer->consumePeek();
        }
        else {
            expression.setToken(token);
        }
    }
    else if (token.getType() == TokenType::OPEN_BRACKET) {
        // very similar to function call parsing
        tokenizer->consumePeek();
        expression.setContainerLiteral(memPool.makeContainerLiteral());
        if (tokenizer->peekNext().getType() == TokenType::CLOSE_BRACKET) {
            tokenizer->consumePeek();
            expression.getContainerLiteral()->values.curr.setToken(token);
            expression.getContainerLiteral()->values.curr.setType(ExpressionType::NONE);
            return ParseExpressionErrorType::NONE;
        }
        ExpressionList *list = &expression.getContainerLiteral()->values;
        ParseExpressionErrorType errorType = getIndexedExpressions(*list, TokenType::CLOSE_BRACKET);
        if (errorType != ParseExpressionErrorType::NONE) {
            return ParseExpressionErrorType::REPORTED;
        }
        if (tokenizer->peekNext().getType() != TokenType::CLOSE_BRACKET) {
            expected.emplace_back(ExpectedType::TOKEN, tokenizer->peeked, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
            return ParseExpressionErrorType::REPORTED;
        }
        tokenizer->consumePeek();
    }
    else {
        expected.emplace_back(ExpectedType::EXPRESSION, token, tokenizer->tokenizerIndex);
        return ParseExpressionErrorType::REPORTED;
    }
    return ParseExpressionErrorType::NONE;
}

/**
 * Parses a type, tokens are stored in reverse order
 * Ex:
 *  var : int32 ptr
 *  would be stored with first being ptr, and next being int32
 * Does NOT consume the final token
*/
ParseTypeErrorType Parser::getType(TokenList& type) {
    Token tp = tokenizer->peekNext();
    TokenList *curr = memPool.makeTokenList();
    if (!isBuiltInType(tp.getType()) && !isTypeQualifier(tp.getType()) && tp.getType() != TokenType::REFERENCE && tp.getType() != TokenType::IDENTIFIER) {
        expected.emplace_back(ExpectedType::TOKEN, tp, TokenType::TYPE, tokenizer->tokenizerIndex);
        return ParseTypeErrorType::REPORTED;
    }
    {
        tokenizer->consumePeek();
        curr->token = tp;
        TokenList *prev = curr;
        curr = memPool.makeTokenList();
        curr->next = prev;
        tp = tokenizer->peekNext();
    }
    while (tp.getType() != TokenType::END_OF_FILE) {
        if (tp.getType() == TokenType::OPEN_BRACKET) {
            // array type
            tokenizer->consumePeek();
            // this is kinda a cursed way to do this, but the length field will act as a isLengthGiven boolean.
            // if token.length evaluates to true, this means the following token (curr->next) contains the length for the array
            // else no length was given, and the next token is whatever follows
            curr->token = {tp.getPosition(), 0, TokenType::ARRAY_TYPE};
            tp = tokenizer->tokenizeNext();
            if (
                tp.getType() == TokenType::DECIMAL_NUMBER ||
                tp.getType() == TokenType::HEX_NUMBER ||
                tp.getType() == TokenType::BINARY_NUMBER ||
                tp.getType() == TokenType::IDENTIFIER
            ) {
                // set length to 'true' to say next token is the length
                curr->token.setLength(1);

                // make length token list
                TokenList* lengthToken = memPool.makeTokenList();
                lengthToken->token = tp;

                // insert between array type and next
                lengthToken->next = curr->next;
                curr->next = lengthToken;

                tp = tokenizer->tokenizeNext();
            }
            TokenList *prev = curr;
            curr = memPool.makeTokenList();
            curr->next = prev;

            if (tp.getType() != TokenType::CLOSE_BRACKET) {
                expected.emplace_back(ExpectedType::TOKEN, tp, TokenType::CLOSE_BRACKET, tokenizer->tokenizerIndex);
                return ParseTypeErrorType::REPORTED;
            }
            tp = tokenizer->peekNext();
        }
        else if (!isBuiltInType(tp.getType()) && !isTypeQualifier(tp.getType()) && tp.getType() != TokenType::REFERENCE && tp.getType() != TokenType::IDENTIFIER) {
            break;
        } else {
            tokenizer->consumePeek();
            curr->token = tp;
            TokenList *prev = curr;
            curr = memPool.makeTokenList();
            curr->next = prev;
            tp = tokenizer->peekNext();
        }
    }
    type = *curr->next;
    memPool.release(curr);
    return ParseTypeErrorType::NONE;
}
