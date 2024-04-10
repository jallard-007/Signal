#include <iostream>
#include <cstring>
#include "tokenizer.hpp"

TokenPositionInfo::TokenPositionInfo(uint32_t lineNum, uint32_t linePos): lineNum{lineNum}, linePos{linePos} {}

Tokenizer::Tokenizer(std::string&& filePath, std::vector<unsigned char>&& fileContent):
    filePath{std::move(filePath)}, content{std::move(fileContent)}
{
    if (fileContent.size() > UINT32_MAX) {
        std::cerr << "File too large\n";
        exit(1);
    }
    newlinePositions.reserve(fileContent.size() / 40);
    newlinePositions.emplace_back(0);
}
Tokenizer::Tokenizer(std::string&& filePath, const std::vector<unsigned char>& fileContent):
    filePath{std::move(filePath)}, content{fileContent}
{
    if (fileContent.size() > UINT32_MAX) {
        std::cerr << "File too large\n";
        exit(1);
    }
    newlinePositions.reserve(fileContent.size() / 40);
    newlinePositions.emplace_back(0);
}
Tokenizer::Tokenizer(std::string&& filePath, const std::string& fileContent):
    filePath{std::move(filePath)}, content{fileContent.begin(), fileContent.end() + 1}
{
    if (fileContent.size() > UINT32_MAX) {
        std::cerr << "File too large\n";
        exit(1);
    }
    newlinePositions.reserve(fileContent.size() / 40);
    newlinePositions.emplace_back(0);
}

// does binary search on the newline list to find the line number
TokenPositionInfo Tokenizer::getTokenPositionInfo(const Token& tk) {
    if (newlinePositions.empty()) {
        return {1, tk.getPosition() + 1};
    }
    uint32_t high = (uint32_t)newlinePositions.size() - 1;
    uint32_t low = 0;
    uint32_t middle = high / 2;
    while (low < high) {
        if (tk.getPosition() < newlinePositions[middle]) {
            high = middle - 1;
        }
        else if (tk.getPosition() >= newlinePositions[middle + 1]) {
            low = middle + 1;
        }
        else {
            return {middle + 1, tk.getPosition() + 1 - newlinePositions[middle]};
        }
        middle = (high + low) / 2;
    }
    return {high + 1, tk.getPosition() + 1 - newlinePositions[high]};
}

void Tokenizer::tokenizeAll(std::vector<Token>& tokens) {
    while (tokens.emplace_back(tokenizeNext()).getType() != TokenType::END_OF_FILE);
}

#define END_OF_IDENTIFIER(c) TokenType tNext = numToType[(uint8_t)content[++position]]; \
if (tNext != TokenType::IDENTIFIER && tNext != TokenType::DECIMAL_NUMBER) { \
    type = c; \
    break; \
}

/**
 * Allows peeking to the next token
 * Successive calls to this function will return the same Token.
 * The Token must be consumed by calling tokenizeNext before peeking to the next
*/
Token Tokenizer::peekNext() {
    if (peeked.getType() != TokenType::NONE) {
        return peeked;
    }
    peeked = tokenizeNext();
    return peeked;
}

void Tokenizer::consumePeek() {
    peeked.setType(TokenType::NONE);
}

Token Tokenizer::tokenizeNext() {
    if (peeked.getType() != TokenType::NONE) {
        const Token temp = peeked;
        peeked.setType(TokenType::NONE);
        return temp;
    }
    moveToNextNonWhiteSpaceChar();
    const uint32_t tokenStartPos = position;
    char c = content[position];
    if (c < 0) {
        std::cerr << "Error: Non-ASCII character encountered: value = [" << (uint8_t)c << "]\n";
        exit(1);
    }
    TokenType type = numToType[(uint8_t)c];
    switch (type) {
        case TokenType::IDENTIFIER: {
            movePastIdentifier();
            const uint32_t length = position - tokenStartPos;
            switch (length) {
                case 2: {
                    switch (c) {
                        case 'a': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "s", sizeof("s") - 1) == 0) {
                                type = TokenType::AS; break;
                            }
                            break;
                        }
                        case 'i': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "f", sizeof("f") - 1) == 0) {
                                type = TokenType::IF; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 3: {
                    switch (c) {
                        case 'f': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "or", sizeof("or") - 1) == 0) {
                                type = TokenType::FOR; break;
                            }
                            break;
                        }
                        case 'p': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "tr", sizeof("tr") - 1) == 0) {
                                type = TokenType::POINTER; break;
                            }
                            break;
                        }
                        case 'r': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "ef", sizeof("ef") - 1) == 0) {
                                type = TokenType::REFERENCE; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 4: {
                    switch (c) {
                        case 'b': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "ool", sizeof("ool") - 1) == 0) {
                                type = TokenType::BOOL; break;
                            }
                            break;
                        }
                        case 'c': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "ase", sizeof("ase") - 1) == 0) {
                                type = TokenType::CASE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "har", sizeof("har") - 1) == 0) {
                                type = TokenType::CHAR_TYPE; break;
                            }
                            break;
                        }
                        case 'e': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "lif", sizeof("lif") - 1) == 0) {
                                type = TokenType::ELIF; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "lse", sizeof("lse") - 1) == 0) {
                                type = TokenType::ELSE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "num", sizeof("num") - 1) == 0) {
                                type = TokenType::ENUM; break;
                            }
                            break;
                        }
                        case 'f': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "unc", sizeof("unc") - 1) == 0) {
                                type = TokenType::FUNC; break;
                            }
                            break;
                        }
                        case 'i': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "nt8", sizeof("nt8") - 1) == 0) {
                                type = TokenType::INT8_TYPE; break;
                            }
                            break;
                        }
                        case 't': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "rue", sizeof("rue") - 1) == 0) {
                                type = TokenType::TRUE; break;
                            }
                            break;
                        }
                        case 'v': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "oid", sizeof("oid") - 1) == 0) {
                                type = TokenType::VOID; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 5: {
                    switch (c) {
                        case 'b': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "reak", sizeof("reak") - 1) == 0) {
                                type = TokenType::BREAK; break;
                            }
                            break;
                        }
                        case 'c': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "onst", sizeof("onst") - 1) == 0) {
                                type = TokenType::CONST; break;
                            }
                            break;
                        }
                        case 'f': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "alse", sizeof("alse") - 1) == 0) {
                                type = TokenType::FALSE; break;
                            }
                            break;
                        }
                        case 'i': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "nt16", sizeof("nt16") - 1) == 0) {
                                type = TokenType::INT16_TYPE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "nt32", sizeof("nt32") - 1) == 0) {
                                type = TokenType::INT32_TYPE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "nt64", sizeof("nt64") - 1) == 0) {
                                type = TokenType::INT64_TYPE; break;
                            }
                            break;
                        }
                        case 's': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "tdin", sizeof("tdin") - 1) == 0) {
                                type = TokenType::STDIN; break;
                            }
                            break;
                        }
                        case 'u': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "int8", sizeof("int8") - 1) == 0) {
                                type = TokenType::UINT8_TYPE; break;
                            }
                            break;
                        }
                        case 'w': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "hile", sizeof("hile") - 1) == 0) {
                                type = TokenType::WHILE; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 6: {
                    switch (c) {
                        case 'c': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "reate", sizeof("reate") - 1) == 0) {
                                type = TokenType::CREATE; break;
                            }
                            break;
                        }
                        case 'd': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "efine", sizeof("efine") - 1) == 0) {
                                type = TokenType::DEFINE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "ouble", sizeof("ouble") - 1) == 0) {
                                type = TokenType::DOUBLE_TYPE; break;
                            }
                            break;
                        }
                        case 'f': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "ile_t", sizeof("ile_t") - 1) == 0) {
                                type = TokenType::FILE_TYPE; break;
                            }
                            break;
                        }
                        case 'r': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "eturn", sizeof("eturn") - 1) == 0) {
                                type = TokenType::RETURN; break;
                            }
                            break;
                        }
                        case 's': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "izeof", sizeof("izeof") - 1) == 0) {
                                type = TokenType::SIZEOF; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "tderr", sizeof("tderr") - 1) == 0) {
                                type = TokenType::STDERR; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "tdout", sizeof("tdout") - 1) == 0) {
                                type = TokenType::STDOUT; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "truct", sizeof("truct") - 1) == 0) {
                                type = TokenType::STRUCT; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "witch", sizeof("witch") - 1) == 0) {
                                type = TokenType::SWITCH; break;
                            }
                            break;
                        }
                        case 'u': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "int16", sizeof("int16") - 1) == 0) {
                                type = TokenType::UINT16_TYPE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "int32", sizeof("int32") - 1) == 0) {
                                type = TokenType::UINT32_TYPE; break;
                            }
                            if (strncmp((const char *)&content[tokenStartPos +1], "int64", sizeof("int64") - 1) == 0) {
                                type = TokenType::UINT64_TYPE; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 7: {
                    switch (c) {
                        case 'd': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "efault", sizeof("efault") - 1) == 0) {
                                type = TokenType::DEFAULT; break;
                            }
                            break;
                        }
                        case 'i': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "nclude", sizeof("nclude") - 1) == 0) {
                                type = TokenType::INCLUDE; break;
                            }
                            break;
                        }
                        case 'n': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "ullptr", sizeof("ullptr") - 1) == 0) {
                                type = TokenType::NULL_PTR; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 8: {
                    switch (c) {
                        case 'c': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "ontinue", sizeof("ontinue") - 1) == 0) {
                                type = TokenType::CONTINUE; break;
                            }
                            break;
                        }
                        case 't': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "emplate", sizeof("emplate") - 1) == 0) {
                                type = TokenType::TEMPLATE; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                case 9: {
                    switch (c) {
                        case '_': {
                            if (strncmp((const char *)&content[tokenStartPos +1], "_builtin", sizeof("_builtin") - 1) == 0) {
                                type = TokenType::BUILTIN; break;
                            }
                            break;
                        }
                        default: break;
                    }
                    break;
                }
                default: break;
            }
            break;
        }

        case TokenType::END_OF_FILE: {
            break;
        }

        case TokenType::STRING_LITERAL: {
            if (!movePastLiteral('"')) {
                TokenPositionInfo posInfo = getTokenPositionInfo({position, 0, TokenType::STRING_LITERAL});
                std::cerr << filePath << ':' << posInfo.lineNum << ':' << posInfo.linePos << "\nUnclosed string literal\n";
                exit(1);
            }
            break;
        }

        case TokenType::CHAR_LITERAL: {
            if (!movePastLiteral('\'')) {
                TokenPositionInfo posInfo = getTokenPositionInfo({position, 0, TokenType::CHAR_LITERAL});
                std::cerr << filePath << ':' << posInfo.lineNum << ':' << posInfo.linePos << "\nUnclosed character literal\n";
                exit(1);
            }
            // TODO: validate content of character. escaped characters :(
            // maybe do this during type checking?
            // ' '\n \r \t \' \" \\ \v \f \e \b \a \127 - \0
            break;
        }

        case TokenType::COMMENT: {
            movePastNewLine();
            return tokenizeNext();
        }

        case TokenType::NEWLINE: {
            newlinePositions.emplace_back(++position);
            return tokenizeNext();
        }

        case TokenType::DECIMAL_NUMBER: {
            if (c == '0' && position + 1 < content.size()) {
                c = content[++position];
                if (c == 'x') {
                    type = TokenType::HEX_NUMBER;
                    movePastHexNumber();
                } else {
                    if (c == 'b') {
                        type = TokenType::BINARY_NUMBER;
                    } else {
                        type = TokenType::DECIMAL_NUMBER;
                        --position;
                    }
                    movePastNumber();
                }
            } else {
                type = TokenType::DECIMAL_NUMBER;
                movePastNumber();
            }
            if (content[position] == '.' && type == TokenType::DECIMAL_NUMBER) {
                uint32_t prevPosition = position;
                // float
                movePastNumber();
                if (position > prevPosition + 1) {
                    type = TokenType::FLOAT_NUMBER;
                } else {
                    position = prevPosition;
                }
            }
            break;
        }
    
        case TokenType::BAD_VALUE: {
            std::cerr << filePath << ':' << std::to_string(newlinePositions.size() + 1) << ':' << std::to_string(position + 1 - newlinePositions.back()) << '\n'; 
            std::cerr << "Invalid character with ASCII code: [" << (int)c << "]\n";
            exit(1);
        }

        default: {
            const TokenType tNext = numToType[(uint8_t)content[++position]];
            switch (type) {
                case TokenType::NOT: {
                    if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::NOT_EQUAL;
                    }
                    break;
                }
                case TokenType::BITWISE_XOR: {
                    if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::BITWISE_XOR_ASSIGNMENT;
                    }
                    break;
                }
                case TokenType::MODULO: {
                    if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::MODULO_ASSIGNMENT;
                    }
                    break;
                }
                case TokenType::ASSIGNMENT: {
                    if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::EQUAL;
                    }
                    break;
                }
                case TokenType::DIVISION: {
                    if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::DIVISION_ASSIGNMENT;
                    }
                    break;
                }
                case TokenType::BITWISE_OR: {
                    if (tNext == TokenType::BITWISE_OR) {
                        type = TokenType::LOGICAL_OR;
                        ++position;
                    } else if (tNext == TokenType::ASSIGNMENT) {
                        type = TokenType::BITWISE_OR_ASSIGNMENT;
                        ++position;
                    }
                    break;
                }
                case TokenType::BITWISE_AND: {
                    if (tNext == TokenType::BITWISE_AND) {
                        type = TokenType::LOGICAL_AND;
                        ++position;
                    } else if (tNext == TokenType::ASSIGNMENT) {
                        type = TokenType::BITWISE_AND_ASSIGNMENT;
                        ++position;
                    }
                    break;
                }
                case TokenType::LESS_THAN: {
                    if (tNext == TokenType::LESS_THAN) {
                        ++position;
                        if (numToType[(uint8_t)content[position]] == TokenType::ASSIGNMENT) {
                            ++position;
                            type = TokenType::SHIFT_LEFT_ASSIGNMENT;
                        } else {
                            type = TokenType::SHIFT_LEFT;
                        }
                    } else if (tNext == TokenType::ASSIGNMENT) {
                        type = TokenType::LESS_THAN_EQUAL;
                        ++position;
                    }
                    break;
                }
                case TokenType::GREATER_THAN: {
                    if (tNext == TokenType::GREATER_THAN) {
                        ++position;
                        if (numToType[(uint8_t)content[position]] == TokenType::ASSIGNMENT) {
                            ++position;
                            type = TokenType::SHIFT_RIGHT_ASSIGNMENT;
                        } else {
                            type = TokenType::SHIFT_RIGHT;
                        }
                    } else if (tNext == TokenType::ASSIGNMENT) {
                        type = TokenType::GREATER_THAN_EQUAL;
                        ++position;
                    }
                    break;
                }
                case TokenType::SUBTRACTION: {
                    if (tNext == TokenType::GREATER_THAN) {
                        ++position;
                        type = TokenType::PTR_MEMBER_ACCESS;
                    } else if (tNext == TokenType::SUBTRACTION) {
                        ++position;
                        if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACKET) {
                            type = TokenType::DECREMENT_POSTFIX;
                        } else {
                            type = TokenType::DECREMENT_PREFIX;
                        }
                    } else if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::SUBTRACTION_ASSIGNMENT;
                    } else if (
                        prevType != TokenType::IDENTIFIER &&
                        prevType != TokenType::CLOSE_PAREN &&
                        prevType != TokenType::CLOSE_BRACKET && !isLiteral(prevType)
                    ) {
                        type = TokenType::NEGATIVE;
                    }
                    break;
                }
                case TokenType::ADDITION: {
                    if (tNext == TokenType::ADDITION) {
                        ++position;
                        if (prevType == TokenType::IDENTIFIER || prevType == TokenType::CLOSE_PAREN || prevType == TokenType::CLOSE_BRACE) {
                            type = TokenType::INCREMENT_POSTFIX;
                        } else {
                            type = TokenType::INCREMENT_PREFIX;
                        }
                    } else if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::ADDITION_ASSIGNMENT;
                    }
                    break;
                }
                case TokenType::MULTIPLICATION: {
                    if (tNext == TokenType::ASSIGNMENT) {
                        ++position;
                        type = TokenType::MULTIPLICATION_ASSIGNMENT;
                    } else if (prevType == TokenType::OPEN_BRACE || prevType == TokenType::OPEN_PAREN || prevType == TokenType::OPEN_BRACKET || isBinaryOp(prevType)) {
                        type = TokenType::DEREFERENCE;
                    }
                    break;
                }
                default: {
                    break;
                }
            }
            break;
        }
    }

    if (position - tokenStartPos > UINT16_MAX) {
        // error
        std::cerr << "Token too long\n";
        exit(1);
    }
    prevType = type;
    return {tokenStartPos, (uint16_t)(position - tokenStartPos), type};
}

void Tokenizer::moveToNextNonWhiteSpaceChar() {
    for (; position < content.size(); ++position) {
        const char c = content[position];
        if (c != ' ' && c != '\t') {
            return;
        }
    }
}

void Tokenizer::movePastIdentifier() {
    for (; position < content.size(); ++position) {
        const TokenType type = numToType[(uint8_t)content[position]];
        if (type != TokenType::IDENTIFIER && type != TokenType::DECIMAL_NUMBER) {
            return;
        }
    }
}

void Tokenizer::movePastNumber() {
    for (++position; position < content.size(); ++position) {
        const char c = content[position];
        if (c < '0' || c > '9') {
            return;
        }
    }
}

void Tokenizer::movePastHexNumber() {
    for (++position; position < content.size(); ++position) {
        const char c = content[position];
        if (!((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'))) {
            return;
        }
    }
}

bool Tokenizer::movePastLiteral(char delimiter) {
    char prev = content[position];
    char prevPrev = content[position];
    for (++position; position < content.size(); ++position) {
        const char c = content[position];
        if (c == '\n') {
            newlinePositions.emplace_back(++position);
            return false;
        }
        if (c == delimiter && !(prev == '\\' && prevPrev != '\\')) {
            ++position;
            return true;
        }
        prevPrev = prev;
        prev = c;
    }
    return false;
}

void Tokenizer::movePastNewLine() {
    for (; position < content.size(); ++position) {
        if (content[position] == '\n') {
            newlinePositions.emplace_back(++position);
            return;
        }
    }
}

const std::string& Tokenizer::extractToken(const Token &token) {
    extracted.resize(token.getLength());
    ::memcpy(extracted.data(), content.data() + token.getPosition(), token.getLength());
    // content.copy(extracted.data(), token.length, token.position);
    return extracted;
}
