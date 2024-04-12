#pragma once

#include <vector>
#include <unordered_map>
#include "token.hpp"

/**
 * Stores extra token position information not directly found in the token
*/
struct TokenPositionInfo{
    uint32_t lineNum;
    uint32_t linePos;
    TokenPositionInfo() = delete;
    TokenPositionInfo(uint32_t lineNum, uint32_t linePos);
};

/**
 * Tokenizer class
*/
struct Tokenizer {
    /**
     * Stores newline positions
    */
    std::vector<uint32_t> newlinePositions;

    /**
     * Path to the file that we are tokenizing
    */
    const std::string filePath;

    /**
     * File content
    */
    const std::vector<unsigned char> content;

    /**
     * Used to store extracted tokens
    */
    std::string extracted;

    /**
     * Stores peeked tokens
    */
    Token peeked;

    /**
     * Current position within 'content'
    */
    uint32_t position{0};

    /**
     * This tokenizer's index within a tokenizers array
     * Helps with switch between tokenizers
    */
    uint32_t tokenizerIndex{0};

    /**
     * The previous token type
    */
    TokenType prevType{TokenType::NONE};


    explicit Tokenizer(std::string&& filePath, std::vector<unsigned char>&& fileContent);
    explicit Tokenizer(std::string&& filePath, const std::vector<unsigned char>& fileContent);
    explicit Tokenizer(std::string&& filePath, const std::string& fileContent);

    /**
     * Tokenize all remaining tokens
     * \param tokens a vector to store tokens in
    */
    void tokenizeAll(std::vector<Token>& tokens);

    /**
     * Tokenize the next token. If 'peeked' is set, it will be returned, and peeked will be cleared
    */
    Token tokenizeNext();

    /**
     * Peek at the next token. Successive calls to this will return the same token.
    */
    Token peekNext();

    /**
     * Clears the peeked token so that following call to any one of the tokenize functions returns the next token
    */
    void consumePeek();

    /**
     * Extract a token as it appears in source.
     * The tokenizer object contains a member of type std::string to reduce allocations while extracting many tokens, so
     * If you are planing on calling this multiple times, you must make a copy rather than take a reference,
     * otherwise the next call to this function will modify your reference
     * \param token the token to extract
    */
    const std::string& extractToken(const Token& token);

    /**
     * Gets some extra token position information
     * \param token the token to get information for
    */
    TokenPositionInfo getTokenPositionInfo(const Token& token);

private:
    void setup();

    void moveToNextNonWhiteSpaceChar();
    void movePastIdentifier();
    /**
     * Moves past a base 10 integer. 'position' will be set to the character just after the number
    */
    void movePastNumber();
    /**
     * Moves past a hex number. 'position' will be set to the character just after the hex number
    */
    void movePastHexNumber();
    /**
     * Move past a string or char literal, accounting for escaped delimiters
     * \param delimiter the delimiter of the string/char literal. " or '
     * \returns true if we moved past the literal successfully, false if the literal is unclosed
    */
    bool movePastLiteral(char delimiter);
    /**
     * Moves past the next newline character. 'position' will be set to the character just after the newline
    */
    void movePastNewLine();
};
