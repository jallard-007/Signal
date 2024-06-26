#pragma once

#include <string>
#include <cstdint>
#include <iostream>
#include <unordered_map>

enum class TokenType : uint8_t {
    // special
    NONE,
    BAD_VALUE,
    END_OF_FILE,
    BUILTIN,
    DEFINE,

    // literals
    CHAR_LITERAL,
    STRING_LITERAL,
    DECIMAL_NUMBER,
    BINARY_NUMBER,
    FLOAT_NUMBER,
    HEX_NUMBER,
    FALSE, //
    TRUE, //
    STDIN,
    STDERR,
    STDOUT,
    NULL_PTR, //

    // keywords
    AS, //
    BREAK,
    CASE, //
    CONTINUE,
    CREATE, //
    DEFAULT, //
    ELIF,
    ELSE,
    IF,
    FOR,
    SWITCH, //
    RETURN,
    EXIT,
    WHILE,
    ENUM,
    FUNC,
    INCLUDE, //
    EXTERN,
    STRUCT,
    TEMPLATE,

    // general
    IDENTIFIER,
    COMMENT,
    NEWLINE,
    OPEN_PAREN,
    OPEN_BRACE,
    OPEN_BRACKET,
    CLOSE_PAREN,
    CLOSE_BRACE,
    CLOSE_BRACKET,
    SEMICOLON,
    BACK_SLASH,
    COLON,
    COMMA,
    TERNARY,

    // BINARY
    // general
    DOT,
    PTR_MEMBER_ACCESS,

    // arithmetic
    ADDITION,
    SUBTRACTION,
    MULTIPLICATION,
    DIVISION,
    MODULO,
    BITWISE_OR,
    BITWISE_AND,
    BITWISE_XOR,
    SHIFT_LEFT,
    SHIFT_RIGHT,

    // assignments
    ASSIGNMENT,
    ADDITION_ASSIGNMENT,
    SUBTRACTION_ASSIGNMENT,
    MULTIPLICATION_ASSIGNMENT,
    DIVISION_ASSIGNMENT,
    MODULO_ASSIGNMENT,
    BITWISE_OR_ASSIGNMENT,
    BITWISE_XOR_ASSIGNMENT,
    BITWISE_AND_ASSIGNMENT,
    SHIFT_LEFT_ASSIGNMENT,
    SHIFT_RIGHT_ASSIGNMENT,

    // logical
    EQUAL,
    NOT_EQUAL,
    LOGICAL_AND,
    LOGICAL_OR,
    LESS_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,

    // UNARY
    NOT,
    SIZEOF,
    ADDRESS_OF,
    DEREFERENCE,
    INCREMENT_POSTFIX,
    INCREMENT_PREFIX,
    DECREMENT_POSTFIX,
    DECREMENT_PREFIX,
    NEGATIVE,

    // types
    VOID,
    BOOL,
    CHAR_TYPE,
    INT8_TYPE,
    UINT8_TYPE,
    INT16_TYPE,
    UINT16_TYPE,
    INT32_TYPE,
    UINT32_TYPE,
    INT64_TYPE,
    UINT64_TYPE,
    FILE_TYPE,
    DOUBLE_TYPE,
    POINTER,
    ARRAY_TYPE,
    REFERENCE,

    // type modifiers
    CONST,

    // extra types used by parser to report errors
    TYPE,
    OPERATOR,

    // extra types used by checker
    DEC_PTR,
    /* container literal is different from array literal in that container literal is a temporary,
     whereas array literals have space allocated for them.
     can assign container literals to array type
     can assign array type to ptr type
     can assign ptr to ptr
     string literal falls under both container literal and array type
     you can do both char [] = "hello", and const char ptr = "hello"
     Ex:
     string literals are array literals
     variables defined as : int32 [] are array literals
     initializer lists are container literals
     {'a', 'b'} is a container literal. string literals fall under the array literal section
     since they are placed in the data section of the bytecode file, so have space allocated for them
    */
    CONTAINER_LITERAL,

    // special tokens that are not directly parsed
    THIS,
    INDEXED_ASSIGNMENT,
};

struct Token {
    private:
    uint64_t token = 0;
    public:
    Token() = default;
    Token(uint32_t pos, uint16_t len, TokenType t) {
        token = ((uint64_t)t << 8) | ((uint64_t)len << 16) | ((uint64_t)pos << 32);
    }
    bool operator==(const Token&) const;
    inline TokenType getType() const { return (TokenType)((token & 0xff00) >> 8); };
    inline void setType(TokenType type) { token = (token & ~0xff00ul) | ((uint64_t)type << 8); };
    inline uint16_t getLength() const { return (uint16_t)((token & 0xffff0000) >> 16); };
    inline void setLength(uint16_t length) { token = (token & ~0xffff0000ul) | ((uint64_t)length << 16); };
    inline uint32_t getPosition() const { return (uint32_t)((token & 0xffffffff00000000) >> 32); };
    inline void setPosition(uint32_t pos) { token = (token & ~0xffffffff00000000) | ((uint64_t)pos << 32); };
};


bool isBuiltInType(TokenType);
bool isStrictType(TokenType);
bool isIndirectionType(TokenType);
bool isConcreteBuiltInType(TokenType);
bool isTypeQualifier(TokenType);
bool isConcreteType(TokenType);
bool isBinaryOp(TokenType);
bool isUnaryOp(TokenType);
bool isControlFlow(TokenType);
bool isLiteral(TokenType);
bool isLogicalOp(TokenType);
bool isAssignment(TokenType);
bool isFloatingPoint(TokenType);
bool isIntegral(TokenType);
bool isUnsigned(TokenType);
bool isSigned(TokenType);

/**
 * Maps single character to (potential) token type
*/
extern const TokenType charToType [128];

/**
 * Maps token type to it's string representation as it would look in source
*/
extern const std::unordered_map<TokenType, std::string> typeToString;

/**
 * print function for a token type
*/
std::ostream& operator<<(std::ostream& os, const TokenType& obj);

/**
 * Maps token type to a string representation
*/
extern const char * tokenTypeToTypeString [];
