#include <catch2/catch_test_macros.hpp>
#include "tokenizer.hpp"

TokenType firstToken(const std::string& str) {
    Tokenizer tokenizer{"./src/tokenizer/test_tokenizer.cpp", str};
    return tokenizer.tokenizeNext().getType();
}

TokenType tokenAtN(const std::string& str, uint32_t n) {
    Tokenizer tokenizer{"./src/tokenizer/test_tokenizer.cpp", str};
    for (uint32_t i = 0; i < n; ++i) {
        tokenizer.tokenizeNext();
    }
    return tokenizer.tokenizeNext().getType();
}

TEST_CASE("Unit Test - Special", "[tokenizer][tokenType]") {
    CHECK(firstToken("") == TokenType::END_OF_FILE);
}

TEST_CASE("Unit Test - Literals", "[tokenizer][tokenType]") {
    CHECK(firstToken("' '") == TokenType::CHAR_LITERAL);
    CHECK(firstToken(R"("\\")") == TokenType::STRING_LITERAL);
    CHECK(firstToken("0") == TokenType::DECIMAL_NUMBER);
    CHECK(firstToken("0.0") == TokenType::FLOAT_NUMBER);
    CHECK(firstToken("0.") == TokenType::DECIMAL_NUMBER);
    CHECK(firstToken("0b") == TokenType::BINARY_NUMBER);
    CHECK(firstToken("1b") == TokenType::DECIMAL_NUMBER);
    CHECK(firstToken("0x") == TokenType::HEX_NUMBER);
    CHECK(firstToken("1x") == TokenType::DECIMAL_NUMBER);
}

TEST_CASE("Unit Test - Keywords", "[tokenizer][tokenType]") {
    CHECK(firstToken("__builtin") == TokenType::BUILTIN);
    CHECK(firstToken("as") == TokenType::AS);
    CHECK(firstToken("break") == TokenType::BREAK);
    CHECK(firstToken("case") == TokenType::CASE);
    CHECK(firstToken("continue") == TokenType::CONTINUE);
    CHECK(firstToken("const") == TokenType::CONST);
    CHECK(firstToken("create") == TokenType::CREATE);
    CHECK(firstToken("default") == TokenType::DEFAULT);
    CHECK(firstToken("define") == TokenType::DEFINE);
    CHECK(firstToken("elif") == TokenType::ELIF);
    CHECK(firstToken("else") == TokenType::ELSE);
    CHECK(firstToken("enum") == TokenType::ENUM);
    CHECK(firstToken("false") == TokenType::FALSE);
    CHECK(firstToken("for") == TokenType::FOR);
    CHECK(firstToken("func") == TokenType::FUNC);
    CHECK(firstToken("function") == TokenType::IDENTIFIER);
    CHECK(firstToken("if") == TokenType::IF);
    CHECK(firstToken("ifelif") == TokenType::IDENTIFIER);
    CHECK(firstToken("nullptr") == TokenType::NULL_PTR);
    CHECK(firstToken("include") == TokenType::INCLUDE);
    CHECK(firstToken("return") == TokenType::RETURN);
    CHECK(firstToken("returns") == TokenType::IDENTIFIER);
    CHECK(firstToken("struct") == TokenType::STRUCT);
    CHECK(firstToken("stdin") == TokenType::STDIN);
    CHECK(firstToken("stderr") == TokenType::STDERR);
    CHECK(firstToken("stdout") == TokenType::STDOUT);
    CHECK(firstToken("switch") == TokenType::SWITCH);
    CHECK(firstToken("ptr") == TokenType::POINTER);
    CHECK(firstToken("template") == TokenType::TEMPLATE);
    CHECK(firstToken("templat ") == TokenType::IDENTIFIER);
    CHECK(firstToken("true") == TokenType::TRUE);
    CHECK(firstToken("while") == TokenType::WHILE);
}

TEST_CASE("Unit Test - General", "[tokenizer][tokenType]") {
    CHECK(firstToken("_") == TokenType::IDENTIFIER);
    CHECK(firstToken("(") == TokenType::OPEN_PAREN);
    CHECK(firstToken(")") == TokenType::CLOSE_PAREN);
    CHECK(firstToken("{") == TokenType::OPEN_BRACE);
    CHECK(firstToken("}") == TokenType::CLOSE_BRACE);
    CHECK(firstToken("[") == TokenType::OPEN_BRACKET);
    CHECK(firstToken("]") == TokenType::CLOSE_BRACKET);
    CHECK(firstToken("\\") == TokenType::BACK_SLASH);
    CHECK(firstToken(":") == TokenType::COLON);
    CHECK(firstToken(";") == TokenType::SEMICOLON);
    // CHECK(firstToken("?") == TokenType::TERNARY);
    CHECK(firstToken(",") == TokenType::COMMA);
    CHECK(firstToken(".") == TokenType::DOT);
    CHECK(firstToken("->") == TokenType::PTR_MEMBER_ACCESS);
}

TEST_CASE("Unit Test - Operations", "[tokenizer][tokenType]") {
    // UNARY
    CHECK(firstToken("@") == TokenType::ADDRESS_OF);
    CHECK(tokenAtN("(* ", 1) == TokenType::DEREFERENCE);
    CHECK(firstToken("++") == TokenType::INCREMENT_PREFIX);
    CHECK(firstToken("--") == TokenType::DECREMENT_PREFIX);
    CHECK(tokenAtN("identifier++", 1) == TokenType::INCREMENT_POSTFIX);
    CHECK(tokenAtN("identifier--", 1) == TokenType::DECREMENT_POSTFIX);
    CHECK(firstToken(" - ") == TokenType::NEGATIVE);
    CHECK(tokenAtN("return - ", 1) == TokenType::NEGATIVE);
    CHECK(firstToken("sizeof") == TokenType::SIZEOF);

    // BINARY
    CHECK(firstToken(" + ") == TokenType::ADDITION);
    CHECK(tokenAtN("id - ", 1) == TokenType::SUBTRACTION);
    CHECK(tokenAtN("10 - ", 1) == TokenType::SUBTRACTION);
    CHECK(tokenAtN("0b10 - ", 1) == TokenType::SUBTRACTION);
    CHECK(tokenAtN("0xFA - ", 1) == TokenType::SUBTRACTION);
    CHECK(tokenAtN("'0' - ", 1) == TokenType::SUBTRACTION);
    CHECK(tokenAtN("\"0\" - ", 1) == TokenType::SUBTRACTION);

    CHECK(firstToken("*") == TokenType::MULTIPLICATION);
    CHECK(firstToken("/") == TokenType::DIVISION);
    CHECK(firstToken("%") == TokenType::MODULO);
    CHECK(firstToken("^") == TokenType::BITWISE_XOR);
    CHECK(firstToken("&") == TokenType::BITWISE_AND);
    CHECK(firstToken("|") == TokenType::BITWISE_OR);
    CHECK(firstToken("<<") == TokenType::SHIFT_LEFT);
    CHECK(firstToken(">>") == TokenType::SHIFT_RIGHT);
}

TEST_CASE("Unit Test - Assignments", "[tokenizer][tokenType]") {
    CHECK(firstToken("=") == TokenType::ASSIGNMENT);
    CHECK(firstToken("+=") == TokenType::ADDITION_ASSIGNMENT);
    CHECK(firstToken("-=") == TokenType::SUBTRACTION_ASSIGNMENT);
    CHECK(firstToken("*=") == TokenType::MULTIPLICATION_ASSIGNMENT);
    CHECK(firstToken("/=") == TokenType::DIVISION_ASSIGNMENT);
    CHECK(firstToken("%=") == TokenType::MODULO_ASSIGNMENT);
    CHECK(firstToken("|=") == TokenType::BITWISE_OR_ASSIGNMENT);
    CHECK(firstToken("&=") == TokenType::BITWISE_AND_ASSIGNMENT);
    CHECK(firstToken("^=") == TokenType::BITWISE_XOR_ASSIGNMENT);
    CHECK(firstToken("<<=") == TokenType::SHIFT_LEFT_ASSIGNMENT);
    CHECK(firstToken(">>=") == TokenType::SHIFT_RIGHT_ASSIGNMENT);
}

TEST_CASE("Unit Test - Logical", "[tokenizer][tokenType]") {
    CHECK(firstToken("!") == TokenType::NOT);
    CHECK(firstToken("==") == TokenType::EQUAL);
    CHECK(firstToken("!=") == TokenType::NOT_EQUAL);
    CHECK(firstToken("&&") == TokenType::LOGICAL_AND);
    CHECK(firstToken("||") == TokenType::LOGICAL_OR);
    CHECK(firstToken("<") == TokenType::LESS_THAN);
    CHECK(firstToken("<=") == TokenType::LESS_THAN_EQUAL);
    CHECK(firstToken(">") == TokenType::GREATER_THAN);
    CHECK(firstToken(">=") == TokenType::GREATER_THAN_EQUAL);
}

TEST_CASE("Unit Test - Types", "[tokenizer][tokenType]") {
    CHECK(firstToken("bool") == TokenType::BOOL);
    CHECK(firstToken("char") == TokenType::CHAR_TYPE);
    CHECK(firstToken("int8") == TokenType::INT8_TYPE);
    CHECK(firstToken("uint8") == TokenType::UINT8_TYPE);
    CHECK(firstToken("int16") == TokenType::INT16_TYPE);
    CHECK(firstToken("uint16") == TokenType::UINT16_TYPE);
    CHECK(firstToken("int32") == TokenType::INT32_TYPE);
    CHECK(firstToken("uint32") == TokenType::UINT32_TYPE);
    CHECK(firstToken("int64") == TokenType::INT64_TYPE);
    CHECK(firstToken("uint64") == TokenType::UINT64_TYPE);
    CHECK(firstToken("double") == TokenType::DOUBLE_TYPE);
    CHECK(firstToken("file_t") == TokenType::FILE_TYPE);
    CHECK(firstToken("ref") == TokenType::REFERENCE);
    CHECK(firstToken("ptr") == TokenType::POINTER);
    CHECK(firstToken("void") == TokenType::VOID);
}

TEST_CASE("Unit Test - Token Extraction", "[tokenizer][tokenExtraction]") {
    std::vector<Token> tokens;
    {
        const std::string str = "func functionName(content:char ptr, size:int)\n# this is a comment\nnotAComment  ";
        Tokenizer tokenizer{"./src/tokenizer/test_tokenizer.cpp", str};
        tokens.clear();
        tokenizer.tokenizeAll(tokens);
        REQUIRE(tokens.size() == 14);
        CHECK(tokenizer.extractToken(tokens[0]) == "func");
        CHECK(tokenizer.extractToken(tokens[1]) == "functionName");
        CHECK(tokenizer.extractToken(tokens[3]) == "content");
        CHECK(tokenizer.extractToken(tokens[5]) == "char");
        CHECK(tokenizer.extractToken(tokens[8]) == "size");
        CHECK(tokenizer.extractToken(tokens[10]) == "int");
        CHECK(tokenizer.extractToken(tokens[12]) == "notAComment");
    }

    {
        const std::string str = "0xFFF 0b10101101 0xFABDECAAaaffbceda1010199747393";
        Tokenizer tokenizer{"./src/tokenizer/test_tokenizer.cpp", str};
        tokens.clear();
        tokenizer.tokenizeAll(tokens);
        REQUIRE(tokens.size() == 4);
        CHECK(tokenizer.extractToken(tokens[0]) == "0xFFF");
        CHECK(tokenizer.extractToken(tokens[1]) == "0b10101101");
        CHECK(tokenizer.extractToken(tokens[2]) == "0xFABDECAAaaffbceda1010199747393");
    }
}
