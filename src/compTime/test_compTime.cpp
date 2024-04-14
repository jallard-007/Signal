#include <iostream>
#include "compTime.hpp"
#include "parser/parser.hpp"
#include "checker/checker.hpp"
#include "testingMemPool.hpp"

#include <catch2/catch_test_macros.hpp>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define bc bytecode_t

#define testBoilerPlate(str) \
    std::vector<Tokenizer> tokenizers; \
    Tokenizer& tokenizer = tokenizers.emplace_back("./src/parser/test_parser.cpp", str); \
    Parser parser{tokenizer, memPool}; \
    Checker checker{parser.program, tokenizers, memPool}; \
    checker.tk = &tokenizer; \

LiteralValue evaluate(Tokenizer& tk, const Expression& exp) {
    switch (exp.getType()) {
        case ExpressionType::BINARY_OP: {
            LiteralValue leftResult = evaluate(tk, exp.getBinOp()->leftSide);
            LiteralValue rightResult = evaluate(tk, exp.getBinOp()->rightSide);
            return evaluateBinOpImmExpression(exp.getBinOp()->op.getType(), leftResult, rightResult);
        }
        case ExpressionType::UNARY_OP: {
            LiteralValue operandValue = evaluate(tk, exp.getUnOp()->operand);
            return evaluateUnaryOpImmExpression(exp.getUnOp()->op.getType(), operandValue);
        }
        case ExpressionType::VALUE: {
            return loadLiteralValue(tk, exp.getToken());
        }
        default: {
            assert(false);
            exit(1);
        }
    }
}

TEST_CASE("constant expressions", "[compTime]") {
    // should return the actual value of the expression, and the correct type (smallest type that supports that value, min int32_t)
    SECTION("4 + 4") {
        #define EXPRESSION 4 + 4
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("max int32") {
        #define EXPRESSION INT32_MAX
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("min int32") {
        #define EXPRESSION INT32_MIN
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("max uint32") {
        #define EXPRESSION 4294967295
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::UINT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("negative max uint32") {
        #define EXPRESSION -4294967295U
        auto result = (uint32_t)EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::UNARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::UINT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("complex") {
        #define EXPRESSION 2.0 * 100 / 3
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::DOUBLE_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("overflow") {
        #define EXPRESSION 9223372036854775807 + 1
        auto result = (int64_t)((uint64_t)EXPRESSION); // doing cast to bypass compiler warning
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT64_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 1") {
        #define EXPRESSION 55 % 6
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 2") {
        #define EXPRESSION 8 * 5.3
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);

        CHECK(expRes.type->exp.getToken().getType() == TokenType::DOUBLE_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 3") {
        #define EXPRESSION 23984723 - 9234.2 * 3 || 0x823ff2
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::BOOL);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 4") {
        #define EXPRESSION 0b11111111111 ^ ((0xfffffff + (15 - 0b0101) / 17  + 7) - 2)
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT32_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 5") {
        #define EXPRESSION (false || 0x23423ac ^ 128) + 1843709551615/2
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT64_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 6") {
        #define EXPRESSION (18446551615-123876) + 2
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::INT64_TYPE);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 7") {
        #define EXPRESSION 5 | (0x3a3423 | 0b1) * 102 || 66
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::BOOL);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 8") {
        #define EXPRESSION 5 > 0.000078
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::BOOL);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 9") {
        #define EXPRESSION ((234532 & 234) < 0xB234) && !(34 - 3234.2 * 1.5 - 75)
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::BOOL);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
    SECTION("ben expression 10") {
        #define EXPRESSION 808 - 0b111010010100001110100111111 && true
        auto result = EXPRESSION;
        const std::string str = std::string(TOSTRING(EXPRESSION)) + ';';
        #undef EXPRESSION
        testBoilerPlate(str);
        Expression expression;
        ParseExpressionErrorType errorType = parser.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(expression.getType() == ExpressionType::BINARY_OP);
        LiteralValue expRes = evaluate(tokenizer, expression);
        CHECK(expRes.type->exp.getToken().getType() == TokenType::BOOL);
        CHECK(*(decltype(result) *)expRes.getData() == result);
    }
}
