#include <catch2/catch_test_macros.hpp>
#include <span>
#include "checker.hpp"
#include "parser/parser.hpp"
#include "testingMemPool.hpp"

TEST_CASE("scanTopLevel", "[checker]") {
    const std::string str = "func funcName(): int {other: char; }var : int;struct thing {  var : int;}";
    std::vector<Tokenizer> tks;
    tks.emplace_back("./src/checker/test_checker.cpp", str);
    Parser pr{tks.back(), memPool};
    REQUIRE(pr.parse());
    REQUIRE(pr.expected.empty());
    REQUIRE(pr.unexpected.empty());
    Checker tc{pr.program, tks, memPool};
    tc.firstTopLevelScan();
    CHECK(tc.errors.empty());
    CHECK(tc.lookUp["funcName"]);
    CHECK(tc.lookUp["var"]);
    CHECK(tc.lookUp["thing"]);
    const GeneralDec* genDec = tc.lookUp["thing"];
    REQUIRE(genDec);
    StructInformation& structInfo = tc.structLookUp[genDec->structDec];
    CHECK(structInfo.memberLookup.size() == 1);
    CHECK(structInfo.memberLookup["var"].memberDec);
    CHECK_FALSE(structInfo.memberLookup["other"].memberDec);
}

TEST_CASE("checkType", "[checker]") {
    const std::string str = "struct other { } struct thing { var : another; } func another(): int {} ";
    std::vector<Tokenizer> tks;
    tks.emplace_back("./src/checker/test_checker.cpp", str);
    Parser pr{tks.back(), memPool};
    REQUIRE(pr.parse());
    REQUIRE(pr.expected.empty());
    REQUIRE(pr.unexpected.empty());
    Checker tc{pr.program, tks, memPool};
    tc.firstTopLevelScan();
    {
        TokenList tokenList;
        tokenList.exp.setToken({7, 5, TokenType::IDENTIFIER});
        tc.tk = &tks.back();
        CHECK(tc.checkType(tokenList));
        tokenList.next = nullptr;
        TokenList nextType = tokenList;
        tokenList.next = &nextType;
        tokenList.exp.setToken(TokenType::POINTER);
        CHECK(tc.checkType(tokenList));
        nextType.next = nullptr;
        TokenList nextNextType = tokenList;
        tokenList.next = &nextNextType;
        tokenList.exp.setToken(TokenType::REFERENCE);
        CHECK(tc.checkType(tokenList));
        nextType.next = nullptr;
        TokenList nextNextNextType = tokenList;
        tokenList.next = &nextNextNextType;
        tokenList.exp.setToken(TokenType::POINTER);
        CHECK_FALSE(tc.checkType(tokenList));
    }

    {
        TokenList notAType;
        notAType.exp.setToken({38, 7, TokenType::IDENTIFIER});
        CHECK_FALSE(tc.checkType(notAType));
    }
}

TEST_CASE("secondScan", "[checker]") {
    const std::string str = 
R"(

func main(): int32 {
    obj: customType;
    obj.size = 10;
    if (functionName(0, @customType)) {
        return 0;
    }
    return 1;
}

func functionName(param1: int32, param2: customType): bool {
    if (customType.size < param1) {
        return false;
    }
    return true;
}

struct customType {
    size: uint64;
    position: uint64;
    data: char ptr;
}

)";
    std::vector<Tokenizer> tks;
    tks.emplace_back("./src/checker/test_checker.cpp", str);
    Parser pr{tks.back(), memPool};
    pr.parse();
    REQUIRE(pr.expected.empty());
    REQUIRE(pr.unexpected.empty());
    Checker tc{pr.program, tks, memPool};
    tc.firstTopLevelScan();
    tc.secondTopLevelScan(true);
    REQUIRE(tc.errors.size() == 1);
    CHECK(tc.errors.back().type == CheckerErrorType::TYPE_TOO_LARGE_TO_BE_AN_ARGUMENT);
    CHECK(tks.back().extractToken(tc.errors.back().token) == "param2");
}


TEST_CASE("fullScan", "[checker]") {
    const std::string str = 
R"(

func main(): int32 {
    obj: customType;
    obj.size = 10;
    if (functionName(0, obj)) {
        return 0;
    }
    return 1;
}

func functionName(param1: int32, param2: customType ref): bool {
    return param2.size < param1;
}

struct customType {
    size: uint64;
    position: uint64;
    data: char ptr;
}

)";
    std::vector<Tokenizer> tks;
    tks.emplace_back("./src/checker/test_checker.cpp", str);
    Parser pr{tks.back(), memPool};
    pr.parse();
    REQUIRE(pr.expected.empty());
    REQUIRE(pr.unexpected.empty());
    Checker tc{pr.program, tks, memPool};
    tc.firstTopLevelScan();
    tc.secondTopLevelScan(true);
    tc.fullScan();
    CHECK(tc.errors.empty());
}


class TestFixture_GetStructInfo {
    public:
    StructInformation structInfo;
    void setUp(const std::string &str) {
        std::vector<Tokenizer> tokenizers;
        Tokenizer& tokenizer = tokenizers.emplace_back("./src/parser/test_parser.cpp", str);
        Parser parser{tokenizer, memPool};
        Checker checker{parser.program, tokenizers, memPool};
        checker.tk = &tokenizer;
        REQUIRE(parser.parse());
        REQUIRE(checker.check(true));
        GeneralDec* genDec = checker.lookUp["StructName"];
        REQUIRE(genDec);
        REQUIRE(genDec->structDec);
        structInfo = checker.getStructInfo(*genDec->structDec);
    }
};

TEST_CASE_METHOD(TestFixture_GetStructInfo, "getStructInfo", "[codeGen]") {
    SECTION("one") {
        const std::string str = "struct StructName { x: uint32; }";
        setUp(str);
        CHECK(structInfo.size == 4);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["x"].position == 0);
    }
    SECTION("two") {
        const std::string str = "struct StructName { y:bool; x: uint32; }";
        setUp(str);
        CHECK(structInfo.size == 8);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["y"].position == 0);
        CHECK(structInfo.memberLookup["x"].position == 4);
    }
    SECTION("three") {
        const std::string str = "struct StructName { x: uint32; y:bool; }";
        setUp(str);
        CHECK(structInfo.size == 8);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["y"].position == 4);
        CHECK(structInfo.memberLookup["x"].position == 0);
    }
    SECTION("four") {
        const std::string str = "struct StructName { y:bool; x: uint32; j:bool; }";
        setUp(str);
        CHECK(structInfo.size == 12);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["y"].position == 0);
        CHECK(structInfo.memberLookup["x"].position == 4);
        CHECK(structInfo.memberLookup["j"].position == 8);
    }
    SECTION("five") {
        const std::string str = "struct Thing { y:bool; x: uint32; j:bool; } struct StructName { y:Thing; x: bool; j:uint32 ptr; }";
        setUp(str);
        CHECK(structInfo.size == 24);
        CHECK(structInfo.alignTo == 8);
        CHECK(structInfo.memberLookup["y"].position == 0);
        CHECK(structInfo.memberLookup["x"].position == 12);
        CHECK(structInfo.memberLookup["j"].position == 16);
    }
}

TEST_CASE("array types", "[checker]") {
    SECTION("1") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [10]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement; 
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("size too small") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [0]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement; 
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::INVALID_ARRAY_SIZE);
    }
    SECTION("size too small again") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [-1]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement; 
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::INVALID_ARRAY_SIZE);
    }
    SECTION("size too large") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [0xffffffffff]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement; 
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::INVALID_ARRAY_SIZE);
    }
    SECTION("missing size") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 []; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::EXPECTED_SIZE);
    }
    SECTION("initial assignment 1") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [] = [1, 2]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.empty());
    }
    SECTION("initial assignment 2") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [1] = [1, 2]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
    }
    SECTION("initial assignment 3") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: const char ptr [] = [\"hello\", \"yup\"]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("initial assignment 4") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [2][2]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.empty());
    }
    SECTION("initial assignment 5") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [2][2] = [[1, 2], [1, 2]]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("initial assignment 6") {
        std::vector<Tokenizer> tks;
        const std::string str = "var: int32 [2][2] = [[], [1, 2]]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        TokenList retType = TokenListTypes::voidValue;
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.empty());
    }
}

TokenList* makeTypeList(const std::span<const TokenType> types) {
    TokenList *base = memPool.makeTokenList();
    TokenList *curr = base, *prev = nullptr;
    for (TokenType type : types) {
        prev = curr;
        curr->exp.setToken(type);
        curr->next = memPool.makeTokenList();
        curr = curr->next;
    }
    prev->next = nullptr;
    memPool.release(curr);
    return base;
}

TEST_CASE("checkAssignment", "[codeGen]") {
    SECTION("1") {
        TokenList *leftType = makeTypeList({{TokenType::POINTER, TokenType::CHAR_TYPE, TokenType::CONST}});
        TokenList *rightType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE, TokenType::CONST}});
        Program program;
        std::vector<Tokenizer> tks;
        Checker tc{program, tks, memPool};
        CHECK(tc.checkAssignment(leftType, rightType, false));
    }
    SECTION("2") {
        TokenList *leftType = makeTypeList({{TokenType::POINTER, TokenType::CHAR_TYPE}});
        TokenList *rightType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE, TokenType::CONST}});
        Program program;
        std::vector<Tokenizer> tks;
        Checker tc{program, tks, memPool};
        CHECK_FALSE(tc.checkAssignment(leftType, rightType, false));
    }
    SECTION("3") {
        std::vector<Tokenizer> tks;
        Program program;
        Checker tc{program, tks, memPool};
        TokenList *leftType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE, TokenType::CONST}});
        TokenList *rightType = makeTypeList({{TokenType::CONTAINER_LITERAL, TokenType::CHAR_TYPE, TokenType::CONST}});
        rightType->exp.getTokenRef().setLength(10);
        const bool res = tc.checkAssignment(leftType, rightType, false);
        CHECK(res);
    }
    SECTION("4") {
        std::vector<Tokenizer> tks;
        Program program;
        Checker tc{program, tks, memPool};
        TokenList *leftType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE}});
        TokenList *rightType = makeTypeList({{TokenType::CONTAINER_LITERAL, TokenType::CHAR_TYPE, TokenType::CONST}});
        rightType->exp.getTokenRef().setLength(10);
        const bool res = tc.checkAssignment(leftType, rightType, false);
        CHECK(res);
    }
    SECTION("5") {
        // container literal is too big for explicitly defined size
        std::vector<Tokenizer> tks;
        Program program;
        Checker tc{program, tks, memPool};
        TokenList *leftType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE}});
        leftType->exp.getTokenRef().setLength(5);
        TokenList *rightType = makeTypeList({{TokenType::CONTAINER_LITERAL, TokenType::CHAR_TYPE, TokenType::CONST}});
        rightType->exp.getTokenRef().setLength(10);
        const bool res = tc.checkAssignment(leftType, rightType, false);
        CHECK_FALSE(res);
    }
}

TEST_CASE("checkContainerLiteral", "[codeGen]") {
    SECTION("1") {
        std::vector<Tokenizer> tks;
        const std::string str = "[]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Expression expression;
        ParseExpressionErrorType errorType = pr.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(pr.expected.empty());
        REQUIRE(pr.unexpected.empty());
        REQUIRE(expression.getType() == ExpressionType::CONTAINER_LITERAL);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        ResultingType res = tc.checkExpression(expression);
        CHECK(res.isLiteral);
        CHECK(!res.isLValue);
        REQUIRE(res.value.type->exp.getType() == ExpressionType::VALUE);
        Token token = res.value.type->exp.getToken();
        CHECK(token.getType() == TokenType::CONTAINER_LITERAL);
        CHECK(token.getLength() == 0);
        CHECK_FALSE(res.value.type->next);
    }
    SECTION("2") {
        std::vector<Tokenizer> tks;
        const std::string str = "[1]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Expression expression;
        ParseExpressionErrorType errorType = pr.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(pr.expected.empty());
        REQUIRE(pr.unexpected.empty());
        REQUIRE(expression.getType() == ExpressionType::CONTAINER_LITERAL);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        ResultingType res = tc.checkExpression(expression);
        CHECK(res.isLiteral);
        CHECK(!res.isLValue);
        REQUIRE(res.value.type->exp.getType() == ExpressionType::VALUE);
        Token token = res.value.type->exp.getToken();
        CHECK(token.getType() == TokenType::CONTAINER_LITERAL);
        CHECK(token.getLength() == 1);
        REQUIRE(res.value.type->next);
        REQUIRE(res.value.type->next->exp.getType() == ExpressionType::VALUE);
        Token tokenNext = res.value.type->next->exp.getToken();
        CHECK(tokenNext.getType() == TokenType::INT32_TYPE);
    }
    SECTION("3") {
        std::vector<Tokenizer> tks;
        const std::string str = "['a', 'B']; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Expression expression;
        ParseExpressionErrorType errorType = pr.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(pr.expected.empty());
        REQUIRE(pr.unexpected.empty());
        REQUIRE(expression.getType() == ExpressionType::CONTAINER_LITERAL);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        ResultingType res = tc.checkExpression(expression);
        CHECK(res.isLiteral);
        CHECK(!res.isLValue);
        REQUIRE(res.value.type->exp.getType() == ExpressionType::VALUE);
        Token token = res.value.type->exp.getToken();
        CHECK(token.getType() == TokenType::CONTAINER_LITERAL);
        CHECK(token.getLength() == 2);
        REQUIRE(res.value.type->next);
        REQUIRE(res.value.type->next->exp.getType() == ExpressionType::VALUE);
        Token tokenNext = res.value.type->next->exp.getToken();
        CHECK(tokenNext.getType() == TokenType::CHAR_TYPE);
    }
    SECTION("4") {
        std::vector<Tokenizer> tks;
        const std::string str = "['a', 2]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Expression expression;
        ParseExpressionErrorType errorType = pr.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(pr.expected.empty());
        REQUIRE(pr.unexpected.empty());
        REQUIRE(expression.getType() == ExpressionType::CONTAINER_LITERAL);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        ResultingType res = tc.checkExpression(expression);
        CHECK(res.isLiteral);
        CHECK(!res.isLValue);
        REQUIRE(res.value.type->exp.getType() == ExpressionType::VALUE);
        Token token = res.value.type->exp.getToken();
        CHECK(token.getType() == TokenType::CONTAINER_LITERAL);
        CHECK(token.getLength() == 2);
        REQUIRE(res.value.type->next);
        REQUIRE(res.value.type->next->exp.getType() == ExpressionType::VALUE);
        Token tokenNext = res.value.type->next->exp.getToken();
        CHECK(tokenNext.getType() == TokenType::INT32_TYPE);
    }
    SECTION("5") {
        std::vector<Tokenizer> tks;
        const std::string str = "['a', 2.0]; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Expression expression;
        ParseExpressionErrorType errorType = pr.parseExpression(expression);
        REQUIRE(errorType == ParseExpressionErrorType::NONE);
        REQUIRE(pr.expected.empty());
        REQUIRE(pr.unexpected.empty());
        REQUIRE(expression.getType() == ExpressionType::CONTAINER_LITERAL);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        ResultingType res = tc.checkExpression(expression);
        REQUIRE(res.value.type->exp.getType() == ExpressionType::VALUE);
        Token token = res.value.type->exp.getToken();
        REQUIRE(token.getType() == TokenType::BAD_VALUE);
    }
}
