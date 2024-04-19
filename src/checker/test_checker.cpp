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
        tokenList.token = {7, 5, TokenType::IDENTIFIER};
        tc.tk = &tks.back();
        CHECK(tc.checkType(tokenList));
        tokenList.next = nullptr;
        TokenList nextType = tokenList;
        tokenList.next = &nextType;
        tokenList.token = {0, 0, TokenType::POINTER};
        CHECK(tc.checkType(tokenList));
        nextType.next = nullptr;
        TokenList nextNextType = tokenList;
        tokenList.next = &nextNextType;
        tokenList.token = {0, 0, TokenType::REFERENCE};
        CHECK(tc.checkType(tokenList));
        nextType.next = nullptr;
        TokenList nextNextNextType = tokenList;
        tokenList.next = &nextNextNextType;
        tokenList.token = {0, 0, TokenType::POINTER};
        CHECK_FALSE(tc.checkType(tokenList));
    }

    {
        TokenList notAType;
        notAType.token = {38, 7, TokenType::IDENTIFIER};
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
    tc.check(true);
    CHECK(tc.errors.empty());
}

#define SET_UP_GET_STRUCT_INFO_TEST(str) \
    std::vector<Tokenizer> tokenizers; \
    Tokenizer& tokenizer = tokenizers.emplace_back("./src/parser/test_parser.cpp", str); \
    Parser parser{tokenizer, memPool}; \
    Checker checker{parser.program, tokenizers, memPool}; \
    checker.tk = &tokenizer; \
    REQUIRE(parser.parse()); \
    REQUIRE(checker.check(true)); \
    const GeneralDec* genDec = checker.lookUp["StructName"]; \
    REQUIRE(genDec); \
    REQUIRE(genDec->structDec); \
    auto& structInfo = checker.getStructInfo(*genDec->structDec)

TEST_CASE("getStructInfo", "[checker]") {
    SECTION("one") {
        const std::string str = "struct StructName { x: uint32; }";
        SET_UP_GET_STRUCT_INFO_TEST(str);
        CHECK(structInfo.size == 4);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["x"].position == 0);
    }
    SECTION("two") {
        const std::string str = "struct StructName { y:bool; x: uint32; }";
        SET_UP_GET_STRUCT_INFO_TEST(str);
        CHECK(structInfo.size == 8);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["y"].position == 0);
        CHECK(structInfo.memberLookup["x"].position == 4);
    }
    SECTION("three") {
        const std::string str = "struct StructName { x: uint32; y:bool; }";
        SET_UP_GET_STRUCT_INFO_TEST(str);
        CHECK(structInfo.size == 8);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["y"].position == 4);
        CHECK(structInfo.memberLookup["x"].position == 0);
    }
    SECTION("four") {
        const std::string str = "struct StructName { y:bool; x: uint32; j:bool; }";
        SET_UP_GET_STRUCT_INFO_TEST(str);
        CHECK(structInfo.size == 12);
        CHECK(structInfo.alignTo == 4);
        CHECK(structInfo.memberLookup["y"].position == 0);
        CHECK(structInfo.memberLookup["x"].position == 4);
        CHECK(structInfo.memberLookup["j"].position == 8);
    }
    SECTION("five") {
        const std::string str = "struct Thing { y:bool; x: uint32; j:bool; } struct StructName { y:Thing; x: bool; j:uint32 ptr; }";
        SET_UP_GET_STRUCT_INFO_TEST(str);
        CHECK(structInfo.size == 24);
        CHECK(structInfo.alignTo == 8);
        CHECK(structInfo.memberLookup["y"].position == 0);
        CHECK(structInfo.memberLookup["x"].position == 12);
        CHECK(structInfo.memberLookup["j"].position == 16);
    }
}

#undef SET_UP_GET_STRUCT_INFO_TEST

#define SET_UP_VAR_DEC_TEST(str) \
    std::vector<Tokenizer> tks; \
    tks.emplace_back("./src/checker/test_checker.cpp", str); \
    Parser pr{tks.back(), memPool}; \
    Statement statement; \
    ParseStatementErrorType errorType = pr.parseStatement(statement); \
    REQUIRE(errorType == ParseStatementErrorType::NONE); \
    Checker tc{pr.program, tks, memPool}; \
    tc.tk = &tks.back(); \
    TokenList retType = BaseTypeListTypes::voidValue

#define SET_UP_TWO_VAR_DEC_TEST(str) \
    SET_UP_VAR_DEC_TEST(str); \
    tc.checkStatement(statement, retType, false, false); \
    REQUIRE(tc.errors.empty()); \
    errorType = pr.parseStatement(statement); \
    REQUIRE(errorType == ParseStatementErrorType::NONE)


TEST_CASE("var dec", "[checker]") {
    SECTION("1") {
        const std::string str = "var: int32; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
}

TEST_CASE("reference var", "[checker]") {
    SECTION("1") {
        const std::string str = "var: int32;  varRef: int32 ref = var; ";
        SET_UP_TWO_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("2") {
        const std::string str = "var: int32;  varRef: const int32 ref = var; ";
        SET_UP_TWO_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("3") {
        // qualifier dropped
        const std::string str = "var: const int32 = 0;  varRef: int32 ref = var; ";
        SET_UP_TWO_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.size() == 1);
    }
    SECTION("4") {
        const std::string str = "var: int32 [10];  varRef: int32 [10] ref = var; ";
        SET_UP_TWO_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("5") {
        const std::string str = "var: const char ptr;  varRef: const char ptr ref = var; ";
        SET_UP_TWO_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("6") {
        // missing const in ref type
        const std::string str = "var: const char ptr;  varRef: char ptr ref = var; ";
        SET_UP_TWO_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
    }
    SECTION("7") {
        // missing initializer
        const std::string str = " varRef: int32 ref; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::REFERENCE_VARIABLE_MISSING_INITIALIZER);
    }
    SECTION("8") {
        // missing const 
        const std::string str = " varRef: int32 ref = 0; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
    }
    SECTION("9") {
        const std::string str = " varRef: const int32 ref = 0; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
}

TEST_CASE("array types", "[checker]") {
    SECTION("1") {
        const std::string str = "var: int32 [10]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("size too small") {
        const std::string str = "var: int32 [0]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::INVALID_ARRAY_SIZE);
    }
    SECTION("size too large") {
        const std::string str = "var: int32 [0xffffffffff]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::INVALID_ARRAY_SIZE);
    }
    SECTION("missing size") {
        const std::string str = "var: int32 []; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::EXPECTED_SIZE);
    }
    SECTION("initial assignment 1") {
        const std::string str = "var: int32 [] = [1, 2]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.empty());
    }
    SECTION("initial assignment 2") {
        const std::string str = "var: int32 [1] = [1, 2]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.size() == 1);
    }
    SECTION("initial assignment 3") {
        const std::string str = "var: const char ptr [] = [\"hello\", \"yup\"]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("initial assignment 4") {
        const std::string str = "var: int32 [2][2]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.empty());
    }
    SECTION("initial assignment 5") {
        const std::string str = "var: int32 [2][2] = [[1, 2], [1, 2]]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        CHECK(tc.errors.empty());
    }
    SECTION("initial assignment 6") {
        const std::string str = "var: int32 [2][2] = [[], [1, 2]]; ";
        SET_UP_VAR_DEC_TEST(str);
        tc.checkStatement(statement, retType, false, false);
        REQUIRE(tc.errors.empty());
    }
}

TEST_CASE("cursed", "[checker]") {
    SECTION("1") {
        std::vector<Tokenizer> tks;
        const std::string str = "struct MyType { field: int32; } func main(): void { var: MyType; var.4; var.\"field\"; var.'a'; var.true; var.false; var.nullptr; var.MyType; } ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        const bool res = pr.parse();
        REQUIRE(res);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        tc.check(true);
        REQUIRE(tc.errors.size() == 7);
    }
    SECTION("2") {
        std::vector<Tokenizer> tks;
        const std::string str = "struct MyType { field: int32; } func main(): void { var: int32 [10]; var[\"hello\"]; var['a']; var[true]; var[false]; var[nullptr]; var[MyType]; } ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        REQUIRE(pr.parse());
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        tc.check(true);
        REQUIRE(tc.errors.size() == 6);
    }
    SECTION("2") {
        std::vector<Tokenizer> tks;
        const std::string str = " \"hello\" = \"wut\"; ";
        tks.emplace_back("./src/checker/test_checker.cpp", str);
        Parser pr{tks.back(), memPool};
        Statement statement;
        ParseStatementErrorType errorType = pr.parseStatement(statement);
        REQUIRE(errorType == ParseStatementErrorType::NONE);
        Checker tc{pr.program, tks, memPool};
        tc.tk = &tks.back();
        tc.checkStatement(statement, BaseTypeListTypes::voidValue, false, false);
        REQUIRE(tc.errors.size() == 1);
    }
}

#define SET_UP_DEFAULT_TEST(str) \
    std::vector<Tokenizer> tks; \
    tks.emplace_back("./src/checker/test_checker.cpp", str); \
    Parser pr{tks.back(), memPool}; \
    REQUIRE(pr.parse()); \
    Checker tc{pr.program, tks, memPool}; \
    tc.tk = &tks.back();


TEST_CASE("struct types", "[checker]") {
    SECTION("1") {
        const std::string str = "struct MyType { field: int32; } func main(): void { var: MyType; } ";
        SET_UP_DEFAULT_TEST(str);
        tc.check(true);
        CHECK(tc.errors.empty());
    }
    SECTION("2") {
        const std::string str = "struct MyType { field: int32; } func main(): void { var: MyType = []; } ";
        SET_UP_DEFAULT_TEST(str);
         tc.check(true);
        CHECK(tc.errors.empty());
    }
    SECTION("3") {
        const std::string str = "struct MyType { field: int32; } func main(): void { var: MyType = [field = 0]; } ";
        SET_UP_DEFAULT_TEST(str);
        tc.check(true);
        CHECK(tc.errors.empty());
    }
    SECTION("4") {
        const std::string str = "struct MyType { field: int32; } func main(): void { var: MyType = [field = 0, other = 0]; } ";
        SET_UP_DEFAULT_TEST(str);
        CHECK_FALSE(tc.check(true));
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::NO_SUCH_MEMBER_VARIABLE);
    }
    SECTION("5") {
        const std::string str = "struct MyType { field: int32; } func main(): void { var: MyType = [0]; } ";
        SET_UP_DEFAULT_TEST(str);
        tc.check(true);
        REQUIRE(tc.errors.size() == 1);
        CHECK(tc.errors.back().type == CheckerErrorType::EXPECTING_NAMED_INDEX);
    }
    SECTION("6") {
        const std::string str = "struct MyType { field: int32; var: double; } func main(): void { var: MyType = [var = 1.0]; } ";
        SET_UP_DEFAULT_TEST(str);
        tc.check(true);
        CHECK(tc.errors.empty());
    }
}


TokenList* makeTypeList(const std::span<const TokenType> types) {
    TokenList *base = memPool.makeTokenList();
    TokenList *curr = base, *prev = nullptr;
    for (TokenType type : types) {
        prev = curr;
        curr->token = {0, 0, type};
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
        CHECK(tc.checkAssignment(leftType, ResultingType{rightType, true}, false));
    }
    SECTION("2") {
        TokenList *leftType = makeTypeList({{TokenType::POINTER, TokenType::CHAR_TYPE}});
        TokenList *rightType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE, TokenType::CONST}});
        Program program;
        std::vector<Tokenizer> tks;
        Checker tc{program, tks, memPool};
        CHECK_FALSE(tc.checkAssignment(leftType, ResultingType{rightType, true}, false));
    }
    SECTION("3") {
        std::vector<Tokenizer> tks;
        Program program;
        Checker tc{program, tks, memPool};
        TokenList *leftType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE, TokenType::CONST}});
        TokenList *rightType = makeTypeList({{TokenType::CONTAINER_LITERAL, TokenType::CHAR_TYPE, TokenType::CONST}});
        rightType->token.setLength(10);
        const bool res = tc.checkAssignment(leftType, ResultingType{rightType, false}, false);
        CHECK(res);
    }
    SECTION("4") {
        std::vector<Tokenizer> tks;
        Program program;
        Checker tc{program, tks, memPool};
        TokenList *leftType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE}});
        TokenList *rightType = makeTypeList({{TokenType::CONTAINER_LITERAL, TokenType::CHAR_TYPE, TokenType::CONST}});
        rightType->token.setLength(10);
        const bool res = tc.checkAssignment(leftType, ResultingType{rightType, false}, false);
        CHECK(res);
    }
    SECTION("5") {
        // container literal is too big for explicitly defined size
        std::vector<Tokenizer> tks;
        Program program;
        Checker tc{program, tks, memPool};
        TokenList *leftType = makeTypeList({{TokenType::ARRAY_TYPE, TokenType::CHAR_TYPE}});
        leftType->token.setLength(5);
        TokenList *rightType = makeTypeList({{TokenType::CONTAINER_LITERAL, TokenType::CHAR_TYPE, TokenType::CONST}});
        rightType->token.setLength(10);
        const bool res = tc.checkAssignment(leftType, ResultingType{rightType, false}, false);
        CHECK_FALSE(res);
    }
}

#define SET_UP_CHECK_CONTAINER_TEST(str) \
    std::vector<Tokenizer> tks; \
    tks.emplace_back("./src/checker/test_checker.cpp", str); \
    Parser pr{tks.back(), memPool}; \
    Expression expression; \
    ParseExpressionErrorType errorType = pr.parseExpression(expression); \
    REQUIRE(errorType == ParseExpressionErrorType::NONE); \
    REQUIRE(pr.expected.empty()); \
    REQUIRE(pr.unexpected.empty()); \
    REQUIRE(expression.getType() == ExpressionType::CONTAINER_LITERAL); \
    Checker tc{pr.program, tks, memPool}; \
    tc.tk = &tks.back()

#define POST_EXPRESSION_CHECK_CONTAINER_TEST() \
    CHECK(!res.isLiteral); \
    CHECK(!res.isLValue); \
    Token token = res.value.type->token; \
    CHECK(token.getType() == TokenType::CONTAINER_LITERAL)

TEST_CASE("checkContainerLiteral", "[codeGen]") {
    SECTION("1") {
        const std::string str = "[]; ";
        SET_UP_CHECK_CONTAINER_TEST(str);
        ResultingType res = tc.checkExpression(expression);
        POST_EXPRESSION_CHECK_CONTAINER_TEST();
        CHECK(token.getLength() == 0);
        CHECK_FALSE(res.value.type->next);
    }
    SECTION("2") {
        const std::string str = "[1]; ";
        SET_UP_CHECK_CONTAINER_TEST(str);
        ResultingType res = tc.checkExpression(expression);
        POST_EXPRESSION_CHECK_CONTAINER_TEST();
        CHECK(token.getLength() == 1);
        REQUIRE(res.value.type->next);
        Token tokenNext = res.value.type->next->token;
        CHECK(tokenNext.getType() == TokenType::INT32_TYPE);
    }
    SECTION("3") {
        const std::string str = "['a', 'B']; ";
        SET_UP_CHECK_CONTAINER_TEST(str);
        ResultingType res = tc.checkExpression(expression);
        POST_EXPRESSION_CHECK_CONTAINER_TEST();
        CHECK(token.getLength() == 2);
        REQUIRE(res.value.type->next);
        Token tokenNext = res.value.type->next->token;
        CHECK(tokenNext.getType() == TokenType::CHAR_TYPE);
    }
    SECTION("4") {
        const std::string str = "['a', 2]; ";
        SET_UP_CHECK_CONTAINER_TEST(str);
        ResultingType res = tc.checkExpression(expression);
        POST_EXPRESSION_CHECK_CONTAINER_TEST();
        CHECK(token.getLength() == 2);
        REQUIRE(res.value.type->next);
        Token tokenNext = res.value.type->next->token;
        CHECK(tokenNext.getType() == TokenType::INT32_TYPE);
    }
    SECTION("5") {
        const std::string str = "['a', 2.0]; ";
        SET_UP_CHECK_CONTAINER_TEST(str);
        ResultingType res = tc.checkExpression(expression);
        Token token = res.value.type->token;
        REQUIRE(token.getType() == TokenType::BAD_VALUE);
    }
}
