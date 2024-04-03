#include <catch2/catch_test_macros.hpp>
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
        tokenList.token.setLength(5);
        tokenList.token.setPosition(7);
        tokenList.token.setType(TokenType::IDENTIFIER);
        tc.tk = &tks.back();
        CHECK(tc.checkType(tokenList));
        tokenList.next = nullptr;
        TokenList nextType = tokenList;
        tokenList.next = &nextType;
        tokenList.token.setType(TokenType::POINTER);
        CHECK(tc.checkType(tokenList));
        nextType.next = nullptr;
        TokenList nextNextType = tokenList;
        tokenList.next = &nextNextType;
        tokenList.token.setType(TokenType::REFERENCE);
        CHECK(tc.checkType(tokenList));
        nextType.next = nullptr;
        TokenList nextNextNextType = tokenList;
        tokenList.next = &nextNextNextType;
        tokenList.token.setType(TokenType::POINTER);
        CHECK_FALSE(tc.checkType(tokenList));
    }

    {
        TokenList notAType;
        notAType.token.setLength(7);
        notAType.token.setPosition(38);
        notAType.token.setType(TokenType::IDENTIFIER);
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
    CHECK(tc.errors.empty());
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

func functionName(param1: int32, param2: customType): bool {
    if (param2.size < param1) {
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
        structInfo = checker.getStructInfo(*checker.lookUp["StructName"]);
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
