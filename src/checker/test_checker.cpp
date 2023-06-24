#include <catch2/catch_test_macros.hpp>
#include "checker.hpp"
#include "../parser/parser.hpp"

NodeMemPool mem3;

TEST_CASE("scanTopLevel", "[checker]") {
  const std::string str = "func funcName(): int {other: char; }var : int;struct thing {  var : int;}";
  Tokenizer tk{R"(./src/checker/test_checker.cpp)", str};
  Parser pr{tk, mem3};
  REQUIRE(pr.parse());
  REQUIRE(pr.expected.empty());
  REQUIRE(pr.unexpected.empty());
  Checker tc{pr.program, tk, mem3};
  tc.firstTopLevelScan();
  CHECK(tc.errors.empty());
  CHECK(tc.lookUp["funcName"]);
  CHECK(tc.lookUp["var"]);
  CHECK(tc.lookUp["thing"]);
  auto &r = tc.structsLookUp["thing"];
  CHECK(r.size() == 1);
  CHECK(r["var"]);
  CHECK_FALSE(tc.lookUp["other"]);
}

TEST_CASE("checkType", "[checker]") {
  const std::string str = "struct other { } struct thing { var : another; } func another(): int {} ";
  Tokenizer tk{R"(./src/checker/test_checker.cpp)", str};
  Parser pr{tk, mem3};
  REQUIRE(pr.parse());
  REQUIRE(pr.expected.empty());
  REQUIRE(pr.unexpected.empty());
  Checker tc{pr.program, tk, mem3};
  tc.firstTopLevelScan();
  {
    TokenList tokenList;
    tokenList.token.length = 5;
    tokenList.token.position = 7;
    tokenList.token.type = TokenType::IDENTIFIER;
    CHECK(tc.checkType(tokenList));
    TokenList nextType = tokenList;
    tokenList.next = &nextType;
    tokenList.token.type = TokenType::POINTER;
    CHECK(tc.checkType(tokenList));
    TokenList nextNextType = tokenList;
    tokenList.next = &nextNextType;
    tokenList.token.type = TokenType::REFERENCE;
    CHECK(tc.checkType(tokenList));
    TokenList nextNextNextType = tokenList;
    tokenList.next = &nextNextNextType;
    tokenList.token.type = TokenType::POINTER;
    CHECK_FALSE(tc.checkType(tokenList));
  }

  {
    TokenList notAType;
    notAType.token.length = 7;
    notAType.token.position = 38;
    notAType.token.type = TokenType::IDENTIFIER;
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
  size: uint64 = 0;
  position: uint64 = 0;
  data: char ptr = nullptr;
}

)";
  Tokenizer tk{R"(./src/checker/test_checker.cpp)", str};
  Parser pr{tk, mem3};
  pr.parse();
  REQUIRE(pr.expected.empty());
  REQUIRE(pr.unexpected.empty());
  Checker tc{pr.program, tk, mem3};
  tc.firstTopLevelScan();
  tc.secondTopLevelScan();
  CHECK(tc.errors.empty());
}


TEST_CASE("fullScan", "[checker]") {
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
  size: uint64 = 0;
  position: uint64 = 0;
  data: char ptr = nullptr;
}

)";
  Tokenizer tk{R"(./src/checker/test_checker.cpp)", str};
  Parser pr{tk, mem3};
  pr.parse();
  REQUIRE(pr.expected.empty());
  REQUIRE(pr.unexpected.empty());
  Checker tc{pr.program, tk, mem3};
  tc.firstTopLevelScan();
  tc.secondTopLevelScan();
  tc.fullScan();
  CHECK(tc.errors.empty());
}