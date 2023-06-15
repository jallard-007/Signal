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
    Type typeList;
    TokenList type;
    typeList.tokens.curr.length = 5;
    typeList.tokens.curr.position = 7;
    typeList.tokens.curr.type = TokenType::IDENTIFIER;
    CHECK(tc.checkType(typeList));
    TokenList nextType = std::move(typeList.tokens);
    typeList.tokens.next = &nextType;
    typeList.tokens.curr.type = TokenType::POINTER;
    CHECK(tc.checkType(typeList));
    TokenList nextNextType = std::move(typeList.tokens);
    typeList.tokens.next = &nextNextType;
    typeList.tokens.curr.type = TokenType::REFERENCE;
    CHECK(tc.checkType(typeList));
    TokenList nextNextNextType = std::move(typeList.tokens);
    typeList.tokens.next = &nextNextNextType;
    typeList.tokens.curr.type = TokenType::POINTER;
    CHECK_FALSE(tc.checkType(typeList));
  }

  {
    Type notAType;
    notAType.tokens.curr.length = 7;
    notAType.tokens.curr.position = 38;
    notAType.tokens.curr.type = TokenType::IDENTIFIER;
    CHECK_FALSE(tc.checkType(notAType));
  }
}
