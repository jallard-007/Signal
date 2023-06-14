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
  Checker tc{pr.program, tk};
  tc.scanTopLevel();
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
  Checker tc{pr.program, tk};
  tc.scanTopLevel();
  {
    Type isAType;
    isAType.tokens.curr.length = 5;
    isAType.tokens.curr.position = 7;
    isAType.tokens.curr.type = TokenType::IDENTIFIER;
    CHECK(tc.checkType(isAType));
    TokenList nextType;
    isAType.tokens.next = &nextType;
    nextType.curr.type = TokenType::POINTER;
    CHECK(tc.checkType(isAType));
    TokenList nextNextType;
    nextType.next = &nextNextType;
    nextNextType.curr.type = TokenType::REFERENCE;
    CHECK(tc.checkType(isAType));
    TokenList nextNextNextType;
    nextNextType.next = &nextNextNextType;
    nextNextNextType.curr.type = TokenType::POINTER;
    CHECK_FALSE(tc.checkType(isAType));
  }

  {
    Type notAType;
    notAType.tokens.curr.length = 7;
    notAType.tokens.curr.position = 38;
    notAType.tokens.curr.type = TokenType::IDENTIFIER;
    CHECK_FALSE(tc.checkType(notAType));
  }
}
