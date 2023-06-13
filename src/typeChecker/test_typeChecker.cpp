#include <catch2/catch_test_macros.hpp>
#include "typeChecker.hpp"
#include "../parser/parser.hpp"

NodeMemPool mem3;

TEST_CASE("scanTopLevel", "[typeChecker]") {
  const std::string str = "func funcName(): int {other: char; }var : int;struct thing {  var : int;}";
  Tokenizer tk{"./src/typeChecker/test_typeChecker.cpp", str};
  Parser pr{tk, mem3};
  REQUIRE(pr.parse());
  REQUIRE(pr.expected.empty());
  REQUIRE(pr.unexpected.empty());
  TypeChecker tc{pr.program, tk};
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
