#include <catch2/catch_test_macros.hpp>

#include "interpreter/interpreter.hpp"
#include "parser/parser.hpp"
#include "codeGen/interpreter/codeGen.hpp"
#include "checker/checker.hpp"
#include "utils.hpp"
#include "testingMemPool.hpp"

#define currDir "src/integrationTests/"

TEST_CASE("addFunctionSignatureToVirtualStack", "[codeGen]") {
    SECTION("hello world") {
        std::vector<unsigned char> program;
        std::string file = currDir "helloWorld.py";
        REQUIRE(openAndReadFile(file, program));
        std::vector<Tokenizer> tokenizers; \
        Tokenizer& tokenizer = tokenizers.emplace_back(file, std::move(program)); \
        Parser parser{tokenizer, memPool}; \
        REQUIRE(parser.parse());
        Checker checker{parser.program, tokenizers, memPool}; \
        REQUIRE(checker.check());
        CodeGen codeGen{parser.program, tokenizers, checker.lookUp}; \
        codeGen.tk = &tokenizer;
        REQUIRE(codeGen.generate());
    }
}
