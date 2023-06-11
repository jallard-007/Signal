#pragma once

#include "../parser/parser.hpp"

struct CodeGen {
  Parser &parser;
  Tokenizer& tokenizer;
  CodeGen(const Parser&, const Tokenizer&);
};