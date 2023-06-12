#include "nodes.hpp"
#include "tokenizer/tokenizer.hpp"

const uint8_t indentationSize = 2;

void Type::prettyPrint(Tokenizer& tk, std::string& str) {
  if (tokens.curr.type == TokenType::NOTHING) {
    return;
  }
  TokenList * iter = &tokens;
  for (; iter->next; iter = iter->next) {
    if (iter->curr.type != TokenType::POINTER) {
      str += tk.extractToken(iter->curr) + " ";
    } else {
      str += typeToString.at(TokenType::POINTER);
    }
  }
  str += tk.extractToken(iter->curr);
}

void VariableDec::prettyPrint(Tokenizer& tk, std::string& str) {
  str += tk.extractToken(name) + ": ";
  type.prettyPrint(tk, str);
}

void Statement::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  if (!unOp) {
    return;
  }
  switch (type) {
    case StatementType::UNARY_OP:
      unOp->prettyPrint(tk, str, indentation); break;
    case StatementType::BINARY_OP:
      binOp->prettyPrint(tk, str, indentation); break;
    case StatementType::VARIABLE_DEC:
      varDec->prettyPrint(tk, str); break;
    case StatementType::FUNCTION_CALL:
      funcCall->prettyPrint(tk, str, indentation); break;
    case StatementType::ARRAY_ACCESS:
      arrAccess->prettyPrint(tk, str, indentation); break;
    case StatementType::WRAPPED_VALUE:
      str += '(';
      wrapped->prettyPrint(tk, str, indentation); 
      str += ')';
      break;
    case StatementType::SCOPE:
      scope->prettyPrint(tk, str, indentation);  break;
    case StatementType::ARRAY_OR_STRUCT_LITERAL:
      arrOrStructLiteral->prettyPrint(tk, str, indentation); break;
    case StatementType::FOR_LOOP_HEADER:
      list->prettyPrint(tk, str, indentation); break;
    case StatementType::KEY_W_BODY:
      keywBody->prettyPrint(tk, str, indentation); break;
    case StatementType::VALUE:
      str += tk.extractToken(var); break;
    default: break;
  }
}

void UnOp::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  if (op == TokenType::DECREMENT_POSTFIX || op == TokenType::INCREMENT_POSTFIX) {
    operand.prettyPrint(tk, str, indentation);
    str += typeToString.at(op);
  } else {
    str += typeToString.at(op);
    operand.prettyPrint(tk, str, indentation);
  }
}

void BinOp::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation ) {
  leftSide.prettyPrint(tk, str, indentation);
  str += typeToString.at(op);
  rightSide.prettyPrint(tk, str, indentation);
}

void FunctionCall::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += tk.extractToken(name);
  str += '(';
  if (args.curr.type != StatementType::NONE) {
    indentation += indentationSize;
    StatementList * iter = &args;
    for (; iter->next; iter = iter->next) {
      iter->curr.prettyPrint(tk, str, indentation);
      str += ", ";
    }
    iter->curr.prettyPrint(tk, str, indentation);
    indentation -= indentationSize;
  }
  str += ')';
}

void ArrayAccess::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += tk.extractToken(array);
  str += '[';
  offset.prettyPrint(tk, str, indentation);
  str += ']';
}

void Scope::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += "{\n";
  if (scopeStatements.curr.type != StatementType::NONE) {
    indentation += indentationSize;
    for (StatementList * iter = &scopeStatements; iter; iter = iter->next) {
      str += std::string(indentation, ' ');
      iter->curr.prettyPrint(tk, str, indentation);
      if (iter->curr.type != StatementType::SCOPE && !(iter->curr.type == StatementType::KEY_W_BODY && iter->curr.keywBody->keyword != TokenType::RETURN)) {
        str += ";\n";
      }
    }
    indentation -= indentationSize;
  }
  str += std::string(indentation, ' ');
  str += "}\n";
}

void ForLoopHeader::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += '(';
  if (list.curr.type != StatementType::NONE) {
    StatementList * iter = &list;
    for (; iter->next; iter = iter->next) {
      iter->curr.prettyPrint(tk, str, indentation);
      if (iter->curr.type != StatementType::NONE) {
        str += "; ";
      } else {
        str += ";";
      }
    }
    iter->curr.prettyPrint(tk, str, indentation);
  }
  str += ')';
}

void KeywordWithBody::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += typeToString.at(keyword);
  if (keyword == TokenType::RETURN) {
    if (header.type != StatementType::NONE) {
      str += ' ';
    }
  }
  header.prettyPrint(tk, str, indentation);
  if (body.type != StatementType::NONE) {
    str += ' ';
    body.prettyPrint(tk, str, indentation);
  }
}

void ArrOrStructLiteral::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += '[';
  if (list.curr.type != StatementType::NONE) {
    indentation += indentationSize;
    StatementList * iter = &list;
    for (; iter->next; iter = iter->next) {
      iter->curr.prettyPrint(tk, str, indentation);
      str += ", ";
    }
    iter->curr.prettyPrint(tk, str, indentation);
    indentation -= indentationSize;
  }
  str += ']';
}

void FunctionDec::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += typeToString.at(TokenType::FUNC);
  str += tk.extractToken(name) + '(';
  if (params.curr.type != StatementType::NONE) {
    indentation += indentationSize;
    StatementList * iter = &params;
    for (; iter->next; iter = iter->next) {
      iter->curr.prettyPrint(tk, str, indentation);
      str += ", ";
    }
    iter->curr.prettyPrint(tk, str, indentation);
    indentation -= indentationSize;
  }
  str += "): ";
  returnType.prettyPrint(tk, str);
  str += ' ';
  body.prettyPrint(tk, str, indentation);
}

void Enum::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += typeToString.at(TokenType::ENUM);
  str += tk.extractToken(name);
  indentation += indentationSize;
  str += "{\n";
  for (uint32_t i = 0; i < members.size() - 1; ++i) {
    str += std::string(indentation, ' ');
    str += tk.extractToken(members[i]);
    str += ",\n";
  }
  indentation -= indentationSize;
  str += std::string(indentation, ' ');
  str += "}\n" + std::string(indentation, ' ');
}

void Declaration::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  if (!func) {
    return;
  }
  switch (decType) {
    case DecType::FUNCTION:
      func->prettyPrint(tk, str, indentation); break;
    case DecType::STATEMENT:
      statement->prettyPrint(tk, str, indentation); break;
    case DecType::TEMPLATE:
      temp->prettyPrint(tk, str, indentation); break;
    case DecType::STRUCT:
      struc->prettyPrint(tk, str, indentation); break;
    case DecType::ENUM:
      enm->prettyPrint(tk, str, indentation); break;
    default: break;
  }
}

void Struct::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += typeToString.at(TokenType::STRUCT);
  str += tk.extractToken(name);
  indentation += indentationSize;
  str += " {\n";
  for (uint32_t i = 0; i < decs.size(); ++i) {
    str += std::string(indentation, ' ');
    decs[i].prettyPrint(tk, str, indentation);
    if (decs[i].decType == DecType::STATEMENT && !(decs[i].statement->type == StatementType::KEY_W_BODY && decs[i].statement->keywBody->keyword != TokenType::RETURN)) {
      str += ";\n";
    }
  }
  indentation -= indentationSize;
  str += std::string(indentation, ' ');
  str += "}\n" + std::string(indentation, ' ');
}

void Template::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) {
  str += typeToString.at(TokenType::TEMPLATE);
  str += '[';
  if (templateIdentifiers.curr.type != StatementType::NONE) {
    StatementList * iter = &templateIdentifiers;
    for (; iter->next; iter = iter->next) {
      iter->curr.prettyPrint(tk, str, indentation);
      str += ", ";
    }
    iter->curr.prettyPrint(tk, str, indentation);
  }
  str += "] ";
  dec.prettyPrint(tk, str, indentation);
}

void Program::prettyPrint(Tokenizer& tk, std::string& str) {
  for (uint32_t i = 0; i < decs.size(); ++i) {
    decs[i].prettyPrint(tk, str, 0);
    str += '\n';
  }
}
