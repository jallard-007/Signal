#include "nodes.hpp"

bool notFirstOfExpression(TokenType type) {
  return type != TokenType::IDENTIFIER &&
    !isBinaryOp(type) && !isUnaryOp(type) &&
    type != TokenType::DECIMAL_NUMBER &&
    type != TokenType::OPEN_PAREN &&
    type != TokenType::STRING_LITERAL &&
    type != TokenType::CHAR_LITERAL &&
    type != TokenType::BINARY_NUMBER &&
    type != TokenType::HEX_NUMBER;
}


Unexpected::Unexpected(const Token& tk): token{tk} {};
std::string Unexpected::getErrorMessage(Tokenizer&, const std::string&) {
  return "";
}

Expected::Expected(ExpectedType exType, const Token& tk): tokenWhereExpected{tk}, expectedTokenType{TokenType::NOTHING}, expectedType{exType} {}
Expected::Expected(ExpectedType exType, const Token& tk, TokenType tkType): tokenWhereExpected{tk}, expectedTokenType{tkType}, expectedType{exType} {}
std::string Expected::getErrorMessage(Tokenizer&, const std::string&) {
  return "";
}

Expression::Expression(): binOp{nullptr}, type{ExpressionType::NONE} {}
Expression::Expression(const Expression& ref): binOp{ref.binOp}, type{ref.type} {}
Expression::Expression(Token *tk): value{tk}, type{ExpressionType::VALUE} {}
void Expression::operator=(const Expression&ref) {
  binOp = ref.binOp;
  type = ref.type;
}

ExpressionList::ExpressionList(): curr{}, next{nullptr} {}

Statement::Statement(): expression{}, type{StatementType::NOTHING} {}
Statement::Statement(const Statement& ref): expression{ref.expression}, type{ref.type} {}
void Statement::operator=(const Statement& ref) {
  expression = ref.expression;
  type = ref.type;
}

TokenList::TokenList(const Token& tk): token{tk}, next{nullptr} {}
void TokenList::operator=(const TokenList& ref) {
  token = ref.token;
  next = ref.next;
}

VariableDec::VariableDec(const Token& tk): name{tk}, type{}, initialAssignment{nullptr} {}

void StatementList::operator=(const StatementList &ref) {
  curr = ref.curr;
  next = ref.next;
}

void Scope::operator=(const Scope &ref) {
  scopeStatements = ref.scopeStatements;
}

ArrayAccess::ArrayAccess(const Token& tk): array{tk}, offset{} {}

BinOp::BinOp(const Token& token): op{token}, leftSide{}, rightSide{} {}

UnOp::UnOp(const Token& token): op{token}, operand{} {};

FunctionCall::FunctionCall(const Token& tk): name{tk}, args{} {}

ReturnStatement::ReturnStatement(const Token& tk): token{tk}, returnValue{} {}

SwitchStatement::SwitchStatement(const Token& token): token{token}, switched{0,0,TokenType::NOTHING}, body{} {}

ForLoop::ForLoop(const ForLoop &ref): initialize{ref.initialize}, condition{ref.condition}, iteration{ref.iteration}, body{ref.body} {}

ControlFlowStatement::ControlFlowStatement(): forLoop{}, type{ControlFlowStatementType::NONE} {}
ControlFlowStatement::ControlFlowStatement(const ForLoop& val): forLoop{val}, type{ControlFlowStatementType::FOR_LOOP} {}
ControlFlowStatement::ControlFlowStatement(const WhileLoop& val): whileLoop{val}, type{ControlFlowStatementType::WHILE_LOOP} {}
ControlFlowStatement::ControlFlowStatement(const ConditionalStatement& val): conditional{val}, type{ControlFlowStatementType::CONDITIONAL_STATEMENT} {}
ControlFlowStatement::ControlFlowStatement(const ReturnStatement& val): returnStatement{val}, type{ControlFlowStatementType::RETURN_STATEMENT} {}
ControlFlowStatement::ControlFlowStatement(const SwitchStatement& val): switchStatement{val}, type{ControlFlowStatementType::SWITCH_STATEMENT} {}

FunctionDec::FunctionDec(const Token& token): name{token}, params{}, returnType{}, body{} {};
void FunctionDec::operator=(const FunctionDec &ref) {
  name = ref.name;
  params = ref.params;
  returnType = ref.returnType;
  body = ref.body;
}

StructDec::StructDec(const Token& token): token{token}, decs{} {}

StructDecList::StructDecList(): funcDec{}, next{nullptr}, type{StructDecType::NONE} {}
StructDecList::StructDecList(const StructDecList&ref): next{ref.next}, type{ref.type} {
  if (type == StructDecType::VAR) {
    varDec = ref.varDec;
  } else if (type == StructDecType::FUNC) {
    funcDec = ref.funcDec;
  }
}

EnumDec::EnumDec(const Token&tk): token{tk}, members{} {};

TemplateDec::TemplateDec(): funcDec{}, templateTypes{}, token{0,0,TokenType::NOTHING}, isStruct{false}  {};

GlobalDec::GlobalDec(): tempDec{} {}

