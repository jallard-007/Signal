#include "nodes.hpp"

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
Expression::Expression(Expression&& ref): binOp{ref.binOp}, type{ref.type} {
  ref.type = ExpressionType::NONE;
  ref.binOp = nullptr;
}
void Expression::operator=(Expression&& ref) {
  binOp = ref.binOp;
  type = ref.type;
  ref.binOp = nullptr;
  ref.type = ExpressionType::NONE;
}
void Expression::swap(Expression &ref) {
  BinOp * temp = ref.binOp;
  ref.binOp = binOp;
  binOp = temp;
  ExpressionType temp2 = ref.type;
  ref.type = type;
  type = temp2;
}

ExpressionList::ExpressionList(): curr{}, next{nullptr} {}

Statement::Statement(): expression{}, type{StatementType::NOTHING} {}
Statement::Statement(Expression *val): expression{val}, type{StatementType::EXPRESSION} {}
Statement::Statement(ControlFlowStatement *val): controlFlow{val}, type{StatementType::CONTROL_FLOW} {}
Statement::Statement(Scope *val): scope{val}, type{StatementType::SCOPE} {}
Statement::Statement(VariableDec *val): varDec{val}, type{StatementType::VARIABLE_DEC} {}
Statement::Statement(const Statement& ref): expression{ref.expression}, type{ref.type} {}

TokenList::TokenList(): token{0,0,TokenType::NOTHING}, next{nullptr} {}
TokenList::TokenList(const Token& tk): token{tk}, next{nullptr} {}
TokenList::TokenList(const TokenList& ref): token{ref.token}, next{ref.next} {}

VariableDec::VariableDec(const Token& tk): name{tk}, type{}, initialAssignment{nullptr} {}

ArrayAccess::ArrayAccess(const Token& tk): array{tk}, offset{} {}

BinOp::BinOp(const Token& token): op{token}, leftSide{}, rightSide{} {}

UnOp::UnOp(const Token& token): op{token}, operand{} {};

FunctionCall::FunctionCall(const Token& tk): name{tk}, args{} {}

IfStatement::IfStatement(const Token& token):token{token}, condition{}, body{} {}

ElifStatementList::ElifStatementList(const Token& tk): elif{tk}, next{nullptr} {}

ConditionalStatement::ConditionalStatement(const Token& token): ifStatement{token}, elifStatement{nullptr}, elseStatement{nullptr} {}

ReturnStatement::ReturnStatement(const Token& tk): token{tk}, returnValue{} {}

SwitchStatement::SwitchStatement(const Token& token): token{token}, switched{0,0,TokenType::NOTHING}, body{} {}

WhileLoop::WhileLoop(const Token& token): token{token}, condition{}, body{} {}

ForLoop::ForLoop(const Token& token): token{token}, initialize{}, condition{}, iteration{}, body{}, isVarDec{false} {}

ForLoop::ForLoop(ForLoop&& ref): token{ref.token}, isVarDec{ref.isVarDec}, condition{std::move(ref.condition)}, iteration{std::move(ref.iteration)}, body{std::move(ref.body)} {
  if (isVarDec) {
    varDec = std::move(ref.varDec);
  } else {
    initialize = std::move(ref.initialize);
  }
}

ControlFlowStatement::ControlFlowStatement(): forLoop{Token{0,0,TokenType::NOTHING}}, type{ControlFlowStatementType::NONE} {}
ControlFlowStatement::ControlFlowStatement(ForLoop&& val): forLoop{std::move(val)}, type{ControlFlowStatementType::FOR_LOOP} {}
ControlFlowStatement::ControlFlowStatement(const WhileLoop& val): whileLoop{val}, type{ControlFlowStatementType::WHILE_LOOP} {}
ControlFlowStatement::ControlFlowStatement(const ConditionalStatement& val): conditional{val}, type{ControlFlowStatementType::CONDITIONAL_STATEMENT} {}
ControlFlowStatement::ControlFlowStatement(const ReturnStatement& val): returnStatement{val}, type{ControlFlowStatementType::RETURN_STATEMENT} {}
ControlFlowStatement::ControlFlowStatement(const SwitchStatement& val): switchStatement{val}, type{ControlFlowStatementType::SWITCH_STATEMENT} {}

FunctionDec::FunctionDec(const Token& token): name{token}, params{}, returnType{}, body{} {};

Initialization::Initialization(): arrOrStruct{}, isExpression{false} {};

VarDecList::VarDecList(const Token& tk): curr{tk}, next{nullptr} {}

StructDec::StructDec(const Token& token): token{token}, decs{} {}

StructDecList::StructDecList() {}
StructDecList::StructDecList(const StructDecList&ref): next{ref.next}, isVarDec{ref.isVarDec} {
  if (isVarDec) {
    varDec = ref.varDec;
  } else {
    funcDec = ref.funcDec;
  }
}

EnumDec::EnumDec(const Token&tk): token{tk}, members{} {};

IdentifierList::IdentifierList(): token{0,0,TokenType::NOTHING}, next{nullptr} {};

TemplateDec::TemplateDec(): token{0,0,TokenType::NOTHING}, templateTypes{}, structDec{Token{0,0,TokenType::NOTHING}} {};

TemplateCreation::TemplateCreation(const Token& tk): token{tk}, templateDec{nullptr}, templateTypes{}, identifier{0,0,TokenType::NOTHING} {}

GlobalDec::GlobalDec(): structDec{Token{0,0,TokenType::NOTHING}} {}

GlobalDecList::GlobalDecList(): curr{}, next{nullptr} {}
