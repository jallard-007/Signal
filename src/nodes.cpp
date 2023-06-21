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


Unexpected::Unexpected(const Token& tk): token{tk} {}
std::string Unexpected::getErrorMessage(Tokenizer& tk, const std::string& file) {
  TokenPositionInfo posInfo = tk.getTokenPositionInfo(token);
  std::string message = file + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
  return message + "Unexpected Token: " + tk.extractToken(token) + "\n\n";
}

Expected::Expected(ExpectedType exType, const Token& tk): tokenWhereExpected{tk}, expectedTokenType{TokenType::NOTHING}, expectedType{exType} {}
Expected::Expected(ExpectedType exType, const Token& tk, TokenType tkType): tokenWhereExpected{tk}, expectedTokenType{tkType}, expectedType{exType} {}
std::string Expected::getErrorMessage(Tokenizer& tk, const std::string& file) {
  TokenPositionInfo posInfo = tk.getTokenPositionInfo(tokenWhereExpected);
  std::string message = file + ':' + std::to_string(posInfo.lineNum) + ':' + std::to_string(posInfo.linePos) + '\n';
  if (expectedType == ExpectedType::EXPRESSION) {
    return message + "Expected Expression\n\n";
  }
  if (expectedType == ExpectedType::TOKEN) {
    return message + "Expected Token: " + typeToString.at(expectedTokenType) + "\n\n";
  }
  return message + "\n\n";
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

VariableDec::VariableDec(const Token& tk): name{tk} {}

void StatementList::operator=(const StatementList &ref) {
  curr = ref.curr;
  next = ref.next;
}

void Scope::operator=(const Scope &ref) {
  scopeStatements = ref.scopeStatements;
}

ArrayAccess::ArrayAccess(const Token& tk): array{tk} {}

BinOp::BinOp(const Token& token): op{token} {}

UnOp::UnOp(const Token& token): op{token} {}

FunctionCall::FunctionCall(const Token& tk): name{tk} {}

ReturnStatement::ReturnStatement(const Token& tk): token{tk} {}

SwitchStatement::SwitchStatement(const Token& token): token{token} {}

ForLoop::ForLoop(const ForLoop &ref):  body{ref.body}, initialize{ref.initialize}, condition{ref.condition}, iteration{ref.iteration} {}

ControlFlowStatement::ControlFlowStatement(): forLoop{}, type{ControlFlowStatementType::NONE} {}
ControlFlowStatement::ControlFlowStatement(const ForLoop& val): forLoop{val}, type{ControlFlowStatementType::FOR_LOOP} {}
ControlFlowStatement::ControlFlowStatement(const WhileLoop& val): whileLoop{val}, type{ControlFlowStatementType::WHILE_LOOP} {}
ControlFlowStatement::ControlFlowStatement(const ConditionalStatement& val): conditional{val}, type{ControlFlowStatementType::CONDITIONAL_STATEMENT} {}
ControlFlowStatement::ControlFlowStatement(const ReturnStatement& val): returnStatement{val}, type{ControlFlowStatementType::RETURN_STATEMENT} {}
ControlFlowStatement::ControlFlowStatement(const SwitchStatement& val): switchStatement{val}, type{ControlFlowStatementType::SWITCH_STATEMENT} {}

FunctionDec::FunctionDec(const Token& token): name{token} {};
void FunctionDec::operator=(const FunctionDec &ref) {
  name = ref.name;
  params = ref.params;
  returnType = ref.returnType;
  body = ref.body;
}

StructDec::StructDec(const Token& token): name{token} {}

StructDecList::StructDecList(): funcDec{} {}
StructDecList::StructDecList(const StructDecList&ref): next{ref.next}, type{ref.type}, isValid{ref.isValid} {
  if (type == StructDecType::VAR) {
    varDec = ref.varDec;
  } else if (type == StructDecType::FUNC) {
    funcDec = ref.funcDec;
  }
}

EnumDec::EnumDec(const Token&tk): name{tk} {}

TemplateDec::TemplateDec(): funcDec{} {}

GeneralDec::GeneralDec(): tempDec{nullptr} {}

bool TokenList::operator==(const TokenList& ref) const {
  const TokenList* refCurr = &ref;
  const TokenList* thisCurr = this;
  while (refCurr->next && thisCurr->next) {
    if (!(refCurr->token == thisCurr->token)) {
      return false;
    }
    refCurr = refCurr->next;
    thisCurr = thisCurr->next;
  }
  if (refCurr->next || thisCurr->next) {
    return false;
  }
  return true;
}
