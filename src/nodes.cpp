#include "nodes.hpp"
#include "nodeMemPool.hpp"

bool notFirstOfExpression(TokenType type) {
  return type != TokenType::IDENTIFIER &&
    !isBinaryOp(type) &&
    !isUnaryOp(type) &&
    !isLiteral(type) &&
    type != TokenType::OPEN_PAREN;
}

Expression::Expression(): binOp{nullptr} {}
Expression::Expression(const Expression& ref): binOp{ref.binOp} {}
Expression::Expression(Token tk): value{tk} {}
Expression& Expression::operator=(const Expression&ref) {
  binOp = ref.binOp;
  return *this;
}

Statement::Statement(): expression{}, type{StatementType::NONE} {}
Statement::Statement(const Statement& ref): expression{ref.expression}, type{ref.type} {}
Statement& Statement::operator=(const Statement& ref) {
  expression = ref.expression;
  type = ref.type;
  return *this;
}

TokenList::TokenList(const Token& tk): token{tk}, next{nullptr} {}
TokenList::TokenList(const Token& tk, TokenList* next): token{tk}, next{next} {}

VariableDec::VariableDec(const Token& tk): name{tk} {}


ArrayAccess::ArrayAccess(const Token& tk): array{tk} {}

BinOp::BinOp(const Token& token): op{token} {}

UnOp::UnOp(const Token& token): op{token} {}

FunctionCall::FunctionCall(const Token& tk): name{tk} {}

ReturnStatement::ReturnStatement(const Token& tk): token{tk} {}

ControlFlowStatement::ControlFlowStatement(): forLoop{}, type{ControlFlowStatementType::NONE} {}

FunctionDec::FunctionDec(const Token& token): name{token} {}

StructDec::StructDec(const Token& token): name{token} {}

StructDecList::StructDecList(): funcDec{} {}
StructDecList::StructDecList(const StructDecList& ref): next{ref.next}, type{ref.type} {
  if (type == StructDecType::VAR) {
    varDec = ref.varDec;
  } else if (type == StructDecType::FUNC) {
    funcDec = ref.funcDec;
  }
}
StructDecList& StructDecList::operator=(const StructDecList& other) {
  next = other.next;
  type = other.type;
  varDec = other.varDec;
  return *this;
}

EnumDec::EnumDec(const Token&tk): name{tk} {}

TemplateDec::TemplateDec(): funcDec{} {}

GeneralDec::GeneralDec(): tempDec{nullptr} {}

bool TokenList::operator==(const TokenList& ref) const {
  const TokenList* refCurr = &ref;
  const TokenList* thisCurr = this;
  while (refCurr && thisCurr) {
    if (refCurr->token.type != thisCurr->token.type) {
      return false;
    }
    refCurr = refCurr->next;
    thisCurr = thisCurr->next;
  }
  if (refCurr || thisCurr) {
    return false;
  }
  return true;
}


///////////////////////////////////////
//             deep copy             //
///////////////////////////////////////

GeneralDec* TemplateDec::deepCopy(NodeMemPool& mem, Token name) {
  GeneralDec *copy = mem.makeGeneralDec();
  if (isStruct) {
    copy->type = GeneralDecType::STRUCT;
    copy->structDec = structDec.deepCopy(mem);
    copy->structDec->name = name;
  } else {
    copy->type = GeneralDecType::FUNCTION;
    copy->funcDec = funcDec.deepCopy(mem);
    copy->funcDec->name = name;
  }
  return copy;
}

StructDec* StructDec::deepCopy(NodeMemPool& mem) {
  StructDec *copy = mem.makeStructDec();
  copy->name = name;
  copy->decs = decs.deepCopy(mem);
  return copy;
}

StructDecList StructDecList::deepCopy(NodeMemPool& mem) {
  StructDecList copy;
  StructDecList *copiedList = &copy;
  for (StructDecList *list = this; list; list = list->next, copiedList = copiedList->next) {
    copiedList->type = list->type;
    if (list->type == StructDecType::FUNC) {
      copiedList->funcDec = list->funcDec->deepCopy(mem);
    } else if (list->type == StructDecType::VAR) {
      copiedList->varDec = mem.makeVariableDec(VariableDec{list->varDec->name});
      *copiedList->varDec = list->varDec->deepCopy(mem);
    }
    copiedList->next = mem.makeStructDecList();
  }
  mem.release(copiedList);
  return copy;
}

FunctionDec* FunctionDec::deepCopy(NodeMemPool& mem) {
  FunctionDec *copy = mem.makeFunctionDec();
  copy->name = name;
  copy->params = params.deepCopy(mem);
  copy->body = body.deepCopy(mem);
  copy->returnType = returnType.deepCopy(mem);
  return copy;
}

Scope Scope::deepCopy(NodeMemPool& mem) {
  Scope copied;
  copied.scopeStatements = scopeStatements.deepCopy(mem);
  return copied;
}

StatementList StatementList::deepCopy(NodeMemPool& mem) {
  StatementList copy;
  StatementList *copiedList = &copy;
  for (StatementList *list = this; list; list = list->next, copiedList = copiedList->next) {
    copiedList->curr = list->curr.deepCopy(mem);
    copiedList->next = mem.makeStatementList();
  }
  mem.release(copiedList);
  return copy;
}

Statement Statement::deepCopy(NodeMemPool& mem) {
  Statement copy;
  copy.type = type;
  switch (type) {
    case StatementType::CONTROL_FLOW: copy.controlFlow = controlFlow->deepCopy(mem); break;
    case StatementType::EXPRESSION: copy.expression = mem.makeExpression(); *copy.expression = expression->deepCopy(mem); break;
    case StatementType::KEYWORD: copy.keyword = keyword; break;
    case StatementType::SCOPE: copy.scope = mem.makeScope(); *copy.scope = scope->deepCopy(mem); break;
    case StatementType::VARIABLE_DEC: copy.varDec = mem.makeVariableDec(VariableDec{varDec->name}); *copy.varDec = varDec->deepCopy(mem); break;
    case StatementType::NONE: break;
  }
  return copy;
}

ControlFlowStatement* ControlFlowStatement::deepCopy(NodeMemPool& mem) {
  ControlFlowStatement *copy = mem.makeControlFlowStatement();
  copy->type = type;
  switch (type) {
    case ControlFlowStatementType::CONDITIONAL_STATEMENT: copy->conditional = conditional->deepCopy(mem); break;
    case ControlFlowStatementType::EXIT_STATEMENT:
    case ControlFlowStatementType::RETURN_STATEMENT: copy->returnStatement = returnStatement->deepCopy(mem); break;
    case ControlFlowStatementType::FOR_LOOP: copy->forLoop = forLoop->deepCopy(mem); break;
    case ControlFlowStatementType::SWITCH_STATEMENT: copy->switchStatement = switchStatement->deepCopy(mem); break;
    case ControlFlowStatementType::WHILE_LOOP: copy->whileLoop = whileLoop->deepCopy(mem); break;
    case ControlFlowStatementType::NONE: break;
  }
  return copy;
}

ConditionalStatement* ConditionalStatement::deepCopy(NodeMemPool& mem) {
  ConditionalStatement *copy = mem.makeConditionalStatement();
  copy->ifStatement = ifStatement.deepCopy(mem);
  if (elifStatement) {
    copy->elifStatement = mem.makeElifStatementList();
    ElifStatementList *copyList = copy->elifStatement;
    for (ElifStatementList *list = elifStatement; list; list = list->next, copyList = copyList->next) {
      copyList->elif = list->elif.deepCopy(mem);
      copyList->next = mem.makeElifStatementList();
    }
    mem.release(copyList);
  }
  if (elseStatement) {
    copy->elseStatement = mem.makeScope();
    *copy->elseStatement = elseStatement->deepCopy(mem);
  }
  return copy;
}

ReturnStatement* ReturnStatement::deepCopy(NodeMemPool& mem) {
  ReturnStatement *copy = mem.makeReturnStatement();
  copy->token = token;
  copy->returnValue = returnValue.deepCopy(mem);
  return copy;
}

ForLoop* ForLoop::deepCopy(NodeMemPool& mem) {
  ForLoop *copy = mem.makeForLoop();
  copy->statement = statement.deepCopy(mem);
  copy->initialize = initialize.deepCopy(mem);
  copy->iteration = iteration.deepCopy(mem);
  return copy;
}

SwitchStatement* SwitchStatement::deepCopy(NodeMemPool& mem) {
  SwitchStatement *copy = mem.makeSwitchStatement();
  copy->body = body.deepCopy(mem);
  copy->switched = switched.deepCopy(mem);
  return copy;
}

SwitchScopeStatementList SwitchScopeStatementList::deepCopy(NodeMemPool& mem) {
  SwitchScopeStatementList copy;
  SwitchScopeStatementList *copyList = &copy;
  for (SwitchScopeStatementList *list = this; list; list = list->next, copyList = copyList->next) {
    *copyList = *list;
    copyList->next = mem.makeSwitchScopeStatementList();
  }
  mem.release(copyList);
  return copy;
}

Expression Expression::deepCopy(NodeMemPool& mem) {
  Expression copy;
  copy.setType(getType());
  switch (getType()) {
    case ExpressionType::ARRAY_ACCESS: copy.arrAccess = arrAccess->deepCopy(mem); break;
    // case ExpressionType::ARRAY_LITERAL:
    // case ExpressionType::STRUCT_LITERAL: copy.arrayOrStruct = arrayOrStruct->deepCopy(mem); break;
    case ExpressionType::BINARY_OP: copy.binOp = binOp->deepCopy(mem); break;
    case ExpressionType::FUNCTION_CALL: copy.funcCall = funcCall->deepCopy(mem); break;
    case ExpressionType::UNARY_OP: copy.unOp = unOp->deepCopy(mem); break;
    case ExpressionType::VALUE: copy.value = value; break;
    case ExpressionType::NONE: break;
  }
  return copy;
}

BranchStatement BranchStatement::deepCopy(NodeMemPool& mem) {
  BranchStatement copy;
  copy.condition = condition.deepCopy(mem);
  copy.body = body.deepCopy(mem);
  return copy;
}

VariableDec VariableDec::deepCopy(NodeMemPool& mem) {
  VariableDec copy{name};
  copy.type = type.deepCopy(mem);
  copy.initialAssignment = mem.makeExpression();
  *copy.initialAssignment = initialAssignment->deepCopy(mem);
  return copy;
}

TokenList TokenList::deepCopy(NodeMemPool& mem) {
  TokenList copy;
  TokenList *copyList = &copy;
  for (TokenList *list = this; list; list = list->next, copyList = copyList->next) {
    copyList->token = list->token;
    copyList->next = mem.makeTokenList();
  }
  mem.release(copyList);
  return copy;
}

WhileLoop* WhileLoop::deepCopy(NodeMemPool& mem) {
  WhileLoop* copy = mem.makeWhileLoop();
  copy->statement = statement.deepCopy(mem);
  return copy;
}

ArrayAccess* ArrayAccess::deepCopy(NodeMemPool& mem) {
  ArrayAccess* copy = mem.makeArrayAccess(ArrayAccess{array.getToken()});
  copy->offset = offset.deepCopy(mem);
  return copy;
}

FunctionCall* FunctionCall::deepCopy(NodeMemPool& mem) {
  FunctionCall* copy = mem.makeFunctionCall(FunctionCall{name});
  copy->args = args.deepCopy(mem);
  return copy;
}

ArrayOrStructLiteral* ArrayOrStructLiteral::deepCopy(NodeMemPool& mem) {
  ArrayOrStructLiteral* copy = mem.makeArrayOrStruct();
  copy->values = values.deepCopy(mem);
  return copy;
}

UnOp* UnOp::deepCopy(NodeMemPool& mem) {
  UnOp *copy = mem.makeUnOp(UnOp{op});
  copy->operand = operand.deepCopy(mem);
  return copy;
}

BinOp* BinOp::deepCopy(NodeMemPool& mem) {
  BinOp *copy = mem.makeBinOp(BinOp{op});
  copy->leftSide = leftSide.deepCopy(mem);
  copy->rightSide = rightSide.deepCopy(mem);
  return copy;
}

ExpressionList ExpressionList::deepCopy(NodeMemPool& mem) {
  ExpressionList copy;
  ExpressionList *copyList = &copy;
  for (ExpressionList *list = this; list; list = list->next, copyList = copyList->next) {
    copyList->curr = list->curr.deepCopy(mem);
    copyList->next = mem.makeExpressionList();
  }
  mem.release(copyList);
  return copy;
}
