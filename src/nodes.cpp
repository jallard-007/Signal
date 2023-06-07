#include "nodes.hpp"

bool hasData(StatementType type) {
  return type >= StatementType::VALUE && type <= StatementType::ARRAY_ACCESS;
}

VariableDec::VariableDec(Token token): name{token} {}

Statement::Statement(): unOp{nullptr} {}

Statement::Statement(StatementType type): type{type}, unOp{nullptr} {}

void Statement::operator=(Statement&& st) noexcept {
  type = st.type;
  switch (st.type) {
    case StatementType::UNARY_OP:
      new (&unOp) std::unique_ptr<UnOp>{std::move(st.unOp)}; break;
    case StatementType::BINARY_OP:
      new (&binOp) std::unique_ptr<BinOp>{std::move(st.binOp)}; break;
    case StatementType::VARIABLE_DEC:
      new (&varDec) std::unique_ptr<VariableDec>{std::move(st.varDec)}; break;
    case StatementType::FUNCTION_CALL:
      new (&funcCall) std::unique_ptr<FunctionCall>{std::move(st.funcCall)}; break;
    case StatementType::ARRAY_ACCESS:
      new (&arrAccess) std::unique_ptr<ArrayAccess>{std::move(st.arrAccess)}; break;
    case StatementType::VALUE:
      var = st.var; break;
    default:
      break;
  }
  st.type = StatementType::NONE;
}

Statement::Statement(Statement&& st) noexcept {
  operator=(std::move(st));
}

Statement::Statement(std::unique_ptr<UnOp> ptr) {
  new (&unOp) std::unique_ptr<UnOp>{std::move(ptr)};
  type = StatementType::UNARY_OP;
}

Statement::Statement(std::unique_ptr<BinOp> ptr) {
  new (&binOp) std::unique_ptr<BinOp>{std::move(ptr)};
  type = StatementType::BINARY_OP;
}

Statement::Statement(std::unique_ptr<VariableDec> ptr) {
  new (&varDec) std::unique_ptr<VariableDec>{std::move(ptr)};
  type = StatementType::VARIABLE_DEC;
}

Statement::Statement(std::unique_ptr<FunctionCall> ptr) {
  new (&funcCall) std::unique_ptr<FunctionCall>{std::move(ptr)};
  type = StatementType::FUNCTION_CALL;
}

Statement::Statement(std::unique_ptr<ArrayAccess> ptr) {
  new (&arrAccess) std::unique_ptr<ArrayAccess>{std::move(ptr)};
  type = StatementType::ARRAY_ACCESS;
}

Statement::Statement(Token tok): var{tok} {
  type = StatementType::VALUE;
}

Statement::~Statement() {
  switch (type) {
    case StatementType::UNARY_OP: unOp.~unique_ptr<UnOp>(); break;
    case StatementType::BINARY_OP: binOp.~unique_ptr<BinOp>(); break;
    case StatementType::VARIABLE_DEC: varDec.~unique_ptr<VariableDec>(); break;
    case StatementType::FUNCTION_CALL: funcCall.~unique_ptr<FunctionCall>(); break;
    case StatementType::ARRAY_ACCESS: arrAccess.~unique_ptr<ArrayAccess>(); break;
    default: break;
  }
}

std::unique_ptr<Statement>* Statement::getChild() {
  switch (type) {
    case StatementType::UNARY_OP:
      return &unOp->operand;
    case StatementType::BINARY_OP:
      return &binOp->rightSide;
    default:
      return nullptr;
  }
}

Statement* Statement::addStatementToNode(Statement&& st) {
  switch (type) {
    case StatementType::UNARY_OP:
      if (unOp->operand) {
        return nullptr;
      }
      unOp->operand = std::make_unique<Statement>(std::move(st));
      return unOp->operand.get();
    case StatementType::BINARY_OP:
      if (binOp->rightSide) {
        return nullptr;
      }
      binOp->rightSide = std::make_unique<Statement>(std::move(st));
      return binOp->rightSide.get();
    default:
      return nullptr;
  }
}

ArrayAccess::ArrayAccess(Token token): array{token} {}

BinOp::BinOp(TokenType op): op{op} {}

BinOp::BinOp(BinOp&& binOp) noexcept : op{binOp.op} {
  leftSide = std::move(binOp.leftSide);
  rightSide = std::move(binOp.rightSide);
}


UnOp::UnOp(TokenType op): op{op} {}

FunctionDec::FunctionDec(Token token): name{token} {}

FunctionCall::FunctionCall(Token token): name{token} {}

Declaration::Declaration(): decType{DecType::NONE} {}

Declaration::Declaration(Declaration&& dec) noexcept : decType{dec.decType} {
  new (&func) std::unique_ptr<FunctionDec>{std::move(dec.func)};
}

Declaration::Declaration(std::unique_ptr<FunctionDec> funcDec): decType{DecType::FUNCTION} {
  new (&func) std::unique_ptr<FunctionDec>{std::move(funcDec)};
}

Declaration::~Declaration() {
  switch(decType) {
    case DecType::FUNCTION: func.~unique_ptr<FunctionDec>(); break;
    default: break;
  }
}

Program::Program(Program&& prog) noexcept : name{std::move(prog.name)}, decs{std::move(prog.decs)} {}
