#include "nodes.hpp"

Constant::Constant(Token token): value{token} {}

Variable::Variable(Token token): name{token} {}

Statement::Statement(std::unique_ptr<Constant> ptr) {
  new (&var) std::unique_ptr<Constant>{std::move(ptr)};
  type = unionType::constant;
}

Statement::Statement(std::unique_ptr<UnOp> ptr) {
  new (&var) std::unique_ptr<UnOp>{std::move(ptr)};
  type = unionType::unOp;
}

Statement::Statement(std::unique_ptr<BinOp> ptr) {
  new (&var) std::unique_ptr<BinOp>{std::move(ptr)};
  type = unionType::binOp;
}

Statement::Statement(std::unique_ptr<Variable> ptr): var{} {
  new (&var) std::unique_ptr<Variable>{std::move(ptr)};
  type = unionType::var;
}

Statement::~Statement() {
  switch (type) {
    case unionType::constant: c.~unique_ptr<Constant>(); break;
    case unionType::unOp: uOp.~unique_ptr<UnOp>(); break;
    case unionType::binOp: bOp.~unique_ptr<BinOp>(); break;
    case unionType::var: var.~unique_ptr<Variable>(); break;
    default: break;
  }
}

FunctionDec::FunctionDec(Token token): name{token} {}

Declaration::Declaration(): decType{DecType::NONE} {}

Declaration::Declaration(Declaration&& dec): decType{dec.decType} {
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
