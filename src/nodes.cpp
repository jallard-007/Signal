#include "nodes.hpp"

Type::Type(TokenType tp): which{whichOne::builtIn}, tp{tp} {}
Type::Type(const std::string& str): which{whichOne::custom}, str{str} {}

Variable::Variable(const std::string& name): name{name} {}

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

FunctionDec::FunctionDec(const std::string& name): name{std::move(name)} {}

Declaration::Declaration(): type{DecType::NONE} {}

Declaration::Declaration(Declaration&& dec): type{dec.type} {
  new (&func) std::unique_ptr<FunctionDec>{std::move(dec.func)};
}

Declaration::Declaration(std::unique_ptr<FunctionDec> funcDec): type{DecType::FUNCTION} {
  new (&func) std::unique_ptr<FunctionDec>{std::move(funcDec)};
}

Declaration::~Declaration() {
  switch(type) {
    case DecType::FUNCTION: func.~unique_ptr<FunctionDec>(); break;
    default: break;
  }
}
