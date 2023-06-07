#include "nodes.hpp"

bool hasData(StatementType type) {
  return type >= StatementType::VALUE && type <= StatementType::WRAPPED_VALUE;
}

Expected::Expected(ExpectedType type, uint32_t position):
  position{position}, expectedType{type}, tokenType{TokenType::NOTHING} {}

Expected::Expected(ExpectedType type, uint32_t position, TokenType tokenType):
  position{position}, expectedType{type}, tokenType{tokenType} {}


bool Type::operator==(const Type& tk) const {
  return tk.tokens == tokens;
}

VariableDec::VariableDec(Token token): name{token} {}

bool VariableDec::operator==(const VariableDec& varDec) const {
  return name == varDec.name && type == varDec.type;
}

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
    case StatementType::WRAPPED_VALUE:
      new (&wrapped) std::unique_ptr<Statement>{std::move(st.wrapped)}; break;
    case StatementType::VALUE:
      var = st.var; break;
    default:
      break;
  }
  st.type = StatementType::NONE;
}

bool Statement::operator==(const Statement& st) const {
  if (type != st.type) {
    return false;
  }
  switch (type) {
    case StatementType::UNARY_OP:
      if (!unOp && !st.unOp) {
        return true;
      }
      if (!unOp || !st.unOp) {
        return false;
      }
      return *unOp == *st.unOp;
    case StatementType::BINARY_OP:
      if (!binOp && !st.binOp) {
        return true;
      }
      if (!binOp || !st.binOp) {
        return false;
      }
      return *binOp == *st.binOp;
    case StatementType::VARIABLE_DEC:
      if (!varDec && !st.varDec) {
        return true;
      }
      if (!varDec || !st.varDec) {
        return false;
      }
      return *varDec == *st.varDec;
    case StatementType::FUNCTION_CALL:
      if (!funcCall && !st.funcCall) {
        return true;
      }
      if (!funcCall || !st.funcCall) {
        return false;
      }
      return *funcCall == *st.funcCall;
    case StatementType::ARRAY_ACCESS:
      if (!arrAccess && !st.arrAccess) {
        return true;
      }
      if (!arrAccess || !st.arrAccess) {
        return false;
      }
      return *arrAccess == *st.arrAccess;
    case StatementType::WRAPPED_VALUE:
      if (!wrapped && !st.wrapped) {
        return true;
      }
      if (!wrapped || !st.wrapped) {
        return false;
      }
      return *wrapped == *st.wrapped;
    case StatementType::VALUE:
      return var == st.var;
    default:
      return true;
  }
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

Statement::Statement(std::unique_ptr<Statement> ptr) {
  new (&wrapped) std::unique_ptr<Statement>{std::move(ptr)};
  type = StatementType::WRAPPED_VALUE;
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
    case StatementType::WRAPPED_VALUE: wrapped.~unique_ptr<Statement>(); break;
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

ExpectedType Statement::isValid() const {
  switch (type) {
    case StatementType::UNARY_OP:
      if (unOp->operand == nullptr) {
        return ExpectedType::EXPRESSION;
      }
    case StatementType::BINARY_OP:
      if (binOp->rightSide == nullptr) {
        return ExpectedType::EXPRESSION;
      }
    default:
      break;
  }
  return ExpectedType::NOTHING;
}

bool Arguments::operator==(const Arguments& args) const {
  return args.list == list;
}

ArrayAccess::ArrayAccess(Token token): array{token} {}

bool ArrayAccess::operator==(const ArrayAccess& arrAcc) const {
  return array == arrAcc.array && offset == arrAcc.offset;
}

BinOp::BinOp(TokenType op): op{op} {}

BinOp::BinOp(BinOp&& binOp) noexcept : op{binOp.op} {
  leftSide = std::move(binOp.leftSide);
  rightSide = std::move(binOp.rightSide);
}

bool BinOp::operator==(const BinOp& bo) const {
  if (op != bo.op) {
    return false;
  }
  // short circuit right side
  bool r = false;
  if (rightSide || bo.rightSide) {
    if (!rightSide || !bo.rightSide) {
      return false;
    }
    r = true;
  }
  if (leftSide && bo.leftSide) {
    if (!(leftSide == bo.leftSide)) {
      return false;
    }
  } else if (leftSide || bo.leftSide) {
    return false;
  }
  if (r) {
    if (rightSide == bo.rightSide) {
      return false;
    }
  }
  return true;
}

UnOp::UnOp(TokenType op): op{op} {}

bool UnOp::operator==(const UnOp& uo) const {
  if (operand && uo.operand) {
    if (!(operand == uo.operand)) {
      return false;
    }
  } else if (operand || uo.operand) {
    return false;
  }
  return true;
}

FunctionDec::FunctionDec(Token token): name{token} {}

FunctionCall::FunctionCall(Token token): name{token} {}

bool FunctionCall::operator==(const FunctionCall& fc) const {
  return name == fc.name && args == fc.args;
}

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
