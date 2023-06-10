#include "nodes.hpp"

bool hasData(StatementType type) {
  return type >= StatementType::VALUE && type <= StatementType::WRAPPED_VALUE;
}

Unexpected::Unexpected(Token token, uint32_t line, uint32_t column):
  token{token}, line{line}, column{column} {}

Expected::Expected(ExpectedType type, uint32_t line, uint32_t column):
  line{line}, column{column}, tokenType{TokenType::NOTHING}, expectedType{type} {}

Expected::Expected(ExpectedType type, uint32_t line, uint32_t column, TokenType tokenType):
  line{line}, column{column}, tokenType{tokenType}, expectedType{type} {}

bool Type::operator==(const Type& tk) const {
  return tk.tokens == tokens;
}

VariableDec::VariableDec(Token token): name{token} {}

bool VariableDec::operator==(const VariableDec& varDec) const {
  return name == varDec.name && type == varDec.type;
}

Statement::Statement(): unOp{nullptr} {}

Statement::Statement(StatementType type): unOp{nullptr}, type{type} {}

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
    case StatementType::SCOPE:
      new (&scope) std::unique_ptr<Scope>{std::move(st.scope)}; break;
    case StatementType::LIST:
      new (&list) std::unique_ptr<List>{std::move(st.list)}; break;
    case StatementType::KEY_W_BODY:
      new (&keywBody) std::unique_ptr<KeywordWithBody>{std::move(st.keywBody)}; break;
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
    case StatementType::SCOPE:
      if (!scope && !st.scope) {
        return true;
      }
      if (!scope || !st.scope) {
        return false;
      }
      return *scope == *st.scope;
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

Statement::Statement(std::unique_ptr<Scope> ptr) {
  new (&scope) std::unique_ptr<Scope>{std::move(ptr)};
  type = StatementType::SCOPE;
}

Statement::Statement(std::unique_ptr<List> ptr) {
  new (&list) std::unique_ptr<List>{std::move(ptr)};
  type = StatementType::LIST;
}

Statement::Statement(std::unique_ptr<KeywordWithBody> ptr) {
  new (&keywBody) std::unique_ptr<KeywordWithBody>{std::move(ptr)};
  type = StatementType::KEY_W_BODY;
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
    case StatementType::SCOPE: scope.~unique_ptr<Scope>(); break;
    case StatementType::LIST: list.~unique_ptr<List>(); break;
    case StatementType::KEY_W_BODY: keywBody.~unique_ptr<KeywordWithBody>(); break;
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

ExpectedType Statement::addStatementToNode(Statement&& st) {
  switch (type) {
    case StatementType::UNARY_OP:
      if (unOp->operand) {
        return ExpectedType::TOKEN;
      }
      unOp->operand = std::make_unique<Statement>(std::move(st));
      return ExpectedType::NOTHING;
    case StatementType::BINARY_OP:
      if (binOp->rightSide) {
        return ExpectedType::TOKEN;
      }
      binOp->rightSide = std::make_unique<Statement>(std::move(st));
      return ExpectedType::NOTHING;

    case StatementType::KEY_W_BODY:
      if (keywBody->body) {
        return ExpectedType::TOKEN;
      }
      if (st.type == StatementType::WRAPPED_VALUE && keywBody->keyword != TokenType::ELSE && !keywBody->header) {
        keywBody->header = std::make_unique<Statement>(std::move(st));
        return ExpectedType::NOTHING;
      }
      else if (st.type == StatementType::SCOPE && keywBody->header) {
        keywBody->body = std::make_unique<Statement>(std::move(st));
        return ExpectedType::NOTHING;
      }
      return ExpectedType::TOKEN;

    default:
      return ExpectedType::TOKEN;
  }
}

ExpectedType Statement::isValid() const {
  switch (type) {
    case StatementType::UNARY_OP:
      if (unOp->operand == nullptr) {
        return ExpectedType::EXPRESSION;
      }
      break;
    case StatementType::BINARY_OP:
      if (binOp->rightSide == nullptr) {
        return ExpectedType::EXPRESSION;
      }
      break;
    default:
      break;
  }
  return ExpectedType::NOTHING;
}

KeywordWithBody::KeywordWithBody(TokenType type): keyword{type} {}

ArrayAccess::ArrayAccess(Token token): array{token} {}

bool ArrayAccess::operator==(const ArrayAccess& arrAcc) const {
  return array == arrAcc.array && offset == arrAcc.offset;
}

bool Scope::operator==(const Scope& sp) const {
  return sp.scopeStatements == scopeStatements;
}

BinOp::BinOp(TokenType op): op{op} {}

BinOp::BinOp(BinOp&& binOp) noexcept:
  leftSide{std::move(binOp.leftSide)}, rightSide{std::move(binOp.rightSide)}, op{binOp.op} {}

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

UnOp::UnOp(UnOp&& unOp) noexcept : operand{std::move(unOp.operand)} , op{unOp.op} {}

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

FunctionDec::FunctionDec(FunctionDec&& fd):
  params{std::move(fd.params)}, bodyStatements{std::move(fd.bodyStatements)},
  returnType{std::move(fd.returnType)}, name{fd.name} {}

FunctionCall::FunctionCall(Token token): name{token} {}

bool FunctionCall::operator==(const FunctionCall& fc) const {
  return name == fc.name && args == fc.args;
}

Struct::Struct(Token tok): name{tok} {}

Declaration::Declaration(): func{nullptr}, decType{DecType::NONE} {}

Declaration::Declaration(Declaration&& dec) noexcept : decType{dec.decType} {
  switch (dec.decType) {
    case DecType::FUNCTION:
      new (&func) std::unique_ptr<FunctionDec>{std::move(dec.func)};
      break;
    case DecType::STATEMENT:
      new (&statement) std::unique_ptr<Statement>{std::move(dec.statement)};
      break;
    case DecType::STRUCT:
      new (&struc) std::unique_ptr<Struct>{std::move(dec.struc)};
      break;
    case DecType::TEMPLATE:
      new (&temp) std::unique_ptr<Template>{std::move(dec.temp)};
      break;
    default:
      break;
  }
  dec.decType = DecType::NONE;
}

Declaration::Declaration(std::unique_ptr<FunctionDec> funcDec): decType{DecType::FUNCTION} {
  new (&func) std::unique_ptr<FunctionDec>{std::move(funcDec)};
}

Declaration::Declaration(std::unique_ptr<Statement> st): decType{DecType::STATEMENT} {
  new (&statement) std::unique_ptr<Statement>{std::move(st)};
}

Declaration::Declaration(std::unique_ptr<Template> tDec): decType{DecType::TEMPLATE} {
  new (&temp) std::unique_ptr<Template>{std::move(tDec)};
}

Declaration::Declaration(std::unique_ptr<Struct> sDec): decType{DecType::STRUCT} {
  new (&struc) std::unique_ptr<Struct>{std::move(sDec)};
}

Declaration::~Declaration() {
  switch(decType) {
    case DecType::FUNCTION: func.~unique_ptr<FunctionDec>(); break;
    case DecType::STATEMENT: statement.~unique_ptr<Statement>(); break;
    case DecType::TEMPLATE: temp.~unique_ptr<Template>(); break;
    case DecType::STRUCT: struc.~unique_ptr<Struct>(); break;
    default: break;
  }
}

Program::Program(Program&& prog) noexcept : name{std::move(prog.name)}, decs{std::move(prog.decs)} {}
