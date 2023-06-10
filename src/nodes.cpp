#include "nodes.hpp"
#include <iostream>

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

VariableDec::VariableDec(Token token): type{}, name{token} {}

bool VariableDec::operator==(const VariableDec& varDec) const {
  return name == varDec.name && type == varDec.type;
}

Statement::Statement(): unOp{nullptr}, type{StatementType::NONE} {}

Statement::Statement(StatementType type): unOp{nullptr}, type{type} {}

void Statement::operator=(Statement&& st) noexcept {
  type = st.type;
  switch (st.type) {
    case StatementType::UNARY_OP:
      unOp = st.unOp; st.unOp = nullptr; break;
    case StatementType::BINARY_OP:
      binOp = st.binOp; st.binOp = nullptr; break;
    case StatementType::VARIABLE_DEC:
      varDec = st.varDec; st.varDec = nullptr; break;
    case StatementType::FUNCTION_CALL:
      funcCall = st.funcCall; st.funcCall = nullptr; break;
    case StatementType::ARRAY_ACCESS:
      arrAccess = st.arrAccess; st.arrAccess = nullptr; break;
    case StatementType::WRAPPED_VALUE:
      wrapped = st.wrapped; st.wrapped = nullptr; break;
    case StatementType::SCOPE:
      scope = st.scope; st.scope = nullptr; break;
    case StatementType::LIST:
      list = st.list; st.list = nullptr; break;
    case StatementType::KEY_W_BODY:
      keywBody = st.keywBody; st.keywBody = nullptr; break;
    case StatementType::VALUE:
      var = st.var; break;
    default: break;
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

Statement::Statement(UnOp *ptr) {
  unOp = ptr;
  type = StatementType::UNARY_OP;
}

Statement::Statement(BinOp *ptr) {
  binOp = ptr;
  type = StatementType::BINARY_OP;
}

Statement::Statement(VariableDec *ptr) {
  varDec = ptr;
  type = StatementType::VARIABLE_DEC;
}

Statement::Statement(FunctionCall *ptr) {
  funcCall = ptr;
  type = StatementType::FUNCTION_CALL;
}

Statement::Statement(ArrayAccess *ptr) {
  arrAccess = ptr;
  type = StatementType::ARRAY_ACCESS;
}

Statement::Statement(Statement *ptr) {
  wrapped = ptr;
  type = StatementType::WRAPPED_VALUE;
}

Statement::Statement(Scope *ptr) {
  scope = ptr;
  type = StatementType::SCOPE;
}

Statement::Statement(List *ptr) {
  list = ptr;
  type = StatementType::LIST;
}

Statement::Statement(KeywordWithBody *ptr) {
  keywBody = ptr;
  type = StatementType::KEY_W_BODY;
}

Statement::Statement(Token tok): var{tok} {
  type = StatementType::VALUE;
}

Statement **Statement::getChild() {
  switch (type) {
    case StatementType::UNARY_OP:
      return &unOp->operand;
    case StatementType::BINARY_OP:
      return &binOp->rightSide;
    default:
      return nullptr;
  }
}

ExpectedType Statement::addStatementToNode(Statement *st) {
  switch (type) {
    case StatementType::UNARY_OP:
      if (unOp->operand) {
        return ExpectedType::TOKEN;
      }
      unOp->operand = st;
      return ExpectedType::NOTHING;

    case StatementType::BINARY_OP:
      if (binOp->rightSide) {
        return ExpectedType::TOKEN;
      }

      binOp->rightSide = st;
      return ExpectedType::NOTHING;

    case StatementType::KEY_W_BODY:
      if (keywBody->body) {
        return ExpectedType::TOKEN;
      }
      if (st->type == StatementType::WRAPPED_VALUE && keywBody->keyword != TokenType::ELSE && !keywBody->header) {
        keywBody->header = st;
        return ExpectedType::NOTHING;
      }
      else if (st->type == StatementType::SCOPE && keywBody->header) {
        keywBody->body = st;
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

KeywordWithBody::KeywordWithBody(TokenType type): body{}, header{}, keyword{type} {}

ArrayAccess::ArrayAccess(Token token): array{token} {}

bool ArrayAccess::operator==(const ArrayAccess& arrAcc) const {
  return array == arrAcc.array && offset == arrAcc.offset;
}

bool Scope::operator==(const Scope& sp) const {
  return sp.scopeStatements == scopeStatements;
}

BinOp::BinOp(TokenType op): leftSide{nullptr}, rightSide{nullptr}, op{op} {}

BinOp::BinOp(BinOp&& binOp) noexcept:
leftSide{binOp.leftSide}, rightSide{binOp.rightSide}, op{binOp.op} {
  binOp.leftSide = nullptr;
  binOp.rightSide = nullptr;
  binOp.op = TokenType::NOTHING;
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

UnOp::UnOp(UnOp&& unOp) noexcept : operand{unOp.operand} , op{unOp.op} {}

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

Enum::Enum(): name{0,0,TokenType::NOTHING} {}

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
      func = dec.func; dec.func = nullptr; break;
    case DecType::STATEMENT:
      statement = dec.statement; dec.statement = nullptr; break;
    case DecType::STRUCT:
      struc = dec.struc; dec.struc = nullptr; break;
    case DecType::TEMPLATE:
      temp = dec.temp; dec.temp = nullptr; break;
    case DecType::ENUM:
      enm = dec.enm; dec.enm = nullptr; break;
    default: break;
  }
  dec.decType = DecType::NONE;
}

Declaration::Declaration(FunctionDec *ptr): func{ptr}, decType{DecType::FUNCTION} {}

Declaration::Declaration(Statement *ptr): statement{ptr}, decType{DecType::STATEMENT} {}

Declaration::Declaration(Template *ptr): temp{ptr}, decType{DecType::TEMPLATE} {}

Declaration::Declaration(Struct *ptr): struc{ptr}, decType{DecType::STRUCT} {}

Declaration::Declaration(Enum *ptr): enm{ptr}, decType{DecType::ENUM} {}

Program::Program(Program&& prog) noexcept : name{std::move(prog.name)}, decs{std::move(prog.decs)} {}
