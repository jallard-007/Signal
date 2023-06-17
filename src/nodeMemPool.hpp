#pragma once

#include "nodes.hpp"
#include "memPool.hpp"

class NodeMemPool {
  MemPool<UnOp> unOps;
  MemPool<BinOp> binOps;
  MemPool<ControlFlowStatement> controlFlows;
  MemPool<GlobalDecList> decs;
  MemPool<Token> tokens;
  MemPool<TokenList> tokenLists;
  MemPool<VariableDec> varDecs;
  MemPool<Expression> expressions;
  MemPool<ExpressionList> expressionLists;
  MemPool<FunctionCall> funcCalls;
  MemPool<ArrayAccess> arrayAccesses;
  MemPool<Statement> statements;
  MemPool<Scope> scopes;
  MemPool<ArrayOrStructLiteral> arrOrStructs;
  MemPool<WhileLoop> whileLoops;
  MemPool<ForLoop> forLoops;
  MemPool<StatementList> statementList;
  MemPool<FunctionDec> functionDecs;
  MemPool<EnumDec> enums;
  MemPool<StructDec> structs;
  MemPool<TemplateDec> templates;

public:
  void reset() {
    unOps.reset();
    binOps.reset();
    decs.reset();
    tokens.reset();
    varDecs.reset();
    funcCalls.reset();
    arrayAccesses.reset();
    statements.reset();
    scopes.reset();
    arrOrStructs.reset();
    statementList.reset();
    functionDecs.reset();
    enums.reset();
    structs.reset();
    templates.reset();
  }

  UnOp* getUnOp(UnOp&& ref) {return unOps.get(std::move(ref));}
  BinOp* getBinOp(BinOp&& ref) {return binOps.get(std::move(ref));}
  GlobalDecList* getGlobalDec() {return decs.get();}
  VariableDec* getVariableDec(VariableDec&& ref) {return varDecs.get(std::move(ref));}
  FunctionCall* getFunctionCall(FunctionCall&& ref) {return funcCalls.get(std::move(ref));}
  ControlFlowStatement* getControlFlowStatement(ControlFlowStatement&& ref) {return controlFlows.get(std::move(ref));}
  ArrayAccess* getArrayAccess(ArrayAccess&& ref) {return arrayAccesses.get(std::move(ref));}
  Token* getToken(const Token& ref) {return tokens.get(ref);}
  TokenList* getTokenList() {return tokenLists.get();}
  Expression* getDefaultedExpression() {return expressions.get();}
  Expression* getExpression(Expression&& ref) {return expressions.get(std::move(ref));}
  ExpressionList* getExpressionList() {return expressionLists.get();}
  Statement* getStatement(Statement&& ref) {return statements.get(std::move(ref));}
  Scope* getScope(Scope&& ref) {return scopes.get(std::move(ref));}
  ArrayOrStructLiteral* getArrayOrStructLiteral(ArrayOrStructLiteral&& ref) {return arrOrStructs.get(std::move(ref));}
  StatementList* getStatementList() {return statementList.get();}
  FunctionDec* getFunctionDec(FunctionDec&& ref) {return functionDecs.get(std::move(ref));}
  EnumDec* getEnumDec(EnumDec&& ref) {return enums.get(std::move(ref));}
  StructDec* getStructDec(StructDec&& ref) {return structs.get(std::move(ref));}
  TemplateDec* getTemplateDec(TemplateDec&& ref) {return templates.get(std::move(ref));}

  void release(UnOp* ptr) { unOps.release(ptr);}
  void release(BinOp* ptr) { binOps.release(ptr);}
  void release(GlobalDecList* ptr) { decs.release(ptr);}
  void release(VariableDec* ptr) { varDecs.release(ptr);}
  void releaseExpression(Expression* ptr) { expressions.release(ptr);}
  void releaseExpressionList(ExpressionList* ptr) { expressionLists.release(ptr);}
  void release(Token* ptr) { tokens.release(ptr);}
  void release(TokenList* ptr) { tokenLists.release(ptr);}
  void release(FunctionCall* ptr) { funcCalls.release(ptr);}
  void release(ArrayAccess* ptr) { arrayAccesses.release(ptr);}
  void release(Statement* ptr) { statements.release(ptr);}
  void release(Scope* ptr) { scopes.release(ptr);}
  void release(ArrayOrStructLiteral* ptr) { arrOrStructs.release(ptr);}
  void release(StatementList* ptr) { statementList.release(ptr);}
  void release(FunctionDec* ptr) { functionDecs.release(ptr);}
  void release(EnumDec* ptr) { enums.release(ptr);}
  void release(StructDec* ptr) { structs.release(ptr);}
  void release(TemplateDec* ptr) { templates.release(ptr);}
};
