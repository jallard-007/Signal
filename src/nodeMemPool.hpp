#pragma once

#include "nodes.hpp"
#include "memPool.hpp"

class NodeMemPool {
  MemPool<UnOp> unOps;
  MemPool<BinOp> binOps;
  MemPool<GlobalDecList> decs;
  MemPool<VariableDec> varDecs;
  MemPool<FunctionCall> funcCalls;
  MemPool<ElifStatementList> elifs;
  MemPool<ControlFlowStatement> controlFlows;
  MemPool<ArrayAccess> arrayAccesses;
  MemPool<Token> tokens;
  MemPool<TokenList> tokenLists;
  MemPool<Expression> expressions;
  MemPool<ExpressionList> expressionLists;
  MemPool<Scope> scopes;
  MemPool<StatementList> statementLists;
  MemPool<StructDecList> structDecLists;
  MemPool<ArrayOrStructLiteral> arraysOrStructs;

public:
  void reset() {
    unOps.reset();
    binOps.reset();
    decs.reset();
    varDecs.reset();
    funcCalls.reset();
    elifs.reset();
    controlFlows.reset();
    arrayAccesses.reset();
    tokens.reset();
    tokenLists.reset();
    expressions.reset();
    expressionLists.reset();
    scopes.reset();
    statementLists.reset();
    structDecLists.reset();
    arraysOrStructs.reset();
  }

  UnOp* makeUnOp(UnOp&& ref) {return unOps.get(std::move(ref));}
  BinOp* makeBinOp(BinOp&& ref) {return binOps.get(std::move(ref));}
  GlobalDecList* makeGlobalDec() {return decs.get();}
  VariableDec* makeVariableDec(VariableDec&& ref) {return varDecs.get(std::move(ref));}
  FunctionCall* makeFunctionCall(FunctionCall&& ref) {return funcCalls.get(std::move(ref));}
  ElifStatementList* makeElifStatementList() {return elifs.get();}
  ControlFlowStatement* makeControlFlowStatement() {return controlFlows.get();}
  ArrayAccess* makeArrayAccess(ArrayAccess&& ref) {return arrayAccesses.get(std::move(ref));}
  Token* makeToken(const Token& ref) {return tokens.get(ref);}
  TokenList* makeTokenList() {return tokenLists.get();}
  Expression* makeDefaultedExpression() {return expressions.get();}
  ExpressionList* makeExpressionList() {return expressionLists.get();}
  Scope* makeScope() {return scopes.get();}
  StatementList* makeStatementList() {return statementLists.get();}
  StructDecList* makeStructDecList() {return structDecLists.get();}
  ArrayOrStructLiteral* makeArrayOrStruct() {return arraysOrStructs.get();}

  void release(UnOp* ptr) { unOps.release(ptr);}
  void release(BinOp* ptr) { binOps.release(ptr);}
  void release(GlobalDecList* ptr) { decs.release(ptr);}
  void release(VariableDec* ptr) { varDecs.release(ptr);}
  void release(FunctionCall* ptr) { funcCalls.release(ptr);}
  void release(ElifStatementList* ptr) { elifs.release(ptr);}
  void release(ControlFlowStatement* ptr) { controlFlows.release(ptr);}
  void release(ArrayAccess* ptr) { arrayAccesses.release(ptr);}
  void release(Token* ptr) { tokens.release(ptr);}
  void release(TokenList* ptr) { tokenLists.release(ptr);}
  void release(Expression* ptr) { expressions.release(ptr);}
  void release(ExpressionList* ptr) { expressionLists.release(ptr);}
  void release(Scope* ptr) { scopes.release(ptr);}
  void release(StatementList* ptr) { statementLists.release(ptr);}
  void release(StructDecList* ptr) { structDecLists.release(ptr);}
  void release(ArrayOrStructLiteral* ptr) { arraysOrStructs.release(ptr);}
};
