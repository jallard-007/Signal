#pragma once

#include "nodes.hpp"
#include "memPool.hpp"

class NodeMemPool {
  MemPool<UnOp> unOps;
  MemPool<BinOp> binOps{500};
  MemPool<GeneralDec> decs;
  MemPool<GeneralDecList> decLists;
  MemPool<VariableDec> varDecs;
  MemPool<FunctionCall> funcCalls;
  MemPool<ElifStatementList> elifs;
  MemPool<ControlFlowStatement> controlFlows;
  MemPool<ArrayAccess> arrayAccesses;
  MemPool<TokenList> tokenLists;
  MemPool<Expression> expressions;
  MemPool<ExpressionList> expressionLists;
  MemPool<Scope> scopes;
  MemPool<StatementList> statementLists;
  MemPool<StructDecList> structDecLists;
  MemPool<ArrayOrStructLiteral> arraysOrStructs;
  MemPool<FunctionDec> functionDecs;
  MemPool<StructDec> structDecs;
  MemPool<TemplateDec> templateDecs;
  MemPool<ConditionalStatement> conditionalStatements;
  MemPool<ReturnStatement> returnStatements;
  MemPool<ForLoop> forLoops;
  MemPool<WhileLoop> whileLoops;
  MemPool<SwitchStatement> switchStatements;
  MemPool<SwitchScopeStatementList> switchScopeStatementLists;

public:
  void reset() {
    unOps.reset();
    binOps.reset();
    decs.reset();
    decLists.reset();
    varDecs.reset();
    funcCalls.reset();
    elifs.reset();
    controlFlows.reset();
    arrayAccesses.reset();
    tokenLists.reset();
    expressions.reset();
    expressionLists.reset();
    scopes.reset();
    statementLists.reset();
    structDecLists.reset();
    arraysOrStructs.reset();
    functionDecs.reset();
    structDecs.reset();
    templateDecs.reset();
    conditionalStatements.reset();
    returnStatements.reset();
    forLoops.reset();
    whileLoops.reset();
    switchStatements.reset();
    switchScopeStatementLists.reset();
  }

  UnOp* makeUnOp(const UnOp& ref) {return unOps.get(ref);}
  BinOp* makeBinOp(const BinOp& ref) {return binOps.get(ref);}
  GeneralDec* makeGeneralDec() {return decs.get();}
  GeneralDecList* makeGeneralDecList() {return decLists.get();}
  VariableDec* makeVariableDec(const VariableDec& ref) {return varDecs.get(ref);}
  FunctionCall* makeFunctionCall(const FunctionCall& ref) {return funcCalls.get(ref);}
  ElifStatementList* makeElifStatementList() {return elifs.get();}
  ControlFlowStatement* makeControlFlowStatement() {return controlFlows.get();}
  ArrayAccess* makeArrayAccess(const ArrayAccess& ref) {return arrayAccesses.get(ref);}
  TokenList* makeTokenList() {return tokenLists.get();}
  Expression* makeExpression() {return expressions.get();}
  ExpressionList* makeExpressionList() {return expressionLists.get();}
  Scope* makeScope() {return scopes.get();}
  StatementList* makeStatementList() {return statementLists.get();}
  StructDecList* makeStructDecList() {return structDecLists.get();}
  ArrayOrStructLiteral* makeArrayOrStruct() {return arraysOrStructs.get();}
  FunctionDec* makeFunctionDec() {return functionDecs.get();}
  StructDec* makeStructDec() {return structDecs.get();}
  TemplateDec* makeTemplateDec() {return templateDecs.get();}
  ConditionalStatement* makeConditionalStatement() {return conditionalStatements.get();}
  ReturnStatement* makeReturnStatement() {return returnStatements.get();}
  ForLoop* makeForLoop() {return forLoops.get();}
  WhileLoop* makeWhileLoop() {return whileLoops.get();}
  SwitchStatement* makeSwitchStatement() {return switchStatements.get();}
  SwitchScopeStatementList* makeSwitchScopeStatementList() {return switchScopeStatementLists.get();}



  void release(UnOp* ptr) { unOps.release(ptr);}
  void release(BinOp* ptr) { binOps.release(ptr);}
  void release(GeneralDec* ptr) { decs.release(ptr);}
  void release(GeneralDecList* ptr) { decLists.release(ptr);}
  void release(VariableDec* ptr) { varDecs.release(ptr);}
  void release(FunctionCall* ptr) { funcCalls.release(ptr);}
  void release(ElifStatementList* ptr) { elifs.release(ptr);}
  void release(ControlFlowStatement* ptr) { controlFlows.release(ptr);}
  void release(ArrayAccess* ptr) { arrayAccesses.release(ptr);}
  void release(TokenList* ptr) { tokenLists.release(ptr);}
  void release(Expression* ptr) { expressions.release(ptr);}
  void release(ExpressionList* ptr) { expressionLists.release(ptr);}
  void release(Scope* ptr) { scopes.release(ptr);}
  void release(StatementList* ptr) { statementLists.release(ptr);}
  void release(StructDecList* ptr) { structDecLists.release(ptr);}
  void release(ArrayOrStructLiteral* ptr) { arraysOrStructs.release(ptr);}
  void release(FunctionDec* ptr) { functionDecs.release(ptr);}
  void release(StructDec* ptr) { structDecs.release(ptr);}
  void release(TemplateDec* ptr) { templateDecs.release(ptr);}
  void release(ConditionalStatement* ptr) { conditionalStatements.release(ptr);}
  void release(ReturnStatement* ptr) { returnStatements.release(ptr);}
  void release(ForLoop* ptr) {forLoops.release(ptr);}
  void release(WhileLoop* ptr) { whileLoops.release(ptr);}
  void release(SwitchStatement* ptr) { switchStatements.release(ptr);}
  void release(SwitchScopeStatementList* ptr) { switchScopeStatementLists.release(ptr);}
};
