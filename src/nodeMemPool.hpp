#pragma once

#include "nodes.hpp"
#include "memPool.hpp"

class NodeMemPool {
  MemPool<UnOp> unOps;
  MemPool<BinOp> binOps;
  MemPool<ControlFlowStatement> controlFlows;
  MemPool<ElifStatementList> elifs;
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
  Expression* makeExpression(Expression&& ref) {return expressions.get(std::move(ref));}
  ExpressionList* makeExpressionList() {return expressionLists.get();}
  Statement* makeStatement(Statement&& ref) {return statements.get(std::move(ref));}
  Scope* makeScope() {return scopes.get();}
  ArrayOrStructLiteral* makeArrayOrStructLiteral(ArrayOrStructLiteral&& ref) {return arrOrStructs.get(std::move(ref));}
  StatementList* makeStatementList() {return statementList.get();}
  FunctionDec* makeFunctionDec(FunctionDec&& ref) {return functionDecs.get(std::move(ref));}
  EnumDec* makeEnumDec(EnumDec&& ref) {return enums.get(std::move(ref));}
  StructDec* makeStructDec(StructDec&& ref) {return structs.get(std::move(ref));}
  TemplateDec* makeTemplateDec() {return templates.get();}

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
