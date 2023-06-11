#pragma once

#include "nodes.hpp"
#include "memPool.hpp"

class NodeMemPool {
  MemPool<UnOp> unOps;
  MemPool<BinOp> binOps;
  MemPool<VariableDec> varDecs;
  MemPool<FunctionCall> funcCalls;
  MemPool<ArrayAccess> arrayAccesses;
  MemPool<Statement> statements;
  MemPool<Scope> scopes;
  MemPool<List> lists;
  MemPool<KeywordWithBody> keysWBodies;
  MemPool<FunctionDec> functionDecs;
  MemPool<Enum> enums;
  MemPool<Struct> structs;
  MemPool<Template> templates;

public:
  void reset() {
    unOps.reset();
    binOps.reset();
    varDecs.reset();
    funcCalls.reset();
    arrayAccesses.reset();
    statements.reset();
    scopes.reset();
    lists.reset();
    keysWBodies.reset();
    functionDecs.reset();
    enums.reset();
    structs.reset();
    templates.reset();
  }

  UnOp* get(UnOp&& ref) {return unOps.get(std::move(ref));}
  BinOp* get(BinOp&& ref) {return binOps.get(std::move(ref));}
  VariableDec* get(VariableDec&& ref) {return varDecs.get(std::move(ref));}
  FunctionCall* get(FunctionCall&& ref) {return funcCalls.get(std::move(ref));}
  ArrayAccess* get(ArrayAccess&& ref) {return arrayAccesses.get(std::move(ref));}
  Statement* get(Statement&& ref) {return statements.get(std::move(ref));}
  Scope* get(Scope&& ref) {return scopes.get(std::move(ref));}
  List* get(List&& ref) {return lists.get(std::move(ref));}
  KeywordWithBody* get(KeywordWithBody&& ref) {return keysWBodies.get(std::move(ref));}
  FunctionDec* get(FunctionDec&& ref) {return functionDecs.get(std::move(ref));}
  Enum* get(Enum&& ref) {return enums.get(std::move(ref));}
  Struct* get(Struct&& ref) {return structs.get(std::move(ref));}
  Template* get(Template&& ref) {return templates.get(std::move(ref));}

  void release(UnOp* ptr) { unOps.release(ptr);}
  void release(BinOp* ptr) { binOps.release(ptr);}
  void release(VariableDec* ptr) { varDecs.release(ptr);}
  void release(FunctionCall* ptr) { funcCalls.release(ptr);}
  void release(ArrayAccess* ptr) { arrayAccesses.release(ptr);}
  void release(Statement* ptr) { statements.release(ptr);}
  void release(Scope* ptr) { scopes.release(ptr);}
  void release(List* ptr) { lists.release(ptr);}
  void release(KeywordWithBody* ptr) { keysWBodies.release(ptr);}
  void release(FunctionDec* ptr) { functionDecs.release(ptr);}
  void release(Enum* ptr) { enums.release(ptr);}
  void release(Struct* ptr) { structs.release(ptr);}
  void release(Template* ptr) { templates.release(ptr);}
};
