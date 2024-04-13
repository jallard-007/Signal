#pragma once

#include "nodes.hpp"
#include "memPool.hpp"

/**
 * Memory pool for all nodes that require dynamic allocations
*/
class NodeMemPool {
    MemPool<UnOp> unOps;
    MemPool<BinOp> binOps;
    MemPool<BuiltinFunc> builtinFuncs;
    MemPool<BuiltinType> builtinTypes;
    MemPool<GeneralDec> decs;
    MemPool<GeneralDecList> decLists;
    MemPool<VariableDec> varDecs;
    MemPool<FunctionCall> funcCalls;
    MemPool<ElifStatementList> elifs;
    MemPool<ControlFlowStatement> controlFlows;
    MemPool<ArrayAccess> arrayAccesses;
    MemPool<TokenList> tokenLists;
    MemPool<LiteralValue> literalValues;
    MemPool<Expression> expressions;
    MemPool<ExpressionList> expressionLists;
    MemPool<Scope> scopes;
    MemPool<StatementList> statementLists;
    MemPool<StructDecList> structDecLists;
    MemPool<ContainerLiteral> containerLiterals;
    MemPool<FunctionDec> functionDecs;
    MemPool<StructDec> structDecs;
    MemPool<TemplateDec> templateDecs;
    MemPool<ConditionalStatement> conditionalStatements;
    MemPool<ReturnStatement> returnStatements;
    MemPool<ForLoop> forLoops;
    MemPool<WhileLoop> whileLoops;
    MemPool<SwitchStatement> switchStatements;
    MemPool<SwitchScopeStatementList> switchScopeStatementLists;
    MemPool<TemplateCreation> templateCreations;
    MemPool<IncludeDec> includeDecs;

public:
    void reset() {
        unOps.reset();
        binOps.reset();
        builtinFuncs.reset();
        builtinTypes.reset();
        decs.reset();
        decLists.reset();
        varDecs.reset();
        funcCalls.reset();
        elifs.reset();
        controlFlows.reset();
        arrayAccesses.reset();
        tokenLists.reset();
        literalValues.reset();
        expressions.reset();
        expressionLists.reset();
        scopes.reset();
        statementLists.reset();
        structDecLists.reset();
        containerLiterals.reset();
        functionDecs.reset();
        structDecs.reset();
        templateDecs.reset();
        conditionalStatements.reset();
        returnStatements.reset();
        forLoops.reset();
        whileLoops.reset();
        switchStatements.reset();
        switchScopeStatementLists.reset();
        templateCreations.reset();
        includeDecs.reset();
    }

    UnOp* makeUnOp(const UnOp& ref) {return unOps.get(ref);}
    BinOp* makeBinOp(const BinOp& ref) {return binOps.get(ref);}
    BuiltinFunc* makeBuiltinFunc(const BuiltinFunc& ref) {return builtinFuncs.get(ref);}
    BuiltinType* makeBuiltinType(const BuiltinType& ref) {return builtinTypes.get(ref);}
    GeneralDec* makeGeneralDec() {return decs.get();}
    GeneralDecList* makeGeneralDecList() {return decLists.get();}
    VariableDec* makeVariableDec(const VariableDec& ref) {return varDecs.get(ref);}
    FunctionCall* makeFunctionCall(const FunctionCall& ref) {return funcCalls.get(ref);}
    ElifStatementList* makeElifStatementList() {return elifs.get();}
    ControlFlowStatement* makeControlFlowStatement() {return controlFlows.get();}
    ArrayAccess* makeArrayAccess(const ArrayAccess& ref) {return arrayAccesses.get(ref);}
    TokenList* makeTokenList() {return tokenLists.get();}
    LiteralValue* makeLiteralValue() {return literalValues.get();}
    Expression* makeExpression() {return expressions.get();}
    ExpressionList* makeExpressionList() {return expressionLists.get();}
    Scope* makeScope() {return scopes.get();}
    StatementList* makeStatementList() {return statementLists.get();}
    StructDecList* makeStructDecList() {return structDecLists.get();}
    ContainerLiteral* makeContainerLiteral() {return containerLiterals.get();}
    FunctionDec* makeFunctionDec() {return functionDecs.get();}
    StructDec* makeStructDec() {return structDecs.get();}
    TemplateDec* makeTemplateDec() {return templateDecs.get();}
    ConditionalStatement* makeConditionalStatement() {return conditionalStatements.get();}
    ReturnStatement* makeReturnStatement() {return returnStatements.get();}
    ForLoop* makeForLoop() {return forLoops.get();}
    WhileLoop* makeWhileLoop() {return whileLoops.get();}
    SwitchStatement* makeSwitchStatement() {return switchStatements.get();}
    SwitchScopeStatementList* makeSwitchScopeStatementList() {return switchScopeStatementLists.get();}
    TemplateCreation* makeTemplateCreation() {return templateCreations.get();}
    IncludeDec* makeIncludeDec() {return includeDecs.get();}

    void release(const UnOp* ptr) { unOps.release(ptr);}
    void release(const BinOp* ptr) { binOps.release(ptr);}
    void release(const BuiltinFunc* ptr) { builtinFuncs.release(ptr);}
    void release(const BuiltinType* ptr) { builtinTypes.release(ptr);}
    void release(const GeneralDec* ptr) { decs.release(ptr);}
    void release(const GeneralDecList* ptr) { decLists.release(ptr);}
    void release(const VariableDec* ptr) { varDecs.release(ptr);}
    void release(const FunctionCall* ptr) { funcCalls.release(ptr);}
    void release(const ElifStatementList* ptr) { elifs.release(ptr);}
    void release(const ControlFlowStatement* ptr) { controlFlows.release(ptr);}
    void release(const ArrayAccess* ptr) { arrayAccesses.release(ptr);}
    void release(const TokenList* ptr) { tokenLists.release(ptr);}
    void release(const LiteralValue* ptr) { literalValues.release(ptr);}
    void release(const Expression* ptr) { expressions.release(ptr);}
    void release(const ExpressionList* ptr) { expressionLists.release(ptr);}
    void release(const Scope* ptr) { scopes.release(ptr);}
    void release(const StatementList* ptr) { statementLists.release(ptr);}
    void release(const StructDecList* ptr) { structDecLists.release(ptr);}
    void release(const ContainerLiteral* ptr) { containerLiterals.release(ptr);}
    void release(const FunctionDec* ptr) { functionDecs.release(ptr);}
    void release(const StructDec* ptr) { structDecs.release(ptr);}
    void release(const TemplateDec* ptr) { templateDecs.release(ptr);}
    void release(const ConditionalStatement* ptr) { conditionalStatements.release(ptr);}
    void release(const ReturnStatement* ptr) { returnStatements.release(ptr);}
    void release(const ForLoop* ptr) {forLoops.release(ptr);}
    void release(const WhileLoop* ptr) { whileLoops.release(ptr);}
    void release(const SwitchStatement* ptr) { switchStatements.release(ptr);}
    void release(const SwitchScopeStatementList* ptr) { switchScopeStatementLists.release(ptr);}
    void release(const TemplateCreation* ptr) { templateCreations.release(ptr);}
    void release(const IncludeDec* ptr) { includeDecs.release(ptr);}
};
