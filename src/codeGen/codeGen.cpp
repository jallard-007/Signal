#include <iostream>
#include "codeGen.hpp"

// start simple. load off of stack whenever needed, redundancy is fine, similar to -O0
// functions will start by putting all arguments on the stack
void CodeGen::generateExpression(const Expression &currExp) {
  switch (currExp.type) {
    case ExpressionType::ARRAY_ACCESS: {
      generateExpressionArrAccess(*currExp.arrAccess);
      break;
    }
    case ExpressionType::ARRAY_OR_STRUCT_LITERAL: {
      generateExpressionArrOrStructLit(*currExp.arrayOrStruct);
      break;
    }
    case ExpressionType::BINARY_OP: {
      generateExpressionBinOp(*currExp.binOp);
      break;
    }
    case ExpressionType::FUNCTION_CALL: {
      generateExpressionFunctionCall(*currExp.funcCall);
      break;
    }
    case ExpressionType::NONE: {
      break;
    }
    case ExpressionType::UNARY_OP: {
      generateExpressionUnOp(*currExp.unOp);
      break;
    }
    case ExpressionType::VALUE: {
      generateExpressionValue(currExp.value);
      break;
    }
    case ExpressionType::WRAPPED: {
      generateExpression(*currExp.wrapped);
      break;
    }
    default: {
      std::cerr << "Code generation not implemented for this expression type\n";
      exit(1);
    }
  }
}

void CodeGen::generateExpressionArrAccess(const ArrayAccess &arrAcc) {

}

void CodeGen::generateExpressionArrOrStructLit(const ArrayOrStructLiteral &arrStructLit) {

}

void CodeGen::generateExpressionFunctionCall(const FunctionCall &funcCall) {

}


void CodeGen::generateExpressionBinOp(const BinOp &binOp) {
}

void CodeGen::generateExpressionUnOp(const UnOp &unOp) {
}

int CodeGen::generateExpressionValue(const Token &token) {
}

int CodeGen::allocateRegister() {

}

void CodeGen::getRegister() {

}

int getStackOffset(const std::string &varName) {

}

