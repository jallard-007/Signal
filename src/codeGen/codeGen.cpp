#include <cstdint>
#include <iostream>
#include "codeGen.hpp"

uint32_t getAvailableRegister() {
  static bool registers[16]{};

}

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
  // map operation to asm code. + -> add, - -> sub


  // store result in register
  // return code of register
}

void CodeGen::generateExpressionUnOp(const UnOp &unOp) {

}

int CodeGen::generateExpressionValue(const Token &token) {
  switch (token.type) {
    case TokenType::IDENTIFIER: { // stack variable, load it into a register
      int reg = allocateRegister();
      int pos = getStackOffset(tk->extractToken(token));

      // load stack data into register
      // mov size suffix  offset(%rbp), reg

      // for global
      // mov size suffix  label(%rip), reg

      // checker should find size of structs, how much space is needed for locals in a scope. need to add local for that in struct/scope node
      return reg;
    }
    case TokenType::STRING_LITERAL:
    case TokenType::CHAR_LITERAL: { // let caller handle
      return -(int)token.type;
    }
  }
}

int CodeGen::allocateRegister() {

}

void CodeGen::getRegister() {

}

int getStackOffset(const std::string &varName) {

}

