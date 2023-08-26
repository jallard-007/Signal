#include <iostream>
#include "codeGen.hpp"

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define bp registers[basePointerIndex]
#define dp registers[dataPointerIndex]
#define misc registers[miscIndex]

CodeGen::CodeGen(
  Program& program,
  std::vector<Tokenizer>& tokenizers
): program{program}, tokenizers{tokenizers} {
  sp.inUse = true;
  ip.inUse = true;
  bp.inUse = true;
  dp.inUse = true;
  misc.inUse = true;
}

/*

keep track of which register a variable is in,
if its not in a register then allocate one for it
if theres no more register then :/
update value on stack with the value in the register if its been updated (need to mark a register as changed or not)
be sure to free registers if they are no longer needed

*/


// start simple. load off of stack whenever needed, redundancy is fine, similar to -O0
// functions will start by putting all arguments on the stack
uint32_t CodeGen::generateExpression(const Expression &currExp) {
  switch (currExp.type) {
    case ExpressionType::ARRAY_ACCESS: {
      return generateExpressionArrAccess(*currExp.arrAccess);
    }
    case ExpressionType::ARRAY_OR_STRUCT_LITERAL: {
      return generateExpressionArrOrStructLit(*currExp.arrayOrStruct);
    }
    case ExpressionType::BINARY_OP: {
      return generateExpressionBinOp(*currExp.binOp);
    }
    case ExpressionType::FUNCTION_CALL: {
      return generateExpressionFunctionCall(*currExp.funcCall);
    }
    case ExpressionType::NONE: {
      return 0;
    }
    case ExpressionType::UNARY_OP: {
      return generateExpressionUnOp(*currExp.unOp);
    }
    case ExpressionType::VALUE: {
      return loadValue(currExp.value);
    }
    case ExpressionType::WRAPPED: {
      return generateExpression(*currExp.wrapped);
    }
    default: {
      std::cerr << "Code generation not implemented for this expression type\n";
      exit(1);
    }
  }
}

uint32_t CodeGen::generateExpressionArrAccess(const ArrayAccess &arrAcc) {
}

uint32_t CodeGen::generateExpressionArrOrStructLit(const ArrayOrStructLiteral &arrStructLit) {
}

uint32_t CodeGen::generateExpressionFunctionCall(const FunctionCall &funcCall) {
}

uint32_t CodeGen::generateExpressionBinOp(const BinOp &binOp) {
  // imm op imm
  // imm op var
  // var op imm
  // var op var
}

uint32_t CodeGen::generateExpressionUnOp(const UnOp &unOp) {
}

uint64_t CodeGen::loadValue(const Token &token) {
  switch (token.type) {
    case TokenType::CHAR_LITERAL: {
      std::string charLiteral = tk->extractToken(token);
      // convert charLiteral to its numeric value and return it
      if (charLiteral.size() == 3) {
        return charLiteral[1];
      } else if (charLiteral.size() == 4) {
        if (charLiteral[2] >= '0' && charLiteral[2] <= '9') {
          return charLiteral[2] - '0';
        } else if (charLiteral[2] == 'n') {
          return '\n';
        } else if (charLiteral[2] == '\\') {
          return '\\';
        } else if (charLiteral[2] == '\'') {
          return '\'';
        }
      }
      return 0;
    }
    case TokenType::STRING_LITERAL: { 
      std::string stringLiteral = tk->extractToken(token);
      // place string literal in data section of bytecode string
    }
    case TokenType::DECIMAL_NUMBER: {
      std::string decimalNumber = tk->extractToken(token);
      // convert to numeric and return
    }
    case TokenType::BINARY_NUMBER: { 
      std::string binaryNumber = tk->extractToken(token);
      // convert to numeric and return
    }
    case TokenType::HEX_NUMBER: { 
      std::string hexNumber = tk->extractToken(token);
      // convert to numeric and return
    }
    case TokenType::FALSE: { 
      return 0;
    }
    case TokenType::TRUE: { 
      return 1;
    }
    case TokenType::NULL_PTR: {
      return 0;
    }
  }
}

uint32_t CodeGen::allocateRegister() {
  for (uint32_t i = 0; i < NUM_REGISTERS; ++i) {
    if (!registers[i].inUse) {
      registers[i].inUse = true;
      return i;
    }
  }
  std::cerr << "Compiler Error: Failed to allocate a register; zero available registers\n";
  exit(1);
}

void CodeGen::freeRegister(uint32_t regNum) {
  RegisterInfo& regInfo = registers[regNum];
  if (regInfo.stackOffset && regInfo.changed) {
    // SUB_I misc, bp, regInfo.stackOffset
    // STORE misc, currReg
  }
  regInfo.stackOffset = 0;
  regInfo.changed = false;
  regInfo.inUse = false;
}

int32_t CodeGen::getStackOffset(const std::string &varName) {
}
