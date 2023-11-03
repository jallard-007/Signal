#include <iostream>
#include "codeGen.hpp"

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define bp registers[basePointerIndex]
#define dp registers[dataPointerIndex]
#define misc registers[miscIndex] // used for temporary/intermediate values

#define uc unsigned char

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
ExpressionResult CodeGen::generateExpression(const Expression &currExp) {
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
      return {};
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

ExpressionResult CodeGen::loadValue(const Token &token) {
  switch (token.type) {
    case TokenType::CHAR_LITERAL: { // TODO: validate chars
      std::string charLiteral = tk->extractToken(token);
      // convert charLiteral to its numeric value and return it
      if (charLiteral.size() == 3) {
        return {false, charLiteral[1]};
      } else if (charLiteral.size() == 4) {
        if (charLiteral[2] >= '0' && charLiteral[2] <= '9') {
          return {false, charLiteral[2] - '0'};
        } else if (charLiteral[2] == 'n') {
          return {false, '\n'};
        } else if (charLiteral[2] == '\\') {
          return {false, '\\'};
        } else if (charLiteral[2] == '\'') {
          return {false, '\''};
        }
      }
      return {false, 0};
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
      return {false, 0};
    }
    case TokenType::TRUE: { 
      return {false, 1};
    }
    case TokenType::NULL_PTR: {
      return {false, 0};
    }
  }
}

uc CodeGen::allocateRegister() {
  for (uc i = 0; i < NUM_REGISTERS; ++i) {
    if (!registers[i].inUse) {
      registers[i].inUse = true;
      return i;
    }
  }
  std::cerr << "Compiler Error: Failed to allocate a register; zero available registers\n";
  exit(1);
}

void CodeGen::freeRegister(uc regNum) {
  RegisterInfo& regInfo = registers[regNum];
  if (regInfo.stackOffset && regInfo.changed) {
    // SUB_I misc, bp, regInfo.stackOffset
    // STORE misc, currReg
  }
  regInfo.stackOffset = 0;
  regInfo.changed = false;
  regInfo.inUse = false;
}

void CodeGen::addByte(uc byte) {
  byteCode.emplace_back(byte);
}
void CodeGen::addByte(OpCodes opCode) {
  addByte((uc)opCode);
}

void CodeGen::addBytes(const std::vector<uc>& bytes) {
  for (const uc byte: bytes) {
    addByte(byte);
  }
}

/**
 * Align for 4 byte immediate with offset
 * Use before any instruction that has an immediate value
 * Param offset: number of bytes before immediate
*/
void CodeGen::alignForImm(const uint32_t offset) {
  uint8_t mod = (byteCode.size() + offset) % 4;
  while(mod--) {
    addByte(OpCodes::NOP);
  }
}

/**
 * Returns true if any of the largest 32 bits are set, false otherwise
*/
bool topBitsSet(uint64_t val) {
  return val & 0xFFFFFFFF00000000; // check if any bit in largest 4 bytes are set
}

uint64_t evaluateExpression(OpCodes op, uint64_t left, uint64_t right) {

}

bool isCommutative(OpCodes op) {
  return
    op == OpCodes::SUB ||
    op == OpCodes::DIV ||
    op == OpCodes::F_SUB ||
    op == OpCodes::F_DIV;
}

/**
 * Moves any integer size val into a register.
 * Does bit shifting as necessary with 4 byte immediate limit
*/
void CodeGen::moveImmToReg(const uc reg, const uint64_t val) {
  uc* split = (unsigned char *)&val; // split 8 byte val into array of 8 1 byte values
  if (topBitsSet(val)) { // check if any bit in largest 4 bytes are set
    // add value in little endian format
    alignForImm(2);
    addBytes({(uc)OpCodes::MOVE_I, reg, split[4], split[5], split[6], split[7]});
    alignForImm(2);
    addBytes({(uc)OpCodes::SHIFT_L_I, reg, 32, 0, 0, 0});
  }
  alignForImm(2);
  addBytes({(uc)OpCodes::MOVE_I, reg, split[0], split[1], split[2], split[3]});
}

/**
 * Generates the byte code for a mathematical binary expression
*/
ExpressionResult CodeGen::mathematicalBinOp(const BinOp& binOp, const OpCodes op, const OpCodes opImm) {
  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  ExpressionResult rightResult = generateExpression(binOp.rightSide);

  if (!leftResult.isReg && !rightResult.isReg) {
    // both operands are immediate values, return the result
    return {false, evaluateExpression(op, leftResult.val, rightResult.val)};
  }
  // if we get here, one of leftSide and rightSide are not immediate values
  // therefore, one of leftResult.isReg and rightResult.isReg are true
  if (!leftResult.isReg) {
    // if we get here, leftResult.isReg is false, so rightResult.isReg must be true
    if (topBitsSet(leftResult.val)) {
      const uc reg = allocateRegister();
      moveImmToReg(reg, leftResult.val);
      leftResult.val = reg;
      leftResult.isReg = true;
    }
  }
  else {
    // inversely, if we get here, leftResult.isReg is true, so rightResult.isReg must be false
    if (topBitsSet(rightResult.val)) {
      const uc reg = allocateRegister();
      moveImmToReg(reg, rightResult.val);
      rightResult.val = reg;
      rightResult.isReg = true;
    }
  }

  if (leftResult.isReg && rightResult.isReg) {
    // register was allocated for both
    addBytes({(uc)op, (uc)leftResult.val, (uc)rightResult.val});
    freeRegister(rightResult.val);
    return {true, leftResult.val};
  }

  // reg was allocated for only one side
  uc reg = 0;
  if (isCommutative(op)) {
    uc *split = nullptr;
    if (rightResult.isReg) {
      reg = rightResult.val;
      split = (uc *)&leftResult.val; // use left side for immediate
    } else { // leftReg was allocated
      reg = leftResult.val;
      split = (uc *)&rightResult.val; // use right side for immediate
    }
    alignForImm(2);
    addBytes({(uc)opImm, reg, split[0], split[1], split[2], split[3]});
    return {true, reg};
  }

  // non commutative operations
  reg = leftResult.val;
  if (rightResult.isReg) {
    // have to place right side in reg since op is not commutative
    moveImmToReg(miscIndex, rightResult.val);
    addBytes({(uc)op, (uc)leftResult.val, miscIndex});
  }
  else {
    // left side is reg
    uc *split = (uc *)&rightResult.val;
    alignForImm(2);
    addBytes({(uc)opImm, reg, split[0], split[1], split[2], split[3]});
  }
  return {true, reg};
}

ExpressionResult CodeGen::generateExpressionBinOp(const BinOp &binOp) {
  switch (binOp.op.type) {
    case TokenType::ADDITION: {
      return mathematicalBinOp(binOp, OpCodes::ADD, OpCodes::ADD_I);
    }
    case TokenType::SUBTRACTION: {
      return mathematicalBinOp(binOp, OpCodes::SUB, OpCodes::SUB_I);
    }
    case TokenType::MULTIPLICATION: {
      return mathematicalBinOp(binOp, OpCodes::MUL, OpCodes::MUL_I);
    }
    case TokenType::DIVISION: {
      return mathematicalBinOp(binOp, OpCodes::DIV, OpCodes::DIV_I);
    }
    case TokenType::MODULO: {
      return mathematicalBinOp(binOp, OpCodes::MOD, OpCodes::MOD_I);
    }
    case TokenType::BITWISE_OR: {
      return mathematicalBinOp(binOp, OpCodes::OR, OpCodes::OR_I);
    }
    case TokenType::BITWISE_AND: {
      return mathematicalBinOp(binOp, OpCodes::AND, OpCodes::AND_I);
    }
    case TokenType::BITWISE_XOR: {
      return mathematicalBinOp(binOp, OpCodes::XOR, OpCodes::XOR_I);
    }
    case TokenType::SHIFT_LEFT: {
      return mathematicalBinOp(binOp, OpCodes::SHIFT_L, OpCodes::SHIFT_L_I);
    }
    case TokenType::SHIFT_RIGHT: {
      return mathematicalBinOp(binOp, OpCodes::SHIFT_R, OpCodes::SHIFT_R_I);
    }
    default: {
      std::cerr << "Invalid token type in BinOp expression [" << (int32_t)binOp.op.type << "]\n";
      exit(1);
    }
  }

}

#undef uc

