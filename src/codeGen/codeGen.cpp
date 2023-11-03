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
ExpressionResult CodeGen::generateExpression(const Expression &currExp, bool controlFlow) {
  switch (currExp.type) {
    case ExpressionType::ARRAY_ACCESS: {
      return generateExpressionArrAccess(*currExp.arrAccess);
    }
    case ExpressionType::ARRAY_OR_STRUCT_LITERAL: {
      return generateExpressionArrOrStructLit(*currExp.arrayOrStruct);
    }
    case ExpressionType::BINARY_OP: {
      return generateExpressionBinOp(*currExp.binOp, controlFlow);
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
      return generateExpression(*currExp.wrapped, controlFlow);
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
        return {false, true, (uint64_t)charLiteral[1]};
      } else if (charLiteral.size() == 4) {
        if (charLiteral[2] >= '0' && charLiteral[2] <= '9') {
          return {false, false, (uint64_t)(charLiteral[2] - '0')};
        } else if (charLiteral[2] == 'n') {
          return {false, false, '\n'};
        } else if (charLiteral[2] == '\\') {
          return {false, false, '\\'};
        } else if (charLiteral[2] == '\'') {
          return {false, false, '\''};
        }
      }
      return {false, false, 0};
    }
    case TokenType::STRING_LITERAL: { 
      std::string stringLiteral = tk->extractToken(token);
      return {false, false, 0};
      // place string literal in data section of bytecode string, return offset to start of string
    }
    case TokenType::DECIMAL_NUMBER: {
      std::string decimalNumber = tk->extractToken(token);
      uint64_t num = std::stoull(decimalNumber);
      return {false, false, num};
    }
    case TokenType::BINARY_NUMBER: { 
      std::string binaryNumber = tk->extractToken(token);
      uint64_t num = std::stoull(binaryNumber, nullptr, 2);
      return {false, false, num};
    }
    case TokenType::HEX_NUMBER: { 
      std::string hexNumber = tk->extractToken(token);
      /*
      std::invalid_argument
      std::out_of_range
      */
      uint64_t num = std::stoull(hexNumber, nullptr, 16);
      return {false, false, num};
    }
    case TokenType::FALSE: { 
      return {false, false, 0};
    }
    case TokenType::TRUE: { 
      return {false, false, 1};
    }
    case TokenType::NULL_PTR: {
      return {false, false, 0};
    }
    default: {
      return {false, false, 0};
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
 * Align for size byte immediate with offset
 * Use before any instruction that has an immediate value
 * Param offset: number of bytes before immediate
 * Param size: size in bytes of immediate
*/
void CodeGen::alignForImm(const uint32_t offset, const uint32_t size) {
  uint8_t mod = (byteCode.size() + offset) % size;
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
  switch (op) {
    case OpCodes::ADD: {
      return left + right;
    }
    case OpCodes::SUB: {
      return left - right;
    }
    case OpCodes::MUL: {
      return left * right;
    }
    case OpCodes::DIV: {
      return left / right;
    }
    case OpCodes::MOD: {
      return left % right;
    }
    case OpCodes::OR: {
      return left | right;
    }
    case OpCodes::AND: {
      return left & right;
    }
    case OpCodes::XOR: {
      return left ^ right;
    }
    case OpCodes::SHIFT_L: {
      return left << right;
    }
    case OpCodes::SHIFT_R: {
      return left >> right;
    }
    default: {
      std::cerr << "Invalid OpCode in codeGen::evaluateExpression [" << (uint32_t)op  << "]\n";
      exit(1);
    }
  }
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
    alignForImm(2, 4);
    addBytes({(uc)OpCodes::MOVE_I, reg, split[4], split[5], split[6], split[7]});
    alignForImm(2, 4);
    addBytes({(uc)OpCodes::SHIFT_L_I, reg, 32, 0, 0, 0});
  }
  alignForImm(2, 4);
  addBytes({(uc)OpCodes::MOVE_I, reg, split[0], split[1], split[2], split[3]});
}

/**
 * Generates byte code for a mathematical binary expression
 * Preserves any non-temporary values
*/
ExpressionResult CodeGen::mathematicalBinOp(const BinOp& binOp, const OpCodes op, const OpCodes opImm) {
  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  const bool leftImm = !leftResult.isReg;
  const bool rightImm = !rightResult.isReg;

  // left imm, right imm ./
  // left imm, right temp ./
  // left imm, right var ./
  // left temp, right imm ./
  // left temp, right temp ./
  // left temp, right var ./
  // left var, right imm ./
  // left var, right temp ./
  // left var, right var ./

  // left imm, right imm
  if (leftImm && rightImm) {
    // both operands are immediate values, return the result
    return {false, true, evaluateExpression(op, leftResult.val, rightResult.val)};
  }

  if (leftImm) {
    if (topBitsSet(leftResult.val)) {
      const uc reg = allocateRegister();
      moveImmToReg(reg, leftResult.val);
      leftResult.val = reg;
      leftResult.isReg = true;
    }
  }
  else if (rightImm) {
    if (topBitsSet(rightResult.val)) {
      const uc reg = allocateRegister();
      moveImmToReg(reg, rightResult.val);
      rightResult.val = reg;
      rightResult.isReg = true;
    }
  }

  // left temp
  if (leftResult.isTemp) {
    // right imm
    if (!rightResult.isReg) {
      uc *split = (uc *)&rightResult.val;
      alignForImm(2, 4);
      addBytes({(uc)opImm, (uc)leftResult.val, split[0], split[1], split[2], split[3]});
      return {true, true, leftResult.val};
    }
    // right temp / var
    else {
      addBytes({(uc)op, (uc)leftResult.val, (uc)rightResult.val});
      if (rightResult.isTemp) {
        freeRegister(rightResult.val);
      }
      return {true, true, leftResult.val};
    }
  }
  // left imm
  if (!leftResult.isReg) {
    // right temp
    if (rightResult.isTemp) {
      if (isCommutative(op)) {
        uc *split = (uc *)&leftResult.val;
        alignForImm(2, 4);
        addBytes({(uc)opImm, (uc)rightResult.val, split[0], split[1], split[2], split[3]});
        return {true, true, rightResult.val};
      }
      // cannot flip args
      else {
        uc reg = allocateRegister();
        moveImmToReg(reg, leftResult.val);
        addBytes({(uc)op, (uc)reg, (uc)rightResult.val});
        freeRegister(rightResult.val);
        return {true, true, reg};
      }
    }
    // right var
    else {
      uc reg = allocateRegister();
      moveImmToReg(reg, leftResult.val);
      addBytes({(uc)op, (uc)reg, (uc)rightResult.val});
      return {true, true, reg};
    }
  }
  // left var
  {
    // right imm
    if (!rightResult.isReg) {
      if (isCommutative(op)) {
        uc reg = allocateRegister();
        moveImmToReg(reg, rightResult.val);
        addBytes({(uc)op, reg, (uc)leftResult.val});
        return {true, true, reg};
      } else {
        uc reg = allocateRegister();
        addBytes({(uc)OpCodes::MOVE, reg, (uc)leftResult.val});
        uc *split = (uc *)&rightResult.val;
        addBytes({(uc)op, (uc)reg, split[0], split[1], split[2], split[3]});
        return {true, true, reg};
      }
    }
    // right temp
    else if (rightResult.isTemp) {
      if (isCommutative(op)) {
        addBytes({(uc)op, (uc)rightResult.val, (uc)leftResult.val});
        return rightResult;
      } else {
        uc reg = allocateRegister();
        addBytes({(uc)OpCodes::MOVE, reg, (uc)leftResult.val});
        addBytes({(uc)op, (uc)reg, (uc)rightResult.val});
        freeRegister(rightResult.val);
        return {true, true, reg};
      }
    }
    // right var
    else {
      uc reg = allocateRegister();
      addBytes({(uc)OpCodes::MOVE, reg, (uc)leftResult.val});
      addBytes({(uc)op, (uc)reg, (uc)rightResult.val});
      return {true, true, reg};
    }
  }
}

/**
 * Generates byte code for assignment expressions
 * Values on the left side are not preserved
*/
ExpressionResult CodeGen::assignmentBinOp(const BinOp& binOp, const OpCodes op, const OpCodes opImm) {
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  registers[leftResult.val].changed = true;
  if (!rightResult.isReg) {
    if (topBitsSet(rightResult.val)) {
      moveImmToReg(miscIndex, rightResult.val);
      addBytes({(uc)op, (uc)leftResult.val, miscIndex});
    } else {
      uc *split = (uc *)&rightResult.val;
      alignForImm(2, 4);
      addBytes({(uc)opImm, (uc)leftResult.val, split[0], split[1], split[2], split[3]});
    }
  } else {
    addBytes({(uc)op, (uc)leftResult.val, (uc)rightResult.val});
  }
  return leftResult;
}

ExpressionResult CodeGen::logicalBinOp(const BinOp& binOp, bool controlFlow, OpCodes op, OpCodes jumpOp, OpCodes getOp) {
  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  if (!leftResult.isReg) {
    const uc reg = allocateRegister();
    moveImmToReg(reg, leftResult.val);
    leftResult.isReg = true;
    leftResult.val = reg;
    leftResult.isTemp = true;
  }
  if (!rightResult.isReg) {
    const uc reg = allocateRegister();
    moveImmToReg(reg, rightResult.val);
    rightResult.isReg = true;
    rightResult.val = reg;
    rightResult.isTemp = true;
  }
  addBytes({(uc)op, (uc)leftResult.val, (uc)rightResult.val});
  if (rightResult.isTemp) {
    freeRegister(rightResult.val);
  }
  if (leftResult.isTemp) {
    freeRegister(leftResult.val);
  }
  ExpressionResult expRes;
  if (controlFlow) {
    expRes.isJumpOp = true;
    expRes.val = (uint64_t)jumpOp;
  } else {
    expRes.isReg = true;
    expRes.isTemp = true;
    const uc reg = allocateRegister();
    addBytes({(uc)getOp, reg});
    expRes.val = reg;
  }
  return expRes;
}

ExpressionResult CodeGen::generateExpressionBinOp(const BinOp &binOp, bool controlFlow) {
  switch (binOp.op.type) {
    // member access
    case TokenType::DOT: {

    }
    case TokenType::PTR_MEMBER_ACCESS: {

    }

    // mathematical ops
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

    // assignment ops
    case TokenType::ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::MOVE, OpCodes::MOVE_I);
    }
    case TokenType::ADDITION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::ADD, OpCodes::ADD_I);
    }
    case TokenType::SUBTRACTION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::SUB, OpCodes::SUB_I);
    }
    case TokenType::MULTIPLICATION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::MUL, OpCodes::MUL_I);
    }
    case TokenType::DIVISION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::DIV, OpCodes::DIV_I);
    }
    case TokenType::MODULO_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::MOD, OpCodes::MOD_I);
    }
    case TokenType::BITWISE_OR_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::OR, OpCodes::OR_I);
    }
    case TokenType::BITWISE_XOR_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::XOR, OpCodes::XOR_I);
    }
    case TokenType::BITWISE_AND_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::AND, OpCodes::AND_I);
    }
    case TokenType::SHIFT_LEFT_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::SHIFT_L, OpCodes::SHIFT_L_I);
    }
    case TokenType::SHIFT_RIGHT_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCodes::SHIFT_R, OpCodes::SHIFT_R_I);
    }

    // figure out marker system to allow replacement of jump instruction values
    // logical
    case TokenType::EQUAL: {
      return logicalBinOp(binOp, controlFlow, OpCodes::CMP, OpCodes::JUMP_E, OpCodes::GET_E);
    }
    case TokenType::NOT_EQUAL: {
      return logicalBinOp(binOp, controlFlow, OpCodes::CMP, OpCodes::JUMP_NE, OpCodes::GET_NE);
    }
    case TokenType::LOGICAL_AND: {
      return logicalBinOp(binOp, controlFlow, OpCodes::LOGICAL_AND, OpCodes::JUMP_NE, OpCodes::GET_NE);
    }
    case TokenType::LOGICAL_OR: {
      return logicalBinOp(binOp, controlFlow, OpCodes::LOGICAL_OR, OpCodes::JUMP_NE, OpCodes::GET_NE);
    }
    case TokenType::LESS_THAN: {
      return logicalBinOp(binOp, controlFlow, OpCodes::CMP, OpCodes::JUMP_L, OpCodes::GET_L);
    }
    case TokenType::LESS_THAN_EQUAL: {
      return logicalBinOp(binOp, controlFlow, OpCodes::CMP, OpCodes::JUMP_LE, OpCodes::GET_LE);
    }
    case TokenType::GREATER_THAN: {
      return logicalBinOp(binOp, controlFlow, OpCodes::CMP, OpCodes::JUMP_G, OpCodes::GET_G);
    }
    case TokenType::GREATER_THAN_EQUAL: {
      return logicalBinOp(binOp, controlFlow, OpCodes::CMP, OpCodes::JUMP_GE, OpCodes::GET_GE);
    }
    default: {
      std::cerr << "Invalid token type in BinOp expression [" << (int32_t)binOp.op.type << "]\n";
      exit(1);
    }
  }
}
