#include <iostream>
#include <cassert>
#include "codeGen.hpp"

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define bp registers[basePointerIndex]
#define dp registers[dataPointerIndex]
#define misc registers[miscIndex] // used for temporary/intermediate values

#define uc unsigned char

ExpressionResult::ExpressionResult(bool isReg, bool isTemp, uint64_t val):
  val{val}, jumpOp{OpCodes::NOP}, isReg{isReg}, isTemp{isTemp}, isStruct{false} {}

JumpMarker::JumpMarker(uint64_t index, JumpMarkerType type):
  index{index}, type{type} {}

bool JumpMarker::operator==(const JumpMarker other) const {
  return other.index == index && other.type == type;
}

CodeGen::CodeGen(
  Program& program,
  std::vector<Tokenizer>& tokenizers,
  std::map<std::string, GeneralDec *>& lookUp
): program{program}, lookUp{lookUp}, tokenizers{tokenizers} {
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
ExpressionResult CodeGen::generateExpressionArrAccess(const ArrayAccess &arrAccess) {
  if (arrAccess.array.wrapped) {
    return {false, false, 0};
  }
  return {false, false, 0};
}

ExpressionResult CodeGen::generateExpressionArrOrStructLit(const ArrayOrStructLiteral &arrOrStructLit) {
  if (arrOrStructLit.values.next) {
    return {false, false, 0};
  }
  return {false, false, 0};
}
ExpressionResult CodeGen::generateExpressionFunctionCall(const FunctionCall &functionCall) {
  if (functionCall.name.length) {
    return {false, false, 0};
  }
  return {false, false, 0};
}


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
        return {false, false, (uint64_t)charLiteral[1]};
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
    case TokenType::IDENTIFIER: {
      // check if value is already in a reg. to do that we need a lookup. variable name to information regarding location, if its already in a register, etc.
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
void CodeGen::addByteOp(OpCodes opCode) {
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
  if (mod == 0) {
    return;
  }
  while(mod++ != size) {
    addByteOp(OpCodes::NOP);
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
    return {false, false, evaluateExpression(op, leftResult.val, rightResult.val)};
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
      uc reg = allocateRegister();
      addBytes({(uc)OpCodes::MOVE, reg, (uc)leftResult.val});
      uc *split = (uc *)&rightResult.val;
      addBytes({(uc)op, (uc)reg, split[0], split[1], split[2], split[3]});
      return {true, true, reg};
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

/**
 * Generates the code for a boolean bin op 
 * On controlFlow, this will set the jumpOp field in ExpressionResult to the correct jump op
 * On !controlFlow, the result will be put into a register
 * \param binOp the binOp object to generate code for
 * \param op the op code used to do the comparison
 * \param jumpOp the jump op code to use on control flow statement. jump on !condition, so the jump op jumps when the condition is false. used when controlFlow is true
 * \param getOp the get op code to use when placing the result in a register. used when controlFlow is false
 * \param controlFlow dictates if the jump op will be returned, or if the get op will be used and a register will be returned
*/
ExpressionResult CodeGen::booleanBinOp(const BinOp& binOp, OpCodes op, OpCodes jumpOp, OpCodes getOp, bool controlFlow) {
  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  if (!leftResult.isReg) {
    const uc reg = allocateRegister();
    moveImmToReg(reg, leftResult.val);
    leftResult.isReg = true;
    leftResult.val = reg;
    leftResult.isTemp = true;
  }
  // short-circuit logical ops
  if (binOp.op.type == TokenType::LOGICAL_AND) {
    addBytes({(uc)OpCodes::SET_Z, (uc)leftResult.val});
    alignForImm(1, 8);
    addMarker(JumpMarkerType::SHORT_CIRCUIT);
    addBytes({(uc)OpCodes::JUMP_E, 0, 0, 0, 0, 0, 0, 0, 0});
  }
  else if (binOp.op.type == TokenType::LOGICAL_OR) {
    addBytes({(uc)OpCodes::SET_Z, (uc)leftResult.val});
    alignForImm(1, 8);
    addMarker(JumpMarkerType::SHORT_CIRCUIT);
    addBytes({(uc)OpCodes::JUMP_NE, 0, 0, 0, 0, 0, 0, 0, 0});
  }
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  if (!rightResult.isReg) {
    const uc reg = allocateRegister();
    moveImmToReg(reg, rightResult.val);
    rightResult.isReg = true;
    rightResult.val = reg;
    rightResult.isTemp = true;
  }
  addBytes({(uc)op, (uc)leftResult.val, (uc)rightResult.val});
  updateJumpOpTo(byteCode.size(), JumpMarkerType::SHORT_CIRCUIT);
  if (rightResult.isTemp) {
    freeRegister(rightResult.val);
  }
  if (leftResult.isTemp) {
    freeRegister(leftResult.val);
  }
  ExpressionResult expRes;
  if (controlFlow) {
    expRes.jumpOp = jumpOp;
  } else {
    expRes.isReg = true;
    expRes.isTemp = true;
    const uc reg = allocateRegister();
    addBytes({(uc)getOp, reg});
    expRes.val = reg;
  }
  return expRes;
}

ExpressionResult CodeGen::generateExpressionBinOp(const BinOp& binOp, bool controlFlow) {
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
      return booleanBinOp(binOp, OpCodes::CMP, OpCodes::JUMP_NE, OpCodes::GET_E, controlFlow);
    }
    case TokenType::NOT_EQUAL: {
      return booleanBinOp(binOp, OpCodes::CMP, OpCodes::JUMP_E, OpCodes::GET_NE, controlFlow);
    }
    case TokenType::LOGICAL_AND: {
      return booleanBinOp(binOp, OpCodes::LOGICAL_AND, OpCodes::JUMP_E, OpCodes::GET_NE, controlFlow);
    }
    case TokenType::LOGICAL_OR: {
      return booleanBinOp(binOp, OpCodes::LOGICAL_OR, OpCodes::JUMP_E, OpCodes::GET_NE, controlFlow);
    }
    case TokenType::LESS_THAN: {
      return booleanBinOp(binOp, OpCodes::CMP, OpCodes::JUMP_GE, OpCodes::GET_L, controlFlow);
    }
    case TokenType::LESS_THAN_EQUAL: {
      return booleanBinOp(binOp, OpCodes::CMP, OpCodes::JUMP_G, OpCodes::GET_LE, controlFlow);
    }
    case TokenType::GREATER_THAN: {
      return booleanBinOp(binOp, OpCodes::CMP, OpCodes::JUMP_LE, OpCodes::GET_G, controlFlow);
    }
    case TokenType::GREATER_THAN_EQUAL: {
      return booleanBinOp(binOp, OpCodes::CMP, OpCodes::JUMP_L, OpCodes::GET_GE, controlFlow);
    }
    default: {
      std::cerr << "Invalid token type in BinOp expression [" << (int32_t)binOp.op.type << "]\n";
      exit(1);
    }
  }
}

ExpressionResult CodeGen::generateExpressionUnOp(const UnOp& unOp) {
  ExpressionResult expRes = generateExpression(unOp.operand);
  switch (unOp.op.type) {
    case TokenType::NOT: {
      if (!expRes.isReg) {
        const uc reg = allocateRegister();
        moveImmToReg(reg, expRes.val);
        expRes.val = reg;
        expRes.isReg = true;
        expRes.isTemp = true;
      }
      addBytes({(uc)OpCodes::NOT, (uc)expRes.val});
      return expRes;
    }
    case TokenType::ADDRESS_OF: {
      return expRes;
    }
    case TokenType::DEREFERENCE: {
      return expRes;
    }
    case TokenType::INCREMENT_POSTFIX: {
      return expRes;
    }
    case TokenType::INCREMENT_PREFIX: {
      return expRes;
    }
    case TokenType::DECREMENT_POSTFIX: {
      return expRes;
    }
    case TokenType::DECREMENT_PREFIX: {
      return expRes;
    }
    case TokenType::NEGATIVE: {
      return expRes;
    }
    default: {
      std::cerr << "Invalid\n";
      exit(1);
    }
  }
}

uint32_t sizeOfType(const TokenType type) {
  switch (type) {
    case TokenType::BOOL: {
      return 1;
    }
    case TokenType::CHAR_TYPE: {
      return 1;
    }
    case TokenType::STRING_TYPE: {
      return 8;
    }
    case TokenType::INT8_TYPE: {
      return 1;
    }
    case TokenType::UINT8_TYPE: {
      return 1;
    }
    case TokenType::INT16_TYPE: {
      return 2;
    }
    case TokenType::UINT16_TYPE: {
      return 2;
    }
    case TokenType::INT32_TYPE: {
      return 4;
    }
    case TokenType::UINT32_TYPE: {
      return 4;
    }
    case TokenType::INT64_TYPE: {
      return 8;
    }
    case TokenType::UINT64_TYPE: {
      return 8;
    }
    case TokenType::POINTER: {
      return 8;
    }
    case TokenType::DOUBLE_TYPE: {
      return 8;
    }
    case TokenType::VOID: {
      return 0;
    }
    default: {
      std::cerr << "Invalid TokenType in CodeGen::sizeOfType\n";
      exit(1);
    }
  }
}

/**
 * Makes rooms on the stack for the variable and assigns the initial assignment to it
 * \returns the size of the type
*/
uint32_t CodeGen::generateVariableDeclaration(const VariableDec& varDec, bool initialize, bool inStruct) {
  int16_t reg = -1; 
  Token typeToken = varDec.type.token;
  if (isBuiltInType(typeToken.type)) {
    if (initialize && varDec.initialAssignment) {
      ExpressionResult expRes = generateExpression(*varDec.initialAssignment);
      if (!expRes.isReg) {
        reg = allocateRegister();
        moveImmToReg(reg, expRes.val);
      } else {
        reg = expRes.val;
      }
    }
    const uint32_t size = sizeOfType(varDec.type.token.type);
    switch (size) {
      case 0: {
        break;
      }
      case 1: {
        if (reg != -1) {
          addBytes({(uc)OpCodes::PUSH_B, (uc)reg});
        } else {
          addBytes({(uc)OpCodes::DEC, stackPointerIndex});
        }
        break;
      }
      case 2: {
        if (reg != -1) {
          addBytes({(uc)OpCodes::PUSH_W, (uc)reg});
        } else {
          addBytes({(uc)OpCodes::DEC, stackPointerIndex});
          addBytes({(uc)OpCodes::DEC, stackPointerIndex});
        }
        break;
      }
      case 4: {
        if (reg != -1) {
          addBytes({(uc)OpCodes::PUSH_D, (uc)reg});
        } else {
          alignForImm(2, 4);
          addBytes({(uc)OpCodes::SUB_I, stackPointerIndex, 4, 0, 0, 0});
        }
        break;
      }
      case 8: {
        if (reg != -1) {
          addBytes({(uc)OpCodes::PUSH_Q, (uc)reg});
        } else {
          alignForImm(2, 4);
          addBytes({(uc)OpCodes::SUB_I, stackPointerIndex, 8, 0, 0, 0});
        }
        break;
      }
      default: {
        std::cerr << "Invalid Size in generateVariableDeclaration\n";
        exit(1);
      }
    }
    return size;
  }
  else if (typeToken.type == TokenType::REFERENCE) {
    if (!inStruct) {
      return 0;
    }
    addBytes({(uc)OpCodes::PUSH_Q, (uc)reg});
    return 8;
  }
  else if (typeToken.type == TokenType::IDENTIFIER) {
    const uint32_t size = generateVariableDeclarationStructType(varDec, initialize);
    return size;
  }
  exit(1);
}

StructInformation& CodeGen::getStructInfo(const std::string& structName) {
  StructInformation &info = structNameToInfoMap[structName];
  if (info.size != -1) {
    return info;
  }
  info.size = 0;
  GeneralDec const * const &generalDec = lookUp[structName];
  Tokenizer *oldTk = tk;
  tk = &tokenizers[generalDec->tokenizerIndex];
  StructDecList *structDecList = &generalDec->structDec->decs;
  while (structDecList) {
    if (structDecList->type != StructDecType::VAR) {
      continue;
    }
    Token typeToken = structDecList->varDec->type.token;
    uint32_t& offset = info.offsetMap[tk->extractToken(structDecList->varDec->name)];
    uint32_t size, alignTo;
    if (typeToken.type == TokenType::IDENTIFIER) {
      StructInformation& subStructInfo = getStructInfo(tk->extractToken(typeToken));
      size = subStructInfo.size;
      alignTo = subStructInfo.alignTo;
    }
    else if (isBuiltInType(typeToken.type)) {
      size = sizeOfType(typeToken.type);
      alignTo = size;
    }
    else /*if (typeToken.type == TokenType::REFERENCE)*/ {
      size = 8;
      alignTo = size;
    }
    uint32_t paddingRequired = size - ((info.size) % size);
    if (paddingRequired == size) {
      paddingRequired = 0;
    }
    info.size += paddingRequired;
    offset = info.size;
    info.size += size;
    if (alignTo > info.alignTo) {
      info.alignTo = alignTo;
    }
    structDecList = structDecList->next;
  }
  uint32_t paddingRequired = info.alignTo - ((info.size) % info.alignTo);
  if (paddingRequired != info.alignTo) {
    info.size += paddingRequired;
  }
  tk = oldTk;
  return info;
}

// TODO:
uint32_t CodeGen::generateVariableDeclarationStructType(const VariableDec& varDec, bool initialize) {
  std::string structName = tk->extractToken(varDec.type.token);
  GeneralDec const * const &generalDec = lookUp[structName];
  Tokenizer *oldTk = tk;
  tk = &tokenizers[generalDec->tokenizerIndex];
  StructDecList *structDecList = &generalDec->structDec->decs;
  while (structDecList) {
    if (structDecList->type == StructDecType::VAR) {
      generateVariableDeclaration(*structDecList->varDec, initialize, true);
    }
    structDecList = structDecList->next;
  }
  tk = oldTk;
  return 999; // need to update
}

uint64_t CodeGen::addMarker(JumpMarkerType type) {
  jumpMarkers.emplace_back(byteCode.size(), type);
  return byteCode.size();
}

void CodeGen::generateIfStatement(const IfStatement& ifStatement) {
  ExpressionResult expRes = generateExpression(ifStatement.condition, true);
  if (expRes.jumpOp == OpCodes::NOP) {
    if (!expRes.isReg) {
      if (expRes.val) {
        // condition always true
        generateScope(ifStatement.body);
      }
      // else condition always false
      return;
    } else {
      addBytes({(uc)OpCodes::SET_Z, (uc)expRes.val});
      expRes.jumpOp = OpCodes::JUMP_E;
    }
  }
  alignForImm(1, 8);
  addMarker(JumpMarkerType::IF_STATEMENT);
  addBytes({(uc)expRes.jumpOp, 0, 0, 0, 0, 0, 0, 0, 0});
  // if condition is false, jump to next in chain (elif if there is one, otherwise else if there is one, otherwise next statement)
  generateScope(ifStatement.body);
}

void CodeGen::updateJumpOpTo(const uint64_t indexTo, JumpMarkerType type, JumpMarkerType until) {
  for (auto jumpMarker = jumpMarkers.rbegin(); jumpMarker != jumpMarkers.rend(); ++jumpMarker) {
    if (until != JumpMarkerType::NONE && jumpMarker->type == until) {
      jumpMarker->type = JumpMarkerType::NONE;
      break;
    }
    if (jumpMarker->type == type) {
      uint64_t index = jumpMarker->index + 1;
      *(uint64_t *)(byteCode.data() + index) = indexTo;
      jumpMarker->type = JumpMarkerType::NONE;
      if (until == JumpMarkerType::NONE) {
        break;
      }
    }
  }
  // clean up
  while (!jumpMarkers.empty() && jumpMarkers.back().type == JumpMarkerType::NONE) {
    jumpMarkers.pop_back();
  }
}

void CodeGen::generateControlFlowStatement(const ControlFlowStatement& controlFlowStatement) {
  switch (controlFlowStatement.type) {
    case ControlFlowStatementType::FOR_LOOP: {
      generateStatement(controlFlowStatement.forLoop->initialize);
      const uint64_t startOfLoopIndex = addMarker(JumpMarkerType::START_LOOP);
      if (controlFlowStatement.forLoop->condition.type != ExpressionType::NONE) {
        ExpressionResult expRes = generateExpression(controlFlowStatement.forLoop->condition);
        if (expRes.jumpOp == OpCodes::NOP) {
          if (!expRes.isReg) {
            if (!expRes.val) {
              // condition always false
              break;
            }
            // dont need to add a jump statement, go straight to the body
          } else {
            addBytes({(uc)OpCodes::SET_Z, (uc)expRes.val});
            alignForImm(1, 8);
            addMarker(JumpMarkerType::IF_STATEMENT);
            addBytes({(uc)OpCodes::JUMP_E, 0, 0, 0, 0, 0, 0, 0, 0});
          }
        } else {
          alignForImm(1, 8);
          addMarker(JumpMarkerType::IF_STATEMENT);
          addBytes({(uc)expRes.jumpOp, 0, 0, 0, 0, 0, 0, 0, 0});
        }
      }
      generateScope(controlFlowStatement.forLoop->body);
      generateExpression(controlFlowStatement.forLoop->iteration);
      alignForImm(1, 8);
      std::vector<uc> jumpOp  = {(uc)OpCodes::JUMP, 0, 0, 0, 0, 0, 0, 0, 0};
      *(uint64_t *)(jumpOp.data() + 1) = startOfLoopIndex;
      addBytes(jumpOp);
      updateJumpOpTo(byteCode.size(), JumpMarkerType::IF_STATEMENT);
      updateJumpOpTo(byteCode.size(), JumpMarkerType::BREAK, JumpMarkerType::START_LOOP);
      updateJumpOpTo(startOfLoopIndex, JumpMarkerType::CONTINUE, JumpMarkerType::START_LOOP);
      break;
    }
    case ControlFlowStatementType::WHILE_LOOP: {
      const uint64_t startOfLoopIndex = addMarker(JumpMarkerType::START_LOOP);
      IfStatement& whileLoop = controlFlowStatement.whileLoop->statement;
      generateIfStatement(whileLoop);
      alignForImm(1, 8);
      std::vector<uc> jumpOp  = {(uc)OpCodes::JUMP, 0, 0, 0, 0, 0, 0, 0, 0};
      *(uint64_t *)(jumpOp.data() + 1) = startOfLoopIndex;
      addBytes(jumpOp);
      updateJumpOpTo(byteCode.size(), JumpMarkerType::IF_STATEMENT);
      updateJumpOpTo(byteCode.size(), JumpMarkerType::BREAK, JumpMarkerType::START_LOOP);
      updateJumpOpTo(startOfLoopIndex, JumpMarkerType::CONTINUE, JumpMarkerType::START_LOOP);
      break; 
    }
    case ControlFlowStatementType::CONDITIONAL_STATEMENT: {
      const IfStatement& ifStatement = controlFlowStatement.conditional->ifStatement;
      const ElifStatementList *elifStatementList = controlFlowStatement.conditional->elifStatement;
      const Scope* elseStatement = controlFlowStatement.conditional->elseStatement;
      addMarker(JumpMarkerType::START_IF); // mark start of this if/elif/else chain

      // if statement
      generateIfStatement(ifStatement);
      if (elifStatementList || elseStatement) {
        alignForImm(1, 8);
        addMarker(JumpMarkerType::END_IF);
        addBytes({(uc)OpCodes::JUMP, 0, 0, 0, 0, 0, 0, 0, 0});
      }
      updateJumpOpTo(byteCode.size(), JumpMarkerType::IF_STATEMENT);

      // elif statements
      while (elifStatementList) {
        generateIfStatement(elifStatementList->elif);
        elifStatementList = elifStatementList->next;
        if (elifStatementList || elseStatement) {
          alignForImm(1, 8);
          addMarker(JumpMarkerType::END_IF);
          addBytes({(uc)OpCodes::JUMP, 0, 0, 0, 0, 0, 0, 0, 0});
        }
        updateJumpOpTo(byteCode.size(), JumpMarkerType::IF_STATEMENT);
      }

      // else statement
      if (elseStatement) {
        generateScope(*elseStatement);
      }

      // update all END_IF jump ops (until START_IF) to go to current index
      updateJumpOpTo(byteCode.size(), JumpMarkerType::END_IF, JumpMarkerType::START_IF);
      break;
    }
    case ControlFlowStatementType::RETURN_STATEMENT: {
      break;
    }
    case ControlFlowStatementType::EXIT_STATEMENT: {
      break;
    }
    case ControlFlowStatementType::SWITCH_STATEMENT: {
      break;
    }
    default: {
      std::cerr << "Invalid ControlFlowStatementType in CodeGen::generateControlFlowStatement\n";
      exit(1);
    }
  }
}

void CodeGen::generateScope(const Scope& scope) {
  for (
    const StatementList *statementList = &scope.scopeStatements;
    statementList;
    statementList = statementList->next
  ) {
    generateStatement(statementList->curr);
    return;
  }
}

void CodeGen::generateStatement(const Statement& statement) {
  switch(statement.type) {
    case StatementType::NONE: {
      break;
    }
    case StatementType::EXPRESSION: {
      generateExpression(*statement.expression);
      break;
    }
    case StatementType::CONTROL_FLOW: {
      generateControlFlowStatement(*statement.controlFlow);
      break;
    }
    case StatementType::SCOPE: {
      generateScope(*statement.scope);
      break;
    }
    case StatementType::VARIABLE_DEC: {
      generateVariableDeclaration(*statement.varDec);
      break;
    }
    case StatementType::KEYWORD: {
      // continue or break
      if (statement.keyword.type == TokenType::BREAK) {
        alignForImm(1, 8);
        addMarker(JumpMarkerType::BREAK);
        addBytes({(uc)OpCodes::JUMP, 0, 0, 0, 0, 0, 0, 0, 0});
      }
      else if (statement.keyword.type == TokenType::CONTINUE) {
        alignForImm(1, 8);
        addMarker(JumpMarkerType::CONTINUE);
        addBytes({(uc)OpCodes::JUMP, 0, 0, 0, 0, 0, 0, 0, 0});
      }
      else {
        std::cerr << "Invalid keyword type in CodeGen::generateStatement\n";
        exit(1);
      }
      break;
    }
  }
}
