#include <iostream>
#include <cassert>
#include "codeGen.hpp"
#include "utils.hpp"

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define dp registers[dataPointerIndex]
#define miscReg registers[miscRegisterIndex] // used for temporary/intermediate values

#define bc bytecode_t

JumpMarker::JumpMarker(uint64_t start, JumpMarkerType type):
  start{start}, type{type} {}

CodeGen::CodeGen(
  Program& program,
  std::vector<Tokenizer>& tokenizers,
  std::map<std::string, GeneralDec *>& lookUp
): program{program}, lookUp{lookUp}, tokenizers{tokenizers} {
  sp.inUse = true;
  ip.inUse = true;
  dp.inUse = true;
  miscReg.inUse = true;
}

/*

keep track of which register a variable is in,
if its not in a register then allocate one for it
if theres no more register then :/
update value on stack with the value in the register if its been updated (need to mark a register as changed or not)
be sure to free registers if they are no longer needed

*/
ExpressionResult CodeGen::generateExpressionArrAccess(const ArrayAccess &arrAccess) {
  if (arrAccess.array.getBinOp()) {
    return {};
  }
  return {};
}

ExpressionResult CodeGen::generateExpressionArrOrStructLit(const ArrayOrStructLiteral &arrOrStructLit) {
  if (arrOrStructLit.values.next) {
    return {};
  }
  return {};
}

/**
 * Generates a function call expression
 * \param functionCall the function call node
 * \returns an ExpressionResult object
 * 
*/
ExpressionResult CodeGen::generateExpressionFunctionCall(const FunctionCall &functionCall) {
  // need to lookup return type so we can make room for it
  const std::string& functionName = tk->extractToken(functionCall.name);
  GeneralDec const *const &generalDec = lookUp[functionName];
  Token returnType = getTypeFromTokenList(generalDec->funcDec->returnType);
  (void)returnType;
  // 8 byte align return type since function assumes it is
  // makeRoomOnStack(returnType, 8);
  // add arguments to stack
  if (functionCall.args.curr.getType() != ExpressionType::NONE) {
    const ExpressionList *expressionList = &functionCall.args;
    do {
      ExpressionResult expRes = generateExpression(expressionList->curr);
      addExpressionResToStack(expRes);
    } while (expressionList);
  }

  // add function call ?
  alignForImm(1, 4);
  // register this position to be updated
  addByteOp(OpCode::JUMP);
  add4ByteNum(0);
  return {};
}

bool CodeGen::generate() {
  const GeneralDecList* decList = &program.decs;
  while (decList) {
    if (!generateGeneralDeclaration(decList->curr)) {
      return false;
    }
    decList = decList->next;
  }
  return true;
}


ExpressionResult CodeGen::generateExpression(const Expression &currExp, bool controlFlow) {
  switch (currExp.getType()) {
    case ExpressionType::ARRAY_ACCESS: {
      return generateExpressionArrAccess(*currExp.getArrayAccess());
    }
    // case ExpressionType::ARRAY_LITERAL:
    // case ExpressionType::STRUCT_LITERAL: {
    //   return generateExpressionArrOrStructLit(*currExp.getArrayOrStructLiteral());
    // }
    case ExpressionType::BINARY_OP: {
      return generateExpressionBinOp(*currExp.getBinOp(), controlFlow);
    }
    case ExpressionType::FUNCTION_CALL: {
      return generateExpressionFunctionCall(*currExp.getFunctionCall());
    }
    case ExpressionType::NONE: {
      return {};
    }
    case ExpressionType::UNARY_OP: {
      return generateExpressionUnOp(*currExp.getUnOp());
    }
    case ExpressionType::VALUE: {
      return loadValue(currExp.getToken());
    }
    default: {
      std::cerr << "Code generation not implemented for this expression type\n";
      exit(1);
    }
  }
}

ExpressionResult CodeGen::loadValue(const Token &token) {
  switch (token.type) {
    case TokenType::CHAR_LITERAL: {
      std::string charLiteral = tk->extractToken(token);
      // convert charLiteral to its numeric value and return it
      if (charLiteral.size() == 3) {
        return {.val = (uint64_t)charLiteral[1]};
      } else if (charLiteral.size() == 4) {
        if (charLiteral[2] >= '0' && charLiteral[2] <= '9') {
          return {.val = (uint64_t)(charLiteral[2] - '0')};
        } else if (charLiteral[2] == 'n') {
          return {.val = '\n'};
        } else if (charLiteral[2] == '\\') {
          return {.val = '\\'};
        } else if (charLiteral[2] == '\'') {
          return {.val = '\''};
        }
      }
      return {};
    }
    case TokenType::STRING_LITERAL: { 
      std::string stringLiteral = tk->extractToken(token);
      // place string literal in data section of bytecode, return offset to start of string
      return {};
    }
    case TokenType::DECIMAL_NUMBER: {
      std::string decimalNumber = tk->extractToken(token);
      uint64_t num = std::stoull(decimalNumber);
      return {.val = num};
    }
    case TokenType::BINARY_NUMBER: { 
      std::string binaryNumber = tk->extractToken(token);
      uint64_t num = std::stoull(binaryNumber, nullptr, 2);
      return {.val = num};
    }
    case TokenType::HEX_NUMBER: { 
      std::string hexNumber = tk->extractToken(token);
      /*
      std::invalid_argument
      std::out_of_range
      */
      uint64_t num = std::stoull(hexNumber, nullptr, 16);
      return {.val = num};
    }
    case TokenType::FALSE: { 
      return {};
    }
    case TokenType::TRUE: { 
      return {.val = 1};
    }
    case TokenType::NULL_PTR: {
      return {};
    }
    case TokenType::IDENTIFIER: {
      std::string identifier = tk->extractToken(token);
      uint32_t stackItemIndex = nameToStackItemIndex[identifier];
      StackVariable &variableInfo = stack[stackItemIndex].variable;
      if (variableInfo.reg) {
        return {.val = variableInfo.reg, .isReg = true};
      }
      const uint32_t varOffset = getVarOffsetFromSP(variableInfo);
      const uint32_t sizeOfVar = sizeOfType(getTypeFromTokenList(variableInfo.varDec.type));
      if (sizeOfVar > SIZE_OF_REGISTER) {
        // too large to fit into a reg, just return offset
        return {.val = varOffset};
      }
      // load value into a register
      variableInfo.reg = allocateRegister();
      OpCode loadOp = getLoadOpForSize(sizeOfVar);
      if (varOffset) {
        addBytes({{(bc)OpCode::MOVE, miscRegisterIndex, stackPointerIndex}});
        alignForImm(2, 4);
        addBytes({{(bc)OpCode::ADD_I, miscRegisterIndex}});
        add4ByteNum(varOffset);
        addBytes({{(bc)loadOp, variableInfo.reg, miscRegisterIndex}});
      } else {
        addBytes({{(bc)loadOp, variableInfo.reg, stackPointerIndex}});
      }
      return {.val = variableInfo.reg, .isReg = true};
    }
    default: {
      return {};
    }
  }
}

uint32_t CodeGen::getVarOffsetFromSP(const StackVariable &variableInfo) {
  // |                   | <- sp
  // |       | <- positionOnStack
  // | value |
  return getCurrStackPointerPosition() - variableInfo.positionOnStack;
}


OpCode getLoadOpForSize(const uint8_t size) {
  switch(size) {
    case 1: return OpCode::LOAD_B;
    case 2: return OpCode::LOAD_W;
    case 4: return OpCode::LOAD_D;
    case 8: return OpCode::LOAD_Q;
    default: std::cerr << "invalid size in getLoadOpForSize: " << (uint32_t)size << '\n'; exit(1);
  }
}


bytecode_t CodeGen::allocateRegister() {
  for (uint8_t i = 0; i < NUM_REGISTERS; ++i) {
    if (!registers[i].inUse) {
      registers[i].inUse = true;
      return i;
    }
  }
  std::cerr << "Compiler Error: Failed to allocate a register; zero available registers\n";
  exit(1);
}

void CodeGen::freeRegister(bytecode_t regNum) {
  RegisterInfo& regInfo = registers[regNum];
  if (regInfo.changed) {
    // SUB_I misc, bp, regInfo.stackOffset
    // STORE misc, currReg
  }
  regInfo.changed = false;
  regInfo.inUse = false;
}

void CodeGen::addByte(bc byte) {
  byteCode.emplace_back(byte);
}
void CodeGen::addByteOp(OpCode opCode) {
  addByte((bc)opCode);
}

void CodeGen::addBytes(const std::span<const bc> bytes) {
  byteCode.reserve(byteCode.size() + bytes.size());
  byteCode.insert(byteCode.end(), bytes.begin(), bytes.end());
}

void CodeGen::addNumBytes(const void *data, const uint64_t n) {
  byteCode.resize(byteCode.size() + n);
  memcpy(byteCode.end().base() - n, data, n);
}

void CodeGen::add2ByteNum(const uint16_t num) {
  addNumBytes(&num, sizeof(num));
}

void CodeGen::add4ByteNum(const uint32_t num) {
  addNumBytes(&num, sizeof(num));
}

void CodeGen::add8ByteNum(const uint64_t num) {
  addNumBytes(&num, sizeof(num));
}

void CodeGen::addPointer() {
  void *p = 0;
  addNumBytes(&p, sizeof p);
}

/**
 * Used to add a jump op
*/
void CodeGen::addJumpOp(OpCode jumpOp) {
  assert(jumpOp >= OpCode::RS_JUMP && jumpOp <= OpCode::RS_JUMP_LE);
  addByteOp(jumpOp);
  addByte(0);
}

/**
 * Align for size byte immediate with offset
 * Use before any instruction that has an immediate value
 * Param offset: number of bytes before immediate
 * Param size: size in bytes of immediate
*/
void CodeGen::alignForImm(const uint32_t offset, const uint32_t size) {
  uint32_t mod = (byteCode.size() + offset) % size;
  if (mod == 0) {
    return;
  }
  while(mod++ != size) {
    addByteOp(OpCode::NOP);
  }
}

/**
 * Problem with this is that we are using it for all numbers, including signed. need to rework
 * 
 * Returns true if any of the largest 32 bits are set, false otherwise
*/
bool topBitsSet(uint64_t val) {
  return val & 0xFFFFFFFF00000000; // check if any bit in largest 4 bytes are set
}

uint64_t evaluateExpression(OpCode op, uint64_t left, uint64_t right) {
  switch (op) {
    case OpCode::ADD: {
      return left + right;
    }
    case OpCode::SUB: {
      return left - right;
    }
    case OpCode::MUL: {
      return left * right;
    }
    case OpCode::DIV: {
      return left / right;
    }
    case OpCode::MOD: {
      return left % right;
    }
    case OpCode::OR: {
      return left | right;
    }
    case OpCode::AND: {
      return left & right;
    }
    case OpCode::XOR: {
      return left ^ right;
    }
    case OpCode::SHIFT_L: {
      return left << right;
    }
    case OpCode::SHIFT_R: {
      return left >> right;
    }
    default: {
      std::cerr << "Invalid OpCode in evaluateExpression [" << (uint32_t)op  << "]\n";
      exit(1);
    }
  }
}

bool evaluateBooleanBinOp(TokenType op, uint64_t leftSide, uint64_t rightSide) {
  switch (op) {
    case TokenType::EQUAL: {
      return leftSide == rightSide;
    }
    case TokenType::NOT_EQUAL: {
      return leftSide != rightSide;
    }
    case TokenType::LOGICAL_AND: {
      return leftSide && rightSide;
    }
    case TokenType::LOGICAL_OR: {
      return leftSide || rightSide;
    }
    case TokenType::LESS_THAN: {
      return leftSide < rightSide;
    }
    case TokenType::LESS_THAN_EQUAL: {
      return leftSide <= rightSide;
    }
    case TokenType::GREATER_THAN: {
      return leftSide > rightSide;
    }
    case TokenType::GREATER_THAN_EQUAL: {
      return leftSide >= rightSide;
    }
    default: {
      std::cerr << "Invalid TokenType in evaluateBooleanBinOp [" << (uint32_t)op  << "]\n";
      exit(1);
    }
  }
}

bool isCommutative(OpCode op) {
  return !(
    op == OpCode::SUB ||
    op == OpCode::DIV ||
    op == OpCode::F_SUB ||
    op == OpCode::F_DIV
  );
}

/**
 * Moves any integer size val into a register.
 * Does bit shifting as necessary with 4 byte immediate limit
*/
void CodeGen::moveImmToReg(const bytecode_t reg, const uint64_t val) {
  if (topBitsSet(val)) { // check if any bit in largest 4 bytes are set
    alignForImm(2, 8);
    addBytes({{(bc)OpCode::MOVE_LI, reg}});
    add8ByteNum(val);
  } else {
    alignForImm(2, 4);
    addBytes({{(bc)OpCode::MOVE_I, reg}});
    add4ByteNum(val);
  }
}

/**
 * Generates byte code for a mathematical binary expression
 * Preserves any non-temporary values
*/
ExpressionResult CodeGen::mathematicalBinOp(const BinOp& binOp, const OpCode op, const OpCode opImm) {
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
    return {.val = evaluateExpression(op, leftResult.val, rightResult.val)};
  }
  // beyond this point, only one of left or right can possibly be an imm

  // move any immediate values larger than 32 bits into registers, we only have 4 byte immediate instructions
  if (leftImm) {
    if (topBitsSet(leftResult.val)) {
      const bc reg = allocateRegister();
      moveImmToReg(reg, leftResult.val);
      leftResult.val = reg;
      leftResult.isReg = true;
    }
  }
  else if (rightImm) {
    if (topBitsSet(rightResult.val)) {
      const bc reg = allocateRegister();
      moveImmToReg(reg, rightResult.val);
      rightResult.val = reg;
      rightResult.isReg = true;
    }
  }

  // left temp
  if (leftResult.isTemp) {
    // right imm
    if (!rightResult.isReg) {
      alignForImm(2, 4);
      addBytes({{(bc)opImm, leftResult.reg}});
      add4ByteNum((uint32_t)rightResult.val);
      return {.reg = leftResult.reg, .isReg = true, .isTemp = true};
    }
    // right temp / var
    else {
      addBytes({{(bc)op, leftResult.reg, rightResult.reg}});
      if (rightResult.isTemp) {
        freeRegister(rightResult.reg);
      }
      return {.reg = leftResult.reg, .isReg = true, .isTemp = true};
    }
  }
  // left imm
  if (!leftResult.isReg) {
    // right is a reg
    assert(rightResult.isReg);
    // right temp
    if (rightResult.isTemp) {
      if (isCommutative(op)) {
        alignForImm(2, 4);
        addBytes({{(bc)opImm, rightResult.reg}});
        add4ByteNum((uint32_t)leftResult.val);
        return {.reg = rightResult.reg, .isReg = true, .isTemp = true};
      }
      // cannot flip args
      else {
        bc reg = allocateRegister();
        moveImmToReg(reg, leftResult.val);
        addBytes({{(bc)op, reg, rightResult.reg}});
        freeRegister(rightResult.reg);
        return {.val = reg, .isReg = true, .isTemp = true};
      }
    }
    // right var
    else {
      bc reg = allocateRegister();
      moveImmToReg(reg, leftResult.val);
      addBytes({{(bc)op, reg, rightResult.reg}});
      return {.val = reg, .isReg = true, .isTemp = true};
    }
  }
  // left var
  {
    // right imm
    if (!rightResult.isReg) {
      bc reg = allocateRegister();
      addBytes({{(bc)OpCode::MOVE, reg, leftResult.reg}});
      alignForImm(2, 4);
      addBytes({{(bc)op, reg}});
      add4ByteNum(uint32_t(rightResult.val));
      return {.val = reg, .isReg = true, .isTemp = true};
    }
    // right temp
    else if (rightResult.isTemp) {
      if (isCommutative(op)) {
        addBytes({{(bc)op, rightResult.reg, leftResult.reg}});
        return rightResult;
      } else {
        bc reg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, reg, leftResult.reg}});
        addBytes({{(bc)op, reg, rightResult.reg}});
        freeRegister(rightResult.reg);
        return {.reg = reg, .isReg = true, .isTemp = true};
      }
    }
    // right var
    else {
      bc reg = allocateRegister();
      addBytes({{(bc)OpCode::MOVE, reg, leftResult.reg}});
      addBytes({{(bc)op, reg, rightResult.reg}});
      return {.reg = reg, .isReg = true, .isTemp = true};
    }
  }
}

/**
 * Generates byte code for assignment expressions
 * Values on the left side are not preserved
*/
ExpressionResult CodeGen::assignmentBinOp(const BinOp& binOp, const OpCode op, const OpCode opImm) {
  // checkers job to insure the assignment is valid
  ExpressionResult leftResult = getAddressOfExpression(binOp.leftSide);
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  assert(!leftResult.isTemp);
  assert(leftResult.isReg);
  registers[leftResult.val].changed = true;
  if (!rightResult.isReg) {
    if (topBitsSet(rightResult.val)) {
      moveImmToReg(miscRegisterIndex, rightResult.val);
      addBytes({{(bc)op, leftResult.reg, miscRegisterIndex}});
    } else {
      alignForImm(2, 4);
      addBytes({{(bc)opImm, leftResult.reg}});
      add4ByteNum((uint32_t)rightResult.val);
    }
  } else {
    addBytes({{(bc)op, leftResult.reg, rightResult.reg}});
    if (rightResult.isTemp){
      freeRegister(rightResult.reg);
    }
  }
  // write new value to stack
  // have to know where leftResult is. it could be on the stack or heap
  // it could be an element in an array and/or an element in a struct
  // it could have already been calculated before and in a register
  // need a function to get that info based on an expression
  return leftResult;
}

/**
 * Generates the expression and returns the address of the result.
 * the expression must be of a type that has a resulting address.
 * Valid expression types: value (value must be an identifier, which is a valid variable), array access, binary op member access, unary op dereference
 * \returns an ExpressionResult object. isTemp is true if isReg is true
 * if isReg is set to false, it means there is something wrong with the expression. this should have been caught in the checker stage
*/
ExpressionResult CodeGen::getAddressOfExpression(const Expression& expression) {
  ExpressionResult expRes;
  switch (expression.getType()) {
    case ExpressionType::BINARY_OP: {
      // if binary op is not a member access, cannot get address of the result of a binary op
      assert(expression.getBinOp()->op.getType() == TokenType::PTR_MEMBER_ACCESS || expression.getBinOp()->op.getType() == TokenType::DOT);
      ExpressionResult thing = getAddressOfExpression(expression.getBinOp()->leftSide);
      /* need type of result to lookup struct info. since this is recursive, we need to return the resulting type so that the higher node can use it
        example:
        structArr[i].structMember.nestedStructMember

        ast would look like:
                            dot 
                          /     |
                      dot       nestedStructMember
                    /     |
            arrayAccess     structMember
              /     |
        structArr     i


      */
      const StructInformation& structInfo = structNameToInfoMap[];
      // get offset of member and add to structVar
      return expRes;
    }
    case ExpressionType::UNARY_OP: {
      // if unary op is not dereference, cannot get address of the result of a unary op
      assert(expression.getUnOp()->op.getType() == TokenType::DEREFERENCE);
      expRes = generateExpression(expression.getUnOp()->operand);
      return expRes;
    }
    case ExpressionType::VALUE: {
      // a plain identifier, just need to look up if it's a reference and then use original
      // otherwise it's a stack variable so get offset from stack pointer
      const std::string& ident = tk->extractToken(expression.getToken());
      uint32_t stackItemIndex = nameToStackItemIndex[ident];
      expRes.reg = allocateRegister();
      addBytes({{(bc)OpCode::MOVE, expRes.reg, stackPointerIndex}});
      alignForImm(2, 4);
      addBytes({{(bc)OpCode::ADD_I, expRes.reg}});
      add4ByteNum(getVarOffsetFromSP(stack[stackItemIndex].variable));
      return expRes;
    }
    case ExpressionType::ARRAY_ACCESS: {
      // need the value from expression.getArrayAccess()->offset
      // then add that to expression.getArrayAccess()->array
      ExpressionResult offset = generateExpression(expression.getArrayAccess()->offset);
      ExpressionResult array = getAddressOfExpression(expression.getArrayAccess()->array);
      assert(array.isReg); // 
      if (!offset.isReg) {
        // offset is immediate
        if (topBitsSet(offset.val)) {
          moveImmToReg(miscRegisterIndex, offset.val);
          addBytes({{(bc)OpCode::ADD, array.reg, miscRegisterIndex}});
        } else {
          alignForImm(2, 4);
          addBytes({{(bc)OpCode::ADD_I, array.reg}});
          add4ByteNum((uint32_t)offset.val);
        }
      } else {
        addBytes({{(bc)OpCode::ADD, array.reg, offset.reg}});
      }
      expRes.isReg = true;
      expRes.isTemp = true;
      expRes.reg = array.reg;
      return expRes;
    }
    default: {
      // invalid type
      assert(false);
      return expRes;
    }
  }

}

/**
 * Generates the code for a boolean bin op.
 * When controlFlow is set to true, this will set the jumpOp field in the return value to the correct jump op.
 * When controlFlow is set to false, the result will be put into a register
 * \param binOp the binOp object to generate code for
 * \param op the op code used to do the comparison
 * \param jumpOp the jump op code to use on control flow statement. jump on !condition, so the jump op jumps when the condition is false. used when controlFlow is true
 * \param getOp the get op code to use when placing the result in a register. used when controlFlow is false
 * \param controlFlow dictates if the jump op will be returned, or if the get op will be used and a register will be returned
*/
ExpressionResult CodeGen::booleanBinOp(const BinOp& binOp, OpCode op, OpCode jumpOp, OpCode getOp, bool controlFlow) {
  // TODO: this is really messy, try to clean up. logical and / logical or are done on their own
  if (binOp.op.type == TokenType::LOGICAL_AND || binOp.op.type == TokenType::LOGICAL_OR) {
    uint64_t shortCircuitIndexStart = 0;
    ExpressionResult leftResult = generateExpression(binOp.leftSide);
    if (!leftResult.isReg) {
      // left side is immediate value
      if (!leftResult.val) {
        // for &&, expression is false, don't generate anymore
        if (binOp.op.type == TokenType::LOGICAL_AND) {
          return {.val = false};
        }
        // for ||, need to check next, but can remove left side
      } else {
        // for ||, expression is true, don't generate anymore
        if (binOp.op.type == TokenType::LOGICAL_OR) {
          return {.val = true};
        }
        // for &&, need to check next, but can remove left side
      }
    } else {
      // short-circuit logical ops
      shortCircuitIndexStart = byteCode.size();
      if (binOp.op.type == TokenType::LOGICAL_AND) {
        addBytes({{(bc)OpCode::SET_FLAGS, leftResult.reg}});
        addJumpMarker(JumpMarkerType::TO_LOGICAL_BIN_OP_END);
        addJumpOp(OpCode::RS_JUMP_E); // if leftResult is false, skip right side
      }
      else {
        addBytes({{(bc)OpCode::SET_FLAGS, leftResult.reg}});
        addJumpMarker(JumpMarkerType::TO_LOGICAL_BIN_OP_END);
        addJumpOp(OpCode::RS_JUMP_NE); // if leftResult is true, skip right side
      }
    }
    ExpressionResult rightResult = generateExpression(binOp.rightSide);
    if (!rightResult.isReg) {
      // right side is immediate value
      if (!leftResult.isReg) {
        // both sides are immediate
        return {.val = evaluateBooleanBinOp(binOp.op.type, leftResult.val, rightResult.val)};
      }
      // left result is not an immediate, clear out short circuit code
      byteCode.resize(shortCircuitIndexStart);
      if (!rightResult.val) {
        // for &&, cond is always false
        if (binOp.op.type == TokenType::LOGICAL_AND) {
          return {.val = false};
        }
        // for ||, depends on left side
      } else {
        // for ||, cond is always true
        if (binOp.op.type == TokenType::LOGICAL_AND) {
          return {.val = true};
        }
        // for &&, depends on left side
      }
      addBytes({{(bc)OpCode::SET_FLAGS, (bc)leftResult.val}});
    } else {
      if (!leftResult.isReg) {
        addBytes({{(bc)OpCode::SET_FLAGS, (bc)rightResult.val}});
      } else {
        addBytes({{(bc)op, leftResult.reg, rightResult.reg}});
        updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_LOGICAL_BIN_OP_END);
      }
    }
    ExpressionResult expRes;
    if (controlFlow) {
      expRes.jumpOp = jumpOp;
    } else {
      expRes.isReg = true;
      expRes.isTemp = true;
      const bc reg = allocateRegister();
      addBytes({{(bc)getOp, reg}});
      expRes.val = reg;
    }
    return expRes;
  }

  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  if (!leftResult.isReg) { // immediate value
    if (!rightResult.isReg) {
      return {.val = evaluateBooleanBinOp(binOp.op.type, leftResult.val, rightResult.val)};
    }
    const bc reg = allocateRegister();
    moveImmToReg(reg, leftResult.val);
    leftResult.isReg = true;
    leftResult.val = reg;
    leftResult.isTemp = true;
  } else if (!rightResult.isReg) {
    const bc reg = allocateRegister();
    moveImmToReg(reg, rightResult.val);
    rightResult.isReg = true;
    rightResult.val = reg;
    rightResult.isTemp = true;
  }
  addBytes({{(bc)op, leftResult.reg, rightResult.reg}});
  if (rightResult.isTemp) {
    freeRegister(rightResult.reg);
  }
  if (leftResult.isTemp) {
    freeRegister(leftResult.reg);
  }
  ExpressionResult expRes;
  if (controlFlow) {
    expRes.jumpOp = jumpOp;
  } else {
    expRes.isReg = true;
    expRes.isTemp = true;
    const bc reg = allocateRegister();
    addBytes({{(bc)getOp, reg}});
    expRes.val = reg;
  }
  return expRes;
}

ExpressionResult CodeGen::generateExpressionBinOp(const BinOp& binOp, bool controlFlow) {
  switch (binOp.op.type) {
    // member access
    case TokenType::DOT: {
      return {};
    }
    case TokenType::PTR_MEMBER_ACCESS: {
      return {};
    }

    // mathematical ops
    case TokenType::ADDITION: {
      return mathematicalBinOp(binOp, OpCode::ADD, OpCode::ADD_I);
    }
    case TokenType::SUBTRACTION: {
      return mathematicalBinOp(binOp, OpCode::SUB, OpCode::SUB_I);
    }
    case TokenType::MULTIPLICATION: {
      return mathematicalBinOp(binOp, OpCode::MUL, OpCode::MUL_I);
    }
    case TokenType::DIVISION: {
      return mathematicalBinOp(binOp, OpCode::DIV, OpCode::DIV_I);
    }
    case TokenType::MODULO: {
      return mathematicalBinOp(binOp, OpCode::MOD, OpCode::MOD_I);
    }
    case TokenType::BITWISE_OR: {
      return mathematicalBinOp(binOp, OpCode::OR, OpCode::OR_I);
    }
    case TokenType::BITWISE_AND: {
      return mathematicalBinOp(binOp, OpCode::AND, OpCode::AND_I);
    }
    case TokenType::BITWISE_XOR: {
      return mathematicalBinOp(binOp, OpCode::XOR, OpCode::XOR_I);
    }
    case TokenType::SHIFT_LEFT: {
      return mathematicalBinOp(binOp, OpCode::SHIFT_L, OpCode::SHIFT_L_I);
    }
    case TokenType::SHIFT_RIGHT: {
      return mathematicalBinOp(binOp, OpCode::SHIFT_R, OpCode::SHIFT_R_I);
    }

    // assignment ops
    case TokenType::ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::MOVE, OpCode::MOVE_I);
    }
    case TokenType::ADDITION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::ADD, OpCode::ADD_I);
    }
    case TokenType::SUBTRACTION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::SUB, OpCode::SUB_I);
    }
    case TokenType::MULTIPLICATION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::MUL, OpCode::MUL_I);
    }
    case TokenType::DIVISION_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::DIV, OpCode::DIV_I);
    }
    case TokenType::MODULO_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::MOD, OpCode::MOD_I);
    }
    case TokenType::BITWISE_OR_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::OR, OpCode::OR_I);
    }
    case TokenType::BITWISE_XOR_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::XOR, OpCode::XOR_I);
    }
    case TokenType::BITWISE_AND_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::AND, OpCode::AND_I);
    }
    case TokenType::SHIFT_LEFT_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::SHIFT_L, OpCode::SHIFT_L_I);
    }
    case TokenType::SHIFT_RIGHT_ASSIGNMENT: {
      return assignmentBinOp(binOp, OpCode::SHIFT_R, OpCode::SHIFT_R_I);
    }

    // figure out marker system to allow replacement of jump instruction values
    // logical
    case TokenType::EQUAL: {
      return booleanBinOp(binOp, OpCode::CMP, OpCode::RS_JUMP_NE, OpCode::GET_E, controlFlow);
    }
    case TokenType::NOT_EQUAL: {
      return booleanBinOp(binOp, OpCode::CMP, OpCode::RS_JUMP_E, OpCode::GET_NE, controlFlow);
    }
    case TokenType::LOGICAL_AND: {
      return booleanBinOp(binOp, OpCode::LOGICAL_AND, OpCode::RS_JUMP_E, OpCode::GET_NE, controlFlow);
    }
    case TokenType::LOGICAL_OR: {
      return booleanBinOp(binOp, OpCode::LOGICAL_OR, OpCode::RS_JUMP_E, OpCode::GET_NE, controlFlow);
    }
    case TokenType::LESS_THAN: {
      return booleanBinOp(binOp, OpCode::CMP, OpCode::RS_JUMP_GE, OpCode::GET_L, controlFlow);
    }
    case TokenType::LESS_THAN_EQUAL: {
      return booleanBinOp(binOp, OpCode::CMP, OpCode::RS_JUMP_G, OpCode::GET_LE, controlFlow);
    }
    case TokenType::GREATER_THAN: {
      return booleanBinOp(binOp, OpCode::CMP, OpCode::RS_JUMP_LE, OpCode::GET_G, controlFlow);
    }
    case TokenType::GREATER_THAN_EQUAL: {
      return booleanBinOp(binOp, OpCode::CMP, OpCode::RS_JUMP_L, OpCode::GET_GE, controlFlow);
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
        const bc reg = allocateRegister();
        moveImmToReg(reg, expRes.val);
        expRes.val = reg;
        expRes.isReg = true;
        expRes.isTemp = true;
      } else if (!expRes.isTemp) {
        const bc reg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, reg, expRes.reg}});
        expRes.val = reg;
        expRes.isReg = true;
        expRes.isTemp = true;
      }
      addBytes({{(bc)OpCode::NOT, expRes.reg}});
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

/**
 * Returns the size of a type
 * \param token the token of the type
 * \returns the size of the type
*/
uint32_t CodeGen::sizeOfType(const Token token) {
  switch (token.type) {
    case TokenType::BOOL: {
      return sizeof (bool);
    }
    case TokenType::CHAR_TYPE: {
      return sizeof (char);
    }
    case TokenType::STRING_TYPE: {
      return sizeof (char *);
    }
    case TokenType::INT8_TYPE:
    case TokenType::UINT8_TYPE: {
      return sizeof (uint8_t);
    }
    case TokenType::INT16_TYPE:
    case TokenType::UINT16_TYPE: {
      return sizeof (uint16_t);
    }
    case TokenType::INT32_TYPE:
    case TokenType::UINT32_TYPE: {
      return sizeof (uint32_t);
    }
    case TokenType::INT64_TYPE:
    case TokenType::UINT64_TYPE: {
      return sizeof (uint64_t);
    }
    case TokenType::POINTER: {
      return sizeof (void *);
    }
    case TokenType::DOUBLE_TYPE: {
      return sizeof (double);
    }
    case TokenType::VOID: {
      return 0;
    }
    case TokenType::REFERENCE: {
      return sizeof (void *);
    }
    case TokenType::IDENTIFIER: {
      return getStructInfo(tk->extractToken(token)).size;
    }
    default: {
      std::cerr << "Invalid TokenType ["<< (uint32_t)token.type << "] in CodeGen::sizeOfType\n";
      exit(1);
    }
  }
}


bool CodeGen::generateGeneralDeclaration(const GeneralDec& genDec) {
  switch(genDec.type) {
    case GeneralDecType::NONE: {
      break;
    }
    case GeneralDecType::STRUCT: {
      // generate the functions within, add a parameter for 'this'
      std::cerr << "Unsupported dec STRUCT\n";
      exit(1);
    }
    case GeneralDecType::VARIABLE: {
      return generateVariableDeclaration(*genDec.varDec);
    }
    case GeneralDecType::FUNCTION: {
      return generateFunctionDeclaration(*genDec.funcDec);
    }
    case GeneralDecType::ENUM: {
      // assign values for each item in the enum
      std::cerr << "Unsupported dec ENUM\n";
      exit(1);
    }
    case GeneralDecType::TEMPLATE: {
      // don't do anything
      std::cerr << "Unsupported dec TEMPLATE\n";
      exit(1);
    }
    case GeneralDecType::TEMPLATE_CREATE: {
      // generate whatever is in the template with the template type
      // might handle the copy during checker stage, in that case don't do anything
      std::cerr << "Unsupported dec TEMPLATE_CREATE\n";
      exit(1);
    }
    case GeneralDecType::INCLUDE_DEC: {
      // don't do anything, handled by parser
      break;
    }
  }
  return true;
}


/**
 * Have to figure out stack alignment
 * \returns the size of the type
*/
bool CodeGen::generateVariableDeclaration(const VariableDec& varDec, bool initialize) {
  bc reg = 0;
  const uint32_t preAddStackPointerOffset = getCurrStackPointerPosition();
  StackVariable& stackVar = addVarDecToVirtualStack(varDec);
  const uint32_t diff = stackVar.positionOnStack - preAddStackPointerOffset;
  const Token typeToken = getTypeFromTokenList(varDec.type);
  const uint32_t size = sizeOfType(typeToken);
  const uint32_t padding = diff - size;
  assert(padding < size);
  if (isBuiltInType(typeToken.type) || typeToken.type == TokenType::REFERENCE) {
    if (initialize && varDec.initialAssignment) {
      ExpressionResult expRes = generateExpression(*varDec.initialAssignment);
      if (!expRes.isReg) {
        reg = allocateRegister();
        moveImmToReg(reg, expRes.val);
      } else {
        reg = expRes.reg;
      }
      stackVar.reg = reg;
    }
    switch (size) {
      case 1: {
        if (reg) {
          addBytes({{(bc)OpCode::PUSH_B, reg}});
        } else {
          addBytes({{(bc)OpCode::DEC, stackPointerIndex}});
        }
        break;
      }
      case 2: {
        if (reg) {
          if (padding) {
            addBytes({{(bc)OpCode::DEC, stackPointerIndex}});
          }
          addBytes({{(bc)OpCode::PUSH_W, reg}});
        } else {
          alignForImm(2, 4);
          addBytes({{(bc)OpCode::SUB_I, stackPointerIndex}});
          add4ByteNum(size + padding);
        }
        break;
      }
      case 4: {
        if (reg) {
          // add padding to align
          if (padding) {
            alignForImm(2, 4);
            addBytes({{(bc)OpCode::SUB_I, stackPointerIndex}});
            add4ByteNum(padding);
          }
          // push value to stack
          addBytes({{(bc)OpCode::PUSH_D, reg}});
        } else {
          alignForImm(2, 4);
          addBytes({{(bc)OpCode::SUB_I, stackPointerIndex}});
          add4ByteNum(size + padding);
        }
        break;
      }
      case 8: {
        if (reg) {
          if (padding) {
            alignForImm(2, 4);
            addBytes({{(bc)OpCode::SUB_I, stackPointerIndex}});
            add4ByteNum(padding);
          }
          addBytes({{(bc)OpCode::PUSH_Q, reg}});
        } else {
          alignForImm(2, 4);
          addBytes({{(bc)OpCode::SUB_I, stackPointerIndex}});
          add4ByteNum(size + padding);
        }
        break;
      }
      default: {
        std::cerr << "Invalid Size [" << size << "] in generateVariableDeclaration\n";
        exit(1);
      }
    }
  }
  else if (typeToken.type == TokenType::IDENTIFIER) {
    alignForImm(2, 4);
    addBytes({{(bc)OpCode::SUB_I, stackPointerIndex}});
    add4ByteNum(size + padding);
    // generateVariableDeclarationStructType(varDec, initialize);
  }
  else {
    std::cerr << "Invalid TokenType [" << (uint32_t)typeToken.type << "] in generateVariableDeclaration\n";
    exit(1);
  }
  // adding to stack
  // uint32_t offset = size;
  // for (StackItem &item : stack) {
  //   if (item.type == StackItemType::MARKER) {
  //     if (item.marker == StackMarkerType::HARD_SCOPE_START) {
  //       break;
  //     }
  //   } else if (item.type == StackItemType::VARIABLE) {
  //     offset = item.variable.offset + size;
  //     break;
  //   }
  // }
  // StackItem stackItem {
  //   .variable = {
  //     .varDec = varDec,
  //     .offset = offset,
  //   },
  //   .type = StackItemType::VARIABLE,
  // };
  // stack.emplace_back(stackItem);
  return true;
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
    Token typeToken = getTypeFromTokenList(structDecList->varDec->type);
    uint32_t& offset = info.offsetMap[tk->extractToken(structDecList->varDec->name)];
    uint32_t size, alignTo;
    if (typeToken.type == TokenType::IDENTIFIER) {
      StructInformation& subStructInfo = getStructInfo(tk->extractToken(typeToken));
      size = subStructInfo.size;
      alignTo = subStructInfo.alignTo;
    }
    else if (isBuiltInType(typeToken.type) || typeToken.type == TokenType::REFERENCE) {
      size = sizeOfType(typeToken);
      alignTo = size;
    } else {
      std::cerr << "Invalid token type with enum value: " << (uint32_t)typeToken.type << '\n';
      exit(1);
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

/**
 * UNUSED
*/
void CodeGen::generateVariableDeclarationStructType(const VariableDec& varDec, bool initialize) {
  assert (false);
  std::string structName = tk->extractToken(varDec.type.token);
  GeneralDec const * const &generalDec = lookUp[structName];
  Tokenizer *oldTk = tk;
  tk = &tokenizers[generalDec->tokenizerIndex];
  StructDecList *structDecList = &generalDec->structDec->decs;
  while (structDecList) {
    if (structDecList->type == StructDecType::VAR) {
      generateVariableDeclaration(*structDecList->varDec, initialize);
    }
    structDecList = structDecList->next;
  }
  tk = oldTk;
}

uint64_t CodeGen::addJumpMarker(JumpMarkerType type) {
  jumpMarkers.emplace_back(byteCode.size(), type);
  return byteCode.size();
}

/**
 * Generate a branching statement
 * if return result is true,
 * adds a jump marker of type JumpMarkerType::TO_BRANCH_END that must be updated by the caller to land wherever needed
*/
BranchStatementResult CodeGen::generateBranchStatement(const BranchStatement& ifStatement) {
  ExpressionResult expRes = generateExpression(ifStatement.condition, true);
  if (expRes.jumpOp == OpCode::NOP) {
    if (!expRes.isReg) {
      // condition is a constant value, don't need to test
      if (expRes.val) {
        // condition always true
        generateScope(ifStatement.body);
        return BranchStatementResult::ALWAYS_TRUE;
      } // else condition always false, don't generate
      return BranchStatementResult::ALWAYS_FALSE;
    }
    addBytes({{(bc)OpCode::SET_FLAGS, expRes.reg}});
    expRes.jumpOp = OpCode::RS_JUMP_E;
  }
  addJumpMarker(JumpMarkerType::TO_BRANCH_END);
  addJumpOp(expRes.jumpOp);
  generateScope(ifStatement.body);
  return BranchStatementResult::ADDED_JUMP;
}

/**
 * Updates jump markers with destination. Sets type of each updated marker to none
 * \param destination the destination
 * \param type type of jump marker to update
 * \param until keep updating markers till this type is reached. if none, updates the first marker and stops
 * \param clear set the 'until' marker to type none if reached
*/
void CodeGen::updateJumpMarkersTo(const uint64_t destination, JumpMarkerType type, JumpMarkerType until, bool clear) {
  for (auto jumpMarker = jumpMarkers.rbegin(); jumpMarker != jumpMarkers.rend(); ++jumpMarker) {
    if (until != JumpMarkerType::NONE && jumpMarker->type == until) {
      if (clear) {
        jumpMarker->type = JumpMarkerType::NONE;
      }
      break;
    }
    if (jumpMarker->type == type) {
      jumpMarker->destination = destination;
      jumpMarker->type = JumpMarkerType::NONE;
      if (until == JumpMarkerType::NONE) {
        break;
      }
    }
  }
  // clean up
  // while (!jumpMarkers.empty() && jumpMarkers.back().type == JumpMarkerType::NONE) {
  //   jumpMarkers.pop_back();
  // }
}

void CodeGen::generateControlFlowStatement(const ControlFlowStatement& controlFlowStatement) {
  switch (controlFlowStatement.type) {
    case ControlFlowStatementType::FOR_LOOP: {
      generateStatement(controlFlowStatement.forLoop->initialize);
      const uint64_t startOfLoopIndex = addJumpMarker(JumpMarkerType::LOOP_START);
      const BranchStatementResult res = generateBranchStatement(controlFlowStatement.forLoop->statement);
      generateExpression(controlFlowStatement.forLoop->iteration);
      addJumpMarker(JumpMarkerType::TO_LOOP_START);
      addJumpOp(OpCode::RS_JUMP);
      if (res == BranchStatementResult::ADDED_JUMP) {
        updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END);
      }
      updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_LOOP_END, JumpMarkerType::LOOP_START);
      updateJumpMarkersTo(startOfLoopIndex, JumpMarkerType::TO_LOOP_START, JumpMarkerType::LOOP_START, true);
      break;
    }
    case ControlFlowStatementType::WHILE_LOOP: {
      const uint64_t startOfLoopIndex = addJumpMarker(JumpMarkerType::LOOP_START);
      BranchStatement& whileLoop = controlFlowStatement.whileLoop->statement;
      const BranchStatementResult res = generateBranchStatement(whileLoop);
      // place unconditional jump at the end of the while loop to go to start
      addJumpMarker(JumpMarkerType::TO_LOOP_START);
      addJumpOp(OpCode::RS_JUMP);
      if (res == BranchStatementResult::ADDED_JUMP) {
        updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END);
      }
      // update jump markers added by break / continue
      updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_LOOP_END, JumpMarkerType::LOOP_START);
      updateJumpMarkersTo(startOfLoopIndex, JumpMarkerType::TO_LOOP_START, JumpMarkerType::LOOP_START, true);
      break; 
    }
    case ControlFlowStatementType::CONDITIONAL_STATEMENT: {
      const BranchStatement& ifStatement = controlFlowStatement.conditional->ifStatement;
      const ElifStatementList *elifStatementList = controlFlowStatement.conditional->elifStatement;
      const Scope* elseStatement = controlFlowStatement.conditional->elseStatement;
      addJumpMarker(JumpMarkerType::IF_CHAIN_START); // mark start of this if/elif/else chain

      // if statement
      {
        const BranchStatementResult res = generateBranchStatement(ifStatement);
        if (elifStatementList || elseStatement) {
          // elif/else statements follow, add a jump after the this so that we skip the rest
          addJumpMarker(JumpMarkerType::TO_IF_CHAIN_END);
          addJumpOp(OpCode::RS_JUMP);
        }
        if (res == BranchStatementResult::ADDED_JUMP) {
          // update if false condition jump to go to code after the branch
          updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END);
        }
      }

      // elif statements
      while (elifStatementList) {
        const BranchStatementResult res = generateBranchStatement(elifStatementList->elif);
        elifStatementList = elifStatementList->next;
        if (elifStatementList || elseStatement) {
          // elif/else statements follow, add a jump after the this so that we skip the rest
          addJumpMarker(JumpMarkerType::TO_IF_CHAIN_END);
          addJumpOp(OpCode::RS_JUMP);
        }
        if (res == BranchStatementResult::ADDED_JUMP) {
          // update elif false condition jump to go to code after the branch
          updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END);
        }
      }

      // else statement
      if (elseStatement) {
        generateScope(*elseStatement);
      }

      // update all TO_IF_CHAIN_END jump ops until IF_CHAIN_START to go to current index
      updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_IF_CHAIN_END, JumpMarkerType::IF_CHAIN_START, true);
      break;
    }
    case ControlFlowStatementType::RETURN_STATEMENT: {

    }
    case ControlFlowStatementType::EXIT_STATEMENT: {
      ExpressionResult expRes = generateExpression(controlFlowStatement.returnStatement->returnValue);
      if (!expRes.isReg) {
        moveImmToReg(miscRegisterIndex, expRes.val);
        expRes.val = miscRegisterIndex;
      }
      addBytes({{(bc)OpCode::EXIT, expRes.reg}});
      break;
    }
    case ControlFlowStatementType::SWITCH_STATEMENT: {
      std::cerr << "ControlFlowStatementType::SWITCH_STATEMENT not implemented in CodeGen::generateControlFlowStatement\n";
      exit(1);
      break;
    }
    default: {
      std::cerr << "Invalid ControlFlowStatementType in CodeGen::generateControlFlowStatement\n";
      exit(1);
    }
  }
}

void CodeGen::generateScope(const Scope& scope) {
  startSoftScope();
  for (
    const StatementList *statementList = &scope.scopeStatements;
    statementList;
    statementList = statementList->next
  ) {
    generateStatement(statementList->curr);
  }
  endSoftScope();
}

bool CodeGen::generateFunctionDeclaration(const FunctionDec& funcDec) {
  // we want callee to handle unwinding arguments from stack
  startFunctionScope(funcDec);
  generateScope(funcDec.body);
  endFunctionScope(funcDec);
  return true;
}

void CodeGen::startSoftScope() {
  StackItem stackItem {
    .marker = StackMarkerType::SOFT_SCOPE_START,
    .type = StackItemType::MARKER
  };
  stack.emplace_back(stackItem);
}

void CodeGen::endSoftScope() {
  uint32_t stackUsage = 0;
  while (!stack.empty()) {
    if (
      stack.back().type == StackItemType::MARKER &&
      stack.back().marker == StackMarkerType::SOFT_SCOPE_START
    ) {
      stack.pop_back();
      break;
    }
    // TODO: run destructor for each item within this scope
    stackUsage += sizeOfType(getTypeFromTokenList(stack.back().variable.varDec.type));
    stack.pop_back();
    assert(!stack.empty()); // paired with marker comment above
  }
  if (stackUsage) {
    alignForImm(2, 4);
    addBytes({{(bc)OpCode::ADD_I, stackPointerIndex}});
    add4ByteNum(stackUsage);
  }
}

void CodeGen::startFunctionScope(const FunctionDec& funcDec) {
  StackItem scopeMarker {
    .marker = StackMarkerType::HARD_SCOPE_START,
    .type = StackItemType::MARKER
  };
  stack.emplace_back(scopeMarker);
  addFunctionSignatureToVirtualStack(funcDec);
}

void CodeGen::endFunctionScope(const FunctionDec& funcDec) {
  (void)funcDec;
  while (!stack.empty()) {
    // shouldn't need this marker as this *should* always be the first item in the stack
    // for now we'll leave it in
    if (
      stack.back().type == StackItemType::MARKER &&
      stack.back().marker == StackMarkerType::HARD_SCOPE_START
    ) {
      stack.pop_back();
      assert(stack.empty());
      break;
    }
    // TODO: run destructor for each item within this scope
    stack.pop_back();
    assert(!stack.empty()); // paired with marker comment above
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
        addJumpMarker(JumpMarkerType::TO_LOOP_END);
        addJumpOp(OpCode::RS_JUMP);
      }
      else if (statement.keyword.type == TokenType::CONTINUE) {
        addJumpMarker(JumpMarkerType::TO_LOOP_START);
        addJumpOp(OpCode::RS_JUMP);
      }
      else {
        std::cerr << "Invalid keyword type in CodeGen::generateStatement\n";
        exit(1);
      }
      break;
    }
  }
}

/**
 * Returns the actual type from a token list
 * Once type qualifiers or similar things are supported, this will be handy
*/
Token CodeGen::getTypeFromTokenList(const TokenList& tokenList) {
  return tokenList.token;
}

/**
 * 
 * \returns position of item
*/
uint32_t CodeGen::getPositionPushingItemToStack(Token typeToken, uint32_t alignTo) {
  uint32_t sizeOfParameter = 0;
  if (typeToken.type == TokenType::IDENTIFIER) {
    const StructInformation &structInfo = getStructInfo(tk->extractToken(typeToken));
    sizeOfParameter = structInfo.size;
    if (!alignTo) {
      alignTo = structInfo.alignTo;
    }
  } else {
    sizeOfParameter = sizeOfType(typeToken);
    if (!alignTo) {
      alignTo = sizeOfParameter;
    }
  }
  if (alignTo > sizeOfParameter) {
    sizeOfParameter = alignTo;
  }
  assert(alignTo);
  const uint32_t curr_sp_position = getCurrStackPointerPosition();
  const uint32_t mod = curr_sp_position % alignTo;
  const uint32_t paddingNeeded = mod != 0 ? alignTo - mod : 0;
  return curr_sp_position + paddingNeeded + sizeOfParameter;
}

StackVariable& CodeGen::addVarDecToVirtualStack(const VariableDec& varDec) {
  StackItem stackItem {
    .variable = {
      .varDec = varDec,
      .positionOnStack = getPositionPushingItemToStack(getTypeFromTokenList(varDec.type)),
    },
    .type = StackItemType::VARIABLE,
  };
  nameToStackItemIndex[tk->extractToken(varDec.name)] = stack.size();
  stack.emplace_back(stackItem);
  return stack.back().variable;
}

uint32_t CodeGen::getCurrStackPointerPosition() {
  auto iter = stack.rbegin();
  while (iter != stack.rend()) {
    if (iter->type == StackItemType::VARIABLE) {
      return iter->variable.positionOnStack;
    } else if (iter->type != StackItemType::MARKER) {
      return iter->positionOnStack;
    }
    ++iter;
  }
  return 0;
}

void CodeGen::makeRoomOnVirtualStack(Token token, uint32_t alignTo) {
  (void)token;
  (void)alignTo;
}

void CodeGen::addExpressionResToStack(const ExpressionResult& expRes) {
  (void)expRes;
}

/*
  * This doesn't actually need to be known before hand, maybe just add to stack of each function...
  * Naturally, generate each function independently
  * 
  * 
  * Memory layout information needs to be standardized before hand per function,
  * but generally, memory layout for a function call:
  *    HIGH ADDRESS                                              LOW ADDRESS
  *  Return Value | Argument 1 | Argument 2 | ... | Argument N | Return Address
  * 
  * - Caller needs to 8 byte align 'Return Value'
  * - Callee unwinds and destructs all variables after 'Return Value' on return
*/
void CodeGen::addFunctionSignatureToVirtualStack(const FunctionDec& funcDec) {
  // add return value, 8 byte aligned
  if (funcDec.returnType.token.type != TokenType::VOID) {
    StackItem returnValue {
      .positionOnStack = getPositionPushingItemToStack(getTypeFromTokenList(funcDec.returnType), 8),
      .type = StackItemType::RETURN_VALUE,
    };
    nameToStackItemIndex["-rv"] = stack.size();
    stack.emplace_back(returnValue);
  }

  // add parameters
  if (funcDec.params.curr.type != StatementType::NONE) {
    const StatementList *parameterList = &funcDec.params;
    do {
      addVarDecToVirtualStack(*parameterList->curr.varDec);
      parameterList = parameterList->next;
    } while (parameterList);
  }

  // add return address
  StackItem returnAddress {
    .positionOnStack = getPositionPushingItemToStack(Token{0, 0, TokenType::POINTER}),
    .type = StackItemType::RETURN_ADDRESS,
  };
  nameToStackItemIndex["-ra"] = stack.size();
  stack.emplace_back(returnAddress);
}
