#include <iostream>
#include <cassert>
#include <cstring>
#include <utility>
#include <concepts>
#include "codeGen.hpp"
#include "utils.hpp"

#define sp registers[stackPointerIndex]
#define ip registers[instructionPointerIndex]
#define dp registers[dataPointerIndex]
#define miscReg registers[miscRegisterIndex] // used for temporary/intermediate values

#define bc bytecode_t


void ExpressionResult::setData(const void *newData, uint8_t size) {
  assert(size <= sizeof(data));
  memmove(data, newData, size);
}

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


const DataSectionEntry& CodeGen::addToDataSection(DataSectionEntryType type, void *data, uint32_t n) {
  const uint64_t indexInDataSection = dataSection.size();
  dataSectionEntries.emplace_back(type, indexInDataSection);
  dataSection.resize(dataSection.size() + n);
  memcpy(&dataSection[indexInDataSection], data, n);
  return dataSectionEntries.back();
}


// ========================================
// ADDING TO BYTECODE
// ========================================

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
 * Moves any val into a register.
 * Updates exp by setting isReg and isTemp to true, as well as setting the register to the register parameter
*/
void CodeGen::moveImmToReg(const bytecode_t reg, ExpressionResult& exp) {
  assert(!exp.isReg);
  exp.isTemp = true;
  exp.isReg = true;
  TokenType rightSideExpType = exp.type->token.getType();
  if (rightSideExpType == TokenType::POINTER) {
    rightSideExpType = TokenType::UINT64_TYPE;
  }
  assert(
    rightSideExpType == TokenType::DOUBLE_TYPE ||
    rightSideExpType == TokenType::POINTER ||
    rightSideExpType == TokenType::UINT64_TYPE ||
    rightSideExpType == TokenType::INT64_TYPE ||
    rightSideExpType == TokenType::INT32_TYPE ||
    rightSideExpType == TokenType::UINT32_TYPE
  );
  bool large;
  if (rightSideExpType == TokenType::DOUBLE_TYPE) {
    large = true;
  }
  else if (rightSideExpType == TokenType::INT64_TYPE || rightSideExpType == TokenType::INT32_TYPE) {
    int64_t val;
    if (rightSideExpType == TokenType::INT32_TYPE) {
      val = *(int32_t*)exp.getData();
    } else {
      val = *(int64_t*)exp.getData();
    }
    if (val <= INT8_MAX && val >= INT8_MIN) {
      addBytes({{(bc)OpCode::MOVE_SI, reg, (bc)val}});
      exp.setReg(reg);
      return;
    }
    large = val > INT32_MAX || val < INT32_MIN;
  }
  else {
    uint64_t val;
    if (rightSideExpType == TokenType::UINT32_TYPE) {
      val = *(uint32_t*)exp.getData();
    } else {
      val = *(uint64_t*)exp.getData();
    }
    if (val <= UINT8_MAX) {
      addBytes({{(bc)OpCode::MOVE_SI, reg, (bc)val}});
      exp.setReg(reg);
      return;
    }
    large = val > UINT32_MAX;
  }
  if (large) {
    // have to use full 8 byte
    alignForImm(2, 8);
    addBytes({{(bc)OpCode::MOVE_LI, reg}});
    add8ByteNum(*(uint64_t *)exp.getData());
  } else {
    alignForImm(2, 4);
    addBytes({{(bc)OpCode::MOVE_I, reg}});
    add4ByteNum(*(uint32_t *)exp.getData());
  }
  exp.setReg(reg);
}


// ========================================
// JUMP MARKERS
// ========================================

uint64_t CodeGen::addJumpMarker(JumpMarkerType type) {
  jumpMarkers.emplace_back(byteCode.size(), type);
  return byteCode.size();
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


// ========================================
// DECLARATIONS
// ========================================

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
        moveImmToReg(reg, expRes);
      } else {
        reg = expRes.getReg();
      }
      stackVar.reg = reg;
    }
    assert(size >= 1 && size <= 8);
    if (!reg) {
      uint32_t spaceToAdd = size + padding;
      ExpressionResult spaceToAddExp;
      spaceToAddExp.setData(&spaceToAdd, sizeof(spaceToAdd));
      spaceToAddExp.type = &Checker::uint32Value;
      ExpressionResult stackPointerExp;
      stackPointerExp.setReg(stackPointerIndex);
      stackPointerExp.isReg = true;
      stackPointerExp.type = &Checker::uint64Value;
      expressionResWithOp(OpCode::SUB, OpCode::SUB_I, stackPointerExp, spaceToAddExp);
    } else {
      switch (size) {
        case 1: {
          addBytes({{(bc)OpCode::PUSH_B, reg}});
          break;
        }
        case 2: {
          if (padding) {
            addBytes({{(bc)OpCode::DEC, stackPointerIndex}});
          }
          addBytes({{(bc)OpCode::PUSH_W, reg}});
          break;
        }
        case 4: {
          // add padding to align
          if (padding) {
            ExpressionResult paddingToAdd;
            paddingToAdd.setData(&padding, sizeof(padding));
            paddingToAdd.type = &Checker::uint64Value;
            ExpressionResult stackPointerExp;
            stackPointerExp.setReg(stackPointerIndex);
            stackPointerExp.isReg = true;
            stackPointerExp.type = &Checker::uint64Value;
            expressionResWithOp(OpCode::SUB, OpCode::SUB_I, stackPointerExp, paddingToAdd);
          }
          // push value to stack
          addBytes({{(bc)OpCode::PUSH_D, reg}});
          break;
        }
        case 8: {
          if (padding) {
            ExpressionResult paddingToAdd;
            paddingToAdd.setData(&padding, sizeof(padding));
            paddingToAdd.type = &Checker::uint64Value;
            ExpressionResult stackPointerExp;
            stackPointerExp.setReg(stackPointerIndex);
            stackPointerExp.isReg = true;
            stackPointerExp.type = &Checker::uint64Value;
            expressionResWithOp(OpCode::SUB, OpCode::SUB_I, stackPointerExp, paddingToAdd);
          }
          addBytes({{(bc)OpCode::PUSH_Q, reg}});
          break;
        }
        default: {
          std::cerr << "Invalid Size [" << size << "] in generateVariableDeclaration\n";
          exit(1);
        }
      }
    }
  }
  else if (typeToken.type == TokenType::IDENTIFIER) {
    uint32_t spaceToAdd = size + padding;
    ExpressionResult spaceToAddExp;
    spaceToAddExp.setData(&spaceToAdd, sizeof(spaceToAdd));
    spaceToAddExp.type = &Checker::uint64Value;
    ExpressionResult stackPointerExp;
    stackPointerExp.setReg(stackPointerIndex);
    stackPointerExp.isReg = true;
    stackPointerExp.type = &Checker::uint64Value;
    expressionResWithOp(OpCode::SUB, OpCode::SUB_I, stackPointerExp, spaceToAddExp);
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


// ========================================
// COMPILE TIME EVALUATION
// ========================================

template< class T, class U >
struct OperatorAdd {
    constexpr auto operator()(T t, U u) noexcept {
        return t + u;
    }
};
template< class T, class U >
struct OperatorSub {
    constexpr auto operator()(T t, U u) noexcept {
        return t - u;
    }
};
template< class T, class U >
struct OperatorMul {
    constexpr auto operator()(T t, U u) noexcept {
        return t * u;
    }
};
template< class T, class U >
struct OperatorDiv {
    constexpr auto operator()(T t, U u) noexcept {
        return t / u;
    }
};
template< class T, class U >
struct OperatorModulo {
    constexpr auto operator()(T t, U u) noexcept {
        return t % u;
    }
};
template< class T, class U >
struct OperatorBitwiseOr {
    constexpr auto operator()(T t, U u) noexcept {
        return t | u;
    }
};
template< class T, class U >
struct OperatorBitwiseAnd {
    constexpr auto operator()(T t, U u) noexcept {
        return t & u;
    }
};
template< class T, class U >
struct OperatorBitwiseXor {
    constexpr auto operator()(T t, U u) noexcept {
        return t ^ u;
    }
};
template< class T, class U >
struct OperatorShiftLeft {
    constexpr auto operator()(T t, U u) noexcept {
        return t << u;
    }
};
template< class T, class U >
struct OperatorShiftRight {
    constexpr auto operator()(T t, U u) noexcept {
        return t >> u;
    }
};
template< class T, class U>
struct OperatorEqual {
    constexpr auto operator()(T t, U u) noexcept requires (std::integral<T> && std::integral<U>) {
        return std::cmp_equal(t, u);
    }
    constexpr auto operator()(T t, U u) noexcept {
        return t == u;
    }
};
template< class T, class U >
struct OperatorNotEqual {
    constexpr auto operator()(T t, U u) noexcept requires (std::integral<T> && std::integral<U>) {
        return std::cmp_not_equal(t, u);
    }
    constexpr auto operator()(T t, U u) noexcept {
        return t != u;
    }
};
template< class T, class U >
struct OperatorLogicalAnd {
    constexpr auto operator()(T t, U u) noexcept {
        return t && u;
    }
};
template< class T, class U >
struct OperatorLogicalOr {
    constexpr auto operator()(T t, U u) noexcept {
        return t || u;
    }
};
template< class T, class U >
struct OperatorGreater {
    constexpr auto operator()(T t, U u) noexcept requires (std::integral<T> && std::integral<U>) {
        return std::cmp_greater(t, u);
    }
    constexpr auto operator()(T t, U u) noexcept  {
        return t > u;
    }
};
template< class T, class U >
struct OperatorGreaterEqual {
    constexpr auto operator()(T t, U u) noexcept requires (std::integral<T> && std::integral<U>) {
        return std::cmp_greater_equal(t, u);
    }
    constexpr auto operator()(T t, U u) noexcept {
        return t >= u;
    }
};
template< class T, class U >
struct OperatorLess {
    constexpr auto operator()(T t, U u) noexcept requires (std::integral<T> && std::integral<U>) {
        return std::cmp_less(t, u);
    }
    constexpr auto operator()(T t, U u) noexcept {
        return t < u;
    }
};
template< class T, class U >
struct OperatorLessEqual {
    constexpr auto operator()(T t, U u) noexcept requires (std::integral<T> && std::integral<U>) {
        return std::cmp_less_equal(t, u);
    }
    constexpr auto operator()(T t, U u) noexcept {
        return t <= u;
    }
};

template<template<typename, typename> class TFunctor>
void doBinaryEvaluate(TokenType leftSideType, TokenType rightSideType, const ExpressionResult& left, const ExpressionResult& right, ExpressionResult& res) {
  assert(leftSideType == TokenType::DOUBLE_TYPE || leftSideType == TokenType::UINT64_TYPE || leftSideType == TokenType::INT64_TYPE);
  assert(rightSideType == TokenType::DOUBLE_TYPE || rightSideType == TokenType::UINT64_TYPE || rightSideType == TokenType::INT64_TYPE);
  if (leftSideType == TokenType::DOUBLE_TYPE) {
    if (rightSideType == TokenType::DOUBLE_TYPE) {
      auto temp = TFunctor<double, double>()(*(double*)left.getData(), *(double*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::UINT64_TYPE) {
      auto temp = TFunctor<double, uint64_t>()(*(double*)left.getData(), *(uint64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::INT64_TYPE) {
      auto temp = TFunctor<double, int64_t>()(*(double*)left.getData(), *(int64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    }
  } else if (leftSideType == TokenType::UINT64_TYPE) {
    if (rightSideType == TokenType::DOUBLE_TYPE) {
      auto temp = TFunctor<uint64_t, double>()(*(uint64_t*)left.getData(), *(double*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::UINT64_TYPE) {
      auto temp = TFunctor<uint64_t, uint64_t>()(*(uint64_t*)left.getData(), *(uint64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::INT64_TYPE) {
      auto temp = TFunctor<uint64_t, int64_t>()(*(uint64_t*)left.getData(), *(int64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    }
  } else if (leftSideType == TokenType::INT64_TYPE) {
    if (rightSideType == TokenType::DOUBLE_TYPE) {
      auto temp = TFunctor<int64_t, double>()(*(int64_t*)left.getData(), *(double*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::UINT64_TYPE) {
      auto temp = TFunctor<int64_t, uint64_t>()(*(int64_t*)left.getData(), *(uint64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::INT64_TYPE) {
      auto temp = TFunctor<int64_t, int64_t>()(*(int64_t*)left.getData(), *(int64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    }
  }
}

template<template<typename, typename> class TFunctor>
void doBinaryIntegralEvaluate(TokenType leftSideType, TokenType rightSideType, const ExpressionResult& left, const ExpressionResult& right, ExpressionResult& res) {
  assert(leftSideType == TokenType::UINT64_TYPE || leftSideType == TokenType::INT64_TYPE);
  assert(leftSideType == TokenType::UINT64_TYPE || leftSideType == TokenType::INT64_TYPE);
  if (leftSideType == TokenType::UINT64_TYPE) {
    if (rightSideType == TokenType::UINT64_TYPE) {
      auto temp = TFunctor<uint64_t, uint64_t>()(*(uint64_t*)left.getData(), *(uint64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::INT64_TYPE) {
      auto temp = TFunctor<uint64_t, int64_t>()(*(uint64_t*)left.getData(), *(int64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    }
  } else if (leftSideType == TokenType::INT64_TYPE) {
    if (rightSideType == TokenType::UINT64_TYPE) {
      auto temp = TFunctor<int64_t, uint64_t>()(*(int64_t*)left.getData(), *(uint64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    } else if (rightSideType == TokenType::INT64_TYPE) {
      auto temp = TFunctor<int64_t, int64_t>()(*(int64_t*)left.getData(), *(int64_t*)right.getData());
      res.setData(&temp, sizeof(temp));
    }
  }
}

ExpressionResult evaluateBinOpImmExpression(TokenType op, ExpressionResult& left, ExpressionResult& right) {
  assert(!left.isReg && !right.isReg);
  assert(left.type && right.type);
  ExpressionResult res;
  TokenType leftSideType = left.type->token.getType();
  TokenType rightSideType = right.type->token.getType();
  assert(isBuiltInType(leftSideType) && leftSideType != TokenType::VOID && leftSideType != TokenType::STRING_TYPE);
  assert(isBuiltInType(rightSideType) && rightSideType != TokenType::VOID && rightSideType != TokenType::STRING_TYPE);
  
  // assign to largest type, minimum of int32
  res.type = &Checker::int32Value;
  if (leftSideType > res.type->token.getType()) {
    res.type = left.type;
  }
  if (rightSideType > res.type->token.getType()) {
    res.type = right.type;
  }

  if (isUnsigned(leftSideType) || leftSideType == TokenType::BOOL) {
    // temporarily mark as uint64_t
    leftSideType = TokenType::UINT64_TYPE;
  } else if (isSigned(leftSideType)) {
    // temporarily mark as int64_t
    // sign extend
    int64_t signExtended = 0;
    switch (leftSideType) {
      case TokenType::CHAR_TYPE:
      case TokenType::INT8_TYPE: {
        signExtended = *(int8_t*)left.getData();
        break;
      }
      case TokenType::INT16_TYPE: {
        signExtended = *(int16_t*)left.getData();
        break;
      }
      case TokenType::INT32_TYPE: {
        signExtended = *(int32_t*)left.getData();
        break;
      }
      case TokenType::INT64_TYPE: break;
      default: {
        exit(1);
      }
    }
    if (signExtended) {
      left.setData(&signExtended, sizeof(signExtended));
    }
    leftSideType = TokenType::INT64_TYPE;
  }
  if (isUnsigned(rightSideType) || rightSideType == TokenType::BOOL) {
    // temporarily mark as uint64_t
    rightSideType = TokenType::UINT64_TYPE;
  } else if (isSigned(rightSideType)) {
    // temporarily mark as int64_t
    // sign extend
    int64_t signExtended = 0;
    switch (rightSideType) {
      case TokenType::CHAR_TYPE:
      case TokenType::INT8_TYPE: {
        signExtended = *(int8_t*)right.getData();
        break;
      }
      case TokenType::INT16_TYPE: {
        signExtended = *(int16_t*)right.getData();
        break;
      }
      case TokenType::INT32_TYPE: {
        signExtended = *(int32_t*)right.getData();
        break;
      }
      case TokenType::INT64_TYPE: break;
      default: {
        exit(1);
      }
    }
    if (signExtended) {
      right.setData(&signExtended, sizeof(signExtended));
    }
    rightSideType = TokenType::INT64_TYPE;
  }
  switch (op) {
    case TokenType::ADDITION: {
      doBinaryEvaluate<OperatorAdd>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::SUBTRACTION: {
      doBinaryEvaluate<OperatorSub>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::MULTIPLICATION: {
      doBinaryEvaluate<OperatorMul>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::DIVISION: {
      doBinaryEvaluate<OperatorDiv>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::MODULO: {
      doBinaryIntegralEvaluate<OperatorModulo>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::BITWISE_OR: {
      doBinaryIntegralEvaluate<OperatorBitwiseOr>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::BITWISE_AND: {
      doBinaryIntegralEvaluate<OperatorBitwiseAnd>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::BITWISE_XOR: {
      doBinaryIntegralEvaluate<OperatorBitwiseXor>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::SHIFT_LEFT: {
      doBinaryIntegralEvaluate<OperatorShiftLeft>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::SHIFT_RIGHT: {
      doBinaryIntegralEvaluate<OperatorShiftRight>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::EQUAL: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorEqual>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::NOT_EQUAL: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorNotEqual>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::LOGICAL_AND: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorLogicalAnd>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::LOGICAL_OR: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorLogicalOr>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::LESS_THAN: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorLess>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::LESS_THAN_EQUAL: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorLessEqual>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::GREATER_THAN: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorGreater>(leftSideType, rightSideType, left, right, res);
      break;
    }
    case TokenType::GREATER_THAN_EQUAL: {
      res.type = &Checker::boolValue;
      doBinaryEvaluate<OperatorGreaterEqual>(leftSideType, rightSideType, left, right, res);
      break;
    }
    default: {
      std::cerr << "Invalid TokenType in evaluateExpression [" << (uint32_t)op  << "]\n";
      exit(1);
    }
  }
  return res;
}


template< class T >
struct OperatorNegate {
    constexpr auto operator()(T t) noexcept {
      return -t;
    }
};

template< class T >
struct OperatorNot {
    constexpr auto operator()(T t) noexcept {
      return !t;
    }
};

template<template<typename> class TFunctor>
void doUnaryEvaluate(TokenType operandType, const ExpressionResult& operand, ExpressionResult& res)
{
  assert(
    operandType == TokenType::DOUBLE_TYPE ||
    operandType == TokenType::UINT64_TYPE ||
    operandType == TokenType::INT64_TYPE
  );
  if (operandType == TokenType::DOUBLE_TYPE) {
    auto unaryOpValue = TFunctor<double>()(*(double*)operand.getData());
    res.setData(&unaryOpValue, sizeof(unaryOpValue));
  } else if (operandType == TokenType::UINT64_TYPE) {
    auto unaryOpValue = TFunctor<uint64_t>()(*(uint64_t*)operand.getData());
    res.setData(&unaryOpValue, sizeof(unaryOpValue));
  } else if (operandType == TokenType::INT64_TYPE) {
    auto unaryOpValue = TFunctor<int64_t>()(*(int64_t*)operand.getData());
    res.setData(&unaryOpValue, sizeof(unaryOpValue));
  }
}

ExpressionResult evaluateUnaryOpImmExpression(TokenType op, ExpressionResult& operand) {
  assert(!operand.isReg && operand.type);
  ExpressionResult res;
  TokenType operandType = operand.type->token.getType();
  assert(isBuiltInType(operandType) && operandType != TokenType::VOID && operandType != TokenType::STRING_TYPE);
  
  // assign to largest type, minimum of int32
  res.type = &Checker::int32Value;
  if (operandType > res.type->token.getType()) {
    res.type = operand.type;
  }
  if (isUnsigned(operandType) || operandType == TokenType::BOOL) {
    // temporarily mark as uint64_t
    operandType = TokenType::UINT64_TYPE;
  } else if (isSigned(operandType)) {
    // temporarily mark as int64_t
    // sign extend
    int64_t signExtended = 0;
    switch (operandType) {
      case TokenType::CHAR_TYPE:
      case TokenType::INT8_TYPE: {
        signExtended = *(int8_t*)operand.getData();
        break;
      }
      case TokenType::INT16_TYPE: {
        signExtended = *(int16_t*)operand.getData();
        break;
      }
      case TokenType::INT32_TYPE: {
        signExtended = *(int32_t*)operand.getData();
        break;
      }
      case TokenType::INT64_TYPE: break;
      default: {
        exit(1);
      }
    }
    if (signExtended) {
      operand.setData(&signExtended, sizeof(signExtended));
    }
    operandType = TokenType::INT64_TYPE;
  }
  switch (op) {
    case TokenType::NOT: {
      res.type = &Checker::boolValue;
      doUnaryEvaluate<OperatorNot>(operandType, operand, res);
      break;
    }
    case TokenType::NEGATIVE: {
      doUnaryEvaluate<OperatorNegate>(operandType, operand, res);
      break;
    }
    default: {
      std::cerr << "Invalid TokenType in evaluateExpression [" << (uint32_t)op << "]\n";
      exit(1);
    }
  }
  return res;
}


// ========================================
// GENERAL EXPRESSIONS
// ========================================

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


/**
*/
ExpressionResult CodeGen::expressionResWithOp(OpCode op, OpCode immOp, const ExpressionResult& left, const ExpressionResult& right) {
  assert(left.isReg);
  ExpressionResult expRes;
  const bytecode_t resReg = left.getReg();
  if (right.isReg) {
    assert(op != OpCode::NOP);
    addBytes({{(bc)op, resReg, right.getReg()}});
    return expRes;
  }
  assert(immOp != OpCode::NOP);
  bool large;
  TokenType rightSideExpType = right.type->token.getType();
  assert(isBuiltInType(rightSideExpType));
  // need to covert types to their highest level (uint64_t, int64_t, or double)

  assert(
    rightSideExpType == TokenType::UINT64_TYPE ||
    rightSideExpType == TokenType::INT64_TYPE ||
    rightSideExpType == TokenType::INT32_TYPE ||
    rightSideExpType == TokenType::UINT32_TYPE
  );
  if (rightSideExpType == TokenType::INT64_TYPE || rightSideExpType == TokenType::INT32_TYPE) {
    int64_t val;
    if (rightSideExpType == TokenType::INT32_TYPE) {
      val = *(int32_t*)right.getData();
    } else {
      val = *(int64_t*)right.getData();
    }
    if (val <= INT16_MAX && val >= INT16_MIN) {
      alignForImm(2, 2);
      addBytes({{(bc)immOp, resReg}});
      add2ByteNum((int16_t)val);
      return expRes;
    }
    large = val > INT32_MAX || val < INT32_MIN;
  } else {
    uint64_t val;
    if (rightSideExpType == TokenType::UINT32_TYPE) {
      val = *(uint32_t*)right.getData();
    } else {
      val = *(uint64_t*)right.getData();
    }
    if (val <= UINT16_MAX) {
      alignForImm(2, 2);
      addBytes({{(bc)immOp, resReg}});
      add2ByteNum((uint16_t)val);
      return expRes;
    }
    large = val > UINT32_MAX;
  }
  if (large) {
    // have to use full 8 byte
    alignForImm(2, 8);
    addBytes({{(bc)OpCode::MOVE_LI, miscRegisterIndex}});
    add8ByteNum(*(uint64_t *)right.getData());
  } else {
    alignForImm(2, 4);
    addBytes({{(bc)OpCode::MOVE_I, miscRegisterIndex}});
    add4ByteNum(*(uint32_t *)right.getData());
  }
  assert(op != OpCode::NOP);
  addBytes({{(bc)op, resReg, miscRegisterIndex}});
  return expRes;
}

/**
 * Generates the expression and returns the address of the result.
 * the expression must be of a type that has a resulting address.
 * Valid expression types: value (value must be an identifier, which is a valid variable), array access, binary op member access, unary op dereference
 * \returns an ExpressionResult object. isTemp is true if isReg is true.
 * if isReg is set to false, it means there is something wrong with the expression. this should have been caught in the checker stage
*/
ExpressionResult CodeGen::getAddressOfExpression(const Expression& expression) {
  ExpressionResult expRes;
  switch (expression.getType()) {
    case ExpressionType::BINARY_OP: {
      // if binary op is not a member access, cannot get address of the result of a binary op
      BinOp* binOp = expression.getBinOp();
      TokenType op = binOp->op.getType();
      assert(op == TokenType::PTR_MEMBER_ACCESS || op == TokenType::DOT);
      expRes = getAddressOfExpression(binOp->leftSide);
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

      if (op == TokenType::PTR_MEMBER_ACCESS) {
        expRes.type = expRes.type->next;
      }
      const StructInformation& structInfo = structNameToInfoMap[tk->extractToken(expRes.type->token)];
      assert(binOp->rightSide.getType() == ExpressionType::VALUE);
      const std::string& memberName = tk->extractToken(binOp->rightSide.getToken());
      const uint32_t offsetInStruct = structInfo.offsetMap.at(memberName);
      if (offsetInStruct) {
        ExpressionResult offsetExpression;
        offsetExpression.type = &Checker::uint32Value;
        offsetExpression.setData(&offsetInStruct, sizeof(offsetInStruct));
        expressionResWithOp(OpCode::ADD, OpCode::ADD_I, expRes, offsetExpression);
      }
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
      expRes.setReg(allocateRegister());
      addBytes({{(bc)OpCode::MOVE, expRes.getReg(), stackPointerIndex}});
      const StackVariable& stackVar = stack[stackItemIndex].variable;
      uint32_t offset = getVarOffsetFromSP(stackVar);
      ExpressionResult varOffsetExp;
      varOffsetExp.setData(&offset, sizeof(offset));
      varOffsetExp.type = &Checker::uint32Value;
      expressionResWithOp(OpCode::ADD, OpCode::ADD_I, expRes, varOffsetExp);
      expRes.type = &stackVar.varDec.type;
      if (expRes.type->token.getType() == TokenType::REFERENCE) {
        // always move past references
        expRes.type = expRes.type->next;
      }
      expRes.isReg = true;
      expRes.isTemp = true;
      return expRes;
    }
    case ExpressionType::ARRAY_ACCESS: {
      // need the value from expression.getArrayAccess()->offset
      // then add that to expression.getArrayAccess()->array
      ExpressionResult offset = generateExpression(expression.getArrayAccess()->offset);
      ExpressionResult array = getAddressOfExpression(expression.getArrayAccess()->array);
      // have to multiple offset by size of array type
      assert(array.isReg); // 
      (void)offset;
      expRes.isReg = true;
      expRes.isTemp = true;
      expRes.setReg(array.getReg());
      return expRes;
    }
    default: {
      // invalid type
      assert(false);
      return expRes;
    }
  }

}


ExpressionResult CodeGen::loadValue(const Token &token) {
  ExpressionResult expRes;
  switch (token.type) {
    case TokenType::CHAR_LITERAL: {
      std::string charLiteral = tk->extractToken(token);
      // convert charLiteral to its numeric value and return it
      // might do this during the tokenizer stage...
      if (charLiteral.size() == 3) {
        expRes.setData(&charLiteral[1], 1);
        return expRes;
      } else if (charLiteral.size() == 4) {
        if (charLiteral[2] >= '0' && charLiteral[2] <= '9') {
          charLiteral[2] -= '0';
          expRes.setData(&charLiteral[2], 1);
          return expRes;
        } else if (charLiteral[2] == 'n') {
          charLiteral[2] = '\n';
          expRes.setData(&charLiteral[2], 1);
          return expRes;
        } else if (charLiteral[2] == '"') {
          charLiteral[2] = '\"';
          expRes.setData(&charLiteral[2], 1);
          return expRes;
        } else if (charLiteral[2] == '\\') {
          expRes.setData(&charLiteral[2], 1);
          return expRes;
        } else if (charLiteral[2] == '\'') {
          expRes.setData(&charLiteral[2], 1);
          return expRes;
        }
      }
      assert(false);
      return expRes;
    }
    case TokenType::STRING_LITERAL: { 
      // place string literal in data section of bytecode, return offset to start of string, have to replace escaped characters
      // check if string is already in data section
      expRes.type = &Checker::stringValue;
      std::string stringLiteral = tk->extractToken(token);
      for (uint32_t i = 0; i < dataSectionEntries.size(); ++i) {
        if (dataSectionEntries[i].type != DataSectionEntryType::STRING_LITERAL) {
          continue;
        }
        const char *pString = (const char *)dataSection.data() + dataSectionEntries[i].indexInDataSection;
        uint64_t stringLength = strlen(pString);
        if (stringLiteral.length() == stringLength && stringLiteral == pString) {
          expRes.setData(&i, sizeof(i));
          return expRes;
        }
        i += stringLength;
      }
      const DataSectionEntry& dataSectionEntry = addToDataSection(DataSectionEntryType::STRING_LITERAL, stringLiteral.data(), stringLiteral.length() + 1);
      if (dataSectionEntry.indexInDataSection) {
        ExpressionResult stringExp;
        stringExp.setReg(allocateRegister());
        stringExp.isReg = true;
        stringExp.isTemp = true;
        ExpressionResult dataPointerExp;
        dataPointerExp.setReg(dataPointerIndex);
        dataPointerExp.isReg = true;
        expRes = expressionResWithOp(OpCode::MOVE, OpCode::NOP, stringExp, dataPointerExp);
        ExpressionResult offsetExp;
        offsetExp.setData(&dataSectionEntry.indexInDataSection, sizeof(dataSectionEntry.indexInDataSection));
        offsetExp.type = &Checker::uint32Value;
        expressionResWithOp(OpCode::ADD, OpCode::ADD_I, expRes, offsetExp);
      }
      expRes.setReg(dataPointerIndex);
      expRes.isReg = true;
      expRes.isTemp = true;
      return expRes;
    }
    case TokenType::DECIMAL_NUMBER: {
      std::string decimalNumber = tk->extractToken(token);
      uint64_t num = std::stoull(decimalNumber);
      expRes.setData(&num, sizeof(num));
      if (num <= INT32_MAX) {
        expRes.type = &Checker::int32Value;
      } else if (num <= UINT32_MAX) {
        expRes.type = &Checker::uint32Value;
      } else if (num <= INT64_MAX) {
        expRes.type = &Checker::int64Value;
      } else {
        expRes.type = &Checker::uint64Value;
      }
      return expRes;
    }
    case TokenType::BINARY_NUMBER: {
      assert(token.length > 2);
      std::string binaryNumber = tk->extractToken(Token{token.position + 2, (uint16_t)(token.length - 2), TokenType::BINARY_NUMBER});
      uint64_t num = std::stoull(binaryNumber, nullptr, 2);
      expRes.setData(&num, sizeof(num));
      if (num <= INT32_MAX) {
        expRes.type = &Checker::int32Value;
      } else if (num <= UINT32_MAX) {
        expRes.type = &Checker::uint32Value;
      } else if (num <= INT64_MAX) {
        expRes.type = &Checker::int64Value;
      } else {
        expRes.type = &Checker::uint64Value;
      }
      return expRes;
    }
    case TokenType::FLOAT_NUMBER: {
      std::string binaryNumber = tk->extractToken(token);
      double num = std::stod(binaryNumber);
      expRes.setData(&num, sizeof(num));
      expRes.type = &Checker::doubleValue;
      return expRes;
    }
    case TokenType::HEX_NUMBER: { 
      assert(token.length > 2);
      std::string hexNumber = tk->extractToken(Token{token.position + 2, (uint16_t)(token.length - 2), TokenType::HEX_NUMBER});
      /*
      std::invalid_argument
      std::out_of_range
      */
      uint64_t num = std::stoull(hexNumber, nullptr, 16);
      expRes.setData(&num, sizeof(num));
      if (num <= INT32_MAX) {
        expRes.type = &Checker::int32Value;
      } else if (num <= UINT32_MAX) {
        expRes.type = &Checker::uint32Value;
      } else if (num <= INT64_MAX) {
        expRes.type = &Checker::int64Value;
      } else {
        expRes.type = &Checker::uint64Value;
      }
      return expRes;
    }
    case TokenType::FALSE: {
      expRes.type = &Checker::boolValue;
      return expRes;
    }
    case TokenType::TRUE: {
      uint8_t trueValue = 1;
      expRes.setData(&trueValue, sizeof(trueValue));
      expRes.type = &Checker::boolValue;
      return expRes;
    }
    case TokenType::NULL_PTR: {
      expRes.type = &Checker::ptrValue;
      return expRes;
    }
    case TokenType::IDENTIFIER: {
      std::string identifier = tk->extractToken(token);
      uint32_t stackItemIndex = nameToStackItemIndex[identifier];
      StackVariable &variableInfo = stack[stackItemIndex].variable;
      expRes.type = &variableInfo.varDec.type;
      if (variableInfo.reg) {
        expRes.setReg(variableInfo.reg);
        expRes.isReg = true;
        return expRes;
      }
      const uint32_t varOffset = getVarOffsetFromSP(variableInfo);
      const uint32_t sizeOfVar = sizeOfType(getTypeFromTokenList(variableInfo.varDec.type));
      if (sizeOfVar > SIZE_OF_REGISTER) {
        // too large to fit into a reg, just return offset
        // TODO: figure out what to make the type in this scenario, maybe need another flag in exp res
        expRes.setData(&varOffset, sizeof(varOffset));
        return expRes;
      }
      // load value into a register
      variableInfo.reg = allocateRegister();
      OpCode loadOp = getLoadOpForSize(sizeOfVar);
      if (varOffset) {
        addBytes({{(bc)OpCode::MOVE, miscRegisterIndex, stackPointerIndex}});
        ExpressionResult varOffsetExp;
        varOffsetExp.setData(&varOffset, sizeof(varOffset));
        varOffsetExp.type = &Checker::uint64Value;
        ExpressionResult miscRegExp;
        miscRegExp.setReg(miscRegisterIndex);
        miscRegExp.isReg = true;
        varOffsetExp.type = &Checker::uint64Value;
        expressionResWithOp(OpCode::ADD, OpCode::ADD_I, miscRegExp, varOffsetExp);
        addBytes({{(bc)loadOp, variableInfo.reg, miscRegisterIndex}});
      } else {
        addBytes({{(bc)loadOp, variableInfo.reg, stackPointerIndex}});
      }
      expRes.isReg = true;
      expRes.setData(&variableInfo.reg, sizeof(variableInfo.reg));
      return expRes;
    }
    default: {
      assert(false);
      return expRes;
    }
  }
}


// ========================================
// BINARY EXPRESSIONS
// ========================================

bool isCommutative(OpCode op) {
  return !(
    op == OpCode::SUB ||
    op == OpCode::DIV ||
    op == OpCode::F_SUB ||
    op == OpCode::F_DIV
  );
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
    return evaluateBinOpImmExpression(binOp.op.getType(), leftResult, rightResult);
  }
  // beyond this point, only one of left or right can possibly be an imm

  // left temp
  if (leftResult.isTemp) {
    // right imm
    if (!rightResult.isReg) {
      return expressionResWithOp(op, opImm, leftResult, rightResult);
    }
    // right temp / var
    else {
      if (rightResult.isTemp) {
        freeRegister(rightResult.getReg());
      }
      return expressionResWithOp(op, opImm, leftResult, rightResult);
    }
  }
  // left imm
  if (!leftResult.isReg) {
    // right is a reg
    assert(rightResult.isReg);
    // right temp
    if (rightResult.isTemp) {
      if (isCommutative(op)) {
        return expressionResWithOp(op, opImm, rightResult, leftResult);
      }
      // cannot flip args
      moveImmToReg(allocateRegister(), leftResult);
      freeRegister(rightResult.getReg());
      return expressionResWithOp(op, opImm, leftResult, rightResult);
    }
    // right var
    moveImmToReg(allocateRegister(), leftResult);
    return expressionResWithOp(op, opImm, leftResult, rightResult);
  }
  // left var
  {
    // right temp
    if (rightResult.isReg && rightResult.isTemp) {
      if (isCommutative(op)) {
        return expressionResWithOp(op, opImm, rightResult, leftResult);
      }
      bc reg = allocateRegister();
      addBytes({{(bc)OpCode::MOVE, reg, leftResult.getReg()}});
      leftResult.setReg(reg);
      leftResult.isTemp = true;
      freeRegister(rightResult.getReg());
      return expressionResWithOp(op, opImm, leftResult, rightResult);
    }
    // right var / imm
    bc reg = allocateRegister();
    addBytes({{(bc)OpCode::MOVE, reg, leftResult.getReg()}});
    leftResult.setReg(reg);
    leftResult.isTemp = true;
    return expressionResWithOp(op, opImm, leftResult, rightResult);
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
  registers[leftResult.getReg()].changed = true;
  expressionResWithOp(op, opImm, leftResult, rightResult);
  if (rightResult.isTemp){
    freeRegister(rightResult.getReg());
  }
  // write new value to stack
  // have to know where leftResult is. it could be on the stack or heap
  // it could be an element in an array and/or an element in a struct
  // it could have already been calculated before and in a register
  // need a function to get that info based on an expression
  return leftResult;
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
      // essentially run !leftResult.value
      ExpressionResult zeroExp;
      zeroExp.type = &Checker::int32Value;
      leftResult = evaluateBinOpImmExpression(TokenType::NOT_EQUAL, leftResult, zeroExp);
      if (!*(bool*)leftResult.getData()) {
        // for &&, expression is false, don't generate anymore
        if (binOp.op.type == TokenType::LOGICAL_AND) {
          return leftResult;
        }
        // for ||, need to check next, but can remove left side
      } else {
        // for ||, expression is true, don't generate anymore
        if (binOp.op.type == TokenType::LOGICAL_OR) {
          return leftResult;
        }
        // for &&, need to check next, but can remove left side
      }
    } else {
      // short-circuit logical ops
      shortCircuitIndexStart = byteCode.size();
      if (binOp.op.type == TokenType::LOGICAL_AND) {
        addBytes({{(bc)OpCode::SET_FLAGS, leftResult.getReg()}});
        addJumpMarker(JumpMarkerType::TO_LOGICAL_BIN_OP_END);
        addJumpOp(OpCode::RS_JUMP_E); // if leftResult is false, skip right side
      }
      else {
        addBytes({{(bc)OpCode::SET_FLAGS, leftResult.getReg()}});
        addJumpMarker(JumpMarkerType::TO_LOGICAL_BIN_OP_END);
        addJumpOp(OpCode::RS_JUMP_NE); // if leftResult is true, skip right side
      }
    }
    ExpressionResult rightResult = generateExpression(binOp.rightSide);
    if (!rightResult.isReg) {
      // right side is immediate value
      if (!leftResult.isReg) {
        // both sides are immediate
        return evaluateBinOpImmExpression(binOp.op.type, leftResult, rightResult);
      }
      // left result is not an immediate, clear out short circuit code
      byteCode.resize(shortCircuitIndexStart);
      ExpressionResult zeroExp;
      zeroExp.type = &Checker::int32Value;
      rightResult = evaluateBinOpImmExpression(TokenType::NOT_EQUAL, rightResult, zeroExp);
      // have to use type to check this
      if (!*(bool*)rightResult.getData()) {
        // for &&, cond is always false
        if (binOp.op.type == TokenType::LOGICAL_AND) {
          return rightResult;
        }
        // for ||, depends on left side
      } else {
        // for ||, cond is always true
        if (binOp.op.type == TokenType::LOGICAL_AND) {
          return rightResult;
        }
        // for &&, depends on left side
      }
      addBytes({{(bc)OpCode::SET_FLAGS, leftResult.getReg()}});
    } else {
      if (!leftResult.isReg) {
        addBytes({{(bc)OpCode::SET_FLAGS, rightResult.getReg()}});
      } else {
        addBytes({{(bc)op, leftResult.getReg(), rightResult.getReg()}});
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
      expRes.setReg(reg);
    }
    return expRes;
  }

  ExpressionResult leftResult = generateExpression(binOp.leftSide);
  ExpressionResult rightResult = generateExpression(binOp.rightSide);
  if (!leftResult.isReg) { // immediate value
    if (!rightResult.isReg) {
      return evaluateBinOpImmExpression(binOp.op.type, leftResult, rightResult);
    }
    const bc reg = allocateRegister();
    moveImmToReg(reg, leftResult);
    leftResult.isReg = true;
    leftResult.setReg(reg);
    leftResult.isTemp = true;
  } else if (!rightResult.isReg) {
    const bc reg = allocateRegister();
    moveImmToReg(reg, rightResult);
    rightResult.isReg = true;
    rightResult.setReg(reg);
    rightResult.isTemp = true;
  }
  addBytes({{(bc)op, leftResult.getReg(), rightResult.getReg()}});
  if (rightResult.isTemp) {
    freeRegister(rightResult.getReg());
  }
  if (leftResult.isTemp) {
    freeRegister(leftResult.getReg());
  }
  ExpressionResult expRes;
  if (controlFlow) {
    expRes.jumpOp = jumpOp;
  } else {
    expRes.isReg = true;
    expRes.isTemp = true;
    const bc reg = allocateRegister();
    addBytes({{(bc)getOp, reg}});
    expRes.setReg(reg);
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


// ========================================
// UNARY EXPRESSIONS
// ========================================

ExpressionResult CodeGen::generateExpressionUnOp(const UnOp& unOp) {
  ExpressionResult expRes = generateExpression(unOp.operand);
  // TokenType expType = expRes.type->token.getType();
  switch (unOp.op.type) {
    case TokenType::NOT: {
      if (!expRes.isReg) {
        // evaluate imm
        return evaluateUnaryOpImmExpression(unOp.op.getType(), expRes);
      }
      if (!expRes.isTemp) {
        const bc reg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, reg, expRes.getReg()}});
        expRes.setReg(reg);
        expRes.isReg = true;
        expRes.isTemp = true;
      }
      addBytes({{(bc)OpCode::NOT, expRes.getReg()}});
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
      if (!expRes.isReg) {
        // evaluate imm
        return evaluateUnaryOpImmExpression(unOp.op.getType(), expRes);
      }
      if (!expRes.isTemp) {
        const bc reg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, reg, expRes.getReg()}});
        expRes.setReg(reg);
        expRes.isReg = true;
        expRes.isTemp = true;
      }
      addBytes({{(bc)OpCode::NEGATE, expRes.getReg()}});
      return expRes;
    }
    default: {
      std::cerr << "Invalid\n";
      exit(1);
    }
  }
}


// ========================================
// STRUCTS
// ========================================

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


// ========================================
// SCOPES
// ========================================

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
    ExpressionResult stackUsageExp;
    stackUsageExp.setData(&stackUsage, sizeof(stackUsage));
    stackUsageExp.type = &Checker::uint32Value;
    ExpressionResult stackPointerExp;
    stackPointerExp.setReg(stackPointerIndex);
    stackPointerExp.isReg = true;
    stackPointerExp.type = &Checker::uint64Value;
    expressionResWithOp(OpCode::ADD, OpCode::ADD_I, stackPointerExp, stackUsageExp);
  }
}


// ========================================
// FUNCTIONS
// ========================================

bool CodeGen::generateFunctionDeclaration(const FunctionDec& funcDec) {
  // we want callee to handle unwinding arguments from stack
  startFunctionScope(funcDec);
  generateScope(funcDec.body);
  // endFunctionScope(funcDec);
  return true;
}

void CodeGen::startFunctionScope(const FunctionDec& funcDec) {
  assert(stack.empty());
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


// ========================================
// STATEMENTS
// ========================================

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
      break;
    }
    case ControlFlowStatementType::EXIT_STATEMENT: {
      ExpressionResult expRes = generateExpression(controlFlowStatement.returnStatement->returnValue);
      if (!expRes.isReg) {
        moveImmToReg(miscRegisterIndex, expRes);
        expRes.setReg(miscRegisterIndex);
      }
      addBytes({{(bc)OpCode::EXIT, expRes.getReg()}});
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

/**
 * Generate a branching statement
 * if return result is true,
 * adds a jump marker of type JumpMarkerType::TO_BRANCH_END that must be updated by the caller to land wherever needed
*/
BranchStatementResult CodeGen::generateBranchStatement(const BranchStatement& ifStatement) {
  ExpressionResult expRes = generateExpression(ifStatement.condition, true);
  if (expRes.jumpOp == OpCode::NOP) {
    if (!expRes.isReg) {
      // condition is a constant value, can test at compile time
      // TODO: have to evaluate this based on type
      if (*(uint32_t *)expRes.getData()) {
        // condition always true
        generateScope(ifStatement.body);
        return BranchStatementResult::ALWAYS_TRUE;
      } // else condition always false, don't generate
      return BranchStatementResult::ALWAYS_FALSE;
    }
    addBytes({{(bc)OpCode::SET_FLAGS, expRes.getReg()}});
    expRes.jumpOp = OpCode::RS_JUMP_E;
  }
  addJumpMarker(JumpMarkerType::TO_BRANCH_END);
  addJumpOp(expRes.jumpOp);
  generateScope(ifStatement.body);
  return BranchStatementResult::ADDED_JUMP;
}

/**
*/
void CodeGen::generateReturnStatement(const ReturnStatement& returnStatement) {
  (void)returnStatement;
}


// ========================================
// STACK MANAGEMENT
// ========================================

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

uint32_t CodeGen::getVarOffsetFromSP(const StackVariable &variableInfo) {
  // |                   | <- sp
  // |       | <- positionOnStack
  // | value |
  return getCurrStackPointerPosition() - variableInfo.positionOnStack;
}


// ========================================
// OTHER
// ========================================

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

/**
 * Returns the actual type from a token list
 * Once type qualifiers or similar things are supported, this will be handy
*/
Token CodeGen::getTypeFromTokenList(const TokenList& tokenList) {
  return tokenList.token;
}

OpCode getLoadOpForSize(const uint8_t size) {
  switch(size) {
    case 1: return OpCode::LOAD_B;
    case 2: return OpCode::LOAD_W;
    case 3:
    case 4: return OpCode::LOAD_D;
    case 5:
    case 6:
    case 7:
    case 8: return OpCode::LOAD_Q;
    default: std::cerr << "invalid size in getLoadOpForSize: " << (uint32_t)size << '\n'; exit(1);
  }
}


// ========================================
// PRINTING
// ========================================

std::ostream& operator<<(std::ostream& os, const StackMarkerType& obj) {
  switch (obj) {
    case StackMarkerType::NONE: {
      os << "NONE\n";
      break;
    }
    case StackMarkerType::SOFT_SCOPE_START: {
      os << "SOFT_SCOPE_START\n";
      break;
    }
    case StackMarkerType::HARD_SCOPE_START: {
      os << "HARD_SCOPE_START\n";
      break;
    }
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const StackVariable& obj) {
  os << obj.varDec;
  os << "Position on stack: " << obj.positionOnStack << '\n';
  return os;
}

std::ostream& operator<<(std::ostream& os, const StackItem& obj) {
  switch (obj.type) {
    case StackItemType::NONE: {
      os << "NONE\n";
      break;
    }
    case StackItemType::MARKER: {
      os << "MARKER\n";
      os << obj.marker;
      break;
    }
    case StackItemType::VARIABLE: {
      os << "VARIABLE\n";
      os << obj.variable;
      break;
    }
    case StackItemType::RETURN_ADDRESS: {
      os << "RETURN_ADDRESS\n";
      os << "Return Address Position: " << obj.positionOnStack;
      break;
    }
    case StackItemType::RETURN_VALUE: {
      os << "RETURN_VALUE\n";
      os << "Return Value Position: " << obj.positionOnStack;
      break;
    }
  }
  return os;
}
std::ostream& operator<<(std::ostream& os, const std::vector<StackItem>& obj) {
  for (uint32_t i = 0; i < obj.size(); ++i) {
    os << "Item #" << i << '\n';
    os << obj[i] << "\n\n";
  }
  return os;
}


// ========================================
// COMPARING
// ========================================

