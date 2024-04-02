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


void ExpressionResult::set(char c) {
    *(decltype(c) *)data = c;
    type = &Checker::charValue;
}
void ExpressionResult::set(uint32_t val) {
    *(decltype(val) *)data = val;
    type = &Checker::uint32Value;
}
void ExpressionResult::set(uint64_t val) {
    *(decltype(val) *)data = val;
    type = &Checker::uint64Value;
}
void ExpressionResult::set(int32_t val) {
    *(decltype(val) *)data = val;
    type = &Checker::int32Value;
}
void ExpressionResult::set(int64_t val) {
    *(decltype(val) *)data = val;
    type = &Checker::int64Value;
}
void ExpressionResult::set(FILE *val) {
    *(decltype(val) *)data = val;
    type = &Checker::fileValue;
}
void ExpressionResult::set(double val) {
    *(decltype(val) *)data = val;
    type = &Checker::doubleValue;
}
void ExpressionResult::set(bool val) {
    *(decltype(val) *)data = val;
    type = &Checker::boolValue;
}

template <class T>
void ExpressionResult::setUntyped(T t) requires (std::integral<T> || std::floating_point<T>) {
    static_assert(sizeof(T)<=sizeof(data), "T does not fit in data");
    *(decltype(t) *)data = t;
}


JumpMarker::JumpMarker(uint64_t start, JumpMarkerType type):
    start{start}, type{type} {}

CodeGen::CodeGen(
    Program& program,
    std::vector<Tokenizer>& tokenizers,
    std::unordered_map<std::string, GeneralDec *>& lookUp,
    std::unordered_map<StructDec *, StructInformation>& structLookup
): program{program}, tokenizers{tokenizers}, lookUp{lookUp}, structLookUp{structLookup} {
    sp.inUse = true;
    ip.inUse = true;
    dp.inUse = true;
    miscReg.inUse = true;
    FILE *file_p = nullptr;
    addToDataSection(DataSectionEntryType::STDOUT, &file_p, sizeof(file_p));
    addToDataSection(DataSectionEntryType::STDIN, &file_p, sizeof(file_p));
    addToDataSection(DataSectionEntryType::STDERR, &file_p, sizeof(file_p));
    assert(dataSectionEntries.size() == 3);
    assert(dataSectionEntries[0].indexInDataSection == stdoutDataIndex);
    assert(dataSectionEntries[1].indexInDataSection == stdinDataIndex);
    assert(dataSectionEntries[2].indexInDataSection == stderrDataIndex);
}

void CodeGen::generate() {
    const GeneralDecList* decList = &program.decs;
    while (decList) {
        generateGeneralDeclaration(decList->curr);
        decList = decList->next;
    }
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
 * Moves an immediate value within an ExpressionResult into a register
 * Updates exp by setting isReg and isTemp to true, as well as setting the register to the register parameter
*/
void CodeGen::moveImmToReg(const bytecode_t reg, ExpressionResult& exp) {
    assert(!exp.isReg);
    exp.isTemp = true;
    TokenType rightSideExpType = exp.type->token.getType();
    if (rightSideExpType == TokenType::POINTER) {
        rightSideExpType = TokenType::UINT64_TYPE;
    }
    assert(
        rightSideExpType == TokenType::DOUBLE_TYPE ||
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

bool jumpMarkerTypeHasJump(JumpMarkerType markerType) {
    return markerType >= JumpMarkerType::TO_BRANCH_END && markerType <= JumpMarkerType::TO_LOGICAL_BIN_OP_END;
}

/**
 * \returns the index of the jump marker within the jump markers array
*/
JumpMarker& CodeGen::addJumpMarker(JumpMarkerType type) {
    return jumpMarkers.emplace_back(byteCode.size(), type);
}

/**
 * Updates jump markers with destination. Sets type of each updated marker to none
 * \param destination the destination
 * \param type type of jump marker to update
 * \param until keep updating markers till this type is reached. if none, updates the first marker and stops
 * \param clear set the 'until' marker to type none if reached
*/
void CodeGen::updateJumpMarkersTo(const uint64_t destination, JumpMarkerType type, uint32_t from) {
    for (; from < jumpMarkers.size(); ++from) {
        JumpMarker& jumpMarker = jumpMarkers[from];
        if (jumpMarker.type == type) {
            jumpMarker.type = JumpMarkerType::SET;
            jumpMarker.destination = destination;
        }
    }
}

void CodeGen::writeLocalJumpOffsets() {
    // first pass is to shift and update jump markers as needed depending on offset distance
    while (true) {
        bool updated = false;
        for (uint32_t i = 0; i < jumpMarkers.size(); ++i) {
            // only jumps within a function, no function calls
            JumpMarker& jumpMarker = jumpMarkers[i];
            if (jumpMarker.type != JumpMarkerType::SET) {
                continue;
            }
            if (jumpMarker.start > UINT32_MAX || jumpMarker.destination > UINT32_MAX) {
                std::cerr << "Compiler error, bytecode too large\n";
                exit(1);
            }
            int64_t res = jumpMarker.start - jumpMarker.destination;
            const OpCode jumpOp = (OpCode)byteCode[jumpMarker.start];
            const uint8_t jumpOpImmSize = immediateSizeForRelativeJumpOp(jumpOp);
            const uint8_t minImmSize = smallestImmediateSizeForOffset(res);
            if (jumpOpImmSize == minImmSize) {
                continue;
            }
            assert(minImmSize == 4);
            byteCode[jumpMarker.start] = (bc)OpCode::NOP;
            // currently we only have the 4 byte relative immediate, so
            // extend by 8 bytes since moving by a multiple of 8 bytes keep everything after aligned
            const uint32_t shiftAmount = 8;
            byteCode.insert(byteCode.begin() + jumpMarker.start, shiftAmount, (bc)OpCode::NOP);
            // find where we would be 4 byte aligned
            while (true) {
                if ((jumpMarker.start + 1) % minImmSize == 0) {
                    break;
                }
                ++jumpMarker.start;
            }
            byteCode[jumpMarker.start] = (bc)upgradeRelativeJumpOp(jumpOp);

            for (uint32_t k = 0; k < jumpMarkers.size(); ++k) {
                JumpMarker& otherJumpMarker = jumpMarkers[k];
                if (otherJumpMarker.destination > jumpMarker.start) {
                    otherJumpMarker.destination += shiftAmount;
                    updated = true;
                }
                if (otherJumpMarker.start > jumpMarker.start) {
                    otherJumpMarker.start += shiftAmount;
                    updated = true;
                }
            }
        }
        if (!updated) {
            break;
        }
    }

    // second pass is to write the offset in
    for (uint32_t i = 0; i < jumpMarkers.size(); ++i) {
        JumpMarker& jumpMarker = jumpMarkers[i];
        if (jumpMarker.type != JumpMarkerType::SET) {
            continue;
        }
        const OpCode jumpOp = (OpCode)byteCode[jumpMarker.start];
        const uint32_t immSize = immediateSizeForRelativeJumpOp(jumpOp);
        int64_t res = jumpMarker.destination - jumpMarker.start;
        assert(smallestImmediateSizeForOffset(res) == immSize);
        assert(immSize == 1 || immSize == 4);
        if (immSize == 1) {
            byteCode[jumpMarker.start + 1] = (int8_t)res;
        } else if (immSize == 4) {
            *(int32_t *)&byteCode[jumpMarker.start + 1] = (int32_t)res;
        } else {
            // compiler 
            std::cerr << "Compiler error, unsupported relative jump immediate size\n";
            exit(1);
        }
    }
}

void CodeGen::writeFunctionJumpOffsets() {
    for (uint32_t i = 0; i < jumpMarkers.size(); ++i) {
        JumpMarker& jumpMarker = jumpMarkers[i];
        if (jumpMarker.type != JumpMarkerType::FUNCTION_CALL) {
            continue;
        }
        const uint32_t functionIndex = functionDecPointerToIndex[jumpMarker.funcDec];
        assert(jumpMarker.start < UINT32_MAX);
        const int32_t offset = functionIndex - jumpMarker.start;
        *(int32_t *)&byteCode[jumpMarker.start + 1] = (int32_t)offset;
    }
}



// ========================================
// DECLARATIONS
// ========================================

void CodeGen::generateGeneralDeclaration(const GeneralDec& genDec) {
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
            generateVariableDeclaration(*genDec.varDec);
            break;
        }
        case GeneralDecType::FUNCTION: {
            generateFunctionDeclaration(*genDec.funcDec);
            break;
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
        case GeneralDecType::INCLUDE_DEC:
        case GeneralDecType::BUILTIN_FUNCTION: {
            // don't do anything, handled by parser
            break;
        }
    }
}

/**
 * Have to figure out stack alignment
 * \returns the size of the type
*/
void CodeGen::generateVariableDeclaration(const VariableDec& varDec, bool initialize) {
    const uint32_t preAddStackPointerOffset = getCurrStackPointerPosition();
    StackVariable& stackVar = addVarDecToVirtualStack(varDec);
    const uint32_t diff = stackVar.positionOnStack - preAddStackPointerOffset;
    const Token typeToken = getTypeFromTokenList(varDec.type);

    if (typeToken.type == TokenType::IDENTIFIER) {
        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
        const StructInformation& structInfo = structLookUp[genDec->structDec];
        const uint32_t size = structInfo.size;
        const uint32_t padding = diff - size;
        assert(padding < size);
        uint32_t spaceToAdd = size + padding;
        efficientImmAddOrSub(stackPointerIndex, spaceToAdd, OpCode::SUB);
        return;
    }

    if (!isBuiltInType(typeToken.type) && typeToken.type != TokenType::REFERENCE) {
        std::cerr << "Invalid TokenType [" << (uint32_t)typeToken.type << "] in generateVariableDeclaration\n";
        exit(1);
    }

    ExpressionResult expRes;
    const uint32_t size = getSizeOfBuiltinType(typeToken.getType());
    if (initialize && varDec.initialAssignment) {
        expRes = generateExpression(*varDec.initialAssignment);
        if (!expRes.isReg) {
            moveImmToReg(allocateRegister(), expRes);
        } else if (expRes.isPointerToValue) {
            expRes = loadValueFromPointer(expRes);
        }
        // stackVar.reg = reg;
    }
    const uint32_t padding = diff - size;
    assert(padding < size);
    assert(size >= 1 && size <= 8);
    if (!expRes.isReg) {
        uint32_t spaceToAdd = size + padding;
        efficientImmAddOrSub(stackPointerIndex, spaceToAdd, OpCode::SUB);
        return;
    }
    const OpCode pushOp = getPushOpForSize(size);
    assert(pushOp != OpCode::NOP);
    efficientImmAddOrSub(stackPointerIndex, padding, OpCode::SUB);
    addBytes({{(bc)pushOp, expRes.getReg()}});
    if (expRes.isTemp) {
        freeRegister(expRes.getReg());
    }
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
void doBinaryEvaluate(const ExpressionResult& left, const ExpressionResult& right, ExpressionResult& res) {
    const TokenType leftSideType = left.type->token.getType();
    const TokenType rightSideType = right.type->token.getType();
    assert(leftSideType == TokenType::DOUBLE_TYPE || leftSideType == TokenType::UINT64_TYPE || leftSideType == TokenType::INT64_TYPE);
    assert(rightSideType == TokenType::DOUBLE_TYPE || rightSideType == TokenType::UINT64_TYPE || rightSideType == TokenType::INT64_TYPE);
    if (leftSideType == TokenType::DOUBLE_TYPE) {
        if (rightSideType == TokenType::DOUBLE_TYPE) {
            auto temp = TFunctor<double, double>()(*(double*)left.getData(), *(double*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::UINT64_TYPE) {
            auto temp = TFunctor<double, uint64_t>()(*(double*)left.getData(), *(uint64_t*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::INT64_TYPE) {
            auto temp = TFunctor<double, int64_t>()(*(double*)left.getData(), *(int64_t*)right.getData());
            res.setUntyped(temp);
        }
    } else if (leftSideType == TokenType::UINT64_TYPE) {
        if (rightSideType == TokenType::DOUBLE_TYPE) {
            auto temp = TFunctor<uint64_t, double>()(*(uint64_t*)left.getData(), *(double*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::UINT64_TYPE) {
            auto temp = TFunctor<uint64_t, uint64_t>()(*(uint64_t*)left.getData(), *(uint64_t*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::INT64_TYPE) {
            auto temp = TFunctor<uint64_t, int64_t>()(*(uint64_t*)left.getData(), *(int64_t*)right.getData());
            res.setUntyped(temp);
        }
    } else if (leftSideType == TokenType::INT64_TYPE) {
        if (rightSideType == TokenType::DOUBLE_TYPE) {
            auto temp = TFunctor<int64_t, double>()(*(int64_t*)left.getData(), *(double*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::UINT64_TYPE) {
            auto temp = TFunctor<int64_t, uint64_t>()(*(int64_t*)left.getData(), *(uint64_t*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::INT64_TYPE) {
            auto temp = TFunctor<int64_t, int64_t>()(*(int64_t*)left.getData(), *(int64_t*)right.getData());
            res.setUntyped(temp);
        }
    }
}

template<template<typename, typename> class TFunctor>
void doBinaryIntegralEvaluate(const ExpressionResult& left, const ExpressionResult& right, ExpressionResult& res) {
    const TokenType leftSideType = left.type->token.getType();
    const TokenType rightSideType = right.type->token.getType();
    assert(leftSideType == TokenType::UINT64_TYPE || leftSideType == TokenType::INT64_TYPE);
    assert(leftSideType == TokenType::UINT64_TYPE || leftSideType == TokenType::INT64_TYPE);
    if (leftSideType == TokenType::UINT64_TYPE) {
        if (rightSideType == TokenType::UINT64_TYPE) {
            auto temp = TFunctor<uint64_t, uint64_t>()(*(uint64_t*)left.getData(), *(uint64_t*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::INT64_TYPE) {
            auto temp = TFunctor<uint64_t, int64_t>()(*(uint64_t*)left.getData(), *(int64_t*)right.getData());
            res.setUntyped(temp);
        }
    } else if (leftSideType == TokenType::INT64_TYPE) {
        if (rightSideType == TokenType::UINT64_TYPE) {
            auto temp = TFunctor<int64_t, uint64_t>()(*(int64_t*)left.getData(), *(uint64_t*)right.getData());
            res.setUntyped(temp);
        } else if (rightSideType == TokenType::INT64_TYPE) {
            auto temp = TFunctor<int64_t, int64_t>()(*(int64_t*)left.getData(), *(int64_t*)right.getData());
            res.setUntyped(temp);
        }
    }
}

ExpressionResult evaluateBinOpImmExpression(TokenType op, ExpressionResult& left, ExpressionResult& right) {
    assert(!left.isReg && !right.isReg);
    assert(left.type && right.type);
    const TokenType leftSideType = left.type->token.getType();
    const TokenType rightSideType = right.type->token.getType();
    assert(isBuiltInType(leftSideType) && leftSideType != TokenType::VOID && leftSideType != TokenType::STRING_TYPE);
    assert(isBuiltInType(rightSideType) && rightSideType != TokenType::VOID && rightSideType != TokenType::STRING_TYPE);
    ExpressionResult res;
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
        left.type = &Checker::uint64Value;
    } else if (isSigned(leftSideType)) {
        // temporarily mark as int64_t
        // sign extend
        switch (leftSideType) {
            case TokenType::CHAR_TYPE:
            case TokenType::INT8_TYPE: { left.set(static_cast<int64_t>(*(int8_t*)left.getData())); break; }
            case TokenType::INT16_TYPE: { left.set(static_cast<int64_t>(*(int16_t*)left.getData())); break; }
            case TokenType::INT32_TYPE: { left.set(static_cast<int64_t>(*(int32_t*)left.getData())); break; }
            case TokenType::INT64_TYPE: break;
            default: {
                exit(1);
            }
        }
    }
    if (isUnsigned(rightSideType) || rightSideType == TokenType::BOOL) {
        // temporarily mark as uint64_t
        right.type = &Checker::uint64Value;
    } else if (isSigned(rightSideType)) {
        // temporarily mark as int64_t
        // sign extend
        switch (rightSideType) {
            case TokenType::CHAR_TYPE:
            case TokenType::INT8_TYPE: { right.set(static_cast<int64_t>(*(int8_t*)right.getData())); break; }
            case TokenType::INT16_TYPE: { right.set(static_cast<int64_t>(*(int16_t*)right.getData())); break; }
            case TokenType::INT32_TYPE: { right.set(static_cast<int64_t>(*(int32_t*)right.getData())); break; }
            case TokenType::INT64_TYPE: break;
            default: {
                exit(1);
            }
        }
    }
    switch (op) {
        case TokenType::ADDITION: {
            doBinaryEvaluate<OperatorAdd>(left, right, res);
            break;
        }
        case TokenType::SUBTRACTION: {
            doBinaryEvaluate<OperatorSub>(left, right, res);
            break;
        }
        case TokenType::MULTIPLICATION: {
            doBinaryEvaluate<OperatorMul>(left, right, res);
            break;
        }
        case TokenType::DIVISION: {
            doBinaryEvaluate<OperatorDiv>(left, right, res);
            break;
        }
        case TokenType::MODULO: {
            doBinaryIntegralEvaluate<OperatorModulo>(left, right, res);
            break;
        }
        case TokenType::BITWISE_OR: {
            doBinaryIntegralEvaluate<OperatorBitwiseOr>(left, right, res);
            break;
        }
        case TokenType::BITWISE_AND: {
            doBinaryIntegralEvaluate<OperatorBitwiseAnd>(left, right, res);
            break;
        }
        case TokenType::BITWISE_XOR: {
            doBinaryIntegralEvaluate<OperatorBitwiseXor>(left, right, res);
            break;
        }
        case TokenType::SHIFT_LEFT: {
            doBinaryIntegralEvaluate<OperatorShiftLeft>(left, right, res);
            break;
        }
        case TokenType::SHIFT_RIGHT: {
            doBinaryIntegralEvaluate<OperatorShiftRight>(left, right, res);
            break;
        }
        case TokenType::EQUAL: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorEqual>(left, right, res);
            break;
        }
        case TokenType::NOT_EQUAL: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorNotEqual>(left, right, res);
            break;
        }
        case TokenType::LOGICAL_AND: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorLogicalAnd>(left, right, res);
            break;
        }
        case TokenType::LOGICAL_OR: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorLogicalOr>(left, right, res);
            break;
        }
        case TokenType::LESS_THAN: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorLess>(left, right, res);
            break;
        }
        case TokenType::LESS_THAN_EQUAL: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorLessEqual>(left, right, res);
            break;
        }
        case TokenType::GREATER_THAN: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorGreater>(left, right, res);
            break;
        }
        case TokenType::GREATER_THAN_EQUAL: {
            res.type = &Checker::boolValue;
            doBinaryEvaluate<OperatorGreaterEqual>(left, right, res);
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
void doUnaryEvaluate(const ExpressionResult& operand, ExpressionResult& res) {
    TokenType operandType = operand.type->token.getType();
    assert(operandType == TokenType::DOUBLE_TYPE || operandType == TokenType::UINT64_TYPE || operandType == TokenType::INT64_TYPE);
    if (operandType == TokenType::DOUBLE_TYPE) {
        auto unaryOpValue = TFunctor<double>()(*(double*)operand.getData());
        res.setUntyped(unaryOpValue);
    } else if (operandType == TokenType::UINT64_TYPE) {
        auto unaryOpValue = TFunctor<uint64_t>()(*(uint64_t*)operand.getData());
        res.setUntyped(unaryOpValue);
    } else if (operandType == TokenType::INT64_TYPE) {
        auto unaryOpValue = TFunctor<int64_t>()(*(int64_t*)operand.getData());
        res.setUntyped(unaryOpValue);
    }
}

ExpressionResult evaluateUnaryOpImmExpression(TokenType op, ExpressionResult& operand) {
    assert(!operand.isReg && operand.type);
    ExpressionResult res;
    const TokenType operandType = operand.type->token.getType();
    assert(isBuiltInType(operandType) && operandType != TokenType::VOID && operandType != TokenType::STRING_TYPE);
    
    // assign to largest type, minimum of int32
    res.type = &Checker::int32Value;
    if (operandType > res.type->token.getType()) {
        res.type = operand.type;
    }
    if (isUnsigned(operandType) || operandType == TokenType::BOOL) {
        // temporarily mark as uint64_t
        operand.type = &Checker::uint64Value;
    } else if (isSigned(operandType)) {
        // temporarily mark as int64_t
        // sign extend
        switch (operandType) {
            case TokenType::CHAR_TYPE:
            case TokenType::INT8_TYPE: { operand.set(static_cast<int64_t>(*(int8_t*)operand.getData())); break; }
            case TokenType::INT16_TYPE: { operand.set(static_cast<int64_t>(*(int16_t*)operand.getData())); break; }
            case TokenType::INT32_TYPE: { operand.set(static_cast<int64_t>(*(int32_t*)operand.getData())); break; }
            case TokenType::INT64_TYPE: break;
            default: {
                exit(1);
            }
        }
    }
    switch (op) {
        case TokenType::NOT: {
            res.type = &Checker::boolValue;
            doUnaryEvaluate<OperatorNot>(operand, res);
            break;
        }
        case TokenType::NEGATIVE: {
            doUnaryEvaluate<OperatorNegate>(operand, res);
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
            return loadValue(currExp);
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
    const FunctionDec* p_funcDec;
    if (generalDec->type == GeneralDecType::BUILTIN_FUNCTION) {
        p_funcDec = &generalDec->builtinFunc->funcDec;
    } else {
        p_funcDec = generalDec->funcDec;
    }
    {
        Token returnType = getTypeFromTokenList(p_funcDec->returnType);
        uint32_t size = getSizeOfType(returnType);
        if (size) {
            // 8 byte align return type since function assumes it is
            const uint32_t padding = getPaddingNeeded(getCurrStackPointerPosition(), size, 8);
            addSpaceToStack(padding + size);
            StackItem returnValue {
                .tempValue {
                    .positionOnStack = getCurrStackPointerPosition() + padding + size
                },
                .type = StackItemType::TEMP_VALUE
            };
            stackItems.emplace_back(returnValue);
        }
    }
    // the function will reset the stack to this point
    const uint32_t resetTo = stackItems.size();

    // add arguments to stack
    if (functionCall.args.curr.getType() != ExpressionType::NONE) {
        const ExpressionList *expressionList = &functionCall.args;
        do {
            ExpressionResult expRes = generateExpression(expressionList->curr);
            const uint32_t positionOnStack = addExpressionResToStack(expRes);
            StackItem argumentItem {
                .tempValue {
                    .positionOnStack = positionOnStack
                },
                .type = StackItemType::TEMP_VALUE
            };
            stackItems.emplace_back(argumentItem);
            expressionList = expressionList->next;
        } while (expressionList);
    }
    addSpaceToStack(getPaddingNeeded(getCurrStackPointerPosition(), 8, 8));
    alignForImm(1, 4);
    // register this position to be updated
    if (generalDec->type == GeneralDecType::BUILTIN_FUNCTION) {
        const std::string& builtinFunctionName = tokenizers[generalDec->tokenizerIndex].extractToken(generalDec->builtinFunc->name);
        if (!builtin_function_to_bytecode_t.contains(builtinFunctionName)) {
            std::cerr << "Compiler error, builtin function for " << builtinFunctionName << " does not exist\n";
            exit(1);
        }
        BuiltInFunction builtInFunc = builtin_function_to_bytecode_t.at(builtinFunctionName);
        addByteOp(OpCode::CALL_B);
        addByte((bytecode_t)builtInFunc);
    } else {
        JumpMarker& jumpMarker = addJumpMarker(JumpMarkerType::FUNCTION_CALL);
        jumpMarker.funcDec = p_funcDec;
        addByteOp(OpCode::CALL);
        add4ByteNum(0);
    }
    while (stackItems.size() > resetTo) {
        stackItems.pop_back();
    }

    // have to return 'return value' expression result, which is stored on the stack
    // pointer is in sp register
    ExpressionResult returnValuePointerExpression;
    returnValuePointerExpression.setReg(stackPointerIndex);
    // returnValuePointerExpression.isPointerToStackValue; ?
    returnValuePointerExpression.type = &p_funcDec->returnType;
    return returnValuePointerExpression;
}


/**
 * Uses the misc register
*/
void CodeGen::expressionResWithOp(OpCode op, OpCode immOp, const ExpressionResult& left, const ExpressionResult& right) {
    assert(left.isReg);
    const bytecode_t resReg = left.getReg();
    if (right.isReg) {
        assert(op != OpCode::NOP);
        addBytes({{(bc)op, resReg, right.getReg()}});
        return;
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
            return;
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
            return;
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
    expRes.isPointerToValue = true;
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

            Token type;
            if (op == TokenType::PTR_MEMBER_ACCESS) {
                // pointer to type, get type after the pointer
                type = getTypeFromTokenList(*getNextFromTokenList(*expRes.type));
            } else {
                type = getTypeFromTokenList(*expRes.type);
            }
            const std::string& structName = tk->extractToken(type);
            const GeneralDec* genDec = lookUp[structName];
            const StructInformation& structInfo = structLookUp[genDec->structDec];
            assert(binOp->rightSide.getType() == ExpressionType::VALUE);
            const std::string& memberName = tk->extractToken(binOp->rightSide.getToken());
            const uint32_t offsetInStruct = structInfo.memberLookup.at(memberName).position;
            if (offsetInStruct) {
                efficientImmAddOrSub(expRes.getReg(), offsetInStruct, OpCode::ADD);
            }
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
            StackVariable& stackVar = stackItems[stackItemIndex].variable;
            uint32_t offset = getVarOffsetFromSP(stackVar);
            if (offset) {
                expRes.setReg(allocateRegister());
                addBytes({{(bc)OpCode::MOVE, expRes.getReg(), stackPointerIndex}});
                efficientImmAddOrSub(expRes.getReg(), offset, OpCode::ADD);
            } else {
                expRes.setReg(stackPointerIndex);
                expRes.isTemp = false;
            }
            expRes.type = &stackVar.varDec.type;
            if (expRes.type->token.getType() == TokenType::REFERENCE) {
                // always move past references
                expRes.type = expRes.type->next;
            }
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
            expRes.setReg(array.getReg());
            expRes.isTemp = true;
            return expRes;
        }
        default: {
            // invalid type
            assert(false);
            return expRes;
        }
    }
}


ExpressionResult CodeGen::loadValue(const Expression &expression) {
    ExpressionResult expRes;
    const Token& token = expression.getToken();
    switch (token.type) {
        case TokenType::CHAR_LITERAL: {
            std::string charLiteral = tk->extractToken(token);
            // convert charLiteral to its numeric value and return it
            // might do this during the tokenizer stage...
            if (charLiteral.size() == 3) {
                expRes.set(charLiteral[1]);
                return expRes;
            } else if (charLiteral.size() == 4) {
                assert(charLiteral[1] == '\\');
                switch (charLiteral[2]) {
                    case '0': expRes.set('\0'); break;
                    case 'n': expRes.set('\n'); break;
                    case 't': expRes.set('\t'); break;
                    default: expRes.set(charLiteral[2]); break;
                }
                return expRes;
            }
            assert(false);
            return expRes;
        }
        case TokenType::STRING_LITERAL: { 
            // place string literal in data section of bytecode, return offset to start of string
            expRes.type = &Checker::stringValue;
            std::string stringLiteral = tk->extractToken(token);
              // remove quotes around string
            stringLiteral.erase(stringLiteral.end() - 1);
            stringLiteral.erase(stringLiteral.begin());
            // TODO: need to replace escaped characters with actual value
            // check if string is already in data section
            for (uint32_t i = 0; i < dataSectionEntries.size(); ++i) {
                if (dataSectionEntries[i].type != DataSectionEntryType::STRING_LITERAL) {
                    continue;
                }
                const char *pString = (const char *)dataSection.data() + dataSectionEntries[i].indexInDataSection;
                uint64_t stringLength = strlen(pString);
                if (stringLiteral.length() == stringLength && stringLiteral == pString) {
                    expRes.set(i);
                    return expRes;
                }
                i += stringLength;
            }
            const DataSectionEntry& dataSectionEntry = addToDataSection(DataSectionEntryType::STRING_LITERAL, stringLiteral.data(), stringLiteral.length() + 1);
            if (dataSectionEntry.indexInDataSection) {
                {
                    ExpressionResult stringExp;
                    stringExp.setReg(allocateRegister());
                    ExpressionResult dataPointerExp;
                    dataPointerExp.setReg(dataPointerIndex);
                    expressionResWithOp(OpCode::MOVE, OpCode::NOP, stringExp, dataPointerExp);
                    expRes.setReg(stringExp.getReg());
                }
                efficientImmAddOrSub(expRes.getReg(), dataSectionEntry.indexInDataSection, OpCode::ADD);
            } else {
                expRes.setReg(dataPointerIndex);
                expRes.isTemp = true;
            }
            return expRes;
        }
        case TokenType::DECIMAL_NUMBER: {
            std::string decimalNumber = tk->extractToken(token);
            uint64_t num = std::stoull(decimalNumber);
            expRes.set(num);
            if (num <= INT32_MAX) {
                expRes.type = &Checker::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.type = &Checker::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.type = &Checker::int64Value;
            } // already set to uint64_t
            return expRes;
        }
        case TokenType::BINARY_NUMBER: {
            assert(token.length > 2);
            std::string binaryNumber = tk->extractToken(Token{token.position + 2, (uint16_t)(token.length - 2), TokenType::BINARY_NUMBER});
            uint64_t num = std::stoull(binaryNumber, nullptr, 2);
            expRes.set(num);
            if (num <= INT32_MAX) {
                expRes.type = &Checker::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.type = &Checker::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.type = &Checker::int64Value;
            } // already set to uint64_t
            return expRes;
        }
        case TokenType::FLOAT_NUMBER: {
            std::string binaryNumber = tk->extractToken(token);
            double num = std::stod(binaryNumber);
            expRes.set(num);
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
            expRes.set(num);
            if (num <= INT32_MAX) {
                expRes.type = &Checker::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.type = &Checker::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.type = &Checker::int64Value;
            } // already set to uint64_t
            return expRes;
        }
        case TokenType::FALSE: {
            expRes.set(false);
            return expRes;
        }
        case TokenType::TRUE: {
            expRes.set(true);
            return expRes;
        }
        case TokenType::STDIN: {
            expRes.isPointerToValue = true;
            expRes.setReg(allocateRegister());
            expRes.isTemp = true;
            addBytes({{(bc)OpCode::MOVE, expRes.getReg(), dataPointerIndex}});
            efficientImmAddOrSub(expRes.getReg(), stdinDataIndex, OpCode::ADD);
            expRes.type = &Checker::fileValue;
            return expRes;
        }
        case TokenType::STDERR: {
            expRes.isPointerToValue = true;
            expRes.setReg(allocateRegister());
            expRes.isTemp = true;
            addBytes({{(bc)OpCode::MOVE, expRes.getReg(), dataPointerIndex}});
            efficientImmAddOrSub(expRes.getReg(), stderrDataIndex, OpCode::ADD);
            expRes.type = &Checker::fileValue;
            return expRes;
        }
        case TokenType::STDOUT: {
            expRes.isPointerToValue = true;
            expRes.setReg(allocateRegister());
            expRes.isTemp = true;
            addBytes({{(bc)OpCode::MOVE, expRes.getReg(), dataPointerIndex}});
            efficientImmAddOrSub(expRes.getReg(), stdoutDataIndex, OpCode::ADD);
            expRes.type = &Checker::fileValue;
            return expRes;
        }
        case TokenType::NULL_PTR: {
            expRes.type = &Checker::ptrValue;
            return expRes;
        }
        case TokenType::IDENTIFIER: {
            const std::string& identifier = tk->extractToken(token);
            const uint32_t stackItemIndex = nameToStackItemIndex[identifier];
            const StackVariable &variableInfo = stackItems[stackItemIndex].variable;
            const uint32_t sizeOfVar = getSizeOfType(getTypeFromTokenList(variableInfo.varDec.type));
            if (sizeOfVar > SIZE_OF_REGISTER) {
                return getAddressOfExpression(expression);
            }
            expRes.type = &variableInfo.varDec.type;
            // if (variableInfo.reg) {
            //   expRes.setReg(variableInfo.reg);
            //   return expRes;
            // }
            // load value into a register
            expRes.setReg(allocateRegister());
            // variableInfo.reg = expRes.getReg();
            const OpCode loadOp = getLoadOpForSize(sizeOfVar);
            const uint32_t varOffset = getVarOffsetFromSP(variableInfo);
            if (varOffset) {
                addBytes({{(bc)OpCode::MOVE, miscRegisterIndex, stackPointerIndex}});
                efficientImmAddOrSub(miscRegisterIndex, varOffset, OpCode::ADD);
                addBytes({{(bc)loadOp, expRes.getReg(), miscRegisterIndex}});
            } else {
                addBytes({{(bc)loadOp, expRes.getReg(), stackPointerIndex}});
            }
            return expRes;
        }
        default: {
            assert(false);
            return expRes;
        }
    }
}

/**
 * Loads the data pointed at by expRes into a register
*/
ExpressionResult CodeGen::loadValueFromPointer(const ExpressionResult &pointerExp) {
    assert(pointerExp.isReg);
    const TokenList* type;
    if (pointerExp.isPointerToValue) {
        type = pointerExp.type;
    } else {
        assert(getTypeFromTokenList(*pointerExp.type).getType() == TokenType::POINTER);
        type = getNextFromTokenList(*pointerExp.type);
    }
    const Token typeToken = getTypeFromTokenList(*type);
    const uint32_t sizeOfType = getSizeOfType(typeToken);
    const OpCode loadOp = getLoadOpForSize(sizeOfType);
    assert(loadOp != OpCode::NOP);
    ExpressionResult expRes;
    expRes.setReg(allocateRegister());
    expRes.type = type;
    addBytes({{(bc)loadOp, expRes.getReg(), pointerExp.getReg()}});
    return expRes;
}

/**
 * Stores data
*/
void CodeGen::storeValueToPointer(const ExpressionResult &pointerExp, ExpressionResult &valueExp) {
    assert(pointerExp.isReg);
    if (!valueExp.isReg) {
        moveImmToReg(allocateRegister(), valueExp);
    }
    const TokenList* type;
    if (pointerExp.isPointerToValue) {
        type = pointerExp.type;
    } else {
        assert(getTypeFromTokenList(*pointerExp.type).getType() == TokenType::POINTER);
        type = getNextFromTokenList(*pointerExp.type);
    }
    const Token typeToken = getTypeFromTokenList(*type);
    const uint32_t sizeOfType = getSizeOfType(typeToken);
    const OpCode storeOp = getStoreOpForSize(sizeOfType);
    assert(storeOp != OpCode::NOP);
    addBytes({{(bc)storeOp, pointerExp.getReg(), valueExp.getReg()}});
}

/**
 * Adds to a pointer based on type size
 * \param pointerExp the pointer expression to add to. Must be a pointer type
 * \param indexExp the index expression, can be any integral value
*/
void CodeGen::doPointerIndex(const ExpressionResult &pointerExp, ExpressionResult &indexExp) {
    assert(isSigned(getTypeFromTokenList(*indexExp.type).getType()) || isUnsigned(getTypeFromTokenList(*indexExp.type).getType()));
    assert(getTypeFromTokenList(*pointerExp.type).getType() == TokenType::POINTER);
    const Token nextType = getTypeFromTokenList(*getNextFromTokenList(*pointerExp.type));
    uint32_t sizeOfType = getSizeOfType(nextType);
    if (sizeOfType > 1) {
        ExpressionResult sizeExp;
        sizeExp.set(sizeOfType);
        if (!indexExp.isReg) {
            indexExp = evaluateBinOpImmExpression(TokenType::MULTIPLICATION, indexExp, sizeExp);
        } else {
            expressionResWithOp(OpCode::MUL, OpCode::MUL_I, indexExp, sizeExp);
        }
    }
    expressionResWithOp(OpCode::ADD, OpCode::ADD_I, pointerExp, indexExp);
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
            expressionResWithOp(op, opImm, leftResult, rightResult);
            return leftResult;
        }
        // right temp / var
        else {
            if (rightResult.isTemp) {
                freeRegister(rightResult.getReg());
            }
            expressionResWithOp(op, opImm, leftResult, rightResult);
            return leftResult;
        }
    }
    // left imm
    if (!leftResult.isReg) {
        // right is a reg
        assert(rightResult.isReg);
        // right temp
        if (rightResult.isTemp) {
            if (isCommutative(op)) {
                expressionResWithOp(op, opImm, rightResult, leftResult);
                return rightResult;
            }
            // cannot flip args
            moveImmToReg(allocateRegister(), leftResult);
            freeRegister(rightResult.getReg());
            expressionResWithOp(op, opImm, leftResult, rightResult);
            return leftResult;

        }
        // right var
        moveImmToReg(allocateRegister(), leftResult);
        expressionResWithOp(op, opImm, leftResult, rightResult);
        return leftResult;

    }
    // left var
    {
        // right temp
        if (rightResult.isReg && rightResult.isTemp) {
            if (isCommutative(op)) {
                expressionResWithOp(op, opImm, rightResult, leftResult);
                return rightResult;
            }
            bc reg = allocateRegister();
            addBytes({{(bc)OpCode::MOVE, reg, leftResult.getReg()}});
            leftResult.setReg(reg);
            leftResult.isTemp = true;
            freeRegister(rightResult.getReg());
            expressionResWithOp(op, opImm, leftResult, rightResult);
            return leftResult;
        }
        // right var / imm
        bc reg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, reg, leftResult.getReg()}});
        leftResult.setReg(reg);
        leftResult.isTemp = true;
        expressionResWithOp(op, opImm, leftResult, rightResult);
        return leftResult;
    }
}

/**
 * Generates byte code for assignment expressions
 * Values on the left side are not preserved
*/
ExpressionResult CodeGen::assignmentBinOp(const BinOp& binOp, const OpCode op, const OpCode opImm) {
    // checkers job to insure the assignment is valid
    ExpressionResult rightResult = generateExpression(binOp.rightSide);
    ExpressionResult leftResult = getAddressOfExpression(binOp.leftSide);
    assert(leftResult.isReg);
    assert(leftResult.isPointerToValue);
    ExpressionResult leftValue = loadValueFromPointer(leftResult);
    // registers[leftResult.getReg()].changed = true;
    expressionResWithOp(op, opImm, leftValue, rightResult);
    if (rightResult.isTemp){
        freeRegister(rightResult.getReg());
    }
    storeValueToPointer(leftResult, leftValue);
    if (leftValue.isTemp){
        freeRegister(leftValue.getReg());
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
        // if left and right are both constant, we need to save the left side till we get the right, and then do the eval
        // if left is a constant, we can discard the left side since either the expression will not go on, or the result is dependent on the right side
        // if right is a constant, we still need to have saved the left side before hand, unless it's also a constant
        
        // so if left is not a reg, we know it's value does not matter

        ExpressionResult leftResult = generateExpression(binOp.leftSide, controlFlow);
        uint32_t startJumpMarkers = jumpMarkers.size();
        // if jump op is set, the flags are already set, 
        if (!leftResult.isReg && leftResult.jumpOp == OpCode::NOP) {
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
            if (!leftResult.isReg) {
                leftResult.setReg(allocateRegister());
                addBytes({{(bc)OpCode::GET_NE, leftResult.getReg()}}); // save the result
                shortCircuitIndexStart = byteCode.size();
            } else {
                shortCircuitIndexStart = byteCode.size();
                addBytes({{(bc)OpCode::SET_FLAGS, leftResult.getReg()}});
            }
            if (binOp.op.type == TokenType::LOGICAL_AND) {
                addJumpMarker(JumpMarkerType::TO_LOGICAL_BIN_OP_END);
                addJumpOp(OpCode::RS_JUMP_E); // if leftResult is false, skip right side
            }
            else {
                addJumpMarker(JumpMarkerType::TO_LOGICAL_BIN_OP_END);
                addJumpOp(OpCode::RS_JUMP_NE); // if leftResult is true, skip right side
            }
        }

        // l no reg immediate
        // l is reg value is loaded in register
        
        ExpressionResult rightResult = generateExpression(binOp.rightSide, controlFlow && !leftResult.isReg);
        // r no reg r no jump, immediate
        // r is reg r no jump, value is loaded in register
        // r is reg r is jump, not possible
        // r no reg r is jump, flags set

        if (!rightResult.isReg && rightResult.jumpOp == OpCode::NOP) {
            // right side is immediate value
            if (!leftResult.isReg) {
                // both sides are immediate
                return evaluateBinOpImmExpression(binOp.op.type, leftResult, rightResult);
            }
            // left result is not an immediate
            byteCode.resize(shortCircuitIndexStart);
            ExpressionResult zeroExp;
            zeroExp.type = &Checker::int32Value;
            rightResult = evaluateBinOpImmExpression(TokenType::NOT_EQUAL, rightResult, zeroExp);
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
            } else if (rightResult.isReg) {
                addBytes({{(bc)op, leftResult.getReg(), rightResult.getReg()}});
                updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_LOGICAL_BIN_OP_END, startJumpMarkers);
            } else {
                assert(rightResult.jumpOp != OpCode::NOP);
                addBytes({{(bc)OpCode::GET_NE, miscRegisterIndex}});
                addBytes({{(bc)op, leftResult.getReg(), miscRegisterIndex}});
            }
        }
        
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
    } else {
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

ExpressionResult CodeGen::defaultUnOp(const UnOp& unOp, OpCode op) {
    ExpressionResult expRes = generateExpression(unOp.operand);
    if (!expRes.isReg) {
        // evaluate imm
        return evaluateUnaryOpImmExpression(unOp.op.getType(), expRes);
    }
    if (!expRes.isTemp) {
        const bc reg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, reg, expRes.getReg()}});
        expRes.setReg(reg);
        expRes.isTemp = true;
    }
    addBytes({{(bc)op, expRes.getReg()}});
    return expRes;
}

ExpressionResult CodeGen::incrementOrDecrement(const UnOp& unOp, TokenType op) {
    assert(op == TokenType::DECREMENT_PREFIX || op == TokenType::INCREMENT_PREFIX || op == TokenType::DECREMENT_POSTFIX || op == TokenType::INCREMENT_POSTFIX);
    ExpressionResult addressExp = getAddressOfExpression(unOp.operand);
    ExpressionResult dataExp = loadValueFromPointer(addressExp);
    ExpressionResult incrementExp;
    OpCode opCode;
    if (op == TokenType::DECREMENT_PREFIX || op == TokenType::DECREMENT_POSTFIX) {
        opCode = OpCode::DEC;
        incrementExp.set(-1);
    } else {
        opCode = OpCode::INC;
        incrementExp.set(1);
    }
    const Token typeToken = getTypeFromTokenList(*dataExp.type);
    if (op == TokenType::DECREMENT_POSTFIX || op == TokenType::INCREMENT_POSTFIX) {
        bytecode_t currReg = dataExp.getReg();
        dataExp.setReg(allocateRegister());
        addBytes({{(bc)OpCode::MOVE, dataExp.getReg(), currReg}});
        if (typeToken.getType() == TokenType::POINTER) {
            doPointerIndex(dataExp, incrementExp);
        } else {
            addBytes({{(bc)opCode, dataExp.getReg()}});
        }
        storeValueToPointer(addressExp, dataExp);
        freeRegister(dataExp.getReg());
        dataExp.setReg(currReg);
    } else {
        if (typeToken.getType() == TokenType::POINTER) {
            doPointerIndex(dataExp, incrementExp);
        } else {
            addBytes({{(bc)opCode, dataExp.getReg()}});
        }
        storeValueToPointer(addressExp, dataExp);
    }
    return dataExp;
}

ExpressionResult CodeGen::generateExpressionUnOp(const UnOp& unOp) {
    switch (unOp.op.getType()) {
        case TokenType::NOT: {
            return defaultUnOp(unOp, OpCode::NOT);
        }
        case TokenType::ADDRESS_OF: {
            return getAddressOfExpression(unOp.operand);
        }
        case TokenType::DEREFERENCE: {
            ExpressionResult expRes = generateExpression(unOp.operand);
            const Token type = getTypeFromTokenList(*expRes.type);
            assert(type.getType() == TokenType::POINTER);
            expRes.type = getNextFromTokenList(*expRes.type);
            expRes.isPointerToValue = true;
            return expRes;
        }
        case TokenType::INCREMENT_POSTFIX:
        case TokenType::INCREMENT_PREFIX:
        case TokenType::DECREMENT_POSTFIX:
        case TokenType::DECREMENT_PREFIX: {
            return incrementOrDecrement(unOp, unOp.op.getType());
        }
        case TokenType::NEGATIVE: {
            return defaultUnOp(unOp, OpCode::NEGATE);
        }
        default: {
            std::cerr << "Invalid token type in CodeGen::generateExpressionUnOp " << unOp.op.getType() << '\n';
            exit(1);
        }
    }
}


// ========================================
// STRUCTS
// ========================================


// ========================================
// SCOPES
// ========================================

void CodeGen::generateScope(const Scope& scope) {
    startSoftScope();
    generateStatementList(&scope.scopeStatements);
    endSoftScope();
}

void CodeGen::generateStatementList(const StatementList *statementList) {
    while (statementList) {
        generateStatement(statementList->curr);
        statementList = statementList->next;
    }
}

void CodeGen::startSoftScope() {
    StackItem stackItem {
        .marker = StackMarkerType::SOFT_SCOPE_START,
        .type = StackItemType::MARKER
    };
    stackItems.emplace_back(stackItem);
}

void CodeGen::endSoftScope() {
    uint32_t stackUsage = 0;
    while (!stackItems.empty()) {
        if (
            stackItems.back().type == StackItemType::MARKER &&
            stackItems.back().marker == StackMarkerType::SOFT_SCOPE_START
        ) {
            stackItems.pop_back();
            break;
        }
        // TODO: run destructor for each item within this scope
        stackUsage += getSizeOfType(getTypeFromTokenList(stackItems.back().variable.varDec.type));
        stackItems.pop_back();
        assert(!stackItems.empty()); // paired with marker comment above
    }
    efficientImmAddOrSub(stackPointerIndex, stackUsage, OpCode::ADD);
}


// ========================================
// FUNCTIONS
// ========================================

void CodeGen::generateFunctionDeclaration(const FunctionDec& funcDec) {
    assert(stackItems.empty());
    addFunctionSignatureToVirtualStack(funcDec);
    // have checker add empty return statement for void functions
    functionDecPointerToIndex[&funcDec] = byteCode.size();
    generateStatementList(&funcDec.body.scopeStatements);
    stackItems.clear();
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
            // const uint32_t currStack = stackItems.size();
            generateStatement(controlFlowStatement.forLoop->initialize);
            const uint64_t startOfLoopIndex = byteCode.size();
            const uint32_t loopJumpMarkersFrom = jumpMarkers.size();
            const BranchStatementResult res = generateBranchStatement(controlFlowStatement.forLoop->statement);
            generateExpression(controlFlowStatement.forLoop->iteration);
            addJumpMarker(JumpMarkerType::TO_LOOP_START);
            addJumpOp(OpCode::RS_JUMP);
            if (res == BranchStatementResult::ADDED_JUMP) {
                updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END, loopJumpMarkersFrom);
            }
            updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_LOOP_END, loopJumpMarkersFrom);
            updateJumpMarkersTo(startOfLoopIndex, JumpMarkerType::TO_LOOP_START, loopJumpMarkersFrom);
            if (controlFlowStatement.forLoop->initialize.type == StatementType::VARIABLE_DEC) {
                assert(nameToStackItemIndex[tk->extractToken(controlFlowStatement.forLoop->initialize.varDec->name)] == stackItems.size() - 1);
                fakeClearStackFromTo(stackItems.size() - 1, stackItems.size() - 2);
                stackItems.pop_back();
            }
            break;
        }
        case ControlFlowStatementType::WHILE_LOOP: {
            const uint64_t startOfLoopIndex = byteCode.size();
            const uint32_t loopJumpMarkersFrom = jumpMarkers.size();
            BranchStatement& whileLoop = controlFlowStatement.whileLoop->statement;
            const BranchStatementResult res = generateBranchStatement(whileLoop);
            // place unconditional jump at the end of the while loop to go to start
            addJumpMarker(JumpMarkerType::TO_LOOP_START);
            addJumpOp(OpCode::RS_JUMP);
            if (res == BranchStatementResult::ADDED_JUMP) {
                updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END, loopJumpMarkersFrom);
            }
            // update jump markers added by break / continue
            updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_LOOP_END, loopJumpMarkersFrom);
            updateJumpMarkersTo(startOfLoopIndex, JumpMarkerType::TO_LOOP_START, loopJumpMarkersFrom);
            break; 
        }
        case ControlFlowStatementType::CONDITIONAL_STATEMENT: {
            const BranchStatement& ifStatement = controlFlowStatement.conditional->ifStatement;
            const ElifStatementList *elifStatementList = controlFlowStatement.conditional->elifStatement;
            const Scope* elseStatement = controlFlowStatement.conditional->elseStatement;
            const uint32_t ifChainJumpMarkersFrom = jumpMarkers.size();
            // addJumpMarker(JumpMarkerType::IF_CHAIN_START); // mark start of this if/elif/else chain

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
                    updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END, ifChainJumpMarkersFrom);
                }
            }

            // elif statements
            while (elifStatementList) {
                const uint32_t markersStart = jumpMarkers.size();
                const BranchStatementResult res = generateBranchStatement(elifStatementList->elif);
                elifStatementList = elifStatementList->next;
                if (elifStatementList || elseStatement) {
                    // elif/else statements follow, add a jump after the this so that we skip the rest
                    addJumpMarker(JumpMarkerType::TO_IF_CHAIN_END);
                    addJumpOp(OpCode::RS_JUMP);
                }
                if (res == BranchStatementResult::ADDED_JUMP) {
                    // update elif false condition jump to go to code after the branch
                    updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_BRANCH_END, markersStart);
                }
            }

            // else statement
            if (elseStatement) {
                generateScope(*elseStatement);
            }

            // update all TO_IF_CHAIN_END jump ops until IF_CHAIN_START to go to current index
            updateJumpMarkersTo(byteCode.size(), JumpMarkerType::TO_IF_CHAIN_END, ifChainJumpMarkersFrom);
            break;
        }
        case ControlFlowStatementType::RETURN_STATEMENT: {
            generateReturnStatement(*controlFlowStatement.returnStatement);
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
    // TODO: call destructor for all items in function scope, this includes function parameters
    // don't clear the virtual stack though, still need it to potentially generate the rest of the function

    // find where return address is
    const uint32_t returnAddressStackIndex = nameToStackItemIndex[STACK_RETURN_ADDRESS_IDENTIFIER];
    // const StackItem& returnAddressItem = stackItems[returnAddressStackIndex];
    assert(stackItems.size() > 1);
    fakeClearStackFromTo(stackItems.size() - 1, returnAddressStackIndex);
    addBytes({{(bc)OpCode::POP_Q, miscRegisterIndex}});
    const uint32_t returnValueStackIndex = nameToStackItemIndex[STACK_RETURN_VALUE_IDENTIFIER];
    assert(stackItems[returnAddressStackIndex].positionOnStack >= 8);
    // since we used POP_Q above, we remove 8 (q word size) from the position so that the clear function
    // adds the right amount to the stack pointer
    stackItems[returnAddressStackIndex].positionOnStack -= 8;
    fakeClearStackFromTo(returnAddressStackIndex, returnValueStackIndex);
    stackItems[returnAddressStackIndex].positionOnStack += 8;
    addBytes({{(bc)OpCode::JUMP, miscRegisterIndex}});
}


// ========================================
// STACK MANAGEMENT
// ========================================

/**
 * 
 * \returns position of item
*/
uint32_t CodeGen::getPositionPushingTypeToStack(Token typeToken, uint32_t alignTo) {
    if (typeToken.type == TokenType::IDENTIFIER) {
        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
        const StructInformation &structInfo = structLookUp[genDec->structDec];
        if (!alignTo) {
            alignTo = structInfo.alignTo;
        }
        return getPositionPushingToStack(structInfo.size, alignTo);
    }
    uint32_t sizeOfParameter = getSizeOfBuiltinType(typeToken.getType());
    if (!alignTo) {
        alignTo = sizeOfParameter;
    }
    else if (alignTo > sizeOfParameter) {
        sizeOfParameter = alignTo;
    }
    return getPositionPushingToStack(sizeOfParameter, alignTo);
}

uint32_t CodeGen::getPositionPushingStuctToStack(const StructInformation &structInfo) {
    return getPositionPushingToStack(structInfo.size, structInfo.alignTo);
}

uint32_t CodeGen::getPositionPushingToStack(const uint32_t sizeOfType, const uint32_t alignTo) {
    const uint32_t curr_sp_position = getCurrStackPointerPosition();
    return curr_sp_position + getPaddingNeeded(curr_sp_position, sizeOfType, alignTo) + sizeOfType;
}

void CodeGen::addSpaceToStack(uint32_t space) {
    efficientImmAddOrSub(stackPointerIndex, space, OpCode::SUB);
}

StackVariable& CodeGen::addVarDecToVirtualStack(const VariableDec& varDec) {
    StackItem stackItem {
        .variable = {
            .varDec = varDec,
            .positionOnStack = getPositionPushingTypeToStack(getTypeFromTokenList(varDec.type)),
        },
        .type = StackItemType::VARIABLE,
    };
    nameToStackItemIndex[tk->extractToken(varDec.name)] = stackItems.size();
    stackItems.emplace_back(stackItem);
    return stackItems.back().variable;
}

uint32_t CodeGen::getCurrStackPointerPosition() {
    auto iter = stackItems.rbegin();
    while (iter != stackItems.rend()) {
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

/**
 * 
*/
uint32_t CodeGen::addExpressionResToStack(ExpressionResult& expRes) {
    const TokenList* type = expRes.type;
    if (expRes.isPointerToValue) {
        type = getNextFromTokenList(*type);
    } else if (!expRes.isReg) {
        moveImmToReg(allocateRegister(), expRes);
    }
    const Token typeToken = getTypeFromTokenList(*type);
    const uint32_t spPosition = getCurrStackPointerPosition();
    if (typeToken.getType() != TokenType::IDENTIFIER) {
        const uint32_t size = getSizeOfBuiltinType(typeToken.getType());
        const uint32_t padding = getPaddingNeeded(spPosition, size, size);
        addSpaceToStack(padding);
        if (!expRes.isPointerToValue) {
            addBytes({{(bc)getPushOpForSize(size), expRes.getReg()}});
        }
        else {
            addBytes({{(bc)getLoadOpForSize(size), miscRegisterIndex, expRes.getReg()}});
            addBytes({{(bc)getPushOpForSize(size), miscRegisterIndex}});
        }
        if (expRes.isTemp) {
            freeRegister(expRes.getReg());
        }
        return padding + size;
    } else {
        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
        const StructInformation& structInfo = structLookUp[genDec->structDec];
        uint32_t size = structInfo.size;
        const uint32_t padding = getPaddingNeeded(spPosition, size, structInfo.alignTo);
        if (!expRes.isPointerToValue) {
            addSpaceToStack(padding);
            addBytes({{(bc)getPushOpForSize(size), expRes.getReg()}});
        }
        else if (getLoadOpForSize(size) != OpCode::NOP) {
            addBytes({{(bc)getLoadOpForSize(size), miscRegisterIndex, expRes.getReg()}});
            addBytes({{(bc)getPushOpForSize(size), miscRegisterIndex}});
        }
        else {
            addSpaceToStack(padding + size);
            addBytes({{(bc)OpCode::PUSH_D, miscRegisterIndex}}); // add space for return value
            addBytes({{(bc)OpCode::PUSH_D, stackPointerIndex}}); // add destination
            addBytes({{(bc)OpCode::PUSH_D, expRes.getReg()}}); // add source
            ExpressionResult sizeExp;
            sizeExp.set(size);
            addExpressionResToStack(sizeExp); // add size
            addBytes({{(bc)OpCode::CALL_B, (bc)BuiltInFunction::MEM_COPY}});
            addBytes({{(bc)OpCode::POP_Q, miscRegisterIndex}}); // pop return value
        }
        if (expRes.isTemp) {
            freeRegister(expRes.getReg());
        }
        return padding + size;
    }
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
    StackItem returnValue {
        .positionOnStack = funcDec.returnType.token.type != TokenType::VOID ? 
            getPositionPushingTypeToStack(getTypeFromTokenList(funcDec.returnType), 8) : 0,
        .type = StackItemType::RETURN_VALUE,
    };
    nameToStackItemIndex[STACK_RETURN_VALUE_IDENTIFIER] = stackItems.size();
    stackItems.emplace_back(returnValue);

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
        .positionOnStack = getPositionPushingTypeToStack(Token{0, 0, TokenType::POINTER}),
        .type = StackItemType::RETURN_ADDRESS,
    };
    nameToStackItemIndex[STACK_RETURN_ADDRESS_IDENTIFIER] = stackItems.size();
    stackItems.emplace_back(returnAddress);
}

/**
 * Generates the byte code needed to clear the stack at index 'from' until 'to'
 * Does not remove any items from the virtual stack
 * 
 * Have to be careful using this since we have to pass the indexes in
 * For example, when we want to do a return statement, we clear the stack until the return address index
 * pop the return address off, clear the rest of the stack (function arguments) until the return value index, and then return.
 * 
 * \param from the index to start from
 * \param to the index to go to
*/
void CodeGen::fakeClearStackFromTo(const uint32_t from, const uint32_t to) {
    // this for loop is going in the wrong order for destruction
    // must be reverse order of placed on stack, not same order
    // for  (uint32_t i = from; i > to; --i) {
    //   if (stackItems[i].type != StackItemType::VARIABLE) {
    //     continue;
    //   }
    //   // call destructor for each variable
    // }
    assert(from < stackItems.size() && to < stackItems.size() && from >= to);
    const uint32_t stackUsage = getPositionOnStack(from) - getPositionOnStack(to);
    efficientImmAddOrSub(stackPointerIndex, stackUsage, OpCode::ADD);
}

uint32_t CodeGen::getPositionOnStack(uint32_t stackItemIndex) {
    StackItem& stackItem = stackItems[stackItemIndex];
    switch (stackItem.type) {
        case StackItemType::NONE: { assert(false); break; }
        case StackItemType::MARKER: { assert(false); break; }
        case StackItemType::VARIABLE: { return stackItem.variable.positionOnStack; }
        case StackItemType::RETURN_ADDRESS: 
        case StackItemType::RETURN_VALUE: { return stackItem.positionOnStack; }
        case StackItemType::TEMP_VALUE: { return stackItem.tempValue.positionOnStack; }
    }
    exit(1);
}

uint32_t CodeGen::getVarOffsetFromSP(const StackVariable &variableInfo) {
    return getOffsetFromSP(variableInfo.positionOnStack);
}

uint32_t CodeGen::getOffsetFromSP(uint32_t positionOnStack) {
    // |                   | <- sp
    // |       | <- positionOnStack
    // | value |
    return getCurrStackPointerPosition() - positionOnStack;
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
    assert(
        regNum != stackPointerIndex &&
        regNum != instructionPointerIndex && 
        regNum != miscRegisterIndex &&
        regNum != dataPointerIndex
    );
    RegisterInfo& regInfo = registers[regNum];
    regInfo.changed = false;
    regInfo.inUse = false;
}

uint32_t CodeGen::getSizeOfType(Token typeToken) {
    if (typeToken.getType() == TokenType::IDENTIFIER) {
        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
        const StructInformation& structInfo = structLookUp[genDec->structDec];
        return structInfo.size;
    }
    return getSizeOfBuiltinType(typeToken.getType());
}

/**
 * 
*/
void CodeGen::efficientImmAddOrSub(bytecode_t reg, uint64_t val, OpCode op) {
    assert(op == OpCode::SUB || op == OpCode::ADD);
    OpCode small, imm;
    if (op == OpCode::SUB) {
        // dec
        small = OpCode::DEC;
        imm = OpCode::SUB_I;
    } else {
        small = OpCode::INC;
        imm = OpCode::ADD_I;
    }
    if (val == 1) {
        addBytes({{(bc)small, reg}});
    } else if (val) {
        ExpressionResult paddingToAdd;
        paddingToAdd.set(val);
        ExpressionResult regExp;
        regExp.setReg(reg);
        regExp.type = &Checker::uint64Value;
        expressionResWithOp(op, imm, regExp, paddingToAdd);
        if (paddingToAdd.isTemp) {
            freeRegister(paddingToAdd.getReg());
        }
    }
}


constexpr OpCode getLoadOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::LOAD_B;
        case 2: return OpCode::LOAD_W;
        case 4: return OpCode::LOAD_D;
        case 8: return OpCode::LOAD_Q;
        default: return OpCode::NOP;
    }
}

constexpr OpCode getStoreOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::STORE_B;
        case 2: return OpCode::STORE_W;
        case 4: return OpCode::STORE_D;
        case 8: return OpCode::STORE_Q;
        default: return OpCode::NOP;
    }
}

constexpr OpCode getPushOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::PUSH_B;
        case 2: return OpCode::PUSH_W;
        case 4: return OpCode::PUSH_D;
        case 8: return OpCode::PUSH_Q;
        default: return OpCode::NOP;
    }
}

constexpr OpCode getPopOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::POP_B;
        case 2: return OpCode::POP_W;
        case 4: return OpCode::POP_D;
        case 8: return OpCode::POP_Q;
        default: return OpCode::NOP;
    }
}


// ========================================
// PRINTING
// ========================================

std::ostream& operator<<(std::ostream& os, const StackMarkerType& obj) {
    switch (obj) {
        case StackMarkerType::NONE: { os << "NONE"; break; }
        case StackMarkerType::SOFT_SCOPE_START: { os << "SOFT_SCOPE_START"; break; }
        case StackMarkerType::HARD_SCOPE_START: { os << "HARD_SCOPE_START"; break; }
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const StackVariable& obj) {
    os << obj.varDec << '\n';
    os << "Position on stack: " << obj.positionOnStack;
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
        case StackItemType::TEMP_VALUE: {
            os << "TEMP_VALUE\n";
            os << "Temp Value Position: " << obj.tempValue.positionOnStack;
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

