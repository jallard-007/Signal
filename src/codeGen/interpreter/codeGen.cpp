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

JumpMarker::JumpMarker(uint64_t start, JumpMarkerType type):
    start{start}, type{type} {}

CodeGen::CodeGen(
    Program& program,
    std::vector<Tokenizer>& tokenizers,
    std::unordered_map<std::string, GeneralDec *>& lookUp,
    std::unordered_map<const StructDec *, StructInformation>& structLookup
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
    TokenType rightSideExpType = exp.value.type->exp.getToken().getType();
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
            val = *(int32_t*)exp.value.getData();
        } else {
            val = *(int64_t*)exp.value.getData();
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
            val = *(uint32_t*)exp.value.getData();
        } else {
            val = *(uint64_t*)exp.value.getData();
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
        add8ByteNum(*(uint64_t *)exp.value.getData());
    } else {
        alignForImm(2, 4);
        addBytes({{(bc)OpCode::MOVE_I, reg}});
        add4ByteNum(*(uint32_t *)exp.value.getData());
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
 * Updates jump markers with destination. Sets type of each updated marker to JumpMarkerType::SET
 * \param destination the destination
 * \param type type of jump marker to update
 * \param from update markers from this index within the jumper markers container until the end
*/
void CodeGen::updateJumpMarkersTo(const uint64_t destination, JumpMarkerType type, uint32_t from) {
    for (auto iter = std::next(jumpMarkers.begin(), from); iter != jumpMarkers.end(); ++iter) {
        JumpMarker& jumpMarker = *iter;
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
        for (auto iter = jumpMarkers.begin(); iter != jumpMarkers.end(); ++iter) {
            JumpMarker& jumpMarker = *iter;
            // only updated set jump markers
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
            assert(minImmSize == 4 && minImmSize > jumpOpImmSize);
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

            for (auto subIter = jumpMarkers.begin(); subIter != jumpMarkers.end(); ++subIter) {
                JumpMarker& otherJumpMarker = *subIter;
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
    for (auto iter = jumpMarkers.begin(); iter != jumpMarkers.end();) {
        JumpMarker& jumpMarker = *iter;
        if (jumpMarker.type != JumpMarkerType::SET) {
            ++iter;
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
            std::cerr << "Compiler error, unsupported relative jump immediate size\n";
            exit(1);
        }
        iter = jumpMarkers.erase(iter);
    }
}

void CodeGen::writeFunctionJumpOffsets() {
    auto iter = jumpMarkers.begin();
    while (iter != jumpMarkers.end()) {
        JumpMarker& jumpMarker = *iter;
        if (jumpMarker.type != JumpMarkerType::FUNCTION_CALL || !functionDecPointerToIndex.contains(jumpMarker.funcDec)) {
            ++iter;
            continue;
        }
        const uint32_t functionIndex = functionDecPointerToIndex[jumpMarker.funcDec];
        assert(jumpMarker.start < UINT32_MAX);
        const int32_t offset = functionIndex - jumpMarker.start;
        *(int32_t *)&byteCode[jumpMarker.start + 1] = (int32_t)offset;
        iter = jumpMarkers.erase(iter);
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
            generateStructDeclaration(*genDec.structDec);
            break;
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
void CodeGen::generateVariableDeclaration(VariableDec& varDec, bool initialize) {
    const Token typeToken = getTypeFromTokenList(varDec.type);
    if (typeToken.getType() == TokenType::REFERENCE) {
        // TODO: implement as pointer to result? 
    }
    else if (isBuiltInType(typeToken.getType())) {
        ExpressionResult expRes;
        if (initialize && varDec.initialAssignment) {
            expRes = generateExpression(*varDec.initialAssignment);
            if (!expRes.isReg) {
                moveImmToReg(allocateRegister(), expRes);
            } else if (expRes.isPointerToValue) {
                bytecode_t reg = allocateRegister();
                expRes = loadValueFromPointer(expRes, reg);
            }
        }
        addVarDecToStack(varDec);
        const uint32_t size = getSizeOfBuiltinType(typeToken.getType());
        assert(size >= 1 && size <= 8);
        if (!expRes.isReg) {
            efficientImmAddOrSub(stackPointerIndex, size, OpCode::SUB);
            return;
        }
        const OpCode pushOp = getPushOpForSize(size);
        assert(pushOp != OpCode::NOP);
        addBytes({{(bc)pushOp, expRes.getReg()}});
        if (expRes.isTemp) {
            freeRegister(expRes.getReg());
        }
    }
    else if (typeToken.getType() == TokenType::IDENTIFIER) {
        // TODO:
        // valid initial assignment can be an initializer list
        // need special generator for that
        const uint32_t beforePos = getCurrStackPointerPosition();
        addVarDecToStack(varDec, 0, true);
        addSpaceToStack(getCurrStackPointerPosition() - beforePos);
    }
    // TODO: add array support
    else {
        assert(false);
    }
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

ExpressionResult CodeGen::generateExpressionContainerLiteral(const ContainerLiteral &containerLiteral) {
    if (containerLiteral.values.next) {
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
    Tokenizer* oldTk = tk;
    tk = &tokenizers[generalDec->tokenizerIndex];
    FunctionDec* p_funcDec;
    if (generalDec->type == GeneralDecType::BUILTIN_FUNCTION) {
        p_funcDec = &generalDec->builtinFunc->funcDec;
    } else {
        p_funcDec = generalDec->funcDec;
    }

    // save any registers currently in use
    bool returnRegisterUsed = false;
    {
        for (bytecode_t reg = 0; reg < NUM_REGISTERS; ++reg) {
            if (isReservedRegister(reg) || !registers[reg].inUse) {
                continue;
            }
            if (reg == returnRegisterIndex) {
                returnRegisterUsed = true;
            }
            ExpressionResult regVal;
            regVal.setReg(reg);
            regVal.value.type = &TokenListTypes::uint64Value;
            addExpressionResToStack(regVal, StackItemType::SAVED_TEMPORARY);
        }
    }

    // the function will reset the stack to this point
    uint32_t resetTo = stackItems.size();

    // add arguments to stack
    if (functionCall.args.curr.getType() != ExpressionType::NONE) {
        const ExpressionList *expressionList = &functionCall.args;
        ExpressionResult expRes = generateExpression(expressionList->curr);
        addExpressionResToStack(expRes, StackItemType::ARGUMENT, 8);
        expressionList = expressionList->next;
        while (expressionList) {
            ExpressionResult expRes = generateExpression(expressionList->curr);
            addExpressionResToStack(expRes, StackItemType::ARGUMENT);
            expressionList = expressionList->next;
        }
    }

    // add function call
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
        // add padding to align return address
        addPaddingToStack(getPaddingNeeded(getCurrStackPointerPosition(), 8, 8));
        alignForImm(1, 4);
        // register this position to be updated
        JumpMarker& jumpMarker = addJumpMarker(JumpMarkerType::FUNCTION_CALL);
        jumpMarker.funcDec = p_funcDec;
        addByteOp(OpCode::CALL);
        add4ByteNum(0);
    }

    // pop all arguments off
    if (stackItems.size() > resetTo && stackItems.at(resetTo).type == StackItemType::PADDING) {
        ++resetTo;
    }
    while (stackItems.size() > resetTo) {
        stackItems.pop_back();
    }

    ExpressionResult returnValueExpression;
    // return value is in returnRegisterIndex
    // if returnRegisterIndex register was being used before, move it to a different reg
    if (returnRegisterUsed) {
        const bytecode_t moveTo = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, moveTo, returnRegisterIndex}});
        returnValueExpression.setReg(moveTo);
    } else {
        returnValueExpression.setReg(returnRegisterIndex);
    }
    returnValueExpression.value.type = &p_funcDec->returnType;

    // put saved values back into their expected registers (reverse of pushed)
    for (int16_t reg = NUM_REGISTERS - 1; reg >= 0; --reg) {
        if (isReservedRegister(reg) || !registers[reg].inUse) {
            continue;
        }
        assert(stackItems.back().type == StackItemType::SAVED_TEMPORARY);
        popValue(TokenListTypes::uint64Value, reg);
    }

    tk = oldTk;

    return returnValueExpression;
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
    TokenType rightSideExpType = right.value.type->exp.getToken().getType();
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
            val = *(int32_t*)right.value.getData();
        } else {
            val = *(int64_t*)right.value.getData();
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
            val = *(uint32_t*)right.value.getData();
        } else {
            val = *(uint64_t*)right.value.getData();
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
        add8ByteNum(*(uint64_t *)right.value.getData());
    } else {
        alignForImm(2, 4);
        addBytes({{(bc)OpCode::MOVE_I, miscRegisterIndex}});
        add4ByteNum(*(uint32_t *)right.value.getData());
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
                type = getTypeFromTokenList(*getNextFromTokenList(*expRes.value.type));
            } else {
                type = getTypeFromTokenList(*expRes.value.type);
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
            expRes.value.type = &stackVar.varDec.type;
            if (expRes.value.type->exp.getToken().getType() == TokenType::REFERENCE) {
                // always move past references
                expRes.value.type = expRes.value.type->next;
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
    switch (token.getType()) {
        case TokenType::CHAR_LITERAL: {
            return expRes;
        }
        case TokenType::STRING_LITERAL: { 
            // place string literal in data section of bytecode, return offset to start of string
            expRes.value.type = &TokenListTypes::stringValue;
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
                    expRes.value.set(i);
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
            expRes.value.set(num);
            if (num <= INT32_MAX) {
                expRes.value.type = &TokenListTypes::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.value.type = &TokenListTypes::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.value.type = &TokenListTypes::int64Value;
            } // already set to uint64_t
            return expRes;
        }
        case TokenType::BINARY_NUMBER: {
            assert(token.getLength() > 2);
            std::string binaryNumber = tk->extractToken(Token{token.getPosition() + 2, (uint16_t)(token.getLength() - 2), TokenType::BINARY_NUMBER});
            uint64_t num = std::stoull(binaryNumber, nullptr, 2);
            expRes.value.set(num);
            if (num <= INT32_MAX) {
                expRes.value.type = &TokenListTypes::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.value.type = &TokenListTypes::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.value.type = &TokenListTypes::int64Value;
            } // already set to uint64_t
            return expRes;
        }
        case TokenType::FLOAT_NUMBER: {
            std::string binaryNumber = tk->extractToken(token);
            double num = std::stod(binaryNumber);
            expRes.value.set(num);
            return expRes;
        }
        case TokenType::HEX_NUMBER: { 
            assert(token.getLength() > 2);
            std::string hexNumber = tk->extractToken(Token{token.getPosition() + 2, (uint16_t)(token.getLength() - 2), TokenType::HEX_NUMBER});
            /*
            std::invalid_argument
            std::out_of_range
            */
            uint64_t num = std::stoull(hexNumber, nullptr, 16);
            expRes.value.set(num);
            if (num <= INT32_MAX) {
                expRes.value.type = &TokenListTypes::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.value.type = &TokenListTypes::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.value.type = &TokenListTypes::int64Value;
            } // already set to uint64_t
            return expRes;
        }
        case TokenType::FALSE: {
            expRes.value.set(false);
            return expRes;
        }
        case TokenType::TRUE: {
            expRes.value.set(true);
            return expRes;
        }
        case TokenType::STDIN: {
            expRes.isPointerToValue = true;
            expRes.setReg(allocateRegister());
            expRes.isTemp = true;
            addBytes({{(bc)OpCode::MOVE, expRes.getReg(), dataPointerIndex}});
            efficientImmAddOrSub(expRes.getReg(), stdinDataIndex, OpCode::ADD);
            expRes.value.type = &TokenListTypes::fileValue;
            return expRes;
        }
        case TokenType::STDERR: {
            expRes.isPointerToValue = true;
            expRes.setReg(allocateRegister());
            expRes.isTemp = true;
            addBytes({{(bc)OpCode::MOVE, expRes.getReg(), dataPointerIndex}});
            efficientImmAddOrSub(expRes.getReg(), stderrDataIndex, OpCode::ADD);
            expRes.value.type = &TokenListTypes::fileValue;
            return expRes;
        }
        case TokenType::STDOUT: {
            expRes.isPointerToValue = true;
            expRes.setReg(allocateRegister());
            expRes.isTemp = true;
            addBytes({{(bc)OpCode::MOVE, expRes.getReg(), dataPointerIndex}});
            efficientImmAddOrSub(expRes.getReg(), stdoutDataIndex, OpCode::ADD);
            expRes.value.type = &TokenListTypes::fileValue;
            return expRes;
        }
        case TokenType::NULL_PTR: {
            expRes.value.type = &TokenListTypes::ptrValue;
            return expRes;
        }
        case TokenType::IDENTIFIER: {
            const std::string& identifier = tk->extractToken(token);
            const uint32_t stackItemIndex = nameToStackItemIndex[identifier];
            const StackVariable &variableInfo = stackItems[stackItemIndex].variable;
            const bytecode_t resReg = allocateRegister();
            ExpressionResult pointerExp;
            pointerExp.isPointerToValue = true;
            pointerExp.value.type = &variableInfo.varDec.type;
            const uint32_t varOffset = getVarOffsetFromSP(variableInfo);
            if (varOffset) {
                pointerExp.setReg(allocateRegister());
                addBytes({{(bc)OpCode::MOVE, pointerExp.getReg(), stackPointerIndex}});
                efficientImmAddOrSub(pointerExp.getReg(), varOffset, OpCode::ADD);
            } else {
                pointerExp.setReg(stackPointerIndex);
                pointerExp.isTemp = false;
            }
            expRes = loadValueFromPointer(pointerExp, resReg);
            if (expRes.getReg() == resReg) {
                if (pointerExp.isTemp) {
                    freeRegister(pointerExp.getReg());
                }
            } // else failed to load, type too big
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
ExpressionResult CodeGen::loadValueFromPointer(const ExpressionResult &pointerExp, bytecode_t reg) {
    assert(pointerExp.isReg);
    assert(pointerExp.isPointerToValue);
    TokenList* type = pointerExp.value.type;
    const Token typeToken = getTypeFromTokenList(*type);
    const uint32_t sizeOfType = getSizeOfType(typeToken);
    if (sizeOfType > 8) {
        // cannot be loaded into a register
        return pointerExp;
    }
    ExpressionResult expRes;
    expRes.setReg(reg);
    expRes.value.type = type;
    const OpCode loadOp = getLoadOpForSize(sizeOfType);
    if (loadOp != OpCode::NOP) {
        addBytes({{(bc)loadOp, expRes.getReg(), pointerExp.getReg()}});
        return expRes;
    }
    // size can be one of 3, 5, 6, 7
    assert(sizeOfType == 3 || sizeOfType == 5 || sizeOfType == 6 || sizeOfType == 7);
    bytecode_t pointerReg = pointerExp.getReg();
    if (!pointerExp.isTemp) {
        pointerReg = allocateRegister();
        addBytes({{(bc)OpCode::MOVE, pointerReg, pointerExp.getReg()}});
    }
    uint32_t leftToLoad = sizeOfType;
    if (leftToLoad >= 4) {
        addBytes({{(bc)OpCode::LOAD_D, reg, pointerReg}});
        addBytes({{(bc)OpCode::ADD_I, pointerReg, 4, 0}});
        leftToLoad -= 4;
    }
    if (leftToLoad >= 2) {
        leftToLoad -= 2;
        if ((sizeOfType < 4)) {
            addBytes({{(bc)OpCode::LOAD_W, reg, pointerReg}});
        } else {
            addBytes({{(bc)OpCode::LOAD_W, miscRegisterIndex, pointerReg}});
            addBytes({{(bc)OpCode::SHIFT_L_I, miscRegisterIndex, 32, 0}});
            addBytes({{(bc)OpCode::OR, reg, miscRegisterIndex}});
        }
        if (leftToLoad) {
            addBytes({{(bc)OpCode::ADD_I, pointerReg, 2, 0}});
        }
    }
    if (leftToLoad == 1) {
        addBytes({{(bc)OpCode::LOAD_B, miscRegisterIndex, pointerReg}});
        addBytes({{(bc)OpCode::SHIFT_L_I, miscRegisterIndex, (bc)((sizeOfType - 1) * 8), 0}});
        addBytes({{(bc)OpCode::OR, reg, miscRegisterIndex}});
        --leftToLoad;
    }
    assert(leftToLoad == 0);
    if (!pointerExp.isTemp) {
        freeRegister(pointerReg);
    }
    return expRes;
}

/**
 * Stores data
*/
void CodeGen::storeValueToPointer(const ExpressionResult &pointerExp, ExpressionResult &valueExp) {
    assert(pointerExp.isReg);
    assert(pointerExp.isPointerToValue);
    if (!valueExp.isReg) {
        moveImmToReg(allocateRegister(), valueExp);
    }
    const TokenList* type = pointerExp.value.type;
    const Token typeToken = getTypeFromTokenList(*type);
    const uint32_t sizeOfType = getSizeOfType(typeToken);
    const OpCode storeOp = getStoreOpForSize(sizeOfType);
    assert(storeOp != OpCode::NOP);
    addBytes({{(bc)storeOp, pointerExp.getReg(), valueExp.getReg()}});
}

void CodeGen::copyValue(ExpressionResult &pointerExp, ExpressionResult &valueExp) {
    (void)pointerExp;
    (void)valueExp;
}


/**
 * Adds to a pointer based on type size
 * \param pointerExp the pointer expression to add to. Must be a pointer type
 * \param indexExp the index expression, can be any integral value
*/
void CodeGen::doPointerIndex(const ExpressionResult &pointerExp, ExpressionResult &indexExp) {
    assert(isSigned(getTypeFromTokenList(*indexExp.value.type).getType()) || isUnsigned(getTypeFromTokenList(*indexExp.value.type).getType()));
    assert(getTypeFromTokenList(*pointerExp.value.type).getType() == TokenType::POINTER);
    const Token nextType = getTypeFromTokenList(*getNextFromTokenList(*pointerExp.value.type));
    uint32_t sizeOfType = getSizeOfType(nextType);
    if (sizeOfType > 1) {
        if (!indexExp.isReg) {
            LiteralValue sizeLiteral;
            sizeLiteral.set(sizeOfType);
            indexExp.value = evaluateBinOpImmExpression(TokenType::MULTIPLICATION, indexExp.value, sizeLiteral);
        } else {
            ExpressionResult sizeExp;
            sizeExp.value.set(sizeOfType);
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
        assert(false); // should be handled by checker
        // both operands are immediate values, return the result
        // return evaluateBinOpImmExpression(binOp.op.getType(), leftResult, rightResult);
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
    const bytecode_t reg = allocateRegister();
    ExpressionResult leftValue = loadValueFromPointer(leftResult, reg);
    assert(leftValue.getReg() == reg);
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
    if (binOp.op.getType() == TokenType::LOGICAL_AND || binOp.op.getType() == TokenType::LOGICAL_OR) {
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
            LiteralValue zeroExp;
            zeroExp.set((int64_t)0);
            leftResult.value = evaluateBinOpImmExpression(TokenType::NOT_EQUAL, leftResult.value, zeroExp);
            if (!*(bool*)leftResult.value.getData()) {
                // for &&, expression is false, don't generate anymore
                if (binOp.op.getType() == TokenType::LOGICAL_AND) {
                    return leftResult;
                }
                // for ||, need to check next, but can remove left side
            } else {
                // for ||, expression is true, don't generate anymore
                if (binOp.op.getType() == TokenType::LOGICAL_OR) {
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
            if (binOp.op.getType() == TokenType::LOGICAL_AND) {
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
                assert(false); // handled by checker
                // both sides are immediate
                // return evaluateBinOpImmExpression(binOp.op.getType(), leftResult, rightResult);
            }
            // left result is not an immediate
            byteCode.resize(shortCircuitIndexStart);
            LiteralValue zeroExp;
            zeroExp.set((int64_t)0);
            zeroExp.type = &TokenListTypes::int32Value;
            rightResult.value = evaluateBinOpImmExpression(TokenType::NOT_EQUAL, rightResult.value, zeroExp);
            if (!*(bool*)rightResult.value.getData()) {
                // for &&, cond is always false
                if (binOp.op.getType() == TokenType::LOGICAL_AND) {
                    return rightResult;
                }
                // for ||, depends on left side
            } else {
                // for ||, cond is always true
                if (binOp.op.getType() == TokenType::LOGICAL_AND) {
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
                assert(false); // handled by checker
                // return evaluateBinOpImmExpression(binOp.op.getType(), leftResult, rightResult);
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
    switch (binOp.op.getType()) {
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
            std::cerr << "Invalid token type in BinOp expression [" << (int32_t)binOp.op.getType() << "]\n";
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
        assert(false); // handled by checker
        // evaluate imm
        // return evaluateUnaryOpImmExpression(unOp.op.getType(), expRes);
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
    const bytecode_t reg = allocateRegister();
    ExpressionResult dataExp = loadValueFromPointer(addressExp, reg);
    assert(dataExp.getReg() == reg);
    ExpressionResult incrementExp;
    OpCode opCode;
    if (op == TokenType::DECREMENT_PREFIX || op == TokenType::DECREMENT_POSTFIX) {
        opCode = OpCode::DEC;
        incrementExp.value.set(-1);
    } else {
        opCode = OpCode::INC;
        incrementExp.value.set(1);
    }
    const Token typeToken = getTypeFromTokenList(*dataExp.value.type);
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
            assert(getTypeFromTokenList(*expRes.value.type).getType() == TokenType::POINTER);
            expRes.value.type = getNextFromTokenList(*expRes.value.type);
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

void CodeGen::generateStructDeclaration(const StructDec& structDec) {
    const StructDecList* structDecList = &structDec.decs;
    while (structDecList) {
        switch (structDecList->type) {
            case StructDecType::NONE: {
                break;
            }
            case StructDecType::FUNC: {
                generateFunctionDeclaration(*structDecList->funcDec);
                break;
            }
            case StructDecType::VAR: {
                break;
            }
        }
        structDecList = structDecList->next;
    }
}


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
            ExpressionResult exp = generateExpression(*statement.expression);
            if (exp.isTemp) {
                freeRegister(exp.getReg());
            }
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
            if (statement.keyword.getType() == TokenType::BREAK) {
                addJumpMarker(JumpMarkerType::TO_LOOP_END);
                addJumpOp(OpCode::RS_JUMP);
            }
            else if (statement.keyword.getType() == TokenType::CONTINUE) {
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
            {
                const ExpressionResult iterExp = generateExpression(controlFlowStatement.forLoop->iteration);
                if (iterExp.isTemp) {
                    freeRegister(iterExp.getReg());
                }
            }
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
            if (*(uint32_t *)expRes.value.getData()) {
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
    // generate return expression
    ExpressionResult returnValueExp = generateExpression(returnStatement.returnValue);
    // clear until return address
    const uint32_t returnAddressStackIndex = nameToStackItemIndex[STACK_RETURN_ADDRESS_IDENTIFIER];
    assert(stackItems[returnAddressStackIndex].positionOnStack >= 8);
    fakeClearStackFromTo(stackItems.size() - 1, returnAddressStackIndex);
    // might want to change this to an allocated register
    bytecode_t raReg = allocateRegister();
    if (raReg == returnRegisterIndex) {
        raReg = allocateRegister();
    }
    addBytes({{(bc)OpCode::POP_Q, raReg}});
    if (returnStatement.returnValue.getType() != ExpressionType::NONE) {
        const Token typeToken = getTypeFromTokenList(*returnValueExp.value.type);
        const uint32_t typeSize = getSizeOfType(typeToken);
        assert(typeSize <= SIZE_OF_REGISTER);
        // move value into returnRegisterIndex
        if (returnAddressStackIndex > 0) {
            fakeClearStackFromTo(returnAddressStackIndex - 1, -1);
        }
        if (returnValueExp.isPointerToValue) {
            returnValueExp = loadValueFromPointer(returnValueExp, returnRegisterIndex);
            assert(returnValueExp.getReg() == returnRegisterIndex);
        } else if (returnValueExp.isReg) {
            if (returnValueExp.getReg() != returnRegisterIndex) {
                addBytes({{(bc)OpCode::MOVE, returnRegisterIndex, returnValueExp.getReg()}});
            }
        } else {
            moveImmToReg(returnRegisterIndex, returnValueExp);
        }
    }
    else if (returnAddressStackIndex > 0) {
        fakeClearStackFromTo(returnAddressStackIndex - 1, -1);
    }
    addBytes({{(bc)OpCode::JUMP, raReg}});
    freeRegister(raReg);
    freeRegister(returnRegisterIndex);
    if (returnValueExp.isTemp) {
        freeRegister(returnValueExp.getReg());
    }
}


// ========================================
// STACK MANAGEMENT
// ========================================


void CodeGen::addSpaceToStack(uint32_t space) {
    efficientImmAddOrSub(stackPointerIndex, space, OpCode::SUB);
}

/**
 * Adds padding to the stack
 * \param padding how much padding to add
 * \param virtualOnly if padding should only be added to the virtual stack
*/
void CodeGen::addPaddingToStack(uint32_t padding, bool virtualOnly) {
    if (padding) {
        if (!virtualOnly) {
            addSpaceToStack(padding);
        }
        StackItem paddingItem = {
            .positionOnStack = getCurrStackPointerPosition() + padding,
            .type = StackItemType::PADDING
        };
        stackItems.emplace_back(paddingItem);
    }
}

/**
 * Adds padding + space room to the stack, and adds a stack item for the padding
 * \param padding how much padding to add
 * \param space how much space after the padding to add
 * \returns the offset for start of 'space'
*/
uint32_t CodeGen::addPaddingAndSpaceToStack(uint32_t padding, uint32_t space) {
    assert(space);
    addSpaceToStack(padding + space);
    const uint32_t currSP = getCurrStackPointerPosition();
    if (padding) {
        StackItem paddingItem = {
            .positionOnStack = currSP + padding,
            .type = StackItemType::PADDING
        };
        stackItems.emplace_back(paddingItem);
    }
    return currSP + padding + space;
}

/**
 * Adds a variable dec to the virtual stack. Also adds padding if necessary.
 * \param varDec the variable declaration to be added
 * \param alignTo optionally set the alignment. if left at the default of 0, will use the type's alignment
 * \param virtualOnly if set to true, only adds padding and var dec to virtual stack (does not update bytecode)
 *  otherwise will update the bytecode by padding, and padding only
 * \returns a pointer to the struct info of the type if the type is a struct, otherwise nullptr
*/
const StructInformation* CodeGen::addVarDecToStack(VariableDec& varDec, uint32_t alignTo, bool virtualOnly) {
    const Token typeToken = getTypeFromTokenList(varDec.type);
    uint32_t padding, size;
    const StructInformation* pStructInfo = nullptr;
    if (typeToken.getType() == TokenType::IDENTIFIER) {
        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
        pStructInfo = &structLookUp[genDec->structDec];
        if (!alignTo) {
            alignTo = pStructInfo->alignTo;
        }
        size = pStructInfo->size;
        padding = getPaddingNeeded(getCurrStackPointerPosition(), pStructInfo->size, alignTo);
    } else {
        size = getSizeOfBuiltinType(typeToken.getType());
        if (!alignTo) {
            alignTo = size;
        }
        else if (alignTo > size) {
            size = alignTo;
        }
        padding = getPaddingNeeded(getCurrStackPointerPosition(), size, alignTo);
    }
    addPaddingToStack(padding, virtualOnly);
    StackItem stackItem {
        .variable = {
            .varDec = varDec,
            .positionOnStack = getCurrStackPointerPosition() + size,
        },
        .type = StackItemType::VARIABLE,
    };
    nameToStackItemIndex[tk->extractToken(varDec.name)] = stackItems.size();
    stackItems.emplace_back(stackItem);
    return pStructInfo;
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

/**
 * 
*/
StackItem& CodeGen::addExpressionResToStack(ExpressionResult& expRes, StackItemType stackItemType, uint32_t alignTo) {
    const TokenList* type = expRes.value.type;
    assert(type);
    if (!expRes.isReg) {
        moveImmToReg(allocateRegister(), expRes);
    }
    const Token typeToken = getTypeFromTokenList(*type);
    const uint32_t spPosition = getCurrStackPointerPosition();
    if (typeToken.getType() != TokenType::IDENTIFIER) {
        const uint32_t size = getSizeOfBuiltinType(typeToken.getType());
        const uint32_t padding = getPaddingNeeded(spPosition, size, alignTo ? alignTo : size);
        if (padding) {
            addPaddingToStack(padding);
        }
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
        StackItem si = {
            .positionOnStack = getCurrStackPointerPosition() + size,
            .type = stackItemType
        };
        return stackItems.emplace_back(si);
    } else {
        const GeneralDec* genDec = lookUp[tk->extractToken(typeToken)];
        const StructInformation& structInfo = structLookUp[genDec->structDec];
        uint32_t size = structInfo.size;
        assert(size <= SIZE_OF_REGISTER);
        const uint32_t padding = getPaddingNeeded(spPosition, size, alignTo ? alignTo : structInfo.alignTo);
        if (!expRes.isPointerToValue) {
            addPaddingToStack(padding);
            addBytes({{(bc)getPushOpForSize(size), expRes.getReg()}});
        }
        else if (getLoadOpForSize(size) != OpCode::NOP) {
            addPaddingToStack(padding);
            addBytes({{(bc)getLoadOpForSize(size), miscRegisterIndex, expRes.getReg()}});
            addBytes({{(bc)getPushOpForSize(size), miscRegisterIndex}});
        }
        else {
            // size can be one of 1, 3, 5, 6, 7
            assert(size == 1 || size == 3 || size == 5 || size == 6 || size == 7);
            addPaddingAndSpaceToStack(padding, size);
            const bytecode_t reg = allocateRegister();
            addBytes({{(bc)OpCode::MOVE, reg, stackPointerIndex}});
            addBytes({{(bc)OpCode::ADD_I, reg, (bc)(size)}});
            if (size % 2 == 1) {
                addBytes({{(bc)OpCode::DEC, reg}});
                addBytes({{(bc)OpCode::LOAD_B, miscRegisterIndex, reg}});
                addBytes({{(bc)OpCode::PUSH_B, miscRegisterIndex}});
                --size;
            }
            assert(size % 2 == 0);
            if (size % 4 == 2) {
                addBytes({{(bc)OpCode::DEC, reg, (bc)OpCode::DEC, reg}});
                addBytes({{(bc)OpCode::LOAD_W, miscRegisterIndex, reg}});
                addBytes({{(bc)OpCode::PUSH_W, miscRegisterIndex}});
                size -= 2;
            }
            if (size == 4) {
                addBytes({{(bc)OpCode::SUB_I, reg, 4, 0}});
                addBytes({{(bc)OpCode::LOAD_D, miscRegisterIndex, reg}});
                addBytes({{(bc)OpCode::PUSH_D, miscRegisterIndex}});
                size -= 4;
            }
            assert(size == 0);
        }
        if (expRes.isTemp) {
            freeRegister(expRes.getReg());
        }
        StackItem si = {
            .positionOnStack = getCurrStackPointerPosition() + size,
            .type = stackItemType
        };
        return stackItems.emplace_back(si);
    }
}

/*
  * Memory layout information needs to be standardized per function,
  * but general memory layout for a function call:
  *    HIGH ADDRESS                                              LOW ADDRESS
  *   Argument 1 | Argument 2 | ... | Argument N | Return Address
  * 
  * - Callee unwinds and destructs all variables on return
*/
void CodeGen::addFunctionSignatureToVirtualStack(const FunctionDec& funcDec) {
    // add parameters
    if (funcDec.params.curr.type != StatementType::NONE) {
        const StatementList *parameterList = &funcDec.params;
        // need to align first one to 8 bytes if a return value address has not already been added
        addVarDecToStack(*parameterList->curr.varDec, 8, true);
        parameterList = parameterList->next;
        while (parameterList) {
            addVarDecToStack(*parameterList->curr.varDec, 0, true);
            parameterList = parameterList->next;
        }
    }

    assert(getSizeOfType(getTypeFromTokenList(funcDec.returnType)) <= SIZE_OF_REGISTER);
    const uint32_t sizeOfPointer = getSizeOfBuiltinType(TokenType::POINTER);
    // add return address
    const uint32_t padding = getPaddingNeeded(getCurrStackPointerPosition(), sizeOfPointer, sizeOfPointer);
    if (padding) {
        StackItem paddingItem {
            .positionOnStack = getCurrStackPointerPosition() + padding,
            .type = StackItemType::PADDING,
        };
        stackItems.emplace_back(paddingItem);   
    }
    StackItem returnAddress {
        .positionOnStack = getCurrStackPointerPosition() + sizeOfPointer,
        .type = StackItemType::RETURN_ADDRESS,
    };
    nameToStackItemIndex[STACK_RETURN_ADDRESS_IDENTIFIER] = stackItems.size();
    stackItems.emplace_back(returnAddress);
}

/**
 * Generates the byte code needed to clear the stack at index 'from' until 'to'.
 * A negative 'to' value results in the entire stack being cleared.
 * Does not remove any items from the virtual stack.
 * 
 * Have to be careful using this since we have to pass the indexes in.
 * For example, when we want to do a return statement, we clear the stack until the return address index
 * pop the return address off, clear the rest of the stack (function arguments) until the return value index, and then return.
 * 
 * \param from the index to start from
 * \param to the index to go to
*/
void CodeGen::fakeClearStackFromTo(const uint32_t from, const int32_t to) {
    // this for loop is going in the wrong order for destruction
    // must be reverse order of placed on stack, not same order
    // for  (uint32_t i = from; i > to; --i) {
    //   if (stackItems[i].type != StackItemType::VARIABLE) {
    //     continue;
    //   }
    //   // call destructor for each variable
    // }
    assert(from < stackItems.size());
    uint32_t stackUsage;
    if (to < 0) {
        stackUsage = getPositionOnStack(from);
    } else { // to is a positive value
        assert((uint32_t)to < stackItems.size() && from >= (uint32_t)to);
        stackUsage = getPositionOnStack(from) - getPositionOnStack(to);
    }
    efficientImmAddOrSub(stackPointerIndex, stackUsage, OpCode::ADD);
}

uint32_t CodeGen::getPositionOnStack(uint32_t stackItemIndex) {
    StackItem& stackItem = stackItems[stackItemIndex];
    switch (stackItem.type) {
        case StackItemType::NONE: { assert(false); break; }
        case StackItemType::MARKER: { assert(false); break; }
        case StackItemType::VARIABLE: { return stackItem.variable.positionOnStack; }
        case StackItemType::PADDING:
        case StackItemType::ARGUMENT:
        case StackItemType::RETURN_ADDRESS: 
        case StackItemType::RETURN_VALUE_SPACE_POINTER:
        case StackItemType::RETURNED_VALUE_SPACE: { return stackItem.positionOnStack; }
        case StackItemType::SAVED_TEMPORARY: { return stackItem.savedTempValue.positionOnStack; }
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

/**
 * Pops the item at the top of the stack off
*/
void CodeGen::popValue(const TokenList& type, const bytecode_t reg) {
    const uint32_t size = getSizeOfType(getTypeFromTokenList(type));
    const OpCode popOp = getPopOpForSize(size);
    if (popOp == OpCode::NOP) {
        // TODO:
    }
    addBytes({{(bc)popOp, reg}});
    stackItems.pop_back();
}

/**
 * Pops saved values back into the registers where they are expected to be.
 * Should always be called after an expression with isReturnedValue is acquired,
 * but the returned value should be popped first
*/
void CodeGen::postFunctionCall() {
    while (stackItems.back().type == StackItemType::SAVED_TEMPORARY) {
        SavedTempValue& savedTemp = stackItems.back().savedTempValue;
        popValue(savedTemp.type, savedTemp.reg);
    }
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
        paddingToAdd.value.set(val);
        ExpressionResult regExp;
        regExp.setReg(reg);
        regExp.value.type = &TokenListTypes::uint64Value;
        expressionResWithOp(op, imm, regExp, paddingToAdd);
        if (paddingToAdd.isTemp) {
            freeRegister(paddingToAdd.getReg());
        }
    }
}


OpCode getLoadOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::LOAD_B;
        case 2: return OpCode::LOAD_W;
        case 4: return OpCode::LOAD_D;
        case 8: return OpCode::LOAD_Q;
        default: return OpCode::NOP;
    }
}

OpCode getStoreOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::STORE_B;
        case 2: return OpCode::STORE_W;
        case 4: return OpCode::STORE_D;
        case 8: return OpCode::STORE_Q;
        default: return OpCode::NOP;
    }
}

OpCode getPushOpForSize(const uint32_t size) {
    switch(size) {
        case 1: return OpCode::PUSH_B;
        case 2: return OpCode::PUSH_W;
        case 4: return OpCode::PUSH_D;
        case 8: return OpCode::PUSH_Q;
        default: return OpCode::NOP;
    }
}

OpCode getPopOpForSize(const uint32_t size) {
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
            os << "Position: " << obj.positionOnStack;
            break;
        }
        case StackItemType::RETURN_VALUE_SPACE_POINTER: {
            os << "RETURN_VALUE_SPACE_POINTER\n";
            os << "Position: " << obj.positionOnStack;
            break;
        }
        case StackItemType::RETURNED_VALUE_SPACE: {
            os << "RETURNED_VALUE_SPACE\n";
            os << "Position: " << obj.positionOnStack;
            break;
        }
        case StackItemType::SAVED_TEMPORARY: {
            os << "SAVED_TEMPORARY\n";
            os << "Position: " << obj.savedTempValue.positionOnStack;
            break;
        }
        case StackItemType::ARGUMENT: {
            os << "ARGUMENT\n";
            os << "Position: " << obj.positionOnStack;
            break;
        }
        case StackItemType::PADDING: {
            os << "PADDING\n";
            os << "Position: " << obj.positionOnStack;
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

