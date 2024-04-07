#include "compTime.hpp"
#include "checker/checker.hpp"
#include <cassert>


void LiteralValue::set(char c) {
    *(decltype(c) *)data = c;
    type = &TokenListTypes::charValue;
}
void LiteralValue::set(uint32_t val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::uint32Value;
}
void LiteralValue::set(uint64_t val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::uint64Value;
}
void LiteralValue::set(int32_t val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::int32Value;
}
void LiteralValue::set(int64_t val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::int64Value;
}
void LiteralValue::set(FILE *val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::fileValue;
}
void LiteralValue::set(double val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::doubleValue;
}
void LiteralValue::set(bool val) {
    *(decltype(val) *)data = val;
    type = &TokenListTypes::boolValue;
}

LiteralValue loadLiteralValue(Tokenizer& tk, const Expression &expression) {
    LiteralValue expRes;
    const Token token = expression.getToken();
    switch (token.getType()) {
        case TokenType::CHAR_LITERAL: {
            std::string charLiteral = tk.extractToken(token);
            // convert charLiteral to its numeric value and return it
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
        // case TokenType::STRING_LITERAL: {
        //     // could maybe do something here
        // }
        case TokenType::DECIMAL_NUMBER: {
            std::string decimalNumber = tk.extractToken(token);
            uint64_t num = std::stoull(decimalNumber);
            expRes.set(num);
            if (num <= INT32_MAX) {
                expRes.type = &TokenListTypes::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.type = &TokenListTypes::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.type = &TokenListTypes::int64Value;
            } // else already set to uint64_t
            return expRes;
        }
        case TokenType::BINARY_NUMBER: {
            assert(token.getLength() > 2);
            std::string binaryNumber = tk.extractToken(Token{token.getPosition() + 2, (uint16_t)(token.getLength() - 2), TokenType::BINARY_NUMBER});
            uint64_t num = std::stoull(binaryNumber, nullptr, 2);
            expRes.set(num);
            if (num <= INT32_MAX) {
                expRes.type = &TokenListTypes::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.type = &TokenListTypes::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.type = &TokenListTypes::int64Value;
            } // else already set to uint64_t
            return expRes;
        }
        case TokenType::FLOAT_NUMBER: {
            std::string binaryNumber = tk.extractToken(token);
            double num = std::stod(binaryNumber);
            expRes.set(num);
            return expRes;
        }
        case TokenType::HEX_NUMBER: { 
            assert(token.getLength() > 2);
            std::string hexNumber = tk.extractToken(Token{token.getPosition() + 2, (uint16_t)(token.getLength() - 2), TokenType::HEX_NUMBER});
            /*
            std::invalid_argument
            std::out_of_range
            */
            uint64_t num = std::stoull(hexNumber, nullptr, 16);
            expRes.set(num);
            if (num <= INT32_MAX) {
                expRes.type = &TokenListTypes::int32Value;
            } else if (num <= UINT32_MAX) {
                expRes.type = &TokenListTypes::uint32Value;
            } else if (num <= INT64_MAX) {
                expRes.type = &TokenListTypes::int64Value;
            } // else already set to uint64_t
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
        case TokenType::NULL_PTR: {
            expRes.type = &TokenListTypes::ptrValue;
            return expRes;
        }
        default: {
            assert(false);
            return expRes;
        }
    }
}

LiteralValue evaluateBinOpImmExpression(TokenType op, LiteralValue& left, LiteralValue& right) {
    assert(left.type && right.type);
    const TokenType leftSideType = left.type->exp.getToken().getType();
    const TokenType rightSideType = right.type->exp.getToken().getType();
    assert(isBuiltInType(leftSideType) && leftSideType != TokenType::VOID && leftSideType != TokenType::STRING_TYPE);
    assert(isBuiltInType(rightSideType) && rightSideType != TokenType::VOID && rightSideType != TokenType::STRING_TYPE);
    LiteralValue res;
    // assign to largest type, minimum of int32
    res.type = &TokenListTypes::int32Value;
    if (leftSideType > res.type->exp.getToken().getType()) {
        res.type = left.type;
    }
    if (rightSideType > res.type->exp.getToken().getType()) {
        res.type = right.type;
    }

    if (isUnsigned(leftSideType) || leftSideType == TokenType::BOOL) {
        // temporarily mark as uint64_t
        left.type = &TokenListTypes::uint64Value;
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
        right.type = &TokenListTypes::uint64Value;
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
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorEqual>(left, right, res);
            break;
        }
        case TokenType::NOT_EQUAL: {
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorNotEqual>(left, right, res);
            break;
        }
        case TokenType::LOGICAL_AND: {
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorLogicalAnd>(left, right, res);
            break;
        }
        case TokenType::LOGICAL_OR: {
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorLogicalOr>(left, right, res);
            break;
        }
        case TokenType::LESS_THAN: {
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorLess>(left, right, res);
            break;
        }
        case TokenType::LESS_THAN_EQUAL: {
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorLessEqual>(left, right, res);
            break;
        }
        case TokenType::GREATER_THAN: {
            res.type = &TokenListTypes::boolValue;
            doBinaryEvaluate<OperatorGreater>(left, right, res);
            break;
        }
        case TokenType::GREATER_THAN_EQUAL: {
            res.type = &TokenListTypes::boolValue;
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


LiteralValue evaluateUnaryOpImmExpression(TokenType op, LiteralValue& operand) {
    assert(operand.type);
    LiteralValue res;
    const TokenType operandType = operand.type->exp.getToken().getType();
    assert(isBuiltInType(operandType) && operandType != TokenType::VOID && operandType != TokenType::STRING_TYPE);
    
    // assign to largest type, minimum of int32
    res.type = &TokenListTypes::int32Value;
    if (operandType > res.type->exp.getToken().getType()) {
        res.type = operand.type;
    }
    if (isUnsigned(operandType) || operandType == TokenType::BOOL) {
        // temporarily mark as uint64_t
        operand.type = &TokenListTypes::uint64Value;
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
            res.type = &TokenListTypes::boolValue;
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
