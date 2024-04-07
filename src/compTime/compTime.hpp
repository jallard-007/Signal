#pragma once

#include <cstdint>
#include <cstdio>
#include "nodes.hpp"

#define SIZE_OF_REGISTER 8

struct LiteralValue {
    private:
    unsigned char data [SIZE_OF_REGISTER] {0};
    public:
    const TokenList *type {nullptr};
    inline const void *getData() const { return (void *)data; }

    template <class T> void set(T) = delete;
    void set(char);
    void set(uint32_t);
    void set(uint64_t);
    void set(int32_t);
    void set(int64_t);
    void set(FILE *);
    void set(double);
    void set(bool);

    template <class T>
    void setUntyped(T t) requires (std::integral<T> || std::floating_point<T>) {
        static_assert(sizeof(T)<=sizeof(data), "T does not fit in data");
        *(decltype(t) *)data = t;
    }
};





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
void doBinaryEvaluate(const LiteralValue& left, const LiteralValue& right, LiteralValue& res) {
    const TokenType leftSideType = left.type->exp.getToken().getType();
    const TokenType rightSideType = right.type->exp.getToken().getType();
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
void doBinaryIntegralEvaluate(const LiteralValue& left, const LiteralValue& right, LiteralValue& res) {
    const TokenType leftSideType = left.type->exp.getToken().getType();
    const TokenType rightSideType = right.type->exp.getToken().getType();
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
void doUnaryEvaluate(const LiteralValue& operand, LiteralValue& res) {
    TokenType operandType = operand.type->exp.getToken().getType();
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

LiteralValue loadLiteralValue(Tokenizer& tk, const Expression &expression);

LiteralValue evaluateBinOpImmExpression(TokenType op, LiteralValue& left, LiteralValue& right);

LiteralValue evaluateUnaryOpImmExpression(TokenType op, LiteralValue& operand);

