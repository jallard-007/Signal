#include <cassert>
#include "bytecodeDesign.hpp"

bool isReservedRegister(bytecode_t reg) {
    return (
        reg == instructionPointerIndex ||
        reg == stackPointerIndex ||
        reg == dataPointerIndex ||
        reg == returnRegisterIndex ||
        reg == miscRegisterIndex
    );
}

const bytecode_t *printInstruction(std::ostream& os, const bytecode_t *bytecode) {
    if (*bytecode > (bytecode_t)OpCode::LAST) {
        os << "Invalid OpCode [" << (uint32_t)*bytecode << "]";
        return nullptr;
    }
    const char *op_code_string = bytecode_t_to_op[*bytecode];
    os << op_code_string;
    switch ((OpCode)*bytecode) {
        // no args
        case OpCode::NOP: {
            return ++bytecode;
        }

        case OpCode::CALL_B: {
            const char *built_in_function = bytecode_t_to_builtin_function[*++bytecode];
            os << " " << built_in_function;
            return ++bytecode;
        }

        // one register arg
        case OpCode::EXIT:
        case OpCode::SET_FLAGS:
        case OpCode::GET_E:
        case OpCode::GET_NE:
        case OpCode::GET_G:
        case OpCode::GET_GE:
        case OpCode::GET_L:
        case OpCode::GET_LE:
        case OpCode::JUMP:
        case OpCode::JUMP_E:
        case OpCode::JUMP_NE:
        case OpCode::JUMP_G:
        case OpCode::JUMP_GE:
        case OpCode::JUMP_L:
        case OpCode::JUMP_LE:
        case OpCode::PUSH_B:
        case OpCode::PUSH_W:
        case OpCode::PUSH_D:
        case OpCode::PUSH_Q:
        case OpCode::POP_B:
        case OpCode::POP_W:
        case OpCode::POP_D:
        case OpCode::POP_Q:
        case OpCode::INC:
        case OpCode::DEC: {
            uint32_t arg1 = *++bytecode;
            os <<  " " << arg1;
            return ++bytecode;
        }

        // single 4 byte signed int arg
        case OpCode::CALL:
        case OpCode::R_JUMP:
        case OpCode::R_JUMP_E:
        case OpCode::R_JUMP_NE:
        case OpCode::R_JUMP_G:
        case OpCode::R_JUMP_GE:
        case OpCode::R_JUMP_L:
        case OpCode::R_JUMP_LE: {
            int32_t arg1 = *(int32_t *)++bytecode;
            os << " " << arg1;
            return bytecode + 4;
        }

        // single 1 byte signed int arg
        case OpCode::RS_JUMP:
        case OpCode::RS_JUMP_E:
        case OpCode::RS_JUMP_NE:
        case OpCode::RS_JUMP_G:
        case OpCode::RS_JUMP_GE:
        case OpCode::RS_JUMP_L:
        case OpCode::RS_JUMP_LE: {
            int32_t arg1 = (int8_t)*++bytecode;
            os << " " << arg1;
            return ++bytecode;
        }

        // two register arg
        case OpCode::CMP:
        case OpCode::LOAD_B:
        case OpCode::LOAD_W:
        case OpCode::LOAD_D:
        case OpCode::LOAD_Q:
        case OpCode::STORE_B:
        case OpCode::STORE_W:
        case OpCode::STORE_D:
        case OpCode::STORE_Q:
        case OpCode::MOVE:
        case OpCode::NOT:
        case OpCode::NEGATE:
        case OpCode::ADD:
        case OpCode::SUB:
        case OpCode::MUL:
        case OpCode::DIV:
        case OpCode::MOD:
        case OpCode::OR:
        case OpCode::AND:
        case OpCode::XOR:
        case OpCode::SHIFT_L:
        case OpCode::SHIFT_R:
        case OpCode::LOGICAL_OR:
        case OpCode::LOGICAL_AND:
        case OpCode::F_ADD:
        case OpCode::F_SUB:
        case OpCode::F_MUL:
        case OpCode::F_DIV: {
            uint32_t arg1 = *++bytecode;
            uint32_t arg2 = *++bytecode;
            os << " " << arg1 << " " << arg2;
            return ++bytecode;
        }

        // 1 register arg, followed by a 1 byte number
        case OpCode::MOVE_SI: {
            uint32_t arg1 = *++bytecode;
            int32_t arg2 = (int8_t)*++bytecode;
            os << " " << arg1 << " " << arg2;
            return ++bytecode;
        }

        // 1 register arg, followed by a 2 byte number
        case OpCode::ADD_I:
        case OpCode::SUB_I:
        case OpCode::MUL_I:
        case OpCode::DIV_I:
        case OpCode::MOD_I:
        case OpCode::OR_I:
        case OpCode::AND_I:
        case OpCode::XOR_I:
        case OpCode::SHIFT_L_I:
        case OpCode::SHIFT_R_I: {
            uint32_t arg1 = *++bytecode;
            int32_t arg2 = *(int16_t *)++bytecode;
            os << " " << arg1 << " " << arg2;
            return bytecode + 2;
        }

        // 1 register arg, followed by a 4 byte number
        case OpCode::MOVE_I: {
            uint32_t arg1 = *++bytecode;
            int32_t arg2 = *(int32_t *)++bytecode;
            os << " " << arg1 << " " << arg2;
            return bytecode + 4;
        }

        // 1 register arg, followed by a 8 byte number
        case OpCode::MOVE_LI: {
            uint32_t arg1 = *++bytecode;
            int64_t arg2 = *(int64_t *)++bytecode;
            os << " " << arg1 << " " << arg2;
            return bytecode + 8;
        }

        // 1 register arg, followed by a 8 byte float
        case OpCode::F_ADD_I:
        case OpCode::F_SUB_I:
        case OpCode::F_MUL_I:
        case OpCode::F_DIV_I: {
            uint32_t arg1 = *++bytecode;
            double arg2 = *(double *)++bytecode;
            os << " " << arg1 << " " << arg2;
            return bytecode + 8;
        }

        default: {
            os << "Unhandled OpCode " << (uint32_t)*++bytecode;
            assert(false);
            return nullptr;
        }
    }
}

std::ostream& operator<<(std::ostream& os, const std::vector<bytecode_t>& vec) {
    const bytecode_t* bytecode = vec.data();
    while (bytecode && bytecode < vec.data() + vec.size()) {
        bytecode = printInstruction(os, bytecode);
        os << '\n';
    }
    return os;
}

const char * bytecode_t_to_builtin_function [] = {
    "ALLOCATE",
    "REALLOCATE",
    "DEALLOCATE",
    "MEM_COPY",
    "MEM_MOVE",
    "MEM_COMPARE",
    "STR_LENGTH",
    "STR_COMPARE",
    "STR_N_COMPARE",
    "STR_COPY",
    "STR_N_COPY",
    "STR_CAT",
    "STR_N_CAT",
    "PRINT_STRING",
    "PRINT_CHAR",
    "PRINT_SIGNED",
    "PRINT_UNSIGNED",
    "PRINT_HEX",
    "FFLUSH",
    "OPEN",
    "CLOSE",
    "READ",
    "READ_LINE",
    "READ_CHAR",
    "WRITE",
    "SEEK",
};

const std::unordered_map<std::string, BuiltInFunction> builtin_function_to_bytecode_t = {
    {"ALLOCATE", BuiltInFunction::ALLOCATE},
    {"REALLOCATE", BuiltInFunction::REALLOCATE},
    {"DEALLOCATE", BuiltInFunction::DEALLOCATE},
    {"MEM_COPY", BuiltInFunction::MEM_COPY},
    {"MEM_MOVE", BuiltInFunction::MEM_MOVE},
    {"MEM_COMPARE", BuiltInFunction::MEM_COMPARE},
    {"STR_LENGTH", BuiltInFunction::STR_LENGTH},
    {"STR_COMPARE", BuiltInFunction::STR_COMPARE},
    {"STR_N_COMPARE", BuiltInFunction::STR_N_COMPARE},
    {"STR_COPY", BuiltInFunction::STR_COPY},
    {"STR_N_COPY", BuiltInFunction::STR_N_COPY},
    {"STR_CAT", BuiltInFunction::STR_CAT},
    {"STR_N_CAT", BuiltInFunction::STR_N_CAT},
    {"PRINT_STRING", BuiltInFunction::PRINT_STRING},
    {"PRINT_CHAR", BuiltInFunction::PRINT_CHAR},
    {"PRINT_SIGNED", BuiltInFunction::PRINT_SIGNED},
    {"PRINT_UNSIGNED", BuiltInFunction::PRINT_UNSIGNED},
    {"PRINT_HEX", BuiltInFunction::PRINT_HEX},
    {"FFLUSH", BuiltInFunction::FFLUSH},
    {"OPEN", BuiltInFunction::OPEN},
    {"CLOSE", BuiltInFunction::CLOSE},
    {"READ", BuiltInFunction::READ},
    {"READ_LINE", BuiltInFunction::READ_LINE},
    {"READ_CHAR", BuiltInFunction::READ_CHAR},
    {"WRITE", BuiltInFunction::WRITE},
    {"SEEK", BuiltInFunction::SEEK},
};

const char * bytecode_t_to_op [] = {
    "NOP",
    "EXIT",
    "CALL_B",
    "CALL",
    "CMP",
    "SET_FLAGS",
    "GET_E",
    "GET_NE",
    "GET_G",
    "GET_GE",
    "GET_L",
    "GET_LE",
    "LOAD_B",
    "LOAD_W",
    "LOAD_D",
    "LOAD_Q",
    "STORE_B",
    "STORE_W",
    "STORE_D",
    "STORE_Q",
    "JUMP",
    "JUMP_E",
    "JUMP_NE",
    "JUMP_G",
    "JUMP_GE",
    "JUMP_L",
    "JUMP_LE",
    "RS_JUMP",
    "RS_JUMP_E",
    "RS_JUMP_NE",
    "RS_JUMP_G",
    "RS_JUMP_GE",
    "RS_JUMP_L",
    "RS_JUMP_LE",
    "R_JUMP",
    "R_JUMP_E",
    "R_JUMP_NE",
    "R_JUMP_G",
    "R_JUMP_GE",
    "R_JUMP_L",
    "R_JUMP_LE",
    "MOVE",
    "MOVE_SI",
    "MOVE_I",
    "MOVE_LI",
    "PUSH_B",
    "PUSH_W",
    "PUSH_D",
    "PUSH_Q",
    "POP_B",
    "POP_W",
    "POP_D",
    "POP_Q",
    "INC",
    "DEC",
    "NOT",
    "NEGATE",
    "ADD",
    "ADD_I",
    "SUB",
    "SUB_I",
    "MUL",
    "MUL_I",
    "DIV",
    "DIV_I",
    "MOD",
    "MOD_I",
    "OR",
    "OR_I",
    "AND",
    "AND_I",
    "XOR",
    "XOR_I",
    "SHIFT_L",
    "SHIFT_L_I",
    "SHIFT_R",
    "SHIFT_R_I",
    "LOGICAL_OR",
    "LOGICAL_AND",
    "F_ADD",
    "F_ADD_I",
    "F_SUB",
    "F_SUB_I",
    "F_MUL",
    "F_MUL_I",
    "F_DIV",
    "F_DIV_I",
};
