#include "typeChecker.hpp"

TypeCheckerError::TypeCheckerError(TypeCheckerErrorType type): errorType{type} {}

TypeChecker::TypeChecker(Program& prog, Tokenizer& tk):
structsLookUp{}, lookUp{}, errors{}, program{prog}, tokenizer{tk} {}

void TypeChecker::scanTopLevel() {
  for (auto& dec : program.decs) {
    switch (dec.decType) {
      case DecType::FUNCTION: {
        Declaration* &f = lookUp[tokenizer.extractToken(dec.func->name)];
        if (f) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          f = &dec;
        }
        break;
      }
      case DecType::VARIABLEDEC: {
        Declaration* &v = lookUp[tokenizer.extractToken(dec.varDec->name)];
        if (v) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          v = &dec;
        }
        break;
      }
      case DecType::STRUCT: {
        const std::string structName = tokenizer.extractToken(dec.struc->name);
        Declaration* &jj = lookUp[structName];
        if (jj) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          jj = &dec;
          auto& s = structsLookUp[structName];
          for (auto& inner : dec.struc->decs) {
            if (inner.decType == DecType::VARIABLEDEC) {
              Declaration* &v = s[tokenizer.extractToken(inner.varDec->name)];
              if (v) {
                errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
              } else {
                v = &inner;
              }
            } else if (inner.decType == DecType::FUNCTION) {
              Declaration* &f = s[tokenizer.extractToken(inner.func->name)];
              if (f) {
                errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
              } else {
                f = &inner;
              }
            } else {
              // only functions and variables at top level of struct. parser catches this
              exit (1);
            }
          }
        }

        break;
      }
      case DecType::TEMPLATE: {
        Declaration** f = nullptr;
        if (dec.temp->dec.decType == DecType::STRUCT) {
          f = &lookUp[tokenizer.extractToken(dec.temp->dec.struc->name)];
        } else if (dec.temp->dec.decType == DecType::FUNCTION) {
          f = &lookUp[tokenizer.extractToken(dec.temp->dec.func->name)];
        } else {
          // not a valid template. parser catches this
          exit(1);
        }
        if (*f) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          *f = &dec;
        }
      }
      case DecType::ENUM: {
        break;
      }
      default: {
        break;
      }
    }
  }
}

// void TypeChecker::validateProgram() {
//   for (auto& dec : program.decs) {
//     switch (dec.decType) {
//       case DecType::FUNCTION: {
//         FunctionDec* f = functionLookUp[tokenizer.extractToken(dec.func->name)];
//         if (!f) {
//           errors.emplace_back(TypeCheckerErrorType::NO_MATCH);
//         } else {
//           f = dec.func;
//         }
//         break;
//       }
//       case DecType::VARIABLEDEC: {
//         Statement *currStatement = dec.statement;
//         if (currStatement->type == StatementType::BINARY_OP && currStatement->binOp->op == TokenType::ASSIGNMENT) {
//           currStatement = &currStatement->binOp->leftSide;
//         }
//         if (currStatement->type == StatementType::VARIABLE_DEC) {
//           VariableDec* v = varLookUp[tokenizer.extractToken(currStatement->varDec->name)];
//           if (!v) {
//             errors.emplace_back(TypeCheckerErrorType::NO_MATCH);
//           } else {
//             v = currStatement->varDec;
//           }
//         } else {
//           // invalid top level statement. only declarations are allowed
//           errors.emplace_back(TypeCheckerErrorType::DEFAULT);
//         }
//         break;
//       }
//       case DecType::STRUCT: {
//         const std::string structName = tokenizer.extractToken(dec.struc->name);
//         structLookUp[structName] = dec.struc;
//         for (auto& inner : dec.struc->decs) {
//           if (inner.decType == DecType::VARIABLEDEC) {
//             Statement *currStatement = inner.statement;
//             if (currStatement->type == StatementType::BINARY_OP && currStatement->binOp->op == TokenType::ASSIGNMENT) {
//               currStatement = &currStatement->binOp->leftSide;
//             }
//             if (currStatement->type == StatementType::VARIABLE_DEC) {
//               VariableDec* v = varLookUp[structName + '+' + tokenizer.extractToken(currStatement->varDec->name)];
//               if (!v) {
//                 errors.emplace_back(TypeCheckerErrorType::NO_MATCH);
//               } else {
//                 v = currStatement->varDec;
//               }
//             } else {
//               // invalid top level statement. only declarations are allowed
//             }
//           } else if (inner.decType == DecType::FUNCTION) {
//             FunctionDec* f = functionLookUp[structName + '+' + tokenizer.extractToken(inner.func->name)];
//             if (!f) {
//               errors.emplace_back(TypeCheckerErrorType::NO_MATCH);
//             } else {
//               f = inner.func;
//             }
//           } else {
//             // only functions and variables at top level of struct. parser catches this
//             exit (1);
//           }
//         }
//
//         break;
//       }
//       case DecType::TEMPLATE: {
//         Template* f = nullptr;
//         if (dec.temp->dec.decType == DecType::STRUCT) {
//           f = templateLookUp[tokenizer.extractToken(dec.temp->dec.struc->name)];
//         } else if (dec.temp->dec.decType == DecType::FUNCTION) {
//           f = templateLookUp[tokenizer.extractToken(dec.temp->dec.func->name)];
//         } else {
//           // not a valid template. parser catches this
//           exit(1);
//         }
//         if (!f) {
//           errors.emplace_back(TypeCheckerErrorType::NO_MATCH);
//         } else {
//           f = dec.temp;
//         }
//       }
//       case DecType::ENUM: {
//         break;
//       }
//       default: {
//         break;
//       }
//     }
//   }
// }

// Type *TypeChecker::unaryTypeOperandCheck(const TokenType operator_, const Token operand2) {
//   return nullptr;
// }
// Type *TypeChecker::binaryTypeOperandCheck(const Token operand1, const TokenType operator_, const Token operand2) {
//   return nullptr;
// }
