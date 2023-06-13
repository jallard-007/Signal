#include "typeChecker.hpp"

TypeCheckerError::TypeCheckerError(TypeCheckerErrorType type): errorType{type} {}

TypeChecker::TypeChecker(Program& prog, Tokenizer& tk):
structsLookUp{}, lookUp{}, errors{}, program{prog}, tokenizer{tk} {}

void TypeChecker::scanTopLevel() {
  for (auto& dec : program.decs) {
    switch (dec.decType) {
      case DecType::FUNCTION: {
        Declaration* &decPtr = lookUp[tokenizer.extractToken(dec.func->name)];
        if (decPtr) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          decPtr = &dec;
        }
        break;
      }
      case DecType::VARIABLEDEC: {
        Declaration* &decPtr = lookUp[tokenizer.extractToken(dec.varDec->name)];
        if (decPtr) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          decPtr = &dec;
        }
        break;
      }
      case DecType::STRUCT: {
        const std::string structName = tokenizer.extractToken(dec.struc->name);
        Declaration* &decPtr = lookUp[structName];
        if (decPtr) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          decPtr = &dec;
          auto& structDecLookUp = structsLookUp[structName];
          for (auto& inner : dec.struc->decs) {
            if (inner.decType == DecType::VARIABLEDEC) {
              Declaration* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner.varDec->name)];
              if (innerStructDecPtr) {
                errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
              } else {
                innerStructDecPtr = &inner;
              }
            } else if (inner.decType == DecType::FUNCTION) {
              Declaration* &innerStructDecPtr = structDecLookUp[tokenizer.extractToken(inner.func->name)];
              if (innerStructDecPtr) {
                errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
              } else {
                innerStructDecPtr = &inner;
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
        Declaration** decPtrPtr = nullptr;
        if (dec.temp->dec.decType == DecType::STRUCT) {
          decPtrPtr = &lookUp[tokenizer.extractToken(dec.temp->dec.struc->name)];
        } else if (dec.temp->dec.decType == DecType::FUNCTION) {
          decPtrPtr = &lookUp[tokenizer.extractToken(dec.temp->dec.func->name)];
        } else {
          // not a valid template. parser catches this
          exit(1);
        }
        if (*decPtrPtr) {
          errors.emplace_back(TypeCheckerErrorType::NAME_ALREADY_IN_USE);
        } else {
          *decPtrPtr = &dec;
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


// Type *TypeChecker::unaryTypeOperandCheck(const TokenType operator_, const Token operand2) {
//   return nullptr;
// }
// Type *TypeChecker::binaryTypeOperandCheck(const Token operand1, const TokenType operator_, const Token operand2) {
//   return nullptr;
// }
