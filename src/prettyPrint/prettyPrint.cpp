#include "nodes.hpp"
#include "parser/parser.hpp"

const uint8_t indentationSize = 4;

void TokenList::prettyPrint(Tokenizer& tk, std::string& str) const {
    std::vector<const TokenList *> r;
    for (const TokenList *iter = this; iter; iter = iter->next) {
        if (iter->token.getType() == TokenType::DEC_PTR) {
            break;
        }
        r.emplace_back(iter);
    }
    if (!r.empty()) {
        for (size_t i = r.size() - 1; i > 0 ; --i) {
            str += tk.extractToken(r[i]->token);
            str += " ";
        }
        str += tk.extractToken(r.front()->token);
    }
}

void VariableDec::prettyPrintDefinition(Tokenizer& tk, std::string& str) const {
    str += tk.extractToken(name) + ": ";
    type.prettyPrint(tk, str);
}

void VariableDec::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += tk.extractToken(name) + ": ";
    type.prettyPrint(tk, str);
    if (initialAssignment) {
        str += " = ";
        initialAssignment->prettyPrint(tk, str);
    }
}

void Statement::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    if (!expression) {
        return;
    }
    switch (type) {
        case StatementType::EXPRESSION:
            expression->prettyPrint(tk, str); break;
        case StatementType::CONTROL_FLOW:
            controlFlow->prettyPrint(tk, str, indentation); break;
        case StatementType::SCOPE:
            scope->prettyPrint(tk, str, indentation); break;
        case StatementType::VARIABLE_DEC:
            varDec->prettyPrint(tk, str); break;
        case StatementType::KEYWORD: str += typeToString.at(keyword.getType()); break;
        case StatementType::NONE: break;
        default: str += "{not yet implemented in pretty printer}"; break;
    }
}

void UnOp::prettyPrint(Tokenizer& tk, std::string& str) const {
    bool wrap = false;
    if (operand.getType() == ExpressionType::BINARY_OP || operand.getType() == ExpressionType::UNARY_OP) {
        if (operatorPrecedence[(uint8_t)operand.getBinOp()->op.getType()] < operatorPrecedence[(uint8_t)op.getType()]) {
            wrap = true;
        }
    }
    if (op.getType() == TokenType::DECREMENT_POSTFIX || op.getType() == TokenType::INCREMENT_POSTFIX) {
        if (wrap) {
            str += '(';
        }
        operand.prettyPrint(tk, str);
        if (wrap) {
            str += ')';
        }
        str += typeToString.at(op.getType());
    } else {
        str += typeToString.at(op.getType());
        if (wrap) {
            str += '(';
        }
        operand.prettyPrint(tk, str);
        if (wrap) {
            str += ')';
        }
    }
}

void BinOp::prettyPrint(Tokenizer& tk, std::string& str ) const {
    bool wrap = false;
    if (leftSide.getType() == ExpressionType::BINARY_OP || leftSide.getType() == ExpressionType::UNARY_OP) {
        if (operatorPrecedence[(uint8_t)leftSide.getBinOp()->op.getType()] < operatorPrecedence[(uint8_t)op.getType()]) {
            wrap = true;
            str += '(';
        }
    }
    leftSide.prettyPrint(tk, str);
    if (wrap) {
        str += ')';
    }
    wrap = false;
    str += typeToString.at(op.getType());
    if (rightSide.getType() == ExpressionType::BINARY_OP || rightSide.getType() == ExpressionType::UNARY_OP) {
        if (operatorPrecedence[(uint8_t)rightSide.getBinOp()->op.getType()] < operatorPrecedence[(uint8_t)op.getType()]) {
            wrap = true;
            str += '(';
        }
    }
    rightSide.prettyPrint(tk, str);
    if (wrap) {
        str += ')';
    }
}

void FunctionCall::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += tk.extractToken(name) + '(';
    if (args.curr.getType() != ExpressionType::NONE) {
        const ExpressionList * iter = &args;
        for (; iter->next; iter = iter->next) {
            iter->curr.prettyPrint(tk, str);
            str += ", ";
        }
        iter->curr.prettyPrint(tk, str);
    }
    str += ')';
}

void ArrayAccess::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += tk.extractToken(array.getToken()) + '[';
    offset.prettyPrint(tk, str);
    str += ']';
}

void Scope::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += "{\n";
    if (scopeStatements.curr.type != StatementType::NONE) {
        indentation += indentationSize;
        for (const StatementList *iter = &scopeStatements; iter; iter = iter->next) {
            if (iter->curr.type != StatementType::NONE) {
                str += std::string(indentation, ' ');
                iter->curr.prettyPrint(tk, str, indentation);
                if (iter->curr.type != StatementType::SCOPE && (iter->curr.type != StatementType::CONTROL_FLOW || iter->curr.controlFlow->type == ControlFlowStatementType::RETURN_STATEMENT)) {
                    str += ";\n";
                }
            }
        }
        indentation -= indentationSize;
    }
    str += std::string(indentation, ' ') + "}\n";
}

void FunctionDec::prettyPrintDefinition(Tokenizer& tk, std::string& str) const {
    str += typeToString.at(TokenType::FUNC);
    str += tk.extractToken(name) + '(';
    if (params.curr.type != StatementType::NONE) {
        const StatementList * iter = &params;
        for (; iter->next; iter = iter->next) {
            iter->curr.prettyPrint(tk, str, indentationSize);
            str += ", ";
        }
        iter->curr.prettyPrint(tk, str, indentationSize);
    }
    str += "): ";
    returnType.prettyPrint(tk, str);
}

void FunctionDec::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::FUNC);
    str += tk.extractToken(name) + '(';
    if (params.curr.type != StatementType::NONE) {
        indentation += indentationSize;
        const StatementList * iter = &params;
        for (; iter->next; iter = iter->next) {
            iter->curr.prettyPrint(tk, str, indentation);
            str += ", ";
        }
        iter->curr.prettyPrint(tk, str, indentation);
        indentation -= indentationSize;
    }
    str += "): ";
    returnType.prettyPrint(tk, str);
    str += ' ';
    body.prettyPrint(tk, str, indentation);
}

void EnumDec::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::ENUM) + tk.extractToken(name) + "{\n";
    indentation += indentationSize;
    for (const TokenList *iter = &members; iter; iter = iter->next) {
        str += std::string(indentation, ' ');
        str += tk.extractToken(iter->token);
        str += ",\n";
    }
    indentation -= indentationSize;
    str += std::string(indentation, ' ') + "}\n";
}

void GeneralDec::prettyPrintDefinition(std::vector<Tokenizer>& tks, std::string& str) const {
    if (type == GeneralDecType::NONE) {
        return;
    }
    Tokenizer& tk = tks[tokenizerIndex];
    switch (type) {
        case GeneralDecType::FUNCTION:
            funcDec->prettyPrintDefinition(tk, str); break;
        case GeneralDecType::VARIABLE:
            varDec->prettyPrintDefinition(tk, str); break;
        case GeneralDecType::TEMPLATE:
            tempDec->prettyPrintDefinition(tk, str); break;
        case GeneralDecType::STRUCT:
            structDec->prettyPrintDefinition(tk, str); break;
        case GeneralDecType::TEMPLATE_CREATE:
            tempCreate->prettyPrint(tk, str); break;
        case GeneralDecType::INCLUDE_DEC:
            includeDec->prettyPrint(tk, str); break;
        case GeneralDecType::BUILTIN_FUNCTION:
            builtinFunc->funcDec.prettyPrintDefinition(tk, str); break;
        
        default: break;
    }
}

void GeneralDec::prettyPrint(std::vector<Tokenizer>& tks, std::string& str) const {
    if (type == GeneralDecType::NONE) {
        return;
    }
    Tokenizer& tk = tks[tokenizerIndex];
    switch (type) {
        case GeneralDecType::FUNCTION:
            funcDec->prettyPrint(tk, str, 0); break;
        case GeneralDecType::VARIABLE:
            varDec->prettyPrint(tk, str); break;
        case GeneralDecType::TEMPLATE:
            tempDec->prettyPrint(tk, str, 0); break;
        case GeneralDecType::STRUCT:
            structDec->prettyPrint(tk, str, 0); break;
        case GeneralDecType::ENUM:
            enumDec->prettyPrint(tk, str, 0); break;
        default: break;
    }
}

void GeneralDecList::prettyPrint(std::vector<Tokenizer>& tk, std::string& str) const {
    const GeneralDecList*list = this;
    for (; list->next; list = list->next) {
        list->curr.prettyPrint(tk, str);
        str += '\n';
    }
    list->curr.prettyPrint(tk, str);
}

void StructDec::prettyPrintDefinition(Tokenizer& tk, std::string& str) const {
    str += typeToString.at(TokenType::STRUCT) + tk.extractToken(name);
}

void StructDec::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::STRUCT) + tk.extractToken(name) + " {\n";
    indentation += indentationSize;
    for (const StructDecList* list = &decs; list; list = list->next) {
        str += std::string(indentation, ' ');
        if (list->type == StructDecType::FUNC) {
            list->funcDec->prettyPrint(tk, str, indentation);
        } else if (list->type == StructDecType::VAR) {
            list->varDec->prettyPrint(tk, str);
            str += ";\n";
        } 
    }
    indentation -= indentationSize;
    str += std::string(indentation, ' ') + "}\n";
}

void TemplateDec::prettyPrintDefinition(Tokenizer& tk, std::string& str) const {
    str += typeToString.at(TokenType::TEMPLATE) + '[';
    if (templateTypes.token.getType() != TokenType::NONE) {
        const TokenList * iter = &templateTypes;
        for (; iter->next; iter = iter->next) {
            str += tk.extractToken(iter->token);
            str += ", ";
        }
        str += tk.extractToken(iter->token);
    }
    str += "] ";
    if (isStruct) {
        structDec.prettyPrintDefinition(tk, str);
    } else {
        funcDec.prettyPrintDefinition(tk, str);
    }
}

void TemplateDec::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::TEMPLATE) + '[';
    if (templateTypes.token.getType() != TokenType::NONE) {
        const TokenList * iter = &templateTypes;
        for (; iter->next; iter = iter->next) {
            str += tk.extractToken(iter->token);
            str += ", ";
        }
        str += tk.extractToken(iter->token);
    }
    str += "] ";
    if (isStruct) {
        structDec.prettyPrint(tk, str, indentation);
    } else {
        funcDec.prettyPrint(tk, str, indentation);
    }
}

void Program::prettyPrint(std::vector<Tokenizer>& tk, std::string& str) const {
    decs.prettyPrint(tk, str);
}

void Expression::prettyPrint(Tokenizer& tk, std::string& str) const {
    if (getType() == ExpressionType::NONE) {
        return;
    }
    switch (getType()) {
        case ExpressionType::ARRAY_ACCESS: getArrayAccess()->prettyPrint(tk ,str); break;
        case ExpressionType::BINARY_OP: getBinOp()->prettyPrint(tk, str); break;
        case ExpressionType::FUNCTION_CALL: getFunctionCall()->prettyPrint(tk, str); break;
        case ExpressionType::UNARY_OP: getUnOp()->prettyPrint(tk, str); break;
        case ExpressionType::VALUE: str += tk.extractToken(getToken()); break;
        case ExpressionType::CONTAINER_LITERAL: getContainerLiteral()->prettyPrint(tk, str); break;
        case ExpressionType::NONE: break;
        default: str += "{not yet implemented in pretty printer}"; break;
    }
}

void ContainerLiteral::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += '[';
   const ExpressionList*list = &values;
    for (; list->next; list = list->next) {
        list->curr.prettyPrint(tk ,str);
        str += ", ";
    }
    list->curr.prettyPrint(tk, str);
    str += ']';
}

void ControlFlowStatement::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    if (type == ControlFlowStatementType::NONE) {
        return;
    }
    switch (type) {
        case ControlFlowStatementType::CONDITIONAL_STATEMENT: conditional->prettyPrint(tk, str, indentation); break;
        case ControlFlowStatementType::FOR_LOOP: forLoop->prettyPrint(tk, str, indentation); break;
        case ControlFlowStatementType::RETURN_STATEMENT: returnStatement->prettyPrint(tk, str); break;
        case ControlFlowStatementType::SWITCH_STATEMENT: switchStatement->prettyPrint(tk, str, indentation); break;
        case ControlFlowStatementType::WHILE_LOOP: whileLoop->prettyPrint(tk, str, indentation); break;
        case ControlFlowStatementType::NONE: break;
        default: str += "{not yet implemented in pretty printer}"; break;
    }
}

void ForLoop::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::FOR) + '(';
    initialize.prettyPrint(tk, str, indentation);
    if (loop.condition.getType() == ExpressionType::NONE) {
        str += ';';
    } else {
        str += "; ";
    }
    loop.condition.prettyPrint(tk, str);
    if (iteration.getType() == ExpressionType::NONE) {
        str += ';';
    } else {
        str += "; ";
    }
    iteration.prettyPrint(tk, str);
    str += ") ";
    loop.body.prettyPrint(tk, str, indentation);
}

void ReturnStatement::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += typeToString.at(TokenType::RETURN);
    if (returnValue.getType() != ExpressionType::NONE) {
        str += ' ';
        returnValue.prettyPrint(tk, str);
    }
}

void SwitchStatement::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::SWITCH);
    switched.prettyPrint(tk, str);
    str += ' ';
    body.prettyPrint(tk, str, indentation);
}

void SwitchScopeStatementList::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += "{\n";
    indentation += indentationSize;
    for (const SwitchScopeStatementList *list = this; list; list = list->next) {
        str += std::string(indentation, ' ');
        if (list->caseExpression) {
            str += typeToString.at(TokenType::CASE);
            list->caseExpression->prettyPrint(tk, str);
        } else {
            str += typeToString.at(TokenType::DEFAULT);
        }
        if (list->caseBody) {
            str += ' ';
            list->caseBody->prettyPrint(tk, str, indentation);
        } else {
            str += '\n';
        }
    }
    indentation -= indentationSize;
    str += std::string(indentation, ' ') + "}\n";
}

void WhileLoop::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::WHILE);
    loop.prettyPrint(tk ,str, indentation);
}

void BranchStatement::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    condition.prettyPrint(tk, str);
    str += ' ';
    body.prettyPrint(tk, str, indentation);
}

void ConditionalStatement::prettyPrint(Tokenizer& tk, std::string& str, uint32_t indentation) const {
    str += typeToString.at(TokenType::IF);
    ifStatement.prettyPrint(tk, str, indentation);
    for (ElifStatementList*list = elifStatement; list; list = list->next) {
        str += std::string(indentation, ' ') + typeToString.at(TokenType::ELIF);
        list->elif.prettyPrint(tk, str, indentation);
    }
    if (elseStatement) {
        str += std::string(indentation, ' ') + typeToString.at(TokenType::ELSE);
        elseStatement->prettyPrint(tk, str, indentation);
    }
}

void IncludeDec::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += typeToString.at(TokenType::INCLUDE) + ' ' + tk.extractToken(file);
}

void TemplateCreation::prettyPrint(Tokenizer& tk, std::string& str) const {
    str += typeToString.at(TokenType::CREATE) + " [";
    if (templateTypes.token.getType() != TokenType::NONE) {
        str += tk.extractToken(templateTypes.token);
        TokenList * list = templateTypes.next;
        while (list) {
            str += ", ";
            str += tk.extractToken(list->token);
            list = list->next;
        }
    }
    str += "] " + typeToString.at(TokenType::AS) + tk.extractToken(templateName) + ";\n";
}
