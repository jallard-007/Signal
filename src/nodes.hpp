#pragma once

#include "tokenizer/tokenizer.hpp"
#include <vector>
#include <memory>

struct Unexpected {
  Token token;
  uint32_t line;
  uint32_t column;
  Unexpected() = delete;
  Unexpected(Token, uint32_t, uint32_t);
  bool operator==(const Unexpected&) const;
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  TOKEN,
};

struct Expected {
  uint32_t line;
  uint32_t column;
  TokenType tokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, uint32_t, uint32_t);
  Expected(ExpectedType, uint32_t, uint32_t, TokenType);
  bool operator==(const Expected&) const;
  std::string getErrorMessage(const std::string&);
};

struct Type {
  std::vector<Token> tokens;
  
  Type() = default;
  Type(Type&&) = default;
  bool operator==(const Type&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct VariableDec {
  Type type;
  Token name;
  VariableDec() = delete;
  explicit VariableDec(Token);
  VariableDec(VariableDec&&) = default;
  bool operator==(const VariableDec&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct Scope Scope;
typedef struct ForLoopHeader ForLoopHeader;
typedef struct ArrOrStructLiteral ArrOrStructLiteral;
typedef struct KeywordWithBody KeywordWithBody;

enum class StatementType: uint8_t {
  NONE,
  BAD,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  VARIABLE_DEC,
  FUNCTION_CALL,
  ARRAY_ACCESS,
  WRAPPED_VALUE,
  ARRAY_OR_STRUCT_LITERAL,
  FOR_LOOP_HEADER,
  SCOPE,
  KEYWORD,
  KEY_W_BODY,
};

struct Statement {
  union {
    UnOp *unOp;
    BinOp *binOp;
    VariableDec *varDec;
    FunctionCall *funcCall;
    ArrayAccess *arrAccess;
    Statement *wrapped;
    Scope *scope;
    ArrOrStructLiteral *arrOrStructLiteral;
    ForLoopHeader *list;
    KeywordWithBody *keywBody;
    Token var;
    TokenType key;
  };
  StatementType type = StatementType::NONE;

  Statement();
  explicit Statement(StatementType);
  Statement(const Statement&) = delete;
  Statement(Statement&&) noexcept ;
  explicit Statement(UnOp *);
  explicit Statement(BinOp *);
  explicit Statement(VariableDec *);
  explicit Statement(FunctionCall *);
  explicit Statement(ArrayAccess *);
  explicit Statement(Statement *);
  explicit Statement(Scope *);
  explicit Statement(ForLoopHeader *);
  explicit Statement(KeywordWithBody *);
  explicit Statement(ArrOrStructLiteral *);

  explicit Statement(Token);
  void operator=(Statement&&) noexcept;
  void operator=(const Statement&) = delete;
  bool operator==(const Statement&) const;
  explicit operator bool() const;

  ExpectedType addStatementToNode(Statement&&);
  Statement *getChild();
  ExpectedType isValid() const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

bool hasData(StatementType);

struct ArrOrStructLiteral {
  std::vector<Statement> list;
  ArrOrStructLiteral() = default;
  ArrOrStructLiteral(ArrOrStructLiteral&&) = default;
  bool operator==(const ArrOrStructLiteral&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct KeywordWithBody {
  Statement body;
  Statement header;
  TokenType keyword;
  KeywordWithBody() = delete;
  KeywordWithBody(TokenType);
  KeywordWithBody(KeywordWithBody&&);
  bool operator==(const KeywordWithBody&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct ForLoopHeader {
  std::vector<Statement> list;
  ForLoopHeader() = default;
  bool operator==(const ForLoopHeader&) const;
  ForLoopHeader(ForLoopHeader&&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Scope {
  std::vector<Statement> scopeStatements;
  Scope() = default;
  bool operator==(const Scope&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct ArrayAccess {
  Statement offset;
  Token array;

  ArrayAccess() = delete;
  explicit ArrayAccess(Token);
  bool operator==(const ArrayAccess&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct BinOp {
  Statement leftSide;
  Statement rightSide;
  TokenType op;
  explicit BinOp(TokenType);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&) noexcept;
  bool operator==(const BinOp&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct UnOp {
  Statement operand;
  TokenType op;
  explicit UnOp(TokenType);
  UnOp(const UnOp&) = delete;
  UnOp(UnOp&&) noexcept;
  bool operator==(const UnOp&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct FunctionCall {
  std::vector<Statement> args;
  Token name;
  FunctionCall() = delete;
  explicit FunctionCall(Token);
  bool operator==(const FunctionCall&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Enum {
  std::vector<Token> members;
  Token name;
  Enum();
  bool operator==(const Enum&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct FunctionDec {
  std::vector<Statement> params;
  Scope body;
  Type returnType;
  Token name;
  FunctionDec() = delete;
  explicit FunctionDec(Token);
  FunctionDec(FunctionDec&&);
  bool operator==(const FunctionDec&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

typedef struct Template Template;
typedef struct Struct Struct;

enum class DecType: uint8_t {
  NONE,
  FUNCTION,
  STATEMENT,
  TEMPLATE,
  STRUCT,
  ENUM
};

struct Declaration {
  union{
    FunctionDec *func;
    Statement *statement;
    Template *temp;
    Struct *struc;
    Enum *enm;
  };
  DecType decType;
  Declaration();
  Declaration(Declaration&&) noexcept;
  explicit Declaration(FunctionDec *);
  explicit Declaration(Statement *);
  explicit Declaration(Template *);
  explicit Declaration(Struct *);
  explicit Declaration(Enum *);
  bool operator==(const Declaration&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Struct {
  std::vector<Declaration> decs;
  Token name;
  Struct(Token);
  Struct(Struct&&) = default;
  bool operator==(const Struct&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Template {
  std::vector<Statement> templateIdentifiers;
  Declaration dec;
  Template() = default;
  Template(const Template&) = delete;
  Template(Template&&) = default;
  bool operator==(const Template&) const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Program {
  std::string name;
  std::vector<Declaration> decs;
  Program() = default;
  Program(Program&&) noexcept;
  bool operator==(const Program&) const;
  void prettyPrint(Tokenizer&, std::string&);
};
