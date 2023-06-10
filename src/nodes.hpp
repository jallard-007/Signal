#pragma once

#include "token.hpp"
#include <vector>
#include <memory>

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  TOKEN,
};

struct Unexpected {
  Token token;
  uint32_t line;
  uint32_t column;
  Unexpected() = delete;
  Unexpected(Token, uint32_t, uint32_t);
};

struct Expected {
  uint32_t line;
  uint32_t column;
  TokenType tokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, uint32_t, uint32_t);
  Expected(ExpectedType, uint32_t, uint32_t, TokenType);
};

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
  SCOPE,
  KEYWORD,
  KEY_W_BODY,
  LIST
};

bool hasData(StatementType);

struct Type {
  std::vector<Token> tokens;
  
  Type() = default;
  Type(Type&&) = default;
  bool operator==(const Type&) const;
};

struct VariableDec {
  Type type;
  Token name;
  VariableDec() = delete;
  explicit VariableDec(Token);
  VariableDec(VariableDec&&) = default;
  bool operator==(const VariableDec&) const;
};

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct Scope Scope;
typedef struct List List;
typedef struct KeywordWithBody KeywordWithBody;

struct Statement {
  union {
    std::unique_ptr<UnOp> unOp;
    std::unique_ptr<BinOp> binOp;
    std::unique_ptr<VariableDec> varDec;
    std::unique_ptr<FunctionCall> funcCall;
    std::unique_ptr<ArrayAccess> arrAccess;
    Statement *wrapped;
    std::unique_ptr<Scope> scope;
    std::unique_ptr<List> list;
    std::unique_ptr<KeywordWithBody> keywBody;
    Token var;
    TokenType key;
  };
  StatementType type = StatementType::NONE;

  Statement();
  Statement(StatementType);
  Statement(const Statement&) = delete;
  Statement(Statement&&) noexcept ;
  explicit Statement(std::unique_ptr<UnOp>);
  explicit Statement(std::unique_ptr<BinOp>);
  explicit Statement(std::unique_ptr<VariableDec>);
  explicit Statement(std::unique_ptr<FunctionCall>);
  explicit Statement(std::unique_ptr<ArrayAccess>);
  explicit Statement(Statement *);
  explicit Statement(std::unique_ptr<Scope>);
  explicit Statement(std::unique_ptr<KeywordWithBody>);
  explicit Statement(std::unique_ptr<List>);

  explicit Statement(Token);
  ~Statement();
  void operator=(Statement&&) noexcept;
  void operator=(const Statement&) = delete;
  bool operator==(const Statement&) const;
  ExpectedType addStatementToNode(Statement&&);
  Statement **getChild();
  ExpectedType isValid() const;
};

struct List {
  std::vector<Statement> list;
  List() = default;
  List(List&&) = default;
};

struct KeywordWithBody {
  Statement *body;
  Statement *header;
  TokenType keyword;
  KeywordWithBody() = delete;
  KeywordWithBody(TokenType);
  KeywordWithBody(KeywordWithBody&&) = default;
};

struct Scope {
  std::vector<Statement> scopeStatements;
  Scope() = default;
  bool operator==(const Scope&) const;
};

struct ArrayAccess {
  Statement offset;
  Token array;

  ArrayAccess() = delete;
  explicit ArrayAccess(Token);
  bool operator==(const ArrayAccess&) const;
};

struct BinOp {
  Statement *leftSide;
  Statement *rightSide;
  TokenType op;
  explicit BinOp(TokenType);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&) noexcept;
  bool operator==(const BinOp&) const;
};

struct UnOp {
  Statement *operand;
  TokenType op;
  explicit UnOp(TokenType);
  UnOp(const UnOp&) = delete;
  UnOp(UnOp&&) noexcept;
  bool operator==(const UnOp&) const;
};

struct FunctionDec {
  std::vector<Statement> params;
  std::vector<Statement> bodyStatements;
  Type returnType;
  Token name;
  FunctionDec() = delete;
  explicit FunctionDec(Token);
  FunctionDec(FunctionDec&&);
};

struct FunctionCall {
  std::vector<Statement> args;
  Token name;
  FunctionCall() = delete;
  explicit FunctionCall(Token);
  bool operator==(const FunctionCall&) const;
};

enum class DecType: uint8_t {
  NONE,
  FUNCTION,
  STATEMENT,
  TEMPLATE,
  STRUCT,
  ENUM
};

struct Enum {
  std::vector<Token> members;
  Token name;
  Enum();
};

typedef struct Template Template;
typedef struct Struct Struct;

struct Declaration {
  union{
    std::unique_ptr<FunctionDec> func;
    Statement *statement;
    std::unique_ptr<Template> temp;
    std::unique_ptr<Struct> struc;
    std::unique_ptr<Enum> enm;
  };
  DecType decType;
  Declaration();
  Declaration(Declaration&&) noexcept;
  explicit Declaration(std::unique_ptr<FunctionDec>);
  explicit Declaration(Statement *);
  explicit Declaration(std::unique_ptr<Template>);
  explicit Declaration(std::unique_ptr<Struct>);
  explicit Declaration(std::unique_ptr<Enum>);
  ~Declaration();
};

struct Struct {
  std::vector<Declaration> decs;
  Token name;
  Struct(Token);
  Struct(Struct&&) = default;
};

struct Template {
  std::vector<Statement> templateIdentifiers;
  Declaration dec;
  Template() = default;
  Template(const Template&) = delete;
  Template(Template&&) = default;
};

struct Program {
  std::string name;
  std::vector<Declaration> decs;
  Program() = default;
  Program(Program&&) noexcept;
};
