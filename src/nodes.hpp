#pragma once

#include "tokenizer/tokenizer.hpp"
#include <vector>
#include <memory>

struct Unexpected {
  Token token;
  Unexpected() = delete;
  Unexpected(Token);
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

enum class ExpectedType : uint8_t {
  NOTHING,
  EXPRESSION,
  TOKEN,
};

struct Expected {
  Token tokenWhereExpected;
  TokenType expectedTokenType;
  ExpectedType expectedType;
  Expected() = delete;
  Expected(ExpectedType, Token);
  Expected(ExpectedType, Token, TokenType);
  std::string getErrorMessage(Tokenizer&, const std::string&);
};

struct TokenList {
  Token curr;
  TokenList *next;
  TokenList();
  TokenList(Token);
  TokenList(TokenList&&) = default;
  ~TokenList() = default;
  bool operator==(const TokenList&) const;
};

struct Type {
  TokenList tokens;
  Type() = default;
  Type(Type&&) = default;
  void prettyPrint(Tokenizer&, std::string&);
};

typedef struct BinOp BinOp;
typedef struct UnOp UnOp;
typedef struct FunctionCall FunctionCall;
typedef struct ArrayAccess ArrayAccess;
typedef struct Scope Scope;
typedef struct ForLoopHeader ForLoopHeader;
typedef struct ArrOrStructLiteral ArrOrStructLiteral;
typedef struct KeywordWithBody KeywordWithBody;
typedef struct Declaration Declaration;
typedef struct VariableDec VariableDec;

enum class StatementType: uint8_t {
  NONE,
  SET,
  BINARY_OP,
  UNARY_OP,
  VALUE,
  FUNCTION_CALL,
  ARRAY_ACCESS,
  WRAPPED_VALUE,
  VARIABLE_DEC,
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
    Declaration *dec;
    FunctionCall *funcCall;
    ArrayAccess *arrAccess;
    Statement *wrapped;
    Scope *scope;
    ArrOrStructLiteral *arrOrStructLiteral;
    ForLoopHeader *list;
    KeywordWithBody *keyWBody;
    Token *var;
    TokenType key;
  };
  StatementType type = StatementType::NONE;

  Statement();
  explicit Statement(StatementType);
  Statement(const Statement&) = delete;
  Statement(Statement&&) noexcept ;
  explicit Statement(UnOp *);
  explicit Statement(BinOp *);
  explicit Statement(Declaration *);
  explicit Statement(FunctionCall *);
  explicit Statement(ArrayAccess *);
  explicit Statement(Statement *);
  explicit Statement(Scope *);
  explicit Statement(ForLoopHeader *);
  explicit Statement(KeywordWithBody *);
  explicit Statement(ArrOrStructLiteral *);
  explicit Statement(Token *);
  void operator=(Statement&&) noexcept;
  void operator=(const Statement&) = delete;
  explicit operator bool() const;

  ExpectedType addStatementToNode(Statement&&);
  Statement *getChild();
  ExpectedType isValid() const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct StatementList {
  Statement curr;
  StatementList *next;
  StatementList();
  StatementList(StatementList&&) = default;
  ~StatementList() = default;
  operator bool() const;
};

bool hasData(StatementType);

struct VariableDec {
  Type type;
  Token name;
  Statement *initialAssignment;
  VariableDec() = delete;
  explicit VariableDec(Token);
  VariableDec(VariableDec&&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct ArrOrStructLiteral {
  StatementList list;
  ArrOrStructLiteral() = default;
  ArrOrStructLiteral(ArrOrStructLiteral&&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct ForLoopHeader {
  StatementList list;
  ForLoopHeader() = default;
  ForLoopHeader(ForLoopHeader&&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Scope {
  StatementList scopeStatements;
  Scope() = default;
  operator bool() const;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct KeywordWithBody {
  Scope body;
  Statement header;
  Token keyword;
  bool isValid;
  KeywordWithBody() = delete;
  KeywordWithBody(Token);
  KeywordWithBody(KeywordWithBody&&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct ArrayAccess {
  Statement offset;
  Token array;

  ArrayAccess() = delete;
  explicit ArrayAccess(Token);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct BinOp {
  Statement leftSide;
  Statement rightSide;
  Token op;
  explicit BinOp(Token);
  BinOp(const BinOp&) = delete;
  BinOp(BinOp&&) noexcept;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct UnOp {
  Statement operand;
  Token op;
  explicit UnOp(Token);
  UnOp(const UnOp&) = delete;
  UnOp(UnOp&&) noexcept;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct FunctionCall {
  StatementList args;
  Token name;
  FunctionCall() = delete;
  explicit FunctionCall(Token);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Enum {
  // we can assign specific values to tokens, has to be statements >:(
  std::vector<Token> members;
  Token name;
  Enum();
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct FunctionDec {
  StatementList params;
  Scope body;
  Type returnType;
  Token name;
  FunctionDec() = delete;
  explicit FunctionDec(Token);
  FunctionDec(FunctionDec&&);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

typedef struct Template Template;
typedef struct Struct Struct;

enum class DecType: uint8_t {
  NONE,
  FUNCTION,
  VARIABLE_DEC,
  TEMPLATE,
  STRUCT,
  ENUM
};

struct Declaration {
  union{
    FunctionDec *func;
    VariableDec *varDec;
    Template *temp;
    Struct *struc;
    Enum *enm;
  };
  DecType decType;
  bool isValid;
  Declaration();
  Declaration(Declaration&&) noexcept;
  explicit Declaration(FunctionDec *);
  explicit Declaration(VariableDec *);
  explicit Declaration(Template *);
  explicit Declaration(Struct *);
  explicit Declaration(Enum *);
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Struct {
  std::vector<Declaration> decs;
  Token name;
  Struct(Token);
  Struct(Struct&&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Template {
  TokenList templateIdentifiers;
  Declaration dec;
  Template() = default;
  Template(const Template&) = delete;
  Template(Template&&) = default;
  void prettyPrint(Tokenizer&, std::string&, uint32_t);
};

struct Program {
  std::string name;
  std::vector<Declaration> decs;
  Program() = default;
  Program(Program&&) noexcept;
  void prettyPrint(Tokenizer&, std::string&);
};
