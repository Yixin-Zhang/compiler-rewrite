#include <iostream>
#include <vector>
#include "llvm/IR/Value.h"

using namespace std;
using namespace llvm;

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;
class NExternDeclaration;
class NFunctionDeclaration;
class NBlock;

typedef std::vector<NBlock*> StatementList;
typedef std::vector<NExpression*> ExpressionList;

typedef std::vector<NExternDeclaration*> ExternList;
typedef std::vector<NFunctionDeclaration*> FuncList;
typedef std::vector<string*> TypeDeclList;
typedef std::vector<NVariableDeclaration*> VarDeclList;
typedef std::vector<char> v;

class Node {
public:
	virtual ~Node() {}
	virtual llvm::Value* codeGen(CodeGenContext& context) { return NULL; }
};

class NExpression : public Node {
};

class NStatement : public Node {
};

class NIdentifier : public NExpression {
public:
	std::string name_;
	NIdentifier(const std::string& name) : name_(name) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


class NVariable : public NExpression {
public:
	const NIdentifier& name;
	NVariable(const NIdentifier& ident_name) : name(ident_name) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression* lhs;
	NExpression* rhs;
	NBinaryOperator(NExpression* lhs, int op, NExpression* rhs) :
		lhs(lhs), rhs(rhs), op(op) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NUnaryOperator : public NExpression {
public:
	int op;
	NExpression* rhs;
	NUnaryOperator(int op, NExpression* rhs) : rhs(rhs), op(op) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NInteger : public NExpression {
public:
	long long value;
	NInteger(const string& str) {
		value = stol(str);
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(const string& str) {
		value = stod(str);
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableExpression : public NExpression {
public:
	NVariable& var;
	NVariableExpression(NVariable& var) : var(var) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

// This should be a function expression
class NFuncExpression : public NExpression {
public:
	NIdentifier& id;
	NExpression* exp;
	NFuncExpression(NIdentifier& id, NExpression* exp) : id(id), exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NExpression {
public:
	NIdentifier& lhs;
	NExpression* rhs;
	NAssignment(NIdentifier& lhs, NExpression* rhs) : 
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock() {
		statements.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NReturnStatement : public NBlock {
public:
	NExpression* exp;
	NReturnStatement(NExpression* exp) : exp(exp) {}
	NReturnStatement() : exp(NULL) {}

	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignStatement : public NBlock {
public:
	NVariableDeclaration* vdecl;
	NExpression* exp;
	NAssignStatement(NVariableDeclaration* vdecl, NExpression* exp) :
		vdecl(vdecl), exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExpressionStatement : public NBlock {
public:
	NExpression* exp;
	NExpressionStatement(NExpression* expression) : 
		exp(expression) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


class NWhileStatement : public NBlock {
public:
	NExpression* exp;
	NBlock* stmt;
	NWhileStatement(NExpression* exp, NBlock* stmt) :
		exp(exp), stmt(stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIfStatement : public NBlock {
public:
	NExpression* exp;
	NBlock* stmt;
	NBlock* else_stmt;
	NIfStatement(NExpression* exp, NBlock* stmt, NBlock* else_stmt) :
		exp(exp), stmt(stmt), else_stmt(else_stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPrintExpressionStatement : public NBlock {
public:
	NExpression* exp;
	NPrintExpressionStatement(NExpression* exp) : exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPrintSlitStatement : public NBlock {
public:
	const string slit;
	NPrintSlitStatement(const string& str) : slit(str) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableDeclaration : public NStatement {
public:
	const string& type;
	const NVariable* var;
	NVariableDeclaration(const string& type, const NVariable* var) :
		type(type), var(var) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionDeclaration : public NBlock {
public:
	const string& type;
	const NIdentifier* name;
	VarDeclList* vdecls;
	NBlock* block;
	NFunctionDeclaration(const string& type, const NIdentifier* name, 
			VarDeclList* vdecls, NBlock* block) :
		type(type), name(name), vdecls(vdecls), block(block) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternDeclaration : public NStatement {
public:
    const string& type;
    const NIdentifier* globid;
    TypeDeclList* tdecls;
    NExternDeclaration(const string& type, const NIdentifier* id,
    	TypeDeclList* input_tdecls) :
        type(type), globid(id), tdecls(input_tdecls) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternList : public NStatement {
public:
	ExternList externs;
	NExternList() {
		externs.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFuncList : public NStatement {
public:
	FuncList funcs;
	NFuncList() {
		funcs.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


class NProgram : public Node {
public:
	NExternList* externs;
	NFuncList* funcs;
	NProgram(NExternList* externs, NFuncList* funcs) :
		externs(externs), funcs(funcs) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);	
};
