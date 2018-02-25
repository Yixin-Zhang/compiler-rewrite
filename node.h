#ifndef NODE_H_
#define NODE_H_

#include <iostream>
#include <fstream>
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


enum OPS {
	// binops
	OP_TIMES,
	OP_DIV,
	OP_PLUS,
	OP_MINUS,
	OP_EQL,
	OP_LSS,
	OP_GTR,
	OP_AND,
	OP_OR,
	OP_ASSIGN,

	// uops
	OP_NOT
};

class Node {
public:
	virtual ~Node() {}
	virtual llvm::Value* codeGen(CodeGenContext& context) { return NULL; }
	virtual void yaml_output(ostream& os, int indent = 0) { return; }
};

class NExpression : public Node {
};

class NStatement : public Node {		
};

class NIdentifier : public NExpression {
public:
	std::string name;
	NIdentifier(const std::string& name) : name(name) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};


class NVariable : public NExpression {
public:
	NIdentifier& name;
	NVariable(NIdentifier& ident_name) : name(ident_name) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression* lhs;
	NExpression* rhs;
	NBinaryOperator(NExpression* lhs, int op, NExpression* rhs) :
		lhs(lhs), rhs(rhs), op(op) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NUnaryOperator : public NExpression {
public:
	int op;
	NExpression* rhs;
	NUnaryOperator(int op, NExpression* rhs) : rhs(rhs), op(op) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NInteger : public NExpression {
public:
	long long value;
	NInteger(const string& str) {
		value = stol(str);
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(const string& str) {
		value = stod(str);
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NFuncCall : public NExpression {
public:
	NIdentifier* funcname;
	ExpressionList* exps;
	NFuncCall(NIdentifier* funcname, ExpressionList* exps) :
		funcname(funcname), exps(exps) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock() {
		statements.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NReturnStatement : public NBlock {
public:
	NExpression* exp;
	NReturnStatement(NExpression* exp) : exp(exp) {}
	NReturnStatement() : exp(NULL) {}

	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NAssignStatement : public NBlock {
public:
	NVariableDeclaration* vdecl;
	NExpression* exp;
	NAssignStatement(NVariableDeclaration* vdecl, NExpression* exp) :
		vdecl(vdecl), exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NExpressionStatement : public NBlock {		
public:
	NExpression* exp;
	NExpressionStatement(NExpression* expression) : 
		exp(expression) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NWhileStatement : public NBlock {
public:
	NExpression* exp;
	NBlock* stmt;
	NWhileStatement(NExpression* exp, NBlock* stmt) :
		exp(exp), stmt(stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NIfStatement : public NBlock {
public:
	NExpression* exp;
	NBlock* stmt;
	NBlock* else_stmt;
	NIfStatement(NExpression* exp, NBlock* stmt, NBlock* else_stmt) :
		exp(exp), stmt(stmt), else_stmt(else_stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NPrintExpressionStatement : public NBlock {
public:
	NExpression* exp;
	NPrintExpressionStatement(NExpression* exp) : exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NPrintSlitStatement : public NBlock {
public:
	const string slit;
	NPrintSlitStatement(const string& str) : slit(str) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NVariableDeclaration : public NStatement {
public:
	const string& type;
	NVariable* var;
	NVariableDeclaration(const string& type, NVariable* var) :
		type(type), var(var) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NFunctionDeclaration : public NBlock {
public:
	const string& type;
	const NIdentifier* globid;
	VarDeclList* vdecls;
	NBlock* block;
	NFunctionDeclaration(const string& type, const NIdentifier* name, 
			VarDeclList* vdecls, NBlock* block) :
		type(type), globid(name), vdecls(vdecls), block(block) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
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
    virtual void yaml_output(ostream& os, int indent = 0);
};

class NExpressionList : public NExpression {
public:
	ExpressionList exps;
	NExpressionList() {
		exps.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NExternList : public NStatement {
public:
	ExternList externs;
	NExternList() {
		externs.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NFuncList : public NStatement {
public:
	FuncList funcs;
	NFuncList() {
		funcs.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};


class NProgram : public Node {
public:
	NExternList* externs;
	NFuncList* funcs;
	NProgram(NExternList* externs, NFuncList* funcs) :
		externs(externs), funcs(funcs) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);	
	virtual void yaml_output(ostream& os, int indent = 0);
};

#endif