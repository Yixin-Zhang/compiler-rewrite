#ifndef NODE_H_
#define NODE_H_

#include <iostream>
#include <fstream>
#include <vector>
#include "llvm/IR/Value.h"

using namespace std;
using namespace llvm;

class CodeGenContext;
class Node;
class NStatement;
class NExpression;
class NVariableDeclaration;
class NExternDeclaration;
class NFunctionDeclaration;
class NBlock;
class NExpressionList;

typedef std::vector<NStatement*> StatementList;
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
public:
    std::string exp_type;
    int mark;
    //for variable, mark = 1; for int, mark = 2; for float, mark = 3; for call, mark = 4; for unary operation, mark = 5; for binary operation, mark = 6.
 
    NExpression(string exp_type_name, int mark_number) {
        exp_type = exp_type_name;
        mark = mark_number;
    }
};

class NStatement : public Node {		
};

class NIdentifier : public Node {
public:
	std::string name;
    NIdentifier(const std::string& name_) { name = name_; }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};


class NVariable : public NExpression {
public:
	NIdentifier* name;
    NVariable(NIdentifier* ident_name, string exp_type_name) : NExpression(exp_type_name, 1) {
        name = ident_name;
        cout << "Creating a new variable: " << ident_name << ", type = " << exp_type << endl;
    }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression* lhs;
	NExpression* rhs;
	NBinaryOperator(NExpression* lhs_, int op_, NExpression* rhs_, string exp_type_name) : NExpression(exp_type_name, 6) {
        lhs = lhs_;
        rhs = rhs_;
        op = op_;
    }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NUnaryOperator : public NExpression {
public:
	int op;
	NExpression* rhs;
	NUnaryOperator(int op_, NExpression* rhs_, string exp_type_name) : NExpression(exp_type_name, 5) {
        rhs = rhs_;
        op = op_;
    }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NInteger : public NExpression {
public:
	long long value;
	NInteger(const string& str, string exp_type_name) : NExpression(exp_type_name, 2) {
		value = stol(str);
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NDouble : public NExpression {
public:
	double value;
	NDouble(const string& str, string exp_type_name) : NExpression(exp_type_name, 3) {
		value = stod(str);
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NFuncCall : public NExpression {
public:
	NIdentifier* funcname;
	NExpressionList* exps;
	NFuncCall(NIdentifier* func_name, NExpressionList* exps_, string exp_type_name) : NExpression(exp_type_name, 4)
    {   funcname = func_name;
        exps = exps_; }
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NBlock : public NStatement {
public:
	StatementList statements;
	NBlock() {
		statements.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NReturnStatement : public NStatement {
public:
	NExpression* exp;
	NReturnStatement(NExpression* exp) : exp(exp) {}
	NReturnStatement() : exp(NULL) {}

	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NAssignStatement : public NStatement {
public:
	NVariableDeclaration* vdecl;
	NExpression* exp;
	NAssignStatement(NVariableDeclaration* vdecl, NExpression* exp) :
		vdecl(vdecl), exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NExpressionStatement : public NStatement {
public:
	NExpression* exp;
	NExpressionStatement(NExpression* expression) : 
		exp(expression) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NWhileStatement : public NStatement {
public:
	NExpression* exp;
	NStatement* stmt;
	NWhileStatement(NExpression* exp, NStatement* stmt) :
		exp(exp), stmt(stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NIfStatement : public NStatement {
public:
	NExpression* exp;
	NStatement* stmt;
	NStatement* else_stmt;
	NIfStatement(NExpression* exp, NStatement* stmt, NStatement* else_stmt) :
		exp(exp), stmt(stmt), else_stmt(else_stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NPrintExpressionStatement : public NStatement {
public:
	NExpression* exp;
	NPrintExpressionStatement(NExpression* exp) : exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NPrintSlitStatement : public NStatement {
public:
	const string slit;
	NPrintSlitStatement(const string& str) : slit(str) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NVariableDeclaration : public Node {
public:
	const string& type;
	NVariable* var;
	NVariableDeclaration(const string& type, NVariable* var) :
		type(type), var(var) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NFunctionDeclaration : public Node{
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

class NExternDeclaration : public Node {
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

class NExpressionList : public Node {
public:
	ExpressionList exps;
	NExpressionList() {
		exps.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NExternList : public Node {
public:
	ExternList externs;
	NExternList() {
		externs.clear();
	}
	virtual llvm::Value* codeGen(CodeGenContext& context);
	virtual void yaml_output(ostream& os, int indent = 0);
};

class NFuncList : public Node {
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
