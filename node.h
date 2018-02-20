#include <iostream>
#include <vector>
#include <llvm/IR/Value.h>

using namespace std;

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<NExpression*> ExpressionList;
typedef std::vector<NVariableDeclaration*> VariableList;

typedef std::vector<NExternDeclaration*> ExternList;
typedef std::vector<NFunctionDeclaration*> FuncList;
typedef std::vector<string> TypeDeclList;
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
	std::string name;
	NIdentifier(const std::string& name) : name(name) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBinaryOperator : public NExpression {
public:
	int op;
	NExpression& lhs;
	NExpression& rhs;
	NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
		lhs(lhs), rhs(rhs), op(op) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NUnaryOperator : public NExpression {
public:
	int op;
	NExpression& rhs;
	NUnaryOperator(int op, NExpression& rhs) : rhs(rhs), op(op) { }
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
class NFuncExpression : public NFuncExpression {
public:
	NIdentifier& id;
	NExpression& exp;
	NFuncExpression(NIdentifier& id, NExpression& exp) : id(id), exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NExpression {
public:
	NIdentifier& lhs;
	NExpression& rhs;
	NAssignment(NIdentifier& lhs, NExpression& rhs) : 
		lhs(lhs), rhs(rhs) { }
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBlock : public NExpression {
public:
	StatementList statements;
	NBlock() {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


class NReturnStatement : public NStatement {
public:
	NExpression& expression;
	NReturnStatement(NExpression& expression) : 
		expression(expression) {}
	NReturnStatement() {}

	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignStatement : public NStatement {
public:
	NVariableDeclaration& vdecl;
	NExpression& exp;
	NAssignment(NVariableDeclaration& vdecl, NExpression& exp) :
		vdecl(vdecl), exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExpressionStatement : public NStatement {
public:
	NExpression& exp;
	NExpressionStatement(NExpression& expression) : 
		exp(expression) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


class NWhileStatement : public NStatement {
public:
	NExpression& exp;
	NStatement& stmt;
	NWhileStatement(NExpression& exp, NStatement& stmt) :
		exp(exp), stmt(stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIfStatement : public NStatement {
public:
	NExpression& exp;
	NStatement& stmt;
	NStatement& else_stmt;
	NIfStatement(NExpression& exp, NStatement& stmt, NStatement& else_stmt) :
		exp(exp), stmt(stmt), else_stmt(else_stmt) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPrintExpressionStatement : public NStatement {
public:
	NExpression& exp;
	NPrintSlitStatement(NExpression& exp) : exp(exp) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NPrintSlitStatement : public NStatement {
public:
	const string slit;
	NPrintSlitStatement(const string& str) : slit(str) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariable : public NExpression {
public:
	const NIdentifier& name;
	NVariable(const NIdentifier& ident_name) : name(ident_name) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableDeclaration : public NStatement {
public:
	const string& type;
	const NVariable& var;
	NVariableDeclaration(const string& type, const NVariable& var) :
		type(type), var(var) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionDeclaration : public NStatement {
public:
	const string& type;
	const NIdentifier& name;
	VarDeclList& vdecls;
	NBlock& block;
	NFunctionDeclaration(const string& type, const NIdentifier& name, 
			const VarDeclList& vdecls, NBlock& block) :
		type(type), name(name), vdecls(vdecls), block(block) {}
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternDeclaration : public NStatement {
public:
    const string& type;
    const NIdentifier& globid;
    NTypeDeclList tdecls;
    NExternDeclaration(const string& type, const NIdentifier& id,
            const TypeDeclList& input_tdecls) :
        type(type), globid(id), tdecls(input_tdecls) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternList : public NStatement {
	ExternList externs;
	virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFuncList : public NStatement {
	FuncList funcs;
	virtual llvm::Value* codeGen(CodeGenContext& context);
};


class NProgram : public Node {
public:
	NExternList externs;
	NFuncList funcs;
	virtual llvm::Value* codeGen(CodeGenContext& context);	
};
