#include "codegen.h"

using namespace std;

llvm::Value* NIdentifier::codeGen(CodeGenContext& context) {
	return NULL;
}


llvm::Value* NVariable::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NBinaryOperator::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NUnaryOperator::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NInteger::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NDouble::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NFuncCall::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NBlock::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NReturnStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NAssignStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NExpressionStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NWhileStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NIfStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NPrintExpressionStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NPrintSlitStatement::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NVariableDeclaration::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NFunctionDeclaration::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NExternDeclaration::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NExpressionList::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NExternList::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NFuncList::codeGen(CodeGenContext& context) {
	return NULL;
}

llvm::Value* NProgram::codeGen(CodeGenContext& context) {
	return NULL;
}