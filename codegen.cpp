#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace llvm;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NProgram& root)
{
    std::cout << "Generating code...\n";
    
    /* Create the top level interpreter function to call as entry */
    vector<Type*> argTypes;
    FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
    mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
    BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction, 0);
    
    /* Push a new variable/block context */
    pushBlock(bblock);
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    ReturnInst::Create(MyContext, bblock);
    popBlock();
    
    /* Print the bytecode in a human-readable format
     to see if our program compiled properly
     */
    std::cout << "Code is generated.\n";
    // module->dump();
    
    legacy::PassManager pm;
    pm.add(createPrintModulePass(outs()));
    pm.run(*module);
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
    std::cout << "Running code...\n";
    ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
    ee->finalizeObject();
    vector<GenericValue> noargs;
    GenericValue v = ee->runFunction(mainFunction, noargs);
    std::cout << "Code was run.\n";
    return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type)
{
    if (type.name.compare("int") == 0) {
        return Type::getInt64Ty(MyContext);
    }
    else if (type.name.compare("double") == 0) {
        return Type::getDoubleTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

static Type *typeOf(const string& type)
{
    if (type.compare("int") == 0) {
        return Type::getInt64Ty(MyContext);
    }
    else if (type.compare("double") == 0) {
        return Type::getDoubleTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */
Value* NIdentifier::codeGen(CodeGenContext& context)
{
    std::cout << "Creating identifier reference: " << name << endl;
    if (context.locals().find(name) == context.locals().end()) {
        std::cerr << "undeclared variable " << name << endl;
        return NULL;
    }
    return new LoadInst(context.locals()[name], "", false, context.currentBlock());
}

Value* NVariable::codeGen(CodeGenContext& context)
{
    std::cout << "Creating variable: " << name.name << endl;
    return name.codeGen();
}

Value* NBinaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating binary operation " << op << endl;
    Instruction::BinaryOps instr;
    switch (op) {
        case OP_PLUS: instr = Instruction::FAdd; goto math;
        case OP_MINUS: instr = Instruction::FSub; goto math;
        case OP_TIMES: instr = Instruction::FMul; goto math;
        case OP_DIV: instr = Instruction::FDiv; goto math;
        case OP_EQL: instr = Instruction::FEq; goto math;
        case OP_LSS: instr = Instruction::FLt; goto math;
        case OP_GTR: instr = Instruction::FGt; goto math;
        case OP_AND: instr = Instruction::FAnd; goto math;
        case OP_OR: instr = Instruction::FOr; goto math;
        case OP_ASSIGN: return NAssignment_codeGen(context);
    }
    
    return NULL;
math:
    return BinaryOperator::Create(instr, lhs.codeGen(context),
                                  rhs.codeGen(context), "", context.currentBlock());
}

Value* NUnaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating unary operation " << op << endl;
    Instruction::BinaryOps instr;
    switch (op) {
        case OP_MINUS: instr = Instruction::FSub; goto math1;
        case OP_NOT: instr = Instruction::FMul; goto math2;
    }
    
    return NULL;
math1:
    string i = "0";
    auto a = new NInteger(i);
    return BinaryOperator::Create(instr, a.codeGen(context),
                                  rhs.codeGen(context), "", context.currentBlock());
math2:
    string j = "-1";
    auto b = new NInteger(j);
    return BinaryOperator::Create(instr, b.codeGen(context),
                                  rhs.codeGen(context), "", context.currentBlock());
}

Value* NInteger::codeGen(CodeGenContext& context) {
    std::cout << "Creating integer: " << value << endl;
    return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context) {
    std::cout << "Creating double: " << value << endl;
    return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value* NAssignment_codeGen(CodeGenContext& context) {
    std::cout << "Creating assignment for " << lhs.name.name << endl;
    if (context.locals().find(lhs.name.name) == context.locals().end()) {
        std::cerr << "undeclared variable " << lhs.name.name << endl;
        return NULL;
    }
    return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name.name], false, context.currentBlock());
}

Value* NReturnStatement::codeGen(CodeGenContext& context) {
    if (!exp) {
        std::cout << "Generating return code for no return value" << endl;
        context.setCurrentReturnValue(NULL);
        return NULL;
    }
    std::cout << "Generating return code for " << typeid(exp).name() << endl;
    Value *returnValue = exp.codeGen(context);
    context.setCurrentReturnValue(returnValue);
    return returnValue;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context) {
    if (!exp)
        return NULL;
    return exp.codeGen(context);
}

Value* NFuncExpression::codeGen(CodeGenContext& context) {
    Function *function = context.module->getFunction(funcname->name.c_str());
    if (function == NULL) {
        std::cerr << "no such function " << funcname->name << endl;
    }
    std::vector<Value*> args;
    ExpressionList::const_iterator it;
    for (it = (*exps).begin(); it != (*exps).end(); it++) {
        args.push_back((**it).codeGen(context));
    }
    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    std::cout << "Creating method call: " << funcname->name << endl;
    return call;
}

Value* NBlock::codeGen(CodeGenContext& context) {
    StatementList::const_iterator it;
    Value *last = NULL;
    for (it = statements.begin(); it != statements.end(); it++) {
        std::cout << "Generating code for " << typeid(**it).name() << endl;
        last = (**it).codeGen(context);
    }
    std::cout << "Creating block" << endl;
    return last;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context) {
    vector<Type*> argTypes;
    VarDeclList::const_iterator it;
    for (it = (*vdecls).begin(); it != (*vdecls).end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, globid->name.c_str(), context.module);
    BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);
    
    context.pushBlock(bblock);
    
    Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;
    
    for (it = (*vdecls).begin(); it != (*vdecls).end(); it++) {
        (**it).codeGen(context);
        
        argumentValue = &*argsValues++;
        argumentValue->setName((*it)->globid->name.c_str());
        StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->globid->name], false, bblock);
    }
    
    block->codeGen(context);
    ReturnInst::Create(MyContext, context.getCurrentReturnValue(), bblock);
    
    context.popBlock();
    std::cout << "Creating function: " << globid->name << endl;
    return function;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context) {
    vector<Type*> argTypes;
    TypeDeclList::const_iterator it;
    for (it = (*tdecls).begin(); it != (*tdecls).end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, globid->name.c_str(), context.module);
    return function;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NWhileStatement::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NIfStatement::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NPrintExpressionStatement::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NPrintSlitStatement::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NAssignStatement::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NExpressionList::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NExternList::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NFuncList::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NProgram::codeGen(CodeGenContext& context) {
    return NULL;
}
