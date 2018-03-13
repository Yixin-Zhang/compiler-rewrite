#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#include "corefn.h"

using namespace llvm;

//extern Function* printfFn;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NProgram& root) {
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

    if (getOpt()) {
        std::cout << "Running optimization...\n";
        // legacy::PassManager pm;
        // pm.add(createPrintModulePass(outs()));
        // pm.run(*module);
        llvm::legacy::PassManager *pm = new llvm::legacy::PassManager();
        int optLevel = 3;
        int sizeLevel = 0;
        PassManagerBuilder builder;
        builder.OptLevel = optLevel;
        builder.SizeLevel = sizeLevel;
        builder.Inliner = createFunctionInliningPass(optLevel, sizeLevel, true);
        builder.DisableUnitAtATime = false;
        builder.DisableUnrollLoops = false;
        builder.LoopVectorize = true;
        builder.SLPVectorize = true;
        builder.populateModulePassManager(*pm);
        pm->run(*module);
    }
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
static Type *typeOf(const NIdentifier& type) {
    if (type.name.compare("int") == 0) {
        return Type::getInt32Ty(MyContext);
    }
    else if (type.name.compare("float") == 0) {
        return Type::getFloatTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

static Type *typeOf(const string& type) {
    if (type.compare("int") == 0) {
        return Type::getInt32Ty(MyContext);
    }
    else if (type.compare("float") == 0) {
        return Type::getFloatTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */
Value* NIdentifier::codeGen(CodeGenContext& context) {
    std::cout << "Creating identifier reference: " << name << endl;
    if (context.locals().find(name) == context.locals().end()) {
        std::cerr << "undeclared variable " << name << endl;
        return NULL;
    }
    return new LoadInst(context.locals()[name], "", false, context.currentBlock());
}

Value* NVariable::codeGen(CodeGenContext& context) {
    std::cout << "Creating variable: " << name->name << endl;
    return name->codeGen(context);
}

Value* NAssignment_codeGen(CodeGenContext& context, NExpression *hs, NExpression *rhs) {
    std::cout << "Creating assignment... " << endl;
    NVariable *lhs = (NVariable *)hs;
    if (context.locals().find(lhs->name->name) == context.locals().end()) {
        std::cerr << "undeclared variable " << lhs->name->name << endl;
        return NULL;
    }
    Value *assign_gen = rhs->codeGen(context);
    Value *assign_rhs;
    if (hs->exp_type.find("float") != string::npos) {
        if (rhs->exp_type.find("float") != string::npos)
            assign_rhs = assign_gen;
        else assign_rhs = new SIToFPInst(assign_gen, Type::getFloatTy(context.module->getContext()), "", context.currentBlock());
    } else {
        if (rhs->exp_type.find("int") != string::npos)
            assign_rhs = assign_gen;
        else assign_rhs = new FPToSIInst(assign_gen, Type::getInt32Ty(context.module->getContext()), "", context.currentBlock());
    }
    return new StoreInst(assign_rhs, context.locals()[lhs->name->name], false, context.currentBlock());
}

Value* NBinaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating binary operation " << op << endl;
    Instruction::BinaryOps instr;
    Instruction::OtherOps other_instr;
    CmpInst::Predicate pre;
    switch (op) {
        case OP_AND: instr = Instruction::And; return BinaryOperator::Create(instr, lhs->codeGen(context), rhs->codeGen(context), "", context.currentBlock());
        case OP_OR: instr = Instruction::Or; return BinaryOperator::Create(instr, lhs->codeGen(context), rhs->codeGen(context), "", context.currentBlock());
        case OP_ASSIGN: return NAssignment_codeGen(context, lhs, rhs);
    }
    
    Value *llhs, *rrhs;
    Value *clhs = lhs->codeGen(context);
    Value *crhs = rhs->codeGen(context);
    if (exp_type.find("float") != string::npos) {
        if (lhs->exp_type.find("float") != string::npos) {
            llhs = clhs;
        } else {
            llhs = new SIToFPInst(clhs, Type::getFloatTy(context.module->getContext()), "", context.currentBlock());
        }
        if (rhs->exp_type.find("float") != string::npos) {
            rrhs = crhs;
        } else {
            rrhs = new SIToFPInst(crhs, Type::getFloatTy(context.module->getContext()), "", context.currentBlock());
        }
    } else if (exp_type.find("int") != string::npos) {
        if (lhs->exp_type.find("int") != string::npos) {
            llhs = clhs;
        } else {
            llhs = new FPToSIInst(clhs, Type::getInt32Ty(context.module->getContext()), "", context.currentBlock());
        }
        if (rhs->exp_type.find("int") != string::npos) {
            rrhs = crhs;
        } else {
            rrhs = new FPToSIInst(crhs, Type::getInt32Ty(context.module->getContext()), "", context.currentBlock());
        }
    }
    
    if (exp_type == "float") {
        switch (op) {
            case OP_PLUS: instr = Instruction::FAdd; goto fmath;
            case OP_MINUS: instr = Instruction::FSub; goto fmath;
            case OP_TIMES: instr = Instruction::FMul; goto fmath;
            case OP_DIV: instr = Instruction::FDiv; goto fmath;
            case OP_EQL: other_instr = Instruction::FCmp; pre = CmpInst::FCMP_OEQ; goto fcmp;
            case OP_LSS: other_instr = Instruction::FCmp; pre = CmpInst::FCMP_OLT; goto fcmp;
            case OP_GTR: other_instr = Instruction::FCmp; pre = CmpInst::FCMP_OGT; goto fcmp;
        }
    } else if (exp_type == "sfloat") {
        switch (op) {
            case OP_PLUS: instr = Instruction::FAdd; goto math;
            case OP_MINUS: instr = Instruction::FSub; goto math;
            case OP_TIMES: instr = Instruction::FMul; goto math;
            case OP_DIV: instr = Instruction::FDiv; goto math;
            case OP_EQL: other_instr = Instruction::FCmp; pre = CmpInst::FCMP_OEQ; goto cmp;
            case OP_LSS: other_instr = Instruction::FCmp; pre = CmpInst::FCMP_OLT; goto cmp;
            case OP_GTR: other_instr = Instruction::FCmp; pre = CmpInst::FCMP_OGT; goto cmp;
        }
    } else if (exp_type == "int") {
        switch (op) {
            case OP_PLUS: instr = Instruction::Add; goto math;
            case OP_MINUS: instr = Instruction::Sub; goto math;
            case OP_TIMES: instr = Instruction::Mul; goto math;
            case OP_DIV: instr = Instruction::SDiv; goto math;
            case OP_EQL: other_instr = Instruction::ICmp; pre = CmpInst::ICMP_EQ; goto cmp;
            case OP_LSS: other_instr = Instruction::ICmp; pre = CmpInst::ICMP_SLT; goto cmp;
            case OP_GTR: other_instr = Instruction::ICmp; pre = CmpInst::ICMP_SGT; goto cmp;
        }
    } /*else if (exp_type == "cint") {
        std::string op_symbol = new string("");
        switch (op) {
            case OP_PLUS: op_symbol += "sadd"; goto cmath;
            case OP_MINUS: op_symbol += "ssub"; goto cmath;
            case OP_TIMES: op_symbol += "smul"; goto cmath;
            case OP_DIV: instr = Instruction::SDiv; goto math;
        }
        //we should add some error reporting code here if overflow happens
        std::string overflow_code = new string("%res = call {i32, i1} @llvm."+op_symbol+".with.overflow.i32(i32 %a, i32 %b)\n%sum = extractvalue {i32, i1} %res, 0\n%obit = extractvalue {i32, i1} %res, 1\nbr i1 %obit, label %overflow, label %normal\noverflow:\n\nlabel:\n");
        return (Value*) overflow_code;
    }*/
    return NULL;
fmath:
{   BinaryOperator *v = BinaryOperator::Create(instr, llhs,
                                  rrhs, "", context.currentBlock());
    FastMathFlags FMF;
    FMF.setUnsafeAlgebra();
    v->setFastMathFlags(FMF);
    return v;
}
math:
    return BinaryOperator::Create(instr, llhs,
                                  rrhs, "", context.currentBlock());
fcmp:
{   CmpInst *v = CmpInst::Create(other_instr, pre, llhs,
                                     rrhs, "", context.currentBlock());
    FastMathFlags FMF;
    FMF.setUnsafeAlgebra();
    v->setFastMathFlags(FMF);
    return v;
}
cmp:
    return CmpInst::Create(other_instr, pre, llhs,
                                  rrhs, "", context.currentBlock());
}

Value* NUnaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating unary operation " << op << endl;
    auto zero = new NInteger(string("0"), "int");
    auto b_zero = new NBinaryOperator(zero, op, rhs, exp_type);
    if (op == OP_MINUS) {
        return b_zero->codeGen(context);
    } else if (op == OP_NOT) {
        return BinaryOperator::CreateNot(rhs->codeGen(context), "", context.currentBlock());
    }
    std::cout << "Invalid unary operation!" << endl;
    return NULL;
}

Value* NInteger::codeGen(CodeGenContext& context) {
    std::cout << "Creating integer: " << value << endl;
    return ConstantInt::get(Type::getInt32Ty(MyContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context) {
    std::cout << "Creating float: " << value << endl;
    return ConstantFP::get(Type::getFloatTy(MyContext), value);
}

Value* NReturnStatement::codeGen(CodeGenContext& context) {
    if (!exp) {
        std::cout << "Generating return code for no return value" << endl;
        context.setCurrentReturnValue(NULL);
        return NULL;
    }
    std::cout << "Generating return code for " << typeid(exp).name() << endl;
    Value *returnValue = exp->codeGen(context);
    context.setCurrentReturnValue(returnValue);
    return returnValue;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context) {
    if (!exp)
        return NULL;
    return exp->codeGen(context);
}

Value* NFuncCall::codeGen(CodeGenContext& context) {
    Function *function = context.module->getFunction(funcname->name.c_str());
    if (function == NULL) {
        std::cerr << "no such function " << funcname->name << endl;
    }
    std::vector<Value*> args;
    ExpressionList::const_iterator it;
    for (it = (exps->exps).begin(); it != (exps->exps).end(); it++) {
        args.push_back((**it).codeGen(context));
    }
    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
    std::cout << "Created method call: " << funcname->name << endl;
    return call;
}

Value* NBlock::codeGen(CodeGenContext& context) {
    StatementList::const_iterator it;
    Value *last = NULL;
    for (it = statements.begin(); it != statements.end(); it++) {
        //std::cout << "Generating code for " << typeid(**it).name() << endl;
        last = (*it)->codeGen(context);
    }
    std::cout << "Created block" << endl;
    return last;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context) {
    vector<Type*> argTypes;
    VarDeclList::const_iterator it;
    if (vdecls) {
        for (it = (*vdecls).begin(); it != (*vdecls).end(); it++) {
            argTypes.push_back(typeOf((**it).type));
        }
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, globid->name.c_str(), context.module);
    BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);

    context.pushBlock(bblock);

    Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;
    
    if (vdecls) {
        for (it = (*vdecls).begin(); it != (*vdecls).end(); it++) {
            (**it).codeGen(context);
            argumentValue = &*argsValues++;
            argumentValue->setName((*it)->var->name->name.c_str());
            StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->var->name->name], false, bblock);
        }
    }

    block->codeGen(context);
    ReturnInst::Create(MyContext, context.getCurrentReturnValue(), bblock);
    
    context.popBlock();
    std::cout << "Created function: " << globid->name << endl;
    return function;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context) {
    vector<Type*> argTypes;
    TypeDeclList::const_iterator it;
    for (it = (*tdecls).begin(); it != (*tdecls).end(); it++) {
        argTypes.push_back(typeOf((**it)));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, globid->name.c_str(), context.module);
    std::cout << "Created extern: " << globid->name << endl;
    return function;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context) {
    std::cout << "Creating variable declaration " << type << " " << var->name->name << endl;
    AllocaInst *alloc = new AllocaInst(typeOf(type), 0, var->name->name.c_str(), context.currentBlock());
    context.locals()[var->name->name] = alloc;
    return alloc;
}

Value* NIfStatement::codeGen(CodeGenContext& context) {
    /*Value *CondV = exp->codeGen(context);
    if (!CondV) return nullptr;
    
    //cout << "1111" << endl;
    CondV = Builder.CreateICmpNE(CondV, Builder.getInt1(0), "ifcond");
    Function *TheFunction = context.currentBlock()->getParent();
    
    //cout << "2111" << endl;
    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    //BasicBlock *ThenBB = BasicBlock::Create(MyContext, "then", TheFunction);
    BasicBlock *ThenBB = BasicBlock::Create(MyContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(MyContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(MyContext, "ifcont");
    
    //cout << "3111" << endl;
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);
    
    // Emit then value.
    Builder.SetInsertPoint(ThenBB);
    
    Value *ThenV = stmt->codeGen(context);
    if (!ThenV) return nullptr;
    
    Builder.CreateBr(MergeBB);
    // codeGen of 'Then' can change the current block, update ThenBB for the PHI.
    ThenBB = Builder.GetInsertBlock();
    
    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);
    
    Value *ElseV = NULL;
    if (else_stmt != NULL) {
        Value *ElseV = else_stmt->codeGen(context);
        if (!ElseV)
            return nullptr;
    }
    
    Builder.CreateBr(MergeBB);
    // codeGen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder.GetInsertBlock();
    
    // Emit merge block.
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    //PHINode *PN = Builder.CreatePHI(Type::getInt32Ty(MyContext), 2, "iftmp");
    
    //PN->addIncoming(ThenV, ThenBB);
    //PN->addIncoming(ElseV, ElseBB);
    //return PN;
    return CondV;*/
    
    return NULL;
}

Value* NWhileStatement::codeGen(CodeGenContext& context) {
    return NULL;
}

Value* NPrintExpressionStatement::codeGen(CodeGenContext& context) {
    Function *function = context.module->getFunction("printf");
    
    IRBuilder <> builder(context.module->getContext());
    builder.SetInsertPoint(context.currentBlock());
    
    Value *str;
    if (exp->exp_type.find("int") != string::npos)
        str = builder.CreateGlobalStringPtr("%d");
    else str = builder.CreateGlobalStringPtr("%f");
    std::vector <Value *> int32_call_params;
    int32_call_params.push_back(str);
    
    int32_call_params.push_back(exp->codeGen(context));
    CallInst *call = CallInst::Create(function, int32_call_params, "", context.currentBlock());
    return call;
}

Value* NPrintSlitStatement::codeGen(CodeGenContext& context) {
    Function *function = context.module->getFunction("printf");
    
    IRBuilder <> builder(context.module->getContext());
    builder.SetInsertPoint(context.currentBlock());
    
    string s_temp = slit.substr(1, slit.size() - 2);
    Value *str = builder.CreateGlobalStringPtr(s_temp);
    std::vector <Value *> int32_call_params;
    int32_call_params.push_back(str);
    
    CallInst *call = CallInst::Create(function, int32_call_params, "", context.currentBlock());
    return call;
}

Value* NAssignStatement::codeGen(CodeGenContext& context) {
    Value* alloc = vdecl->codeGen(context);
    if (exp != NULL) {
        NAssignment_codeGen(context, vdecl->var, exp);
    }
    return alloc;
}

Value* NExpressionList::codeGen(CodeGenContext& context) {
    Value* last = NULL;
    if (!exps.empty()) {
        for (auto exp : exps) {
            last = exp->codeGen(context);
        }
    }
    return last;
}

Value* NExternList::codeGen(CodeGenContext& context) {
    for (auto it = externs.begin(); it != externs.end(); ++it) {
        (**it).codeGen(context);
    }
    return NULL;
}

Value* NFuncList::codeGen(CodeGenContext& context) {
    for (auto it = funcs.begin(); it != funcs.end(); ++it) {
        (**it).codeGen(context);
    }
    return NULL;
}

Value* NProgram::codeGen(CodeGenContext& context) {
    externs->codeGen(context);
    funcs->codeGen(context);
    return NULL;
}
