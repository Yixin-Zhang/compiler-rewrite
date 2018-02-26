#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace llvm;

Value* NAssignment_codeGen(CodeGenContext& context, NVariable *lhs, NExpression *rhs) {
    std::cout << "Creating assignment... " << endl;
    if (context.locals().find(lhs->name.name) == context.locals().end()) {
        std::cerr << "undeclared variable " << lhs->name.name << endl;
        return NULL;
    }
    return new StoreInst(rhs->codeGen(context), context.locals()[lhs->name.name], false, context.currentBlock());
}

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
        return Type::getInt64Ty(MyContext);
    }
    else if (type.name.compare("double") == 0) {
        return Type::getDoubleTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

static Type *typeOf(const string& type) {
    if (type.compare("int") == 0) {
        return Type::getInt64Ty(MyContext);
    }
    else if (type.compare("double") == 0) {
        return Type::getDoubleTy(MyContext);
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
    std::cout << "Creating variable: " << name.name << endl;
    return name.codeGen(context);
}

Value* NBinaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating binary operation " << op << endl;
    Instruction::BinaryOps instr;
    switch (op) {
        case OP_PLUS: instr = Instruction::FAdd; goto math;
        case OP_MINUS: instr = Instruction::FSub; goto math;
        case OP_TIMES: instr = Instruction::FMul; goto math;
        case OP_DIV: instr = Instruction::FDiv; goto math;
        case OP_AND: instr = Instruction::And; goto math;
        case OP_OR: instr = Instruction::Or; goto math;
        case OP_ASSIGN: return NAssignment_codeGen(context, (NVariable*)lhs, rhs);
        /*
         * Deal with comparisons later.
        */
        // case OP_EQL: instr = llvm::CmpInst::FCMP_OEQ; goto math;
        // case OP_LSS: instr = llvm::CmpInst::FCMP_OLT; goto math;
        // case OP_GTR: instr = llvm::CmpInst::FCMP_OGT; goto math;
    }
    return NULL;
math:
    return BinaryOperator::Create(instr, lhs->codeGen(context),
                                  rhs->codeGen(context), "", context.currentBlock());
}

Value* NUnaryOperator::codeGen(CodeGenContext& context) {
    auto zero = new NInteger(string("0"));
    auto neg_one = new NInteger(string("-1"));
    std::cout << "Creating unary operation " << op << endl;
    Instruction::BinaryOps instr;
    switch (op) {
        case OP_MINUS:
        {
            instr = Instruction::FSub;
            return BinaryOperator::Create(instr, zero->codeGen(context),
                rhs->codeGen(context), "", context.currentBlock());
        }
        case OP_NOT:
        {
            instr = Instruction::FMul;
            return BinaryOperator::Create(instr, neg_one->codeGen(context),
                rhs->codeGen(context), "", context.currentBlock());
        }
    }
    return NULL;
}

Value* NInteger::codeGen(CodeGenContext& context) {
    std::cout << "Creating integer: " << value << endl;
    return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context) {
    std::cout << "Creating double: " << value << endl;
    return ConstantFP::get(Type::getDoubleTy(MyContext), value);
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
    for (it = (*exps).begin(); it != (*exps).end(); it++) {
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
        argumentValue->setName((*it)->var->name.name.c_str());
        StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->var->name.name], false, bblock);
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
    return function;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context) {
    std::cout << "Creating variable declaration " << type << " " << var->name.name << endl;
    AllocaInst *alloc = new AllocaInst(typeOf(type), 0, var->name.name.c_str(), context.currentBlock());
    context.locals()[var->name.name] = alloc;
    return alloc;
}

Value* NIfStatement::codeGen(CodeGenContext& context) {
    Value *CondV = exp->codeGen(context);
    if (!CondV)
        return nullptr;
    CondV = Builder.CreateFCmpONE(
         CondV, ConstantFP::get(MyContext, APFloat(0.0)), "ifcond");
    
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    
    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *ThenBB = BasicBlock::Create(MyContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(MyContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(MyContext, "ifcont");
    
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);
    
    // Emit then value.
    Builder.SetInsertPoint(ThenBB);
    
    Value *ThenV = stmt->codeGen(context);
    if (!ThenV)
        return nullptr;
    
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
    PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(MyContext), 2, "iftmp");
    
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;
}

Value* NWhileStatement::codeGen(CodeGenContext& context) {
    /*
    Value *EndCond = exp->codeGen(context);
    if (!EndCond)
        return nullptr;
    
    // Make the new basic block for the loop header, inserting after current
    // block.
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder.GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(MyContext, "loop", TheFunction);
    
    // Insert an explicit fall through from the current block to the LoopBB.
    Builder.CreateBr(LoopBB);
    
    // Start insertion in LoopBB.
    Builder.SetInsertPoint(LoopBB);
    
    // Start the PHI node with an entry for Start.
    PHINode *Variable = Builder.CreatePHI(Type::getDoubleTy(MyContext),
                                          2, VarName.c_str());
    Variable->addIncoming(StartVal, PreheaderBB);
    
    // Within the loop, the variable is defined equal to the PHI node.  If it
    // shadows an existing variable, we have to restore it, so save it now.
    Value *OldVal = NamedValues[VarName];
    NamedValues[VarName] = Variable;
    
    // Emit the body of the loop.  This, like any other expr, can change the
    // current BB.  Note that we ignore the value computed by the body, but don't
    // allow an error.
    if (!Body->codeGen())
        return nullptr;
    
    // Emit the step value.
    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codeGen();
        if (!StepVal)
            return nullptr;
    } else {
        // If not specified, use 1.0.
        StepVal = ConstantFP::get(MyContext, APFloat(1.0));
    }
    
    Value *NextVar = Builder.CreateFAdd(Variable, StepVal, "nextvar");
    
    // Compute the end condition.
    Value *EndCond = End->codeGen();
    if (!EndCond)
        return nullptr;
    
    // Convert condition to a bool by comparing non-equal to 0.0.
    EndCond = Builder.CreateFCmpONE(EndCond, ConstantFP::get(MyContext, APFloat(0.0)), "loopcond");
    
    // Create the "after loop" block and insert it.
    BasicBlock *LoopEndBB = Builder.GetInsertBlock();
    BasicBlock *AfterBB =
    BasicBlock::Create(TheContext, "afterloop", TheFunction);
    
    // Insert the conditional branch into the end of LoopEndBB.
    Builder.CreateCondBr(EndCond, LoopBB, AfterBB);
    
    // Any new code will be inserted in AfterBB.
    Builder.SetInsertPoint(AfterBB);
    
    // Add a new entry to the PHI node for the backedge.
    Variable->addIncoming(NextVar, LoopEndBB);
    
    // Restore the unshadowed variable.
    if (OldVal)
        NamedValues[VarName] = OldVal;
    else
        NamedValues.erase(VarName);
    
    // for expr always returns 0.0.
    return Constant::getNullValue(Type::getDoubleTy(MyContext));
    */
    return NULL;
}

Value* NPrintExpressionStatement::codeGen(CodeGenContext& context) {
    // Function *CalleeF = context.module->getOrInsertFunction(
    //     "printf", FunctionType::get(IntegerType::getInt32Ty(MyContext),
    //     PointerType::get(Type::getInt8Ty(MyContext), 0), true));
    Function* CalleeF = context.module->getFunction("printf");
    std::vector<Value *> ArgsV;
    ArgsV.push_back(exp->codeGen(context));
    return Builder.CreateCall(CalleeF, ArgsV, "printfCall");
}

Value* NPrintSlitStatement::codeGen(CodeGenContext& context) {
    // Function *CalleeF = context.module->getOrInsertFunction(
    //     "printf", FunctionType::get(IntegerType::getInt32Ty(MyContext),
    //     PointerType::get(Type::getInt8Ty(MyContext), 0), true));
    Function* CalleeF = context.module->getFunction("printf");
    std::vector<Value *> ArgsV;
    ArgsV.push_back(llvm::ConstantDataArray::getString(MyContext, StringRef(slit.c_str())));
    return Builder.CreateCall(CalleeF, ArgsV, "printfCall");
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
