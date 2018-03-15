#include "codegen.h"
#include "parser.hpp"

using namespace llvm;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NProgram& root) {
    std::cout << "Generating code...\n";
    
    /* Create the top level interpreter function to call as entry */
    vector<Type*> argTypes;
    FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
    mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
    BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction, 0);
    /*std::vector<Value*> args;
    ExpressionList::const_iterator it;
    CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());*/
    
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
        delete pm;
    }
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
    std::cout << (getJit() ? "JIT " : "") << "Running code...\n";
    vector<GenericValue> noargs;
    Function* TheRunF = cast<Function>(module->getFunction("run"));
    if (TheRunF == NULL) {
        cout << "run() does not exist!" << endl;
    }
    std::string err;
    EngineBuilder* eb = new EngineBuilder(unique_ptr<Module>(module));
    if (getJit()) {

        eb->setEngineKind(llvm::EngineKind::JIT).setErrorStr(&err);
    }
    ExecutionEngine *ee = eb->create();

    if (!ee) {
        cout << "Error while creating ExecutionEngine in runCode()!" << endl;
    }
    // GenericValue v = ee->runFunction(mainFunction, noargs);  // this is wrong!
    ee->finalizeObject();
    GenericValue v = ee->runFunction(TheRunF, noargs);
    std::cout << "\nCode was run.\n";
    // std::cout << "Result: " << v.IntVal << std::endl;
    return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type) {
    if (type.name.compare("int") == 0 || type.name.compare("cint") == 0) {
        return Type::getInt32Ty(MyContext);
    }
    else if (type.name.compare("float") || type.name.compare("sfloat") == 0) {
        return Type::getFloatTy(MyContext);
    }
    else if (type.name.find("ref int") != string::npos || type.name.find("ref cint") != string::npos) {
        return Type::getInt32PtrTy(MyContext);
    }
    else if (type.name.find("ref float") != string::npos || type.name.find("ref sfloat") != string::npos) {
        return Type::getFloatPtrTy(MyContext);
    }
    return Type::getVoidTy(MyContext);
}

static Type *typeOf(const string& type) {
    if (type.compare("int") == 0 || type.compare("cint") == 0) {
        return Type::getInt32Ty(MyContext);
    }
    else if (type.compare("float") == 0 || type.compare("sfloat") == 0) {
        return Type::getFloatTy(MyContext);
    }
    else if (type.find("ref int") != string::npos || type.find("ref cint") != string::npos) {
        return Type::getInt32PtrTy(MyContext);
    }
    else if (type.find("ref float") != string::npos || type.find("ref sfloat") != string::npos) {
        return Type::getFloatPtrTy(MyContext);
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
    return Builder.CreateLoad(context.locals()[name], "");
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
    
    //deal with type conversion
    if (lhs->exp_type.find("float") != string::npos) {
        if (rhs->exp_type.find("float") != string::npos)
            assign_rhs = assign_gen;
        else assign_rhs = Builder.CreateSIToFP(assign_gen, Type::getFloatTy(context.module->getContext()), "");
    } else if (lhs->exp_type.find("int") != string::npos) {
        if (rhs->exp_type.find("int") != string::npos)
            assign_rhs = assign_gen;
        else assign_rhs = Builder.CreateFPToSI(assign_gen, Type::getInt32Ty(context.module->getContext()), "");
    } else assign_rhs = assign_gen;
    
    //deal with ref type
    Value *clhs = assign_rhs;
    string s_temp = context.locals_type()[((NVariable*)hs)->name->name];
    if (s_temp.find("ref int") != string::npos || s_temp.find("ref cint") != string::npos || s_temp.find("ref float") != string::npos || s_temp.find("ref sfloat") != string::npos) {
        clhs = Builder.CreateLoad(context.locals()[lhs->name->name], "");
        return Builder.CreateStore(assign_rhs, clhs);
    } else return Builder.CreateStore(clhs, context.locals()[lhs->name->name]);
    
    return NULL;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating binary operation " << op << endl;
    Instruction::BinaryOps instr;
    Instruction::OtherOps other_instr;
    CmpInst::Predicate pre;
    switch (op) {
        case OP_AND: instr = Instruction::And; return Builder.CreateAnd(lhs->codeGen(context), rhs->codeGen(context), "");
        case OP_OR: instr = Instruction::Or; return Builder.CreateOr(lhs->codeGen(context), rhs->codeGen(context), "");
        case OP_ASSIGN: return NAssignment_codeGen(context, lhs, rhs);
    }
    
    Value *clhs = lhs->codeGen(context);
    Value *crhs = rhs->codeGen(context);
    
    //deal with ref type
    if (lhs->mark == 1) {
        string s_temp = context.locals_type()[((NVariable*)lhs)->name->name];
        if (s_temp.find("ref int") != string::npos || s_temp.find("ref cint") != string::npos) {
            clhs = Builder.CreateLoad(clhs, "");
        } else if (s_temp.find("ref float") != string::npos || s_temp.find("ref sfloat") != string::npos) {
            clhs = Builder.CreateLoad(clhs, "");
        }
    }
    if (rhs->mark == 1) {
        string s_temp = context.locals_type()[((NVariable*)rhs)->name->name];
        if (s_temp.find("ref int") != string::npos || s_temp.find("ref cint") != string::npos) {
            crhs = Builder.CreateLoad(crhs, "");
        } else if (s_temp.find("ref float") != string::npos || s_temp.find("ref sfloat") != string::npos) {
            crhs = Builder.CreateLoad(crhs, "");
        }
    }
    
    //deal with type conversion
    Value *llhs, *rrhs;
    if (exp_type.find("float") != string::npos) {
        if (lhs->exp_type.find("float") != string::npos) {
            llhs = clhs;
        } else {
            llhs = Builder.CreateSIToFP(clhs, Type::getFloatTy(context.module->getContext()), "");
        }
        if (rhs->exp_type.find("float") != string::npos) {
            rrhs = crhs;
        } else {
            rrhs = Builder.CreateSIToFP(crhs, Type::getFloatTy(context.module->getContext()), "");
        }
    } else if (exp_type.find("int") != string::npos) {
        if (lhs->exp_type.find("int") != string::npos) {
            llhs = clhs;
        } else {
            llhs = Builder.CreateFPToSI(clhs, Type::getInt32Ty(context.module->getContext()), "");
        }
        if (rhs->exp_type.find("int") != string::npos) {
            rrhs = crhs;
        } else {
            rrhs = Builder.CreateFPToSI(crhs, Type::getInt32Ty(context.module->getContext()), "");
        }
    } else {
        llhs = clhs;
        rrhs = crhs;
    }
    
    Value *v;
    if (exp_type == "float" || exp_type == "sfloat") {
        switch (op) {
            case OP_PLUS: v = Builder.CreateFAdd(llhs, rrhs, ""); break;
            case OP_MINUS: v = Builder.CreateFSub(llhs, rrhs, ""); break;
            case OP_TIMES: v = Builder.CreateFMul(llhs, rrhs, ""); break;
            case OP_DIV: v = Builder.CreateFDiv(llhs, rrhs, ""); break;
            case OP_EQL: v = Builder.CreateFCmpOEQ(llhs, rrhs, ""); break;
            case OP_LSS: v = Builder.CreateFCmpOLT(llhs, rrhs, ""); break;
            case OP_GTR: v = Builder.CreateFCmpOGT(llhs, rrhs, ""); break;
        }
        if (exp_type == "float") {
            //deal with fast math for float type
            FastMathFlags FMF;
            FMF.setUnsafeAlgebra();
            ((BinaryOperator *)v)->setFastMathFlags(FMF);
        }
        return v;
    } else if (exp_type == "int" || exp_type == "cint") {
        switch (op) {
            case OP_EQL: return Builder.CreateICmpEQ(llhs, rrhs, "");
            case OP_LSS: return Builder.CreateICmpSLT(llhs, rrhs, "");
            case OP_GTR: return Builder.CreateICmpSGT(llhs, rrhs, "");
        }
        if (exp_type == "int") {
            switch (op) {
                case OP_PLUS: return Builder.CreateAdd(llhs, rrhs, "");
                case OP_MINUS: return Builder.CreateSub(llhs, rrhs, "");
                case OP_TIMES: return Builder.CreateMul(llhs, rrhs, "");
                case OP_DIV: return Builder.CreateSDiv(llhs, rrhs, "");
            }
        }  else if (exp_type == "cint") {
            if (op == OP_DIV) {
                /*//create b == 0
                NInteger *zero = new NInteger(string("0"), "int");
                NBinaryOperator *cond1 = new NBinaryOperator(rhs, OP_EQL, zero, "int");
                
                //create  print "Divided by 0!"; c = INT_MAX;
                NPrintSlitStatement *printThen1 = new NPrintSlitStatement("Divided by 0!");
                
                
                //create b == -1 && a == INT_MIN
                NInteger *neg_one = new NInteger(string("-1"), "int");*/
                return Builder.CreateSDiv(llhs, rrhs, "");
            } else {
                Value *F;
                string error_info;
                switch (op) {
                    case OP_PLUS: {
                        F = Intrinsic::getDeclaration(context.module, Intrinsic::sadd_with_overflow, Type::getInt32Ty(MyContext));
                        error_info = ",Add operation overflows!,";
                        break;
                    }
                    case OP_MINUS: {
                        F = Intrinsic::getDeclaration(context.module, Intrinsic::ssub_with_overflow, Type::getInt32Ty(MyContext));
                        error_info = ",Sub operation overflows!,";
                        break;
                    }
                    case OP_TIMES: {
                        F = Intrinsic::getDeclaration(context.module, Intrinsic::smul_with_overflow, Type::getInt32Ty(MyContext));
                        error_info = ",Mul operation overflows!,";
                        break;
                    }
                }
                auto *SBinaryOperatorWithOverflow = Builder.CreateCall(F, {llhs, rrhs}, "t");
                auto *SBinaryOperator = Builder.CreateExtractValue(SBinaryOperatorWithOverflow, 0, "binaryop");
                auto *Overflow = Builder.CreateExtractValue(SBinaryOperatorWithOverflow, 1, "obit");
                
                Function *TheFunction = context.currentBlock()->getParent();
                BasicBlock *NormalBB = BasicBlock::Create(MyContext, "normal", TheFunction);
                BasicBlock *OverflowBB = BasicBlock::Create(MyContext, "overflow");
                BasicBlock *MergeBB = BasicBlock::Create(MyContext, "continue");
                
                Builder.CreateCondBr(Overflow, NormalBB, OverflowBB);
                
                Builder.SetInsertPoint(NormalBB);
                Builder.CreateBr(MergeBB);
                
                TheFunction->getBasicBlockList().push_back(OverflowBB);
                Builder.SetInsertPoint(OverflowBB);
                auto printError = new NPrintSlitStatement(error_info);
                printError->codeGen(context);
                Builder.CreateBr(MergeBB);
                
                TheFunction->getBasicBlockList().push_back(MergeBB);
                Builder.SetInsertPoint(MergeBB);
                
                return SBinaryOperator;
            }
        }
    }
    return NULL;
}

Value* NUnaryOperator::codeGen(CodeGenContext& context) {
    std::cout << "Creating unary operation " << op << endl;
    auto zero = new NInteger(string("0"), "int");
    auto b_zero = new NBinaryOperator(zero, op, rhs, exp_type);
    if (op == OP_MINUS) {
        return b_zero->codeGen(context);
    } else if (op == OP_NOT) {
        return Builder.CreateNot(rhs->codeGen(context), "");
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
    CallInst *call = Builder.CreateCall(function, makeArrayRef(args), "");
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
    Builder.SetInsertPoint(bblock);  //add here to reset the insert point

    Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;
    
    if (vdecls) {
        for (it = (*vdecls).begin(); it != (*vdecls).end(); it++) {
            (**it).codeGen(context);
            argumentValue = &*argsValues++;
            argumentValue->setName((*it)->var->name->name.c_str());
            StoreInst *inst = Builder.CreateStore(argumentValue, context.locals()[(*it)->var->name->name]);
        }
    }

    block->codeGen(context);
    Builder.CreateRet(context.getCurrentReturnValue());
    
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
    AllocaInst *alloc = Builder.CreateAlloca(typeOf(type), 0, var->name->name.c_str());
    context.locals()[var->name->name] = alloc;
    context.locals_type()[var->name->name] = type;
    return alloc;
}

Value* NIfStatement::codeGen(CodeGenContext& context) {
    Value *CondV = exp->codeGen(context);
    if (!CondV) return nullptr;
    
    //CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(MyContext, APFloat(0.0)), "ifcond");
    CondV = Builder.CreateICmpNE(CondV, Builder.getInt1(0), "");
    //CmpInst::Create(Instruction::ICmp, CmpInst::ICMP_NE, CondV, Builder.getInt1(0), "ifcond", context.currentBlock());
    Function *TheFunction = context.currentBlock()->getParent();
    
    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *ThenBB = BasicBlock::Create(MyContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(MyContext, "else");
    BasicBlock *MergeBB = BasicBlock::Create(MyContext, "ifcon");
    
    Builder.CreateCondBr(CondV, ThenBB, ElseBB);
    
    // Emit then value.
    Builder.SetInsertPoint(ThenBB);
    
    Value *ThenV = stmt->codeGen(context);
    
    Builder.CreateBr(MergeBB);
    // codeGen of 'Then' can change the current block, update ThenBB for the PHI.
    ThenBB = Builder.GetInsertBlock();
    
    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);
    
    Value *ElseV = NULL;
    if (else_stmt != nullptr) {
        ElseV = else_stmt->codeGen(context);
    }
    
    Builder.CreateBr(MergeBB);
    // codeGen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder.GetInsertBlock();
    
    // Emit merge block.
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    
    /*PHINode *PN = PHINode::Create(Type::getInt32Ty(MyContext), 2, "", context.currentBlock());
    PN->addIncoming(ThenV, ThenBB);
    PN->addIncoming(ElseV, ElseBB);
    return PN;*/
    
    //return CondV;
    return NULL;
}

Value* NWhileStatement::codeGen(CodeGenContext& context) {
    Function *TheFunction = context.currentBlock()->getParent();
    
    // Create blocks for the then and else cases.  Insert the 'while condition' block at the
    // end of the function.
    BasicBlock *CondBB = BasicBlock::Create(MyContext, "whilecond", TheFunction);
    BasicBlock *ThenBB = BasicBlock::Create(MyContext, "loop");
    BasicBlock *MergeBB = BasicBlock::Create(MyContext, "whilecon");
    
    Builder.CreateBr(CondBB);
    
    Builder.SetInsertPoint(CondBB);
    Value *CondV = exp->codeGen(context);
    Builder.CreateCondBr(CondV, ThenBB, MergeBB);
    
    // Emit then value
    TheFunction->getBasicBlockList().push_back(ThenBB);
    Builder.SetInsertPoint(ThenBB);
    Value *ThenV = stmt->codeGen(context);
    
    Builder.CreateBr(CondBB);
    // codeGen of 'Then' can change the current block, update ThenBB.
    ThenBB = Builder.GetInsertBlock();
    
    // Emit merge block.
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    return NULL;
}

Value* NPrintExpressionStatement::codeGen(CodeGenContext& context) {
    Function *function = context.module->getFunction("printf");
    
    //Builder.SetInsertPoint(context.currentBlock());
    
    Value *str;
    if (exp->exp_type.find("int") != string::npos)
        str = Builder.CreateGlobalStringPtr("%d");
    else str = Builder.CreateGlobalStringPtr("%f");
    std::vector <Value *> int32_call_params;
    int32_call_params.push_back(str);
    
    int32_call_params.push_back(exp->codeGen(context));
    CallInst *call = Builder.CreateCall(function, int32_call_params, "");
    return call;
}

Value* NPrintSlitStatement::codeGen(CodeGenContext& context) {
    Function *function = context.module->getFunction("printf");
    
    //Builder.SetInsertPoint(context.currentBlock());
    
    string s_temp = slit.substr(1, slit.size() - 2);
    Value *str = Builder.CreateGlobalStringPtr(s_temp);
    std::vector <Value *> int32_call_params;
    int32_call_params.push_back(str);
    
    CallInst *call = Builder.CreateCall(function, int32_call_params, "");
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
    if (externs != NULL) {
        externs->codeGen(context);
    }
    if (funcs == NULL) {
        cout << "Program has no functions!\n";
    }
    funcs->codeGen(context);
    return NULL;
}
