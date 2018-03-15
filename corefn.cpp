#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

llvm::Function* createPrintfFunction(CodeGenContext& context) {
    std::vector<llvm::Type*> printf_arg_types;
    printf_arg_types.push_back(llvm::Type::getInt8PtrTy(MyContext)); //char*

    llvm::FunctionType* printf_type =
    llvm::FunctionType::get(
        llvm::Type::getInt32Ty(MyContext), printf_arg_types, true);

    llvm::Function *func = llvm::Function::Create(
        printf_type, llvm::Function::ExternalLinkage,
        llvm::Twine("printf"),
        context.module
        );
    func->setCallingConv(llvm::CallingConv::C);
    return func;
}

void createCoreFunctions(CodeGenContext& context){
    llvm::Function* printfFn = createPrintfFunction(context);
}

void InitializeFunctionPassManager(CodeGenContext& context) {
    if (!context.getOpt()) return;
    cout << "Initializing optimizer..." << endl;
    // Create a new pass manager attached to it.
    context.TheFPM = llvm::make_unique<legacy::FunctionPassManager>(context.module);

    // Promote allocas to registers.
    context.TheFPM->add(createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    context.TheFPM->add(createInstructionCombiningPass());
    // Reassociate expressions.
    context.TheFPM->add(createReassociatePass());
    // Eliminate Common SubExpressions.
    context.TheFPM->add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    context.TheFPM->add(createCFGSimplificationPass());

    context.TheFPM->doInitialization();
    // cout << "TheFPM: " << (context.TheFPM ? "is not null" : "is null") << endl;
}
