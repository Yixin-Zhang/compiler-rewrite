#ifndef CODEGEN_H_
#define CODEGEN_H_

#include <stack>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Bitcode/BitstreamReader.h>
#include <llvm/Bitcode/BitstreamWriter.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include "node.h"
#include "parser.hpp"

using namespace llvm;

static LLVMContext MyContext;
static IRBuilder<> Builder(MyContext);

// static std::unique_ptr<Module> TheModule;
// static std::map<std::string, Value *> NamedValues;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value*> locals;
    std::map<std::string, std::string> locals_type;
};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;
    Function *mainFunction;
    bool opt;
    bool jit;

public:
    Module *module;
    std::unique_ptr<legacy::FunctionPassManager> TheFPM;
    CodeGenContext() {
        module = new Module("My cool compiler", MyContext);
        TheFPM = nullptr;
        opt = false;
        jit = false;
    }

    void setOpt(bool to_opt) {
        opt = to_opt;
    }

    bool getOpt() {
        return opt;
    }

    void setJit(bool to_jit) {
        jit = to_jit;
    }

    bool getJit() {
        return jit;
    }

    void generateCode(NProgram& root);

    // Print out all of the generated code.
    void printGenCode() {
        std::cout << "Printing generated code...\n";
        module->print(errs(), nullptr);
    };
    
    void printToFile(raw_ostream &OS) {
        module->print(OS, nullptr);
    }
    
    GenericValue runCode();

    std::map<std::string, Value*>& locals() {
        return blocks.top()->locals ;
    }
    
    std::map<std::string, std::string>& locals_type() {
        return blocks.top()->locals_type ;
    }
    
    BasicBlock *currentBlock() {
        if (!blocks.empty()) {
            return blocks.top()->block;
        }
    	return NULL;
    }
    
    void pushBlock(BasicBlock *block) {
    	blocks.push(new CodeGenBlock());
    	blocks.top()->returnValue = NULL;
    	blocks.top()->block = block;
    }
    
    void popBlock() {
    	CodeGenBlock *top = blocks.top();
    	blocks.pop();
        //delete top;
    }

    void setCurrentReturnValue(Value *value) {
        if (!blocks.empty()) {
            blocks.top()->returnValue = value;
        }
    }

    Value* getCurrentReturnValue() {
        if (!blocks.empty()) {
            return blocks.top()->returnValue;
        }
        return NULL;
    }
    
};

#endif
