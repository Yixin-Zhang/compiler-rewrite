#include <iostream>
#include "node.h"

using namespace std;

extern int yyparse();
extern NProgram* programBlock;

llvm::Function* createPrintfFunction(CodeGenContext& context);
void createCoreFunctions(CodeGenContext& context);
void InitializeFunctionPassManager(CodeGenContext& context);
