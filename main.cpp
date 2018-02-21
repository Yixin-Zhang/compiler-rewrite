#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

extern int yyparse();
extern NBlock* programBlock;

void createCoreFunctions(CodeGenContext& context);

int main(int argc, char **argv) {
 	// By default DO NOT print AST tree to stdout uness -emit-ast is provided.
	bool emit_ast = false;
	string inputfile, outputfile;
	for (int i = 1; i < argc; ++i) {
		if (string(argv[i]).compare(string("-emit-ast")) == 0) {
			emit_ast = true;
			continue;
		}
		if (string(argv[i]).compare(string("-o")) == 0) {
			if (i == argc - 1) {
				cout << "Please specify output file after -o option!" << endl;
				exit(1);
			}
			outputfile = string(argv[i + 1]);
			i += 1;
			continue;
		}
		if (!inputfile.empty()) {
			cout << "Ignored redundant command line argument: " << string(argv[i]) << endl;
		} else {
			inputfile = string(argv[1]);
		}
	}

	if (inputfile.empty()) {
		cout << "Please specify an inputfile to parse." << endl;
		exit(1);
	}

	FILE *input = fopen(inputfile.c_str(), "r");
	  // make sure it's valid:
	if (!input) {
		cout << "Error while opening inputfile: " << inputfile << endl;
		exit(1);
	}

	cout << "Parsing input ..." << endl;
	yyin = input;
	do {
		yyparse();
	} while (!feof(yyin));

 	// AST YAML output
	if (!outputfile.empty()) {
		cout << "Writing parsed AST tree to outputfile: " << outputfile << endl;
		if (programBlock != NULL) {
			ofstream ofs(outputfile);
			programBlock->yaml_output(ofs, 0);
		}
	} else if (emit_ast) {
    	// YAML to stdout
		if (programBlock != NULL) {
			programBlock->yaml_output(cout, 0);;
		}
	}

	/*
	 * Code generation
	*/
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
	CodeGenContext context;
	createCoreFunctions(context);
	context.generateCode(*programBlock);
	context.runCode();
	
	return 0;







	cout << "Done." << endl;
	return 0;
}
