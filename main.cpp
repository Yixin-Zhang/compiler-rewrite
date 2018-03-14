#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

extern int yyparse();
extern FILE *yyin;
extern NProgram* programBlock;

void createCoreFunctions(CodeGenContext& context);

int main(int argc, char **argv) {
 	// By default DO NOT print AST tree to stdout uness -emit-ast is provided.
	bool emit_ast = false;
	// Only support -O3 optimization level.
	bool opt = false;
	bool jit = false;
	string inputfile, outputfile, outputfile_codegen;
	for (int i = 1; i < argc; ++i) {
		if (string(argv[i]).compare(string("-emit-ast")) == 0) {
			emit_ast = true;
			continue;
		}
		if (string(argv[i]).compare(string("-o1")) == 0) {
			if (i == argc - 1) {
				cout << "Please specify output file after -o option!" << endl;
				exit(1);
			}
			outputfile = string(argv[i + 1]);
			i += 1;
			continue;
		}
		if (string(argv[i]).compare(string("-O")) == 0) {
			opt = true;
			continue;
		}
		if (string(argv[i]).compare(string("-jit")) == 0) {
			jit = true;
			continue;
		}
		if (!inputfile.empty()) {
			cout << "Ignored redundant command line argument: " << string(argv[i]) << endl;
		} else {
			inputfile = string(argv[i]);
		}
        if (string(argv[i]).compare(string("-o2")) == 0) {
            if (i == argc - 1) {
                cout << "Please specify output file after -o option!" << endl;
                exit(1);
            }
            outputfile_codegen = string(argv[i + 1]);
            i += 1;
            continue;
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
	context.setOpt(opt);
	context.setJit(jit);
	createCoreFunctions(context);
	context.generateCode(*programBlock);	
	context.printGenCode();
    if (!outputfile_codegen.empty()) {
        cout << "Writing generated code to outputfile: " << outputfile_codegen << endl;
        if (programBlock != NULL) {
            ofstream ofs(outputfile_codegen);
            string os;
            raw_string_ostream ros(os);
            context.printToFile(ros);
            auto a = os.find("  ret void\n");
            os.insert(a, "  call void @run()\n");
            ofs << os;
        }
    }
	//context.runCode();

	cout << "Done." << endl;
	return 0;
}
