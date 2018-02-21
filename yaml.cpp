#include "node.h"

void print_indent(ostream& os, int indent) {
	os << std::string(indent, ' ');
}

void NIdentifier::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "************* This is wrong!!! *************";
}

void NVariable::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
   os << "name: varval\n";

   print_indent(os, indent);
   os << "var: " << name.name << "\n";
}

void NBinaryOperator::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: binop\n";

	print_indent(os, indent);
	os << "op: ";
	switch(op) {
		case OP_TIMES: {
			os << "mul";
			break;
		}
		case OP_DIV: {
			os << "div";
			break;
		}
		case OP_PLUS: {
			os << "add";
			break;
		}
		case OP_MINUS: {
			os << "sub";
			break;
		}
		case OP_EQL: {
			os << "eq";
			break;
		}
		case OP_LSS: {
			os << "lt";
			break;
		}
		case OP_GTR: {
			os << "gt";
			break;
		}
		case OP_AND: {
			os << "and";
			break;
		}
		case OP_OR: {
			os << "or";
			break;
		}
		case OP_ASSIGN: {
			os << "assign";
		}
		default:
		os << "??? UNsupported binop.";
	}
	os << "\n";
	print_indent(os, indent);
	os << "lhs:\n";
	lhs->yaml_output(os, indent+2);
	print_indent(os, indent);
	os << "rhs:\n";
	rhs->yaml_output(os, indent+2);
}

void NUnaryOperator::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: uop\n";

	print_indent(os, indent);
	os << "op: ";
	switch(op) {
		case OP_NOT: {
			os << "not";
			break;
		}
		case OP_MINUS: {
			os << "neg";
			break;
		}
	}
	os << "\n";
	print_indent(os, indent);
	os << "exp:\n";
	rhs->yaml_output(os, indent+2);
}

void NInteger::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: lit\n";

	print_indent(os, indent);
	os << "value: " << value << "\n";
}

void NDouble::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: lit\n";

	print_indent(os, indent);
	os << "value: " << value << "\n";
}

void NFuncCall::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: funccall\n";

	print_indent(os, indent);
	os << "globid: " << funcname << "\n";
	print_indent(os, indent);
	os << "params:\n";
	for (auto exp : *exps) {
		exp->yaml_output(os, indent+2);
	}
}

void NBlock::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: blk\n";
	print_indent(os, indent);
	os << "contents:\n";

	if (statements.size() > 0) {
		indent += 2;
		print_indent(os, indent);
		os << "name: stmts\n";
		print_indent(os, indent);
		os << "stmts:\n";
		for (auto stmt : statements) {
			print_indent(os, indent + 2);
			os << "-\n";
			stmt->yaml_output(os, indent + 4);
		}
	}
}

void NReturnStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: ret\n";
	if (exp != NULL) {
		print_indent(os, indent);
		os << "exp:\n";
		exp->yaml_output(os, indent+2);
	}
}

void NAssignStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: vardeclstmt\n";
	print_indent(os, indent);
	os << "vdecl:\n";
	vdecl->yaml_output(os, indent+2);
	print_indent(os, indent);
	os << "exp:\n";
	exp->yaml_output(os, indent+2);
}

void NExpressionStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: expstmt\n";
	print_indent(os, indent);
	os << "exp:\n";
	exp->yaml_output(os, indent + 2);
}

void NWhileStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: while\n";
	print_indent(os, indent);
	os << "cond:\n";
	exp->yaml_output(os, indent+2);
	print_indent(os, indent);
	os << "stmt:\n";
	stmt->yaml_output(os, indent+2);
}

void NIfStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: if\n";
	print_indent(os, indent);
	os << "cond:\n";
	exp->yaml_output(os, indent+2);
	print_indent(os, indent);
	os << "stmt:\n";
	stmt->yaml_output(os, indent+2);
	if (else_stmt != NULL) {
		print_indent(os, indent);
		os << "else_stmt:\n";
		else_stmt->yaml_output(os, indent+2);
	}
}

void NPrintExpressionStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: print\n";
	print_indent(os, indent);
	os << "exp:\n";
	exp->yaml_output(os, indent+2);
}

void NPrintSlitStatement::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "name: printslit\n";
	print_indent(os, indent);
	os << "string: " << slit << "\n";
}

void NVariableDeclaration::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "node: vdecl\n";
	print_indent(os, indent);
	os << "type: " << type << "\n";
	print_indent(os, indent);
	os << "var: " << var->name.name << "\n";
}

void NFunctionDeclaration::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "-\n";
	print_indent(os, indent + 2);
	os << "name: func\n";
	print_indent(os, indent + 2);
	os << "ret_type: " << type << "\n";
	print_indent(os, indent + 2);
	os << "globid: " << globid->name << "\n";
	if (vdecls != NULL) {
    	// vdecls->yaml_output(os, indent + 2);
		print_indent(os, indent);
		os << "vdecls:\n";
		print_indent(os, indent + 2);
		os << "name: vdecls\n";
		print_indent(os, indent + 2);
		os << "vars:\n";
		for (auto vdecl : *vdecls) {
			print_indent(os, indent + 4);
			os << "-\n";
			vdecl->yaml_output(os, indent + 6);
		}
	}

	if (block != NULL) {
		print_indent(os, indent + 2);
		os << "blk:\n";
		block->yaml_output(os, indent + 4);
	}
}

void NExternDeclaration::yaml_output(ostream& os, int indent) {
	print_indent(os, indent);
	os << "-\n";
	print_indent(os, indent + 2);
	os << "name: extern\n";
	print_indent(os, indent + 2);
	os << "ret_type: " << type << "\n";
	print_indent(os, indent + 2);
	os << "globid: " << globid->name << "\n";

    // tdecls is a vector of strings
	if (tdecls != NULL && tdecls->size() > 0) {
		print_indent(os, indent + 4);
		os << "tdecls:\n";
		print_indent(os, indent + 4);
		os << "name: tdecls\n";
		print_indent(os, indent + 4);
		os << "types:\n";
		for (auto tdecl : *tdecls) {
			print_indent(os, indent + 6);
			os << "- " << *tdecl << "\n";
		}
	}
}

void NExpressionList::yaml_output(ostream& os, int indent) {
	if (exps.size() > 0) {
		print_indent(os, indent);
		os << "name: exps\n";
		print_indent(os, indent);
		os << "exps:\n";
		for (auto exp : exps) {
			print_indent(os, indent+2);
			os << "-\n";
			exp->yaml_output(os, indent+4);
		}
	}
}

void NExternList::yaml_output(ostream& os, int indent) {
	if (externs.size() > 0) {
		os << "externs:\n";
		print_indent(os, indent);
		os << "name: externs\n";
		print_indent(os, indent);
		os << "externs: \n";
		for (auto exter : externs) {
			exter->yaml_output(os, indent + 4);
		}
	}
}

void NFuncList::yaml_output(ostream& os, int indent) {
	if (funcs.size() > 0) {
		os << "funcs:\n";
		print_indent(os, indent + 2);
		os << "name: funcs\n";
		print_indent(os, indent + 2);
		os << "funcs\n";
		for (auto func : funcs) {
			func->yaml_output(os, indent + 4);
		}
	}
}

void NProgram::yaml_output(ostream& os, int indent) {
	/// NProgram is the top-level
	os << "--- " << endl;
	print_indent(os, indent);
	os << "name: prog\n";
	if (externs != NULL) {
		externs->yaml_output(os, indent);
	}
	if (funcs != NULL) {
		funcs->yaml_output(os, indent);  // It should not be empty actually.
	}
	os << "...";
}