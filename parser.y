%{
  #include "node.h"
  #include <cstdio>
  #include <cstdlib>
  NBlock *programBlock; /* the top level root node of our final AST */

  extern int yylex();
  void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
  Node *node;
  NBlock *block;
  NExpression *expr;
  NStatement *stmt;
  NIdentifier *ident;
  NVariableDeclaration *var_decl;
  std::vector<NVariableDeclaration*> *varvec;
  std::vector<NExpression*> *exprvec;
  std::string *string;  // Save all literals as string
  int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */

%token TIMES SLASH PLUS MINUS
%token ASSIGN
%token EQL LSS GTR
%token AND OR
%token NOT

%token LP RP SEMICOLON LB RB COMMA DOLLOR
%token EXTERNSYM DEFSYM RETURNSYM WHILESYM IFSYM PRINTSYM ELSESYM
%token INTTYPE CINTTYPE FLOATTYPE SFLOATTYPE VOIDTYPE REFTYPE NOALIASTYPE

%right ASSIGN
%left OR
%left AND
%left EQL
%left LSS GTR
%left PLUS MINUS
%left TIMES SLASH
%right NEG NOT

%token <string> TIDENTIFIER TINTEGER TDOUBLE STRING

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr 
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl extern_decl
%type <token> comparison

/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV

%start program

%%

prog:
    opt_externs funcs { $$ = new NProgram(); $$.externs = $1; $$.funcs = $2; programBlock = $$; }
    ;

opt_externs:
       externs { $$ = $1; }
       | { $$ = new NExterns(); }
       ;
externs:
        exters exter { $1.externs.push_back($2); $$ = $1; }
        | exter { $$ = new NExterns(); $$.externs.push_back($1); }
        ;
exter:
      EXTERNSYM type globid LP opt_tdecls RP SEMICOLON { $$ = new NExternDeclaration($2, $3, $5); }
      ;

opt_tdecls:
          tdecls { $$ = kal_ast_tdecls_create($1.count, $1.args); }
          | { $$ = kal_ast_tdecls_create(0, NULL); }
          ;

funcs:
     funcs func { $1.funcs.push_back($2); $$ = $1; }
     | func { $$ = new NFuncs(); $$.funcs.push_back($1); }

func:
    DEFSYM type globid LP opt_vdecls RP blk { $$ = new NFunctionDeclaration();
      // for (auto a : used_function_names) { if (defined_function_names.find(a) == defined_function_names.end())
      //   cout << "error: All functions must be declared and/or defined before they are used." << endl; }
      // used_function_names.clear();
      // var_type.clear();
      // globid_type[$3] = $2;
    }
    ;
opt_vdecls:
         vdecls { $$ = kal_ast_vdecls_create($1.count, $1.args); }
         | { $$ = kal_ast_vdecls_create(0, NULL); }
         ;
blk:
   LB opt_stmts RB { $$ = $2; }
   ;
opt_stmts:
         stmts { $$ = $1; }
       | { $$ = new NBlock(); }
         ;
stmts:
     stmts stmt { $1.statements.push_back($2); $$ = $1; }
     | stmt { $$ = new NBlock(); $$.statements.push_back($1); }
     ;
stmt:
    blk { $$ = $1; }
    | RETURNSYM SEMICOLON { $$ = new NReturnStatement(); }
    | RETURNSYM exp SEMICOLON { $$ = new NReturnStatement($2); }
    | vdecl ASSIGN exp SEMICOLON { $$ = new NAssignment($1, $3); }
    | exp SEMICOLON { $$ = new NExpressionStatement($1); }
    | WHILESYM LP exp  RP stmt { $$ = kal_ast_while_create($3, $5); }
    | IFSYM LP exp RP stmt opt_else { $$ = kal_ast_if_expr_create($3, $5, $6); }
    | PRINTSYM exp SEMICOLON { $$ = kal_ast_print_create($2); }
    | PRINTSYM slit SEMICOLON { $$ = kal_ast_print_slit_create($2); }
    ;
opt_else:
      ELSESYM stmt { $$ = $2; }
      | { $$ = NULL; }
      ;
exps:
    exp { $$.count = 1; $$.args = (kal_ast_node**)malloc(sizeof(kal_ast_node*)); $$.args[0] = $1; }
    |  exps COMMA exp { ++$1.count; $1.args = (kal_ast_node**)realloc($1.args, sizeof(kal_ast_node*) * $1.count); $1.args[$1.count-1] = $3; $$ = $1; }
    ;
exp:
   LP exp RP { $$ = $2; }
   | binop { $$ = $1; }
   | uop { $$ = $1; }
   | lit { $$ = $1; }
   | var { string s_temp = var_type[$1];
           auto found = s_temp.find("ref ");
           if (found != string::npos)
             expr_type = (char *)s_temp.substr(found+4).c_str();
           else expr_type = (char *)s_temp.c_str();
           $$ = kal_ast_variable_create($1, expr_type); }
   | globid LP opt_exps RP { expr_type = (char *)globid_type[$1].c_str(); $$ = kal_ast_call_create($1, $3, expr_type); used_function_names.insert($1); }
   ;
opt_exps:
        exps { $$ = kal_ast_exprs_create($1.count, $1.args); }
        | { $$ = kal_ast_exprs_create(0, NULL); }
        ;

binop:
     exp TIMES exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_MUL, $1, $3, type_inference($1)); }
     |exp SLASH exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_DIV, $1, $3, type_inference($1)); }
     |exp PLUS exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_PLUS, $1, $3, type_inference($1)); }
     |exp MINUS exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_MINUS, $1, $3, type_inference($1)); }
     |exp EQL exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_EQL, $1, $3, type_inference($1)); }
     |exp LSS exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_LSS, $1, $3, type_inference($1)); }
     |exp GTR exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_GTR, $1, $3, type_inference($1)); }
     |exp AND exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_AND, $1, $3, type_inference($1)); }
     |exp OR exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_OR, $1, $3, type_inference($1)); }
     |exp ASSIGN exp { $$ = kal_ast_binary_expr_create(KAL_BINOP_ASSIGN, $1, $3, type_inference($1)); }
     ;
uop:
   NOT exp { $$ = kal_ast_uop_expr_create(KAL_UOP_NOT, $2, type_inference($2)); }
   | MINUS exp %prec NEG { $$ = kal_ast_uop_expr_create(KAL_UOP_MINUS, $2, type_inference($2)); }
   ;
lit:
    LITERAL { if (abs($1 - int($1)) < 0.000000000001)
                expr_type = strdup("int");
              else expr_type = strdup("float");
              $$ = kal_ast_number_create($1, expr_type); }
    ;
slit:
    STRING { $$ = strdup($1); }
    ;
ident:
    IDENTIFER { $$ = strdup($1); }
    ;

var:
   DOLLOR ident { $$ = strdup($2); }
   ;
globid:
      ident { $$ = strdup($1); }
      ;
type:
    INTTYPE { $$ = strdup("int"); }
    | CINTTYPE { $$ = strdup("cint"); }
    | FLOATTYPE { $$ = strdup("float"); }
    | SFLOATTYPE { $$ = strdup("sfloat"); }
    | VOIDTYPE { $$ = strdup("void"); }
    | REFTYPE type { $$ = strdup(strcat(strdup("ref "), $2));
      if (strcmp($2, "void") == 0 || strstr($2, "ref")) cout << "error: In <200b>ref <200b><type><200b>, the type may not be void or itself a reference type." << endl;}
    | NOALIASTYPE REFTYPE type { $$ = strdup(strcat(strdup("noalias ref "), $3)); }
    ;
vdecls:
      vdecl { $$.count = 1; $$.args = (kal_ast_node**)malloc(sizeof(kal_ast_node*)); $$.args[0] = $1; }
      | vdecls COMMA vdecl { ++$1.count; $1.args = (kal_ast_node**)realloc($1.args, sizeof(kal_ast_node*) * $1.count); $1.args[$1.count-1] = $3; $$ = $1; }
      ;
tdecls:
      type { $$.count = 1; $$.args = (char**)malloc(sizeof(char*)); $$.args[0] = $1; }
      | tdecls COMMA type { ++$1.count; $1.args = (char**)realloc($1.args, sizeof(char*) * $1.count); $1.args[$1.count-1] = strdup($3); $$ = $1; }
      ;
vdecl:
      type var { $$ = kal_ast_vdecl_create($1, $2); if (strstr($1, "ref")) ++ref_varibles[$2];
                var_type[$2] = $1;}
      ;
%%
