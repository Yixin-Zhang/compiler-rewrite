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

%token <string> IDENTIFIER INTEGER DOUBLE STRING

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */

%type <string> globid var ident slit type
%type <node> func blk stmt opt_else exp lit vdecl opt_exters opt_vdecls opt_stmts opt_exps opt_tdecls
%type <block> funcs vdecls stmts exps
%type <block> tdecls
%type <stmt> prog opt_externs externs exter
%type <expr> binop uop

%%


prog:
    opt_externs funcs { $$ = new NProgram(); $$.externs = $1; $$.funcs = $2; programBlock = $$; }
    ;

opt_externs:
       externs { $$ = $1; }
       | { $$ = new NExterns(); }
       ;
externs:
        externs exter { $1.externs.push_back($2); $$ = $1; }
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
    | WHILESYM LP exp  RP stmt { $$ = new NWhileStatement($3, $5); }
    | IFSYM LP exp RP stmt opt_else { $$ = new NIfStatement($3, $5, $6); }
    | PRINTSYM exp SEMICOLON { $$ = new NPrintExpressionStatement($2); }
    | PRINTSYM slit SEMICOLON { $$ = NPrintSlitStatement($2); }
    ;
opt_else:
      ELSESYM stmt { $$ = $2; }
      | { $$ = new NStatement(); }
      ;
exps:
    exp { $$ = new NExpression }
    |  exps COMMA exp {  }
    ;

exp:
   LP exp RP { $$ = $2; }
   | binop { $$ = $1; }
   | uop { $$ = $1; }
   | lit { $$ = $1; }
   | var { $$ = $1; }
   | globid LP opt_exps RP { $$ = $3; }  // this is probably wrong, fix it.
   ;

opt_exps:
        exps { $$ = $1; }  // Probably wrong.
        | { $$ = new NExpression(); }
        ;

binop:
     exp TIMES exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp SLASH exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp PLUS exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp MINUS exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp EQL exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp LSS exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp GTR exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp AND exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp OR exp { $$ = new NBinaryOperator($1, $2, $3); }
     |exp ASSIGN exp { $$ = new NBinaryOperator($1, $2, $3); }
     ;
uop:
   NOT exp { $$ = new NUnaryOperator($1, $2); }
   | MINUS exp %prec NEG { $$ = new NUnaryOperator($1, $2); }
   ;

lit:
    INTEGER { $$ = new std::string($1);}
    | DOUBLE { $$ = new std::string($1);}
    ;
slit:
    STRING { $$ = new std::string($1); }
    ;
ident:
    IDENTIFIER { $$ = new std::string($1); }
    ;

var:
   DOLLOR ident { $$ = new string($2); }
   ;
globid:
      ident { $$ = new string($1); }
      ;
type:
    INTTYPE { $$ = new string("int"); }
    | CINTTYPE { $$ = new string("cint"); }
    | FLOATTYPE { $$ = new string("float"); }
    | SFLOATTYPE { $$ = new string("sfloat"); }
    | VOIDTYPE { $$ = new string("void"); }
    | REFTYPE type { $$ = new string(strcat(strdup("ref "), $2));
      if (strcmp($2, "void") == 0 || strstr($2, "ref")) cout << "error: In <200b>ref <200b><type><200b>, the type may not be void or itself a reference type." << endl;}
    | NOALIASTYPE REFTYPE type { $$ = new string(strcat(strdup("noalias ref "), $3)); }
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

int main(int argc, char **argv) {
  yyparse();
  std::cout << programBlock << endl;
  return 0;
}
