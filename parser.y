%{
  #include "node.h"
  #include <cstdio>
  #include <cstdlib>
  NProgram* programBlock; /* the top level root node of our final AST */

  // stuff from flex that bison needs to know about:
  extern "C" int yylex();
  extern "C" int yyparse();
  extern "C" void yyerror(const char* s);
  extern "C" FILE *yyin;
  extern int yylineno;
  extern int yycolumn;
  void yyerror(const char *s) {
    fprintf(stderr, "Error | Line: %d, Column: %d: %s\n", yylineno, yycolumn, s);
    exit(1);
  }
%}

%locations

/* Represents the many different ways we can access our data */
%union {
  NBlock* block;
  NExpression *expr;
  NProgram* program_type;
  NExternList* externlist_type;
  NFuncList* funclist_type;
  NExpressionList* explist_type;
  NExternDeclaration* extern_type;
  NFunctionDeclaration* func_type;
  NIdentifier* ident_type;
  NVariable* var_type;
  std::vector<string*>* stringlist_type;
  std::vector<NVariableDeclaration*>* vardecls_type;
  NVariableDeclaration* vdecl_type;

  std::string* str;  // Save all literals as string
  int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */

%token <token> TIMES SLASH PLUS MINUS
%token <token> ASSIGN
%token <token> EQL LSS GTR
%token <token> AND OR
%token <token> NOT

%token <token> LP RP SEMICOLON LB RB COMMA DOLLOR
%token <token> EXTERNSYM DEFSYM RETURNSYM WHILESYM IFSYM PRINTSYM ELSESYM
%token <token> INTTYPE CINTTYPE FLOATTYPE SFLOATTYPE VOIDTYPE REFTYPE NOALIASTYPE

%right ASSIGN
%left OR
%left AND
%left EQL
%left LSS GTR
%left PLUS MINUS
%left TIMES SLASH
%right NEG NOT

%token <str> IDENTIFIER INTEGER DOUBLE STRING

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */

%type <program_type> prog
%type <externlist_type> opt_externs externs
%type <funclist_type> funcs
%type <extern_type> exter;
%type <func_type> func;
%type <expr> exp lit binop uop
%type <explist_type> opt_exps exps
%type <stringlist_type> opt_tdecls tdecls
%type <vardecls_type> opt_vdecls vdecls
%type <vdecl_type> vdecl
%type <ident_type> ident globid
%type <var_type> var
%type <str> type slit

%type <block> blk opt_stmts stmts stmt opt_else

%%

prog:
    opt_externs funcs { $$ = new NProgram($1, $2); programBlock = $$; }
    ;

opt_externs:
       externs { $$ = $1; }
       | { $$ = new NExternList(); }
       ;

externs:
        externs exter { $1->externs.push_back($2); $$ = $1; }
        | exter { $$ = new NExternList(); $$->externs.push_back($1); }
        ;

exter:
      EXTERNSYM type globid LP opt_tdecls RP SEMICOLON {
        $$ = new NExternDeclaration(*$2, $3, $5);
      }
      ;

funcs:
     funcs func { $1->funcs.push_back($2); $$ = $1; }
     | func { $$ = new NFuncList(); $$->funcs.push_back($1); }

func:
    DEFSYM type globid LP opt_vdecls RP blk {
      $$ = new NFunctionDeclaration(*$2, $3, $5, $7);
      // for (auto a : used_function_names) { if (defined_function_names.find(a) == defined_function_names.end())
      //   cout << "error: All functions must be declared and/or defined before they are used." << endl; }
      // used_function_names.clear();
      // var_type.clear();
      // globid_type[$3] = $2;
    }
    ;

opt_vdecls:
         vdecls { $$ = $1; }
         | { $$ = new std::vector<NVariableDeclaration*>(); }
         ;

vdecls:
      vdecls COMMA vdecl { $1->push_back($3); $$ = $1; }
      | vdecl { $$ = new std::vector<NVariableDeclaration*>(); $$->push_back($1); }
      ;

vdecl:
      type var { $$ = new NVariableDeclaration(*$1, $2); }
      ;

opt_tdecls:
          tdecls { $$ = $1; }
          | { $$ = NULL; }
          ;

tdecls:
      tdecls COMMA type { $1->push_back($3); $$ = $1; }
      | type { $$ = new std::vector<string*>(); $$->push_back($1); }
      ;

blk:
   LB opt_stmts RB { $$ = $2; }
   ;
opt_stmts:
          stmts { $$ = $1; }
          | { $$ = new NBlock(); }
         ;
stmts:
     stmts stmt { $1->statements.push_back($2); $$ = $1; }
     | stmt { $$ = new NBlock(); $$->statements.push_back($1); }
     ;

stmt:
    blk { $$ = $1; }
    | RETURNSYM SEMICOLON { $$ = new NReturnStatement(); }
    | RETURNSYM exp SEMICOLON { $$ = new NReturnStatement($2); }
    | vdecl ASSIGN exp SEMICOLON { $$ = new NAssignStatement($1, $3); }
    | exp SEMICOLON { $$ = new NExpressionStatement($1); }
    | WHILESYM LP exp RP stmt { $$ = new NWhileStatement($3, $5); }
    | IFSYM LP exp RP stmt opt_else { $$ = new NIfStatement($3, $5, $6); }
    | PRINTSYM exp SEMICOLON { $$ = new NPrintExpressionStatement($2); }
    | PRINTSYM slit SEMICOLON { $$ = new NPrintSlitStatement(*$2); }
    ;

opt_else:
      ELSESYM stmt { $$ = $2; }
      | { $$ = new NBlock(); }
      ;

opt_exps:
        exps { $$ = $1; }
        | { $$ = NULL; }
        ;

exps:
    exps COMMA exp { $1->exps.push_back($3); $$ = $1; }
    | exp { $$ = new NExpressionList(); $$->exps.push_back($1);}
    ;

exp:
   LP exp RP { $$ = $2; }
   | binop { $$ = $1; }
   | uop { $$ = $1; }
   | lit { $$ = $1; }
   | var { $$ = $1; }
   | globid LP opt_exps RP { $$ = $3; }
   ;


binop:
     exp TIMES exp { $$ = new NBinaryOperator($1, OP_TIMES, $3); }
     | exp SLASH exp { $$ = new NBinaryOperator($1, OP_DIV, $3); }
     | exp PLUS exp { $$ = new NBinaryOperator($1, OP_PLUS, $3); }
     | exp MINUS exp { $$ = new NBinaryOperator($1, OP_MINUS, $3); }
     | exp EQL exp { $$ = new NBinaryOperator($1, OP_EQL, $3); }
     | exp LSS exp { $$ = new NBinaryOperator($1, OP_LSS, $3); }
     | exp GTR exp { $$ = new NBinaryOperator($1, OP_GTR, $3); }
     | exp AND exp { $$ = new NBinaryOperator($1, OP_AND, $3); }
     | exp OR exp { $$ = new NBinaryOperator($1, OP_OR, $3); }
     | exp ASSIGN exp { $$ = new NBinaryOperator($1, OP_ASSIGN, $3); }
     ;

uop:
   NOT exp { $$ = new NUnaryOperator(OP_NOT, $2); }
   | MINUS exp %prec NEG { $$ = new NUnaryOperator(OP_MINUS, $2); }
   ;

lit:
    INTEGER { $$ = new NInteger(*$1); }
    | DOUBLE { $$ = new NDouble(*$1); }
    ;

slit:
    STRING { $$ = $1; }
    ;

var:
   DOLLOR ident { $$ = new NVariable(*$2); }
   ;

ident:
    IDENTIFIER { $$ = new NIdentifier(*$1); }
    ;

globid:
      ident { $$ = $1; }
      ;

type:
    INTTYPE { $$ = new string("int"); }
    | CINTTYPE { $$ = new string("cint"); }
    | FLOATTYPE { $$ = new string("float"); }
    | SFLOATTYPE { $$ = new string("sfloat"); }
    | VOIDTYPE { $$ = new string("void"); }
    | REFTYPE type { $$ = new string(string("ref").append(*$2)); }
    | NOALIASTYPE REFTYPE type { $$ = new string(string("noalias ref ").append(*$3)); }
    ;

%%