%{
  #include "node.h"
  #include <cstdio>
  #include <cstdlib>
  #include <unordered_map>
  NProgram* programBlock; /* the top level root node of our final AST */

  // stuff from flex that bison needs to know about:
  extern "C" int yylex();
  extern "C" int yyparse();
  extern "C" void yyerror(const char* s);
  extern "C" FILE *yyin;
  extern int yylineno;
  extern int yycolumn;
  
  unordered_map<string, string> globid_type;  //record the return type of each function
  unordered_map<string, string> var_type;  //record the type of each variable
  
  void yyerror(const char *s) {
    fprintf(stderr, "Error | Line: %d, Column: %d: %s\n", yylineno, yycolumn, s);
    exit(1);
  }
  string type_inference(NExpression *node);
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
      var_type.clear();
      globid_type[$3] = $2;
      $$ = new NFunctionDeclaration(*$2, $3, $5, $7);
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
      type var { var_type[$2] = $1; $$ = new NVariableDeclaration(*$1, $2); }
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
   | globid LP opt_exps RP { $$ = new NFuncCall($1, $3, globid_type[node->funcname->name]); }
   ;


binop:
     exp TIMES exp { $$ = new NBinaryOperator($1, OP_TIMES, $3, type_inference($1)); }
     | exp SLASH exp { $$ = new NBinaryOperator($1, OP_DIV, $3, type_inference($1)); }
     | exp PLUS exp { $$ = new NBinaryOperator($1, OP_PLUS, $3, type_inference($1)); }
     | exp MINUS exp { $$ = new NBinaryOperator($1, OP_MINUS, $3, type_inference($1)); }
     | exp EQL exp { $$ = new NBinaryOperator($1, OP_EQL, $3, type_inference($1)); }
     | exp LSS exp { $$ = new NBinaryOperator($1, OP_LSS, $3, type_inference($1)); }
     | exp GTR exp { $$ = new NBinaryOperator($1, OP_GTR, $3, type_inference($1)); }
     | exp AND exp { $$ = new NBinaryOperator($1, OP_AND, $3, type_inference($1)); }
     | exp OR exp { $$ = new NBinaryOperator($1, OP_OR, $3, type_inference($1)); }
     | exp ASSIGN exp { $$ = new NBinaryOperator($1, OP_ASSIGN, $3, type_inference($1)); }
     ;

uop:
   NOT exp { $$ = new NUnaryOperator(OP_NOT, $2, type_inference($2)); }
   | MINUS exp %prec NEG { $$ = new NUnaryOperator(OP_MINUS, $2, type_inference($2)); }
   ;

lit:
    INTEGER { $$ = new NInteger(*$1, "int"); }
    | DOUBLE { $$ = new NDouble(*$1, "float"); }
    ;

slit:
    STRING { $$ = $1; }
    ;

var:
    DOLLOR ident { string s_temp = var_type[$1];
      auto found = s_temp.find("ref ");
      if (found != string::npos)
        s_temp = s_temp.substr(found+4);
      $$ = new NVariable(*$2, s_temp); }
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

string type_inference(NExpression *node) {
    switch(node->mark) {
        case 1: {  //variable expression
            string s_temp = var_type[node->name->name];
            auto found = s_temp.find("ref ");
            if (found != string::npos)
                s_temp = s_temp.substr(found+4);
            return s_temp;
        }
        case 2: {  //int expression
            return "int";
        }
        case 3: {  //float expression
            return "float";
        }
        case 4: {  //call expression
            return globid_type[node->funcname->name];
        }
        case 5: {  //unary operation expression
            return type_inference(node->rhs);
        }
        case 6: {  //binary operation expression
            return type_inference(node->lhs);
        }
        default: {
            return NULL;
        }
    }
}
