%{
#include <string>

#include "node.h"
#include "parser.hpp"

#define YY_DECL extern "C" int yylex()

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)
%}

%%

"*"           {return TOKEN(TIMES);}
"/"           {return TOKEN(SLASH);}
"+"           {return TOKEN(PLUS);}
"-"           {return TOKEN(MINUS);}
"=="          {return TOKEN(EQL);}
"="           {return TOKEN(ASSIGN);}
"<"           {return TOKEN(LSS);}
">"           {return TOKEN(GTR);}
"&&"          {return TOKEN(AND);}
"||"          {return TOKEN(OR);}
"!"           {return TOKEN(NOT);}

"("           {return TOKEN(LP);}
")"           {return TOKEN(RP);}
");"           {return TOKEN(SEMICOLON);}
"{"           {return TOKEN(LB);}
"}"           {return TOKEN(RB);}
","           {return TOKEN(COMMA);}
"$"           {return TOKEN(DOLLOR);}

"extern"      {return TOKEN(EXTERNSYM);}
"def"         {return TOKEN(DEFSYM);}
"return"      {return TOKEN(RETURNSYM);}
"while"       {return TOKEN(WHILESYM);}
"if"          {return TOKEN(IFSYM);}
"print"       {return TOKEN(PRINTSYM);}
"else"        {return TOKEN(ELSESYM);}
"int"         {return TOKEN(INTTYPE);}
"cint"        {return TOKEN(CINTTYPE);}
"float"       {return TOKEN(FLOATTYPE);}
"sfloat"      {return TOKEN(SFLOATTYPE);}
"void"        {return TOKEN(VOIDTYPE);}
"ref"         {return TOKEN(REFTYPE);}
"noalias"     {return TOKEN(NOALIASTYPE);}


#[^\n]*\n  		 ;  // Comments
[ \t\n]          ;  // Spaces
[0-9]+\.[0-9]* 			    {SAVE_TOKEN; return DOUBLE;}
[0-9]+					        {SAVE_TOKEN; return INTEGER;}
[a-zA-Z_][a-zA-Z0-9_]*  {SAVE_TOKEN; return IDENTIFIER;}
\"[^"]*\"         {SAVE_TOKEN; return STRING;}
.                ;
%%

