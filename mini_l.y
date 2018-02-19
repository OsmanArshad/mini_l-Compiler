%{
#include "heading.h"
int yyerror(char *s);
int yylex(void);
%}

%union{
  int		int_val;
  string*	op_val;
}

%start	programStart 

%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token FOREACH
%token IN
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token READ
%token WRITE
%token AND
%token OR
%token NOT
%token TRUE
%token FALSE
%token RETURN
%token SEMICOLON
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token ASSIGN

%token <int_val> NUMBERS
%token <op_val> IDENTIFIERS

%left MULT
%left DIV
%left MOD
%left ADD
%left SUB

%left LT
%left LTE
%left GT
%left GTE
%left EQ
%left NEQ
%left AND OR

%right NOT
%right ASSIGN




%%

Program:	functions	{cout << "Program -> functions " << endl;}
		;

functions: 	/*Epsilon*/			{cout << "functions -> epsilon" << endl;}
		|	function functions	{cout << "functions -> function functions"<< endl;}
		;

function:	FUNCTION IDENTIFIERS SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_PARAMS BEGIN_BODY statements END_BODY
		;

declarations:	/*Epsilon*/	{cout << "declarations -> epsilon" << endl;}
		|		declaration SEMICOLON declarations	{cout << "declaration -> declaration SEMICOLON declarations" << endl;}
		;

declaration:	id COLON assign	{cout << "id COLON assign" << endl;}
		;

id:		IDENTIFIERS	{cout << "id -> IDENT" << *($1) << endl;} 
	|	IDENTIFIERS COMMA id {cout << "id -> IDENT " << *($1) << " COMMA id " << endl;}
		;

assign:		INTEGER	{cout << "assign -> INTEGER" << endl;}
		|	ARRAY L_SQUARE_BRACKET NUMBERS R_SQUARE_BRACKET OF INTEGER	{cout << "assign-> ARRAY l_SQUARE_BRACKET" << $3 << " R_SQUARE_BRACKET OF INTEGER" << endl;}
		;
statements: 	statement SEMICOLON	{cout << "statements -> statement" << endl;}
		|		statement SEMICOLON statements	{cout << "statement -> statments SEMICOLON statements" <<;}
		;

statement:	var			{cout << "statement -> var" << endl;}
		|	if			{cout << "statement -> if" << endl;}
		|	while		{cout << "statement -> while" << endl;}
		|	do			{cout << "statement -> do" << endl;}
		|	foreach		{cout << "statement -> foreach" << endl;}
		|	read		{cout << "statement -> read" << endl;}
		|	write		{cout << "statement -> write" << endl;}
		|	continue	{cout << "statement -> continue" << endl;}
		|	return		{cout << "statement -> return" << endl;}
		;
	
var:	var ASSIGN expression{cout<<"first->var ASSIGN expression"<<endl;} 
		;

if:		IF boolExpr THEN statements ENDIF	{cout << "if -> IF boolExpr THEN statements ENDIF"<<endl;}
	|	IF boolExpr THEN statements ELSE statements ENDIF	{cout<<"IF boolExpr THEN statements ELSE statements ENDIF"<<endl;}
		;

while:		WHILE boolExpr BEGINLOOP statements ENDLOOP{cout<<"third->WHILE boolExpr BEGINLOOP statements ENDLOOP"<<endl;}
		;

fourth:		DO BEGINLOOP statements ENDLOOP WHILE boolExpr{cout<<"fourth->DO BEGINLOOP statements ENDLOOP WHILE boolExpr"<<endl;}
		;

fifth:		FOREACH IDENTIFIER IN IDENTIFIER BEGINLOOP statements ENDLOOP{cout<<"fifth->FOREACH IDENTIFIER IN IDENTIFIER BEGINLOOP statements ENDLOOP"<<endl;}
		;

sixth:		READ var commas{cout<<"READ var commas"<<endl;}
		;

commas:		{cout<<"commas->epsilon"<<endl;}
		|COMMA var commas{cout<<"commas->COMMA var commas"<<endl;}
		;

seventh:	WRITE var commas{cout<<"seventh->WRITE var commas"<<endl;}
		;

eighth:		CONTINUE{cout<<"eighth->CONTINUE"<<endl;}
		;

ninth:		RETURN expression{cout<<"ninth->RETURN expression"<<endl;}
		;

boolExpr:	relAndExpr{cout<<"boolExpr->relationExpr"<<endl;}
		|relAndExpr OR relAndExpr{cout<<""<<endl;}
		;
		
relAndExpr:	relationExp{cout<<"relAndExpr->relationExpr"<<endl;}
		|relationExp AND relationExp{cout<<"relAndExpr->relationExp AND relationExp"<<endl;}
		;

relationExp:	reExpr{cout<<"relationExp->reExpr"<<endl;}
		|NOT reExpr{cout<<"relationExp-> NOT reExpr"<<endl;}
		;

reExpr:		expression comp expression{cout<<"reExpr->expression comp expression"<<endl;}
		|TRUE{cout<<"reExpr-> TRUE"<<endl;}
		|FALSE{cout<<"reExpr->FALSE"<<endl;}
		|L_PAREN boolExpr R_PAREN{cout<<"reExpr->L_PAREN boolExpr R_PAREN"<<endl;}
		;

comp:		EQ{cout<<"comp->EQ"<<endl;}
		|NEQ{cout<<"comp->NEQ"<<endl;}
		|LT{cout<<"comp->LT"<<endl;}
		|GT{cout<<"comp->GT"<<endl;}
		|LTE{cout<<"comp->LTE"<<endl;}
		|GTE{cout<<"comp->GTE"<<endl;}
		;




%%

int yyerror(string s)
{
  extern int yylineno;	// defined and maintained in lex.c
  extern char *yytext;	// defined and maintained in lex.c
  
  cerr << "ERROR: " << s << " at symbol \"" << yytext;
  cerr << "\" on line " << yylineno << endl;
  exit(1);
}

int yyerror(char *s)
{
  return yyerror(string(s));
}


