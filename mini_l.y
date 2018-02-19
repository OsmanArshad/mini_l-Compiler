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
		|	ARRAY L_SQUARE_BRACKET NUMBERS R_SQUARE_BRACKET OF INTEGER	{cout << "assign-> ARRAY L_SQUARE_BRACKET" << $3 << " R_SQUARE_BRACKET OF INTEGER" << endl;}
		;
statements: 	statement SEMICOLON	{cout << "statements -> statement" << endl;}
		|		statement SEMICOLON statements	{cout << "statement -> statments SEMICOLON statements" <<;}
		;

statement:	variable	{cout << "statement -> variable" << endl;}
		|	if			{cout << "statement -> if" << endl;}
		|	while		{cout << "statement -> while" << endl;}
		|	do			{cout << "statement -> do" << endl;}
		|	foreach		{cout << "statement -> foreach" << endl;}
		|	read		{cout << "statement -> read" << endl;}
		|	write		{cout << "statement -> write" << endl;}
		|	continue	{cout << "statement -> continue" << endl;}
		|	return		{cout << "statement -> return" << endl;}
		;
	
variable:	var ASSIGN expression	{cout << "variable -> var ASSIGN expression" << endl;} 
			;

if:			IF boolExpr THEN statements ENDIF	{cout << "if -> IF boolExpr THEN statements ENDIF" << endl;}
		|	IF boolExpr THEN statements ELSE statements ENDIF	{cout << "IF boolExpr THEN statements ELSE statements ENDIF" << endl;}
			;

while:		WHILE boolExpr BEGINLOOP statements ENDLOOP	{cout << "while -> WHILE boolExpr BEGINLOOP statements ENDLOOP" << endl;}
			;

do:			DO BEGINLOOP statements ENDLOOP WHILE boolExpr	{cout << "do -> DO BEGINLOOP statements ENDLOOP WHILE boolExpr" << endl;}
			;

foreach:	FOREACH IDENTIFIERS IN IDENTIFIERS BEGINLOOP statements ENDLOOP	{cout << "foreach -> FOREACH IDENTIFIERS IN IDENTIFIERS BEGINLOOP statements ENDLOOP" << endl;}
			;

read:		READ var commas	{cout << "read -> READ var commas" << endl;}
			;

commas:		/*Epsilon*/			{cout << "commas -> epsilon" << endl;}
		|	COMMA var commas	{cout <<" commas -> COMMA var commas" << endl;}
			;

write:		WRITE var commas	{cout << "write -> WRITE var commas" << endl;}
			;

continue:	CONTINUE	{cout << "continue -> CONTINUE" << endl;}
			;

return:		RETURN expression	{cout << "return -> RETURN expression" << endl;}
			;

boolExpr:	relationAndExpr	{cout << "boolExpr -> relationAndExpr" << endl;}
		|	relationAndExpr OR relationAndExpr	{cout << "boolExpr -> relationAndExpr OR relationAndExpr" << endl;}
		;
		
relationAndExpr:	relationExpr	{cout << "relationAndExpr -> relationExpr" << endl;}
				|	relationExp AND relationExp	{cout << "relationAndExpr -> relationExpr AND relationExpr" << endl;}
		;

relationExpr:	relationStatement	{cout << "relationExp -> relationStatement" << endl;}
		|	NOT relationStatement	{cout << "relationExp -> NOT relationStatement" << endl;}
		;

relationStatement:	expression comp expression	{cout << "relationStatement -> expression comp expression" << endl;}
		|			TRUE	{cout << "relationStatement -> TRUE" << endl;}
		|			FALSE	{cout << "relationStatement -> FALSE" << endl;}
		|			L_PAREN boolExpr R_PAREN	{cout << "relationStatement -> L_PAREN boolExpr R_PAREN" << endl;}
		;

comp:	EQ	{cout << "comp -> EQ" << endl;}
	|	NEQ	{cout << "comp -> NEQ" << endl;}
	|	LT	{cout << "comp -> LT" << endl;}
	|	GT	{cout << "comp -> GT" << endl;}
	|	LTE	{cout << "comp -> LTE" << endl;}
	|	GTE	{cout << "comp -> GTE" << endl;}
	;

expression:	multiplicativeExpr addSubExpr	{cout << "expression -> multiplicativeExpr addSubExpr" << endl;}
	;

addSubExpr:	/*Epsilon*/	{cout << "addSubExpr -> Epsilon" << endl;}
			|	ADD multiplicativeExpr operationExpr {cout << "addSubExpr -> ADD multiplicativeExpr operationExpr" << endl;}
			|	SUB multiplicativeExpr operationExpr {cout << "addSubExpr -> SUB multiplicativeExpr operationExpr" << endl;}
	;

multiplicativeExpr:	term multiplicativeTerm {cout << "multiplicativeExpr -> term multiplicativeTerm" << endl;}
	;

multiplicativeTerm: /*Epsilon*/	{cout << "multiplicativeTerm -> epsilon" << endl;}
				|	MULT term multiplicativeTerm {cout << "multiplicativeTerm -> MULT term multiplicativeTerm" << endl;}
				|	DIV term multiplicativeTerm {cout << "multiplicativeTerm -> DIV term multiplicativeTerm" << endl;}
				|	MOD term multiplicativeTerm {cout << "multiplicativeTerm -> MOD term multiplicativeTerm" << endl;}
	;

term:	positiveTerm 	{cout << "term -> positiveTerm" << endl;}
	|	SUB positiveTerm	{cout << "term -> SUB positiveTerm" << endl;}
	|	IDENTIFIERS termType {cout<< "term -> IDENT " << *($1) << " termType " <<endl;}
	;

positiveTerm:	var 	{cout << "positiveTerm -> var" << endl;}
			|	NUMBERS	{cout << "positiveTerm -> NUMBER "<< $1 << endl;}
			|	L_PAREN expression R_PAREN {cout << "positiveTerm -> L_PAREN expression R_PAREN" << endl;}
			;

termType:	L_PAREN termExpr R_PAREN {cout << "termType -> L_PAREN termExpr R_PAREN" << endl;}
		|	L_PAREN R_PAREN	{cout << "termType -> L_PAREN R_PAREN" << endl;}
		;

termExpr:	expression {cout << "termExpr -> expression" << endl;}
		|	expression COMMA termExpr {cout << "termExpr -> expression COMMA termExpr" << endl;}
		;
var:	IDENTIFIERS	{cout << "var -> IDENT " << *($1) << endl;}
	|	IDENTIFIERS L_SQUARE_BRACKET expression R_SQUARE_BRACKET {cout << "var -> " << *($1) << " L_SQUARE_BRACKET expression R_SQUARE_BRACKET" << endl;}
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


