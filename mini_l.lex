/*
 *	Osman Arshad
 *	861058256
 *	Description: Specification file for analyzing MINI-L files
 *##. printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", lineNum, linePos, yytext); exit(0);}
 */

%{
	int lineNum = 1;
	int linePos = 1;
%}

digit [0-9]
alpha [a-zA-Z]
underscore "_"

%%
function	printf("FUNCTION\n"); linePos += yyleng;
beginparams	printf("BEGIN_PARAMS\n"); linePos += yyleng;
endparams	printf("END_PARAMS\n"); linePos += yyleng;
beginlocals	printf("BEGIN_LOCALS\n"); linePos += yyleng;
endlocals	printf("END_LOCALS\n"); linePos += yyleng;
beginbody	printf("BEGIN_BODY\n"); linePos += yyleng;
endbody		printf("END_BODY\n"); linePos += yyleng;
integer		printf("INTEGER\n"); linePos += yyleng;
array		printf("ARRAY\n"); linePos += yyleng;
of			printf("OF\n"); linePos += yyleng;
if			printf("IF\n"); linePos += yyleng;
then		printf("THEN\n"); linePos += yyleng;
endif		printf("ENDIF\n"); linePos += yyleng;
else		printf("ELSE\n"); linePos += yyleng;
while		printf("WHILE\n"); linePos += yyleng;
do			printf("DO\n"); linePos += yyleng;
foreach		printf("FOREACH\n"); linePos += yyleng;
in			printf("IN\n"); linePos += yyleng;
beginloop	printf("BEGINLOOP\n"); linePos += yyleng;
endloop		printf("ENDLOOP\n"); linePos += yyleng;
continue	printf("CONTINUE\n"); linePos += yyleng;
read		printf("READ\n"); linePos += yyleng;
write		printf("WRITE\n"); linePos += yyleng;
and			printf("AND\n"); linePos += yyleng;
or			printf("OR\n"); linePos += yyleng;
not			printf("NOT\n"); linePos += yyleng;
true		printf("TRUE\n"); linePos += yyleng;
false		printf("FALSE\n"); linePos += yyleng;
return		printf("RETURN\n"); linePos += yyleng;

"-"			printf("MINUS\n"); linePos += yyleng;
"+"			printf("ADD\n"); linePos += yyleng;
"*"			printf("MULT\n"); linePos += yyleng;
"/"			printf("DIV\n"); linePos += yyleng;
"%"			printf("MOD\n"); linePos += yyleng;

"=="		printf("EQ\n"); linePos += yyleng;
"<>"		printf("NEQ\n"); linePos += yyleng;
"<"			printf("LT\n"); linePos += yyleng;
">"			printf("GT\n"); linePos += yyleng;
"<="		printf("LTE\n"); linePos += yyleng;
">="		printf("GTE\n"); linePos += yyleng;

({digit}|{underscore})+({alpha}|{underscore})+		printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", lineNum, linePos, yytext); exit(0);
({alpha}+({alpha}|{digit}|{underscore})*{underscore})	printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", lineNum, linePos, yytext); exit(0);
{digit}+												printf("NUMBER %s\n", yytext); linePos += yyleng;
({alpha}+({alpha}|{digit}|{underscore})*)				printf("IDENT %s\n", yytext); linePos += yyleng;

";"			printf("SEMICOLON\n"); linePos += yyleng;
":"			printf("COLON\n"); linePos += yyleng;
","			printf("COMMA\n"); linePos += yyleng;
"("			printf("L_PAREN\n"); linePos += yyleng;
")"			printf("R_PAREN\n"); linePos += yyleng;
"["			printf("L_SQUARE_BRACKET\n"); linePos += yyleng;
"]"			printf("R_SQUARE_BRACKET\n"); linePos += yyleng;
":="		printf("ASSIGN\n"); linePos += yyleng;

[ \t]+  	linePos += yyleng;
[#]+.*		linePos += yyleng;
"\r\n"		lineNum++; linePos = 1; 
.			printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", lineNum, linePos, yytext); exit(0);
%%

main()
{
	yylex();
}
