%{
#include <stdio.h>
#include "JBison3.tab.h"
extern int numbestr;
%}

%%
\/\/.*\n						++numbestr;
\/\*.*\*\/						;

"import"						return IMPORT;
"package"						return PACKAGE;

"class"						return CLASS;
"interface"						return INTERFACE;

"byte"|"int"|"long"|"short"|"char" 			return INTEGRAL_TYPE;
"String"						return STRING_TYPE;
"double"|"float"					return FLOATINGPOINT_TYPE;
"boolean"						return BOOLEAN_TYPE;
"var"							return VAR;
"void"							return VOID;
"enum"							return ENUM;

"public"						return PUBLIC;
"private"|"protected"					return ACCESS;
"static"						return STATIC;
"final"						return FINAL;
"abstract"						return ABSTRACT;
"transient"						return TRANSIENT;
"native"						return NATIVE;
"strictfp"						return STRICTFP;
"synchronized"						return SYNCHRONIZED;
"volatile"						return VOLATILE;

"extends"						return EXTENDS;
"implements"						return IMPLEMENTS;
"throw"						return THROW;

"if"							return IF;
"else"							return ELSE;

"switch"						return SWITCH;
"case"							return CASE;
"default"						return DEFAULT;

"try"							return TRY;
"catch"						return CATCH;
"throws"						return THROWS;
"finally"						return FINALLY;

"for"							return FOR;
"do"							return DO;
"while"						return WHILE;

"break"						return BREAK;
"continue"						return CONTINUE;

"new"							return NEW;
"super"						return SUPER;
"this"							return THIS;

"assert"						return ASSERT;
"instanceof"						return INSTANCEOF;
"return"						return RETURN;

"@"							return AT;
"null"							return NULL_LITERAL;

"&&"							return DOUBLE_AND;
"||"       						return DOUBLE_OR;
"&"							return AND;
"|"							return OR;

"->"									return LAMBDA_ASSIGNMENT;
"+="|"-="|"*="|"/="|"%="|"&="|"|="|">>="|"<<="|">>>="|"^="		return ASSIGNMENT_ACTION;

"++"                                           			return INCREMENT;
"--"									return DECREMENT;

"~"|"!"                                             			return UNARY_SIGN_ONLY;
"-"                                                 			return MINUS;
"+"                                                 			return PLUS;

"=="|"!="								return COMPARISON_SIGN;
">="|"<="                                 				return MORE_LESS_EQUAL;
"<"                                                 			return OPEN_ANGLE_BRACKET;
">"                                                 			return CLOSE_ANGLE_BRACKET;

"%"									return PERCENT;
"/"									return DIVISION;
"<<"|">>"|">>>"							return SHIFT;
"^"									return EXCLUSIVE;


"*"                                                 			return STAR;
"="                                                 			return ASSIGNMENT;

"("									return OPEN_PARENT;
")"									return CLOSE_PARENT;
"["									return OPEN_BRACKET;
"]"									return CLOSE_BRACKET;
"{"									return OPEN_BRACE;
"}"									return CLOSE_BRACE;

";"                                                 	return SEMICOLON;
"."                                                 	return DOT;
","                                                 	return COMMA;
"?"                                                 	return QUERY;
"::"							return DOUBLE_COLON;
":"                                                 	return COLON;

0{1}(b|B)[0-1]+(\_*[0-1]+)*[0-1]*(l|L)?			return INT2;
0{1}[0-7]+(\_[0-7]+)*[0-7]*(l|L)?				return INT8;
0{1}(x|X)[0-9a-fA-F]+(\_[0-9a-fA-F]+)*[0-9a-fA-F]*(l|L)?	return INT16;
[0-9]+(\_*[0-9]+)*[0-9]*(l|L)?				return INT10;

\.([0-9]+(\_*[0-9]+)*[0-9]*)+((e|E)(\-|\+)?[0-9]+(\_*[0-9]+)*[0-9]*)?[fFdD]?					return FLOAT;
([0-9]+(\_*[0-9]+)*[0-9]*)+\.([0-9]+(\_*[0-9]+)*[0-9]*)+((e|E)(\-|\+)?[0-9]+(\_*[0-9]+)*[0-9]*)?[fFdD]?	return FLOAT;
([0-9]+(\_*[0-9]+)*[0-9]*)((e|E)(\-|\+)?[0-9]+(\_*[0-9]+)*[0-9]*)[fFdD]?					return FLOAT;
([0-9]+(\_*[0-9]+)*[0-9]*)((e|E)(\-|\+)?[0-9]+(\_*[0-9]+)*[0-9]*)?[fFdD]					return FLOAT;
0{1}(x|X)[0-9a-fA-F]+(\_[0-9a-fA-F]+)*[0-9a-fA-F]*\.?[pP][fFdD]?						return FLOAT_HEX;
0{1}(x|X)[0-9a-fA-F]+(\_[0-9a-fA-F]+)*[0-9a-fA-F]*\.([0-9a-fA-F]+(\_[0-9a-fA-F]+)*)+[pP][fFdD]?		return FLOAT_HEX;

\'([^'\\])?\'						return CHAR;
\\[btnfr"'\\]						return CHAR;
\"([^"]|\\.)*\"                                     	return STRING;
"true"|"false"                                      	return BOOL;
([A-Za-z][A-Za-z0-9_]*)(\.(([A-Za-z][A-Za-z0-9_]*)))+	return IDENT_DOT_IDENT;
[A-Za-z][A-Za-z0-9_]*                             	return IDENTIFIER;
\n							++numbestr;
[ \t]+							;

%%

int main(int argc, char const *argv[]){
    yylval=1;
    return yyparse();
}
