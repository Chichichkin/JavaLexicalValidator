%{
#include <stdio.h>
#include <string.h>

extern int yylex();

extern int yylval;
int yydebug=1;
int numbestr=1;

void yyerror(const char *str){
    fprintf(stderr,"error: %s\n", str);
    fprintf(stderr,"str: %d\n", numbestr);
}

int yywrap(){
    return 1;
}

%}

%token IMPORT PACKAGE CLASS INTERFACE
%token INTEGRAL_TYPE STRING_TYPE FLOATINGPOINT_TYPE
%token BOOLEAN_TYPE VAR VOID ENUM
%token PUBLIC ACCESS STATIC FINAL ABSTRACT
%token TRANSIENT NATIVE STRICTFP SYNCHRONIZED VOLATILE
%token EXTENDS IMPLEMENTS THROW
%token IF ELSE 
%token SWITCH CASE DEFAULT
%token TRY CATCH THROWS FINALLY
%token FOR DO WHILE
%token BREAK CONTINUE
%token NEW SUPER THIS
%token ASSERT INSTANCEOF RETURN
%token AT NULL_LITERAL
%token DOUBLE_AND DOUBLE_OR AND OR
%token LAMBDA_ASSIGNMENT ASSIGNMENT_ACTION
%token INCREMENT DECREMENT
%token UNARY_SIGN_ONLY MINUS PLUS
%token COMPARISON_SIGN MORE_LESS_EQUAL OPEN_ANGLE_BRACKET CLOSE_ANGLE_BRACKET 
%token PERCENT DIVISION SHIFT EXCLUSIVE
%token STAR ASSIGNMENT
%token OPEN_PARENT CLOSE_PARENT OPEN_BRACKET CLOSE_BRACKET OPEN_BRACE CLOSE_BRACE
%token SEMICOLON DOT COMMA QUERY DOUBLE_COLON COLON
%token INT2 INT8 INT10 INT16
%token FLOAT FLOAT_HEX
%token CHAR STRING NUMBER BOOL IDENT_DOT_IDENT IDENTIFIER

%left MINUS PLUS
%left STAR
%left COMMA
%left VOID
%left PACKAGE
%right PUBLIC ACCESS STATIC FINAL ABSTRACT TRANSIENT NATIVE STRICTFP SYNCHRONIZED VOLATILE
%left IDENTIFIER
%right DOT
%right OPEN_ANGLE_BRACKET



%start CompilationUnit



%%
Literal:	IntegerLiteral
		| FloatingPointLiteral
		| BooleanLiteral
		| CharacterLiteral
		| StringLiteral
		| NullLiteral;	

IntegerLiteral:	DecimalIntegerLiteral
			| HexIntegerLiteral
			| OctalNumeral
			| BinaryNumeral;
			
DecimalIntegerLiteral:		INT10;	
HexIntegerLiteral:		INT16;
OctalNumeral:			INT8;
BinaryNumeral: 		INT2;

FloatingPointLiteral:	DecimalFloatingPointLiteral
			| HexadecimalFloatingPointLiteral;
			
DecimalFloatingPointLiteral:			FLOAT;
HexadecimalFloatingPointLiteral:		FLOAT_HEX;

BooleanLiteral:				BOOL;
CharacterLiteral:				CHAR;

StringLiteral: 				STRING;

NullLiteral: 					NULL_LITERAL;


PrimitiveType:	NumericType
		| STRING_TYPE
		| BOOLEAN_TYPE;

		
NumericType:	INTEGRAL_TYPE
		| FLOATINGPOINT_TYPE;		
			
//ClassType: 	IDENTIFIER
//		| IDENT_DOT_IDENT
//		| IDENTIFIER TypeArguments
//		| IDENT_DOT_IDENT TypeArguments
//		| ClassType DOT IDENTIFIER TypeArguments;
		
ArrayType:	PrimitiveType Dims
		| IDENTIFIER Dims
		| IDENT_DOT_IDENT Dims;	
		
Dims:	OPEN_BRACKET CLOSE_BRACKET
	| Dims OPEN_BRACKET CLOSE_BRACKET;
	
TypeParameter:	IDENTIFIER
		| IDENTIFIER TypeBound;

			
TypeBound:	EXTENDS IDENT_DOT_IDENT
		| EXTENDS EXTENDS IDENT_DOT_IDENT AdditionalBound
		| EXTENDS IDENTIFIER
		| EXTENDS EXTENDS IDENTIFIER AdditionalBound;
		
AdditionalBound:	AND IDENTIFIER
			| AND IDENT_DOT_IDENT
			| AdditionalBound AND IDENTIFIER
			| AdditionalBound AND IDENT_DOT_IDENT;
				
TypeArguments:	OPEN_ANGLE_BRACKET TypeArgumentList CLOSE_ANGLE_BRACKET;

TypeArgumentList:	TypeArgument 
			| TypeArgumentList COMMA TypeArgument;											
		
TypeArgument:	ReferenceType
		| Wildcard;		
		
Wildcard:	QUERY 
		| QUERY WildcardBounds;

WildcardBounds:	EXTENDS ReferenceType
			| SUPER ReferenceType;
			
CompilationUnit:	%empty
			| PackageDeclaration
			| CompilationUnit ImportDeclaration
			| CompilationUnit TypeDeclaration;
			
PackageDeclaration:	PACKAGE IDENTIFIER SEMICOLON
			| PACKAGE IDENT_DOT_IDENT SEMICOLON;


MethodDeclaration:	Modifiers MethodHeader MethodBody
			| MethodHeader MethodBody;

MethodHeader:	Result MethodDeclarator
		| Result MethodDeclarator Throws
		| TypeParameters Result MethodDeclarator
		| TypeParameters Result MethodDeclarator Throws;
		

		
MethodDeclarator:	IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENTIFIER OPEN_PARENT FormalParameterList CLOSE_PARENT
			| IDENTIFIER OPEN_PARENT CLOSE_PARENT Dims
			| IDENTIFIER OPEN_PARENT FormalParameterList CLOSE_PARENT Dims;
			

			
ImportDeclaration:	IMPORT IDENTIFIER SEMICOLON
			| IMPORT IDENTIFIER DOT STAR SEMICOLON
			| IMPORT STATIC IDENTIFIER SEMICOLON
			| IMPORT STATIC IDENTIFIER DOT STAR SEMICOLON
			| IMPORT IDENT_DOT_IDENT SEMICOLON
			| IMPORT IDENT_DOT_IDENT DOT STAR SEMICOLON
			| IMPORT STATIC IDENT_DOT_IDENT SEMICOLON
			| IMPORT STATIC IDENT_DOT_IDENT DOT STAR SEMICOLON;

TypeDeclaration:	ClassDeclaration
			| InterfaceDeclaration
			| SEMICOLON;

ClassDeclaration:	NormalClassDeclaration
			| EnumDeclaration;

NormalClassDeclaration:	Modifiers CLASS IDENTIFIER ClassBody
				| Modifiers CLASS IDENTIFIER Superclass ClassBody
				| Modifiers CLASS IDENTIFIER Superinterfaces ClassBody
				| Modifiers CLASS IDENTIFIER TypeParameters ClassBody
				| Modifiers CLASS IDENTIFIER TypeParameters Superclass ClassBody
				| Modifiers CLASS IDENTIFIER TypeParameters Superinterfaces ClassBody
				| Modifiers CLASS IDENTIFIER Superclass Superinterfaces ClassBody
				| Modifiers CLASS IDENTIFIER TypeParameters Superclass Superinterfaces ClassBody
				| CLASS IDENTIFIER ClassBody
				| CLASS IDENTIFIER Superclass ClassBody
				| CLASS IDENTIFIER Superinterfaces ClassBody
				| CLASS IDENTIFIER TypeParameters ClassBody
				| CLASS IDENTIFIER TypeParameters Superclass ClassBody
				| CLASS IDENTIFIER TypeParameters Superinterfaces ClassBody
				| CLASS IDENTIFIER Superclass Superinterfaces ClassBody
				| CLASS IDENTIFIER TypeParameters Superclass Superinterfaces ClassBody;
				


TypeParameters: OPEN_ANGLE_BRACKET TypeParameterList CLOSE_ANGLE_BRACKET;

TypeParameterList:	TypeParameter
			| TypeParameterList COMMA TypeParameter;

Superclass:	EXTENDS IDENT_DOT_IDENT
		| EXTENDS IDENTIFIER;

Superinterfaces:	IMPLEMENTS InterfaceTypeList;

InterfaceTypeList:	IDENTIFIER
			| InterfaceTypeList COMMA IDENTIFIER
			| IDENT_DOT_IDENT 
			| InterfaceTypeList COMMA IDENT_DOT_IDENT;
			
ClassBody: 	OPEN_BRACE ClassBodyDeclaration CLOSE_BRACE
		| OPEN_BRACE CLOSE_BRACE;

ClassBodyDeclaration:	ClassMemberDeclaration
			| InstanceInitializer
			| StaticInitializer
			| ConstructorDeclaration
			| ClassBodyDeclaration ClassMemberDeclaration
			| ClassBodyDeclaration InstanceInitializer
			| ClassBodyDeclaration StaticInitializer
			| ClassBodyDeclaration ConstructorDeclaration;

			
ClassMemberDeclaration:	FieldDeclaration
				| MethodDeclaration
				| NormalClassDeclaration
				| InterfaceDeclaration
				| SEMICOLON;	
				
FieldDeclaration:	UnannType VariableDeclaratorList SEMICOLON
			| Modifiers UnannType VariableDeclaratorList SEMICOLON;

VariableDeclaratorList:	VariableDeclarator
				| VariableDeclaratorList COMMA VariableDeclarator;
				
VariableDeclarator:	VariableDeclaratorId
			| VariableDeclaratorId ASSIGNMENT VariableInitializer;
			
VariableDeclaratorId:	IDENTIFIER
			| IDENTIFIER Dims;				
	
VariableInitializer:	Expression
			| ArrayInitializer;

UnannType:	PrimitiveType
		| ReferenceType;
		
		
ReferenceType:	IDENT_DOT_IDENT
		| IDENTIFIER
		| ArrayType;


FormalParameterList:	FormalParameters
			| FormalParameters COMMA LastFormalParameter
			| LastFormalParameter;
			
FormalParameters:	FormalParametersP
			| FormalParametersR;
			
FormalParametersP:	FormalParameter
			| FormalParametersP COMMA FormalParameter;

FormalParametersR:	ReceiverParameter
			| FormalParametersR COMMA ReceiverParameter;
			
			
FormalParameter:	FINAL UnannType VariableDeclaratorId
			| UnannType VariableDeclaratorId;
			
			
ReceiverParameter:	UnannType IDENTIFIER DOT THIS
			| UnannType THIS;


LastFormalParameter:	 UnannType DOT DOT DOT VariableDeclaratorId
			| FINAL UnannType DOT DOT DOT VariableDeclaratorId;

			
Result:	UnannType
		| VOID;
				
Throws:	THROWS ExceptionTypeList;

ExceptionTypeList:	IDENT_DOT_IDENT
			| IDENTIFIER
			| ExceptionTypeList COMMA ExceptionTypeList;
		
MethodBody:	Block
		| SEMICOLON;

InstanceInitializer: Block;

StaticInitializer: STATIC Block;

ConstructorDeclaration:	Modifiers ConstructorDeclarator ConstructorBody
				| Modifiers ConstructorDeclarator Throws ConstructorBody
				| ConstructorDeclarator ConstructorBody
				| ConstructorDeclarator Throws ConstructorBody;

ConstructorDeclarator:	IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| TypeParameters IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENTIFIER OPEN_PARENT FormalParameterList CLOSE_PARENT
			| TypeParameters IDENTIFIER OPEN_PARENT FormalParameterList CLOSE_PARENT;


ConstructorBody:	OPEN_BRACE CLOSE_BRACE
			| OPEN_BRACE ExplicitConstructorInvocation CLOSE_BRACE
			| OPEN_BRACE BlockStatements CLOSE_BRACE
			| OPEN_BRACE ExplicitConstructorInvocation BlockStatements CLOSE_BRACE;

ExplicitConstructorInvocation:	THIS OPEN_PARENT CLOSE_PARENT SEMICOLON
					| TypeArguments THIS OPEN_PARENT CLOSE_PARENT SEMICOLON
					| THIS OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| TypeArguments THIS OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| TypeArguments SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| TypeArguments SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| IDENTIFIER DOT SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| IDENTIFIER DOT SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| IDENTIFIER DOT TypeArguments SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| IDENTIFIER DOT TypeArguments SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| IDENT_DOT_IDENT DOT SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| IDENT_DOT_IDENT DOT SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| IDENT_DOT_IDENT DOT TypeArguments SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| IDENT_DOT_IDENT DOT TypeArguments SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| Primary DOT SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| Primary DOT SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON
					| Primary DOT TypeArguments SUPER OPEN_PARENT CLOSE_PARENT SEMICOLON
					| Primary DOT TypeArguments SUPER OPEN_PARENT ArgumentList CLOSE_PARENT SEMICOLON;
					
EnumDeclaration:	ENUM IDENTIFIER EnumBody
			| ENUM IDENTIFIER Superinterfaces EnumBody
			| Modifiers ENUM IDENTIFIER EnumBody
			| Modifiers ENUM IDENTIFIER Superinterfaces EnumBody;

EnumBody:	OPEN_BRACE CLOSE_BRACE	
		| OPEN_BRACE EnumConstantList CLOSE_BRACE
		| OPEN_BRACE COMMA CLOSE_BRACE
		| OPEN_BRACE EnumBodyDeclarations CLOSE_BRACE
		| OPEN_BRACE EnumConstantList COMMA CLOSE_BRACE
		| OPEN_BRACE COMMA EnumBodyDeclarations CLOSE_BRACE
		| OPEN_BRACE EnumConstantList EnumBodyDeclarations CLOSE_BRACE
		| OPEN_BRACE EnumConstantList COMMA EnumBodyDeclarations CLOSE_BRACE;
		
EnumConstantList:	EnumConstant 
			| EnumConstantList COMMA EnumConstant;
			
EnumConstant:	IDENTIFIER
		| IDENTIFIER ClassBody
		| IDENTIFIER OPEN_PARENT CLOSE_PARENT
		| IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
		| IDENTIFIER OPEN_PARENT CLOSE_PARENT ClassBody
		| IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT ClassBody;
			
EnumBodyDeclarations:	SEMICOLON ClassBodyDeclaration;

InterfaceDeclaration:	NormalInterfaceDeclaration
			| AnnotationTypeDeclaration;
	
NormalInterfaceDeclaration:	Modifiers INTERFACE IDENTIFIER InterfaceBody
				| Modifiers INTERFACE IDENTIFIER TypeParameters InterfaceBody
				| Modifiers INTERFACE IDENTIFIER ExtendsInterfaces InterfaceBody
				| Modifiers INTERFACE IDENTIFIER TypeParameters ExtendsInterfaces InterfaceBody
				| INTERFACE IDENTIFIER InterfaceBody
				| INTERFACE IDENTIFIER TypeParameters InterfaceBody
				| INTERFACE IDENTIFIER ExtendsInterfaces InterfaceBody
				| INTERFACE IDENTIFIER TypeParameters ExtendsInterfaces InterfaceBody;
	
Modifiers:	STATIC
		| ABSTRACT
		| STRICTFP
		| PUBLIC
		| FINAL
		| SYNCHRONIZED
		| NATIVE
		| DEFAULT
		| ACCESS
		| TRANSIENT
		| VOLATILE
		| Modifiers TRANSIENT
		| Modifiers VOLATILE
		| Modifiers ACCESS
		| Modifiers DEFAULT
		| Modifiers PUBLIC
		| Modifiers FINAL
		| Modifiers STATIC
		| Modifiers ABSTRACT
		| Modifiers STRICTFP
		| Modifiers SYNCHRONIZED
		| Modifiers NATIVE;
				
				
ExtendsInterfaces:	EXTENDS InterfaceTypeList;

InterfaceBody:	OPEN_BRACE InterfaceMemberDeclaration CLOSE_BRACE
		| OPEN_BRACE CLOSE_BRACE;

InterfaceMemberDeclaration:	ConstantDeclaration
				| InterfaceMethodDeclaration
				| ClassDeclaration
				| InterfaceDeclaration
				| SEMICOLON
				| InterfaceMemberDeclaration ConstantDeclaration
				| InterfaceMemberDeclaration InterfaceMethodDeclaration
				| InterfaceMemberDeclaration ClassDeclaration
				| InterfaceMemberDeclaration InterfaceDeclaration
				| InterfaceMemberDeclaration SEMICOLON;
				
ConstantDeclaration:	Modifiers UnannType VariableDeclaratorList
			| UnannType VariableDeclaratorList;


InterfaceMethodDeclaration:	MethodHeader MethodBody
				| Modifiers MethodHeader MethodBody;


AnnotationTypeDeclaration:	AT INTERFACE IDENTIFIER AnnotationTypeBody
				| Modifiers AT INTERFACE IDENTIFIER AnnotationTypeBody;

AnnotationTypeBody:	OPEN_BRACE AnnotationTypeMemberDeclaration CLOSE_BRACE
			| OPEN_BRACE CLOSE_BRACE;

AnnotationTypeMemberDeclaration:	AnnotationTypeElementDeclaration
					| ConstantDeclaration
					| ClassDeclaration
					| InterfaceDeclaration
					| SEMICOLON
					| AnnotationTypeMemberDeclaration AnnotationTypeElementDeclaration
					| AnnotationTypeMemberDeclaration ConstantDeclaration
					| AnnotationTypeMemberDeclaration ClassDeclaration
					| AnnotationTypeMemberDeclaration InterfaceDeclaration
					| AnnotationTypeMemberDeclaration SEMICOLON;
					
AnnotationTypeElementDeclaration:	 AnnotationTypeElementDeclarationStart SEMICOLON
					| AnnotationTypeElementDeclarationStart Dims SEMICOLON
					| AnnotationTypeElementDeclarationStart DEFAULT ElementValue SEMICOLON
					| AnnotationTypeElementDeclarationStart Dims DEFAULT ElementValue SEMICOLON;
					
AnnotationTypeElementDeclarationStart: Modifiers UnannType IDENTIFIER OPEN_PARENT CLOSE_PARENT
					| UnannType IDENTIFIER OPEN_PARENT CLOSE_PARENT;


ElementValue:	ConditionalExpression
		| ElementValueArrayInitializer;

ElementValueArrayInitializer:	OPEN_BRACE CLOSE_BRACE
				| OPEN_BRACE COMMA CLOSE_BRACE
				| OPEN_BRACE ElementValueList CLOSE_BRACE
				| OPEN_BRACE ElementValueList COMMA CLOSE_BRACE;
				
ElementValueList:	ElementValue
			| ElementValueList COMMA ElementValue;

ArrayInitializer: 	OPEN_BRACE CLOSE_BRACE
			| OPEN_BRACE VariableInitializerList CLOSE_BRACE
			| OPEN_BRACE COMMA CLOSE_BRACE
			| OPEN_BRACE VariableInitializerList COMMA CLOSE_BRACE;
			
VariableInitializerList:	VariableInitializer
				| VariableInitializerList COMMA VariableInitializer;		

Block: 	OPEN_BRACE BlockStatements CLOSE_BRACE
		| OPEN_BRACE CLOSE_BRACE;

BlockStatements:	BlockStatement
			| BlockStatements BlockStatement;
			
BlockStatement:	LocalVariableDeclarationStatement
			| ClassDeclaration
			| Statement;
			
LocalVariableDeclarationStatement:	LocalVariableDeclaration SEMICOLON;

LocalVariableDeclaration:	UnannType VariableDeclaratorList
				| FINAL UnannType VariableDeclaratorList;

Statement:	StatementWithoutTrailingSubstatement
		| LabeledStatement
		| IfThenStatement
		| IfThenElseStatement
		| WhileStatement
		| ForStatement;
		
StatementNoShortIf:	StatementWithoutTrailingSubstatement
			| LabeledStatementNoShortIf
			| IfThenElseStatementNoShortIf
			| WhileStatementNoShortIf
			| ForStatementNoShortIf;
		
StatementWithoutTrailingSubstatement:	Block
					| EmptyStatement
					| ExpressionStatement
					| AssertStatement
					| SwitchStatement
					| DoStatement
					| BreakStatement
					| ContinueStatement
					| ReturnStatement
					| SynchronizedStatement
					| ThrowStatement
					| TryStatement;
EmptyStatement:	SEMICOLON;

LabeledStatement:	IDENTIFIER COLON Statement;

LabeledStatementNoShortIf:	IDENTIFIER COLON StatementNoShortIf;

ExpressionStatement:	StatementExpression SEMICOLON;

StatementExpression:	ClassInstanceCreationExpression
			| PreIncrementExpression
			| PreDecrementExpression
			| PostIncrementExpression
			| PostDecrementExpression
			| MethodInvocation
			| Assignment;
			 
IfThenStatement:	IF OPEN_PARENT Expression CLOSE_PARENT Statement;

IfThenElseStatement:	IF OPEN_PARENT Expression CLOSE_PARENT StatementNoShortIf ELSE Statement;

IfThenElseStatementNoShortIf:	IF OPEN_PARENT Expression CLOSE_PARENT StatementNoShortIf ELSE StatementNoShortIf;

AssertStatement:	ASSERT Expression SEMICOLON
			| ASSERT Expression COLON Expression SEMICOLON;
			
SwitchStatement:	SWITCH OPEN_PARENT Expression CLOSE_PARENT SwitchBlock;

SwitchBlock:	OPEN_BRACE CLOSE_BRACE
		| OPEN_BRACE SwitchLabel CLOSE_BRACE
		| OPEN_BRACE SwitchBlockStatementGroup CLOSE_BRACE
		| OPEN_BRACE SwitchBlockStatementGroup SwitchLabel CLOSE_BRACE;

SwitchBlockStatementGroup:	SwitchLabels BlockStatements
				| SwitchBlockStatementGroup SwitchLabels BlockStatements;

SwitchLabels:	SwitchLabel 
		| SwitchLabels SwitchLabel;

SwitchLabel:	CASE Expression COLON
		| CASE IDENTIFIER COLON
		| DEFAULT COLON
		| SwitchLabel CASE Expression COLON
		| SwitchLabel CASE IDENTIFIER COLON
		| SwitchLabel DEFAULT COLON;

WhileStatement:	WHILE OPEN_PARENT Expression CLOSE_PARENT Statement;

WhileStatementNoShortIf:	WHILE OPEN_PARENT Expression CLOSE_PARENT StatementNoShortIf;

DoStatement:	DO Statement WHILE OPEN_PARENT Expression CLOSE_PARENT SEMICOLON;

ForStatement:	BasicForStatement
		| EnhancedForStatement;
		
ForStatementNoShortIf:	BasicForStatementNoShortIf
			| EnhancedForStatementNoShortIf;
			
BasicForStatement:	FOR OPEN_PARENT  SEMICOLON  SEMICOLON  CLOSE_PARENT Statement
			| FOR OPEN_PARENT ForInit SEMICOLON SEMICOLON CLOSE_PARENT Statement
			| FOR OPEN_PARENT SEMICOLON Expression SEMICOLON CLOSE_PARENT Statement
			| FOR OPEN_PARENT SEMICOLON SEMICOLON ForUpdate CLOSE_PARENT Statement
			| FOR OPEN_PARENT ForInit SEMICOLON Expression SEMICOLON CLOSE_PARENT Statement
			| FOR OPEN_PARENT ForInit SEMICOLON SEMICOLON ForUpdate CLOSE_PARENT Statement
			| FOR OPEN_PARENT SEMICOLON Expression SEMICOLON ForUpdate CLOSE_PARENT Statement
			| FOR OPEN_PARENT ForInit SEMICOLON Expression SEMICOLON ForUpdate CLOSE_PARENT Statement;

BasicForStatementNoShortIf:	FOR OPEN_PARENT  SEMICOLON  SEMICOLON  CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT ForInit SEMICOLON SEMICOLON CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT SEMICOLON Expression SEMICOLON CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT SEMICOLON SEMICOLON ForUpdate CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT ForInit SEMICOLON Expression SEMICOLON CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT ForInit SEMICOLON SEMICOLON ForUpdate CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT SEMICOLON Expression SEMICOLON ForUpdate CLOSE_PARENT StatementNoShortIf
				| FOR OPEN_PARENT ForInit SEMICOLON Expression SEMICOLON ForUpdate CLOSE_PARENT StatementNoShortIf;

ForInit:	StatementExpressionList
		| LocalVariableDeclaration;

ForUpdate:	StatementExpressionList;

StatementExpressionList:	StatementExpression
				| StatementExpressionList COMMA StatementExpression;
				
EnhancedForStatement:	FOR OPEN_PARENT UnannType VariableDeclaratorId COLON Expression CLOSE_PARENT Statement
			| FOR OPEN_PARENT FINAL UnannType VariableDeclaratorId COLON Expression CLOSE_PARENT Statement;

EnhancedForStatementNoShortIf:	FOR OPEN_PARENT UnannType VariableDeclaratorId COLON Expression CLOSE_PARENT StatementNoShortIf
					| FOR OPEN_PARENT FINAL UnannType VariableDeclaratorId COLON Expression CLOSE_PARENT StatementNoShortIf;

BreakStatement:	BREAK IDENTIFIER SEMICOLON
			| BREAK SEMICOLON;
			
ContinueStatement:	CONTINUE IDENTIFIER SEMICOLON
			| CONTINUE SEMICOLON;

ReturnStatement:	RETURN Expression SEMICOLON
			| RETURN SEMICOLON;
			
ThrowStatement:	THROW Expression SEMICOLON;

SynchronizedStatement:	SYNCHRONIZED OPEN_PARENT Expression CLOSE_PARENT Block;

TryStatement:	TRY Block Catches
		| TRY Block Finally
		| TRY Block Catches Finally
		| TryWithResourcesStatement;
		
Catches:	CatchClause
		| Catches CatchClause;

CatchClause:	CATCH OPEN_PARENT CatchFormalParameter CLOSE_PARENT Block;

CatchFormalParameter:	CatchType VariableDeclaratorId
			| FINAL CatchType VariableDeclaratorId;

CatchType:	IDENT_DOT_IDENT
		| IDENTIFIER
		| CatchType OR IDENT_DOT_IDENT
		| CatchType OR IDENTIFIER;

Finally:	FINALLY Block;

TryWithResourcesStatement:	TRY ResourceSpecification Block
				| TRY ResourceSpecification Block Catches
				| TRY ResourceSpecification Block Finally
				| TRY ResourceSpecification Block Catches Finally;
				
ResourceSpecification:	OPEN_PARENT ResourceList CLOSE_PARENT
			| OPEN_PARENT ResourceList SEMICOLON CLOSE_PARENT;
ResourceList:	Resource
		| ResourceList SEMICOLON Resource;
		
Resource:	UnannType VariableDeclaratorId ASSIGNMENT Expression
		| FINAL UnannType VariableDeclaratorId ASSIGNMENT Expression;

Expression:	LambdaExpression
		| AssignmentExpression;
		
Primary:	PrimaryNoNewArray
		| ArrayCreationExpression;
		
PrimaryNoNewArray:	Literal
			| ClassLiteral
			| THIS
			| IDENT_DOT_IDENT DOT THIS
			| IDENTIFIER DOT THIS
			| OPEN_PARENT Expression CLOSE_PARENT
			| ClassInstanceCreationExpression
			| FieldAccess
			| ArrayAccess
			| MethodInvocation
			| MethodReference;


		
ClassLiteral:	IDENT_DOT_IDENT Dims DOT CLASS
		| IDENTIFIER Dims DOT CLASS
		| NumericType Dims DOT CLASS	
		| BOOLEAN_TYPE Dims DOT CLASS
		| IDENT_DOT_IDENT DOT CLASS
		| IDENTIFIER DOT CLASS
		| NumericType  DOT CLASS	
		| BOOLEAN_TYPE DOT CLASS
		| VOID DOT CLASS;		
				
ClassInstanceCreationExpression:	UnqualifiedClassInstanceCreationExpression
					| IDENT_DOT_IDENT DOT UnqualifiedClassInstanceCreationExpression
					| IDENTIFIER DOT UnqualifiedClassInstanceCreationExpression
					| Primary DOT UnqualifiedClassInstanceCreationExpression;
					
UnqualifiedClassInstanceCreationExpression:	NEW ClassOrInterfaceTypeToInstantiate OPEN_PARENT CLOSE_PARENT
						| NEW ClassOrInterfaceTypeToInstantiate OPEN_PARENT ArgumentList CLOSE_PARENT
						| NEW ClassOrInterfaceTypeToInstantiate OPEN_PARENT CLOSE_PARENT ClassBody
						| NEW ClassOrInterfaceTypeToInstantiate OPEN_PARENT ArgumentList CLOSE_PARENT ClassBody
						| NEW TypeArguments ClassOrInterfaceTypeToInstantiate OPEN_PARENT CLOSE_PARENT
						| NEW TypeArguments ClassOrInterfaceTypeToInstantiate OPEN_PARENT ArgumentList CLOSE_PARENT
						| NEW TypeArguments ClassOrInterfaceTypeToInstantiate OPEN_PARENT CLOSE_PARENT ClassBody
						| NEW TypeArguments ClassOrInterfaceTypeToInstantiate OPEN_PARENT ArgumentList CLOSE_PARENT ClassBody;

ClassOrInterfaceTypeToInstantiate:	IDENT_DOT_IDENT
					| IDENT_DOT_IDENT TypeArgumentsOrDiamond
					| IDENTIFIER
					| IDENTIFIER TypeArgumentsOrDiamond;
	
TypeArgumentsOrDiamond:	TypeArguments
				| OPEN_ANGLE_BRACKET CLOSE_ANGLE_BRACKET;
				
ArrayCreationExpression:	NEW PrimitiveType DimExprs
				| NEW PrimitiveType DimExprs Dims 
				| NEW IDENT_DOT_IDENT DimExprs
				| NEW IDENTIFIER DimExprs
				| NEW IDENTIFIER DimExprs Dims
				| NEW IDENT_DOT_IDENT DimExprs Dims
				| NEW PrimitiveType Dims ArrayInitializer
				| NEW IDENTIFIER Dims ArrayInitializer
				| NEW IDENT_DOT_IDENT Dims ArrayInitializer;
				
DimExprs:	DimExpr
		| DimExprs DimExpr;
		
DimExpr:	OPEN_BRACKET Expression CLOSE_BRACKET;
		
ArrayAccess:	IDENT_DOT_IDENT OPEN_BRACKET Expression CLOSE_BRACKET
		| IDENTIFIER OPEN_BRACKET Expression CLOSE_BRACKET
		| PrimaryNoNewArray OPEN_BRACKET Expression CLOSE_BRACKET;
		
FieldAccess:	Primary DOT IDENTIFIER
		| SUPER DOT IDENTIFIER
		| IDENT_DOT_IDENT DOT SUPER DOT IDENTIFIER
		| IDENTIFIER DOT SUPER DOT IDENTIFIER;
		
MethodInvocation:	IDENT_DOT_IDENT OPEN_PARENT CLOSE_PARENT
			| IDENT_DOT_IDENT DOT TypeArguments IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENT_DOT_IDENT OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENT_DOT_IDENT DOT TypeArguments IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENTIFIER DOT TypeArguments IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENTIFIER DOT TypeArguments IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| Primary DOT IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| Primary DOT TypeArguments IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| Primary DOT IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| Primary DOT TypeArguments IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| SUPER DOT IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| SUPER DOT TypeArguments IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| SUPER DOT IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| SUPER DOT TypeArguments IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENT_DOT_IDENT DOT SUPER DOT IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENT_DOT_IDENT DOT SUPER DOT TypeArguments IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENT_DOT_IDENT DOT SUPER DOT IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENT_DOT_IDENT DOT SUPER DOT TypeArguments IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENTIFIER DOT SUPER DOT IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENTIFIER DOT SUPER DOT TypeArguments IDENTIFIER OPEN_PARENT CLOSE_PARENT
			| IDENTIFIER DOT SUPER DOT IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT
			| IDENTIFIER DOT SUPER DOT TypeArguments IDENTIFIER OPEN_PARENT ArgumentList CLOSE_PARENT;

ArgumentList:	Expression 
		| ArgumentList COMMA Expression;
		
MethodReference:	ReferenceType DOUBLE_COLON IDENTIFIER
			| ReferenceType DOUBLE_COLON TypeArguments IDENTIFIER
			| Primary DOUBLE_COLON IDENTIFIER
			| Primary DOUBLE_COLON TypeArguments IDENTIFIER
			| SUPER DOUBLE_COLON IDENTIFIER
			| SUPER DOUBLE_COLON TypeArguments IDENTIFIER 
			| IDENT_DOT_IDENT DOUBLE_COLON NEW
			| IDENTIFIER DOUBLE_COLON NEW
			| IDENT_DOT_IDENT DOUBLE_COLON TypeArguments NEW
			| IDENTIFIER DOUBLE_COLON TypeArguments NEW
			| ArrayType DOUBLE_COLON NEW;
			
PostfixExpression:	Primary
			| IDENT_DOT_IDENT
			| IDENTIFIER	
			| PostIncrementExpression
			| PostDecrementExpression;

PostIncrementExpression:	PostfixExpression INCREMENT;

PostDecrementExpression:	PostfixExpression DECREMENT;

UnaryExpression:	PreIncrementExpression
			| PreDecrementExpression
			| PLUS UnaryExpression
			| MINUS UnaryExpression
			| UnaryExpressionNotPlusMinus;

PreIncrementExpression:	INCREMENT UnaryExpression;

PreDecrementExpression:	DECREMENT UnaryExpression;

UnaryExpressionNotPlusMinus:	PostfixExpression
				| UNARY_SIGN_ONLY UnaryExpression
				| CastExpression;
				
CastExpression:	OPEN_PARENT PrimitiveType CLOSE_PARENT UnaryExpression
			| OPEN_PARENT IDENT_DOT_IDENT AdditionalBound CLOSE_PARENT UnaryExpressionNotPlusMinus
			| OPEN_PARENT IDENTIFIER AdditionalBound CLOSE_PARENT UnaryExpressionNotPlusMinus
			| OPEN_PARENT ArrayType AdditionalBound CLOSE_PARENT UnaryExpressionNotPlusMinus
			| OPEN_PARENT IDENT_DOT_IDENT AdditionalBound CLOSE_PARENT LambdaExpression
			| OPEN_PARENT IDENTIFIER AdditionalBound CLOSE_PARENT LambdaExpression
			| OPEN_PARENT ArrayType AdditionalBound CLOSE_PARENT LambdaExpression;
			
MultiplicativeExpression:	UnaryExpression
				| MultiplicativeExpression STAR UnaryExpression
				| MultiplicativeExpression DIVISION UnaryExpression
				| MultiplicativeExpression PERCENT UnaryExpression;
				
AdditiveExpression:	MultiplicativeExpression
			| AdditiveExpression PLUS MultiplicativeExpression
			| AdditiveExpression MINUS MultiplicativeExpression;
			
ShiftExpression:	AdditiveExpression
			| ShiftExpression SHIFT AdditiveExpression;
			
RelationalExpression:	ShiftExpression
			| RelationalExpression OPEN_ANGLE_BRACKET ShiftExpression
			| RelationalExpression CLOSE_ANGLE_BRACKET ShiftExpression
			| RelationalExpression MORE_LESS_EQUAL ShiftExpression
			| RelationalExpression INSTANCEOF IDENT_DOT_IDENT
			| RelationalExpression INSTANCEOF IDENTIFIER
			| RelationalExpression INSTANCEOF ArrayType;
			
EqualityExpression:	RelationalExpression
			| EqualityExpression COMPARISON_SIGN RelationalExpression;
		
AndExpression:	EqualityExpression
		| AndExpression AND EqualityExpression;

ExclusiveOrExpression:	AndExpression
			| ExclusiveOrExpression EXCLUSIVE AndExpression;
			
InclusiveOrExpression:	ExclusiveOrExpression
			| InclusiveOrExpression OR ExclusiveOrExpression;
			
ConditionalAndExpression:	InclusiveOrExpression
				| ConditionalAndExpression DOUBLE_AND InclusiveOrExpression;
				
ConditionalOrExpression:	ConditionalAndExpression
				| ConditionalOrExpression DOUBLE_OR ConditionalAndExpression;
				
ConditionalExpression:	ConditionalOrExpression
			| ConditionalOrExpression QUERY Expression COLON ConditionalExpression
			| ConditionalOrExpression QUERY Expression COLON LambdaExpression;
			
AssignmentExpression:	OPEN_PARENT ConditionalExpression CLOSE_PARENT
			|  ConditionalExpression
			| Assignment;
			
Assignment:	LeftHandSide AssignmentOperator Expression;

LeftHandSide:	IDENT_DOT_IDENT
		| IDENTIFIER
		| FieldAccess
		| ArrayAccess;
		
AssignmentOperator:	ASSIGNMENT
			| ASSIGNMENT_ACTION;
		
LambdaExpression:	LambdaParameters LAMBDA_ASSIGNMENT LambdaBody;


LambdaParameters:	IDENTIFIER
			| OPEN_PARENT FormalParameterList CLOSE_PARENT
			| OPEN_PARENT CLOSE_PARENT
			| OPEN_PARENT InferredFormalParameterList CLOSE_PARENT;
			
InferredFormalParameterList:	IDENTIFIER COMMA IDENTIFIER
				| InferredFormalParameterList COMMA IDENTIFIER;

LambdaBody:	Expression
		| Block;

		
%%
