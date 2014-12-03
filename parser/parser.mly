%{

%}

%token <string> ID
%token TYPE
%token VAR
%token FUNCTION
%token BREAK
%token OF
%token END
%token IN
%token NIL
%token LET
%token DO
%token TO
%token FOR
%token WHILE
%token ELSE
%token THEN
%token IF
%token ARRAY
%token ASSIGN
%token OR
%token AND
%token GE
%token GT
%token LE
%token LT
%token NEQ
%token EQ
%token DIVIDE
%token TIMES
%token MINUS
%token PLUS
%token DOT
%token RBRACE
%token LBRACE
%token RBRACK
%token LBRACK
%token RPAREN
%token LPAREN
%token SEMICOLON
%token COLON
%token COMMA
%token EOF
%token <int> INT
%token <string> STRING

%type <unit> exp
%start exp

%%
exp:
    | lvalue {}
    | Nil {}
    | Sequencing {}
    | INT {}
    | STRING {}
    | MINUS exp
    | Funccall {}
    | BinOpExp {}
    | RecordCreation {}
    | ArrayCreation {}
    | assignment {}
    | IfThenElse {}
    | IfThen {}
    | While {}
    | For {}
    | Break {}
    | Let
    | parenthesis

lvalue :
    | ID {}
    | lvalue DOT ID {}
    | lvalue LBRACK exp RBRACK {}

Sequencing :
    | LPAREN expList RPAREN {}

expList : 
    | expList SEMICOLON exp {}
    | exp SEMICOLON exp {}

Funccall :
    | ID LPAREN parameters RPAREN {}

parameters : 
    | params {}
    |  {}
	
params :
    | params COMMA exp {}
    | exp {}

BinOpExp : 

