%{
  let getPos = Parsing.symbol_start
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

%nonassoc ASSIGN
%left AND OR
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%type <unit> exp
%start exp

%%
exp:
    | lvalue {}
    | NIL {}
    | Sequencing {}
    | INT {}
    | STRING {}
    | MINUS exp {}
    | Funccall {}
    | BinOpExp {}
    | RecordCreation {}
    | ArrayCreation {}
    | Assignment {}
    | IfThenElse {}
    | IfThen {}
    | While {}
    | For {}
    | BREAK {}
    | Let {}
    | parenthesis {}

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
    | ID LPAREN parameters RPAREN {Printf.printf "funccall\n";}

parameters : 
    | {}
    | exp {}
    | exp COMMA parameters {}

BinOpExp : 
    | exp PLUS exp {}
    | exp MINUS exp {}
    | exp TIMES exp {}
    | exp DIVIDE exp {}
    | exp EQ exp {Printf.printf "EQQQ";;}
    | exp NEQ exp {}
    | exp LT exp {}
    | exp LE exp {}
    | exp GT exp {}
    | exp GE exp {}
    | exp AND exp {}
    | exp OR exp {}

RecordCreation :
    | ID LBRACE recordExpList RBRACE {Printf.printf "record creation\n";}

recordExpList :
    | recordExpField {}
    | recordExpList COMMA recordExpField {}

recordExpField : 
    | {}
    | ID EQ exp {}

ArrayCreation : 
    | ID LBRACK exp RBRACK OF exp {}
	
Assignment :
    | lvalue ASSIGN exp {}
	
IfThenElse :
    | IF exp THEN exp ELSE exp {}

IfThen :
    | IF exp THEN exp {}
	
While : 
    | WHILE exp DO exp {}

For :
  | FOR ID ASSIGN exp TO exp DO exp {}
      
Let :
  | LET decs IN expseq END {}

decs :
  | decs dec {}
  |  {}

dec : 
  | tydec {}
  | vardec {}
  | fundec {}

tydec :
  | TYPE ID EQ ty {}

ty :
  | ID {} 
  | LBRACE tyfields RBRACE {}
  | ARRAY OF ID {}

tyfields :
  | {}
  | tyfieldlist {}

tyfieldlist :
  | tyfieldlist COMMA tyfield {}
  | tyfield {}

tyfield : 
  | ID COLON ID {}

vardec : 
  | VAR ID ASSIGN exp {}
  | VAR ID COLON ID ASSIGN exp {}

fundec :
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp {}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {}


expseq : 
  | expList {}
  | exp {}
  | {}

parenthesis : 
  | LPAREN exp RPAREN {}
