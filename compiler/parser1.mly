%{
  open Syntax
  module S = Symbol
  let getPos : unit -> Syntax.pos = Parsing.symbol_start
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

%type <Syntax.exp> program
%type <Syntax.exp> exp
%type <Syntax.var> lvalue
%type <Syntax.dec list> decs
%start program

%%
program:
exp EOF {$1}
  ;
exp:
  | lvalue { VarExp($1) }
  | NIL { NilExp }
  | Sequencing {SeqExp($1)}
  | INT {IntExp($1)}
  | STRING { StringExp($1, getPos()) }
  /*| MINUS exp {}*/
  | Funccall { $1 }
  | BinOpExp { $1 }
  | RecordCreation { $1 }
  | ArrayCreation { $1 }
  | Assignment { $1 }
  | IfThenElse { $1 }
  | IfThen { $1 }
  | While { $1 }
  | For { $1 }
  | BREAK { BreakExp (getPos()) }
  | Let { $1 }
  /*| parenthesis { $1 }*/

id : ID { S.symbol $1 }

lvalue :
  | id { SimpleVar($1, getPos()) }
  | lvalue DOT id { FieldVar($1, $3, getPos()) }
  | lvalue LBRACK exp RBRACK { SubscriptVar($1, $3, getPos()) }
      
Sequencing :
  | LPAREN expList RPAREN {$2}

expList : 
  | expList SEMICOLON exp { $1 @ [($3, getPos())] }
  | exp SEMICOLON exp { ($1, getPos()) :: [($3, getPos())] }

Funccall :
  | id LPAREN parameters RPAREN { CallExp($1, $3, getPos()) }

parameters : 
  | { [] }
  | exp {[$1] }
  | exp COMMA parameters { $1::$3 }

BinOpExp : 
  | exp PLUS exp { OpExp($1, PlusOp, $3, getPos()) }
  | exp MINUS exp { OpExp($1, MinusOp, $3, getPos()) }
  | exp TIMES exp { OpExp($1, TimesOp, $3, getPos()) }
  | exp DIVIDE exp { OpExp($1, DivideOp, $3, getPos()) }
  | exp EQ exp { OpExp($1, EqOp, $3, getPos()) }
  | exp NEQ exp { OpExp($1, NeqOp, $3, getPos()) }
  | exp LT exp { OpExp($1, LtOp, $3, getPos()) }
  | exp LE exp { OpExp($1, LeOp, $3, getPos()) }
  | exp GT exp { OpExp($1, GtOp, $3, getPos()) }
  | exp GE exp { OpExp($1, GeOp, $3, getPos()) }
  | exp AND exp { IfExp ($1, $3, Some(IntExp(0)), getPos()) }
  | exp OR exp { IfExp ($1, IntExp(1), Some($3), getPos()) }

RecordCreation :
  | id LBRACE recordExpList RBRACE { RecordExp ($3, $1, getPos()) }

recordExpList :
  | recordExpField { $1 }
  | recordExpList COMMA recordExpField {$1 @ $3}

recordExpField : 
  | {[]}
  | id EQ exp { [($1, $3, getPos())] }

ArrayCreation : 
  | id LBRACK exp RBRACK OF exp { ArrayExp ($1, $3, $6, getPos()) }
	
Assignment :
  | lvalue ASSIGN exp { AssignExp ($1, $3, getPos()) }
	
IfThenElse :
  | IF exp THEN exp ELSE exp { IfExp ($2, $4, Some $6, getPos()) }

IfThen :
  | IF exp THEN exp {IfExp ($2, $4, None, getPos()) }
	
While : 
  | WHILE exp DO exp { WhileExp ($2, $4, getPos()) }

For :
  | FOR id ASSIGN exp TO exp DO exp { ForExp ($2, ref true, $4, $6, $8, getPos()) }
      
Let :
  | LET decs IN expseq END { LetExp ( $2, $4, getPos()) }

decs :
  | dec decs { $1 :: $2 }
  |  { [] }

dec : 
  | tydecs { TypeDec($1) }
  | vardec { $1 }
  | fundecs { FunctionDec($1) }

tydecs :
  | tydec { [$1] }
  | tydec tydecs {$1 :: $2}

tydec : 
  | TYPE id EQ ty { ($2, $4, getPos()) }

ty :
  | id { NameTy ($1, getPos()) } 
  | LBRACE tyfields RBRACE { RecordTy($2) }
  | ARRAY OF id { ArrayTy($3, getPos()) }

tyfields :
  | { [] }
  | tyfieldlist { $1 }

tyfieldlist :
  | tyfieldlist COMMA tyfield { $1 @ [$3] }
  | tyfield { [$1] }

tyfield : 
  | id COLON id { ($1, ref true, $3,  getPos()) }

vardec : 
  | VAR id ASSIGN exp { VarDec ($2, ref true, None, $4, getPos()) }
  | VAR id COLON id ASSIGN exp { VarDec ($2, ref true, Some(($4, getPos())), $6, getPos()) }

fundecs : 
  | fundec { [$1] }
  | fundec fundecs { $1 :: $2 }

fundec :
  | FUNCTION id LPAREN tyfields RPAREN EQ exp { (*fundec*) ($2, $4, None, $7, getPos()) }
  | FUNCTION id LPAREN tyfields RPAREN COLON id EQ exp { (*fundec*) ($2, $4, Some ($7, getPos()), $9, getPos()) }


expseq : 
  | expList { SeqExp($1) }
  | exp { SeqExp([($1, getPos())]) }
  | { SeqExp([]) }


/*parenthesis : 
  | LPAREN exp RPAREN {} */
