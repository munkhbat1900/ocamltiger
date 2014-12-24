%{
  open Ast
  module S = Symbol
  let getPos : unit -> Ast.pos = Parsing.symbol_start
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

%type <Ast.exp> program
%type <Ast.exp> exp
%type <Ast.var> lvalue
%type <Ast.dec list> decs
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
  | id LPAREN parameters RPAREN { CallExp{func = $1; args = $3; call_pos = getPos() }}

parameters : 
  | { [] }
  | exp {[$1] }
  | exp COMMA parameters { $1::$3 }

BinOpExp : 
  | exp PLUS exp { OpExp {left = $1; oper = PlusOp; right = $3; op_pos = getPos() } }
  | exp MINUS exp { OpExp {left = $1; oper = MinusOp; right = $3; op_pos = getPos() } }
  | exp TIMES exp { OpExp {left = $1; oper = TimesOp; right = $3; op_pos = getPos() } }
  | exp DIVIDE exp {OpExp {left = $1; oper = DivideOp; right = $3; op_pos = getPos() }}
  | exp EQ exp { OpExp {left = $1; oper = EqOp; right = $3; op_pos = getPos() } }
  | exp NEQ exp { OpExp {left = $1; oper = NeqOp; right = $3; op_pos = getPos() } }
  | exp LT exp { OpExp {left = $1; oper = LtOp; right = $3; op_pos = getPos() } }
  | exp LE exp { OpExp {left = $1; oper = LeOp; right = $3; op_pos = getPos() } }
  | exp GT exp { OpExp {left = $1; oper = GtOp; right = $3; op_pos = getPos() } }
  | exp GE exp { OpExp {left = $1; oper = GeOp; right = $3; op_pos = getPos() } }
  | exp AND exp { IfExp {if_test = $1; then' = $3; else' = Some(IntExp(0)); if_pos = getPos() } }
  | exp OR exp { IfExp {if_test = $1; then' = IntExp(1); else' = Some($3); if_pos = getPos() } }

RecordCreation :
  | id LBRACE recordExpList RBRACE { RecordExp {fields = $3; typ = $1; record_pos = getPos() } }

recordExpList :
  | recordExpField { $1 }
  | recordExpList COMMA recordExpField {$1 @ $3}

recordExpField : 
  | {[]}
  | id EQ exp { [($1, $3, getPos())] }

ArrayCreation : 
  | id LBRACK exp RBRACK OF exp { ArrayExp {arr_typ = $1; size = $3; init = $6; array_pos = getPos()} }
	
Assignment :
  | lvalue ASSIGN exp { AssignExp {assign_var = $1; exp = $3; assign_pos = getPos()} }
	
IfThenElse :
  | IF exp THEN exp ELSE exp { IfExp {if_test = $2; then' = $4; else' = Some $6; if_pos = getPos()}}

IfThen :
  | IF exp THEN exp {IfExp {if_test = $2; then' = $4; else' = None; if_pos = getPos()}}
	
While : 
  | WHILE exp DO exp { WhileExp {while_test = $2; while_body = $4; while_pos = getPos()} }

For :
  | FOR id ASSIGN exp TO exp DO exp { ForExp {for_var = $2; for_escape = ref true; lo = $4; hi = $6; for_body = $8; for_pos = getPos()} }
      
Let :
  | LET decs IN expseq END { LetExp { decs = $2; body = $4; let_pos = getPos() } }

decs :
  | decs dec { $1 @ $2 }
  |  { [] }

dec : 
  | tydecs { $1 }
  | vardec { $1 }
  | fundecs { $1 (*FunctionDec($1)*) }

tydecs :
  | tydec { [$1] }
  | tydec tydecs {[$1] @ $2}

tydec : 
  | TYPE id EQ ty { type_dec {typ_name = $2; ty = $4; type_pos = getPos()} }

ty :
  | id { NameTy ($1, getPos()) } 
  | LBRACE tyfields RBRACE { RecordTy($2) }
  | ARRAY OF id { ArrayTy($3, getPos()) }

tyfields :
  | { [] }
  | tyfieldlist { $1 }

tyfieldlist :
  | tyfieldlist COMMA tyfield { $1 @ [$3] }
  | tyfield {[$1]}

tyfield : 
  | id COLON id {field { field_name = $1; field_escape = ref true; typ = $3; field_pos = getPos() }}

vardec : 
  | VAR id ASSIGN exp { VarDec {var_name = $2; var_escape = ref true; typ = None; init = $4; var_pos = getPos()} }
  | VAR id COLON id ASSIGN exp { VarDec {var_name = $2; var_escape = ref true; typ = Some(($4, getpos())); init = $6; var_pos = getPos() } }

fundecs : 
  | fundec { $1 :: [] }
  | fundec fundecs { $1 :: $2 }

fundec :
  | FUNCTION id LPAREN tyfields RPAREN EQ exp { fundec {fun_name = $2; params = $4; result = NONE; body = $7; fun_pos = getPos() } }
  | FUNCTION id LPAREN tyfields RPAREN COLON id EQ exp {fundec {fun_name = $2; params = $4; result = Some $7; body = $9; fun_pos = getPos() }}


expseq : 
  | expList { $1 }
  | exp { [$1] }
  | { [] }

/*parenthesis : 
  | LPAREN exp RPAREN {} */
