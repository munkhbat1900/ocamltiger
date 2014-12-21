type pos = int and symbol = Symbol.symbol

type var = SimpleVar of symbol *pos
	   | FieldVar of var * symbol * pos
	   | SubscriptVar of var * exp * pos

and exp = VarExp of var
	  | NilExp
	  | IntExp of int
	  | StringExp of string * pos
	  | CallExp of call_exp
	  | OpExp of op_exp
	  | RecordExp of record_exp
	  | SeqExp of (exp * pos) list
	  | AssignExp of assign_exp
	  | IfExp of if_exp
	  | WhileExp of while_exp
	  | ForExp of for_exp
	  | BreakExp of pos
	  | LetExp of let_exp
	  | ArrayExp of array_exp

and dec = FunctionDec of fundec list
	  | VarDec of var_dec
	  | TypeDec of type_dec list

and ty = NameTy of symbol * pos
	 | RecordTy of field list
	 | ArrayTy of symbol * pos

and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

and field = {name: symbol; escape: bool ref; typ: symbol; pos: pos}
and fundec = {name: symbol; params: field list; result: (symbol * pos) option; body: exp; pos: pos}

and call_exp = {func: symbol; args: exp list; pos: pos}
and op_exp = {left: exp; oper: oper; right: exp; pos: pos}
and record_exp = {fields: (symbol * exp * pos) list; typ: symbol; pos: pos}
and assign_exp = {var: var; exp: exp; pos: pos}
and if_exp = {test: exp; then': exp; else': exp option; pos: pos}
and while_exp = {test: exp; body: exp; pos: pos}
and for_exp = {var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp; pos: pos}
and let_exp = {decs: dec list; body:exp; pos: pos}
and array_exp = {typ: symbol; size: exp; init: exp; pos: pos}
and var_dec = {name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos}
and type_dec = {name: symbol; ty: ty; pos: pos}
