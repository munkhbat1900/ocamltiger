type pos = int and symbol = Symbol.symbol

type var = SimpleVar of pos
	   | FieldVar of var * pos
	   | SubscriptVar of var * pos

and exp = VarExp of var
	  | NilExp
	  | IntExp of int
	  | StringExp of string * pos
	  | CallExp of {func: symbol, args: exp list, pos: pos}
	  | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
	  | RecordExp of {fields: (symbol * exp * pos) list, typ: symbol, pos: pos}
	  | SeqExp of (exp * pos) list
	  | AssignExp of (var: var, exp: exp, pos: pos)
	  | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
	  | WhileExp
