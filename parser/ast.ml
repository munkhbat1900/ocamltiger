type pos = int

type var = SimpleVar of pos
	   | FieldVar of var * pos
	   | SubscriptVar of var * pos

and exp = VarExp of var
	  | NilExp
	  | IntExp of int
	  | StringExp of string * pos
	  | CallExp of {func : }
