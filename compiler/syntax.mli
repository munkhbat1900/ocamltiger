type pos = int
and symbol = Symbol.symbol
type var =
    SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos
and exp =
    VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of symbol * exp list * pos
  | OpExp of exp * oper * exp * pos
  | RecordExp of (symbol * exp * pos) list * symbol * pos
  | SeqExp of (exp * pos) list
  | AssignExp of var * exp * pos
  | IfExp of exp * exp * exp option * pos
  | WhileExp of exp * exp * pos
  | ForExp of symbol * bool ref * exp * exp * exp * pos
  | BreakExp of pos
  | LetExp of dec list * exp * pos
  | ArrayExp of symbol * exp * exp * pos
and dec =
    FunctionDec of
      (symbol * field list * (symbol * pos) option * exp * pos) list
  | VarDec of symbol * bool ref * (symbol * pos) option * exp * pos
  | TypeDec of (symbol * ty * pos) list
and ty =
    NameTy of symbol * pos
  | RecordTy of (symbol * bool ref * symbol * pos) list
  | ArrayTy of symbol * pos
and field = symbol * bool ref * symbol * pos
and oper =
    PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
