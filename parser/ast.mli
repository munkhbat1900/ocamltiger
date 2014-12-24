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
and dec =
    FunctionDec of fundec list
  | VarDec of var_dec
  | TypeDec of type_dec list
and ty =
    NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos
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
and field = {
  field_name : symbol;
  field_escape : bool ref;
  field_typ : symbol;
  field_pos : pos;
}
and fundec = {
  fun_name : symbol;
  params : field list;
  result : (symbol * pos) option;
  func_body : exp;
  fun_pos : pos;
}
and call_exp = { func : symbol; args : exp list; call_pos : pos; }
and op_exp = { left : exp; oper : oper; right : exp; op_pos : pos; }
and record_exp = {
  fields : (symbol * exp * pos) list;
  typ : symbol;
  record_pos : pos;
}
and assign_exp = { assign_var : var; exp : exp; assign_pos : pos; }
and if_exp = {
  if_test : exp;
  then' : exp;
  else' : exp option;
  if_pos : pos;
}
and while_exp = { while_test : exp; while_body : exp; while_pos : pos; }
and for_exp = {
  for_var : symbol;
  for_escape : bool ref;
  lo : exp;
  hi : exp;
  for_body : exp;
  for_pos : pos;
}
and let_exp = { decs : dec list; body : exp; let_pos : pos; }
and array_exp = {
  arr_typ : symbol;
  size : exp;
  init : exp;
  array_pos : pos;
}
and var_dec = {
  var_name : symbol;
  var_escape : bool ref;
  var_typ : (symbol * pos) option;
  init_value : exp;
  var_pos : pos;
}
and type_dec = { typ_name : symbol; ty : ty; type_pos : pos; }
