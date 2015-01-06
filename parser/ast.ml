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

and field = {field_name: symbol; field_escape: bool ref; field_typ: symbol; field_pos: pos}
and fundec = {fun_name: symbol; params: field list; result: (symbol * pos) option; fun_body: exp; fun_pos: pos}

and call_exp = {func: symbol; args: exp list; call_pos: pos}
and op_exp = {left: exp; oper: oper; right: exp; op_pos: pos}
and record_exp = {fields: (symbol * exp * pos) list; typ: symbol; record_pos: pos}
and assign_exp = {assign_var: var; exp: exp; assign_pos: pos}
and if_exp = {if_test: exp; then': exp; else': exp option; if_pos: pos}
and while_exp = {while_test: exp; while_body: exp; while_pos: pos}
and for_exp = {for_var: symbol; for_escape: bool ref; lo: exp; hi: exp; for_body: exp; for_pos: pos}
and let_exp = {decs: dec list; let_body:exp; let_pos: pos}
and array_exp = {arr_typ: symbol; size: exp; init: exp; array_pos: pos}
and var_dec = {var_name: symbol; var_escape: bool ref; var_typ: (symbol * pos) option; init_value: exp; var_pos: pos}
and type_dec = {typ_name: symbol; ty: ty; type_pos: pos}



(* pretty printing *)

let indentPrintf indent fmt =
  let rec printIndent indent = 
    match indent with
0 -> ()
      | i -> (print_string "   "; printIndent (i-1))
  in (printIndent indent; Printf.printf fmt)

let getOperName oper = 
  match oper with
      PlusOp   -> "PlusOp"
    | MinusOp  -> "MinusOp"
    | TimesOp  -> "TimesOp"
    | DivideOp -> "DivideOp"
    | EqOp     -> "EqOp"
    | NeqOp    -> "NeqOp"
    | LtOp     -> "LtOp"
    | LeOp     -> "LeOp"
    | GtOp     -> "GtOp"
    | GeOp     -> "GeOp"
      
let printAst ast = 
  let rec printVar indent var = 
    match var with
	SimpleVar(sym, pos) -> 
	  indentPrintf indent "SimpleVar: %s \n" (Symbol.name sym)
      | FieldVar(var, sym, pos) ->
	indentPrintf indent "FieldVar: ";
	printVar (indent + 1) var;
	indentPrintf (indent + 1) "%s" (Symbol.name sym)
      | SubscriptVar(var, exp, pos) ->
	indentPrintf indent "SubscriptVar: ";
	printVar (indent + 1) var;
	printExp (indent + 1) exp;
  and printDec indent dec = 
    let printReturnType returnType = 
      match returnType with
	  None -> indentPrintf (indent + 1) "NONE\n"
	| Some res -> indentPrintf (indent + 1) "SOME(%s)\n" (Symbol.name (fst res))
    in
    match dec with 
	FunctionDec(fundecList) ->
	  let printParams indent param = 
	    indentPrintf indent "%s:%s\n" (Symbol.name param.field_name) (Symbol.name param.field_typ)
	  in
	  let printFundec indent fundec = 
	    indentPrintf indent "%s\n" (Symbol.name fundec.fun_name);
	    List.iter (printParams (indent + 1)) fundec.params;
	    printReturnType fundec.result;
	    printExp (indent + 2) fundec.fun_body
	  in
	  indentPrintf indent "FunctionDec: \n";
	  List.iter (printFundec (indent + 1)) fundecList
      | VarDec(varDec) -> 
	indentPrintf indent "VarDec: %s\n" (Symbol.name varDec.var_name);
	printReturnType varDec.var_typ;
        printExp (indent + 1) varDec.init_value
      | TypeDec(typeDecList) ->
	let printTypeDec indent typedec =
          indentPrintf indent "%s:\n" (Symbol.name typedec.typ_name); printTy (indent + 1) typedec.ty
        in
        indentPrintf indent "TypeDec:\n";
        List.iter (printTypeDec (indent + 1)) typeDecList
  and printTy indent ty = 
    match ty with
	NameTy(sym, pos) -> 
	  indentPrintf indent "NameTy: %s\n" (Symbol.name sym)
      | RecordTy(fieldList) -> 
	indentPrintf indent "fildList: \n";
	let printField indent field = 
	  indentPrintf indent "%s:%s\n" (Symbol.name field.field_name) (Symbol.name field.field_typ)
	in
	List.iter (printField (indent + 1)) fieldList
      | ArrayTy(sym, pos) ->
	indentPrintf indent "ArrayTy: %s\n" (Symbol.name sym)
  and printExp indent exp = 
    match exp with
	VarExp(variable) -> printVar indent variable
      | NilExp -> indentPrintf indent "NilExp\n"
      | IntExp(v) -> indentPrintf indent "IntExp: %d\n" v
      | StringExp(str, pos) -> indentPrintf indent "StringExp: %s\n" str
      | CallExp(callExp) -> 
	indentPrintf indent "CallExp: %s \n" (Symbol.name callExp.func);
	List.iter (printExp (indent + 1)) callExp.args
	  
      | LetExp(letExp) -> 
	indentPrintf indent "LetExp:\n";
	List.iter (printDec (indent + 1)) letExp.decs;
	indentPrintf indent "IN \n";
	printExp (indent + 1) letExp.let_body
	  
      | OpExp(opExp) ->
	indentPrintf indent "OpExp:\n";
	printExp (indent + 1) opExp.left;
	indentPrintf (indent + 1) "%s" (getOperName opExp.oper);
	printExp (indent + 1) opExp.right
      | RecordExp(recordExp) ->
	
	let printField indent (sym, exp, pos) =
          indentPrintf indent "%s:\n" (Symbol.name sym); 
	  printExp (indent + 1) exp
        in
	indentPrintf indent "RecordExp: \n";
	indentPrintf (indent + 1) "%s\n" (Symbol.name recordExp.typ);
	List.iter (printField (indent + 2)) recordExp.fields
      | SeqExp(seqExpList) ->
	let getExpList seqExpList = 
	  let rec buildList list newList = 
	    match list with
		[] -> []
	      | (exp, pos) :: [] -> newList @ [exp]
	      | (exp, pos) :: tl -> buildList tl (newList @ [exp])
	  in
	  buildList seqExpList []
	in
	indentPrintf indent "SeqExp: \n";
	List.iter (printExp (indent + 1)) (getExpList seqExpList)
      | AssignExp(assignExp) ->
	indentPrintf indent "AssignExp: \n";
	printVar (indent + 1) assignExp.assign_var;
	printExp (indent + 1) assignExp.exp
      | IfExp(ifExp) ->
	indentPrintf indent "IfExp: \n";
	printExp (indent + 1) ifExp.if_test;
	indentPrintf (indent + 1) "then: \n";
	printExp (indent + 2) ifExp.then';
	begin match ifExp.else' with
	    None -> ()
	  | Some e -> 
	    indentPrintf (indent + 1) "else: \n";
	    printExp (indent + 2) e
	end
      | WhileExp (whileExp) ->
	indentPrintf indent "WhileExp: \n";
	printExp (indent + 1) whileExp.while_body
      | ForExp(forExp) -> 
	indentPrintf indent "ForExp: \n";
	indentPrintf (indent + 1) "%s=" (Symbol.name forExp.for_var);
	printExp (indent + 1) forExp.lo;
	indentPrintf (indent + 1) "to=";
	printExp (indent + 1) forExp.hi;
	Printf.printf "\n";
	printExp (indent + 2) forExp.for_body
      | BreakExp(pos) -> 
	indentPrintf indent "BreakExp: \n";
      | ArrayExp(arrayExp) ->
	indentPrintf indent "ArrayExp: \n";
	indentPrintf (indent + 1) "%s\n" (Symbol.name arrayExp.arr_typ);
	indentPrintf (indent + 1) "size:\n";
	printExp (indent + 1) arrayExp.size;
	
  in printExp 0 ast
