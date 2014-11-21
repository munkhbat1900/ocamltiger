exception Not_found

type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
	   | AssignStm of id * exp
	   | PrintStm of exp list
	      
and exp = IdExp of id
	  | NumExp of int
	  | OpExp of exp * binop * exp
	  | EseqExp of stm * exp

let prog = CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a"; OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

let rec lookup env id1 = 
  match env with
      [] -> Printf.printf "there is no identifier %s\n" id1; raise Not_found
    | (id2, value) :: tl -> if id2 = id1 then value else lookup tl id1

let interp stm = 
  let rec interpStm stm env = 
    match stm with
	CompoundStm(stm1, stm2) -> interpStm stm2 (interpStm stm1 env)
      | AssignStm(id1, exp1) -> let (new_env, value) = interpExp exp1 env
				in (id1, value) :: new_env
      | PrintStm(expList) -> let new_env = interpExpList expList env
			     in Printf.printf "¥n"; new_env
  and interpExp exp env = 
    match exp with
	IdExp(id1) -> (env, (lookup env id1))
      | NumExp(num) -> (env, num)
      | OpExp(exp1, binop1, exp2) -> 
	let (new_env1, value1) = interpExp exp1 env in
	let (new_env2, value2) = interpExp exp2 new_env1 in
	begin
	  match binop1 with 
	      Plus -> (new_env2, value1 + value2)
	    | Minus -> (new_env2, value1 - value2)
	    | Times -> (new_env2, value1 * value2)
	    | Div -> (new_env2, value1 / value2)
	end
      | EseqExp(stm1, exp1) -> interpExp exp1 (interpStm stm1 env)
  and interpExpList expList env = 
    match expList with
	[] -> env
      | hd :: tl -> let (new_env, value) = interpExp hd env in
		    Printf.printf "%d " value;
		    interpExpList tl new_env
  in
  interpStm stm []
      
let a = interp prog


let max m n = if m > n then m else n
    
let maxargs stm = 
  let rec max_stm n stm = 
    match stm with 
	CompoundStm(stm1, stm2) -> max (max_stm n stm1) (max_stm n stm2)
      | AssignStm(id1, exp1) -> max_exp n exp1
      | PrintStm(expList) -> max_expList n 0 expList
  and max_exp n exp = 
    match exp with
	NumExp(num) -> n
      | IdExp(id1) -> n
      | OpExp(exp1, binop1, exp2) -> max (max_exp n exp1) (max_exp n exp2)
      | EseqExp(stm, exp1) -> max (max_stm n stm) (max_exp n exp1)
  and max_expList n m expList = 
    match expList with
	[] -> max n m
      | hd :: tl -> max_expList n (m + 1) tl
  in 
  max_stm 0 stm
    
    
let max_arg = maxargs prog
