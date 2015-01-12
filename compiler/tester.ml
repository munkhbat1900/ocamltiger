let ic = open_in "../testcases/test8.tig";;
let s = Parser1.program Lexer.token (Lexing.from_channel ic);;
Printf.printf "\n PRETTY PRINT \n\n";
Format.printf "exp = %a@." Pp.pp_exp s;;
