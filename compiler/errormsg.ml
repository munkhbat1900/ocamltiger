module ERRORMSG : sig
  val anyErrors : bool ref
  val fileName : string ref
  val lineNum : int ref
  val linePos : int list ref
  val error : int -> string -> unit
  exception Error
  val impossible : string -> 'a  (*raises error*)
  val reset : unit -> unit
end
= struct
  let anyErrors = ref false
  let fileName = ref ""
  let lineNum = ref 1
  let linePos = ref [1]

  let reset () = 
    anyErrors := false;

  exception Error

  let error pos msg = 
    let rec look ls n = 
      match ls with 
	  [] -> Printf.printf "0.0"
	| a::rest -> 
	  if a < pos then Printf.printf ": %s . %s" (string_of_int n) (string_of_int (pos-a))
	  else
	    look rest (n - 1)
    in
    begin
      anyErrors := true;
      look !linePos !lineNum;
      Printf.printf ":";
      Printf.printf "%s" msg;
      Printf.printf "\n";
    end
  let impossible msg = 
    Printf.printf "Error: Compiler bug: %s\n" msg;
    raise Error
end
