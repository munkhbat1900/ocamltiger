type symbol = string * int
val hashtable : (string, int) Hashtbl.t
val nextsym : int ref
val symbol : string -> string * int
val name : 'a * 'b -> 'a
val compare : 'a * 'b -> 'c * 'b -> int
