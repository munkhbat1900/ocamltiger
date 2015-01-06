type symbol = string * int

let hashtable : (string, int) Hashtbl.t = Hashtbl.create 128
let nextsym = ref 0

let symbol name = 
  try 
    let value = Hashtbl.find hashtable name in (name, value)
  with Not_found ->
    incr nextsym;
    Hashtbl.add hashtable name !nextsym;
    (name, !nextsym)

let name symbol = fst symbol

let compare (_, n) (_, m) = 
  Pervasives.compare n m
