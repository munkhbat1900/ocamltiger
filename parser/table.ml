module SymbolMap = Map.Make(
  struct
    type t = Symbol.symbol
    let compare a b = Symbol.compare a b
  end
)

(*type key = SymbolMap.t*)
type 'a table = 'a SymbolMap.t

let empty = SymbolMap.empty

let enter symbol value m = 
  SymbolMap.add symbol value m

let look symbol m = 
  try
    Some (SymbolMap.find symbol m)
  with Not_found ->
    None
