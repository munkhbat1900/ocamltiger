(* $Id: print.ml,v 1.1 2012/03/23 03:23:53 ksk Exp $ *)
(* Common functions for pretty printers (PP) *)
(* Copyright (C) 2005-2012 Keisuke Nakano. All rights reserved.*)

open Format

(* PP for basic types *)
let pp_unit fmtr () = pp_print_string fmtr "()"
let pp_bool = pp_print_bool
let pp_int = pp_print_int
let pp_float = pp_print_float
let pp_char fmtr = fprintf fmtr "%C"
let pp_string fmtr = fprintf fmtr "%S"

(* PP for lists with a specified separation *)
let pp_list_with sep pp_a fmtr = function
  | [] -> ()
  | x::xs ->
      pp_a fmtr x;
      List.iter (fprintf fmtr "%s@;%a" sep pp_a) xs

(* PP for lists *)
let pp_list pp_a fmtr xs =
  fprintf fmtr "@[<1>[%a]@]" (pp_list_with ";" pp_a) xs

(* PP for arrays *)
let pp_array pp_a fmtr xs =
  fprintf fmtr "@[<2>[|";
  let len = Array.length xs in
  if len > 0 then begin
    pp_a fmtr xs.(0);
    for i=1 to len-1 do fprintf fmtr ";@;%a" pp_a xs.(i) done end;
  fprintf fmtr "|]@]"

(* Heterogeneous list based on existing type variable emulation *)
type pp_poly = { pp_poly: 'b. 'b pp_neg -> 'b }
and 'b pp_neg = { pp_neg: 'a. (formatter -> 'a -> unit) -> 'a -> 'b }
let pp_poly pp_a x = { pp_poly = fun k -> k.pp_neg pp_a x }
let apply_pp_poly fmtr p = p.pp_poly { pp_neg = fun pp_a -> pp_a fmtr }

(* PP for heterogeneous lists *)
let pp_poly_list fmtr = function
  | [] -> ()
  | p::ps ->
      fprintf fmtr "@[<1>(%a" apply_pp_poly p;
      List.iter (fprintf fmtr ",@;%a" apply_pp_poly) ps;
      fprintf fmtr ")@]"

(* PP for tuples *)
let pp_tuple (make_pps:'a -> pp_poly list) fmtr x = pp_poly_list fmtr (make_pps x)

(* PP for variants *)
let pp_variant (make_cps:'a -> (string * pp_poly list)) fmtr x =
  let cname, ps = make_cps x in
  fprintf fmtr "%s%a" cname pp_poly_list ps

(* PP for options *)
let pp_option pp_a =
  pp_variant (function
                | None -> "None", []
                | Some x -> "Some", [pp_poly pp_a x])

(* PP for records *)
let pp_record (make_pp_fields:'a -> (string * pp_poly) list) fmtr x =
  let apply_pp_field fmtr (f,p) = fprintf fmtr "@[<2>%s = @,%a@]" f apply_pp_poly p in
  fprintf fmtr "@[<1>{";
  begin match make_pp_fields x with
    | [] -> ()
    | fp::fps ->
        apply_pp_field fmtr fp;
        List.iter (fprintf fmtr ";@;%a" apply_pp_field) fps end;
  fprintf fmtr "}@]"

(* PP for Sets *)
module Set = struct
  module Make(Ord:Set.OrderedType) : sig
    include Set.S with type elt = Ord.t
    val pp_t : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
  end = struct
    module S = Set.Make(Ord)
    include S
    let pp_t pp_elt fmtr set =
      fprintf fmtr "@[<1>{";
      ignore (S.fold (fun elt is_fst ->
                        if is_fst then pp_elt fmtr elt
                        else fprintf fmtr ",@;%a" pp_elt elt;
                        false) set true);
      fprintf fmtr "}@]"
  end
end

(* PP for Maps *)
module Map = struct
  module type S = sig
    include Map.S
    val find_some : key -> 'a t -> 'a option
    val pp_t :
      (Format.formatter -> key -> unit) -> (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a t -> unit
  end
  module type OrderedType = Map.OrderedType
  module Make(Ord:OrderedType) : S with type key = Ord.t = struct
    module M = Map.Make(Ord)
    include M
    let find_some key t = try Some(M.find key t) with Not_found -> None
    let pp_t pp_key pp_a fmtr map =
      fprintf fmtr "@[<1>{";
      let pp_each key fmtr v = fprintf fmtr "@[<2>%a => @,%a@]" pp_key key pp_a v in
      ignore (M.fold (fun key v is_fst ->
                        if is_fst then pp_each key fmtr v
                        else fprintf fmtr ";@;%a" (pp_each key) v;
                        false) map true);
      fprintf fmtr "}@]"
  end
end
