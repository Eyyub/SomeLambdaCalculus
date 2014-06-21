open Term

exception Idx_Not_found of int
exception Name_Not_found of string

type naming_context = (string * int * term) list

val add_in_naming_context : string -> term -> naming_context -> naming_context
val find_index : int -> int -> naming_context -> term
val to_debruijn_term : term -> naming_context -> term
