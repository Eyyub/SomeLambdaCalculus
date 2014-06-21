open Term
open Debruijn

val print_term : term -> unit
val eval : term list -> naming_context -> naming_context
