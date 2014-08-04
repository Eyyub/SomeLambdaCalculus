open Type

exception TypingError of string

type ty_term =
  | True
  | False
  | Unit
  | Var    of string * int
  | Abs    of (string * ty) * ty_term
  | App    of ty_term * ty_term
  | If     of ty_term * ty_term * ty_term
  | Assign of string * ty_term
  | Seq    of ty_term * ty_term

val typecheck_ty_terms : ty_context -> ty_term list -> unit

