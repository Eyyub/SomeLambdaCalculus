open Type

type ty_term =
  | True
  | False
  | Var of string * int
  | Abs of (string * ty) * term
  | App of term * term
  | Assign of string * term
