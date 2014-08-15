type term =
  | True
  | False
  | Unit
  | Var    of (string * int) (* x will be 0 (by exemple)*)
  | Abs    of (string * term) (* \x.y, as abs is a new depth we don't care about have its index*)
  | App    of term * term
  | If     of term * term * term
  | LetIn  of string * term * term
  | Seq    of term * term
  | Tuple  of term list
  | Proj   of term * int
