type term =
  | True
  | False
  | Unit
  | Var    of (string * int)
  | Abs    of (string * term)
  | App    of term * term
  | If     of term * term * term
  | LetIn  of string * term * term
  | Seq    of term * term
  | Tuple  of term list
  | Proj   of term * int
