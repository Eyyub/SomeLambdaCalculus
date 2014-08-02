type term =
  | True
  | False
  | Unit
  | Var    of (string * int) (* x will be 0 (by exemple)*)
  | Abs    of (string * term) (* \x.y, as abs is a new depth we don't care about have its index*)
  | App    of term * term
  | If     of term * term * term
  | Assign of string * term (* ugly ? when I match this pattern it looks like a hack,
			       according to theory, assignation is not a valid term *)
