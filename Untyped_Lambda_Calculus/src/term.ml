type term =
  | Var    of (string * int)
  | Abs    of (string * term)
  | App    of term * term
  | Assign of string * term (* ugly ? when I match this pattern it looks like a hack,
			       according to theory, assignation is not a valid term *)
