type term =
   | Var    of (string * int)
   | Abs    of (string * term)
   | App    of term * term
   | Assign of string * term
