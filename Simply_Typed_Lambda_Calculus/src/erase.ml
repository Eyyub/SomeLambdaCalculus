let rec erase = function
  | TypedTerm.True -> Term.True
  | TypedTerm.False -> Term.False
  | TypedTerm.Unit -> Term.Unit
  | TypedTerm.Var (name, idx) -> Term.Var (name, idx)
  | TypedTerm.Abs ((x, _), t) -> Term.Abs (x, erase t)
  | TypedTerm.App (t1, t2) -> Term.App (erase t1, erase t2)
  | TypedTerm.If (c, t, f) -> Term.If (erase c, erase t, erase f)
  | TypedTerm.LetIn (n, t, t_in) -> Term.LetIn(n, erase t, erase t_in)
  | TypedTerm.Seq (t1, t2) -> Term.Seq (erase t1, erase t2)
  | TypedTerm.Tuple l -> Term.Tuple (List.map erase l)
  | TypedTerm.Record l -> Term.Record (List.map (fun (f, v) -> f, erase v) l)
  | TypedTerm.Proj (t, n) -> Term.Proj (erase t, n) (* obliger de matcher sur un tuple en sachant qu'apres le typechercking on sait que c'est obligatoirement un tuple ... *) 

let erase_all ty_e =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (erase x :: acc) xs
  in aux [] ty_e
