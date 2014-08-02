open Term

exception Idx_Not_found of int
exception Name_Not_found of string

type naming_context = (string * int * term) list

let add_in_naming_context k v ctx = 
  match ctx with
  | [] -> [(k, 0, v)]
  | (_, idx, _) :: xs -> (k, succ idx, v) :: ctx

let find_index k j ctx =
  try
    let (_, _, t') = 
      List.find (fun (_, i, _) -> i = k - j) ctx in
    t'
  with Not_found -> raise (Idx_Not_found k)

let rec get_name_list = function
  | [] -> []
  | (name, _, _) :: xs -> name :: get_name_list xs

let find_name_index name ctx =
  let rec aux idx = function
    | [] -> raise (Name_Not_found name)
    | x :: xs -> if x = name then idx else aux (succ idx) xs
  in List.length ctx - aux 0 ctx

 let to_debruijn_term t ctx =
  let rec aux depth inner_ctx = function
    | True -> True
    | False -> False
    | Unit -> Unit
    | Var (x, _) when List.exists (fun (a, _, _) -> a = x) ctx ->
                    let (_, idx, _) = List.find (fun (a, _, _) -> a = x) ctx in 
		    Var (x, depth + idx)
    | Var (x, _) -> Var (x, depth - find_name_index x inner_ctx)
    | Abs (x, t) -> Abs (x, aux (succ depth) (x :: inner_ctx) t)
    | App (t1, t2) -> App (aux depth inner_ctx t1, aux depth inner_ctx t2)
    | If (c, t, f) -> If (aux depth inner_ctx c, aux depth inner_ctx t, aux depth inner_ctx f)
    | Assign _ -> failwith "Assignation in expr."
  in aux 0 [] t
