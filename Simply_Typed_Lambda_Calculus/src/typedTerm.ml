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
  | LetIn  of string * ty_term * ty_term
  | Seq    of ty_term * ty_term
  | Tuple  of ty_term list
  | Proj   of ty_term * int

let rec typeof ctx = function
  | True | False -> TyBool
  | Unit -> TyUnit
  | Var (_, idx) ->
     (let type_from_context n = List.nth ctx n in
      try type_from_context idx with _ -> raise (TypingError "Type of var is unknown."))
  | Abs ((_, ty), t) -> TyArrow(ty, typeof (ty :: ctx) t)
  | App (t1, t2) ->
     let ty1, ty2 = typeof ctx t1, typeof ctx t2 in
     (match ty1 with
     | TyArrow (p, r) -> (* Type checking *)
	if p = ty2 then r else raise (TypingError ("Type mismatch, expected " ^ string_of_ty p ^ " instead of " ^ string_of_ty ty2 ^ "."))
     | _ -> raise (TypingError "Type Arrow expected."))
  | If (c, t, f) ->
     if typeof ctx c = TyBool then
       (let ty_t, ty_f = typeof ctx t, typeof ctx f in
	if ty_t <> ty_f then
	  raise (TypingError "If-bodys must have the same type T.")
	else ty_t)
     else raise (TypingError "If condition must have type Bool.")
  | LetIn (_, t, t_in) -> typeof (typeof ctx t :: ctx) t_in
  | Seq (t1, t2) -> 
    if typeof ctx t1 = TyUnit then typeof ctx t2
    else raise (TypingError "Left seq side must have type unit.")
  | Tuple l -> TyTuple (List.map (typeof ctx) l)
  | Proj (t, n) ->
    match  t, typeof ctx t with 
    | Tuple [], _ -> raise (TypingError "Can't project on empty tuple")
    | Proj _, TyTuple lt -> List.nth lt n 
    | Tuple l, TyTuple lt -> 
        if n >= 0 &&  List.length lt <= n then raise (TypingError (string_of_ty (TyTuple lt) ^ " is a " ^ string_of_int (List.length lt) ^ "-tuple, can't project at index " ^ string_of_int n ^ "."))
	else typeof ctx (List.nth l n)
    | _ -> raise (TypingError "Can only project on tuple type.")

let rec typecheck_ty_terms ctx = function
  | [] -> ()
  | x :: xs -> let _ = typeof ctx x in typecheck_ty_terms ctx xs
