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
	if p = ty2 then r else raise (TypingError "Type mismatch.")
     | _ -> raise (TypingError "Type Arrow expected."))
  | If (c, t, f) ->
     if typeof ctx c = TyBool then
       (let ty_t, ty_f = typeof ctx t, typeof ctx f in
	if ty_t <> ty_f then
	  raise (TypingError "If-bodys must have the same type T.")
	else ty_t)
     else raise (TypingError "If condition must have type Bool.")
  | Assign _ -> failwith "Error : assign in expr. (typeof)"

let rec typecheck_ty_terms ctx = function
  | [] -> ()
  | Assign (_, t) :: xs -> typecheck_ty_terms (typeof ctx t :: ctx) xs
  | x :: xs -> let _ = typeof ctx x in typecheck_ty_terms ctx xs
