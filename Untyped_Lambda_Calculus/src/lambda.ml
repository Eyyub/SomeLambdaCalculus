open Term
open Debruijn

exception NoRuleApplies of term

let rec print_term_name = function
  | Var (v, _) -> Printf.printf "(%s)" v
  | Abs (x, t) -> Printf.printf "(λ%s." x; print_term_name t; print_string ")"
  | App (t1, t2) -> Printf.printf "(("; print_term_name t1; print_term_name t2; print_string ")"
  | Assign (k, v) -> Printf.printf "%s := " k; print_term_name v

let rec print_term_index = function
  | Var (_, x) -> Printf.printf "(%d)" x
  | Abs (_, t) -> Printf.printf "(λ."; print_term_index t; print_string ")"
  | App (t1, t2) -> Printf.printf "(("; print_term_index t1; print_term_index t2; print_string ")"
  | Assign (k, v) -> Printf.printf "%s := " k; print_term_index v

let print_term t =
  print_endline "Printing term with name :";
  print_term_name t;
  print_endline "\n=========End=============";
  print_endline "Printing term with debruijn index :";
  print_term_index t;
  print_endline "\n=========End============="
  
let isval = function
  | Abs _ -> true
  | _ -> false

let rec shift c d = function
  | Var (x, k) -> if k < c then Var (x, k) else Var (x, (k + d))
  | Abs (x, t) -> Abs (x, (shift (succ c) d t))
  | App (t1, t2) -> App (shift c d t1, shift c d t2)
  | Assign _ -> failwith "Assignation in expr.\n"

let rec substitution j s t ctx =
    match t with
    | Var (z, k) ->
        if k > j then
	  let t' = 
	    (try
		find_index k (succ j) ctx
	      with Idx_Not_found _ -> failwith "Unknown name value") in
	  shift 0 1 t';
	else if j = k then s 
	else t
    | Abs (x, t1) -> Abs (x, (substitution (succ j) (shift 0 1 s) t1 ctx))
    | App (t1, t2) -> App (substitution j s t1 ctx, substitution j s t2 ctx)
    | Assign _ -> failwith "Assignation in expr.\n"

let beta_reduction s t ctx =
  let shifted_s = shift 0 1 s in
  let beta_redex = substitution 0 shifted_s t ctx in
  (shift 0 (-1) beta_redex)

let rec eval' e ctx =
  let e  = 
    match e with
    | App (Var (_, k), t2) -> App (find_index k 0 ctx, t2)
    | App (t1, Var (_, k)) -> App (t1, find_index k 0 ctx)
    | _ -> e
  in
    match e with
    | App (t1, t2) when not (isval t1) -> eval' (App (eval' t1 ctx, t2)) ctx
    | App (v1, t2) when not (isval t2) -> eval' (App (v1, eval' t2 ctx)) ctx
    | App (Abs (_, t), s) when isval s -> eval' (beta_reduction s t ctx) ctx
    | _ -> raise (NoRuleApplies e) (* No Rule Applies *)

let rec print_context = function
  | [] -> print_string "End of ctx.\n"
  | (k, idx, v) :: xs -> 
     Printf.printf "k %s idx %d v : " k idx; 
     print_term v;
     print_context xs

let rec eval l ctx =
  match l with
  | [] -> ctx
  | Assign (k, v) :: xs -> eval xs (add_in_naming_context k (to_debruijn_term v ctx) ctx);
  | x :: xs -> 
     print_endline "Before evaluation :";
     print_term (to_debruijn_term x ctx);
     print_endline "===================";
     print_endline "After evaluation ;";
     print_term (try eval' (to_debruijn_term x ctx) ctx 
                   with NoRuleApplies e -> e);
     print_endline "===================";
     eval xs ctx
