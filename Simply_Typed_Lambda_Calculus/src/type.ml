type ty =
  | TyBool (* Bool : true of false *)
  | TyUnit (* Unit : unit *) 
  | TyArrow of ty * ty (* Arrow : T->T *)
  | TyTuple of ty list (* Product : T * T *)
  | TyRecord of (string * ty) list

type ty_context = ty list

let rec print_ty = function
  | TyBool -> print_string "Bool"
  | TyUnit -> print_string "Unit"
  | TyArrow (l, r) -> print_ty l; print_string " -> "; print_ty r
  | TyTuple l -> 
      let x, xs = List.hd l, List.tl l in 
      print_string "{"; print_ty x; List.iter (fun x -> print_string ", "; print_ty x) xs; print_string "}"
  | TyRecord l -> 
      let (f, x), xs = List.hd l, List.tl l in 
      print_string "{"; Printf.printf "%s := " f; print_ty x; List.iter (fun (f, ty) -> print_string ", "; Printf.printf "%s := " f ; print_ty ty) xs; print_string "}"

let rec string_of_ty = function
  | TyBool -> "Bool"
  | TyUnit -> "Unit"
  | TyArrow (l, r) -> (string_of_ty l) ^  " -> " ^ (string_of_ty r)
  | TyTuple [] -> "{}"
  | TyTuple l -> 
    let buf = Buffer.create 128 in
      let x, xs = List.hd l, List.tl l in
      let _ = List.iter (fun x -> Buffer.add_string buf (", " ^ string_of_ty x)) xs in
      "{" ^ string_of_ty x ^ Buffer.contents buf  ^ "}"
  | TyRecord l -> 
    let buf = Buffer.create 128 in
      let (f, x), xs = List.hd l, List.tl l in
      let _ = List.iter (fun (f, ty) -> Buffer.add_string buf (", " ^ f ^ " := " ^ string_of_ty ty)) xs in
      "{" ^ f ^ " := " ^ string_of_ty x ^ Buffer.contents buf  ^ "}"
