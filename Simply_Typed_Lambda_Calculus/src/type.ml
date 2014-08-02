type ty =
  | TyBool (* Bool : true of false *)
  | TyUnit (* Unit : unit *) 
  | TyArrow of ty * ty (* Arrow : T->T *)

type ty_context = ty list
