type ty =
  | Bool (* Bool : true of false *)
  | Arrow of ty * ty (* Arrow : T->T *)

type ty_context = ty list
