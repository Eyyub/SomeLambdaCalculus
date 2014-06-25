type ty =
  | Bool (* Bool : true of false *)
  | Arrow of ty * ty (* Arrow : T->T *)
