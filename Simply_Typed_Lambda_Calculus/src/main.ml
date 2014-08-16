
let _ = 
  let ty_e = (Parser.prgm Lexer.lexer (Lexing.from_channel (open_in Sys.argv.(1)))) in
  let () = 
    (try TypedTerm.typecheck_ty_terms [] ty_e
     with TypedTerm.TypingError err -> Printf.printf "TypeError : %s\n" err; exit 1) in
  let e = Erase.erase_all ty_e in 
  print_endline "Begin evaluation :";
  ignore (Lambda.eval e []);
(*  Lambda.print_term (List.hd e);*)
  print_endline "End of evaluation."


(*let pretty_ty s = Type.print_ty (Parser.ty Lexer.lexer (Lexing.from_string s))*)

(*let _ =
  print_endline "BEGIN TYPE TEST.";
  pretty_ty "Unit->Bool"; print_newline ();
  pretty_ty "{{Bool, Bool->Unit}, Unit}"; print_newline ();
  print_endline "END TYPE TEST."
*)
