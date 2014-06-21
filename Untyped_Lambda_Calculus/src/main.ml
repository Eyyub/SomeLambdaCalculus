open Term
open Lambda
open Debruijn

let _ = 
  let e = (Parser.prgm Lexer.lexer (Lexing.from_channel (open_in Sys.argv.(1))))
  in 
  print_string "Begin evaluation : \n"; 
  ignore (eval e []);
  print_string "End of evaluation.";
  print_newline ()

