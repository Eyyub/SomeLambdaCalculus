open Term
open Lambda
open Debruijn

let _ = 
  let e = (Parser.prgm Lexer.lexer (Lexing.from_channel (open_in Sys.argv.(1))))
  in 
  print_endline "Begin evaluation :"; 
  ignore (eval e []);
  print_endline "End of evaluation.";

