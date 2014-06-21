{
  open Parser
  exception LexingError of string
}

let lambda = ['\\' 'λ']
let alpha = ['a' - 'z' 'A'- 'Z']
let word = alpha+
let empty = [' ' '\t' '\n']

rule lexer = parse
  | '#' [^'\n']* '\n'? { lexer lexbuf }
  | eof                { TEof         }
  | empty+             { lexer lexbuf }
  | '('                { TLPA         }
  | ')'                { TRPA         }
  | ';'                { TSep         }
  | ":="               { TAssign      }
  | lambda             { TLambda      }
  | '.'                { TDot         }
  | word as w          { TWord w      }
  | _ as err           { raise (LexingError ("Unkown character \'" ^ Char.escaped err ^ "\'.\n")) }
