{
  open Parser
  exception LexingError of string
}

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
  | ':'                { TColon       }
(*  | ":="               { TAssign      }*)
  | "\\" | "λ"         { TLambda      }
  | '.'                { TDot         }
  | "if"               { TIf          }
  | "then"             { TThen        }
  | "else"             { TElse        }
  | "Bool"             { TBool        }
  | "true"             { TTrue        }
  | "false"            { TFalse       }
  | "Unit"             { TTyUnit      }
  | "unit"             { TValUnit     }
  | "->"               { TArrow       }
  | word as w          { TWord w      }
  | _ as err           { raise (LexingError ("Unkown character \'" ^ Char.escaped err ^ "\'.\n")) }
