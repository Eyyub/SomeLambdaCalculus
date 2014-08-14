{
  open Parser
  exception LexingError of string
}

let alpha = ['a' - 'z' 'A'- 'Z']
let word = alpha+
let digit = ['0' - '9']
let number = digit+
let empty = [' ' '\t' '\n']

rule lexer = parse
  | '#' [^'\n']* '\n'? { lexer lexbuf }
  | eof                { TEof         }
  | empty+             { lexer lexbuf }
  | '('                { TLPA         }
  | ')'                { TRPA         }
  | '{'                { TLCB         }
  | '}'                { TRCB         }
  | ';'                { TSep         }
  | ':'                { TColon       }
  | ":="               { TAssign      }
  | "\\" | "λ"         { TLambda      }
  | '.'                { TDot         }
  | ','                { TComma       }
  | "if"               { TIf          }
  | "then"             { TThen        }
  | "else"             { TElse        }
  | "let"              { TLet         }
  | "in"               { TIn          }
  | "Bool"             { TBool        }
  | "true"             { TTrue        }
  | "false"            { TFalse       }
  | "Unit"             { TTyUnit      }
  | "unit"             { TValUnit     }
  | "->"               { TArrow       }
(*  | "*"                { TStar        }*)
  | word as w          { TWord w      }
  | number as n        { TNumber (int_of_string n)}
  | _ as err           { raise (LexingError ("Unkown character \'" ^ Char.escaped err ^ "\'.\n")) }
