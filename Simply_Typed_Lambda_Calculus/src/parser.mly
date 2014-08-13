%{

%}

%token TLambda (* λ or \ *)
%token TDot (* . *)
%token TIf TThen TElse (* if <bool> then <T> else <T> *)
%token TBool TTrue TFalse (* Bool : true | false*)
%token TTyUnit TValUnit (* Unit : unit *)
%token TArrow (* -> *)
%token <string> TWord
%token TSep (* ; *)
%token TColon (* : *)
%token TLet TAssign TIn (* let n := t in t_in *)
%token TLPA TRPA (* () *)
%token TEof
%right TArrow

%start <TypedTerm.ty_term list> prgm

%%

prgm:
| s = sequence TEof { s }

sequence:
| t = term TSep { [t] }
| t = term TSep s = sequence { t :: s }

term:
| b   = bool_       { b   }
| u   = unit_       { u   }
| var = variable    { var }
| abs = abstraction { abs }
| app = application { app }
| if_ = ifthenelse  { if_ }
| li  = let_in      { li  }
(*| assign = assignation { assign }*)

bool_:
| TTrue  { TypedTerm.True  }
| TFalse { TypedTerm.False }

unit_:
| TValUnit { TypedTerm.Unit }

variable:
| v = TWord { TypedTerm.Var (v, 0) }

abstraction:
| TLPA TLambda v = TWord TColon ty = type_ TDot t = seq_ TRPA { TypedTerm.Abs ((v, ty), t) }

type_:
| TLPA ty = type_ TRPA { ty }
| TBool { Type.TyBool }
| TTyUnit { Type.TyUnit }
| ty1 = type_ TArrow ty2 = type_ { Type.TyArrow (ty1, ty2) }

application:
| TLPA t1 = term t2 = term TRPA { TypedTerm.App (t1, t2) }

ifthenelse:
| TLPA TIf cond = term TThen true_body = term TElse false_body = term TRPA { TypedTerm.If (cond, true_body, false_body) }

let_in:
| TLPA TLet n = TWord TAssign t = term TIn t_in = term TRPA { TypedTerm.LetIn(n, t, t_in) }

seq_:
| TLPA t = term TRPA  { t }
| TLPA t = term TRPA s = seq_ { TypedTerm.Seq (t, s) }

