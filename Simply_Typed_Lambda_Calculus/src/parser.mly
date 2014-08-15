%{

%}

%token TLambda (* λ or \ *)
%token TDot (* . *)
%token TIf TThen TElse (* if <bool> then <T> else <T> *)
%token TBool TTrue TFalse (* Bool : true | false*)
%token TTyUnit TValUnit (* Unit : unit *)
%token TArrow (* -> *) 
(*%token TStar (* * *)*)
%token <string> TWord
%token <int> TNumber
%token TSep (* ; *)
%token TColon (* : *)
%token TLet TAssign TIn (* let n := t in t_in *)
%token TLPA TRPA (* () *)
%token TLCB TRCB (* {} *)
%token TComma (* , *)
%token TEof
%right TArrow


%start <TypedTerm.ty_term list> prgm
%start <Type.ty> ty

%%

prgm:
| s = sequence TEof { s }

ty:
| t = type_ TEof { t }

sequence:
| t = term TSep { [t] }
| t = term TSep s = sequence { t :: s }

term:
| b   = bool_       { b   }
| u   = unit_       { u   }
| tup = tuple       { tup }
| pro = projection  { pro }
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

tuple:
| TLCB TRCB { TypedTerm.Tuple [] }
| TLCB t = tup_seq TRCB { TypedTerm.Tuple t}

tup_seq:
| t1 = term TComma t2 = term  { [t1; t2] }
| t = term TComma s = tup_seq { t :: s }

projection:
| t = term TDot n = TNumber { TypedTerm.Proj (t, n) }

variable:
| v = TWord { TypedTerm.Var (v, 0) }

abstraction:
| TLPA TLambda v = TWord TColon ty = type_ TDot t = seq_ TRPA { TypedTerm.Abs ((v, ty), t) }

type_:
| TLPA ty = type_ TRPA { ty }
| TBool { Type.TyBool }
| TTyUnit { Type.TyUnit }
| ty1 = type_ TArrow ty2 = type_ { Type.TyArrow (ty1, ty2) }
| TLCB tup = type_seq TRCB { Type.TyTuple tup }
| TLCB TRCB { Type.TyTuple [] }

type_seq:
| ty = type_ { [ty] }
| ty = type_ TComma tyseq = type_seq { ty :: tyseq }

(*| th = type_ TComma tl = type_ { Type.TyBool (*Tuple (th :: tl :: [])*) }*)

application:
| TLPA t1 = term t2 = term TRPA { TypedTerm.App (t1, t2) }

ifthenelse:
| TLPA TIf cond = term TThen true_body = term TElse false_body = term TRPA { TypedTerm.If (cond, true_body, false_body) }

let_in:
| TLPA TLet n = TWord TAssign t = term TIn t_in = term TRPA { TypedTerm.LetIn(n, t, t_in) }

seq_:
| TLPA t = term TRPA  { t }
| TLPA t = term TRPA s = seq_ { TypedTerm.Seq (t, s) }

