%{

%}

%token TLambda (* λ or \ *)
%token TDot (* . *)
%token TBool TTrue TFalse (* Bool : true | false*)
%token TArrow (* -> *)
%token <string> TWord
%token TSep (* ; *)
%token TColon (* : *)
%token TAssign (* := *)
%token TLPA TRPA (* () *)
%token TEof
%right TArrow
%start <TypedTerm.term list> prgm

%%

prgm:
| s = sequence TEof { s }

sequence:
| t = term TSep { t :: [] }
| t = term TSep s = sequence { t :: s }

term:
| b   = bool_       { b   }
| var = variable    { var }
| abs = abstraction { abs }
| app = application { app }
| assign = assignation { assign }

bool_:
| TTrue  { TypedTerm.True  }
| TFalse { TypedTerm.False }

variable:
| v = TWord { TypedTerm.Var (v, 0) }

abstraction:
| TLPA TLambda v = TWord TColon ty = type_  TDot t = term TRPA { TypedTerm.Abs ((v, ty), t) }

type_:
| TBool { Type.Bool }
| ty1 = type_ TArrow ty2 = type_ { Type.Arrow (ty1, ty2) }

application:
| TLPA t1 = term t2 = term TRPA { TypedTerm.App (t1, t2) }

assignation:
| k = TWord TAssign v = term { TypedTerm.Assign (k, v) }
