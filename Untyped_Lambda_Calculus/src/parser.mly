%{

%}

%token TLambda (* λ or \ *)
%token TDot (* . *)
%token <string> TWord
%token TSep (* ; *)
%token TAssign (* := *)
%token TLPA TRPA (* () *)
%token TEof

%start <Term.term list> prgm

%%

prgm:
| s = sequence TEof { s }

sequence:
| t = term TSep { t :: [] }
| t = term TSep s = sequence { t :: s }

term:
| var = variable    { var }
| abs = abstraction { abs }
| app = application { app }
| assign = assignation { assign }

variable:
| v = TWord { Term.Var (v, 0) }

abstraction:
| TLPA TLambda v = TWord TDot t = term TRPA { Term.Abs (v, t) }

application:
| TLPA t1 = term t2 = term TRPA { Term.App (t1, t2) }

assignation:
| k = TWord TAssign v = term { Term.Assign (k, v) }
