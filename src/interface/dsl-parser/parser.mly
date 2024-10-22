%{ 
    open Ast

    let map_u = function
        | "s" -> S
        | "ms" -> Ms
        | "us" -> Us
        | "ns" -> Ns
        | a -> failwith a
%}

%token <string> NAME
%token <string> NUM
%token <string * string> TIME
%token TRUE FALSE
%token LPAR LSQPAR
%token RPAR RSQPAR
%token EOF
%token TWODOTS
%token NEGATION
%token DONTCARE

%token IMPLIES
%token AND
%token OR
%token LESS GREATER
%token LESSOREQUAL GREATEROREQUAL
%token EQUAL

%token PLUS TIMES RANGE

%token WITHIN
%token UNTIL
%token SINCE
%token HISTORICALLY ALWAYS
%token EVENTUALLY PAST
%token NEXT PREV
%token RISE FALL
%token DURATION IN COMMA ON
%token DOTS OF

%nonassoc SINGLE_FALL SINGLE_RISE
%nonassoc WITHIN

%start main
%type <Ast.fm> main


%%

main: f = scope EOF { f }

scope:
  | HISTORICALLY TWODOTS f = formula { Historically (Less (NaN, NaU),f) }
  | ALWAYS TWODOTS f = formula { Always (Less (NaN, NaU),f) }
  | r = formula { r }


formula:
  | r = implies { r }

implies:
  | a = disjunction IMPLIES b = implies { Implies(a, b) }
  | r = disjunction { r }

disjunction:
  | a = disjunction OR b = conjunction { Or(a,b) }
  | r = conjunction { r }

conjunction:
  | a = conjunction AND b = temporal { And(a,b) }
  | r = temporal { r }

temporal:
  | HISTORICALLY a = formula WITHIN t = time { Historically (t,a)  }
  | ALWAYS a = formula WITHIN t = time { Always (t,a)  }
  | EVENTUALLY a = formula WITHIN t = time { Eventually (t,a)  }
  | PAST EVENTUALLY a = formula WITHIN t = time { PastEventually (t,a) }
  | PREV a = atom WITHIN t = time { Prev(t,a) }
  | NEXT a = atom WITHIN t = time { Next(t,a) }
  | FALL a = atom WITHIN t = time { Fall(t,a) }
  | RISE a = atom WITHIN t = time { Rise(t,a) }
  | FALL a = atom { Fall(Less(NaN,NaU),a) } %prec SINGLE_FALL
  | RISE a = atom { Rise(Less(NaN,NaU),a) } %prec SINGLE_RISE
  | a = temporal UNTIL b = formula WITHIN t = time { Until (t,a,b) }
  | a = temporal SINCE b = formula WITHIN t = time { Since (t,a,b) }
  | a = temporal ON t = TIME { Eventually (Equal (N (int_of_string (fst t)), map_u (snd t) ),a)  }
  | r = atom { r }

time:
  | EQUAL t = TIME { Equal (N (int_of_string (fst t)), map_u (snd t) ) }
  | LESSOREQUAL t = TIME { LessOrEqual (N (int_of_string (fst t)), map_u (snd t) ) }
  | LESS t = TIME { Less (N (int_of_string (fst t)), map_u (snd t) ) }
  | RANGE LSQPAR t0 = TIME COMMA t1 = TIME RSQPAR { RangeC((N(int_of_string (fst t0)), map_u (snd t0) ), (N(int_of_string (fst t1)), map_u (snd t1) )) }
  | RANGE LSQPAR t0 = TIME COMMA t1 = TIME LSQPAR { RangeO((N(int_of_string (fst t0)), map_u (snd t0) ), (N(int_of_string (fst t1)), map_u (snd t1) )) }
  | RANGE t0 = TIME DOTS t1 = TIME { RangeO((N(int_of_string (fst t0)), map_u (snd t0) ), (N(int_of_string (fst t1)), map_u (snd t1) )) }
  | t = TIME { Less (N (int_of_string (fst t)), map_u (snd t) ) }
  | DONTCARE { Less (NaN, NaU) }

atom:
| LPAR r = formula RPAR { r }
| a = term LESS b = term { LessThan(a,b) }
| a = term GREATER b = term { GreaterThan(a,b) }
| a = term LESSOREQUAL b = term { GreaterOrEqualThan(a,b) }
| a = term GREATEROREQUAL b = term { GreaterThan(a,b) }
| TRUE { True }
| FALSE { False }
| c = NAME { Prop c }
| NEGATION f = atom { Not(f) }

term:
  | DURATION OF f = formula IN i = interval { Duration(i,f) }
  | a = term PLUS b = term_atom1 { FPlus(a,b) }
  | a = term TIMES b = term_atom1 { FTimes(a,b) }
  | r = term_atom1 { r }

%inline interval:
  | LSQPAR a = term_atom COMMA b = term_atom  RSQPAR { Interval(a,b) }
  | a = term_atom DOTS b = term_atom { Interval(a,b) }

%inline term_atom1:
  | LPAR a = term RPAR { a }
  | r = term_atom { r }

%inline term_atom:
  | c = NUM { Constant( float_of_string c) }
  | t = TIME { Constant( float_of_string (fst t)) }

%%
