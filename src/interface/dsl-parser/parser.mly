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

%token IMPLIES
%token AND
%token OR
%token LESS GREATER
%token LESSOREQUAL GREATEROREQUAL
%token EQUAL

%token PLUS TIMES

%token WITHIN
%token UNTIL
%token SINCE
%token HISTORICALLY ALWAYS
%token EVENTUALLY PAST
%token NEXT PREV
%token RISE FALL
%token DURATION IN COMMA
%token DOTS OF

%left PLUS
%left TIMES

%start main
%type <Ast.fm> main


%%

main: f = scope EOF { f }

scope:
  | HISTORICALLY TWODOTS a = formula { Historically (Less (Unbound, S),a) }
  | ALWAYS TWODOTS a = formula { Always (Less (Unbound, S),a) }
  | r = formula { r }


formula:
  | r = implies { r }

implies:
  | a = implies IMPLIES b = disjunction { Implies(a, b) }
  | r = disjunction { r }

disjunction:
  | a = disjunction OR b = conjunction { Or(a,b) }
  | r = conjunction { r }

conjunction:
  | a = conjunction AND b = temporal { And(a,b) }
  | r = temporal { r }

temporal:
  | HISTORICALLY a = atom WITHIN t = time { Historically (t,a)  }
  | ALWAYS a = atom WITHIN t = time { Always (t,a)  }
  | EVENTUALLY a = atom WITHIN t = time { Eventually (t,a)  }
  | PAST EVENTUALLY a = atom WITHIN t = time { PastEventually (t,a) }
  | PREV a = atom WITHIN t = time { Prev(t,a) }
  | NEXT a = atom WITHIN t = time { Next(t,a) }
  | FALL f = atom WITHIN t = time { Fall(t,f) }
  | RISE f = atom WITHIN t = time { Rise(t,f) }
  | a = temporal UNTIL b = atom WITHIN t = time { Until (t,a,b) }
  | a = temporal SINCE b = atom WITHIN t = time { Since (t,a,b) }
  | r = atom { r }

time:
  | EQUAL t = TIME { Equal (Bound (int_of_string (fst t)), map_u (snd t) ) }
  | LESSOREQUAL t = TIME { LessOrEqual (Bound (int_of_string (fst t)), map_u (snd t) ) }
  | LESS t = TIME { Less (Bound (int_of_string (fst t)), map_u (snd t) ) }
  | t = TIME { Less (Bound (int_of_string (fst t)), map_u (snd t) ) }

atom:
| LPAR r = formula RPAR { r }
| a = term LESS b = term { LessThan(a,b) }
| a = term GREATER b = term { GreaterThan(a,b) }
| a = term LESSOREQUAL b = term { GreaterOrEqualThan(a,b) }
| a = term GREATEROREQUAL b = term { GreaterThan(a,b) }
| TRUE { True }
| FALSE { False }
| c = NAME { Prop c }

term:
  | DURATION OF f = formula IN i = interval { Duration(i,f) }
  | a = term PLUS b = term { FPlus(a,b) } (* benign conflicts here ... *)
  | a = term TIMES b = term { FTimes(a,b) } (* benign conflicts here ... *)
  | LPAR a = term RPAR { a }
  | r = term_atom { r } 

%inline interval:
  | LSQPAR a = term_atom COMMA b = term_atom  RSQPAR { Interval(a,b) }
  | a = term_atom DOTS b = term_atom { Interval(a,b) }

%inline term_atom:
  | c = NUM { Constant( float_of_string c) }
  | t = TIME { Constant( float_of_string (fst t)) }

%%
