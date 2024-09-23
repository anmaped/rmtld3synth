{
    open Parser

    exception Error of string
}

rule token = parse
| ' ' { token lexbuf }
| '\n' { MenhirLib.LexerUtil.newline lexbuf; token lexbuf }
| "true" { TRUE }
| "false" { FALSE }
| "->" { IMPLIES }
(*| '-' { MINUS }*)
| "~" { NEGATION } 
| '+' { PLUS }
| '*' { TIMES }
| "&&" { AND }
| "||" { OR }
| '<' { LESS }
| '>' { GREATER }
| "<=" {LESSOREQUAL }
| ">=" { GREATEROREQUAL }
| "=" { EQUAL }
| ':' { TWODOTS }
| ',' { COMMA }
| "until" { UNTIL }
| "U" { UNTIL }
| "since" { SINCE }
| "S" { SINCE }
| "within" { WITHIN }
| "always" { ALWAYS }
| "historically" { HISTORICALLY }
| "eventually" { EVENTUALLY }
| "past" { PAST }
| "next" { NEXT }
| "N" { NEXT }
| "previous" { PREV }
| "prev" { PREV }
| "P" { NEXT }
| "fall" { FALL }
| "rise" { RISE }
| "duration" { DURATION }
| "range" { RANGE }
| "in" { IN }
| "of" { OF }
| "on" { ON }
| ".." { DOTS }
| "_" { DONTCARE }
| [ 'A'-'Z' 'a'-'z' '_']+ as s { NAME s }
| [ '0'-'9' ]+ as t (("s" | "ms" | "us" | "ns") as u) { TIME (t,u) }
| [ '0'-'9' '.']+ as n { NUM n }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LSQPAR }
| ']' { RSQPAR }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
