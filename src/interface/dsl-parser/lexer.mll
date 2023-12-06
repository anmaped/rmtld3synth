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
| '-' { MINUS }
| '+' { PLUS }
| '*' { TIMES }
| "&&" { AND }
| '<' { LESS }
| '>' { GREATER }
| "<=" {LESSOREQUAL }
| ">=" { GREATEROREQUAL }
| "=" { EQUAL }
| ':' { TWODOTS }
| ',' { COMMA }
| "until" { UNTIL }
| "since" { SINCE }
| "within" { WITHIN }
| "always" { ALWAYS }
| "historically" { HISTORICALLY }
| "eventually" { EVENTUALLY }
| "past" { PAST }
| "next" { NEXT }
| "previous" { PREV }
| "fall" { FALL }
| "rise" { RISE }
| "duration" { DURATION }
| "in" { IN }
| "of" { OF }
| ".." { DOTS }
| [ 'A'-'Z' 'a'-'z']+ as s { NAME s }
| [ '0'-'9' ]+ as t (("s" | "ms" | "us" | "ns") as u) { TIME (t,u) }
| [ '0'-'9' '.']+ as n { NUM n }
| '_' { EMPTY }
| '|' '|' { OR }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LSQPAR }
| ']' { RSQPAR }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
