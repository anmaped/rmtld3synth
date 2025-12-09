open Printf

let parse_string s =
  (* Run the parser. *)
  match Parser.main Lexer.token (Lexing.from_string s) with
  | v ->
      (* Success. The parser has produced a semantic value [v]. *)
      v
  | exception Lexer.Error msg ->
      (* A lexical error has occurred. *)
      eprintf "%s%!" msg;
      raise Exit
  | exception Parser.Error ->
      (* A syntax error has occurred. *)
      Semantic.check "input" s
