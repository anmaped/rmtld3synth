(*

$ echo "true" | ./test.exe

*)

open Dsl
open Ast

let print_u = function S -> "s" | Ms -> "ms" | Us -> "us" | Ns -> "ns"
let print_time = function Unbound -> "inf" | Bound x -> string_of_int x

let print_kind = function
  | Less (t, u) -> "[<" ^ print_time t ^ print_u u ^ "]"
  | Equal (t, u) -> "[=" ^ print_time t ^ print_u u ^ "]"
  | LessOrEqual (t, u) -> "[<=" ^ print_time t ^ print_u u ^ "]"

let rec print_tm = function
  | Constant x -> string_of_float x
  | C (t, u) -> print_time t ^ print_u u
  | FPlus (a, b) -> print_tm a ^ "+" ^ print_tm b
  | FTimes (a, b) -> print_tm a ^ "*" ^ print_tm b
  | Duration (i, f) -> "∫" ^ print_interval i ^ " " ^ print_fm f

and print_interval = function
  | Interval (a, b) -> "[" ^ print_tm a ^ "," ^ print_tm b ^ "]"

and print_fm = function
  | True -> "T"
  | False -> "F"
  | Not a -> "~" ^ print_fm a
  | Prop c -> c
  | Or (a, b) -> "(" ^ print_fm a ^ "||" ^ print_fm b ^ ")"
  | And (a, b) -> "(" ^ print_fm a ^ "&&" ^ print_fm b ^ ")"
  | Implies (a, b) -> "(" ^ print_fm a ^ "->" ^ print_fm b ^ ")"
  | LessThan (a, b) -> print_tm a ^ "<" ^ print_tm b
  | LessOrEqualThan (a, b) -> print_tm a ^ "<=" ^ print_tm b
  | GreaterThan (a, b) -> print_tm b ^ "<" ^ print_tm a
  | GreaterOrEqualThan (a, b) -> print_tm b ^ "<=" ^ print_tm a
  | Until (k, a, b) ->
      "(" ^ print_fm a ^ " U" ^ print_kind k ^ " " ^ print_fm b ^ ")"
  | Since (k, a, b) ->
      "(" ^ print_fm a ^ " S" ^ print_kind k ^ " " ^ print_fm b ^ ")"
  | Rise (k, f) -> "rise" ^ print_kind k ^ " " ^ print_fm f
  | Fall (k, f) -> "fall" ^ print_kind k ^ " " ^ print_fm f
  | Prev (k, f) -> "prev" ^ print_kind k ^ " " ^ print_fm f
  | Next (k, f) -> "next" ^ print_kind k ^ " " ^ print_fm f
  | Always (k, f) -> "always" ^ print_kind k ^ " " ^ print_fm f
  | Historically (k, f) -> "historically" ^ print_kind k ^ " " ^ print_fm f
  | Eventually (k, f) -> "eventually" ^ print_kind k ^ " " ^ print_fm f
  | PastEventually (k, f) -> "past eventually" ^ print_kind k ^ " " ^ print_fm f

open Printf

let parse s =
  (* Run the parser. *)
  match Parser.main Lexer.token (Lexing.from_string s) with
  | v ->
      (* Success. The parser has produced a semantic value [v]. *)
      print_endline (print_fm v);
      exit 0
  | exception Lexer.Error msg ->
      (* A lexical error has occurred. *)
      eprintf "%s%!" msg;
      exit 1
  | exception Parser.Error ->
      (* A syntax error has occurred. *)
      SemanticCheck.attempt2 "stdin" s

(* função Main *)
let () =
  let s = read_line () in

  (*let r = Parser.main Lexer.token (Lexing.from_string s) in
    print_endline (print_fm r);*)
  parse s
