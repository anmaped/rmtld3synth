open Lexing
open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

(* This part concerns the table-based parser [UnitActionsParser]. *)

module I = UnitActionsParser.MenhirInterpreter

(* [env checkpoint] extracts a parser environment out of a checkpoint, which
   must be of the form [HandlingError env]. *)

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory and
         should be fixed in the future. *)
      0

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress
  |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text
   that corresponds to the [i]-th stack cell. The top stack cell is numbered
   zero. *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i] keywords
         are correctly inside the syntax error message database. The integer
         [i] should always be a valid offset into the known suffix of the
         stack. *)
      "???"

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. In our setting, this cannot happen, since the
   table-based parser is invoked only when we know that there is a syntax
   error in the input file. *)

let succeed v = assert false

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location =
    let b = E.last buffer in
    if (fst b).pos_fname <> "stdin" then
      let c1 = (fst b).pos_cnum in
      let c2 = (snd b).pos_cnum in
      let underline = String.sub text c1 (c2 - c1) in
      let s1 = String.sub text 0 c1 in
      let s2 = String.sub text c2 (String.length text - c2) in
      s1 ^ "\x1b[4:3m\x1b[58:2::240:143:104m" ^ underline
      ^ "\x1b[59m\x1b[4:0m" ^ s2 ^ "\n"
    else L.range (E.last buffer)
  in
  (* Show the tokens just before and just after the error. *)
  let indication =
    sprintf "Syntax error %s.\n" (E.show (show text) buffer)
  in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (*print_endline (string_of_int (state checkpoint));*)
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message ;
  exit 1

(* [check filename text] runs the parser. *)

let check filename text =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = L.init filename (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a function
     of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions of
     the last two tokens. This is useful when a syntax error occurs, as these
     are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = UnitActionsParser.Incremental.main lexbuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  I.loop_handle succeed (fail text buffer) supplier checkpoint
