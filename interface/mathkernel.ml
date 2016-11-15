(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` pa_sexp_conv.cma *)

(* Interface between rmtld3tool and Mathematica's MathKernel *)
(* Lexer was adapted from Pierre Weis, projet Cristal, INRIA Rocquencourt *)
(* Parser was adapted from the "Interface between MetiTarski and Mathematica's MathKernel" by G.O.Passmore *)


(* ------------------------------------------------------------------------- *)
(* Lexical analysis.                                                         *)
(* ------------------------------------------------------------------------- *)

open List
open Batteries

open Sexplib
open Sexplib.Conv

open Unix

let matches s =
	let chars = String.explode s in
	fun c -> List.mem c chars;;

let space = matches " \t\n\r"
and punctuation = matches "()[]{},"
and symbolic = matches "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = matches "0123456789"
and alphanumeric = matches
  "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest = lexwhile prop cs in (Char.escaped c)^tok,rest
  | _ -> "",inp;;

let rec lex inp =
  match snd (lexwhile space inp) with
    [] -> []
  | c::cs -> let prop = if alphanumeric(c) then alphanumeric
                        else if symbolic(c) then symbolic
                        else fun c -> false in
             let toktl,rest = lexwhile prop cs in
             ((Char.escaped c)^toktl)::lex rest;;


lex (String.explode "2*((var_1 + x') + 11)");;
lex (String.explode "if (*p1-- == *p2++) then f() else g()");;


(* Parse a Mathematica FullForm term into extended rmtld3 term. *)

type m_tm = Int of int
          | Var of string
          | Plus of m_tm list
          | Times of m_tm list
          | Power of m_tm * int
          | Rational of int * int
          | Sin of m_tm
          | Cos of m_tm
          | Sinh of m_tm
          | Cosh of m_tm
          | Abs of m_tm
          | Log of m_tm
          | Tan of m_tm
          | Sqrt of m_tm
          | CubeRoot of m_tm
          | ArcTan of m_tm
          | ArcSin of m_tm
          | ArcCos of m_tm
          | Exp of m_tm
          | UnaryMinus of m_tm
          | Function of m_tm list
          | Slot of int
          | List of m_tm list with sexp;;


type m_fm = Tm of m_tm | True | False | Aborted with sexp;;


let rec parse_m_tm_lst l =
    match l with
      "[" :: r -> (
        			match parse_m_tm_lst r with
             		  (tm_lst, "]" :: s) -> (tm_lst, s)
           			| (_, s :: s') -> raise (Failure ("bad term list: " ^ s))
           			| _ -> raise (Failure "bad term list")
           		  )

    | _ -> match parse_m_tm' l with
           (tm, r) ->
                if (hd r) = "," then
                    let (tm_lst, r) = parse_m_tm_lst (tl r) in
                        ([tm] @ tm_lst, r)
                else ([tm], r)

and parse_m_tm' l =
    match l with
      [] 				-> raise (Failure "expected non-empty token list")

    | "Plus" :: r 		-> let (tm_lst, s) = parse_m_tm_lst r in
                           (Plus tm_lst, s)

    | "Times" :: r 		-> let (tm_lst, s) = parse_m_tm_lst r in
                           (Times tm_lst, s)

    | "Function" :: r 	-> let (tm_lst, s) = parse_m_tm_lst r in
                           (Function tm_lst, s)

    | "Slot" :: r 		-> 	let (slot_lst, s) = parse_m_tm_lst r in
    						begin
                           	match slot_lst with
                              [Int slot_id] 	-> (Slot slot_id, s)
                           	| _ 				-> raise (Failure "bad Slot[_]")
                       		end

    | "Power" :: r 		-> let (tm_lst, s) = parse_m_tm_lst r in
    						begin
                            match tm_lst with
                              [x; Int y] 	-> (Power (x, y), s)
                            | _ 			-> raise (Failure "bad Power[_]")
                            end

    | "Rational" :: r 	-> let (tm_lst, s) = parse_m_tm_lst r in
    						begin
                            match tm_lst with
                              [Int x; Int y] -> (Rational (x, y), s)
                            | _ 				-> raise (Failure "bad Rational[_]")
                       		end

    | "List" :: r 		-> let (tm_lst, s) = parse_m_tm_lst r in
                           (List tm_lst, s)

    | "-" :: r 			-> let i = int_of_string  (hd r) in 
					       (Int (~- i), (tl r))

    | x :: r 			-> (
    						try
    							let i = int_of_string x in
    							(Int i, r)
    						with
					        | _ -> if List.for_all alphanumeric (String.explode x) then (Var x, r)
					               else raise (Failure ("bad expression: " ^ (String.concat " " l)))
					       )

let parse_m_tm l =
	let (x, y) = parse_m_tm' l in
	x


(* Parse a lex'd repesentation of a Mathematica FullForm formula *)

let parse_m_fm l =
    match l with
      [] ->  raise (Failure "expected non-empty token list")
    | ["True"] -> True
    | ["False"] -> False
    | ["$"; "Aborted"] -> Aborted
    | _ -> Tm (parse_m_tm l)

(* Given a string representation of a Mathematica term, construct a
    Mathematica parse tree of type m_tm.  Note that the Mathematica
    ops Plus and Times are not forced to be binary.  To force this,
    further apply the composition (m_tm_of_mt_tm o mt_tm_of_m_tm). *)

let m_tm_of_str s = parse_m_tm (lex (String.explode s))

(* Given a string representation of a Mathematica formula, construct a
   Mathematica parse tree of type m_fm. The above notes on
   m_tm_of_str apply. *)

let m_fm_of_str s = parse_m_fm (lex (String.explode s))


(* Convert a Mathematica term tree into a string representation
   suitable for Mathematica input.  As above, we require that the
   Mathematica ops are used as binary functions.  This can be
   guaranteed by passing the Mathematica term through the composition
   (m_tm_of_mt_tm o mt_tm_of_m_tm). *)

let rec m_tm_to_str t =
    let int_to_str = (fun i -> if i >= 0 then string_of_int i
                                 else "-" ^ string_of_int (~-i)) in
    match t with
      Rational (p, 1) -> int_to_str p
    | Rational (p, q) -> "Rational[" ^ int_to_str p ^ "," ^ int_to_str q ^ "]"
    | Int i -> int_to_str i
    | Var s -> s
    | Plus [x; y] -> "Plus[" ^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
    | Times [x; y] -> "Times[" ^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
    | Power (x, y) -> "Power[" ^ m_tm_to_str x ^ "," ^ int_to_str y ^ "]"
    | UnaryMinus x -> "Times[-1," ^ m_tm_to_str x ^ "]"
    | Sin x -> "Sin[" ^ m_tm_to_str x ^ "]"
    | Cos x -> "Cos[" ^ m_tm_to_str x ^ "]"
    | Sinh x -> "Sinh[" ^ m_tm_to_str x ^ "]"
    | Cosh x -> "Cosh[" ^ m_tm_to_str x ^ "]"
    | Abs x -> "Abs[" ^ m_tm_to_str x ^ "]"
    | Log x -> "Log[" ^ m_tm_to_str x ^ "]"
    | Tan x -> "Tan[" ^ m_tm_to_str x ^ "]"
    | Sqrt x -> "Sqrt[" ^ m_tm_to_str x ^ "]"
    | CubeRoot x -> "CubeRoot[" ^ m_tm_to_str x ^ "]"
    | ArcTan x -> "ArcTan[" ^ m_tm_to_str x ^ "]"
    | ArcSin x -> "ArcSin[" ^ m_tm_to_str x ^ "]"
    | ArcCos x -> "ArcCos[" ^ m_tm_to_str x ^ "]"
    | Exp x -> "Exp[" ^ m_tm_to_str x ^ "]"
    (* | RootIntervals x => "RootIntervals[{" ^ m_tm_to_str x ^ "}]" *)
    | _ -> raise (Failure "cannot convert Mathematica tm to string")

(* change it for MACOS *)
let mk_proc = ref (Unix.open_process "math -noprompt");;
let mk_writeln s =
     output_string (snd (!mk_proc)) (s ^ "\n");
     BatIO.flush (snd (!mk_proc));;

(* A simple handshaking function.  This should be run immediately
   after opening up an MK process.  It just does a simple check to
   make sure the MK is being responsive, and crucially gets the system
   into a state so that all strings read from it will begin with
   "\nOut[...]//FullForm=" and end with "\n\nIn[...]:= ", with the
   '...' being numbers. *)

let mk_handshake () =
    ((* print ("\n" ^ (mk_opt_str (!mk_active_options)) ^ "\n"); *)
     mk_writeln ("InitTime = TimeUsed[]");

     (* mk_writeln ("FullForm[1+1]");
      block_until_read "FullForm= 2\n\nIn[4]:= " *)

     (* We need to install into Mathematica's runtime Wenda Li's
        polynomial real root isolation code *)

     (*mk_writeln (root_iso_code);*)

     (*** Setup our custom Mathematica REPL so we can use line-based I/O ***)
     mk_writeln ("While[True, NV = Input[\"In>\\n\"]; Print[NV]; If[NV == Quit, Quit[]]]");

 );;



(* Close MathKernel process *)

let mk_close ignore_outcome =
	let status = Unix.close_process !mk_proc in
	();;
	(*Printf.printf status;
    
    (if ignore_outcome orelse Useful.mem status [Unix.W_EXITED, Unix.W_SIGNALED 9] then ()
     else if status = Unix.W_SIGNALED sigxcpu
     then print "Processor time limit exceeded for Mathematica\n"
     else print ("****ERROR: exit status = " ^ stringOfStatus status ^ "\n");
     mk_proc := NONE)*)

let minisleep (sec: float) =
    ignore (Unix.select [] [] [] sec);;

(* Open a Mathematica MathKernel process. *)
(*let channel_from_mathematica, channel_to_mathematica = Unix.open_process "math -noprompt";;*)
(*minisleep 10.;;*)
mk_handshake ();;

(*let answer_from_mathematica = BatIO.nread (fst !mk_proc) 6 ;;
Printf.printf "%s\n" answer_from_mathematica;;*)

let r = String.create 250 in
let n = input (fst !mk_proc) r 0 250 in
    Printf.printf "%s\n" (String.sub r 0 n) ;

(*let x = try Char.escaped (BatIO.read (fst !mk_proc)) with BatIO.No_more_input -> "End of string";;*)

mk_writeln "Quit";;
mk_close ();;
(*BatInnerIO.output channel_to_mathematica ("ss" ^ "\n");;
(*Printf.fprintf channel_to_mathematica "Tell me if this is equal ...\n";;*)*)



(*let mk_proc = ref (NONE : ((TextIO.instream, TextIO.outstream) Unix.proc * TextIO.instream * TextIO.outstream) option)*)




let x = m_fm_of_str "Plus[Times[Rational[-1, 2], Power[b, 3], c], 
 Times[Rational[1, 2], Power[a, 3], Power[c, 2]], 
 Times[Rational[3, 2], a, b, Power[c, 2]], 
 Times[Rational[1, 2], Power[c, 3]]]";;

let y = sexp_of_m_fm x;;

let st = Sexp.to_string y;;

Printf.printf "%s" st;;