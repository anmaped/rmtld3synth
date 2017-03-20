(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)

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
and alphanumeric = matches "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;

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


(* Tests for lexer.
   	lex (String.explode "2*((var_1 + x') + 11)");;
   	lex (String.explode "if (*p1-- == *p2++) then f() else g()");;
*)

(* Parse a Mathematica FullForm term into the intermediate tree. *)

type m_tm =
    Int of int
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
  | List of m_tm list
  (* boolean terms *)
  | BoolConv of m_tm * string
  | And of m_tm list
  | Or of m_tm list
  | Not of m_tm
  (* quantifiers *)
  | Exists of string * m_tm
  (* less term *)
  | Less of m_tm list
  | Equal of m_tm list
  (* Simplify *)
  | Simplify of m_tm
with sexp;;


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
    []                -> raise (Failure "expected non-empty token list")

  | "Plus" :: r       -> let (tm_lst, s) = parse_m_tm_lst r in
    (Plus tm_lst, s)

  | "Times" :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    (Times tm_lst, s)


  | "Equal" :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    (Equal tm_lst, s)

  | "And"   :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    (And tm_lst, s)

  | "Or"    :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    (Or tm_lst, s)

  | "Not"   :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    (Not (hd tm_lst), s)

  | "Less"  :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    (Less tm_lst, s)


  | "Function" :: r   -> let (tm_lst, s) = parse_m_tm_lst r in
    (Function tm_lst, s)

  | "Slot" :: r       -> let (slot_lst, s) = parse_m_tm_lst r in
    begin
      match slot_lst with
        [Int slot_id] 	-> (Slot slot_id, s)
      | _ 				-> raise (Failure "bad Slot[_]")
    end

  | "Power" :: r      -> let (tm_lst, s) = parse_m_tm_lst r in
    begin
      match tm_lst with
        [x; Int y] 	-> (Power (x, y), s)
      | _ 			-> raise (Failure "bad Power[_]")
    end

  | "Rational" :: r   -> let (tm_lst, s) = parse_m_tm_lst r in
    begin
      match tm_lst with
        [Int x; Int y] -> (Rational (x, y), s)
      | _ 				-> raise (Failure "bad Rational[_]")
    end

  | "List" :: r       -> let (tm_lst, s) = parse_m_tm_lst r in
    (List tm_lst, s)

  | "-" :: r          -> let i = int_of_string  (hd r) in 
    (Int (~- i), (tl r))

  | x :: r           -> (
      try
        let i = int_of_string x in
        (Int i, r)
      with
      | _ -> if List.for_all alphanumeric (String.explode x) then (Var x, r)
        else raise (Failure ("bad expression: " ^ (String.concat " " l)))
    )

let parse_m_tm l =
  let (x, y) = parse_m_tm' l
  in x


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
    Rational (p, 1)  -> int_to_str p
  | Rational (p, q)  -> "Rational[" ^ int_to_str p ^ "," ^ int_to_str q ^ "]"
  | Int i            -> int_to_str i
  | Var s            -> s
  | Plus [x; y]      -> "Plus[" ^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
  | Times [x; y]     -> "Times[" ^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
  | Power (x, y)     -> "Power[" ^ m_tm_to_str x ^ "," ^ int_to_str y ^ "]"
  | UnaryMinus x     -> "Times[-1," ^ m_tm_to_str x ^ "]"
  | Sin x            -> "Sin[" ^ m_tm_to_str x ^ "]"
  | Cos x            -> "Cos[" ^ m_tm_to_str x ^ "]"
  | Sinh x           -> "Sinh[" ^ m_tm_to_str x ^ "]"
  | Cosh x           -> "Cosh[" ^ m_tm_to_str x ^ "]"
  | Abs x            -> "Abs[" ^ m_tm_to_str x ^ "]"
  | Log x            -> "Log[" ^ m_tm_to_str x ^ "]"
  | Tan x            -> "Tan[" ^ m_tm_to_str x ^ "]"
  | Sqrt x           -> "Sqrt[" ^ m_tm_to_str x ^ "]"
  | CubeRoot x       -> "CubeRoot[" ^ m_tm_to_str x ^ "]"
  | ArcTan x         -> "ArcTan[" ^ m_tm_to_str x ^ "]"
  | ArcSin x         -> "ArcSin[" ^ m_tm_to_str x ^ "]"
  | ArcCos x         -> "ArcCos[" ^ m_tm_to_str x ^ "]"
  | Exp x            -> "Exp[" ^ m_tm_to_str x ^ "]"
    (*
    	Boolean terms
	*)
  | BoolConv(x, typ) -> "BooleanConvert[" ^ m_tm_to_str x ^ ", " ^ typ ^ "]"
  | And [x; y]       -> "And[" ^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
  | Or [x; y]        -> "Or["^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
  | Not x            -> "Not[" ^ m_tm_to_str x ^ "]"
  (* qunatifiers *)
  | Exists(v,fm)     -> "Exists[" ^ v ^ "," ^ m_tm_to_str fm ^ "]"
    (*
    	Less term
    *)
  | Less [x; y]      -> "Less[" ^ m_tm_to_str x ^ "," ^ m_tm_to_str y ^ "]"
  (* Simplify *)
  | Simplify x       -> "Simplify[" ^ m_tm_to_str x ^ "]"
  | _                -> raise (Failure "cannot convert Mathematica tm to string")

(* change it for MACOS *)
let mk_proc = ref (Unix.open_process "math -noprompt");;

let mk_writeln s =
  output_string (snd (!mk_proc)) (s ^ "\n");
  BatIO.flush (snd (!mk_proc));;

let mk_readln () =
  let r = String.create 250 in
  let n = input (fst !mk_proc) r 0 250 in
  String.sub r 0 n



(* A simple handshaking function.  This should be run immediately
   after opening up an MK process.  It just does a simple check to
   make sure the MK is being responsive, and crucially gets the system
   into a state so that all strings read from it will begin with
   "\nOut[...]//FullForm=" and end with "\n\nIn[...]:= ", with the
   '...' being numbers. *)

let mk_handshake () =
  ((* print ("\n" ^ (mk_opt_str (!mk_active_options)) ^ "\n"); *)
    mk_writeln ("InitTime = TimeUsed[]");

    let x = mk_readln () in
    Printf.printf "%s\n" x;

    (* mk_writeln ("FullForm[1+1]");
       block_until_read "FullForm= 2\n\nIn[4]:= " *)

    (* We need to install into Mathematica's runtime Wenda Li's
       polynomial real root isolation code *)

    (*mk_writeln (root_iso_code);*)

    (*** Setup our custom Mathematica REPL so we can use line-based I/O ***)
    mk_writeln ("While[True, NV = Input[\"In>\\n\"]; Print[NV]; If[NV == Quit, Quit[]]]");

    let x = mk_readln () in
    Printf.printf "%s\n" x;

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


let mathkernel_cad formula =
  (* Open a Mathematica MathKernel process. *)
  (*let channel_from_mathematica, channel_to_mathematica = Unix.open_process "math -noprompt";;*)
  (*minisleep 10.;;*)
  mk_handshake ();

  (*let answer_from_mathematica = BatIO.nread (fst !mk_proc) 6 ;;
    Printf.printf "%s\n" answer_from_mathematica;;*)

  let x = mk_readln () in
  Printf.printf "%s\n" x;

  (*let x = try Char.escaped (BatIO.read (fst !mk_proc)) with BatIO.No_more_input -> "End of string";;*)

  mk_writeln "Quit";
  mk_close ();
  (*BatInnerIO.output channel_to_mathematica ("ss" ^ "\n");;
    (*Printf.fprintf channel_to_mathematica "Tell me if this is equal ...\n";;*)*)



  (*let mk_proc = ref (NONE : ((TextIO.instream, TextIO.outstream) Unix.proc * TextIO.instream * TextIO.outstream) option)*)


  (* Test code for parsing.

     	let x = m_fm_of_str "Plus[Times[Rational[-1, 2], Power[b, 3], c], 
     	 Times[Rational[1, 2], Power[a, 3], Power[c, 2]], 
     	 Times[Rational[3, 2], a, b, Power[c, 2]], 
     	 Times[Rational[1, 2], Power[c, 3]]]" in

     	Printf.printf "%s" (Sexp.to_string (sexp_of_m_fm x));;
  *)

open Rmtld3

(*let equality var1 var2() =
  	And([Less([Var(var1); Var(var2)]); Less([Var(var2); Var(var1)])])*)

let rec rmtld_tm_to_m rmtld_term =
  match rmtld_term with
  | Constant value      -> Int(int_of_float value)
  | Variable id         -> Var(id)
  (*| Duration (trm,sf)   -> rmtld_fm_to_m sf (* todo: replace here! *)*)
  | FPlus (eta1,eta2)   -> Plus([rmtld_tm_to_m eta1; rmtld_tm_to_m eta2])
  | FTimes (eta1,eta2)  -> Times([rmtld_tm_to_m eta1; rmtld_tm_to_m eta2])
  | x                   -> raise (Failure ("bad expression: " ^ (string_of_rmtld_tm x)))

and rmtld_fm_to_m rmtld_formula =
  match rmtld_formula with
  | Prop p                 -> Var("prop"^p) (* TODO: control var replacement *)
  | Not sf                 -> Not(rmtld_fm_to_m sf)
  | Or (sf1, sf2)          -> Or([rmtld_fm_to_m sf1; rmtld_fm_to_m sf2])
  (*| Until (pval, sf1, sf2) -> Less([Var(some_var); Int(1)]) (*rmtld_fm_to_m sf1*) (* todo: replace until operator with some_var = 1 *)*)
  | Exists (var,sf)        -> Exists(var, rmtld_fm_to_m sf)
  | LessThan (tr1,tr2)     -> Less([rmtld_tm_to_m tr1; rmtld_tm_to_m tr2])
  | x                      -> raise (Failure ("bad expression: " ^ (string_of_rmtld_fm x)))


let rec m_tm_to_rmtld m_tm =
  match m_tm with
  | Equal [x; y] -> Not(Or(LessThan(m_tm_to_rmtld_tm y, m_tm_to_rmtld_tm x), LessThan(m_tm_to_rmtld_tm x, m_tm_to_rmtld_tm y)))
  | Var x -> (*if var starts with prop then is a proposition *) Prop(x)

  | And [x; y]   -> Not(Or(Not(m_tm_to_rmtld x), Not(m_tm_to_rmtld y)))
  | Or [x; y]    -> Or(m_tm_to_rmtld x, m_tm_to_rmtld y)
  | Not x        -> Not(m_tm_to_rmtld x)
  | Less [x; y]  -> LessThan(m_tm_to_rmtld_tm x, m_tm_to_rmtld_tm y)

  (* TODO THIS ONLY ALLOWS ARITY 2 *)

  | x            -> raise (Failure ("bad expression: " ^ (Sexp.to_string (sexp_of_m_tm x))))

and m_tm_to_rmtld_tm m_tm =
  match m_tm with
  | Var x -> Variable(x)
  | Int x -> Constant(float_of_int x)
  | x     -> raise (Failure ("bad expression: " ^ (Sexp.to_string (sexp_of_m_tm x))))


let m_fm_to_rmtld m_formula = 
  match m_formula with
  | Tm(x) -> m_tm_to_rmtld x
  | x     -> raise (Failure ("bad expression: " ^ (Sexp.to_string (sexp_of_m_fm x))))

let m_fm_convert mode m_formula =
  (* convert the rmtld formula into the intermediate parse tree for mathematica *)

  print_endline ("m_fm_convert "^mode);

  let mt_formula_string = "OutputForm @ FullForm[" ^ m_tm_to_str (BoolConv(m_formula, mode)) ^ "]" in

  mk_writeln mt_formula_string;

  let answer = mk_readln () in
  Printf.printf "BCONV: %s\n" answer;

  (m_fm_of_str answer)

let m_fm_cnf m_formula = m_fm_convert "CNF" m_formula

let m_fm_dnf m_formula = m_fm_convert "DNF" m_formula

let m_fm_simplify m_formula =
  print_endline "m_fm_simplify";

  let mt_formula_string = "OutputForm @ FullForm[" ^ m_tm_to_str (Simplify(m_formula)) ^ "]" in

  mk_writeln mt_formula_string;

  let answer = mk_readln () in
  Printf.printf "SIMP: %s\n" answer;

  (m_fm_of_str answer)


let close_process_mathematica () =
  mk_writeln "Quit";
  mk_close ();

