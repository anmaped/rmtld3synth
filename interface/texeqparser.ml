(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)


open List
open Batteries

open Sexplib
open Sexplib.Conv

open Rmtld3

(* parsing latex equations *)

let matches s =
	let chars = String.explode s in
	fun c -> List.mem c chars;;

let space = matches " \t\n\r"
and punctuation = matches "()[]{},"
and symbolic = matches "\\^_"
and numeric = matches "0123456789"
and alphanumeric = matches "abcdefghijklmnopqrstuvwxyz'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;

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


(* intermediate representation *)
type op = Leq of unit | Geq of unit | Eq of unit | Less of unit | Greater of unit | N of unit with sexp
type intermediate_ltx_tm = 
	  TTimes of intermediate_ltx_tm list
	| TPlus of intermediate_ltx_tm list
	| TMinus of intermediate_ltx_tm list
	| TFrac of intermediate_ltx_tm * intermediate_ltx_tm
	| TInt of intermediate_ltx_tm * intermediate_ltx_tm
	| TVal of int
	| TVar of string
	| TEmpty of unit
and intermediate_ltx_fm =
	  FIneq of  (intermediate_ltx_fm * op) list
	| Fland of intermediate_ltx_fm list
	| Flor of intermediate_ltx_fm list
	| Alwaysless of intermediate_ltx_fm
	| Eventuallyless of intermediate_ltx_fm
	| FImplies of intermediate_ltx_fm * intermediate_ltx_fm
	| FIsol of intermediate_ltx_fm
	| FTerm of intermediate_ltx_tm
	| Strr of string list
with sexp



let rec parse_latexeq_tm' (l: string list) (feed: intermediate_ltx_tm) : intermediate_ltx_tm * string list =
    match l with
      [] 				   -> (feed,[])
    | "\\" :: r            -> begin
    						  match r with 
    						    "int" :: r         -> let pf,rlst = parse_latexeq_tm' r (TEmpty()) in
    						    					  let pf2,rlst2 = parse_latexeq_tm' rlst (TEmpty()) in (TInt(pf,pf2), rlst2) 

    						  | "times" :: r       -> parse_latexeq_tm' r feed
    						  | "frac"  :: r       -> let pf,rlst = parse_latexeq_tm' r feed in
    						  						  let pf2,rlst2 = parse_latexeq_tm' rlst feed in parse_latexeq_tm' rlst2 (TFrac(pf,pf2))

    						  | _ -> raise (Failure ("bad term:" ^ (String.concat " " r)))
    						  end

    | "+" :: r             -> let pf,rlst = parse_latexeq_tm' r (TEmpty()) in (TPlus([feed; pf]), rlst)
    | "-" :: r             -> let pf,rlst = parse_latexeq_tm' r (TEmpty()) in (TMinus([feed; pf]), rlst)

    | "^" :: r             -> parse_latexeq_tm' r feed (* TODO *)
    | "_" :: r             -> parse_latexeq_tm' r feed (* TODO *)
    | "{" :: r             -> let pf,rlst = parse_latexeq_tm' r feed in (pf,rlst)
    | "}" :: r             -> (feed, r)
    | "(" :: r             -> let pf,rlst = parse_latexeq_tm' r feed in parse_latexeq_tm' rlst pf
    | ")" :: r             -> (feed, r)

    | x :: r               -> try let i = int_of_string x in  (* check feed; something has been removed!!! *)
    						  parse_latexeq_tm' r (TVal(i))
    						  with | _ -> if List.for_all alphanumeric (String.explode x) then parse_latexeq_tm' r (TVar(x))
					               else raise (Failure ("bad expression: " ^ x))

let parse_latexeq_tm l feed = fst (parse_latexeq_tm' l feed)


let rec ineq_parse prefix feed op =
	match prefix with
	  FIneq(a)          -> FIneq((feed, op)::a)

	| Strr(a)           -> FIneq([(feed, op); ( FTerm(parse_latexeq_tm a (TVal(0)) )  , N())])   (* Strr(a)  *)
	| Alwaysless(a)     -> FIneq([(feed, op); (Alwaysless(a),N())])
	| Eventuallyless(a) -> FIneq([(feed, op); (Eventuallyless(a),N())])

	| Fland(a) -> Fland( (ineq_parse (List.hd a) feed op)::(List.tl a))
	| Flor(a) -> Flor( (ineq_parse (List.hd a) feed op)::(List.tl a))
	| FImplies(a,b) -> FImplies(ineq_parse a feed op, b)
	| _ -> raise (Failure ("bad expression for ineq: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm prefix))))

let emptystr = Strr([])
let rec match_feed feed : string list = match feed with Strr(a) -> a | FIsol(x) -> ["("]@(match_feed x)@[")"]  | _ -> raise (Failure ("bad expression for feed: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm feed))))

let rec lifting_ltx' l feed : intermediate_ltx_fm * string list =
	match l with
      [] 				   -> (feed,[])
    | "\\\\" :: r          -> begin
    						  match r with
    						    "leq"  :: r        -> let prefix,rlst = lifting_ltx' r emptystr in
    						                          (ineq_parse prefix feed (Leq()),rlst)
    						  | "geq"  :: r        -> let prefix,rlst = lifting_ltx' r emptystr in
    						  						  (ineq_parse prefix feed (Geq()),rlst)

    						  | "land" :: r        -> let prefix,rlst = lifting_ltx' r emptystr in
    						    					  let itt =
    						    					  match prefix with
    						    					    Fland(a) -> Fland(feed::a)
    						    					  
    						    					  | Strr(a) -> Fland([feed; Strr(a)])
    						    					  | FIneq(a) -> Fland([feed; FIneq(a)])
    						    					  | Alwaysless(a) -> Fland([feed; Alwaysless(a)])
    						    					  | Eventuallyless(a) -> Fland([feed; Eventuallyless(a)])

    						    					  | Flor(a) -> Flor( (Fland(feed::[List.hd a]) )::(List.tl a))
    						    					  | _ -> raise (Failure ("bad expression for and: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm prefix))))
    						    					  in
    						    					  (itt,rlst)

    						  | "lor"  :: r        -> let prefix,rlst = lifting_ltx' r emptystr in
    						    					  let itt = 
    						    					  match prefix with
    						    					    Flor(a) -> Flor(feed::a)

    						    					  | Strr(a) -> Flor([feed; Strr(a)])
    						    					  | Alwaysless(a) -> Flor([feed; Alwaysless(a)])
    						    					  | Eventuallyless(a) -> Flor([feed; Eventuallyless(a)])
    						    					  | Fland(a) -> Flor([feed; Fland(a)])

    						    					  | _ -> raise (Failure ("bad expression for or: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm prefix))))
    						    					  in
    						    					  (itt,rlst)

    						  | "rightarrow" :: r  -> let prefix,rlst = lifting_ltx' r feed in
    						  						  (FImplies(feed, prefix),rlst)

    						  | "alwaysless" :: r  -> (* feed is discarded *) let prefix,rlst = lifting_ltx' r emptystr in (Alwaysless(prefix),rlst)
    						  | "eventuallyless" :: r -> (* feed is discarded *) let prefix,rlst = lifting_ltx' r emptystr in (Eventuallyless(prefix),rlst)

    						  | "int" :: r         -> lifting_ltx' r (Strr((match_feed feed)@["\\"; "int"]))
    						  | "frac" :: r        -> lifting_ltx' r (Strr((match_feed feed)@["\\"; "frac"]))
    						  | "times" :: r        -> lifting_ltx' r (Strr((match_feed feed)@["\\"; "times"]))

    						  (* skip keywords *)
    						  | "scriptstyle" :: r -> lifting_ltx' r feed
    						  | "left" :: r        -> lifting_ltx' r feed
    						  | "right" :: r       -> lifting_ltx' r feed

    						  | _                  -> lifting_ltx' r feed
    						  end

    | "(" :: r             -> let prefix,rlst = lifting_ltx' r feed in lifting_ltx' rlst (FIsol(prefix)) (* TODO *)
    | ")" :: r             -> (feed,r) (* TODO *)

    | "<" :: r             -> let prefix,rlst = lifting_ltx' r emptystr in (ineq_parse prefix feed (Less()),rlst)
    | ">" :: r             -> let prefix,rlst = lifting_ltx' r emptystr in (ineq_parse prefix feed (Greater()),rlst)
    | "=" :: r             -> let prefix,rlst = lifting_ltx' r emptystr in (ineq_parse prefix feed (Eq()),rlst)


    (* skip keywords *)
    | "\\\\\\\\" :: r      -> (* skip linebreaks *) lifting_ltx' r feed
    | "&" :: r             -> lifting_ltx' r feed
    | "%" :: r             -> lifting_ltx' r feed

    | a :: r               -> lifting_ltx' r (Strr((match_feed feed)@[a]))

let lifting_ltx l feed : intermediate_ltx_fm = let x,y = lifting_ltx' l feed in if y = [] then x else raise (Failure ("bad expression; check parenthesis"))

(*

(* parsing latex sample
\int^{\pi_1} \psi_1=0\land 0\leq \int^{\pi_2} \psi_2<\theta )&\lor \ \ %\\
&\scriptstyle\left(0<\int^{\pi_1} \psi_1<\frac{\theta }{4}\land 0\leq \int^{\pi_2} \psi_2<\theta -3 \int^{\pi_1} \psi_1\right)&\lor \\
&\scriptstyle\left(\int^{\pi_1} \psi_1=\frac{\theta }{4}\land 0\leq \int^{\pi_2} \psi_2\leq \frac{\theta }{4}\right)&\lor \ \ %\\
&\scriptstyle\left(\frac{\theta }{4}<\int^{\pi_1} \psi_1<\frac{\theta }{3}\land 0\leq \int^{\pi_2} \psi_2<\frac{\theta -\int^{\pi_1} \psi_1}{3}\right)&\lor \\
&\scriptstyle\left(\int^{\pi_1} \psi_1=\frac{\theta }{3}\land \theta -3 \int^{\pi_1} \psi_1\leq \int^{\pi_2} \psi_2<\frac{\theta -\int^{\pi_1} \psi_1}{3}\right)&\lor \ \ %\\
&\scriptstyle\left(\frac{\theta }{3}<\int^{\pi_1} \psi_1<\theta \land 0\leq \int^{\pi_2} \psi_2<\frac{\theta -\int^{\pi_1} \psi_1}{3}\right)
*)


let s1 = "
(\\int^{\\pi_1} \\psi_1=0\\land 0\\leq \\int^{\\pi_2} \\psi_2<\\theta )&\\lor \\ \\ %\\\\
&\\scriptstyle\\left(0<\\int^{\\pi_1} \\psi_1<\\frac{\\theta }{4}\\land 0\\leq \\int^{\\pi_2} \\psi_2<\\theta -3 \\int^{\\pi_1} \\psi_1\\right)&\\lor \\\\
&\\scriptstyle\\left(\\int^{\\pi_1} \\psi_1=\\frac{\\theta }{4}\\land 0\\leq \\int^{\\pi_2} \\psi_2\\leq \\frac{\\theta }{4}\\right)&\\lor \\ \\ %\\\\
&\\scriptstyle\\left(\\frac{\\theta }{4}<\\int^{\\pi_1} \\psi_1<\\frac{\\theta }{3}\\land 0\\leq \\int^{\\pi_2} \\psi_2<\\frac{\\theta -\\int^{\\pi_1} \\psi_1}{3}\\right)&\\lor \\\\
"

let s2 = "a=\\int^{\\var} \\psi_1 \\land \\alwaysless \\left(
\\left( f_{\\beta,\\alpha}\\left(a\\right) < (\\frac{3}{4} + 10) - 10 \\right) \\rightarrow \\left(\\eventuallyless_{\\var + \\var_2} \\ \\psi_d\\right) \\right)"

let test2_input = lex (String.explode s1)
let test3_input = lex (String.explode s2)

let y = print_endline (List.fold_left (fun a b -> a^" \n"^b) "" test2_input)

let test = print_endline "\ntest 1:\n"; print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (lifting_ltx [ "G"; "\\\\"; "land"; "X"; "Y"; "\\\\"; "leq"; "X"; "Y"; "\\\\"; "land"; "Z"; "\\\\"; "lor"; "H"; "\\\\"; "lor"; "HH"] emptystr)))
let test2 = print_endline "\ntest 2:\n"; print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (lifting_ltx test2_input emptystr)))

let test2 = print_endline "\ntest 3:\n"; print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (lifting_ltx test3_input emptystr)))
*)

