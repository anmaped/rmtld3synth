(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)


open List
open Batteries

open Sexplib
open Sexplib.Conv

open Texeqparser

(* parser for rmdsl *)
(* operators for tasks: \succ, \bowtie; and for RM: \parallel, \gg *)

type parameter = PInt of int | PFreevar of string with sexp

type rmdsl_tk =
  TkEmp of unit
| Tsk of string * parameter list
| Pri of rmdsl_tk * rmdsl_tk
| Arb of rmdsl_tk * rmdsl_tk
with sexp

type rmdsl_rs =
  Emp of unit
| Res of string * rmdsl_tk * parameter list
| Par of rmdsl_rs * rmdsl_rs
| Seq of rmdsl_rs * rmdsl_rs
with sexp


(* direct parsing *)
let rmdsl_rs_parser_string l = (List.hd (List.tl l), List.tl (List.tl (List.tl l)) )

let rec rmdsl_rs_parser_param l (feed: parameter list) : parameter list * tokens =
	let rec pre_match pm =
		match pm with
		  [] -> ""
		| PFreevar(a) :: r when r <> [] -> a^"."^pre_match r
		| PFreevar(a) :: _ -> a
		| PInt(a) :: r when r <> [] -> (string_of_int a)^"."^(pre_match r)
		| PInt(a) :: _ -> string_of_int a
	in 
	match l with
	  "{" :: r -> rmdsl_rs_parser_param r []
	| "}" :: r -> (feed,r)
	| "," :: r -> rmdsl_rs_parser_param r feed
	| "\\\\" :: r -> rmdsl_rs_parser_param r feed
	| a :: ("_" :: ("{" :: r)) when chk_alphanum a
	           -> let pm,rlst = rmdsl_rs_parser_param ("{"::r) []
	              in rmdsl_rs_parser_param rlst (feed@[PFreevar(a^"_"^(pre_match pm))])
	| a :: ("_" :: (b :: r))   when chk_alphanum a && chk_alphanum b
	           -> rmdsl_rs_parser_param r (feed@[PFreevar(a^"_"^b)])
	| a :: r when chk_alphanum a
	           -> (try rmdsl_rs_parser_param r (feed@[PInt(int_of_string a)]) with _ -> rmdsl_rs_parser_param r (feed@[PFreevar(a)]))
	| _        -> raise (Failure ("bad parameter :"^(Sexp.to_string_hum (sexp_of_tokens l))))


let rec rmdsl_rs_parser_tk l (feed: rmdsl_tk) : rmdsl_tk * tokens =
	match l with
	| "{" :: r -> rmdsl_rs_parser_tk r feed
	| "}" :: r -> (feed,r)

	| "\\\\" :: ("tk" :: r)     -> let name, rlst = rmdsl_rs_parser_string r in
								   let param,rlst = rmdsl_rs_parser_param rlst []
								   in rmdsl_rs_parser_tk rlst (Tsk(name,param))

	| "\\\\" :: ("succ" :: r)   -> let tk,rlst = rmdsl_rs_parser_tk r (TkEmp()) in (Pri(feed, tk),rlst)

	| "\\\\" :: ("bowtie" :: r) -> let tk,rlst = rmdsl_rs_parser_tk r (TkEmp()) in (Arb(feed, tk),rlst)

	| _ -> raise (Failure ("bad expression rs tk: "^ ( Sexp.to_string_hum (sexp_of_tokens l))))

let rec rmdsl_rs_parser (l: tokens) (feed: rmdsl_rs) : rmdsl_rs * tokens =
	let rmdsl_rs_parser_vars l (feed: rmdsl_rs) : rmdsl_rs * tokens =
	match l with
	  "parallel" :: r -> let rs,rlst = rmdsl_rs_parser r (Emp()) in (Par(feed, rs), rlst)
	| "gg" :: r       -> let rs,rlst = rmdsl_rs_parser r (Emp()) in (Seq(feed, rs), rlst)
	| "rm" :: r       -> let name, rlst = rmdsl_rs_parser_string r in
						 let exp_tk,rlst = rmdsl_rs_parser_tk rlst (TkEmp()) in
						 let param,rlst = rmdsl_rs_parser_param rlst []
						 in
						 rmdsl_rs_parser rlst (Res(name, exp_tk, param))
	
	| _               -> raise (Failure ("bad expression rs var: "^ ( Sexp.to_string_hum (sexp_of_tokens l))))
	in

	match l with
	  []           -> (feed,[])
	| "\\\\" :: r  -> rmdsl_rs_parser_vars r feed
	| "(" :: r     -> rmdsl_rs_parser r (Emp()) (* feed is discarded *)
	| ")" :: r     -> (feed,r)
	| "{" :: r     -> rmdsl_rs_parser r (Emp())
	| "}" :: r     -> (feed, r)

	(* symbols to discard *)
	| "\\\\\\\\" :: r -> rmdsl_rs_parser r feed
	| "$" :: r     -> rmdsl_rs_parser r feed
	| "." :: r     -> rmdsl_rs_parser r feed
	| "&" :: r     -> rmdsl_rs_parser r feed

	| _            -> raise (Failure ("bad expression rs: "^ ( Sexp.to_string_hum (sexp_of_tokens l))))



let rmdslparser str =
	let rs,_ = rmdsl_rs_parser (Texeqparser.lex (String.explode str)) (Emp()) in
	print_endline ("Rmdsl input: "^str^"\n");
    print_endline (Sexp.to_string_hum (sexp_of_rmdsl_rs rs));
	()
