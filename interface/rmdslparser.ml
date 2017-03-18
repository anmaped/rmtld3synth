
open List
open Batteries

open Texeqparser

(* parser for rmdsl *)
(* operators for tasks: \succ, \bowtie; and for RM: \parallel, \gg *)

type rmdsl_tk =
| Tsk of int * int
| Pri of rmdsl_tk * rmdsl_tk
| Arb of rmdsl_tk * rmdsl_tk
and rmdsl_rs =
  Emp of unit
| Res of rmdsl_tk * int * int
| Par of rmdsl_rs * rmdsl_rs
| Seq of rmdsl_rs * rmdsl_rs


(* direct parsing *)
let rec rmdsl_rs_parser l feed =
	let rmdsl_rs_parser_vars l feed =
	match l with
	  "parallel" :: r -> Par(feed, rmdsl_rs_parser r (Emp()))
	| "gg" :: r       -> Seq(feed, rmdsl_rs_parser r (Emp()))
	| _               -> raise (Failure ("bad expression rs var: "))
	in

	match l with
	| "\\" :: r -> rmdsl_rs_parser_vars r feed
	| _         -> raise (Failure ("bad expression rs: "))



let rmdslparser str =
	rmdsl_rs_parser (Texeqparser.lex (String.explode str)) (Emp())
