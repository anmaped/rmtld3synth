(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)


open List
open Batteries

open Sexplib
open Sexplib.Conv

open Texeqparser
open Rmtld3
open Rmtld3synth_helper

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
  | Cmp of rmdsl_rs * rmdsl_rs
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

  | "(" :: r -> let tk,rlst = rmdsl_rs_parser_tk r (TkEmp()) in rmdsl_rs_parser_tk rlst tk (* feed is discarded *)
  | ")" :: r -> (feed,r)

  | "\\\\" :: ("tk" :: r)     -> let name, rlst = rmdsl_rs_parser_string r in
    let param,rlst = rmdsl_rs_parser_param rlst []
    in rmdsl_rs_parser_tk rlst (Tsk(name,param))

  | "\\\\" :: ("succ" :: r)   -> let tk,rlst = rmdsl_rs_parser_tk r (TkEmp()) in (Pri(feed, tk),rlst)

  | "\\\\" :: ("bowtie" :: r) -> let tk,rlst = rmdsl_rs_parser_tk r (TkEmp()) in (Arb(feed, tk),rlst)

  | _ -> raise (Failure ("bad expression rs tk: "^ ( Sexp.to_string_hum (sexp_of_tokens l))))

let rec rmdsl_rs_parser' (l: tokens) (feed: rmdsl_rs) : rmdsl_rs * tokens =
  let rmdsl_rs_parser_vars l (feed: rmdsl_rs) : rmdsl_rs * tokens =
    match l with
      "parallel" :: r -> let rs,rlst = rmdsl_rs_parser' r (Emp()) in (Par(feed, rs), rlst)
    | "mid" :: r      -> let rs,rlst = rmdsl_rs_parser' r (Emp()) in (Cmp(feed, rs), rlst)
    | "gg" :: r       -> let rs,rlst = rmdsl_rs_parser' r (Emp()) in (Seq(feed, rs), rlst)
    | "rm" :: r       -> let name, rlst = rmdsl_rs_parser_string r in
      let exp_tk,rlst = rmdsl_rs_parser_tk rlst (TkEmp()) in
      let param,rlst = rmdsl_rs_parser_param rlst []
      in
      rmdsl_rs_parser' rlst (Res(name, exp_tk, param))

    | _               -> raise (Failure ("bad expression rs var: "^ ( Sexp.to_string_hum (sexp_of_tokens l))))
  in

  match l with
    []           -> (feed,[])
  | "\\\\" :: r  -> rmdsl_rs_parser_vars r feed
  | "(" :: r     -> rmdsl_rs_parser' r (Emp()) (* feed is discarded *)
  | ")" :: r     -> (feed,r)
  | "{" :: r     -> rmdsl_rs_parser' r (Emp())
  | "}" :: r     -> (feed, r)

  (* symbols to discard *)
  | "\\\\\\\\" :: r -> rmdsl_rs_parser' r feed
  | "$" :: r     -> rmdsl_rs_parser' r feed
  | "." :: r     -> rmdsl_rs_parser' r feed
  | "&" :: r     -> rmdsl_rs_parser' r feed

  | _            -> raise (Failure ("bad expression rs: "^ ( Sexp.to_string_hum (sexp_of_tokens l))))

let rmdsl_rs_parser lx = fst (rmdsl_rs_parser' lx (Emp()))

let rmdslparser str =
  let rs = rmdsl_rs_parser (Texeqparser.lex (String.explode str)) in
  verb (fun _ ->
    print_endline ("Rmdsl input: "^str^"\n");
    print_endline "--------------------------------------------------------------------------------\n";
    print_endline ("rmdsl tree: ");
    print_endline ((Sexp.to_string_hum (sexp_of_rmdsl_rs rs))^"\n");
  );
  rs


(*
  Type conversions:
    - to rmtld3_fm
*)

let get_int el = match el with PInt(x) -> x | _ -> raise (Failure ("get_int conversion"))
let get_float el = float_of_int (get_int el)


let rec prop_list_of_fm' (fm: rmtld3_fm) : rmtld3_fm list = 
  match fm with
  | True()                  -> []
  | Prop p                  -> [Prop(p)]
  | Not sf                  -> prop_list_of_fm' sf
  | Or (sf1, sf2)           -> (prop_list_of_fm' sf1)@(prop_list_of_fm' sf2)
  | Until (gamma, sf1, sf2) -> (prop_list_of_fm' sf1)@(prop_list_of_fm' sf2)
  | LessThan (tr1,tr2)      -> []
  | _ -> raise (Failure "error: prop_list_of_fm'")

let rec remove_dups lst =
  match lst with
  | []   -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let prop_list_of_fm fm : rmtld3_fm = fold_left (fun a b -> Or(a,b)) (Not(mtrue)) (remove_dups (prop_list_of_fm' fm)) (* it removes duplications *)

let rec rmtld3_fm_of_rmdsl_tm tm st : (rmtld3_fm * rmtld3_fm) -> rmtld3_fm -> (rmtld3_fm * rmtld3_fm) =
  let tsk_prop nm t (phi1, phi2) filter = (mand phi1 (Until (t, Or(Prop("B#"^nm),Or(Prop("R#"^nm),Or(Prop("S#"^nm),filter))), Prop("E#"^nm) )),  mtrue) in
  match tm with
  | Tsk(str,plst) when length plst = 2 -> tsk_prop (st^str) (get_float (hd plst))
  | Pri(tk1,tk2)  -> let f1 = rmtld3_fm_of_rmdsl_tm tk1 st
                     in let f2 = rmtld3_fm_of_rmdsl_tm tk2 st
                     in (fun (phi1,phi2) filter -> let fm1 = f1 (phi1,phi2) (prop_list_of_fm filter)
                        in let fm2 = f2 fm1 (prop_list_of_fm (fst fm1) )
                        in fm2)

  | Arb(tk1,tk2)  -> let f1 = rmtld3_fm_of_rmdsl_tm tk1 st
                     in let f2 = rmtld3_fm_of_rmdsl_tm tk2 st
                     in let fm1 = f1 (mtrue,mtrue) (prop_list_of_fm mtrue)
                     in let fm2 = f2 (mtrue,mtrue) (prop_list_of_fm mtrue)

                     in (fun (phi1,phi2) filter -> let fm11 = f1 (phi1,phi2) (prop_list_of_fm (Or((fst fm2), filter)))
                       in let fm22 = f2 fm11 (prop_list_of_fm (Or((fst fm1),filter)))
                       in fm22 )


  | _             -> raise (Failure ("bad rmdsl expression tm"))

let rec rmtld3_fm_of_rmdsl ex : ((rmtld3_fm * rmtld3_fm) -> rmtld3_fm -> (rmtld3_fm * rmtld3_fm)) list =
  match ex with
    Emp ()           -> []
  | Res(str,tk,plst) -> [fun (a,c) b -> rmtld3_fm_of_rmdsl_tm tk str (a,c) b ]
  | Par(rs1,rs2)     -> (rmtld3_fm_of_rmdsl rs1)@(rmtld3_fm_of_rmdsl rs2) (* unreal paralell (split case) *)
  | Seq(rs1,rs2)     -> [fun (a,c) b -> (True(),True()) ] (* TODO *)
  | Cmp(rs1,rs2)     -> [fun (a,c) filter ->
                          let f1 = List.hd (rmtld3_fm_of_rmdsl rs1) in (* skip other list elements; TODO raise something *)
                          let f2 = List.hd (rmtld3_fm_of_rmdsl rs2) in
                          let out1 = f1 (mtrue,mtrue) (prop_list_of_fm mtrue) in
                          let out2 = f2 (mtrue,mtrue) (prop_list_of_fm mtrue) in
                          let out11 = f1 (a,c) (prop_list_of_fm (Or((fst out2), filter))) in
                          let out22 = f2 out11 (prop_list_of_fm (Or((fst out1), filter))) in
                          out22
                        ]

