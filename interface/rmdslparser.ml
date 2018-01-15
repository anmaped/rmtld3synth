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


(* auxiliar fuctions *)
let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)
 
let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let lcm_list lst =
  List.fold_left (fun a b -> lcm a b) (hd lst) (tl lst)

(*
  Type conversions:
    - to rmtld3_fm
*)

let get_int el = match el with PInt(x) -> x | _ -> raise (Failure ("get_int conversion"))
let get_float el = float_of_int (get_int el)


let rec prop_list_of_fm' (fm: rmtld3_fm) : rmtld3_fm list = 
  match fm with
  | True()                      -> []
  | Prop p                      -> [Prop(p)]
  | Not sf                      -> prop_list_of_fm' sf
  | Or (sf1, sf2)               -> (prop_list_of_fm' sf1)@(prop_list_of_fm' sf2)
  | Until (gamma, sf1, sf2)     -> (prop_list_of_fm' sf1)@(prop_list_of_fm' sf2)
  | Until_eq (gamma, sf1, sf2)  -> (prop_list_of_fm' sf1)@(prop_list_of_fm' sf2)
  | Until_leq (gamma, sf1, sf2) -> (prop_list_of_fm' sf1)@(prop_list_of_fm' sf2)
  | LessThan (tr1,tr2)          -> []
  | a                           -> raise (Failure ("Unsupported formula " ^ Sexp.to_string_hum (sexp_of_fm a)) )

let rec remove_dups lst =
  match lst with
  | []   -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let prop_list_of_fm fm : rmtld3_fm = fold_left (fun a b -> Or(a,b)) (Not(mtrue)) (remove_dups (prop_list_of_fm' fm)) (* it removes duplications *)

type tp_tuple = float list * rmtld3_fm list with sexp
let mk_empty_tuple = ([],[])

let rec rmtld3_fm_of_rmdsl_tm tm st : (rmtld3_fm * rmtld3_fm * 'a) -> rmtld3_fm -> (rmtld3_fm * rmtld3_fm * 'a) =

  (* these are old definitions; huge overhead; let's simplify *)
  (*let dur nm t d = equal
  (
    Duration(
      Constant(t),
      Or(
        Prop("B#"^nm),
        Or(
          Prop("R#"^nm),
          Or(
            Prop("S#"^nm),
            Prop("E#"^nm)
          )
        )
      )
    )
  ) (
    Constant(d)
  ) in
  let subb nm t filter = mand_list
  [
    ( malways 20. ( mimplies (Prop("S#"^nm)) (Until (t, Or( Prop("S#"^nm), filter), Prop("R#"^nm))) ) ) ; (* [TODO] HARDCODED BOUND *)
    ( malways 20. ( mimplies (Prop("R#"^nm)) (Until (t, Or( Prop("R#"^nm), filter), Or( Prop("S#"^nm), Prop("E#"^nm) ) )) ) ) ;
    ( malways 20. ( mimplies (Prop("E#"^nm)) (Until (t, Or( Prop("E#"^nm), filter), Prop("B#"^nm))) ) ) ;
  ]
  in
  let tsk_prop nm t d (phi1, phi2) filter =
    (mand phi1 (mand (Until (t, Or(Prop("B#"^nm),Or(Prop("R#"^nm),Or(Prop("S#"^nm),filter))), Prop("E#"^nm) )) (subb nm t filter) ), mand phi2 (dur nm t d) )
  in*)

  let dur nm t d =
  mimplies
    (Prop("RE#"^nm))
    (equal
      (
        Duration(
          Constant(t),
            Prop("RU#"^nm)
        )
      ) (
        Constant(d)
      )
    )
  in
  let tsk_prop nm t d (phi1, phi2, tuple) filter =
    (
      mand phi1 (
        mimplies (Prop("RE#"^nm))
        (mand 
          (meventually_eq t (Prop("RE#"^nm)) )
          (Until(2., Prop("RE#"^nm), (Until (t, Or(Prop("RU#"^nm),mfalse), Prop("SO#"^nm) )) ))
        )
      ),
      mand phi2 (dur nm t d),
      (
        t::(fst tuple),
        Prop(("RE#"^nm))::(snd tuple)
      )
    )
  in



  match tm with
  | Tsk(str,plst) when length plst = 2 -> tsk_prop (st^str) (get_float (hd plst)) (get_float (hd (tl plst)))

  | Pri(tk1,tk2)  -> let f1 = rmtld3_fm_of_rmdsl_tm tk1 st
                     in let f2 = rmtld3_fm_of_rmdsl_tm tk2 st
                     in (fun (phi1,phi2,tp) filter -> let fm1fst,fm1snd,fm1tp = f1 (phi1,phi2,tp) (prop_list_of_fm filter)
                           in let fm2 = f2 (fm1fst,fm1snd,fm1tp) (prop_list_of_fm fm1fst )
                           in fm2
                        )

  | Arb(tk1,tk2)  -> let f1 = rmtld3_fm_of_rmdsl_tm tk1 st
                     in let f2 = rmtld3_fm_of_rmdsl_tm tk2 st

                     in (fun (phi1,phi2,tp) filter ->
                          let fm1fst,fm1snd,fm1tp = f1 (mtrue,mtrue,tp) (prop_list_of_fm mtrue)
                          in let fm2fst,fm2snd,fm2tp = f2 (mtrue,mtrue,tp) (prop_list_of_fm mtrue)
                          in
                          let fm11 = f1 (phi1,phi2,tp) (prop_list_of_fm (Or(fm2fst, filter)))
                          in let fm22 = f2 fm11 (prop_list_of_fm (Or(fm1fst,filter)))
                          in fm22
                        )

  | a             -> raise (Failure ("Unsupported expression rmdsl_tk "^ Sexp.to_string_hum (sexp_of_rmdsl_tk a) ))


let rec rmtld3_fm_of_rmdsl' ex : ((rmtld3_fm * rmtld3_fm * 'a) -> rmtld3_fm -> (rmtld3_fm * rmtld3_fm * 'a)) list =
  match ex with
    Emp ()           -> []

  | Res(nm,tk,plst) when length plst = 2 ->
    let p = (get_float (hd plst))
    in let b = (get_float (hd (tl plst)))
    in let dur_res filter = (mimplies (Prop("RN#"^nm)) (mand (meventually_eq p (Prop("RN#"^nm))) ( LessThan(Duration(Constant(p), filter), Constant(b)) )) )
    in [ fun (a,c,tp) b ->
           let fm_out1,fm_out2,tp = rmtld3_fm_of_rmdsl_tm tk nm (a,c,tp) b
           in let all_prop = prop_list_of_fm fm_out1
           in (mand (dur_res all_prop) (fm_out1), fm_out2, tp)
       ]

  | Par(rs1,rs2)     -> (rmtld3_fm_of_rmdsl' rs1)@(rmtld3_fm_of_rmdsl' rs2) (* unreal paralell (split case) *)
  | Seq(rs1,rs2)     -> [fun (a,c,tp) b -> (True(),True(), tp) ] (* TODO *)
  | Cmp(rs1,rs2)     -> [fun (a,c,tp) filter ->
                          let f1 = List.hd (rmtld3_fm_of_rmdsl' rs1) in (* skip other list elements; TODO raise something *)
                          let f2 = List.hd (rmtld3_fm_of_rmdsl' rs2) in
                          let out1fst,_,_ = f1 (mtrue,mtrue,mk_empty_tuple) (prop_list_of_fm mtrue) in
                          let out2fst,_,_ = f2 (mtrue,mtrue,mk_empty_tuple) (prop_list_of_fm mtrue) in
                          let out11 = f1 (a,c,tp) (prop_list_of_fm (Or(out2fst, filter))) in
                          let out22 = f2 out11 (prop_list_of_fm (Or(out1fst, filter))) in
                          out22
                        ]

  | a                -> raise (Failure ("Unsupported expression rmdsl_rs "^ Sexp.to_string_hum (sexp_of_rmdsl_rs a) ))


let rmtld3_fm_lst_of_rmdsl_lst expression = let rmdsl_lst = rmtld3_fm_of_rmdsl' expression in
  fold_left (fun a b ->
    let ex,ex2,tp = b (mtrue,mtrue,mk_empty_tuple) mtrue in
    let hy = lcm_list (List.map (fun a -> int_of_float a) (fst tp)) in
    let fm = malways (float_of_int hy) (mand ex ex2) in
    let fm = mand (meventually 1. (mand (Prop("RN#core0")) (meventually 1. (Prop("RE#core0ts1")) )) ) (*) (Until(1.,(Prop("RN#core0")),(Prop("RE#core0ts1"))))*) fm in
    (*let fm = (mand (fold_left (fun a b -> Until(1., a, b) ) mtrue (snd tp) ) (mimplies (Prop "RE#core0ts1") (Until(3., Prop "RE#core0ts1", Prop "RX#core0ts1")))) in*)
    (* constructs sequence of operation *)

    verb (fun _ ->
           print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm ex)) ;
           print_endline "##*##" ;
           print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm ex2)) ;
           print_endline "##*##" ;
           print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm fm)) ;
           print_endline "##*##\ntp:\n" ;
           print_endline ( Sexp.to_string_hum (sexp_of_tp_tuple tp)) ;
           print_endline "--------------------------------------------------------------------------------\n" ;
         ) ;
    fm::a
  ) [] rmdsl_lst

