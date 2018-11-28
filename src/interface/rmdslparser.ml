
open List
open Batteries

open Sexplib
open Sexplib.Conv

open Texeqparser
open Rmtld3
open Helper

(* parser for rmdsl *)
(* operators for tasks: \succ, \bowtie; and for RM: \parallel, \gg *)

type parameter = PInt of int | PFreevar of string [@@deriving sexp]

type rmdsl_tk =
    TkEmp of unit
  | Tsk of string * parameter list
  | Pri of rmdsl_tk * rmdsl_tk
  | Arb of rmdsl_tk * rmdsl_tk
[@@deriving sexp]

type rmdsl_rs =
    Emp of unit
  | Res of string * rmdsl_tk * parameter list
  | Par of rmdsl_rs * rmdsl_rs
  | Seq of rmdsl_rs * rmdsl_rs
  | Cmp of rmdsl_rs * rmdsl_rs
[@@deriving sexp]


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

  (* skip keywords *)
  | "\\\\" :: ("left" :: r) -> rmdsl_rs_parser_tk r feed
  | "\\\\" :: ("right" :: r) -> rmdsl_rs_parser_tk r feed

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

    | "mapsto" :: (name :: r)   -> rmdsl_rs_parser' r feed

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

type tp_tuple = float list * rmtld3_fm list [@@deriving sexp]
let mk_empty_tuple = ([],[])

(* Let's define a meta next *)
let next phi = meventually_eq 1. phi

let rec rmtld3_fm_of_rmdsl_tm tm st : (rmtld3_fm * rmtld3_fm * 'a) -> rmtld3_fm -> (rmtld3_fm * rmtld3_fm * 'a) =
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
          ( next ( Until_eq (t, Not(Prop("RE#"^nm)), Prop("RE#"^nm) ) ) )
          ( next ( Until (t, Or(Prop("RU#"^nm), prop_list_of_fm filter ), mand ( Prop("SO#"^nm) ) (next (Until(t,Not(Or(Prop("SO#"^nm), Prop("RU#"^nm))),Prop("RE#"^nm) )))  ) ) )
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
                     in (fun (phi1,phi2,tp) filter ->
                           let fm1fst,fm1snd,fm1tp = f1 (phi1,phi2,tp) (prop_list_of_fm filter)
                           in let fm2 = f2 (fm1fst,fm1snd,fm1tp) (prop_list_of_fm fm1fst)
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
    
    let pi = get_float (hd plst)
    in let budget = get_float (hd (tl plst))
    in
    (* if \pi = \theta then we could simplify it since we have no constraints *)
    let dur_res filter = if pi = budget then mtrue else (
      mimplies
      (Prop("RN#"^nm))
      (
        mand
        ( next (Until_eq (pi, Not(Prop("RN#"^nm)), Prop("RN#"^nm) ) ) )
        ( less_or_equal (Duration(Constant(pi), filter)) (Constant(budget)) )
      )
    )
    in [ fun (a,c,tp) b ->
           let ghost_prop = if pi = budget then mtrue else (Prop("RN#"^nm)) in (* this is for a unconstrained resource *)
           let fm_out1,fm_out2,tp = rmtld3_fm_of_rmdsl_tm tk nm (a,c,tp) (mand b ghost_prop)
           in let all_prop = prop_list_of_fm fm_out1
           in (mand (dur_res all_prop) (fm_out1), fm_out2, ((fst tp)@[pi],(snd tp)@[ghost_prop]) )
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


let rmtld3_fm_lst_of_rmdsl_lst expression =
  let rmdsl_lst = rmtld3_fm_of_rmdsl' expression
  in fold_left (fun a f ->
    let _,_,ghost_propositions = f (mtrue,mtrue,mk_empty_tuple) mtrue (* this step is to find the ghost propositions *)
    in let init () = fold_left (fun a b -> if b == mtrue then a else (next (mand a b)) ) mtrue (snd ghost_propositions)
    in

    let ex,ex2,tp = f (mtrue,mtrue,mk_empty_tuple) (prop_list_of_fm (init ())) in (* call f with the known ghost propositions *)
    let lcm_bound = lcm_list (List.map (fun a -> int_of_float a) (fst tp)) in
    let fm = mand (Prop("sys_init")) (malways (float_of_int lcm_bound) (mand ex ex2)) in

    let fm = (
      mand (
        init ()
      ) (
        fm
      )
    ) in


    verb
    ( fun _ ->
      print_endline "Symbols ((list for LCM), propositions):" ;
      print_endline ( Sexp.to_string_hum (sexp_of_tp_tuple tp)) ;

      print_endline ("\nLCM bound: "^(string_of_int lcm_bound) ) ;

      print_endline "\nTiming order of symbols: " ;
      print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm ex)) ;

      print_endline "\nDuaration of symbols: " ;
      print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm ex2)) ;

      print_endline "\nMerged rmtld3 formula: " ;
      print_endline ( Sexp.to_string_hum (sexp_of_rmtld3_fm fm)) ;

      print_endline "--------------------------------------------------------------------------------\n" ;
    ) ;

    fm::a

  ) [] rmdsl_lst

