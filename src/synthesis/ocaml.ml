(*
   Synthesis from RMTLD3 to Ocaml
*)

open Batteries
open List
open Str

open Rmtld3
open Helper

let has_tm_dur = ref false
let has_fm_uless = ref false
let has_fm_ueq = ref false
let has_fm_ulesseq = ref false

type body = string * string


(* ocaml module api *)
let synth_tm_constant value helper = ("(fun k s t -> "^ (string_of_float value) ^")","")
let synth_tm_variable name helper = failwith "No freevariables allowed."
let synth_tm_duration (tm_call,tm_body) (fm_call,fm_body) helper =
  has_tm_dur := true;
  ("(compute_tm_duration "^ tm_call ^" "^ fm_call ^")", tm_body^fm_body)

let synth_tm_duration_body = "
let compute_tm_duration tm fm k u t =
  let dt = (t,tm k u t) in

  let indicator_function (k,u) t phi = if fm k u t = True then 1. else 0. in
  let riemann_sum m dt (i,i') phi =
    (* dt=(t,t') and t in ]i,i'] or t' in ]i,i'] *)
    count_duration := !count_duration + 1 ;
    let t,t' = dt in
    if i <= t && t < i' then
      (* lower bound *)
      (i'-.t) *. (indicator_function m t phi)
    else (
      if i <= t' && t' < i' then
        (* upper bound *)
        (t'-.i) *. (indicator_function m t' phi)
      else
        (i'-.i) *. (indicator_function m i phi)
    ) in
  let eval_eta m dt phi x = fold_left (fun s (prop,(i,t')) -> (riemann_sum
  m dt (i,t') phi) +. s) 0. x in
  let t,t' = dt in
  eval_eta (k,u) dt fm (sub_k (k,u,t) t')
"

let synth_tm_plus cmptr1 cmptr2 helper =
  ("(fun k s t -> ("^ fst cmptr1 ^" k s t) +. ("^ fst cmptr2 ^" k s t))", (snd cmptr1)^(snd cmptr2))

let synth_tm_times cmptr1 cmptr2 helper =
  ("(fun k s t -> ("^ fst cmptr1 ^" k s t) *. ("^ fst cmptr2 ^" k s t))", (snd cmptr1)^(snd cmptr2))

let synth_fm_true helper = ("(fun k s t -> True)","")

let synth_fm_p p helper =
  let tbl = get_proposition_rev_hashtbl helper in
  let p_id = Hashtbl.find tbl p in
  ("(fun k s t -> k.evaluate k.trace \""^ p_id ^"\" t)","")

let synth_fm_not cmpfm helper = ("(fun k s t -> b3_not ("^ fst cmpfm ^" k s t))", snd cmpfm)

let synth_fm_or cmpfm1 cmpfm2 helper =
  ("(fun k s t -> b3_or ("^ fst cmpfm1 ^" k s t) ("^ fst cmpfm2 ^" k s t))", (snd cmpfm1)^(snd cmpfm2))

let synth_fm_less cmptr1 cmptr2 helper =
  ("(fun k s t -> b3_lessthan ("^ fst cmptr1 ^" k s t) ("^ fst cmptr2 ^" k s t))", (snd cmptr1)^(snd cmptr2))

let synth_fm_uless gamma sf1 sf2 helper =
  has_fm_uless := true;
  ("(compute_uless "^ (string_of_float gamma) ^" "^ (fst sf1) ^" "^ (fst sf2) ^")", (snd sf1)^(snd sf2))

let synth_fm_ueq gamma sf1 sf2 helper =
  has_fm_ueq := true;
  ("(compute_ueq "^ (string_of_float gamma) ^" "^ (fst sf1) ^" "^ (fst sf2) ^")", (snd sf1)^(snd sf2))

let synth_fm_ulesseq gamma sf1 sf2 helper =
  has_fm_ulesseq := true;
  ("(compute_ulesseq "^ (string_of_float gamma) ^" "^ (fst sf1) ^" "^ (fst sf2) ^")", (snd sf1)^(snd sf2))

let synth_fm_uless_body = "
let compute_uless gamma f1 f2 k u t =
  let m = (k,u,t) in
  let eval_i b1 b2 =
    if b2 <> False then
      b3_to_b4 b2
    else if b1 <> True && b2 = False then
      b3_to_b4 b1
    else
      Symbol
  in

  let eval_b (k,u,t) f1 f2 v =
    if v <> Symbol then
      v
    else
      eval_i (f1 k u t) (f2 k u t)
  in

  let eval_fold (k,u,t) f1 f2 x =
    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 v, ii2)) (Symbol,t) x)
  in

  if not (gamma >= 0.) then
    raise  (Failure \"Gamma of U< operator is a non-negative value\")
  else
  begin
    let k,_,t = m in
    let subk = sub_k m gamma in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if k.duration_of_trace <= (t +. gamma) then
        Unknown
      else (
        False
      )
    else
      b4_to_b3 eval_c
  end
"

(* differs from uless on "eval_i" and "sub_k m (gamma +. (epsilon_float *. 100.) )" *)
let synth_fm_ueq_body = "
let compute_ueq gamma f1 f2 k u t =
  let t_i = t in
  let m = (k,u,t) in
  let eval_i t b1 b2 =
    if t -. t_i = gamma then
      b3_to_b4 b2
    else if b1 <> True then
      b3_to_b4 b1
    else
      Symbol
  in

  let eval_b (k,u,t) f1 f2 v =
    if v <> Symbol then
      v
    else
      eval_i t (f1 k u t) (f2 k u t)
  in

  let eval_fold (k,u,t) f1 f2 x =
    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 v, ii2)) (Symbol,t) x)
  in

  if not (gamma >= 0.) then
    raise  (Failure \"Gamma of U= operator is a non-negative value\")
  else
  begin
    let k,_,t = m in
    let subk = sub_k m (gamma +. (epsilon_float *. 100.) ) in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if k.duration_of_trace <= (t +. gamma) then
        Unknown
      else (
        False
      )
    else
      b4_to_b3 eval_c
  end
"

(* differs from uless on "sub_k m (gamma +. (epsilon_float *. 100.) )" *)
let synth_fm_ulesseq_body = "
let compute_uless gamma f1 f2 k u t =
  let m = (k,u,t) in
  let eval_i b1 b2 =
    if b2 <> False then
      b3_to_b4 b2
    else if b1 <> True && b2 = False then
      b3_to_b4 b1
    else
      Symbol
  in

  let eval_b (k,u,t) f1 f2 v =
    if v <> Symbol then
      v
    else
      eval_i (f1 k u t) (f2 k u t)
  in

  let eval_fold (k,u,t) f1 f2 x =
    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 v, ii2)) (Symbol,t) x)
  in

  if not (gamma >= 0.) then
    raise  (Failure \"Gamma of U<= operator is a non-negative value\")
  else
  begin
    let k,_,t = m in
    let subk = sub_k m (gamma +. (epsilon_float *. 100.) ) in
    let eval_c = eval_fold m f1 f2 subk in
    if eval_c = Symbol then
      if k.duration_of_trace <= (t +. gamma) then
        Unknown
      else (
        False
      )
    else
      b4_to_b3 eval_c
  end
"


let synth_ocaml (out_file,out_dir) cluster_name monitor_name monitor_period formula compute helper =
  let mon_call,mon_body = compute formula helper
  in let mon = "
open List
open Rmtld3

module type Trace = sig val trc : trace end
(* one trace :: module OneTrace : Trace = struct let trc = [(\"a\",(1.,2.))] end *)

module "^ (String.capitalize_ascii monitor_name) ^"  ( T : Trace  ) = struct \n"^ mon_body ^"
  "^ (if !has_fm_uless then synth_fm_uless_body else "") ^"
  "^ (if !has_fm_ueq then synth_fm_ueq_body else "") ^"
  "^ (if !has_fm_ulesseq then synth_fm_ulesseq_body else "") ^"
  "^ (if !has_tm_dur then synth_tm_duration_body else "") ^"
  let env = environment T.trc
  let lg_env = logical_environment
  let t = 0.
  let mon = "^ mon_call ^" env lg_env t
end
  " in

  if out_file <> "" then
    (* ... *)
    ()
  else
    (* print to console *)
    print_endline mon


let synth_ocaml_unittests () =
  (* debuging flag *)
  let activate_tests = ref false in
  activate_tests := true;

  if activate_tests = ref true then
  begin
    (* basic tests for RMTLD3 *)
    let test1_trace = [("A",(0.,1.)); ("B",(1.,2.)); ("A",(2.,3.)); ("B",(3.,4.));
      ("B",(4.,5.)); ("A",(5.,6.)); ("C",(6.,6.5))] in
    let test2_trace = [("A",(0.,1.)); ("C",(1.,2.)); ("A",(2.,3.)); ("B",(3.,4.));
      ("B",(4.,5.)); ("A",(5.,6.)); ("C",(6.,6.5))] in
    let test3_trace = [("A",(0.,1.)); ("A",(1.,2.)); ("A",(2.,3.)); ("A",(3.,4.));
    ("A",(4.,5.)); ("A",(5.,6.)); ("A",(6.,9.)); ("B",(9.,20.));] in
    let t_k = environment test1_trace in (* generate environment based on the trace *)
    let t_k2 = environment test2_trace in (* generate environment based on the trace *)
    let t_k3 = environment test3_trace in (* generate environment based on the trace *)

    let t_u = logical_environment in
    let pass_test lb v = Printf.printf "%s -> " lb ; if v = True then
      Printf.printf "[PASSED]\n" else Printf.printf "[FAILED] %s \n" (b3_to_string v) in

    (* basic tests set *)
    pass_test "true " (compute (t_k, t_u, 0.) mtrue) ;
    pass_test "false" (compute (t_k, t_u, 0.) (Not(mfalse)) ) ;
    pass_test "A    " (compute (t_k, t_u, 0.) (Prop("A"))) ;
    pass_test "~C   " (compute (t_k, t_u, 0.) (Not(Prop("C")))) ;

    (* duration tests set *)
    pass_test "int 5 A < 2.0(0)1  " (
      compute (t_k, t_u, 0.)
      (LessThan(Duration(Constant(5.),Prop("A")), Constant(2. +.
      (epsilon_float *. 3.))))
    ) ;
    pass_test "~(int 5 A < 2)     " (
      compute (t_k, t_u, 0.)
      (Not(LessThan(Duration(Constant(5.),Prop("A")), Constant(2.))))
    ) ;
  
    (* until tests set *)
    pass_test "B U A       " (
      compute (t_k, t_u, 0.) (Until (3., Prop("B"), Prop("A")) )
    ) ;
    pass_test "~(C U B)    " (
      compute (t_k, t_u, 0.) (Not(Until (3., Prop("C"), Prop("B"))))
    ) ;
    pass_test "(A U B)     " (
      compute (t_k, t_u, 0.) (Until (3., Prop("A"), Prop("B")))
    ) ;
    pass_test "~(F 6 C)    " (
      compute (t_k, t_u, 0.) (Not(meventually 6. (Prop("C"))))
    ) ;
    pass_test "~(F 5.9 C)  " (
      compute (t_k, t_u, 0.) (Not(meventually 5.9 (Prop("C"))))
    ) ;
    pass_test "F 6.0(0)1 C " (
      compute (t_k, t_u, 0.) (meventually (6. +. (epsilon_float *. 3.)) (Prop("C")))
    ) ;
    pass_test "F_1.0(0)1 ~A" (
      compute (t_k, t_u, 0.) (meventually (1. +. epsilon_float) (Not(Prop("A"))))
    ) ;
  
    (* set of tests for temporal formulas *)
    pass_test "~(A -> (F_1 C))   " (
      compute (t_k, t_u, 0.) (Not(mimplies (Prop("A"))  (meventually 1.
      (Prop("C")))))
    ) ;
    pass_test "A -> (F_1.0(0)1 B)" (
      compute (t_k, t_u, 0.) (mimplies (Prop("A"))  (meventually (1. +.
      epsilon_float) (Prop("B"))))
    ) ;
    pass_test "G_2 ~A" (
      compute (t_k2, t_u, 0.)  (malways 2. (Not(Prop("A"))))
    ) ;
    pass_test "G_4 (A -> (F_2 B))" (
      compute (t_k, t_u, 0.) (malways 4. (mimplies (Prop("A"))
      (meventually 2. (Prop("B")))))
    ) ;
    pass_test "G_9.1 (A -> (F_2 B))" (
      compute (t_k, t_u, 0.) (malways 9.1 (mimplies (Prop("A"))
      (meventually 2. (Prop("B")))))
    ) ;

    (* complexity *)
    (* (y-2)*(x*(2*x))+((y-3)*x)+x *)

    count := 0 ;
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x + x *)
    pass_test "A U_10 *" (
      compute (t_k3, t_u, 0.)
      (
        Until(10.,
          Prop("A"),
          Prop("*")
        )
      )
    ) ;
    Printf.printf "count: %i\n" !count ;

    count := 0 ;
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x + x *)
    pass_test "(A U_10 B) U_10 (A U_10 *)" (
      compute (t_k3, t_u, 0.)
      (
        Until(10.,
          Until(10.,Prop("A"),Prop("B")),
          Until(10.,Prop("A"),Prop("*"))
        )
      )
    ) ;
    Printf.printf "count: %i\n" !count ;

    count := 0 ;
    (* 5*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x) + 4*x *)
    pass_test "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)" (
      compute (t_k3, t_u, 0.)
      (
        Until(10.,
          Until(10.,
            Until(10.,Prop("A"),Prop("B")),
            Until(10.,Prop("A"),Prop("B"))
          ),
          Until(10.,
            Until(10.,Prop("A"),Prop("B")),
            Until(10.,Prop("A"),Prop("*"))
          )
        )
      )
    ) ;
    Printf.printf "count: %i\n" !count ;

    count := 0 ;
    (* number of temporal operators: 15 *)
    (* 13*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x) + 12*x *)
    pass_test "(((A U_10 B) U_10 (A U_10 *)) U_10 ((A U_10 B) U_10 (A U_10 *)) U_10 (((A U_10 B) U_10 (A U_10 *)) U_10 ((A U_10 B) U_10 (A U_10 *)))"
    (
      compute (t_k3, t_u, 0.)
      (
        Until(10.,
          Until(10.,
            Until(10.,
              Until(10.,Prop("A"),Prop("B")),
              Until(10., Prop("A"),Prop("B"))
            ),
            Until(10.,
              Until(10.,Prop("A"),Prop("B")),
              Until(10., Prop("A"),Prop("B"))
            )
          ),
          Until(10.,
            Until(10.,
              Until(10.,Prop("A"),Prop("B")),
              Until(10., Prop("A"),Prop("B"))
            ),
            Until(10.,
              Until(10.,Prop("A"),Prop("B")),
              Until(10., Prop("A"),Prop("*"))
            )
          )
        )
      )
    );
    Printf.printf "count: %i\n" !count ;



    (* binomial(n+(m-1), (m-1)) * 2^n *)
    let g_val = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
    let g_val2 = map (fun a -> (Int.pow 2 a) -1) (tl g_val) in
    let lst1 = [1; 2; 3; 4; 5; 6; 7; 8]
    in
    iter (fun a -> 
      let fm = gen_u_formula_with_triangle_pattern true a 10.
      in print_endline ("D"^(string_of_int a)^": " ^ (string_of_int (asym_comp (lst1, 0., 0.) fm))^" n: "^(string_of_int (snd (measure_formula fm))) );

      (* print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm fm)); *)
    ) g_val;

    iter (fun a -> 
      let fm = gen_u_formula_with_maximum_prop_evaluation a 10. (List.length lst1)
      in print_endline ("N"^(string_of_int a)^": " ^ (string_of_int (asym_comp (lst1, 0., 0.) fm))^" n: "^(string_of_int (snd (measure_formula fm))) );
    ) g_val2;

(*
 * NEW test [TODO]
(((3.528682  < int[int[0.643269 ] (E ) ] ((E  or E )) ) or ((E  U_3.204527 E ) o
r (int[2.963296 ] (E )  < 3.456293 ))) U_4.239142 ((int[0.333695 ] (B )  < int[2
.105323 ] (A ) ) U_2.887519 ((int[3.716714 ] (E )  < 3.871726 ) U_1.413040 (E  o
r E ))))Duration 44.778000s
Metrics:
  Cardinality: 101
  Measure(temporal operators): 4
  Measure(duration terms): 6
*)    
  end;
