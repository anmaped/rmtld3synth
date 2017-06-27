(*
   Synthesis from RMTLD3 to Ocaml
*)

open Batteries
open List
open Str

open Rmtld3


(* ocaml module api *)
let compute_tm_constant value helper = ("("^ (string_of_float value) ^")","")
let compute_tm_duration (tm_call,tm_body) (fm_call,fm_body) helper =
  let id = "xx" (* [TODO: put id dynamic based on helper ] *)
  in ("compute_term_duration"^ id ^" "^ fm_call ^" m (t, "^ tm_call ^") ", tm_body^fm_body^("
compute_term_duration"^ id ^" (k,u) dt formula =
        let indicator_function (k,u) t phi = if compute (k,u,t) phi = True then 1. else 0. in
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
        eval_eta (k,u) dt formula (sub_k (k,u,t) t')
"
))
let compute_tm_plus cmptr1 cmptr2 helper = ("","")
let compute_tm_times cmptr1 cmptr2 helper = ("","")
let compute_fm_p p helper = ("(fun k s t -> env.evaluate env.trace \""^ p ^"\" t)","")
let compute_fm_not cmpfm helper = ("b3_not ("^ fst cmpfm ^" env lg_env t)", snd cmpfm)
let compute_fm_or cmpfm1 cmpfm2 helper = ("b3_or ("^ fst cmpfm1 ^" env lg_env t) ("^ fst cmpfm2 ^" env lg_env t)", (snd cmpfm1)^(snd cmpfm2))
let compute_fm_less cmptr1 cmptr2 helper = ("","")
let compute_fm_uless gamma sf1 sf2 helper = ("","")



let synth_ocaml_compute (out_file,out_dir) cluster_name monitor_name monitor_period formula compute helper =
  let mon_call,mon_body = compute formula helper
  in let mon = "
open Rmtld3
module type Trace = sig val trc : trace end
(* one trace :: module OneTrace : Trace = struct let trc = [(\"a\",(1.,2.))] end *)

module "^ (String.capitalize_ascii monitor_name) ^"  ( T : Trace  ) = struct \n"^ mon_body ^"
  let env = environment T.trc
  let lg_env = logical_environment
  let t = 0.
  let mon = "^ mon_call ^"
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
