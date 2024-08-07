(*
   Synthesis from RMTLD3 to Ocaml
*)

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
let synth_tm_constant value helper =
  ("(fun k s t -> Dsome(" ^ string_of_float value ^ ") )", "")

let synth_tm_variable name helper = failwith "No freevariables allowed."

let synth_tm_duration (tm_call, tm_body) (fm_call, fm_body) helper =
  has_tm_dur := true;
  ("(compute_tm_duration " ^ tm_call ^ " " ^ fm_call ^ ")", tm_body ^ fm_body)

let synth_tm_duration_body =
  "let compute_tm_duration tm fm k u t =\n\
  \  let dt = (t,tm k u t) in\n\n\
  \  let indicator_function (k,u) t phi = if fm k u t = True then 1. else 0. in\n\
  \  let riemann_sum m dt (i,i') phi =\n\
  \    (* dt=(t,t') and t in ]i,i'] or t' in ]i,i'] *)\n\
  \    count_duration := !count_duration + 1 ;\n\
  \    let t,t' = dt in\n\
  \    if i <= t && t < i' then\n\
  \      (* lower bound *)\n\
  \      (i'-.t) *. (indicator_function m t phi)\n\
  \    else (\n\
  \      if i <= t' && t' < i' then\n\
  \        (* upper bound *)\n\
  \        (t'-.i) *. (indicator_function m t' phi)\n\
  \      else\n\
  \        (i'-.i) *. (indicator_function m i phi)\n\
  \    ) in\n\
  \  let eval_eta m dt phi x = fold_left (fun s (prop,(i,t')) -> (riemann_sum\n\
  \  m dt (i,t') phi) +. s) 0. x in\n\
  \    match dt with\n\
  \  | (t, Dsome(t')) when k.duration_of_trace >= t +. t'  ->\n\
  \    Dsome( eval_eta (k, u) (t,t') fm (sub_k (k, u, t) t') )\n\
  \  | _ -> Dnone\n"

let synth_tm_plus cmptr1 cmptr2 helper =
  ( "(fun k s t -> match (" ^ fst cmptr1 ^ " k s t," ^ fst cmptr2
    ^ " k s t) with | Dsome(v1),Dsome(v2) -> Dsome(v1 +. v2) | _ -> Dnone )",
    snd cmptr1 ^ snd cmptr2 )

let synth_tm_times cmptr1 cmptr2 helper =
  ( "(fun k s t -> match (" ^ fst cmptr1 ^ " k s t," ^ fst cmptr2
    ^ " k s t) with | Dsome(v1),Dsome(v2) -> Dsome(v1 *. v2) | _ -> Dnone )",
    snd cmptr1 ^ snd cmptr2 )

let synth_fm_true helper = ("(fun k s t -> True)", "")

let synth_fm_p p helper =
  let p_id = find_proposition_rev_hashtbl p helper in
  ("(fun k s t -> k.evaluate k.trace \"" ^ p_id ^ "\" t)", "")

let synth_fm_not cmpfm helper =
  ("(fun k s t -> b3_not (" ^ fst cmpfm ^ " k s t))", snd cmpfm)

let synth_fm_or cmpfm1 cmpfm2 helper =
  ( "(fun k s t -> b3_or (" ^ fst cmpfm1 ^ " k s t) (" ^ fst cmpfm2 ^ " k s t))",
    snd cmpfm1 ^ snd cmpfm2 )

let synth_fm_less cmptr1 cmptr2 helper =
  ( "(fun k s t -> b3_lessthan (" ^ fst cmptr1 ^ " k s t) (" ^ fst cmptr2
    ^ " k s t))",
    snd cmptr1 ^ snd cmptr2 )

let synth_fm_uless gamma sf1 sf2 helper =
  has_fm_uless := true;
  ( "(compute_uless " ^ string_of_float gamma ^ " " ^ fst sf1 ^ " " ^ fst sf2
    ^ ")",
    snd sf1 ^ snd sf2 )

let synth_fm_ueq gamma sf1 sf2 helper =
  has_fm_ueq := true;
  ( "(compute_ueq " ^ string_of_float gamma ^ " " ^ fst sf1 ^ " " ^ fst sf2 ^ ")",
    snd sf1 ^ snd sf2 )

let synth_fm_ulesseq gamma sf1 sf2 helper =
  has_fm_ulesseq := true;
  ( "(compute_ulesseq " ^ string_of_float gamma ^ " " ^ fst sf1 ^ " " ^ fst sf2
    ^ ")",
    snd sf1 ^ snd sf2 )

let synth_fm_uless_body =
  "let compute_uless gamma f1 f2 k u t =\n\
  \  let m = (k,u,t) in\n\
  \  let eval_i b1 b2 =\n\
  \    if b2 <> False then\n\
  \      b3_to_b4 b2\n\
  \    else if b1 <> True then\n\
  \      b3_to_b4 b1\n\
  \    else\n\
  \      Symbol\n\
  \  in\n\n\
  \  let eval_b (k,u,t) f1 f2 v =\n\
  \    if v <> Symbol then\n\
  \      v\n\
  \    else\n\
  \      eval_i (f1 k u t) (f2 k u t)\n\
  \  in\n\n\
  \  let eval_fold (k,u,t) f1 f2 x =\n\
  \    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 \
   v, ii2)) (Symbol,t) x)\n\
  \  in\n\n\
  \  if not (gamma >= 0.) then\n\
  \    raise  (Failure \"Gamma of U< operator is a non-negative value\")\n\
  \  else\n\
  \  begin\n\
  \    let k,_,t = m in\n\
  \    let subk = sub_k m gamma in\n\
  \    let eval_c = eval_fold m f1 f2 subk in\n\
  \    if eval_c = Symbol then\n\
  \      if k.duration_of_trace <= (t +. gamma) then\n\
  \        Unknown\n\
  \      else (\n\
  \        False\n\
  \      )\n\
  \    else\n\
  \      b4_to_b3 eval_c\n\
  \  end\n"

(* differs from uless on "eval_i" and "sub_k m (gamma +. (epsilon_float *. 100.) )" *)
let synth_fm_ueq_body =
  "let compute_ueq gamma f1 f2 k u t =\n\
  \  let t_i = t in\n\
  \  let m = (k,u,t) in\n\
  \  let eval_i t b1 b2 =\n\
  \    if t -. t_i = gamma then\n\
  \      b3_to_b4 b2\n\
  \    else if b1 <> True then\n\
  \      b3_to_b4 b1\n\
  \    else\n\
  \      Symbol\n\
  \  in\n\n\
  \  let eval_b (k,u,t) f1 f2 v =\n\
  \    if v <> Symbol then\n\
  \      v\n\
  \    else\n\
  \      eval_i t (f1 k u t) (f2 k u t)\n\
  \  in\n\n\
  \  let eval_fold (k,u,t) f1 f2 x =\n\
  \    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 \
   v, ii2)) (Symbol,t) x)\n\
  \  in\n\n\
  \  if not (gamma >= 0.) then\n\
  \    raise  (Failure \"Gamma of U= operator is a non-negative value\")\n\
  \  else\n\
  \  begin\n\
  \    let k,_,t = m in\n\
  \    let subk = sub_k m (gamma +. (epsilon_float *. 100.) ) in\n\
  \    let eval_c = eval_fold m f1 f2 subk in\n\
  \    if eval_c = Symbol then\n\
  \      if k.duration_of_trace <= (t +. gamma) then\n\
  \        Unknown\n\
  \      else (\n\
  \        False\n\
  \      )\n\
  \    else\n\
  \      b4_to_b3 eval_c\n\
  \  end\n"

(* differs from uless on "sub_k m (gamma +. (epsilon_float *. 100.) )" *)
let synth_fm_ulesseq_body =
  "let compute_uless gamma f1 f2 k u t =\n\
  \  let m = (k,u,t) in\n\
  \  let eval_i b1 b2 =\n\
  \    if b2 <> False then\n\
  \      b3_to_b4 b2\n\
  \    else if b1 <> True && b2 = False then\n\
  \      b3_to_b4 b1\n\
  \    else\n\
  \      Symbol\n\
  \  in\n\n\
  \  let eval_b (k,u,t) f1 f2 v =\n\
  \    if v <> Symbol then\n\
  \      v\n\
  \    else\n\
  \      eval_i (f1 k u t) (f2 k u t)\n\
  \  in\n\n\
  \  let eval_fold (k,u,t) f1 f2 x =\n\
  \    fst (fold_left (fun (v,t') (prop,(ii1,ii2)) -> (eval_b (k, u, t') f1 f2 \
   v, ii2)) (Symbol,t) x)\n\
  \  in\n\n\
  \  if not (gamma >= 0.) then\n\
  \    raise  (Failure \"Gamma of U<= operator is a non-negative value\")\n\
  \  else\n\
  \  begin\n\
  \    let k,_,t = m in\n\
  \    let subk = sub_k m (gamma +. (epsilon_float *. 100.) ) in\n\
  \    let eval_c = eval_fold m f1 f2 subk in\n\
  \    if eval_c = Symbol then\n\
  \      if k.duration_of_trace <= (t +. gamma) then\n\
  \        Unknown\n\
  \      else (\n\
  \        False\n\
  \      )\n\
  \    else\n\
  \      b4_to_b3 eval_c\n\
  \  end\n"

let synth_fm_sless gamma (sf1, a) (sf2, b) helper =
  failwith ("S[<" ^ string_of_float gamma ^ "] Not Implemented!")

let synth_fm_seq gamma (sf1, a) (sf2, b) helper =
  failwith ("S[=" ^ string_of_float gamma ^ "] Not Implemented!")

let synth_ocaml compute helper =
  (* out_file cluster_name monitor_period *)
  print_endline "Current Configuration:";
  print_settings helper;

  let expressions = get_all_setting_formula "input_exp" helper in

  print_endline "Expression(s) selected to encode:";
  List.iter
    (fun exp ->
      print_plaintext_formula exp;
      print_endline "")
    expressions;

  let cpp_monitor_lst =
    List.fold_right
      (fun exp lst ->
        let mon_call, mon_body = compute exp helper in
        ((mon_call, mon_body), string_of_int (List.length lst)) :: lst)
      expressions []
  in

  let pair_to_string ((x, _), y) = "(" ^ x ^ ", " ^ y ^ ")" in
  let name =
    insert_string
      (get_setting_string "rtm_monitor_name_prefix" helper)
      (String.sub
         (Digest.string
            (String.concat "" (List.map pair_to_string cpp_monitor_lst))
         |> Digest.to_hex)
         0 4)
      '%'
  in

  let monitor_name = insert_string name "compute" '#' in

  let code1 =
    "(* This file was automatically generated from rmtld3synth tool version\n"
    ^ get_setting_string "version" helper
    ^ ". *)\n\n\
       open List\n\
       open Rmtld3\n\n\
       module type Trace = sig val trc : trace end\n\n"
    ^ List.fold_right
        (fun ((function_call, body), n) str ->
          "module "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n ^ "  ( T : Trace  ) = struct \n" ^ body
          ^ (if !has_fm_uless then synth_fm_uless_body else "")
          ^ (if !has_fm_ueq then synth_fm_ueq_body else "")
          ^ (if !has_fm_ulesseq then synth_fm_ulesseq_body else "")
          ^ (if !has_tm_dur then synth_tm_duration_body else "")
          ^ "  let env = environment T.trc\n\
            \  let lg_env = logical_environment\n\
            \  let t = 0.\n\
            \  let mon = " ^ function_call ^ " env lg_env t\nend\n\n" ^ str)
        cpp_monitor_lst ""
  in

  try
    let out_dir = get_setting_string "out_dir" helper in

    print_endline "Generated Output Files:";

    let monitor_name =
      String.capitalize_ascii (insert_string name "compute" '#')
    in

    let stream = open_out (out_dir ^ "/" ^ monitor_name ^ ".ml") in
    Printf.fprintf stream "%s\n" code1;
    close_out stream;

    print_endline (out_dir ^ "/" ^ monitor_name ^ ".ml")
  with Not_found -> (
    try
      let out_file = get_setting_string "out_file" helper in

      print_endline "Generated Output Files:";

      let monitor_name =
        String.capitalize_ascii (insert_string name "compute" '#')
      in

      let stream = open_out out_file in
      Printf.fprintf stream "%s\n"
        (Str.global_replace (Str.regexp monitor_name)
           (out_file |> Filename.basename |> Filename.remove_extension
          |> String.capitalize_ascii)
           code1);
      close_out stream;

      print_endline out_file
    with Not_found -> (* print to console *)
                      print_endline code1)

let synth_ocaml_unittests () =
  (* debuging flag *)
  let activate_tests = ref false in
  activate_tests := true;

  if activate_tests = ref true then (
    (* basic tests for RMTLD3 *)
    let test1_trace =
      [
        ("A", (0., 1.));
        ("B", (1., 2.));
        ("A", (2., 3.));
        ("B", (3., 4.));
        ("B", (4., 5.));
        ("A", (5., 6.));
        ("C", (6., 6.5));
      ]
    in
    let test2_trace =
      [
        ("A", (0., 1.));
        ("C", (1., 2.));
        ("A", (2., 3.));
        ("B", (3., 4.));
        ("B", (4., 5.));
        ("A", (5., 6.));
        ("C", (6., 6.5));
      ]
    in
    let test3_trace =
      [
        ("A", (0., 1.));
        ("A", (1., 2.));
        ("A", (2., 3.));
        ("A", (3., 4.));
        ("A", (4., 5.));
        ("A", (5., 6.));
        ("A", (6., 9.));
        ("B", (9., 20.));
      ]
    in
    let t_k = environment test1_trace in
    (* generate environment based on the trace *)
    let t_k2 = environment test2_trace in
    (* generate environment based on the trace *)
    let t_k3 = environment test3_trace in

    (* generate environment based on the trace *)
    let t_u = logical_environment in
    let pass_test lb v =
      Printf.printf "%s -> " lb;
      if v = True then Printf.printf "[PASSED]\n"
      else Printf.printf "[FAILED] %s \n" (b3_to_string v)
    in

    (* basic tests set *)
    pass_test "true " (compute (t_k, t_u, 0.) mtrue);
    pass_test "false" (compute (t_k, t_u, 0.) (Not mfalse));
    pass_test "A    " (compute (t_k, t_u, 0.) (Prop "A"));
    pass_test "~C   " (compute (t_k, t_u, 0.) (Not (Prop "C")));

    (* duration tests set *)
    pass_test "int 5 A < 2.0(0)1  "
      (compute (t_k, t_u, 0.)
         (LessThan
            ( Duration (Constant 5., Prop "A"),
              Constant (2. +. (epsilon_float *. 3.)) )));
    pass_test "~(int 5 A < 2)     "
      (compute (t_k, t_u, 0.)
         (Not (LessThan (Duration (Constant 5., Prop "A"), Constant 2.))));

    (* until tests set *)
    pass_test "B U A       "
      (compute (t_k, t_u, 0.) (Until (3., Prop "B", Prop "A")));
    pass_test "~(C U B)    "
      (compute (t_k, t_u, 0.) (Not (Until (3., Prop "C", Prop "B"))));
    pass_test "(A U B)     "
      (compute (t_k, t_u, 0.) (Until (3., Prop "A", Prop "B")));
    pass_test "~(F 6 C)    "
      (compute (t_k, t_u, 0.) (Not (meventually 6. (Prop "C"))));
    pass_test "~(F 5.9 C)  "
      (compute (t_k, t_u, 0.) (Not (meventually 5.9 (Prop "C"))));
    pass_test "F 6.0(0)1 C "
      (compute (t_k, t_u, 0.)
         (meventually (6. +. (epsilon_float *. 3.)) (Prop "C")));
    pass_test "F_1.0(0)1 ~A"
      (compute (t_k, t_u, 0.)
         (meventually (1. +. epsilon_float) (Not (Prop "A"))));

    (* set of tests for temporal formulas *)
    pass_test "~(A -> (F_1 C))   "
      (compute (t_k, t_u, 0.)
         (Not (mimplies (Prop "A") (meventually 1. (Prop "C")))));
    pass_test "A -> (F_1.0(0)1 B)"
      (compute (t_k, t_u, 0.)
         (mimplies (Prop "A") (meventually (1. +. epsilon_float) (Prop "B"))));
    pass_test "G_2 ~A" (compute (t_k2, t_u, 0.) (malways 2. (Not (Prop "A"))));
    pass_test "G_4 (A -> (F_2 B))"
      (compute (t_k, t_u, 0.)
         (malways 4. (mimplies (Prop "A") (meventually 2. (Prop "B")))));
    pass_test "G_9.1 (A -> (F_2 B))"
      (compute (t_k, t_u, 0.)
         (malways 9.1 (mimplies (Prop "A") (meventually 2. (Prop "B")))));

    (* complexity *)
    (* (y-2)*(x*(2*x))+((y-3)*x)+x *)
    count := 0;
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x + x *)
    pass_test "A U_10 *"
      (compute (t_k3, t_u, 0.) (Until (10., Prop "A", Prop "*")));
    Printf.printf "count: %i\n" !count;

    count := 0;
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x + x *)
    pass_test "(A U_10 B) U_10 (A U_10 *)"
      (compute (t_k3, t_u, 0.)
         (Until
            ( 10.,
              Until (10., Prop "A", Prop "B"),
              Until (10., Prop "A", Prop "*") )));
    Printf.printf "count: %i\n" !count;

    count := 0;
    (* 5*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x) + 4*x *)
    pass_test "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)"
      (compute (t_k3, t_u, 0.)
         (Until
            ( 10.,
              Until
                ( 10.,
                  Until (10., Prop "A", Prop "B"),
                  Until (10., Prop "A", Prop "B") ),
              Until
                ( 10.,
                  Until (10., Prop "A", Prop "B"),
                  Until (10., Prop "A", Prop "*") ) )));
    Printf.printf "count: %i\n" !count;

    count := 0;
    (* number of temporal operators: 15 *)
    (* 13*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x) + 12*x *)
    pass_test
      "(((A U_10 B) U_10 (A U_10 *)) U_10 ((A U_10 B) U_10 (A U_10 *)) U_10 \
       (((A U_10 B) U_10 (A U_10 *)) U_10 ((A U_10 B) U_10 (A U_10 *)))"
      (compute (t_k3, t_u, 0.)
         (Until
            ( 10.,
              Until
                ( 10.,
                  Until
                    ( 10.,
                      Until (10., Prop "A", Prop "B"),
                      Until (10., Prop "A", Prop "B") ),
                  Until
                    ( 10.,
                      Until (10., Prop "A", Prop "B"),
                      Until (10., Prop "A", Prop "B") ) ),
              Until
                ( 10.,
                  Until
                    ( 10.,
                      Until (10., Prop "A", Prop "B"),
                      Until (10., Prop "A", Prop "B") ),
                  Until
                    ( 10.,
                      Until (10., Prop "A", Prop "B"),
                      Until (10., Prop "A", Prop "*") ) ) )));
    Printf.printf "count: %i\n" !count;

    (* binomial(n+(m-1), (m-1)) * 2^n *)
    let g_val = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
    let g_val2 = map (fun a -> pow 2 a - 1) (tl g_val) in
    let lst1 = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
    iter
      (fun a ->
        let fm = gen_u_formula_with_triangle_pattern true a 10. in
        print_endline
          ("D" ^ string_of_int a ^ ": "
          ^ string_of_int (asym_comp (lst1, 0., 0.) fm)
          ^ " n: "
          ^ string_of_int (snd (measure_formula fm)))
        (* print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm fm)); *))
      g_val;

    iter
      (fun a ->
        let fm =
          gen_u_formula_with_maximum_prop_evaluation a 10. (List.length lst1)
        in
        print_endline
          ("N" ^ string_of_int a ^ ": "
          ^ string_of_int (asym_comp (lst1, 0., 0.) fm)
          ^ " n: "
          ^ string_of_int (snd (measure_formula fm))))
      g_val2)
