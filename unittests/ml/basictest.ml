(* Basic test suite for RMTLD3 library
 * 
 * This module contains unit tests for temporal logic operations including:
 * - Basic boolean operations (true, false, propositions)
 * - Duration constraints
 * - Until operator and temporal formulas
 * - Complexity analysis of nested temporal operators
 *
 * Note:
 *   This was the initial test suite for RMTLD3 library development.
 *   Some formulas overlap with unittest.ml test cases.
 *)

open Rmtld3
open Helper

let () =
  Printf.printf "\n%s\n" (String.make 80 '=') ;
  Printf.printf "RMTLD3 Basic Test Suite\n" ;
  Printf.printf "%s\n\n" (String.make 80 '=') ;
  (* debuging flag *)
  let activate_tests = ref false in
  activate_tests := true ;
  if activate_tests = ref true then (
    (* basic tests for RMTLD3 *)
    let test1_trace =
      [ ("A", 0.)
      ; ("B", 1.)
      ; ("A", 2.)
      ; ("B", 3.)
      ; ("B", 4.)
      ; ("A", 5.)
      ; ("C", 6.) ]
    in
    let test2_trace =
      [ ("A", 0.)
      ; ("C", 1.)
      ; ("A", 2.)
      ; ("B", 3.)
      ; ("B", 4.)
      ; ("A", 5.)
      ; ("C", 6.) ]
    in
    let test3_trace =
      [ ("A", 0.)
      ; ("A", 1.)
      ; ("A", 2.)
      ; ("A", 3.)
      ; ("A", 4.)
      ; ("A", 5.)
      ; ("A", 6.)
      ; ("B", 9.) ]
    in
    let t_k = environment test1_trace in
    (* generate environment based on the trace *)
    let t_k2 = environment test2_trace in
    (* generate environment based on the trace *)
    let t_k3 = environment test3_trace in
    (* generate environment based on the trace *)
    let t_u = lenv in
    let pass_test lb v =
      Printf.printf "%s -> " lb ;
      if v = True then Printf.printf "True\n"
      else Printf.printf "%s \n" (b3_to_string v)
    in
    (* basic tests set *)
    pass_test "true " (eval (t_k, t_u, 0.) mtrue) ;
    pass_test "false" (eval (t_k, t_u, 0.) (Not mfalse)) ;
    pass_test "A    " (eval (t_k, t_u, 0.) (Prop "A")) ;
    pass_test "~C   " (eval (t_k, t_u, 0.) (Not (Prop "C"))) ;
    (* duration tests set *)
    pass_test "int 5 A < 2.0(0)1  "
      (eval (t_k, t_u, 0.)
         (LessThan
            ( Duration (Constant 5., Prop "A")
            , Constant (2. +. (epsilon_float *. 3.)) ) ) ) ;
    pass_test "~(int 5 A < 2)     "
      (eval (t_k, t_u, 0.)
         (Not (LessThan (Duration (Constant 5., Prop "A"), Constant 2.))) ) ;
    (* until tests set *)
    pass_test "B U A       "
      (eval (t_k, t_u, 0.) (Until (3., Prop "B", Prop "A"))) ;
    pass_test "~(C U B)    "
      (eval (t_k, t_u, 0.) (Not (Until (3., Prop "C", Prop "B")))) ;
    pass_test "(A U B)     "
      (eval (t_k, t_u, 0.) (Until (3., Prop "A", Prop "B"))) ;
    pass_test "~(F 6 C)    "
      (eval (t_k, t_u, 0.) (Not (meventually 6. (Prop "C")))) ;
    pass_test "~(F 5.9 C)  "
      (eval (t_k, t_u, 0.) (Not (meventually 5.9 (Prop "C")))) ;
    pass_test "F 6.0(0)1 C "
      (eval (t_k, t_u, 0.)
         (meventually (6. +. (epsilon_float *. 3.)) (Prop "C")) ) ;
    pass_test "F_1.0(0)1 ~A"
      (eval (t_k, t_u, 0.)
         (meventually (1. +. epsilon_float) (Not (Prop "A"))) ) ;
    (* set of tests for temporal formulas *)
    pass_test "~(A -> (F_1 C))   "
      (eval (t_k, t_u, 0.)
         (Not (mimplies (Prop "A") (meventually 1. (Prop "C")))) ) ;
    pass_test "A -> (F_1.0(0)1 B)"
      (eval (t_k, t_u, 0.)
         (mimplies (Prop "A") (meventually (1. +. epsilon_float) (Prop "B"))) ) ;
    pass_test "G_2 ~A" (eval (t_k2, t_u, 0.) (malways 2. (Not (Prop "A")))) ;
    pass_test "G_4 (A -> (F_2 B))"
      (eval (t_k, t_u, 0.)
         (malways 4. (mimplies (Prop "A") (meventually 2. (Prop "B")))) ) ;
    pass_test "G_9.1 (A -> (F_2 B))"
      (eval (t_k, t_u, 0.)
         (malways 9.1 (mimplies (Prop "A") (meventually 2. (Prop "B")))) ) ;
    (* complexity *)
    (* (y-2)*(x*(2*x))+((y-3)*x)+x *)
    count := 0 ;
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x + x *)
    pass_test "A U_10 *"
      (eval (t_k3, t_u, 0.) (Until (10., Prop "A", Prop "*"))) ;
    Printf.printf "count: %i\n" !count ;
    count := 0 ;
    (* 2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x + x *)
    pass_test "(A U_10 B) U_10 (A U_10 *)"
      (eval (t_k3, t_u, 0.)
         (Until
            ( 10.
            , Until (10., Prop "A", Prop "B")
            , Until (10., Prop "A", Prop "*") ) ) ) ;
    Printf.printf "count: %i\n" !count ;
    count := 0 ;
    (* 5*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x) +
       4*x *)
    pass_test "((A U_10 B) U_10 (A U_10 *) U_10 ((A U_10 B) U_10 A U_10 *)"
      (eval (t_k3, t_u, 0.)
         (Until
            ( 10.
            , Until
                ( 10.
                , Until (10., Prop "A", Prop "B")
                , Until (10., Prop "A", Prop "B") )
            , Until
                ( 10.
                , Until (10., Prop "A", Prop "B")
                , Until (10., Prop "A", Prop "*") ) ) ) ) ;
    Printf.printf "count: %i\n" !count ;
    count := 0 ;
    (* number of temporal operators: 15 *)
    (* 13*(2*(x-7)+2*(x-6)+2*(x-5)+2*(x-4)+2*(x-3)+2*(x-2)+2*(x-1)+2*x) + 12*x *)
    pass_test
      "(((A U_10 B) U_10 (A U_10 *)) U_10 ((A U_10 B) U_10 (A U_10 *)) U_10 \
       (((A U_10 B) U_10 (A U_10 *)) U_10 ((A U_10 B) U_10 (A U_10 *)))"
      (eval (t_k3, t_u, 0.)
         (Until
            ( 10.
            , Until
                ( 10.
                , Until
                    ( 10.
                    , Until (10., Prop "A", Prop "B")
                    , Until (10., Prop "A", Prop "B") )
                , Until
                    ( 10.
                    , Until (10., Prop "A", Prop "B")
                    , Until (10., Prop "A", Prop "B") ) )
            , Until
                ( 10.
                , Until
                    ( 10.
                    , Until (10., Prop "A", Prop "B")
                    , Until (10., Prop "A", Prop "B") )
                , Until
                    ( 10.
                    , Until (10., Prop "A", Prop "B")
                    , Until (10., Prop "A", Prop "*") ) ) ) ) ) ;
    Printf.printf "count: %i\n" !count ;
    (* binomial(n+(m-1), (m-1)) * 2^n *)
    (* let g_val = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in *)
    let g_val = [1; 2; 3; 4] in
    let g_val2 = List.map (fun a -> pow 2 a - 1) (List.tl g_val) in
    let lst1 = [1; 2; 3; 4; 5; 6; 7; 8] in
    List.iter
      (fun a ->
        let fm = gen_u_formula_with_triangle_pattern true a 10. in
        print_endline
          ( "D" ^ string_of_int a ^ ": "
          ^ string_of_int (asym_comp (lst1, 0., 0.) fm)
          ^ " n: "
          ^ string_of_int (snd (measure_formula fm)) )
        (* print_endline (Sexp.to_string_hum (sexp_of_rmtld3_fm fm)); *) )
      g_val ;
    List.iter
      (fun a ->
        let fm =
          gen_u_formula_with_maximum_prop_evaluation a 10. (List.length lst1)
        in
        print_endline
          ( "N" ^ string_of_int a ^ ": "
          ^ string_of_int (asym_comp (lst1, 0., 0.) fm)
          ^ " n: "
          ^ string_of_int (snd (measure_formula fm)) ) )
      g_val2 ) ;
  Printf.printf "%s\n\n" (String.make 80 '=')
