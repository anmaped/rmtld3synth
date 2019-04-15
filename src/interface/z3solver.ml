
(*
   Interface for Z3 smt solver
 *)

open Batteries

open Z3
open Z3.SMT
open Z3.Solver
open Z3.Model
open Z3.Model.FuncInterp
open Z3.Expr
open Z3.AST
open Z3.FuncDecl
open Z3enums
open Z3.Symbol
open Z3.Arithmetic
open Z3.Tactic

open Helper

let parse_smtlibv2 smtlibv2_str =
  (*ignore (Z3.Log.open_ "Z3.log");*)
  verb (fun _ -> Printf.printf "Running Z3 version %s\n" Version.to_string ) ;
  verb (fun _ -> Printf.printf "Z3 full version string: %s\n" Version.full_version ) ;
  let ctx = (mk_context [("model", "true")]) in
  let exp = parse_smtlib2_string ctx smtlibv2_str [] [] [] [] in
  verb (fun _ -> print_endline "\nProblem:\n" ; print_endline (Z3.Expr.to_string exp) ) ;
  (ctx, [exp])

let solve_ ctx exp_lst =
  let tactic_set = and_then ctx (mk_tactic ctx "qe") (mk_tactic ctx "smt") []
  in let solver = mk_solver_t ctx tactic_set
  in let _ = add solver exp_lst
  in let sol = (string_of_status (check solver []), solver)
  in verb (fun _ -> print_endline (Statistics.to_string (get_statistics solver) ) ) ;
  sol


let get_model ctx solver =
  
  let model = match get_model solver with Some x -> x | None -> raise (Failure ("Model is not available.")) in

  verb (fun _ -> print_endline "\nModel:\n" ) ;
  verb (fun _ -> print_endline (Z3.Model.to_string model) ) ;
  model

let string_of_z3model model =
  Z3.Model.to_string model


let get_scheduler ctx model helper =
  verb (fun _ -> List.iter (fun el -> print_endline ( "kind: "^(string_of_int (int_of_decl_kind (get_decl_kind el) ) )^ " Name: " ^ ( get_string (get_name el) ) ) )  (get_decls model) ) ;
  
  let trace = List.find (fun el -> get_string (get_name el) = "trc") (get_decls model) in
  let trace_size = List.find (fun el -> get_string (get_name el) = "trc_size") (get_decls model) in

  let interp = (match get_func_interp model trace with Some x -> x | None -> raise (Failure ("Function interpret is not available."))) in
  verb (fun _ -> print_endline ("N interp: "^(string_of_int (get_num_entries interp)) ) ) ;
  verb (fun _ -> print_endline (Z3.Model.FuncInterp.to_string interp) ) ;

  let size_fun_interp =
  (
    match get_const_interp model trace_size with
    | Some x -> x
    | None -> raise (Failure ("Interpretation of the function (size_fun_interp) is not available."))
  ) in

  verb (fun _ -> print_endline ("\nTrace size: "^(Z3.Expr.to_string size_fun_interp)^"\n") ) ;

  (* set default size to 2 when trace_size is unknown/equal to (- 1); sum two to the trace_size value otherwise *)
  let size = if (Z3.Expr.to_string size_fun_interp) = "(- 1)" then 2 else int_of_string (Z3.Expr.to_string size_fun_interp) + 2 in

  let exp2 = get_else interp in

  (* replace variables and evaluate *)
  List.fold_right (fun a b ->
    let rpl_exp = Expr.substitute_vars exp2 [Integer.mk_numeral_i ctx a] in
    let evaluated_expression = (match eval model rpl_exp true with Some x -> x | None -> raise (Failure ("Evaluation is not available."))) in
    let exp_value = (Z3.AST.to_string (ast_of_expr evaluated_expression)) in
    verb (fun _ -> print_endline ("NM: "^exp_value) ; ) ;
    verb (fun _ -> print_endline (string_of_int a^" -> "^ (find_proposition_rev_hashtbl (int_of_string exp_value)  helper)) ) ;
    (find_proposition_rev_hashtbl (int_of_string exp_value)  helper)::b
    (* print_endline (Z3.AST.to_string (ast_of_expr rpl_exp)) ; *)
  ) (List.of_enum (0--size)) []
  