(*
   Interface for Z3 SMT solver (> 4.7.1)
 *)

open Z3
open Z3.SMT
open Z3.Solver
open Z3.Model
open Z3.Expr
open Z3.FuncDecl
open Z3enums
open Z3.Symbol
open Z3.Arithmetic
open Z3.Tactic
open Helper

let parse_smtlibv2 smtlibv2_str =
  (* ignore (Z3.Log.open_ "Z3.log"); *)
  verb (fun _ -> Printf.printf "Running Z3 version %s\n" Version.to_string);
  verb (fun _ ->
      Printf.printf "Z3 full version string: %s\n" Version.full_version);
  let ctx = mk_context [ ("model", "true") ] in
  let exp = parse_smtlib2_string ctx smtlibv2_str [] [] [] [] in
  verb (fun _ ->
      print_endline "\nProblem:\n";
      print_endline (Z3.AST.ASTVector.to_string exp));
  (ctx, Z3.AST.ASTVector.to_expr_list exp)

let solve_ ctx exp =
  let tactic_set = and_then ctx (mk_tactic ctx "qe") (mk_tactic ctx "smt") [] in
  let solver = mk_solver_t ctx tactic_set in
  let _ = add solver exp in
  let sol = (string_of_status (check solver []), solver) in
  verb (fun _ -> print_endline (Statistics.to_string (get_statistics solver)));
  sol

let get_model _ solver =
  let model =
    match get_model solver with
    | Some x -> x
    | None -> raise (Failure "Model is not available.")
  in
  verb (fun _ -> print_endline "\nModel:\n");
  verb (fun _ -> print_endline (Z3.Model.to_string model));
  model

let string_of_z3model model = Z3.Model.to_string model

let get_scheduler ctx model helper =
  verb (fun _ -> print_endline "get_scheduler");
  verb (fun _ ->
      List.iter
        (fun el ->
          print_endline
            ("kind: "
            ^ string_of_int (int_of_decl_kind (get_decl_kind el))
            ^ " Name: "
            ^ get_string (get_name el)))
        (get_decls model));
  let seq_size =
    List.find
      (fun el -> get_string (get_name el) = "trc_size")
      (get_decls model)
  in
  let size_fun_interp =
    match get_const_interp model seq_size with
    | Some x -> x
    | None ->
        raise
          (Failure
             "Interpreting the function (size_fun_interp) is not possible.")
  in
  verb (fun _ ->
      print_endline
        ("\nSequence size: " ^ Z3.Expr.to_string size_fun_interp ^ "\n"));
  let size = int_of_string (Z3.Expr.to_string size_fun_interp) + 2 in
  (* retrieve time stamps *)
  let timed_seq_model_exp =
    try
      let timed_seq =
        List.find
          (fun el -> get_string (get_name el) = "trc_time")
          (get_decls model)
      in
      let exp_timed_seq = Z3.FuncDecl.apply timed_seq [] in
      verb (fun _ ->
          print_endline (Z3.AST.to_string (ast_of_expr exp_timed_seq)));
      let timed_seq_model_exp =
        match eval model exp_timed_seq true with
        | Some x -> x
        | None ->
            raise (Failure "Evaluation of exp_timed_seq is not available.")
      in
      verb (fun _ ->
          print_endline
            ("Timed Sequence: "
            ^ Z3.AST.to_string (ast_of_expr timed_seq_model_exp)));
      Some timed_seq_model_exp
    with _ -> None
  in
  (* retrieve sequence without time stamps *)
  let seq_model =
    try
      let seq =
        List.find (fun el -> get_string (get_name el) = "trc") (get_decls model)
      in
      let exp_seq = Z3.FuncDecl.apply seq [] in
      verb (fun _ -> print_endline (Z3.AST.to_string (ast_of_expr exp_seq)));
      let seq_model =
        match eval model exp_seq true with
        | Some x -> x
        | None -> raise (Failure "Evaluation of exp_seq is not available.")
      in
      verb (fun _ ->
          print_endline ("Sequence: " ^ Z3.AST.to_string (ast_of_expr seq_model)));
      seq_model
    with _ -> failwith "No sequence available."
  in
  let get_array_interp_from_exp exp i : string =
    (*
        check if array is a quantifier/lambda, a constant array, or a store array;
        error otherwise
    *)
    if Z3.Z3Array.is_constant_array exp || Z3.Z3Array.is_store exp then
      let exp = Z3.Z3Array.mk_select ctx exp (Integer.mk_numeral_i ctx i) in
      let evaluated_expression =
        match eval model exp true with
        | Some x -> x
        | None -> raise (Failure "Evaluation is not available.")
      in
      Z3.AST.to_string (ast_of_expr evaluated_expression)
    else if Z3.AST.is_quantifier (ast_of_expr exp) then
      let body =
        Z3.Quantifier.get_body (Z3.Quantifier.quantifier_of_expr exp)
      in
      let rpl_exp = Expr.substitute_vars body [ Integer.mk_numeral_i ctx i ] in
      let evaluated_expression =
        match eval model rpl_exp true with
        | Some x -> x
        | None -> raise (Failure "Evaluation is not available.")
      in
      Z3.AST.to_string (ast_of_expr evaluated_expression)
    else
      failwith
        ("trc is NOT a quantifier/lambda, a constant array, or a store array: '"
        ^ Z3.AST.to_string (ast_of_expr exp)
        ^ "' "
        ^ string_of_int
            (Z3enums.int_of_ast_kind (Z3.AST.get_ast_kind (ast_of_expr exp)))
        ^ " ")
  in
  (* replace variables and evaluate terms *)
  List.fold_right
    (fun a b ->
      (match timed_seq_model_exp with
      | Some v' ->
          let t' = get_array_interp_from_exp v' a in
          verb (fun _ -> print_endline ("t': " ^ t'));
          ()
      | None -> ());
      let exp_value = get_array_interp_from_exp seq_model a in
      verb (fun _ -> print_endline ("value: " ^ exp_value));
      verb (fun _ ->
          print_endline
            (string_of_int a ^ " -> "
            ^ find_proposition_rev_hashtbl (int_of_string exp_value) helper));
      find_proposition_rev_hashtbl (int_of_string exp_value) helper :: b)
    (of_enum 0 size) []
