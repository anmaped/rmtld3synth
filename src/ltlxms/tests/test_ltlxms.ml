open Z3

(* Initialize Z3 context - This should be done once per test run or per group
   of tests needing the same decls *)
(* We will move ctx and decls initialization inside test_operation or make
   them global if all tests share them *)
let verbose = true

(* Debugging *)
let debug msg = if verbose then Printf.printf "%s\n" msg

(* Read JSON input and deserialize into term or formula *)
let read_input json_file =
  debug (Printf.sprintf "Reading input from %s" json_file) ;
  let json = Yojson.Safe.from_file json_file in
  debug (Printf.sprintf "Parsed JSON: %s" (Yojson.Safe.to_string json)) ;
  try
    let term = Ltlxms.Syntax.term_of_yojson json in
    `Term term
  with _ -> (
    try
      let formula = Ltlxms.Syntax.formula_of_yojson json in
      `Formula formula
    with exn ->
      failwith
        (Printf.sprintf "Failed to parse input: %s" (Printexc.to_string exn)) )

(* Read expected output *)
let read_expected_output output_file =
  debug (Printf.sprintf "Reading expected output from %s" output_file) ;
  let ic = open_in output_file in
  let rec read_all_lines acc =
    try
      let line = input_line ic in
      read_all_lines (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let content = read_all_lines "" in
  close_in ic ;
  debug (Printf.sprintf "Expected output: %s" content) ;
  content

(* Perform operation and compare with expected output *)
(* Modified to accept ctx and decls *)
let test_operation ctx decls input_file output_file =
  debug "Starting operation test" ;
  let input = read_input input_file in
  let expected_output = read_expected_output output_file in
  debug "Transforming input into Z3 expression" ;
  (* Convert the input into a Z3 expression, passing decls *)
  let actual_output_expr =
    match input with
    | `Term term -> Ltlxms.Encoding.Formulas.term_to_z3 ctx decls term
    | `Formula formula ->
        Ltlxms.Encoding.Formulas.formula_to_z3 ctx decls formula
  in
  (* Convert the Z3 expression to a string *)
  let actual_output = String.trim (Z3.Expr.to_string actual_output_expr) in
  let expected_output = String.trim expected_output in
  (* Compare actual output with expected output *)
  if actual_output = expected_output then
    Printf.printf "\027[32m✓ Test passed for %s!\027[0m\n\n" input_file
  else (
    Printf.printf "\027[31mTest failed for %s!\027[0m\n" input_file ;
    Printf.printf "Expected: %s\n" expected_output ;
    Printf.printf "Actual: %s\n" actual_output )

(* Run all tests *)
let () =
  (* Initialize Z3 context and temporal declarations ONCE for all tests in
     this run *)
  let ctx = mk_context [] in
  let bool_sort = Z3.Boolean.mk_sort ctx in
  let shared_next_decl =
    Z3.FuncDecl.mk_func_decl_s ctx "next" [bool_sort] bool_sort
  in
  let shared_nextt_decl =
    Z3.FuncDecl.mk_func_decl_s ctx "nextt" [bool_sort] bool_sort
  in
  let shared_previous_decl =
    Z3.FuncDecl.mk_func_decl_s ctx "previous" [bool_sort] bool_sort
  in
  let shared_once_decl =
    Z3.FuncDecl.mk_func_decl_s ctx "once" [bool_sort] bool_sort
  in
  let shared_always_decl =
    Z3.FuncDecl.mk_func_decl_s ctx "always" [bool_sort] bool_sort
  in
  let shared_eventually_decl =
    Z3.FuncDecl.mk_func_decl_s ctx "eventually" [bool_sort] bool_sort
  in
  let decls : Ltlxms.Encoding.temporal_operator_decls =
    { next_decl= shared_next_decl
    ; nextt_decl= shared_nextt_decl
    ; previous_decl= shared_previous_decl
    ; once_decl= shared_once_decl
    ; always_decl= shared_always_decl
    ; eventually_decl= shared_eventually_decl }
  in
  let files =
    [| "test1.json"
    ; "test2.json"
    ; "test3.json"
    ; "test4.json"
    ; "test5.json"
    ; "test6.json"
    ; "test7.json"
    ; "test8.json"
    ; "test9.json"
    ; "test10.json"
    ; "test11.json"
    ; "test12.json"
    ; "test13.json"
    ; "test14.json"
    ; "test15.json"
    ; "test16.json"
    ; "test17.json"
    ; "test18.json"
    ; "test19.json"
    ; "test20.json"
    ; "test21.json" |]
  in
  Array.iter
    (fun file ->
      if Filename.check_suffix file ".json" then
        let input_file = file in
        let output_file = Filename.chop_suffix file ".json" ^ ".out" in
        if Sys.file_exists output_file then (
          debug
            (Printf.sprintf
               "# Running test with input file: %s and output file: %s"
               input_file output_file ) ;
          (* Pass ctx and decls to test_operation *)
          test_operation ctx decls input_file output_file )
        else
          Printf.printf "Output file %s not found for input file %s\n"
            output_file input_file )
    files
