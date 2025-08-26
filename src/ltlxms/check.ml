open Z3
open Z3.Arithmetic
open Sequence.Snapshot
open Syntax
open Encoding


let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

(* Helper to get all car Ids from the trace *)
let get_all_car_ids (parsed_trace : Sequence.Snapshot.trace) : int list =
  parsed_trace.trace
  |> List.concat_map (fun event -> List.map (fun e -> e.id) event.elements)
  |> List.sort_uniq compare

let run ~trace_file ~property_file ~debug =
  let debug_mode = debug > 0 in
  let ctx = create_z3_context () in
  let bool_sort = Boolean.mk_sort ctx in
  let shared_next_decl =
    FuncDecl.mk_func_decl_s ctx "next" [bool_sort] bool_sort
  in
  let shared_nextt_decl =
    FuncDecl.mk_func_decl_s ctx "nextt" [bool_sort] bool_sort
  in
  let shared_previous_decl =
    FuncDecl.mk_func_decl_s ctx "previous" [bool_sort] bool_sort
  in
  let shared_once_decl =
    FuncDecl.mk_func_decl_s ctx "once" [bool_sort] bool_sort
  in
  let shared_always_decl =
    FuncDecl.mk_func_decl_s ctx "always" [bool_sort] bool_sort
  in
  let shared_eventually_decl =
    FuncDecl.mk_func_decl_s ctx "eventually" [bool_sort] bool_sort
  in
  let temporal_decls : temporal_operator_decls =
    { next_decl= shared_next_decl
    ; nextt_decl= shared_nextt_decl
    ; previous_decl= shared_previous_decl
    ; once_decl= shared_once_decl
    ; always_decl= shared_always_decl
    ; eventually_decl= shared_eventually_decl }
  in
  let json = Yojson.Basic.from_file trace_file in
  let parsed_trace = parse json in
  (* get size of the trace since trace is finite *)
  let num_snapshots = List.length parsed_trace.trace in
  if debug_mode then
    Printf.printf "Number of timestamps (events) in trace: %d\n"
      num_snapshots ;
  if num_snapshots = 0 then failwith "Trace is empty" ;
  if num_snapshots < 2 then
    failwith
      "Trace needs at least 2 timestamps to check 'next' behavior \
       meaningfully." ;
  let x_var = Real.mk_const_s ctx "x" in
  let y_var = Real.mk_const_s ctx "y" in
  let car_ids = get_all_car_ids parsed_trace in
  let car_event_expressions =
    List.map
      (fun car_id ->
        ( car_id
        , get_car_geometric_expressions ctx parsed_trace car_id x_var y_var
        ) )
      car_ids
  in
  List.iter
    (fun (car_id, exprs) ->
      if List.length exprs <> num_snapshots then
        failwith
          (Printf.sprintf
             "Could not extract expressions for all events for car %d" car_id )
      )
    car_event_expressions ;
  let property_json = Yojson.Safe.from_file property_file in
  let property_formula = formula_of_yojson property_json in
  (* Convert the property formula to Z3 *)
  let property_z3 =
    Formulas.formula_to_z3 ~unwinding_depth:num_snapshots ctx temporal_decls
      property_formula
  in
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [property_z3] ;
  for t = 0 to num_snapshots - 2 do
    List.iter
      (fun (_car_id, exprs) ->
        let current_expr = List.nth exprs t in
        let next_actual_expr = List.nth exprs (t + 1) in
        let z3_symbolic_next =
          Formulas.Temporal.next ctx temporal_decls current_expr
        in
        let next_definition =
          Boolean.mk_iff ctx z3_symbolic_next next_actual_expr
        in
        Solver.add solver [next_definition] )
      car_event_expressions
  done ;
  if debug_mode then
    Printf.printf "\nDEBUG: Final assertions in solver before check:\n%s\n"
      (Solver.to_string solver) ;
  match Solver.check solver [] with
  | SATISFIABLE -> Printf.printf "SATISFIABLE\n"
  | UNSATISFIABLE -> Printf.printf "UNSATISFIABLE \n"
  | UNKNOWN ->
      Printf.printf "Solver returned UNKNOWN. Reason: %s\n"
        (Solver.get_reason_unknown solver)
