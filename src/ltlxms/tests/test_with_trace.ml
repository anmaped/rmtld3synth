(* It provides a command-line interface to run LTLxMS checks on traces.
 * It expects two JSON files: one for the trace and one for the property. *)

(* Usage:
 * dune exec src/ltlxms/test/test_with_trace.exe <trace_file.json> <property_file.json>
 *)

(* Note: The trace file should be in the format defined in src/sequence/snapshot.ml,
 * and the property file should contain LTLxMS formulas as defined in src/ltlxms/syntax.ml. *)


(* Standalone entry point for command-line usage *)
let () =
  if Array.length Sys.argv = 3 then
    Ltlxms.Check.run ~trace_file:Sys.argv.(1) ~property_file:Sys.argv.(2)
      ~debug:1
  else if Array.length Sys.argv > 1 then
    Printf.printf "Usage: %s <trace_file.json> <property_file.json>\n"
      Sys.argv.(0)
