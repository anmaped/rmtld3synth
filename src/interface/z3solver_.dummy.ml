(*
   z3 dummy module
*)

let parse_smtlibv2 smtlibv2_str = failwith "rmtld3synth was not compiled with Z3 support."

let solve_ ctx exp = failwith "rmtld3synth was not compiled with Z3 support."

let get_model ctx solver = failwith "rmtld3synth was not compiled with Z3 support."

let string_of_z3model model = failwith "rmtld3synth was not compiled with Z3 support."

let get_scheduler ctx model helper = failwith "rmtld3synth was not compiled with Z3 support."
