(*
   z3 dummy module
*)

let parse_smtlibv2 smtlibv2_str = ((),())

let solve_ ctx exp = ("Dummy",())

let get_model ctx solver = ()

let string_of_z3model model = "Dummy"

let get_scheduler ctx model helper = []
