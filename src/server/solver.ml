open Interface
open Interface.Z3solver

let with_z3 smtlib2_str helper =
  print_endline "Z3 solver enabled." ;
  let ctx, exp = parse_smtlibv2 smtlib2_str in
  let out, solver = solve_ ctx exp in
  print_endline ("Result: " ^ out) ;
  if not (Options.get_trace helper) then print_endline out ;
  if out = "satisfiable" then
    let model = get_model ctx solver in
    if not (Options.get_trace helper) then string_of_z3model model
    else
      let scheduler_trace = get_scheduler ctx model helper in
      if Options.trace_style helper = "tinterval" then
        let _, trc_str =
          List.fold_left
            (fun (cnt, a) b ->
              let cnte = cnt +. 1. in
              (cnte, a ^ " (\"" ^ b ^ "\",(" ^ string_of_float cnt ^ ")); ") )
            (0., "") scheduler_trace
        in
        trc_str
      else if Options.trace_style helper = "tcum" then
        let _, trc_str =
          List.fold_left
            (fun (cnt, a) b ->
              let cnte = cnt +. 1. in
              ( cnte
              , a ^ " (\"" ^ b ^ "\","
                ^ string_of_float (cnte -. cnt)
                ^ "); " ) )
            (0., "") scheduler_trace
        in
        trc_str
      else "no format!"
  else ""

let with_z3_safe smtlib2_str helper =
  Lwt.catch
    (fun () ->
       Lwt_unix.with_timeout 20.0 (fun () ->
         Lwt_preemptive.detach
           (fun () -> with_z3 smtlib2_str helper)
           ()
       )
    )
    (function
      | Lwt_unix.Timeout ->
          Lwt.fail_with "Z3 timed out"
      | exn -> Lwt.fail exn
    ) 
