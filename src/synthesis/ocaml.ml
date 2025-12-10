(* Synthesis from RMTLD3 to OCaml *)

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
  has_tm_dur := true ;
  ("(eval_tm_duration " ^ tm_call ^ " " ^ fm_call ^ ")", tm_body ^ fm_body)

let synth_tm_plus cmptr1 cmptr2 helper =
  ( "(fun k s t -> match (" ^ fst cmptr1 ^ " k s t," ^ fst cmptr2
    ^ " k s t) with | Dsome(v1),Dsome(v2) -> Dsome(v1 +. v2) | _ -> Dnone )"
  , snd cmptr1 ^ snd cmptr2 )

let synth_tm_times cmptr1 cmptr2 helper =
  ( "(fun k s t -> match (" ^ fst cmptr1 ^ " k s t," ^ fst cmptr2
    ^ " k s t) with | Dsome(v1),Dsome(v2) -> Dsome(v1 *. v2) | _ -> Dnone )"
  , snd cmptr1 ^ snd cmptr2 )

let synth_fm_true helper = ("(fun k s t -> True)", "")

let synth_fm_p p helper =
  let p_id = find_proposition_rev_hashtbl p helper in
  ("(fun k s t -> k.evaluate k.trc \"" ^ p_id ^ "\" t)", "")

let synth_fm_not cmpfm helper =
  ("(fun k s t -> b3_not (" ^ fst cmpfm ^ " k s t))", snd cmpfm)

let synth_fm_or cmpfm1 cmpfm2 helper =
  ( "(fun k s t -> b3_or (" ^ fst cmpfm1 ^ " k s t) (" ^ fst cmpfm2
    ^ " k s t))"
  , snd cmpfm1 ^ snd cmpfm2 )

let synth_fm_less cmptr1 cmptr2 helper =
  ( "(fun k s t -> b3_lessthan (" ^ fst cmptr1 ^ " k s t) (" ^ fst cmptr2
    ^ " k s t))"
  , snd cmptr1 ^ snd cmptr2 )

let synth_fm_uless gamma sf1 sf2 helper =
  has_fm_uless := true ;
  ( "(eval_uless " ^ string_of_float gamma ^ " " ^ fst sf1 ^ " " ^ fst sf2
    ^ ")"
  , snd sf1 ^ snd sf2 )

let synth_fm_ueq gamma sf1 sf2 helper =
  has_fm_ueq := true ;
  ( "(eval_ueq " ^ string_of_float gamma ^ " " ^ fst sf1 ^ " " ^ fst sf2 ^ ")"
  , snd sf1 ^ snd sf2 )

let synth_fm_ulesseq gamma sf1 sf2 helper =
  has_fm_ulesseq := true ;
  ( "(eval_ulesseq " ^ string_of_float gamma ^ " " ^ fst sf1 ^ " " ^ fst sf2
    ^ ")"
  , snd sf1 ^ snd sf2 )

let synth_fm_sless gamma (sf1, a) (sf2, b) helper =
  failwith ("S[<" ^ string_of_float gamma ^ "] Not Implemented!")

let synth_fm_seq gamma (sf1, a) (sf2, b) helper =
  failwith ("S[=" ^ string_of_float gamma ^ "] Not Implemented!")

let synth_ocaml fmt compute helper =
  let pp_endline = pp_endline fmt in
  (* out_file cluster_name monitor_period *)
  verbose pp_endline "Configuration before OCaml code generation:" ;
  verbose (pp_settings fmt) helper ;
  let expressions = get_all_setting_formula "input_exp" helper in
  verbose pp_endline "Expression(s) selected to encode:" ;
  List.iter
    (fun exp -> verbose pp_endline (string_of_rmtld_fm exp))
    expressions ;
  let monitor_lst =
    List.fold_right
      (fun exp lst ->
        let mon_call, mon_body = compute exp helper in
        ((mon_call, mon_body), string_of_int (List.length lst)) :: lst )
      expressions []
  in
  let pair_to_string ((x, _), y) = "(" ^ x ^ ", " ^ y ^ ")" in
  let id =
    String.sub
      ( Digest.string (String.concat "" (List.map pair_to_string monitor_lst))
      |> Digest.to_hex )
      0 4
  in
  let name =
    insert_string
      (get_setting_string "rtm_monitor_name_prefix" helper)
      id '%'
  in
  let monitor_name = insert_string name "compute" '#' in
  (* function to add three spaces to each line *)
  let add_spaces str =
    String.concat "\n"
      (List.map (fun line -> "   " ^ line) (String.split_on_char '\n' str))
  in
  let code1 =
    "(* This file was automatically generated from rmtld3synth tool version\n"
    ^ get_setting_string "version" helper
    ^ ".\n *)\n\n(* Settings:\n"
    ^ add_spaces
        ( get_json_string_of_settings helper
        ^ "\n\nFormula(s):\n"
        ^ List.fold_right
            (fun exp b -> b ^ "- " ^ string_of_rmtld_fm exp ^ "\n")
            expressions "" )
    ^ "\n\
      \ *) \n\
       open Rmtld3_eval\n\n\
       module type Trace = sig val trc : trace end\n\n"
    ^ List.fold_right
        (fun ((function_call, body), n) str ->
          "module "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n ^ "  ( T : Trace  ) = struct \n" ^ body
          ^ "  let env = environment T.trc\n\
            \  let lg_env = lenv\n\
            \  let t = 0.\n\
            \  let mon = " ^ function_call ^ " env lg_env t\nend\n\n" ^ str
          )
        monitor_lst ""
    ^ "\n\
       type monitor_factory = (module Trace) -> Rmtld3_eval.three_valued\n\n\
       let registry : (string * string * monitor_factory) list =\n\
      \  [\n"
    ^ List.fold_right2
        (fun ((_, _), n) exp acc ->
          "    ( \""
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n ^ "\"\n    , \"" ^ string_of_rmtld_fm exp
          ^ "\"\n    , fun (module T) ->\n        let module M = "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n ^ " (T) in\n        M.mon );\n" ^ acc )
        monitor_lst expressions ""
    ^ "  ]\n" ^ "(* End of generated file *)"
  in
  try
    let out_dir = get_setting_string "out_dir" helper in
    verbose pp_endline "Generated Output Files:" ;
    let monitor_name =
      String.capitalize_ascii (insert_string name "compute" '#')
    in
    save_file (out_dir ^ "/" ^ monitor_name ^ ".ml") code1 ;
    verbose pp_endline (out_dir ^ "/" ^ monitor_name ^ ".ml")
  with Not_found -> (
    try
      let out_file = get_setting_string "out_file" helper in
      verbose pp_endline "Generated Output Files:" ;
      let monitor_name =
        String.capitalize_ascii (insert_string name "compute" '#')
      in
      save_file out_file
        (Str.global_replace (Str.regexp monitor_name)
           ( out_file |> Filename.basename |> Filename.remove_extension
           |> String.capitalize_ascii )
           code1 ) ;
      verbose pp_endline out_file
    with Not_found ->
      (* print to console *)
      print_boundary_init pp_endline ;
      print_part pp_endline id (monitor_name ^ ".ml") code1 ;
      print_boundary_end pp_endline id )
