(* Synthesis from RMTLD3 to Spark2014 *)

open Sexplib
open Sexplib.Conv
open Rmtld3
open Helper

type body = string * string

(* adjust time base *)
let adjust_base gamma helper =
  let conv = function
    | "s" -> 1.
    | "ms" -> 1_000.
    | "us" -> 1_000_000.
    | "ns" -> 1_000_000_000.
    | _ -> failwith "adjust_base unavailable."
  in
  let rst =
    (get_setting_string "rtm_monitor_time_unit" helper |> conv) *. gamma
  in
  if rst < 1. then
    failwith
      "adjust_base loses precision. Please consider changing the monitor \
       time units!"
  else rst

let gen_adjust_base t helper =
  let conv = function
    | "s" -> 1.
    | "ms" -> 1_000.
    | "us" -> 1_000_000.
    | "ns" -> 1_000_000_000.
    | _ -> failwith "adjust_base unavailable."
  in
  if conv t <= (get_setting_string "rtm_monitor_time_unit" helper |> conv)
  then
    "static timespan time_of_" ^ t ^ "(timespan v) {\n"
    (* (float_of_int v) /. (conv t) *. (get_setting_string
       "rtm_monitor_time_unit" helper |> conv) *)
    ^ "float _v = v; \n _v /=  \n"
    ^ string_of_float (conv t)
    ^ "; _v *= "
    ^ string_of_float
        (get_setting_string "rtm_monitor_time_unit" helper |> conv)
    ^ "; return _v; \n}\n"
  else ""

(*
 * Compute terms
 *)
let synth_tm_constant value helper =
  ("make_duration(" ^ string_of_int (int_of_float value) ^ ",false)", "")

let synth_tm_variable name helper =
  failwith "Free variables are not possible."

let synth_tm_duration (di, a) (tf, b) helper =
  let id = get_duration_counter helper in
  (* [TODO: check di; unknown is not implemented!; nested duration may be a
     problem!] *)
  ("", "")

let synth_tm_plus (cmptr1, a) (cmptr2, b) helper =
  ("sum_dur(" ^ cmptr1 ^ ", " ^ cmptr2 ^ ")", a ^ b)

let synth_tm_times (cmptr1, a) (cmptr2, b) helper =
  ("mult_dur(" ^ cmptr1 ^ ", " ^ cmptr2 ^ ")", a ^ b)

(*
 * compute formulas
 *)
let synth_fm_true helper = ("T_TRUE", "")

let synth_fm_p p helper =
   ( "prop<T>(trace, PROP_" ^ find_proposition_rev_hashtbl p helper ^ ", t)"
  , "" )

let synth_fm_not (cmpfm, a) helper = failwith "unimplemented!" ("", "")

let synth_fm_or (cmpfm1, a) (cmpfm2, b) helper =
  failwith "unimplemented!" ("", "")

let synth_fm_less (cmptr1, a) (cmptr2, b) helper = ("", "")

let convert_to_always_equal (sf2, b) gamma helper =
  print_endline
    ( "The next operator 'false U[" ^ string_of_float gamma
    ^ "] fm' or false U[=" ^ string_of_float gamma
    ^ "] fm' is converted to 'Always[=" ^ string_of_float gamma
    ^ "] fm' since spark2014 synthesis is enabled." ) ;
  (* get new id *)
  let id = get_until_counter helper in
  ("", "")

let convert_to_unbounded_eventually (sf2, b) helper =
  print_endline
    "The unbounded until operator 'true U[infty] fm' is converted to \
     'Eventually[infty] fm' since spark2014 synthesis is enabled." ;
  (* get new id *)
  let id = get_until_counter helper in
  ("", "")

let synth_fm_uless gamma (sf1, a) (sf2, b) helper =
  failwith "unimplemented!" ;
  (* detect case when false U< f2 and f1=false *)
  if (sf1, a) = synth_fm_not (synth_fm_true helper) helper then
    convert_to_always_equal (sf2, b) gamma helper
  else if gamma = max_float && (sf1, a) = synth_fm_true helper then
    convert_to_unbounded_eventually (sf2, b) helper
  else
    (* get new id *)
    let id = get_until_counter helper in
    ("", "")

let synth_fm_ueq gamma (sf1, a) (sf2, b) helper =
  failwith "unimplemented!" ;
  (* detect case when false U= f2 and f1=false *)
  if (sf1, a) = synth_fm_not (synth_fm_true helper) helper then
    convert_to_always_equal (sf2, b) gamma helper
  else
    (* get new id *)
    let id = get_until_counter helper in
    (* Until (=): A Until (=a) B <-> Always(<a) A and Eventually(=a) B *OR*
       Until (=): A Until (=a) B <-> Always(<a) A and Always(=a) B *)
    ("", "")

let rec synth_fm_sless gamma (sf1, a) (sf2, b) helper =
  failwith "unimplemented!" ;
  (* unbounded mprev *)
  if
    gamma = max_float
    && (sf1, a) = synth_fm_not (synth_fm_true helper) helper
  then (
    print_endline
      "The unbounded previous 'prev[∞] ɸ' is converted to 'false since[<1.] \
       ɸ' since spark2014 synthesis is enabled." ;
    synth_fm_sless 1.
      (synth_fm_not (synth_fm_true helper) helper)
      (sf2, b) helper )
  else
    (* get new id *)
    let id = get_until_counter helper in
    ("", "")

let synth_fm_seq gamma (sf1, a) (sf2, b) helper =
  failwith "unimplemented!" ;
  (* get new id *)
  let id = get_until_counter helper in
  ("", "")

(* monitor dependent spark2014 functions begin here *)
let synth_spark2014 compute helper =
  print_endline "\x1b[33mCurrent Configuration:\x1b[0m" ;
  print_settings helper ;
  let expressions = get_all_setting_formula "input_exp" helper in
  if expressions = [] then (
    print_endline "no formula is available." ;
    exit 1 ) ;
  print_endline "\x1b[33mExpression(s) selected to encode:\x1b[0m" ;
  List.iter
    (fun exp ->
      print_plaintext_formula exp ;
      print_endline "" )
    expressions ;
  let cpp_monitor_lst =
    List.fold_right
      (fun exp lst ->
        let x, y = compute exp helper in
        ((x, y), string_of_int (List.length lst)) :: lst )
      expressions []
  in
  let pair_to_string ((x, y), z) = "((" ^ x ^ "," ^ y ^ "), " ^ z ^ ")" in
  let id =
    String.sub
      ( Digest.string
          (String.concat "" (List.map pair_to_string cpp_monitor_lst))
      |> Digest.to_hex )
      0 4
  in
  let name =
    insert_string
      (get_setting_string "rtm_monitor_name_prefix" helper)
      id '%'
  in
  let monitor_name = insert_string name "compute" '#' in
  ()

(* monitor dependent functions ends here *)
