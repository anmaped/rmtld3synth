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
  let id = helper |> get_unique_id |> string_of_int in
  ( "Cons_" ^ id
  , "function Cons_" ^ id ^ " is new X_rmtld3.Cons (Value => "
    ^ Ada_pp.pp_expression (Ada_pp.mk_float value)
    ^ ");" )

let synth_tm_variable name helper =
  failwith "Free variables are not possible."

let synth_tm_duration (di, a) (tf, b) helper =
  failwith "unimplemented!" ;
  let id = get_duration_counter helper in
  (* [TODO: check di; unknown is not implemented!; nested duration may be a
     problem!] *)
  ("", "")

let synth_tm_plus (cmptr1, a) (cmptr2, b) helper =
  let id = helper |> get_unique_id |> string_of_int in
  ( "Sum_" ^ id
  , a ^ "\n" ^ b ^ "\nfunction Sum_" ^ id ^ " is new X_rmtld3.Sum ( tm1 =>"
    ^ cmptr1 ^ ", tm2 => " ^ cmptr2 ^ ");" )

let synth_tm_times (cmptr1, a) (cmptr2, b) helper =
  let id = helper |> get_unique_id |> string_of_int in
  ( "Times_" ^ id
  , a ^ "\n" ^ b ^ "\nfunction Times_" ^ id
    ^ " is new X_rmtld3.Times ( tm1 =>" ^ cmptr1 ^ ", tm2 => " ^ cmptr2
    ^ ");" )

(*
 * compute formulas
 *)
let synth_fm_true helper = ("X_rmtld3.mk_true", "")

let synth_fm_p p helper =
  let id = helper |> get_unique_id |> string_of_int in
  ( "Prop_" ^ id
  , "function Prop_" ^ id ^ " is new X_rmtld3.Prop (Proposition => P'Pos (P_"
    ^ find_proposition_rev_hashtbl p helper
    ^ "));" )

let synth_fm_not (cmpfm, a) helper =
  let id = helper |> get_unique_id |> string_of_int in
  ( "Not3_" ^ id
  , a ^ "\nfunction Not3_" ^ id ^ " is new X_rmtld3.Not3 (fm => " ^ cmpfm
    ^ ");" )

let synth_fm_or (cmpfm1, a) (cmpfm2, b) helper =
  let id = helper |> get_unique_id |> string_of_int in
  ( "Or3_" ^ id
  , a ^ "\n" ^ b ^ "\nfunction Or3_" ^ id ^ " is new X_rmtld3.Or3 (fm1 => "
    ^ cmpfm1 ^ ", fm2 => " ^ cmpfm2 ^ ");" )

let synth_fm_less (cmptr1, a) (cmptr2, b) helper =
  let id = helper |> get_unique_id |> string_of_int in
  ( "Less3_" ^ id
  , a ^ "\n" ^ b ^ "\nfunction Less3_" ^ id
    ^ " is new X_rmtld3.Less3 (tm1 => " ^ cmptr1 ^ ", tm2 => " ^ cmptr2
    ^ ");" )

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
  let save filename =
    if is_setting "out_dir" helper then ( fun a ->
      let stream =
        open_out (get_setting_string "out_dir" helper ^ "/" ^ filename)
      in
      Printf.fprintf stream "%s\n" a ;
      close_out stream )
    else print_endline
  in
  verb_m 1 (fun _ ->
      print_endline "\x1b[33mCurrent Configuration:\x1b[0m" ;
      print_settings helper ) ;
  let expressions = get_all_setting_formula "input_exp" helper in
  if expressions = [] then (
    print_endline "no formula is available." ;
    exit 1 ) ;
  verb_m 1 (fun _ ->
      print_endline "\x1b[33mExpression(s) selected to encode:\x1b[0m" ;
      List.iter
        (fun exp ->
          print_plaintext_formula exp ;
          print_endline "" )
        expressions ) ;
  let cpp_monitor_lst =
    List.fold_right
      (fun exp lst ->
        let x, y = compute exp helper in
        ((x, y), string_of_int (List.length lst)) :: lst )
      expressions []
  in
  let pair_to_string ((x, y), z) = "((" ^ x ^ "," ^ y ^ "), " ^ z ^ ")" in
  let id =
    List.map pair_to_string cpp_monitor_lst
    |> String.concat "" |> Digest.string |> Digest.to_hex
    |> fun s -> String.sub s 0 4
  in
  let name =
    insert_string
      (get_setting_string "rtm_monitor_name_prefix" helper)
      id '%'
  in
  let convert_proposition_to_enumeration_type =
    Hashtbl.fold
      (fun a b acc ->
        match (a, b) with
        | N a, S b -> ("P_" ^ b) :: acc
        | _ -> failwith "prop_lst!" )
      (get_proposition_rev_hashtbl helper)
      []
    |> Ada_pp.mk_type_declaration "P"
  in
  let monitor_name = insert_string name "compute" '#' in
  let code1 =
    List.fold_right
      (fun ((function_call, body), n) str ->
        str
        ^ ( [ n |> int_of_string
              |> List.nth (List.rev expressions)
              |> string_of_rmtld_fm |> Ada_pp.mk_comment
            ; Ada_pp.mk_package_specification
                (monitor_name ^ "_" ^ n)
                [ convert_proposition_to_enumeration_type
                ; Ada_pp.Unsafe_inline body (* unsafe *)
                ; Ada_pp.mk_comment function_call ]
              |> Ada_pp.mk_generic_package
                   [ Ada_pp.mk_formal_package_declaration "X_rmtld3" "Rmtld3"
                       "(<>)" ] ]
          |> Ada_pp.pp_basic_declarations ) )
      cpp_monitor_lst ""
  in
  (Ada_pp.Unsafe_inline "with Rmtld3;\n" |> Ada_pp.pp_basic_declaration)
  ^ code1
  |> save (monitor_name ^ ".ads") ;
  (* generate test package *)
  if is_setting "environment" helper then (
    let json =
      get_setting_string "environment" helper |> Yojson.Safe.from_string
    in
    let json_trc = json |> Yojson.Safe.Util.member "trc" in
    let trc =
      if json_trc <> `Null then trace_of_yojson json_trc
      else failwith "No 'trc' available!"
    in
    Ada_pp.mk_subprogram_specification "Test"
      [Ada_pp.mk_parameter "buf" (Unsafe "access Nat_Buffer.Buffer_Type")]
      None
    |> Ada_pp.mk_generic_subprogram
         [ Ada_pp.mk_formal_package_declaration "Nat_Buffer" "Buffer" "(<>)"
         ; Ada_pp.mk_use_declaration "Nat_Buffer" ]
    |> Ada_pp.mk_list
    |> Ada_pp.mk_package_specification "Unit"
    |> Ada_pp.mk_package_declaration |> Ada_pp.mk_list
    |> Ada_pp.mk_list_append (Ada_pp.Unsafe_inline "with Buffer;")
    |> Ada_pp.pp_basic_declarations |> save "unit.ads" ;
    let rec convert_lst_to_reader lst =
      match lst with
      | [] -> []
      | (p, t) :: tl ->
          Ada_pp.mk_if
            (Ada_pp.Equal
               ( Ada_pp.mk_function_call "Nat_Buffer.Push"
                   [ Ada_pp.mk_expression_parameter (Ada_pp.Name "buf.all")
                   ; Ada_pp.mk_expression_parameter
                       (Ada_pp.mk_function_call "Nat_Buffer.E.Create"
                          [ Ada_pp.mk_expression_parameter ~selector:"Data"
                              (Ada_pp.mk_int
                                 (find_proposition_hashtbl p helper) )
                          ; Ada_pp.mk_expression_parameter ~selector:"Time"
                              (Ada_pp.mk_float t) ] ) ]
               , Ada_pp.mk_name "Nat_Buffer.OK" ) )
            [Ada_pp.mk_print "Ok!"]
            [Ada_pp.mk_print "NOT Ok!"]
          :: convert_lst_to_reader tl
    in
    convert_lst_to_reader trc
    |> Ada_pp.mk_subprogram "Test"
         [Ada_pp.mk_parameter "buf" (Unsafe "access Nat_Buffer.Buffer_Type")]
         None []
    |> Ada_pp.mk_package_body "Unit"
    |> Ada_pp.mk_compilation_unit_package_body [Ada_pp.With "Ada.Text_Io"] |> Ada_pp.pp_compilation_unit |> save "unit.adb" )

(* monitor dependent functions ends here *)
