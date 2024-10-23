open Sexplib
open Sexplib.Conv
open Rmtld3
include Js.Helper_

let _ = Random.self_init

(* global_int settings *)
type global_int = string * int [@@deriving sexp]

(* global_string settings *)
type global_string = string * string [@@deriving sexp]

(* monitor setting entry*)
type monitor = string * int * Rmtld3.fm [@@deriving sexp]

type formula = Rmtld3.fm [@@deriving sexp]

exception Settings_Not_Found of string

type values = N of int | S of string

type settings =
  | Num of int
  | Txt of string
  | Sel of bool
  | Fm of Rmtld3.fm
  | Hash of (values, values) Hashtbl.t

type helper = (string, settings) Hashtbl.t (* new settings structure *)

let verb_mode = ref 0

let verb f = if !verb_mode >= 2 then f () else ()

let verb_m mode f = if !verb_mode >= mode then f () else ()

let mk_helper =
  let tbl = Hashtbl.create 50 in
  Hashtbl.add tbl "init" (Sel true) ;
  Hashtbl.add tbl "prop_map" (Hash (Hashtbl.create 10)) ;
  Hashtbl.add tbl "prop_map_reverse" (Hash (Hashtbl.create 10)) ;
  tbl

(* new settings structure setters *)
let set_setting name v tbl = Hashtbl.add tbl name v

let set_setting_replace name v tbl = Hashtbl.replace tbl name v

let is_setting name tbl =
  try
    let _ = Hashtbl.find tbl name in
    true
  with Not_found -> false

let get_setting_bool name tbl =
  try match Hashtbl.find tbl name with Sel a -> a | _ -> false
  with _ -> false

let get_setting_int name tbl =
  match Hashtbl.find tbl name with
  | Num a -> a
  | _ -> failwith "Error 'get_setting_int'!"

let get_setting_string name tbl =
  match Hashtbl.find tbl name with
  | Txt a -> a
  | _ -> failwith "Error 'get_setting_string'!"

let get_setting_hash name tbl =
  match Hashtbl.find tbl name with
  | Hash a -> a
  | _ -> failwith "Error 'get_setting_hash'!"

let get_all_setting_string name tbl : string list =
  let rec _get_all_setting_string lst =
    match lst with
    | [] -> []
    | Txt a :: b -> a :: _get_all_setting_string b
    | _ -> failwith "Error '_get_all_setting_string'!"
  in
  List.rev (_get_all_setting_string (Hashtbl.find_all tbl name))

let get_all_setting_formula name tbl : Rmtld3.fm list =
  let rec _get_all_setting_formula lst =
    match lst with
    | [] -> []
    | Fm a :: b -> a :: _get_all_setting_formula b
    | _ -> failwith "Error '_get_all_setting_formula'!"
  in
  List.rev (_get_all_setting_formula (Hashtbl.find_all tbl name))

let print_setting a =
  match a with
  | Num v -> print_string (string_of_int v)
  | Txt s -> print_string ("'" ^ s ^ "'")
  | Fm f -> print_string (string_of_rmtld_fm f)
  | Sel b -> print_string (string_of_bool b)
  | Hash ht ->
      print_string
        (Hashtbl.fold
           (fun x y a ->
             let m v = match v with S a -> a | N v -> string_of_int v in
             a ^ "(" ^ m x ^ "->" ^ m y ^ ") " )
           ht "" )

let get_string_of_setting a =
  match a with
  | Num v -> string_of_int v
  | Txt s -> "'" ^ s ^ "'"
  | Fm f -> string_of_rmtld_fm f
  | Sel b -> string_of_bool b
  | Hash ht ->
      Hashtbl.fold
        (fun x y a ->
          let m v = match v with S a -> a | N v -> string_of_int v in
          a ^ "(" ^ m x ^ "->" ^ m y ^ ") " )
        ht ""

let print_settings tbl =
  Hashtbl.iter
    (fun a b ->
      print_string a ;
      print_string " -> " ;
      print_setting b ;
      print_endline "" )
    tbl

let get_string_of_settings tbl =
  Hashtbl.fold
    (fun a b lst -> lst ^ a ^ " -> " ^ get_string_of_setting b ^ "\n")
    tbl ""

let get_proposition_hashtbl helper = get_setting_hash "prop_map" helper

let get_proposition_rev_hashtbl helper =
  get_setting_hash "prop_map_reverse" helper

let find_proposition_hashtbl s helper =
  match Hashtbl.find (get_proposition_hashtbl helper) (S s) with
  | N v -> v
  | _ -> failwith "find_proposition_hashtbl: values mismatch!"

let find_proposition_rev_hashtbl v helper =
  try
    match Hashtbl.find (get_proposition_rev_hashtbl helper) (N v) with
    | S s -> s
    | _ -> failwith "find_proposition_rev_hashtbl: values mismatch!"
  with Not_found -> "idle"

let set_proposition_two_way_map p id helper =
  Hashtbl.add (get_proposition_hashtbl helper) (S p) (N id) ;
  Hashtbl.add (get_proposition_rev_hashtbl helper) (N id) (S p)

let proposition_hashtbl_match x y =
  match (x, y) with
  | S p, N id -> (p, id)
  | _ -> failwith "proposition_hashtbl_match: values mismatch!"

let _get_counter name helper =
  let count =
    try get_setting_int name helper
    with _ ->
      set_setting name (Num 0) helper ;
      0
  in
  let count = count + 1 in
  set_setting_replace name (Num count) helper ;
  count

(* unsigned Szudzik's Pairing *)
let pair (x, y) = if y > x then (y * y) + x else (x * x) + x + y

let unpair z =
  let q = int_of_float (floor (sqrt (float_of_int z))) in
  let l = z - (q * q) in
  if l < q then (l, q) else (q, l - q)

let get_proposition_counter helper = _get_counter "fm_num_prop" helper

let get_until_counter helper =
  let x = _get_counter "fm_num_until" helper in
  (* avoid same template id *)
  let y = Random.int 1000000 in
  pair (x, y)

let get_duration_counter helper =
  let x = _get_counter "fm_num_duration" helper in
  (* avoid same template id *)
  let y = Random.int 1000000 in
  pair (x, y)

let get_inc_counter_test_cases = _get_counter "unittests_num_test_cases"

let get_counter_test_cases = get_setting_int "unittests_num_test_cases"

let set_counter_test_cases n = set_setting "unittests_num_test_cases" (Num n)

(* BEGIN helper for settings *)

(* gets settings from file *)
let settings_from_file filename =
  let default_dir = "config/default" in
  if filename <> "" then
    try Sexp.load_sexps filename
    with Sys_error _ ->
      failwith
        (Printf.sprintf
           "No default configuration file found on '%s'. Use --config-file \
            flag."
           filename )
  else
    try Sexp.load_sexps default_dir
    with Sys_error _ ->
      failwith
        (Printf.sprintf
           "No default configuration file found on '%s'. Use --config-file \
            flag."
           default_dir )

(* gets settings from string *)
let setting_from_string str = Sexp.of_string str

let settings_from_string str =
  Sexp.scan_sexps (Stdlib.Lexing.from_string str)

(* lets parsing configuration file into global_int and monitor type
   variables *)
let settings sexpression =
  let list_global_int_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (global_int_of_sexp sexp_el :: lst, lst2)
        with _ -> (lst, sexp_el :: lst2) )
      ([], []) sexpression
  in
  let list_global_string_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (global_string_of_sexp sexp_el :: lst, lst2)
        with _ -> (lst, sexp_el :: lst2) )
      ([], []) sexpression
  in
  let list_monitor_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (monitor_of_sexp sexp_el :: lst, lst2)
        with _ -> (lst, sexp_el :: lst2) )
      ([], []) sexpression
  in
  (* lets draw the settings that are not recognized *)
  List.fold_left
    (fun _ sexp_el ->
      print_endline
        (Sexp.to_string_hum sexp_el ^ " setting is not recognized.") )
    () sexpression ;
  ( list_global_int_settings
  , list_global_string_settings
  , list_monitor_settings )

let apply_settings s helper =
  let s_num, s_str, _ = settings (settings_from_string s) in
  List.iter (fun (a, b) -> set_setting a (Num b) helper) s_num ;
  List.iter (fun (a, b) -> set_setting a (Txt b) helper) s_str

(* END helper for settings *)

(* compute the heap cost of a formula *)
let rec calculate_heap_cost (formula : rmtld3_fm) =
  match formula with
  | True _ -> 1
  | Prop _ -> 1
  | Not sf -> 1 + calculate_heap_cost sf
  | Or (sf1, sf2) ->
      1 + max (calculate_heap_cost sf1) (calculate_heap_cost sf2)
  | Until (_, sf1, sf2) ->
      1 + max (calculate_heap_cost sf1) (calculate_heap_cost sf2)
  | LessThan (tr1, tr2) ->
      1 + max (calculate_heap_cost_term tr1) (calculate_heap_cost_term tr2)
  | _ ->
      raise
        (Failure
           ( "ERROR: Calculating bound for unsupported formula ("
           ^ Sexp.to_string (sexp_of_fm formula)
           ^ ")." ) )

and calculate_heap_cost_term term =
  match term with
  | Constant _ -> 1
  | Duration (_, phi) -> 1 + calculate_heap_cost phi
  | FPlus (tr1, tr2) ->
      1 + max (calculate_heap_cost_term tr1) (calculate_heap_cost_term tr2)
  | FTimes (tr1, tr2) ->
      1 + max (calculate_heap_cost_term tr1) (calculate_heap_cost_term tr2)
  | _ ->
      raise
        (Failure
           ( "ERROR: Calculating bound for unsupported term ("
           ^ Sexp.to_string (sexp_of_tm term)
           ^ ")." ) )

let rec calculate_cycle_cost (formula : rmtld3_fm) l =
  match formula with
  | True _ -> 0
  | Prop _ -> 0
  | Not sf -> 0 + calculate_cycle_cost sf l
  | Or (sf1, sf2) ->
      0 + calculate_cycle_cost sf1 l + calculate_cycle_cost sf2 l
  | Until (_, sf1, sf2) ->
      let lend i = if List.length i > 0 then List.tl i else [] in
      let x, _ =
        List.fold_left
          (fun (a, c) _ ->
            ( a + calculate_cycle_cost sf1 c + calculate_cycle_cost sf2 c + 1
            , lend c ) )
          (0, l) l
      in
      x + 0
  | LessThan (_, _) -> 0
  | _ ->
      raise
        (Failure
           ( "ERROR: Calculating bound for unsupported formula ("
           ^ Sexp.to_string (sexp_of_fm formula)
           ^ ")." ) )

and calculate_cycle_cost_term term l =
  match term with
  | Constant _ -> 0
  | Duration (_, phi) -> 0 + calculate_cycle_cost phi l
  | FPlus (_, _) -> 0
  | FTimes (_, _) -> 0
  | _ ->
      raise
        (Failure
           ( "ERROR: Calculating bound for unsupported term ("
           ^ Sexp.to_string (sexp_of_tm term)
           ^ ")." ) )

(* trace generation helpers *)

let rec strategic_uniform_trace value samples factor trace =
  (*let timestamp = (Random.float factor) +. value in*)
  let timestamp = factor +. value in
  if samples = 0 then ("B", value (*, timestamp*)) :: trace
  else
    (*let trace_size = List.length trace in if samples <= trace_size then
      strategic_uniform_trace timestamp (samples-1) factor
      (("B",(value,timestamp))::trace) else*)
    strategic_uniform_trace timestamp (samples - 1) factor
      (("A", value (*, timestamp*)) :: trace)

let rec repeat_trace n pattern trace t tsize =
  if n <> 0 then
    repeat_trace (n - 1) pattern
      (List.append trace (List.map (fun (a, b) -> (a, b +. t)) pattern))
      (t +. tsize) tsize
  else trace

(* other auxiliar functions *)
(* replace char with a string inside a string *)
let insert_string str1 str2 ch =
  let rec insert acc str =
    match str with
    | "" -> acc
    | s -> (
      try
        let index = String.index s ch in
        let prefix = String.sub s 0 index in
        let suffix =
          String.sub s (index + 1) (String.length s - index - 1)
        in
        insert (acc ^ prefix ^ str2) suffix
      with Not_found -> acc ^ s )
  in
  insert "" str1

(* beautify cpp code if clang-format is available *)
let beautify_cpp_code code =
  if Sys.command "command -v clang-format" <> 0 then (
    print_endline ("Warning: " ^ "clang-format is missing!") ;
    code )
  else
    let in_chan, out_chan, _ =
      Unix.open_process_full "clang-format -style=GNU -assume-filename=.hpp"
        [|""|]
    in
    output_string out_chan code ;
    close_out out_chan ;
    let rec read_lines line =
      try
        let line = line ^ input_line in_chan ^ "\n" in
        read_lines line
      with End_of_file -> line
    in
    let out = read_lines "" in
    close_in in_chan ; out

(* helper for monitor configuration of events *)
let get_event_type helper = get_setting_string "rtm_event_type" helper

let get_event_subtype helper = get_setting_string "rtm_event_subtype" helper

let get_event_fulltype helper =
  get_event_type helper ^ "< " ^ get_event_subtype helper ^ " >"

(* auxiliar functions *)
let explode s = s |> String.to_seq |> List.of_seq

let rec of_enum_ a b c =
  if a >= b then List.rev (a :: c) else of_enum_ (a + 1) b (a :: c)

let of_enum a b = of_enum_ a b []

let is_even n = n mod 2 = 0

let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative"
  else
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2)
    in
    aux 1 base exponent

let ( % ) f g x = f (g x)
