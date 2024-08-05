open Hashtbl
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

(* reformulate this rmtld3synth state *)
type counter_maps =
  | Proposition_map of (string * int ref * (string, int) t)
  | Proposition_map_reverse of (string * int ref * (int, string) t)
  | Counter_without_map of string * int ref

type settings = Num of int | Txt of string | Sel of bool | Fm of Rmtld3.fm

type helper =
  string ref (* legacy *)
  * string ref (* legacy *)
  * int ref (* legacy *)
  * (global_int list * global_string list * monitor list)
  * (* legacy *)
  counter_maps list (* legacy *)
  * (string, settings) Hashtbl.t (* new settings structure *)

let verb_mode = ref 0
let verb f = if !verb_mode >= 2 then f () else ()
let verb_m mode f = if !verb_mode >= mode then f () else ()

let default_counter_map =
  [
    Proposition_map ("prop", ref 0, Hashtbl.create 10);
    Proposition_map_reverse ("prop", ref 0, Hashtbl.create 10);
    Counter_without_map ("counter_until", ref 0);
    Counter_without_map ("counter_duration", ref 0);
  ]

let mk_helper =
  let tbl = Hashtbl.create 50 in
  let _ = Hashtbl.add tbl "init" (Sel true) in
  (ref "", ref "", ref 0, ([], [], []), default_counter_map, tbl)

(* new settings structure setters *)
let set_setting name v (_, _, _, _, _, tbl) = Hashtbl.replace tbl name v

let get_setting_int name (_, _, _, _, _, tbl) =
  match Hashtbl.find tbl name with
  | Num a -> a
  | _ -> failwith "Error 'get_setting_int'!"

let get_setting_string name (_, _, _, _, _, tbl) =
  match Hashtbl.find tbl name with
  | Txt a -> a
  | _ -> failwith "Error 'get_setting_string'!"

let rec get_all_setting_string name (_, _, _, _, _, tbl) : string list =
  let rec _get_all_setting_string lst =
    match lst with
    | [] -> []
    | Txt a :: b -> a :: _get_all_setting_string b
    | _ -> failwith "Error '_get_all_setting_string'!"
  in
  List.rev (_get_all_setting_string (Hashtbl.find_all tbl name))

let rec get_all_setting_formula name (_, _, _, _, _, tbl) : Rmtld3.fm list =
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
  | Fm f -> print_plaintext_formula f
  | Sel b -> print_string (string_of_bool b)

let get_string_of_setting a =
  match a with
  | Num v -> string_of_int v
  | Txt s -> ("'" ^ s ^ "'")
  | Fm f -> string_of_rmtld_fm f
  | Sel b -> string_of_bool b

let print_settings (_, _, _, _, _, tbl) =
  Hashtbl.iter
    (fun a b ->
      print_string a;
      print_string " -> ";
      print_setting b;
      print_endline "")
    tbl

let get_string_of_settings (_, _, _, _, _, tbl) =
  Hashtbl.fold
    (fun a b lst ->
      lst ^ (a ^ " -> " ^ get_string_of_setting b ^ "\n"))
    tbl ""

let get_proposition_map id lst =
  let fnd =
    List.find
      (fun a ->
        match a with
        | Proposition_map (id_m, _, _) -> if id_m = id then true else false
        | _ -> false)
      lst
  in
  match fnd with
  | Proposition_map (a, b, c) -> (a, b, c)
  | _ -> raise (Failure "proposition_map mismatch.")

let get_proposition_map_rev id lst =
  let fnd =
    List.find
      (fun a ->
        match a with
        | Proposition_map_reverse (id_m, _, _) ->
            if id_m = id then true else false
        | _ -> false)
      lst
  in
  match fnd with
  | Proposition_map_reverse (a, b, c) -> (a, b, c)
  | _ -> raise (Failure "proposition_map mismatch.")

let get_counter_without_map id lst =
  let fnd =
    List.find
      (fun a ->
        match a with
        | Counter_without_map (id_m, _) -> if id_m = id then true else false
        | _ -> false)
      lst
  in
  match fnd with
  | Counter_without_map (a, b) -> (a, b)
  | _ -> raise (Failure "counter_without_map mismatch.")

let get_proposition_hashtbl (_, _, _, _, lst, _) =
  let _, _, tbl = get_proposition_map "prop" lst in
  tbl

let get_proposition_rev_hashtbl (_, _, _, _, lst, _) =
  let _, _, tbl = get_proposition_map_rev "prop" lst in
  tbl

let get_proposition_counter (_, _, _, _, lst, _) =
  let _, count, _ = get_proposition_map "prop" lst in
  count := !count + 1;
  !count

let find_proposition_rev_hashtbl a helper =
  try Hashtbl.find (get_proposition_rev_hashtbl helper) a
  with Not_found -> "idle"

let set_proposition_two_way_map p id helper =
  Hashtbl.add (get_proposition_hashtbl helper) p id;
  Hashtbl.add (get_proposition_rev_hashtbl helper) id p

let get_until_counter (_, _, _, _, lst, _) =
  let _, count = get_counter_without_map "counter_until" lst in
  count := !count + 1;
  !count + Random.int 1000000 (* try to avoid same template id *)

let get_duration_counter (_, _, _, _, lst, _) =
  let _, count = get_counter_without_map "counter_duration" lst in
  count := !count + 1;
  !count + Random.int 1000000 (* try to avoid same template id *)

let get_inc_counter_test_cases (_, _, count, _, _, _) =
  count := !count + 1;
  !count

let get_counter_test_cases (_, _, count, _, _, _) = !count
let set_counter_test_cases n (_, _, count, _, _, _) = count := n

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
           filename)
  else
    try Sexp.load_sexps default_dir
    with Sys_error _ ->
      failwith
        (Printf.sprintf
           "No default configuration file found on '%s'. Use --config-file \
            flag."
           default_dir)

(* gets settings from string *)
let setting_from_string str = Sexp.of_string str
let settings_from_string str = Sexp.scan_sexps (Stdlib.Lexing.from_string str)

(* lets parsing configuration file into global_int and monitor type variables *)
let settings sexpression =
  let list_global_int_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (global_int_of_sexp sexp_el :: lst, lst2)
        with _ -> (lst, sexp_el :: lst2))
      ([], []) sexpression
  in
  let list_global_string_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (global_string_of_sexp sexp_el :: lst, lst2)
        with _ -> (lst, sexp_el :: lst2))
      ([], []) sexpression
  in
  let list_monitor_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (monitor_of_sexp sexp_el :: lst, lst2)
        with _ -> (lst, sexp_el :: lst2))
      ([], []) sexpression
  in
  (* lets draw the settings that are not recognized *)
  List.fold_left
    (fun _ sexp_el ->
      print_endline (Sexp.to_string_hum sexp_el ^ " setting is not recognized."))
    () sexpression;
  (list_global_int_settings, list_global_string_settings, list_monitor_settings)


(* END helper for settings *)

(* compute the heap cost of a formula *)
let rec calculate_heap_cost (formula : rmtld3_fm) =
  match formula with
  | True _ -> 1
  | Prop _ -> 1
  | Not sf -> 1 + calculate_heap_cost sf
  | Or (sf1, sf2) -> 1 + max (calculate_heap_cost sf1) (calculate_heap_cost sf2)
  | Until (_, sf1, sf2) ->
      1 + max (calculate_heap_cost sf1) (calculate_heap_cost sf2)
  | LessThan (tr1, tr2) ->
      1 + max (calculate_heap_cost_term tr1) (calculate_heap_cost_term tr2)
  | _ ->
      raise
        (Failure
           ("ERROR: Calculating bound for unsupported formula ("
           ^ Sexp.to_string (sexp_of_fm formula)
           ^ ")."))

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
           ("ERROR: Calculating bound for unsupported term ("
           ^ Sexp.to_string (sexp_of_tm term)
           ^ ")."))

let rec calculate_cycle_cost (formula : rmtld3_fm) l =
  match formula with
  | True _ -> 0
  | Prop _ -> 0
  | Not sf -> 0 + calculate_cycle_cost sf l
  | Or (sf1, sf2) -> 0 + calculate_cycle_cost sf1 l + calculate_cycle_cost sf2 l
  | Until (_, sf1, sf2) ->
      let lend i = if List.length i > 0 then List.tl i else [] in
      let x, _ =
        List.fold_left
          (fun (a, c) _ ->
            ( a + calculate_cycle_cost sf1 c + calculate_cycle_cost sf2 c + 1,
              lend c ))
          (0, l) l
      in
      x + 0
  | LessThan (_, _) -> 0
  | _ ->
      raise
        (Failure
           ("ERROR: Calculating bound for unsupported formula ("
           ^ Sexp.to_string (sexp_of_fm formula)
           ^ ")."))

and calculate_cycle_cost_term term l =
  match term with
  | Constant _ -> 0
  | Duration (_, phi) -> 0 + calculate_cycle_cost phi l
  | FPlus (_, _) -> 0
  | FTimes (_, _) -> 0
  | _ ->
      raise
        (Failure
           ("ERROR: Calculating bound for unsupported term ("
           ^ Sexp.to_string (sexp_of_tm term)
           ^ ")."))

(* trace generation helpers *)

let rec strategic_uniform_trace value samples factor trace =
  (*let timestamp = (Random.float factor) +. value in*)
  let timestamp = factor +. value in
  if samples = 0 then ("B", (value, timestamp)) :: trace
  else
    (*let trace_size =  List.length trace in
      if samples <= trace_size then
        strategic_uniform_trace timestamp (samples-1) factor (("B",(value,timestamp))::trace)
      else*)
    strategic_uniform_trace timestamp (samples - 1) factor
      (("A", (value, timestamp)) :: trace)

let rec repeat_trace n pattern trace t tsize =
  if n <> 0 then
    repeat_trace (n - 1) pattern
      (List.append trace
         (List.map (fun (a, (b, c)) -> (a, (b +. t, c +. t))) pattern))
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
          let suffix = String.sub s (index + 1) (String.length s - index - 1) in
          insert (acc ^ prefix ^ str2) suffix
        with Not_found -> acc ^ s)
  in
  insert "" str1

(* beautify cpp code if clang-format is available *)
let beautify_cpp_code code =
  if Sys.command "command -v clang-format" <> 0 then (
    print_endline ("Warning: " ^ "clang-format is missing!");
    code)
  else
    let in_chan, out_chan, _ =
      Unix.open_process_full "clang-format -style=GNU -assume-filename=.hpp" [| "" |]
    in
    output_string out_chan code;
    close_out out_chan;

    let rec read_lines line =
      try
        let line = line ^ input_line in_chan ^ "\n" in
        read_lines line
      with End_of_file -> line
    in
    let out = read_lines "" in
    close_in in_chan;
    out

(*
  helper for monitor configuration of events
 *)
let get_event_type helper = get_setting_string "rtm_event_type" helper

let get_event_subtype helper = get_setting_string "rtm_event_subtype" helper

let get_event_fulltype helper =
  get_event_type helper ^ "< "
  ^ get_event_subtype helper
  ^ " >"

(*
  auxiliar functions
 *)
let explode s = s |> String.to_seq |> List.of_seq

let rec of_enum_ a b c = if a >= b then List.rev (a::c) else of_enum_ (a+1) b (a::c)

let rec of_enum a b = of_enum_ a b []

let is_even n = 
  n mod 2 = 0

let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative" else
  let rec aux accumulator base = function
    | 0 -> accumulator
    | 1 -> base * accumulator
    | e when is_even e -> aux accumulator (base * base) (e / 2)
    | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
  aux 1 base exponent

let (%) f g x = fun x -> f(g x)

let adjust_base gamma helper =
  let conv = function
  | "s" -> 1.
  | "ms" -> 1_000.
  | "us" -> 1_000_000.
  | "ns" -> 1_000_000_000.
  | _ -> failwith "adjust_base unavailable." in
  let rst = (get_setting_string "rtm_monitor_time_unit" helper |> conv) *. gamma in
  if rst < 1. then failwith "adjust_base loses precision. Please consider changing the monitor time units!" else rst
