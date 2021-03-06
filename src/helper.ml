open Hashtbl
open Sexplib
open Sexplib.Conv
open Rmtld3
include Js.Helper_

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

type helper =
  string ref
  * string ref
  * int ref
  * (global_int list * global_string list * monitor list)
  * counter_maps list

let verb_mode = ref 0

let verb f = if !verb_mode >= 2 then f () else ()

let verb_m mode f = if !verb_mode >= mode then f () else ()

let default_counter_map =
  [ Proposition_map ("prop", ref 0, Hashtbl.create 10)
  ; Proposition_map_reverse ("prop", ref 0, Hashtbl.create 10)
  ; Counter_without_map ("counter_until", ref 0)
  ; Counter_without_map ("counter_duration", ref 0) ]

let mk_helper = (ref "", ref "", ref 0, ([], [], []), default_counter_map)

let set_parameters p (a, b, c, _, e) = (a, b, c, p, e)

let get_proposition_map id lst =
  let fnd =
    List.find
      (fun a ->
        match a with
        | Proposition_map (id_m, _, _) -> if id_m = id then true else false
        | _ -> false )
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
        | _ -> false )
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
        | _ -> false )
      lst
  in
  match fnd with
  | Counter_without_map (a, b) -> (a, b)
  | _ -> raise (Failure "counter_without_map mismatch.")

(*
  helper for monitor configuration of events
 *)
let get_event_fulltype (t1, t2, _, _, _) = !t1 ^ "< " ^ !t2 ^ " >"

let get_event_type (_, t2, _, _, _) = !t2

let set_event_type type_event (t1, _, _, _, _) = t1 := type_event

let set_event_subtype sub_type_event (_, t2, _, _, _) = t2 := sub_type_event

let get_proposition_hashtbl (_, _, _, _, lst) =
  let _, _, tbl = get_proposition_map "prop" lst in
  tbl

let get_proposition_rev_hashtbl (_, _, _, _, lst) =
  let _, _, tbl = get_proposition_map_rev "prop" lst in
  tbl

let get_proposition_counter (_, _, _, _, lst) =
  let _, count, _ = get_proposition_map "prop" lst in
  count := !count + 1 ;
  !count

let find_proposition_rev_hashtbl a helper =
  try Hashtbl.find (get_proposition_rev_hashtbl helper) a with Not_found ->
    "idle"

let set_proposition_two_way_map p id helper =
  Hashtbl.add (get_proposition_hashtbl helper) p id ;
  Hashtbl.add (get_proposition_rev_hashtbl helper) id p

let get_until_counter (_, _, _, _, lst) =
  let _, count = get_counter_without_map "counter_until" lst in
  count := !count + 1 ;
  !count

let get_duration_counter (_, _, _, _, lst) =
  let _, count = get_counter_without_map "counter_duration" lst in
  count := !count + 1 ;
  !count

let get_inc_counter_test_cases (_, _, count, _, _) =
  count := !count + 1 ;
  !count

let get_counter_test_cases (_, _, count, _, _) = !count

let get_settings_int (_, _, _, (a, _, _), _) = a

let get_settings_string (_, _, _, (_, b, _), _) = b

let get_settings_monitor (_, _, _, (_, _, c), _) = c

let set_counter_test_cases n (_, _, count, _, _) = count := n

(* BEGIN helper for settings *)

(* gets settings from file *)
let settings_from_file filename =
  let default_dir = "config/default" in
  if filename <> "" then
    try Sexp.load_sexps filename with Sys_error _ ->
      failwith
        (Printf.sprintf
           "No default configuration file found on '%s'. Use --config-file \
            flag."
           filename)
  else
    try Sexp.load_sexps default_dir with Sys_error _ ->
      failwith
        (Printf.sprintf
           "No default configuration file found on '%s'. Use --config-file \
            flag."
           default_dir)

(* gets settings from string *)
let setting_from_string str = Sexp.of_string str

(* lets parsing configuration file into global_int and monitor type variables *)
let settings sexpression =
  let list_global_int_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (global_int_of_sexp sexp_el :: lst, lst2) with _ ->
          (lst, sexp_el :: lst2) )
      ([], []) sexpression
  in
  let list_global_string_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (global_string_of_sexp sexp_el :: lst, lst2) with _ ->
          (lst, sexp_el :: lst2) )
      ([], []) sexpression
  in
  let list_monitor_settings, sexpression =
    List.fold_left
      (fun (lst, lst2) sexp_el ->
        try (monitor_of_sexp sexp_el :: lst, lst2) with _ ->
          (lst, sexp_el :: lst2) )
      ([], []) sexpression
  in
  (* lets draw the settings that are not recognized *)
  List.fold_left
    (fun _ sexp_el ->
      print_endline (Sexp.to_string_hum sexp_el ^ " setting is not recognized.")
      )
    () sexpression ;
  (list_global_int_settings, list_global_string_settings, list_monitor_settings)

let rec search_settings lst word =
  if lst = [] then raise (Settings_Not_Found word) ;
  let el_id, el_val = List.hd lst in
  if el_id = word then el_val else search_settings (List.tl lst) word

let search_settings_int word helper =
  search_settings (get_settings_int helper) word

let search_settings_string word helper =
  search_settings (get_settings_string helper) word

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
           ^ ")." ))

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
           ^ ")." ))

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
           ^ ")." ))

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
           ^ ")." ))

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
