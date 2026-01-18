open Sexplib
open Sexplib.Conv
open Rmtld3
include Helper_

(** {1 Verbosity control utilities}
  
  This module provides functions to control verbose output based on a global
  verbosity level. *)

(** Global verbosity level reference. Higher values enable more verbose output. *)
let verb_mode = ref 0

(** [verb_m mode f] executes function [f] only if the current verbosity level
  is greater than or equal to [mode]. *)
let verb_m mode f = if !verb_mode >= mode then f () else ()

(** [verb f] executes function [f] if verbosity level is 2 or higher. *)
let verb f = verb_m 2 f

(** [verbose f] returns [f] if verbosity level is 1 or higher, otherwise returns [ignore]. *)
let verbose f = if !verb_mode >= 1 then f else ignore

(** {1 Settings Management Functions}
  
  Provides a flexible hashtable-based configuration system with support for
  various data types including integers, strings, booleans, formulas, and
  nested hash tables.
  
  The settings system supports:
  - Type-safe getters and setters for different value types
  - Multiple values per key (using {!Hashtbl.find_all})
  - JSON and human-readable serialization
  - Proposition mapping utilities
  
  *)

(* global_int settings *)
type global_int = string * int [@@deriving sexp]

(* global_string settings *)
type global_string = string * string [@@deriving sexp]

(* monitor setting entry*)
type monitor = string * int * Rmtld3.fm [@@deriving sexp]

type formula = Rmtld3.fm [@@deriving sexp]

exception Setting_Not_Found of string

type values = N of int | S of string

type settings =
  | Num of int
  | Txt of string
  | Sel of bool
  | Fm of Rmtld3.fm
  | Hash of (values, values) Hashtbl.t

(** [helper] is a hash table type that maps strings to settings.
  This type represents the new settings structure where each string key
  is associated with a settings value. *)
type helper = (string, settings) Hashtbl.t (* new settings structure *)

let mk_helper () =
  let tbl = Hashtbl.create 50 in
  Hashtbl.add tbl "init" (Sel true) ;
  Hashtbl.add tbl "prop_map" (Hash (Hashtbl.create 10)) ;
  Hashtbl.add tbl "prop_map_reverse" (Hash (Hashtbl.create 10)) ;
  tbl

(* reset helper to initial state *)
let reset_helper tbl =
  Hashtbl.clear tbl ;
  Hashtbl.add tbl "init" (Sel true) ;
  Hashtbl.add tbl "prop_map" (Hash (Hashtbl.create 10)) ;
  Hashtbl.add tbl "prop_map_reverse" (Hash (Hashtbl.create 10)) ;
  ()

(* new settings structure setters *)
let set_setting name v tbl = Hashtbl.add tbl name v

let set_setting_replace name v tbl = Hashtbl.replace tbl name v

let remove_setting name v tbl = Hashtbl.remove tbl name

let rec remove_setting_every_occurence name tbl =
  if Hashtbl.mem tbl name then (
    Hashtbl.remove tbl name ;
    remove_setting_every_occurence name tbl )

let is_setting name tbl =
  try
    let _ = Hashtbl.find tbl name in
    true
  with Not_found -> false

let get_setting name tbl =
  try Hashtbl.find tbl name
  with _ -> raise (Setting_Not_Found ("Setting '" ^ name ^ "' not found!"))

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

let pp_setting fmt a =
  match a with
  | Num v -> Format.fprintf fmt "%d" v
  | Txt s -> Format.fprintf fmt "'%s'" s
  | Fm f -> Format.fprintf fmt "%s" (string_of_rmtld_fm f)
  | Sel b -> Format.fprintf fmt "%b" b
  | Hash ht ->
      Format.fprintf fmt "%s"
        (Hashtbl.fold
           (fun x y a ->
             let m v = match v with S a -> a | N v -> string_of_int v in
             a ^ "(" ^ m x ^ "->" ^ m y ^ ") " )
           ht "" )

let get_json_string_of_setting a =
  match a with
  | Num v -> string_of_int v
  | Txt s -> "\"" ^ s ^ "\""
  | Fm f -> "\"" ^ string_of_rmtld_fm f ^ "\""
  | Sel b -> string_of_bool b
  | Hash ht ->
      "\""
      ^ Hashtbl.fold
          (fun x y a ->
            let m v = match v with S a -> a | N v -> string_of_int v in
            a ^ "(" ^ m x ^ "->" ^ m y ^ ") " )
          ht ""
      ^ "\""

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

let get_string_of_settings ?(exclude = []) tbl =
  Hashtbl.fold
    (fun a b lst ->
      if List.exists (fun x -> a = x) exclude then lst
      else lst ^ a ^ " -> " ^ get_string_of_setting b ^ "\n" )
    tbl ""

(* JSON representation of settings *)
(* need to remove stray quotes and group duplicated keys from json output *)
let get_json_string_of_settings ?(exclude = []) tbl =
  let items =
    Hashtbl.fold
      (fun a b lst ->
        if List.exists (fun x -> a = x) exclude then lst
        else
          match List.assoc_opt a lst with
          | Some values ->
              (a, get_json_string_of_setting b :: values)
              :: List.remove_assoc a lst
          | None -> (a, [get_json_string_of_setting b]) :: lst )
      tbl []
  in
  let json_items =
    List.fold_right
      (fun (a, b) lst ->
        let item =
          Printf.sprintf "\"%s\": %s" a
            ( if List.length b > 1 then
                Printf.sprintf "[ %s ]" (String.concat ", " b)
              else Printf.sprintf "%s" (List.hd b) )
        in
        item :: lst )
      items []
  in
  "{\n  " ^ String.concat ",\n  " json_items ^ "\n}"

let pp_settings fmt tbl =
  Hashtbl.iter
    (fun a b ->
      Format.fprintf fmt "%s -> " a ;
      pp_setting fmt b ;
      Format.fprintf fmt "\n" )
    tbl

let print_settings tbl = pp_settings Format.std_formatter tbl

let pp_endline fmt = Format.fprintf fmt "%s\n"

(** {1 Multipart Message Functions}

  Helper functions for generating multipart message format output. *)

(** [print_boundary_init pp_endline] prints the initial multipart message header
  using the provided output function [pp_endline]. *)
let print_boundary_init pp_endline =
  pp_endline
    "This is a multipart message. To interpret this message correctly, \
     please refer to the boundary delimiter."

(** [print_part pp_endline id filename content] prints a message part with the given
  [id], [filename], and [content] using the output function [pp_endline]. *)
let print_part pp_endline id filename content =
  let boundary = "BOUNDARY_" ^ id in
  pp_endline ("--" ^ boundary) ;
  pp_endline
    ("Content-Disposition: attachment; filename=\"" ^ filename ^ "\"") ;
  pp_endline "Content-Type: text/plain" ;
  pp_endline "" ;
  pp_endline content

(** [print_boundary_end pp_endline id] prints the closing boundary for a multipart
  message with the given [id] using the output function [pp_endline]. *)
let print_boundary_end pp_endline id =
  let boundary = "BOUNDARY_" ^ id in
  pp_endline ("--" ^ boundary ^ "--") ;
  pp_endline "This is the end of the multipart message." ;
  pp_endline ""

(** [to_multipart_message fmt lst tbl] generates a multipart message using the
  provided formatter [fmt] and list of file entries [lst].
  
  @param fmt The formatter used to output the multipart message
  @param lst A list of tuples containing (filename, content) pairs to be included
         in the multipart message
  
  The function generates a random boundary identifier and constructs a multipart
  message with the following structure:
  - Initial boundary marker
  - One part for each (filename, content) pair in [lst]
  - Final boundary marker
  
  Each part is printed using [print_part] with the generated boundary [id]. *)
let to_multipart_message fmt lst =
  let id = string_of_int (Random.int 1000000) in
  let pp_endline = pp_endline fmt in
  print_boundary_init pp_endline ;
  List.iter
    (fun (filename, content) -> print_part pp_endline id filename content)
    lst ;
  print_boundary_end pp_endline id

(* proposition two-way mapping helpers *)
let get_proposition_hashtbl helper = get_setting_hash "prop_map" helper

let get_proposition_rev_hashtbl helper =
  get_setting_hash "prop_map_reverse" helper

let exists_proposition_hashtbl s helper =
  match Hashtbl.find_opt (get_proposition_hashtbl helper) (S s) with
  | Some (N _) -> true
  | _ -> false

let find_proposition_hashtbl s helper =
  match Hashtbl.find_opt (get_proposition_hashtbl helper) (S s) with
  | Some (N v) -> Some v
  | _ -> None

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
  let y = _get_counter "unique_id_counter" helper in
  pair (x, y)

let get_duration_counter helper =
  let x = _get_counter "fm_num_duration" helper in
  (* avoid same template id *)
  let y = _get_counter "unique_id_counter" helper in
  pair (x, y)

let get_unique_id helper =
  let x = _get_counter "fm_num_unique_id" helper in
  (* avoid same template id *)
  let y = _get_counter "unique_id_counter" helper in
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
  let isbool str =
    match String.lowercase_ascii str with
    | "true" | "false" -> true
    | _ -> false
  in
  let tobool str =
    match String.lowercase_ascii str with
    | "true" -> true
    | "false" -> false
    | _ -> false
  in
  let s_num, s_str, _ = settings (settings_from_string s) in
  List.iter (fun (a, b) -> set_setting a (Num b) helper) s_num ;
  List.iter
    (fun (a, b) ->
      if isbool b then set_setting a (Sel (tobool b)) helper
      else set_setting a (Txt b) helper )
    s_str

(* load settings from a file and set them in the helper *)
let load_settings_from_file filename helper =
  try
    let int_settings, str_settings, monitor_settings =
      settings (settings_from_file filename)
    in
    (* load settings and formulas from the configuration file *)
    List.iter
      (fun (name, value) -> set_setting name (Num value) helper)
      int_settings ;
    List.iter
      (fun (name, value) -> set_setting name (Txt value) helper)
      str_settings ;
    List.iter
      (fun (_, _, formula) -> set_setting "input_exp" (Fm formula) helper)
      monitor_settings
  with _ -> failwith ("Failed to load settings from file: " ^ filename)

(** [set_recursive_unrolling_depth formula helper] configures the recursive 
  unrolling depth for [helper] based on the formula's upper time bound.

  If "rec_unrolling_depth" is not set, it is initialized to [bound + 1] where
  [bound] is derived from the formula's upper time bound. If the computation
  fails or the formula is unbounded, a default bound of 20 is used.

  If "rec_unrolling_depth" is already set and the current value is less than
  the newly calculated [bound + 1], the setting is updated to the larger value.

  @param formula The formula whose upper time bound determines the unrolling depth
  @param helper  The helper object storing the "rec_unrolling_depth" configuration
*)
let set_recursive_unrolling_depth formula helper =
  let calculate () =
    int_of_float (try calculate_t_upper_bound formula with Failure _ -> 20.)
  in
  if not (is_setting "rec_unrolling_depth" helper) then
    (* Calculate the upper time bound of the formula to determine unrolling
       depth. Defaults to 20 if the formula is unbounded. *)
    let bound = calculate () in
    set_setting "rec_unrolling_depth" (Num (bound + 1)) helper
  else
    (* reset or update the existing setting if needed *)
    let _ =
      match get_setting "rec_unrolling_depth" helper with
      | Num v ->
          let bound = calculate () in
          if v < bound + 1 then
            set_setting_replace "rec_unrolling_depth"
              (Num (bound + 1))
              helper
      | _ -> ()
    in
    ()

(** {2 Local Variable Management}

  This module provides functions to manage variables stored in the
  "variables" setting of a helper object. Variables are stored in a
  hash table mapping string names to values of type [values]. *)

(** [init_set_variables helper] initializes the "variables" setting in [helper]
  as a hash table if it does not already exist. If it exists, the existing
  hash table is reset to be empty.

  @param helper The helper object in which to initialize the "variables" setting. *)
let init_set_variables helper =
  (* check if hash is already there *)
  if is_setting "variables" helper then
    (* do reset hash table *)
    let h = get_setting_hash "variables" helper in
    Hashtbl.reset h
  else
    let h = Hashtbl.create 5 in
    set_setting "variables" (Hash h) helper

(** [reset_set_variables helper] clears all entries from the "variables" hash table
  stored in the helper's settings.
  
  @param helper The helper object containing settings *)
let reset_set_variables helper =
  (* check if hash is already there *)
  if is_setting "variables" helper then
    let h = get_setting_hash "variables" helper in
    Hashtbl.reset h
  else failwith "'variables' setting does not exist."

(** [set_variable name value helper] sets the variable with the given [name]
  to the specified [value] in the "variables" setting of [helper]. *)
let set_variable name value helper =
  let h = get_setting "variables" helper in
  match h with
  | Hash ht ->
      Hashtbl.replace ht (S name) value ;
      set_setting "variables" (Hash ht) helper
  | _ -> failwith "'variables' setting is not a hash."

(** [get_variable name helper] retrieves the value of the variable
  with the given [name] from the "variables" setting in [helper]. *)
let get_variable name helper =
  let h = get_setting "variables" helper in
  match h with
  | Hash ht -> (
    try
      match Hashtbl.find ht (S name) with
      | S s -> s
      | _ -> failwith ("Variable " ^ name ^ " is not a string.")
    with Not_found -> failwith ("Variable " ^ name ^ " not found.") )
  | _ -> failwith "'variables' setting is not a hash."

(** [append_variable name value helper] appends the given [value] to the
  variable with the specified [name] in the "variables" setting of [helper].
  If the variable does not exist, it is created with the initial [value]. *)
let append_variable name value helper =
  let h = get_setting "variables" helper in
  match h with
  | Hash ht -> (
    try
      match Hashtbl.find ht (S name) with
      | S s ->
          Hashtbl.replace ht (S name) (S (s ^ "\n" ^ value)) ;
          set_setting "variables" (Hash ht) helper
      | _ -> failwith ("Variable " ^ name ^ " is not a string.")
    with Not_found ->
      Hashtbl.add ht (S name) (S value) ;
      set_setting "variables" (Hash ht) helper )
  | _ -> failwith "'variables' setting is not a hash."

(* END helper for settings *)

(* create directory if it does not exist *)
let create_dir dir_name =
  if not (Sys.file_exists dir_name) then Unix.mkdir dir_name 0o777
  else if not (Sys.is_directory dir_name) then
    failwith (Printf.sprintf "'%s' exists but is not a directory" dir_name)

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

(** {1 Utility functions}

  This module provides auxiliary functions including string manipulation,
  range generation, number operations, and function composition. *)

(** [explode s] converts a string [s] into a list of characters. *)
let explode s = s |> String.to_seq |> List.of_seq

(** [of_enum a b] generates a list of integers from [a] to [b] inclusive. *)
let of_enum a b =
  let rec of_enum_ a b c =
    if a >= b then List.rev (a :: c) else of_enum_ (a + 1) b (a :: c)
  in
  of_enum_ a b []

(** [is_even n] returns [true] if [n] is even, [false] otherwise. *)
let is_even n = n mod 2 = 0

(** [pow base exponent] computes [base] raised to the power of [exponent].
  Uses fast exponentiation algorithm.
  @raise Invalid_argument if [exponent] is negative. *)
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

(** [f % g] is the function composition operator. Returns a function that
  applies [g] first, then [f]. Equivalent to [fun x -> f (g x)]. *)
let ( % ) f g x = f (g x)

(** {2 List Serialization/Deserialization}

  This functions provides utilities for serializing and deserializing lists
  using OCaml's Marshal module. *)

(** [list_to_string lst] serializes a list [lst] into a string using
  OCaml's Marshal module. *)
let list_to_string (lst : 'a list) = Marshal.to_string lst []

(** [string_to_list s] deserializes a string [s] back into a list
  using OCaml's Marshal module. *)
let string_to_list s : 'a list = Marshal.from_string s 0

(** {2 Set of Strings Serialization/Deserialization}

  This functions provides utilities for serializing and deserializing sets
  of strings using OCaml's Set and Marshal modules. *)

module SS = Set.Make (String)

(** [set_to_string s] serializes a set of strings [s] into a string
  using OCaml's Marshal module. *)
let set_to_string (s : SS.t) = Marshal.to_string s []

(** [string_to_set str] deserializes a string [str] back into a set
  of strings using OCaml's Marshal module. *)
let string_to_set str : SS.t = Marshal.from_string str 0
