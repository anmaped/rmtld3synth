(* Synthesis from RMTLD3 to cpp11 *)

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
  if (conv t) <= (get_setting_string "rtm_monitor_time_unit" helper |> conv) then
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

let synth_tm_variable name helper = failwith "No freevariables allowed."

let synth_tm_duration (di, a) (tf, b) helper =
  let id = get_duration_counter helper in
  (* [TODO: check di; unknown is not implemented!; nested duration may be a
     problem!] *)
  ( "duration_term<T, Eval_duration" ^ string_of_int id ^ "<T>, " ^ di
    ^ ".first>(trace, t)"
  , a ^ b ^ "template <typename T> class Eval_duration" ^ string_of_int id
    ^ " {\n\
      \  public:\n\
      \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
      \      auto sf = " ^ tf
    ^ ";\n\
      \      return sf;\n\
      \    };\n\
      \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
      \      return T_UNKNOWN;\n\
      \    };\n\
      \  };" )

let synth_tm_plus (cmptr1, a) (cmptr2, b) helper =
  ("sum_dur(" ^ cmptr1 ^ ", " ^ cmptr2 ^ ")", a ^ b)

let synth_tm_times (cmptr1, a) (cmptr2, b) helper =
  ("mult_dur(" ^ cmptr1 ^ ", " ^ cmptr2 ^ ")", a ^ b)

(*
 * compute formulas
 *)
let synth_fm_true helper = ("T_TRUE", "")

let synth_fm_p p helper =
  ( "prop<T>(trace, PROP_"
    ^ Hashtbl.find (get_proposition_rev_hashtbl helper) p
    ^ ", t)"
  , "" )

let synth_fm_not (cmpfm, a) helper =
  ( "[](T &trace, timespan &t){\nauto x = " ^ cmpfm
    ^ ";  \nreturn b3_not(x);}(trace,t)"
  , a )

let synth_fm_or (cmpfm1, a) (cmpfm2, b) helper =
  ( "[](T &trace, timespan &t){\n    auto x = " ^ cmpfm1 ^ ";\n    auto y = "
    ^ cmpfm2 ^ ";\n    return b3_or(x, y); }(trace,t)"
  , a ^ b )

let synth_fm_less (cmptr1, a) (cmptr2, b) helper =
  ( "[](T &trace, timespan &t){\nauto x = " ^ cmptr1 ^ ";\nauto y = "
    ^ cmptr2 ^ ";\nreturn b3_lessthan(x, y);\n}(trace,t)"
  , a ^ b )

let convert_to_always_equal (sf2, b) gamma helper =
  print_endline
    ( "The next operator 'false U[" ^ string_of_float gamma
    ^ "] fm' or false U[=" ^ string_of_float gamma
    ^ "] fm' is converted to 'Always[="
    ^ string_of_int (int_of_float gamma)
    ^ "] fm' since cpp11 synthesis is enabled." ) ;
  (* get new id *)
  let id = get_until_counter helper in
  ( "always_equal<T, Eval_always_b_" ^ string_of_int id ^ "<T>, "
    ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
    ^ ">"
  , b ^ "\n      template <typename T> class Eval_always_b_"
    ^ string_of_int id
    ^ " {\n\
      \  public:\n\
      \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
      \      auto sf = " ^ sf2
    ^ ";\n\
      \      return sf;\n\
      \    };\n\
      \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
      \      return T_UNKNOWN;\n\
      \    };\n\
      \  };\n\
      \  " )

let convert_to_unbounded_eventually (sf2, b) helper =
  print_endline
    "The unbounded until operator 'true U[infty] fm' is converted to \
     'Eventually[infty] fm' since cpp11 synthesis is enabled." ;
  (* get new id *)
  let id = get_until_counter helper in
  ( "eventually_less_unbounded<T, Eval_eventually_less_unbounded_"
    ^ string_of_int id ^ "<T> >(trace, t)"
  , b ^ "\n      template <typename T> class Eval_eventually_less_unbounded_"
    ^ string_of_int id
    ^ " {\n\
      \  public:\n\
      \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
      \      auto sf = " ^ sf2
    ^ ";\n\
      \      return sf;\n\
      \    };\n\
      \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
      \      return T_UNKNOWN;\n\
      \    };\n\
      \  };\n\
      \  " )

let synth_fm_uless gamma (sf1, a) (sf2, b) helper =
  (* detect case when false U< f2 and f1=false *)
  if (sf1, a) = synth_fm_not (synth_fm_true helper) helper then
    convert_to_always_equal (sf2, b) gamma helper
  else if gamma = max_float && (sf1, a) = synth_fm_true helper then
    convert_to_unbounded_eventually (sf2, b) helper
  else
    (* get new id *)
    let id = get_until_counter helper in
    ( "until_less<T, Eval_until_less_" ^ string_of_int id ^ "<T>, "
      ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
      ^ ">(trace, t)"
    , a ^ b ^ "\n  template <typename T> class Eval_until_less_"
      ^ string_of_int id
      ^ " {\n\
        \  public:\n\
        \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
        \      auto sf = " ^ sf1
      ^ ";\n\
        \      return sf;\n\
        \    };\n\
        \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
        \      auto sf = " ^ sf2 ^ ";\n      return sf;\n    };\n  };\n" )

let synth_fm_ueq gamma (sf1, a) (sf2, b) helper =
  (* detect case when false U= f2 and f1=false *)
  if (sf1, a) = synth_fm_not (synth_fm_true helper) helper then
    convert_to_always_equal (sf2, b) gamma helper
  else
    (* get new id *)
    let id = get_until_counter helper in
    (* Until (=): A Until (=a) B <-> Always(<a) A and Eventually(=a) B *OR*
       Until (=): A Until (=a) B <-> Always(<a) A and Always(=a) B *)
    ( "[](T &trace, timespan &t){\n    auto x = always_less<T, Eval_always_a_"
      ^ string_of_int id ^ "<T>, "
      ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
      ^ ">(trace, t);\n    auto y = always_equal<T, Eval_always_b_"
      ^ string_of_int id ^ "<T>, "
      ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
      ^ ">(trace, t);\n    return b3_and(x,y);\n  }(trace, t)\n  "
    , a ^ b ^ "template <typename T> class Eval_always_a_" ^ string_of_int id
      ^ " {\n\
        \  public:\n\
        \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
        \      auto sf = " ^ sf1
      ^ ";\n\
        \      return sf;\n\
        \    };\n\
        \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
        \      return T_UNKNOWN;\n\
        \    };\n\
        \  };\n\
         template <typename T> class Eval_always_b_" ^ string_of_int id
      ^ " {\n\
        \  public:\n\
        \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
        \      auto sf = " ^ sf2
      ^ ";\n\
        \      return sf;\n\
        \    };\n\
        \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
        \      return T_UNKNOWN;\n\
        \    };\n\
        \  };\n\
        \  " )

let synth_fm_sless gamma (sf1, a) (sf2, b) helper =
  (* get new id *)
  let id = get_until_counter helper in
  ( "since_less<T, Eval_since_less_" ^ string_of_int id ^ "<T>, "
    ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
    ^ ">(trace, t)"
  , a ^ b ^ "\n  template <typename T> class Eval_since_less_"
    ^ string_of_int id
    ^ " {\n\
      \  public:\n\
      \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
      \      auto sf = " ^ sf1
    ^ ";\n\
      \      return sf;\n\
      \    };\n\
      \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
      \      auto sf = " ^ sf2 ^ ";\n      return sf;\n    };\n  };\n" )

let synth_fm_seq gamma (sf1, a) (sf2, b) helper =
  (* get new id *)
  let id = get_until_counter helper in
  ( "[](T &trace, timespan &t){\n\
    \    auto x = historically_less<T, Eval_historically_a_"
    ^ string_of_int id ^ "<T>, "
    ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
    ^ ">(trace, t);\n    auto y = historically_equal<T, Eval_historically_b_"
    ^ string_of_int id ^ "<T>, "
    ^ (adjust_base gamma helper |> int_of_float |> string_of_int)
    ^ ">(trace, t);\n    return b3_and(x,y);\n  }(trace, t)\n  "
  , a ^ b ^ "template <typename T> class Eval_historically_a_"
    ^ string_of_int id
    ^ " {\n\
      \  public:\n\
      \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
      \      auto sf = " ^ sf1
    ^ ";\n\
      \      return sf;\n\
      \    };\n\
      \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
      \      return T_UNKNOWN;\n\
      \    };\n\
      \  };\n\
       template <typename T> class Eval_historically_b_" ^ string_of_int id
    ^ " {\n\
      \  public:\n\
      \    static three_valued_type eval_phi1(T &trace, timespan &t) {\n\
      \      auto sf = " ^ sf2
    ^ ";\n\
      \      return sf;\n\
      \    };\n\
      \    static three_valued_type eval_phi2(T &trace, timespan &t) {\n\
      \      return T_UNKNOWN;\n\
      \    };\n\
      \  };\n\
      \  " )

(* monitor dependent c++ functions begin here *)
let synth_cpp11 compute helper =
  print_endline "Current Configuration:" ;
  print_settings helper ;
  let expressions = get_all_setting_formula "input_exp" helper in
  let expressions =
    expressions @ get_all_setting_formula "input_exp_dsl" helper
  in
  let expressions =
    expressions @ get_all_setting_formula "input_exp_ltxeq" helper
  in
  if expressions = [] then (
    print_endline "no formula is available." ;
    exit 1 ) ;
  (* let expressions = expressions @ get_all_setting_formula
     "input_exp_rmdsl" helper in *)
  print_endline "Expression(s) selected to encode:" ;
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
  (* Synthesize ocaml formula evaluation algorithm into c++ *)
  let code1 =
    "/* This file was automatically generated from rmtld3synth tool version\n"
    ^ get_setting_string "version" helper
    ^ ".\n\nSettings:\n"
    ^ get_string_of_settings helper
    ^ "\nFormula(s):\n"
    ^ List.fold_left
        (fun b exp -> b ^ "- " ^ string_of_rmtld_fm exp ^ "\n")
        "" expressions
    ^ "\n*/\n\n  #ifndef "
    ^ String.uppercase_ascii monitor_name
    ^ "_H_\n  #define "
    ^ String.uppercase_ascii monitor_name
    ^ "_H_\n\n\
      \  #include <rmtld3/rmtld3.h>\n\
      \  #include <rmtld3/formulas.h>\n\
      \  \n\
      \  #define RTM_TIME_UNIT "
    ^ get_setting_string "rtm_monitor_time_unit" helper
    ^ "\n  \n  // Propositions\n  "
    ^ Printf.sprintf "enum _auto_gen_prop {"
    ^ Hashtbl.fold
        (fun x y str -> str ^ Printf.sprintf "PROP_%s = %i, " x y)
        (get_proposition_hashtbl helper)
        ""
    ^ "}; \n  "
    ^ List.fold_right
        (fun ((function_call, body), n) str ->
          body
          ^ ( "\n        template<class T>\n        three_valued_type _"
            ^ monitor_name ^ "_" ^ n
            ^ " (T &trace, timespan &t) {\n          return " ^ function_call
            ^ ";\n        };\n        " )
          ^ str )
        cpp_monitor_lst ""
    ^ "\n\
       #ifdef RTMLIB_ENABLE_MAP_SORT\n\
      \  #include <string>\n\
      \  #include <unordered_map>\n\n\
      \  // Create an unordered_map of sorts (that map to integers)\n\
      \  std::unordered_map<std::string, int> _mapsorttostring = {\n\
      \  "
    ^ Hashtbl.fold
        (fun x y str -> str ^ Printf.sprintf "{\"%s\",PROP_%s},\n  " x x)
        (get_proposition_hashtbl helper)
        ""
    ^ "};\n\n\
      \  // Create an unordered_map of sorts (that map to strings)\n\
      \  std::unordered_map<int, std::string> _mapsorttoint = {\n\
      \  "
    ^ Hashtbl.fold
        (fun x y str -> str ^ Printf.sprintf "{PROP_%s,\"%s\"},\n  " x x)
        (get_proposition_hashtbl helper)
        ""
    ^ "};\n#endif\n\n  #endif //"
    ^ String.uppercase_ascii monitor_name
    ^ "_H_\n"
  in
  let monitor_name = insert_string name "monitor" '#' in
  let code2 =
    "/* This file was automatically generated from rmtld3synth tool version\n"
    ^ get_setting_string "version" helper
    ^ ". */\n\n  #ifndef "
    ^ String.uppercase_ascii monitor_name
    ^ "_H\n  #define "
    ^ String.uppercase_ascii monitor_name
    ^ "_H\n\n\
      \  #include <reader.h>\n\
      \  #include <periodicmonitor.h>\n\
      \  #include <rmtld3/reader.h>\n\
      \  #include \""
    ^ String.capitalize_ascii (insert_string name "compute" '#')
    ^ ".h\"\n\n  /* "
    ^ string_of_int (get_setting_int "rtm_buffer_size" helper)
    ^ ", " ^ get_event_fulltype helper ^ " */\n  #define "
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_SIZE "
    ^ string_of_int (get_setting_int "rtm_buffer_size" helper)
    ^ "\n  #define "
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_TYPE " ^ get_event_fulltype helper
    ^ "\n  #define RTML_BUFFER0_SETUP() \\\n    RTML_buffer<"
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_TYPE, "
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_SIZE> __buffer_" ^ monitor_name
    ^ ";\n    #define RTML_BUFFER0_TRIGGER_PERIODIC_MONITORS() \\\n"
    ^ List.fold_right
        (fun (_, n) str ->
          "RMTLD3_reader< \\\nRTML_reader<RTML_buffer<"
          ^ String.uppercase_ascii monitor_name
          ^ "_BUFFER_TYPE, "
          ^ String.uppercase_ascii monitor_name
          ^ "_BUFFER_SIZE>>> \\\n__trace_" ^ monitor_name ^ "_" ^ n
          ^ " = RMTLD3_reader< \\\n    RTML_reader<RTML_buffer<"
          ^ String.uppercase_ascii monitor_name
          ^ "_BUFFER_TYPE, "
          ^ String.uppercase_ascii monitor_name
          ^ "_BUFFER_SIZE>>>(__buffer_" ^ monitor_name ^ "); \\\n\\\n"
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n ^ "<RMTLD3_reader< \\\n        RTML_reader<RTML_buffer<"
          ^ String.uppercase_ascii monitor_name
          ^ "_BUFFER_TYPE, "
          ^ String.uppercase_ascii monitor_name
          ^ "_BUFFER_SIZE>>>> \\\n        rtm_mon" ^ n ^ "("
          ^ ( try string_of_int (get_setting_int "rtm_period" helper)
              with _ -> failwith "Set monitor period!" )
          ^ ", __trace_" ^ monitor_name ^ "_" ^ n ^ "); \\\n" ^ str )
        cpp_monitor_lst ""
    ^ List.fold_right
        (fun (_, n) str ->
          "\n\n  template<class T>\n  class "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n ^ " : public RTML_monitor<"
          ^
          let quote v = "'" ^ v ^ "'" in
          List.fold_left
            (fun a b -> a ^ "," ^ (String.make 1 b |> quote))
            (String.make 1 (List.hd (explode id)) |> quote)
            (List.tl (explode id))
          ^ "> {\n\n\
            \  private:\n\
            \    T &trace;\n\n\
            \  protected:\n\
            \    three_valued_type _out = T_UNKNOWN;\n\
            \    timespan tbegin;\n\n\
            \    void run(){\n\n\
            \      trace.synchronize();\n\
            \      if (trace.set(tbegin) == 0) {\n\
            \                    _out = _"
          ^ insert_string name "compute" '#'
          ^ "_" ^ n
          ^ "<T>(trace,tbegin);\n\
            \      }\n\
            \      DEBUG_RMTLD3(\"status=%d, tbegin=%lu\\n\", _out, tbegin);\n\
            \    }\n\n\
            \  public:\n\
            \    "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n
          ^ "(useconds_t p, T& trc, timespan& t): \
             RTML_monitor(p,DEFAULT_SCHED,DEFAULT_PRIORITY), trace(trc), \
             tbegin(t) { }\n\
            \    "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n
          ^ "(useconds_t p, T& trc): \
             RTML_monitor(p,DEFAULT_SCHED,DEFAULT_PRIORITY), trace(trc) { \
             tbegin = clockgettime(); }\n\
            \    "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n
          ^ "(useconds_t p, T& trc, int sche, int prio): \
             RTML_monitor(p,sche,prio), trace(trc) { tbegin = \
             clockgettime(); }\n\
            \    "
          ^ String.capitalize_ascii monitor_name
          ^ "_" ^ n
          ^ "(useconds_t p, T& trc, int sche, int prio, timespan& t): \
             RTML_monitor(p,sche,prio), trace(trc), tbegin(t) { }\n\
            \   three_valued_type &getVeredict() { return _out; }\n\
            \  };\n" ^ str )
        cpp_monitor_lst ""
    ^ "\n  #endif //"
    ^ String.uppercase_ascii monitor_name
    ^ "_H"
  in
  let monitor_name = insert_string name "instrument" '#' in
  let code3 =
    "/* This file was automatically generated from rmtld3synth tool version\n\
    \  "
    ^ get_setting_string "version" helper
    ^ ". */\n\n  #ifndef "
    ^ String.uppercase_ascii monitor_name
    ^ "_H_\n  #define "
    ^ String.uppercase_ascii monitor_name
    ^ "_H_\n\n"
    ^ "\n #include <writer.h>\n #include <rmtld3/rmtld3.h>\n\n #define "
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_SIZE "
    ^ string_of_int (get_setting_int "rtm_buffer_size" helper)
    ^ "\n\n class "
    ^ String.uppercase_ascii monitor_name
    ^ "{\n public:"
    ^ Printf.sprintf "typedef enum _auto_gen_prop {"
    ^ Hashtbl.fold
        (fun x y str -> str ^ Printf.sprintf "%s = %i, " x y)
        (get_proposition_hashtbl helper)
        ""
    ^ "} prop_t;\n\n typedef " ^ get_event_fulltype helper
    ^ " buffer_t;\n\n"
    ^ gen_adjust_base "ns" helper
    ^ gen_adjust_base "us" helper
    ^ gen_adjust_base "ms" helper
    ^ gen_adjust_base "s" helper
    ^ " \n\n \n};" ^ "\n\n template<typename T, T& buffer>\nclass Writer_"
    ^ String.uppercase_ascii monitor_name
    ^ " : public "
    ^ String.uppercase_ascii monitor_name
    ^ " {\n\n  public:\n typename T::error_t push("
    ^ get_event_subtype helper
    ^ " s) {\n\
      \      typename T::event_t e = typename T::event_t(s,0);\n\
      \      return w.push(e);\n\
      \    };\n\n\
      \ typename T::error_t push(" ^ get_event_subtype helper
    ^ " s, timespan t) {\n\
      \      typename T::event_t e = typename T::event_t(s,t);\n\
      \      return w.push_all(e);\n\
      \    };\n\n\
      \  private:\n\
      \    RTML_writer<T> w = RTML_writer<T>(buffer);\n\n\
       };\n\n\
       // buffer will be assigned at ld step\n\
       extern RTML_buffer<Event<std::underlying_type<"
    ^ String.uppercase_ascii monitor_name
    ^ "::prop_t>::type>, "
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_SIZE> __buffer_"
    ^ insert_string name "monitor" '#'
    ^ ";\n\nusing Writer_" ^ insert_string name "" '#' ^ " = Writer_"
    ^ String.uppercase_ascii monitor_name
    ^ "<RTML_buffer<Event<std::underlying_type<"
    ^ String.uppercase_ascii monitor_name
    ^ "::prop_t>::type>, "
    ^ String.uppercase_ascii monitor_name
    ^ "_BUFFER_SIZE>,__buffer_"
    ^ insert_string name "monitor" '#'
    ^ ">;\n\n    " ^ " #endif //"
    ^ String.uppercase_ascii monitor_name
    ^ "_H_"
  in
  try
    let out_dir = get_setting_string "out_dir" helper in
    print_endline "Generated Output Files:" ;
    let monitor_name =
      String.capitalize_ascii (insert_string name "compute" '#')
    in
    let stream = open_out (out_dir ^ "/" ^ monitor_name ^ ".h") in
    Printf.fprintf stream "%s\n" (beautify_cpp_code code1) ;
    close_out stream ;
    print_endline (out_dir ^ "/" ^ monitor_name ^ ".h") ;
    let monitor_name =
      String.capitalize_ascii (insert_string name "monitor" '#')
    in
    let stream = open_out (out_dir ^ "/" ^ monitor_name ^ ".h") in
    Printf.fprintf stream "%s\n" (beautify_cpp_code code2) ;
    close_out stream ;
    print_endline
      (out_dir ^ "/" ^ String.capitalize_ascii monitor_name ^ ".h") ;
    let monitor_name =
      String.capitalize_ascii (insert_string name "instrument" '#')
    in
    let stream = open_out (out_dir ^ "/" ^ monitor_name ^ ".h") in
    Printf.fprintf stream "%s\n" (beautify_cpp_code code3) ;
    close_out stream ;
    print_endline (out_dir ^ "/" ^ monitor_name ^ ".h")
  with Not_found ->
    (* print to console *)
    print_endline "Generated Output Header Files:" ;
    print_endline (beautify_cpp_code code1) ;
    print_endline (beautify_cpp_code code2) ;
    print_endline (beautify_cpp_code code3)

(* monitor dependent functions ends here *)
