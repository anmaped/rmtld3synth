(*
   Synthesis from RMTLD3 to cpp11
*)

open Batteries
open Unix
open Sexplib
open Sexplib.Conv

open Rmtld3
open Helper

type body = string * string

(* function that pretty prints 'observation' function as a lambda functions in c++ *)
let synth_obs_function observation_funcname struct_name =
  ""

(*
 * Compute terms
 *)
let synth_tm_constant value helper = failwith "Not implemented."

let synth_tm_variable name helper = failwith "No freevariables allowed."

let synth_tm_duration (di,_) (tf,_) helper = failwith "Not implemented."

let synth_tm_plus (cmptr1,_) (cmptr2,_) helper = failwith "Not implemented."

let synth_tm_times (cmptr1,_) (cmptr2,_) helper = failwith "Not implemented."


(*
 * compute formulas
*)
let synth_fm_true helper = ("T_TRUE","")

let synth_fm_p p helper = (
  "prop<T>(trace, PROP_"^ string_of_int p ^", t)",
  ""
)

let synth_fm_not (cmpfm,_) helper = ("b3_not("^ cmpfm ^")","")

let synth_fm_or (cmpfm1,_) (cmpfm2,_) helper = ("b3_or("^ cmpfm1 ^", "^ cmpfm2 ^")","")

let synth_fm_less (cmptr1,_) (cmptr2,_) helper = failwith "Not implemented."

let synth_fm_uless gamma (sf1,_) (sf2,_) helper = failwith "Not implemented."

let synth_fm_ueq gamma (sf1,_) (sf2,_) helper = failwith "Not implemented."

let synth_fm_ulesseq gamma (sf1,a) (sf2,b) helper =
  synth_fm_or (synth_fm_ueq gamma (sf1,a) (sf2,b) helper) (synth_fm_uless gamma (sf1,a) (sf2,b) helper) helper


(* monitor dependent c++ functions begin here *)
let synth_cpp11 _ _ _ _ _ compute helper =

    print_endline "Current Configuration:";
    print_settings helper;

    let expressions = get_all_setting_formula "input_exp" helper in
    let expressions = expressions @ get_all_setting_formula "input_exp_ltxeq" helper in
    (* let expressions = expressions @ get_all_setting_formula "input_exp_rmdsl" helper in *)
    
    print_endline "Expression(s):";
    List.iter (fun exp -> print_plaintext_formula exp; print_endline "") expressions;
    let cpp_monitor_lst = List.fold_right (fun exp lst -> ((compute exp helper), string_of_int (List.length lst))::lst) expressions [] in

    let pair_to_string (x, y) = "(" ^ x ^ ", " ^ y ^ ")" in
    let name = insert_string (get_setting_string "rtm_monitor_name_prefix" helper) (String.sub (Digest.string (String.concat "" (List.map pair_to_string cpp_monitor_lst)) |> Digest.to_hex ) 0 4) '%' in

    let monitor_name = insert_string name "compute" '#' in

    (* Synthesize ocaml formula evaluation algorithm into c++ *)
    let code1 = "/* This file was automatically generated from rmtld3synth tool version
"^ search_settings_string "version" helper ^". */

  #ifndef "^ String.uppercase_ascii (monitor_name) ^"_H_
  #define "^ String.uppercase_ascii (monitor_name) ^"_H_

  #include \"rmtld3/rmtld3.h\"
  #include \"rmtld3/macros.h\"
  
  // Propositions
  "^ Hashtbl.fold (fun x y str -> str^(Printf.sprintf "const proposition PROP_%i = %i;\n  " y y)) (get_proposition_hashtbl helper) "" ^"
  " ^
  ( List.fold_right (fun (cmp_str,n) str ->
  ("template<class T>
  three_valued_type _"^monitor_name^"_"^n^" (T &trace, timespan &t) { return "^ cmp_str ^"; };
  ") ^ str) cpp_monitor_lst "" )
  ^
  "
#ifdef USE_MAP_SORT
  #include <string>
  #include <unordered_map>

  // Create an unordered_map of sorts (that map to integers)
  std::unordered_map<std::string, int> _mapsorttostring = {
  "^ Hashtbl.fold (fun x y str -> str^(Printf.sprintf "{\"%s\",%i},\n  " x y)) (get_proposition_hashtbl helper) "" ^"};

  // Create an unordered_map of sorts (that map to strings)
  std::unordered_map<int, std::string> _mapsorttoint = {
  "^ Hashtbl.fold (fun x y str -> str^(Printf.sprintf "{%i,\"%s\"},\n  " y x)) (get_proposition_hashtbl helper) "" ^"};
#endif

  #endif //"^ String.uppercase_ascii (monitor_name) ^"_H_
    " in
    
    let monitor_name = insert_string name "monitor" '#' in

    let code2 = "/* This file was automatically generated from rmtld3synth tool version
"^ search_settings_string "version" helper ^". */

  #ifndef "^String.uppercase_ascii monitor_name^"_H
  #define "^String.uppercase_ascii monitor_name^"_H

  #include \"reader.h\"
  #include \"periodicmonitor.h\"
  #include \"rmtld3/reader.h\"
  #include \""^String.capitalize_ascii (insert_string name "compute" '#')^".h\"

  /* "^ string_of_int (search_settings_int "buffer_size" helper) ^", "^ get_event_fulltype(helper) ^" */
  #define RTML_BUFFER0_SIZE "^ string_of_int (search_settings_int "buffer_size" helper) ^"
  #define RTML_BUFFER0_TYPE "^ get_event_fulltype(helper) ^"
  #define RTML_BUFFER0_SETUP()                                                   \\
    RTML_buffer<RTML_BUFFER0_TYPE, RTML_BUFFER0_SIZE> __buffer_" ^ monitor_name ^";\\
    int tzero = 0;

    #define RTML_BUFFER0_TRIGGER_PERIODIC_MONITORS()                             \\
"
    ^ ( List.fold_right (fun (_,n) str ->
"\\
RMTLD3_reader<                                                               \\
RTML_reader<RTML_buffer<RTML_BUFFER0_TYPE, RTML_BUFFER0_SIZE>>, int>     \\
__trace_" ^ monitor_name ^ "_" ^ n ^ " = RMTLD3_reader<                                                   \\
    RTML_reader<RTML_buffer<RTML_BUFFER0_TYPE, RTML_BUFFER0_SIZE>>,      \\
    int>(__buffer_" ^ monitor_name ^", tzero);                           \\
\\
"
    ^ String.capitalize_ascii monitor_name^"_"^n^"<RMTLD3_reader<                                                          \\
        RTML_reader<RTML_buffer<RTML_BUFFER0_TYPE, RTML_BUFFER0_SIZE>>, int>>    \\
        rtm_mon" ^ n ^ "(" ^
    ( try string_of_int (get_setting_int "rtm_period" helper)
      with _ -> failwith "Set monitor period!"
    ) ^ ", __trace_" ^ monitor_name ^ "_" ^ n ^ "); \\
"
    ^ str) cpp_monitor_lst "" )
    ^ ( List.fold_right (fun (_,n) str ->
"

  template<class T>
  class "^String.capitalize_ascii monitor_name^"_"^n^" : public RTML_monitor {

  private:
    T &trace;

  protected:
    void run(){

      timespan tzero = 0;
      three_valued_type _out = _"^(insert_string name "compute" '#')^"_"^n^"<T>(trace,tzero);
      DEBUG_RTMLD3(\"Status:%d\\n\", _out);
    }

  public:
    "^String.capitalize_ascii monitor_name^"_"^n^"(useconds_t p, T& trc): RTML_monitor(p,SCHED_FIFO,50), trace(trc) {}
    "^String.capitalize_ascii monitor_name^"_"^n^"(useconds_t p, T& trc, int sche, int prio): RTML_monitor(p,sche,prio), trace(trc) {}

  };
"
^ str) cpp_monitor_lst "" ) ^
"
  #endif //"^String.uppercase_ascii monitor_name^"_H" in
  
    let monitor_name = insert_string name "instrument" '#' in

    let code3 ="/* This file was automatically generated from rmtld3synth tool version
  "^ search_settings_string "version" helper ^". */
  
  #ifndef "^ String.uppercase_ascii monitor_name ^"_H_
  #define "^ String.uppercase_ascii monitor_name ^"_H_
  
  // definition of buffer symbols

  #endif //"^ String.uppercase_ascii monitor_name ^"_H_" in
    
    try
      let out_dir = get_setting_string "out_dir" helper in
      
      print_endline "Generated Output Files List:";
      
      let monitor_name = String.capitalize_ascii (insert_string name "compute" '#') in

      let stream = open_out (out_dir^"/"^monitor_name^".h") in
        Printf.fprintf stream "%s\n" (beautify_cpp_code code1);
      close_out stream;

      print_endline (out_dir^"/"^monitor_name^".h");

      let monitor_name = String.capitalize_ascii (insert_string name "monitor" '#') in

      let stream = open_out (out_dir^"/"^monitor_name^".h") in
        Printf.fprintf stream "%s\n" (beautify_cpp_code code2);
      close_out stream;

      print_endline (out_dir^"/"^String.capitalize_ascii monitor_name^".h");

      let monitor_name = String.capitalize_ascii (insert_string name "instrument" '#') in

      let stream = open_out (out_dir^"/"^monitor_name^".h") in
        Printf.fprintf stream "%s\n" (beautify_cpp_code code3);
      close_out stream;

      print_endline (out_dir^"/"^monitor_name^".h")
      
    with Not_found ->
        (* print to console *)
        print_endline "Generated Output Header Files:";

        print_endline (beautify_cpp_code code1);
        print_endline (beautify_cpp_code code2);
        print_endline (beautify_cpp_code code3)

  (* monitor dependent functions ends here *)
;;

let synth_cpp11_env src_dir cluster_name evt_subtype event_queue_size helper =
(* this standalone cpp file instantiates the monitors that have been synthesized *)
  let stream = open_out (src_dir^"/"^ cluster_name ^".cpp") in
  let headers = List.fold_left (fun h (monitor_name,_,_) -> h^"#include \""^String.capitalize_ascii monitor_name^".h\"\n" ) "" (get_settings_monitor helper) in
  let functions = List.fold_left (fun f (monitor_name,monitor_period,_) -> f^String.capitalize_ascii monitor_name^" mon_"^monitor_name^"("^string_of_int monitor_period^");\n" ) "" (get_settings_monitor helper) in
  let code = headers ^"#include \"RTML_buffer.h\"

int count_until_iterations;

#ifdef __NUTTX__
__EXPORT RTML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^" __attribute__((used));
#else
RTML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^" __attribute__((used));
#endif
"
^ functions ^
"
void __start_periodic_monitors()
{
"^ List.fold_left (fun f (monitor_name,monitor_period,_) -> f^"  if (mon_"^monitor_name^".enable()) {::printf(\"ERROR\\n\");}\n" ) "" (get_settings_monitor helper) ^"
}
"
in
  Printf.fprintf stream "%s\n" code;
  close_out stream;

let stream = open_out (src_dir^"/"^ cluster_name ^".h") in
let propositions = 
  let tbl = get_proposition_hashtbl helper in
  Hashtbl.fold (fun a_string b_int prop_list -> prop_list^ "#define P_"^a_string^" "^(string_of_int b_int)^"\n" ) tbl ""
in
let code = "#ifndef _"^String.uppercase_ascii cluster_name^"_H_
#define _"^String.uppercase_ascii cluster_name^"_H_

#include \"RTML_buffer.h\"

extern void __start_periodic_monitors();

extern RTML_buffer<"^ evt_subtype ^", "^string_of_int event_queue_size^"> __buffer_"^ cluster_name ^";

"^propositions^"

#endif //_"^String.uppercase_ascii cluster_name^"_H_
"
in
Printf.fprintf stream "%s\n" code;
close_out stream;


(* the buffers need to be managed according to its size and type; one buffer
   could be used for several monitors; a monitor is the synthesis of a formula
 *)

;;
