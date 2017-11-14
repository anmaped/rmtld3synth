
open Batteries
open Unix

open Rmtld3synth_helper


(* change it for MACOS *)
let mk_proc = ref (Unix.open_process "");;

let mk_init () =
  (* test if mathematica is installed *)
  if Unix.system "math -noprompt -run \"Quit[]\"" = WEXITED(1) then print_endline "Mathematica is not available or not in PATH." else
  mk_proc := Unix.open_process "math -noprompt"

let mk_writeln s =
  output_string (snd (!mk_proc)) (s ^ "\n");
  BatIO.flush (snd (!mk_proc));;

let rec mk_readln () =
  let block_size = 250
  in let r = String.create (block_size + 1)
  in let n = input (fst !mk_proc) r 0 block_size
  in
  if n=block_size && (String.get r block_size) <> '\n' then (* TODO: we are not sure about the ending of a complete query *)
    (String.sub r 0 n) ^ (mk_readln ()) (* size is not enough *)
  else
    String.sub r 0 n



(* A simple handshaking function.  This should be run immediately
   after opening up an MK process.  It just does a simple check to
   make sure the MK is being responsive, and crucially gets the system
   into a state so that all strings read from it will begin with
   "\nOut[...]//FullForm=" and end with "\n\nIn[...]:= ", with the
   '...' being numbers. *)

let mk_handshake () =
  ((* print ("\n" ^ (mk_opt_str (!mk_active_options)) ^ "\n"); *)
    mk_writeln ("InitTime = TimeUsed[]");

    let x = mk_readln () in
    verb_m 1 (fun _ -> Printf.printf "%s\n" x; );

    (* mk_writeln ("FullForm[1+1]");
       block_until_read "FullForm= 2\n\nIn[4]:= " *)

    (* We need to install into Mathematica's runtime Wenda Li's
       polynomial real root isolation code *)

    (*mk_writeln (root_iso_code);*)

    (*** Setup our custom Mathematica REPL so we can use line-based I/O ***)
    mk_writeln ("While[True, NV = Input[\"In>\\n\"]; Print[NV]; If[NV == Quit, Quit[]]]");

    let x = mk_readln () in
    verb_m 1 (fun _ -> Printf.printf "%s\n" x; );

  );;



(* Close MathKernel process *)

let mk_close ignore_outcome =
  let _ = Unix.close_process !mk_proc in
  ();;
(*Printf.printf status;

   (if ignore_outcome orelse Useful.mem status [Unix.W_EXITED, Unix.W_SIGNALED 9] then ()
    else if status = Unix.W_SIGNALED sigxcpu
    then print "Processor time limit exceeded for Mathematica\n"
    else print ("****ERROR: exit status = " ^ stringOfStatus status ^ "\n");
    mk_proc := NONE)*)

let minisleep (sec: float) =
  ignore (Unix.select [] [] [] sec);;

