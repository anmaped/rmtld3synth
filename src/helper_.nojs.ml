(**
  Helper functions for non-JavaScript targets.

  This module provides utility functions that are specific to native
  and bytecode compilation targets, excluding JavaScript environments.
*)

(** Saves content to a file with the given filename. *)
let save_file filename content =
  let stream = open_out filename in
  Printf.fprintf stream "%s\n" content ;
  close_out stream
