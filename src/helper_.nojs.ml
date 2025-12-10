(**
  Helper functions for non-JavaScript targets.

  This module provides utility functions that are specific to native
  and bytecode compilation targets, excluding JavaScript environments.
*)

(** [save_file filename content] writes the string [content] to a file
  specified by [filename], appending a newline character at the end.
  
  The function creates a new file or overwrites an existing file with
  the given filename. After writing the content, it properly closes
  the output channel.
  
  @param filename The path to the file where content will be written
  @param content The string content to write to the file
  
  @raise Sys_error if the file cannot be opened or written to
  
  Example:
  {[
    save_file "output.txt" "Hello, World!"
    (* Creates/overwrites output.txt with "Hello, World!\n" *)
  ]} *)
(** Saves content to a file with the given filename. *)
let save_file filename content =
  let stream = open_out filename in
  Printf.fprintf stream "%s\n" content ;
  close_out stream
