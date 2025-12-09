(** Helper functions for JavaScript targets.
  
  This module provides utilities for interacting with JavaScript
  environments, including file operations and FFI bindings. *)

include Js.Helper

(** Saves content to a file with the given filename. *)
let save_file filename content =
  let module J = Js_of_ocaml.Js.Unsafe in
  let obj = J.obj [||] in
  J.set obj "name" (Js_of_ocaml.Js.string filename) ;
  J.set obj "content" (Js_of_ocaml.Js.string content) ;
  (* Suppress unsound type warning for FFI call *)
  let _ =
    J.call
      (Js_of_ocaml.Js.Unsafe.pure_js_expr "postMessage")
      (Js_of_ocaml.Js.Unsafe.pure_js_expr "self")
      [|J.inject obj|]
  in
  ()
